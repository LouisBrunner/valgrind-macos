
/*--------------------------------------------------------------------*/
/*--- Linux ticket lock implementation         ticket-lock-linux.c ---*/
/*---                                                              ---*/
/*--- Guarantees fair scheduling even if multiple threads are      ---*/
/*--- runnable at the same time on a multicore system. Has been    ---*/
/*--- observed to cause a slow-down compared to the generic        ---*/
/*--- scheduler lock with CPU frequency scaling enabled. Makes     ---*/
/*--- Valgrind slightly faster if CPU frequency scaling has been   ---*/
/*--- disabled. See also http://bugs.kde.org/show_bug.cgi?id=270006---*/
/*--------------------------------------------------------------------*/

/*
   This file is part of Valgrind, a dynamic binary instrumentation
   framework.

   Copyright (C) 2011 Bart Van Assche <bvanassche@acm.org>.

   This program is free software; you can redistribute it and/or
   modify it under the terms of the GNU General Public License as
   published by the Free Software Foundation; either version 2 of the
   License, or (at your option) any later version.

   This program is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software
   Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
   02111-1307, USA.

   The GNU General Public License is contained in the file COPYING.
*/

#include "pub_core_basics.h"
#include "pub_core_libcassert.h"
#include "pub_core_libcbase.h"     // VG_(memset)()
#include "pub_core_libcprint.h"
#include "pub_core_syscall.h"
#include "pub_core_vki.h"
#include "pub_core_vkiscnums.h"    // __NR_futex
#include "pub_tool_libcproc.h"
#include "pub_tool_mallocfree.h"
#include "pub_tool_threadstate.h"
#include "pub_tool_inner.h"
#if defined(ENABLE_INNER_CLIENT_REQUEST)
#include "helgrind/helgrind.h"
#endif
#include "priv_sched-lock.h"
#include "priv_sched-lock-impl.h"

#define TL_FUTEX_COUNT_LOG2 4
#define TL_FUTEX_COUNT (1U << TL_FUTEX_COUNT_LOG2)
#define TL_FUTEX_MASK (TL_FUTEX_COUNT - 1)

struct sched_lock {
   volatile unsigned head;
   volatile unsigned tail;
   volatile unsigned futex[TL_FUTEX_COUNT];
   int owner;
};

#if 1
static Bool s_debug;
#else
static Bool s_debug = True;
#endif

static const Char *get_sched_lock_name(void)
{
   return "ticket lock";
}

static struct sched_lock *create_sched_lock(void)
{
   struct sched_lock *p;

   p = VG_(malloc)("sched_lock", sizeof(*p));
   if (p) {
      // The futex syscall requires that a futex takes four bytes.
      vg_assert(sizeof(p->futex[0]) == 4);

      p->head = 0;
      p->tail = 0;
      VG_(memset)((void*)p->futex, 0, sizeof(p->futex));
      p->owner = 0;
   }
   INNER_REQUEST(ANNOTATE_RWLOCK_CREATE(p));
   INNER_REQUEST(ANNOTATE_BENIGN_RACE_SIZED(&p->futex, sizeof(p->futex), ""));
   return p;
}

static void destroy_sched_lock(struct sched_lock *p)
{
   INNER_REQUEST(ANNOTATE_RWLOCK_DESTROY(p));
   VG_(free)(p);
}

static int get_sched_lock_owner(struct sched_lock *p)
{
   return p->owner;
}

/*
 * Acquire ticket lock. Increment the tail of the queue and use the original
 * value as the ticket value. Wait until the head of the queue equals the
 * ticket value. The futex used to wait depends on the ticket value in order
 * to avoid that all threads get woken up every time a ticket lock is
 * released. That last effect is sometimes called the "thundering herd"
 * effect.
 *
 * See also Nick Piggin, x86: FIFO ticket spinlocks, Linux kernel mailing list
 * (http://lkml.org/lkml/2007/11/1/125) for more info.
 */
static void acquire_sched_lock(struct sched_lock *p)
{
   unsigned ticket, futex_value;
   volatile unsigned *futex;
   SysRes sres;

   ticket = __sync_fetch_and_add(&p->tail, 1);
   futex = &p->futex[ticket & TL_FUTEX_MASK];
   if (s_debug)
      VG_(printf)("[%d/%d] acquire: ticket %d\n", VG_(getpid)(),
                  VG_(gettid)(), ticket);
   for (;;) {
      futex_value = *futex;
      __sync_synchronize();
      if (ticket == p->head)
         break;
      if (s_debug)
         VG_(printf)("[%d/%d] acquire: ticket %d - waiting until"
                     " futex[%ld] != %d\n", VG_(getpid)(),
                     VG_(gettid)(), ticket, (long)(futex - p->futex),
                     futex_value);
      sres = VG_(do_syscall3)(__NR_futex, (UWord)futex,
                              VKI_FUTEX_WAIT | VKI_FUTEX_PRIVATE_FLAG,
                              futex_value);
      if (sr_isError(sres) && sres._val != VKI_EAGAIN) {
         VG_(printf)("futex_wait() returned error code %ld\n", sres._val);
         vg_assert(False);
      }
   }
   __sync_synchronize();
   INNER_REQUEST(ANNOTATE_RWLOCK_ACQUIRED(p, /*is_w*/1));
   vg_assert(p->owner == 0);
   p->owner = VG_(gettid)();
}

/*
 * Release a ticket lock by incrementing the head of the queue. Only generate
 * a thread wakeup signal if at least one thread is waiting. If the queue tail
 * matches the wakeup_ticket value, no threads have to be woken up.
 *
 * Note: tail will only be read after head has been incremented since both are
 * declared as volatile and since the __sync...() functions imply a memory
 * barrier.
 */
static void release_sched_lock(struct sched_lock *p)
{
   unsigned wakeup_ticket, futex_value;
   volatile unsigned *futex;
   SysRes sres;

   vg_assert(p->owner != 0);
   p->owner = 0;
   INNER_REQUEST(ANNOTATE_RWLOCK_RELEASED(p, /*is_w*/1));
   wakeup_ticket = __sync_fetch_and_add(&p->head, 1) + 1;
   if (p->tail != wakeup_ticket) {
      futex = &p->futex[wakeup_ticket & TL_FUTEX_MASK];
      futex_value = __sync_fetch_and_add(futex, 1);
      if (s_debug)
         VG_(printf)("[%d/%d] release: waking up ticket %d (futex[%ld] = %d)"
                     "\n", VG_(getpid)(), VG_(gettid)(), wakeup_ticket,
                     (long)(futex - p->futex), futex_value);
      sres = VG_(do_syscall3)(__NR_futex, (UWord)futex,
                              VKI_FUTEX_WAKE | VKI_FUTEX_PRIVATE_FLAG,
                              0x7fffffff);
      vg_assert(!sr_isError(sres));
   } else {
      if (s_debug)
         VG_(printf)("[%d/%d] release: no thread is waiting for ticket %d\n",
                     VG_(getpid)(), VG_(gettid)(), wakeup_ticket);
   }
}

const struct sched_lock_ops ML_(linux_ticket_lock_ops) = {
   .get_sched_lock_name  = get_sched_lock_name,
   .create_sched_lock    = create_sched_lock,
   .destroy_sched_lock   = destroy_sched_lock,
   .get_sched_lock_owner = get_sched_lock_owner,
   .acquire_sched_lock   = acquire_sched_lock,
   .release_sched_lock   = release_sched_lock,
};
