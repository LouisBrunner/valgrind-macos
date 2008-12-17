/*
  This file is part of drd, a data race detector.

  Copyright (C) 2006-2008 Bart Van Assche
  bart.vanassche@gmail.com

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


#include "drd_clientobj.h"
#include "drd_error.h"
#include "drd_semaphore.h"
#include "drd_suppression.h"
#include "pub_tool_errormgr.h"    // VG_(maybe_record_error)()
#include "pub_tool_libcassert.h"  // tl_assert()
#include "pub_tool_libcprint.h"   // VG_(printf)()
#include "pub_tool_machine.h"     // VG_(get_IP)()
#include "pub_tool_threadstate.h" // VG_(get_running_tid)()


// Local functions.

static void semaphore_cleanup(struct semaphore_info* p);


// Local variables.

static Bool s_trace_semaphore;
static ULong s_semaphore_segment_creation_count;


// Function definitions.

void semaphore_set_trace(const Bool trace_semaphore)
{
  s_trace_semaphore = trace_semaphore;
}

static
void semaphore_initialize(struct semaphore_info* const p,
                          const Addr semaphore, const UWord value)
{
  tl_assert(semaphore != 0);
  tl_assert(p->a1 == semaphore);
  tl_assert(p->type == ClientSemaphore);

  p->cleanup   = (void(*)(DrdClientobj*))semaphore_cleanup;
  p->value     = value;
  p->waiters   = 0;
  p->last_sem_post_tid = DRD_INVALID_THREADID;
  p->last_sem_post_segment = 0;
}

/** Free the memory that was allocated by semaphore_initialize(). Called by
 *  clientobj_remove().
 */
static void semaphore_cleanup(struct semaphore_info* p)
{
  if (p->waiters > 0)
  {
    SemaphoreErrInfo sei = { p->a1 };
    VG_(maybe_record_error)(VG_(get_running_tid)(),
                            SemaphoreErr,
                            VG_(get_IP)(VG_(get_running_tid)()),
                            "Destruction of semaphore that is being waited"
                            " upon",
                            &sei);
  }
  sg_put(p->last_sem_post_segment);
}

static
struct semaphore_info*
semaphore_get_or_allocate(const Addr semaphore)
{
  struct semaphore_info *p;

  tl_assert(offsetof(DrdClientobj, semaphore) == 0);
  p = &clientobj_get(semaphore, ClientSemaphore)->semaphore;
  if (p == 0)
  {
    tl_assert(offsetof(DrdClientobj, semaphore) == 0);
    p = &clientobj_add(semaphore, ClientSemaphore)->semaphore;
    semaphore_initialize(p, semaphore, 0);
  }
  return p;
}

static struct semaphore_info* semaphore_get(const Addr semaphore)
{
  tl_assert(offsetof(DrdClientobj, semaphore) == 0);
  return &clientobj_get(semaphore, ClientSemaphore)->semaphore;
}

/** Called before sem_init(). */
struct semaphore_info* semaphore_init(const Addr semaphore,
                                      const Word pshared, const UWord value)
{
  struct semaphore_info* p;

  if (s_trace_semaphore)
  {
    VG_(message)(Vg_UserMsg,
                 "[%d/%d] semaphore_init      0x%lx value %ld",
                 VG_(get_running_tid)(),
                 thread_get_running_tid(),
                 semaphore,
                 value);
  }
  p = semaphore_get(semaphore);
  if (p)
  {
    const ThreadId vg_tid = VG_(get_running_tid)();
    SemaphoreErrInfo SEI = { semaphore };
    VG_(maybe_record_error)(vg_tid,
                            SemaphoreErr,
                            VG_(get_IP)(vg_tid),
                            "Semaphore reinitialization",
                            &SEI);
  }
  else
  {
    p = semaphore_get_or_allocate(semaphore);
  }
  tl_assert(p);
  p->value = value;
  return p;
}

/** Called after sem_destroy(). */
void semaphore_destroy(const Addr semaphore)
{
  struct semaphore_info* p;

  p = semaphore_get(semaphore);

  if (s_trace_semaphore)
  {
    VG_(message)(Vg_UserMsg,
                 "[%d/%d] semaphore_destroy   0x%lx value %ld",
                 VG_(get_running_tid)(),
                 thread_get_running_tid(),
                 semaphore,
                 p ? p->value : 0);
  }

  if (p == 0)
  {
    GenericErrInfo GEI;
    VG_(maybe_record_error)(VG_(get_running_tid)(),
                            GenericErr,
                            VG_(get_IP)(VG_(get_running_tid)()),
                            "Not a semaphore",
                            &GEI);
    return;
  }

  clientobj_remove(semaphore, ClientSemaphore);
}

/** Called before sem_wait(). */
void semaphore_pre_wait(const Addr semaphore)
{
  struct semaphore_info* p;

  p = semaphore_get_or_allocate(semaphore);
  if (s_trace_semaphore)
  {
    VG_(message)(Vg_UserMsg,
                 "[%d/%d] semaphore_pre_wait  0x%lx value %ld",
                 VG_(get_running_tid)(),
                 thread_get_running_tid(),
                 semaphore,
                 p->value);
  }
  tl_assert(p);
  tl_assert((int)p->waiters >= 0);
  p->waiters++;
  tl_assert(p->waiters > 0);
}

/** Called after sem_wait() finished.
 *  @note Do not rely on the value of 'waited' -- some glibc versions do
 *        not set it correctly.
 */
void semaphore_post_wait(const DrdThreadId tid, const Addr semaphore,
                         const Bool waited)
{
  struct semaphore_info* p;

  p = semaphore_get(semaphore);
  if (s_trace_semaphore)
  {
    VG_(message)(Vg_UserMsg,
                 "[%d/%d] semaphore_post_wait 0x%lx value %ld",
                 VG_(get_running_tid)(),
                 thread_get_running_tid(),
                 semaphore,
                 p ? p->value - 1 : 0);
  }
  tl_assert(p->waiters > 0);
  p->waiters--;
  tl_assert((int)p->waiters >= 0);
  tl_assert((int)p->value >= 0);
  if (p->value == 0)
  {
    SemaphoreErrInfo sei = { semaphore };
    VG_(maybe_record_error)(VG_(get_running_tid)(),
                            SemaphoreErr,
                            VG_(get_IP)(VG_(get_running_tid)()),
                            "Invalid semaphore",
                            &sei);
    return;
  }
  p->value--;
  tl_assert((int)p->value >= 0);
  if (p->last_sem_post_tid != tid
      && p->last_sem_post_tid != DRD_INVALID_THREADID)
  {
    tl_assert(p->last_sem_post_segment);
    thread_combine_vc2(tid, &p->last_sem_post_segment->vc);
  }
  thread_new_segment(tid);
  s_semaphore_segment_creation_count++;
}

/** Called before sem_post(). */
void semaphore_pre_post(const DrdThreadId tid, const Addr semaphore)
{
  struct semaphore_info* p;

  p = semaphore_get_or_allocate(semaphore);
  p->value++;

  if (s_trace_semaphore)
  {
    VG_(message)(Vg_UserMsg,
                 "[%d/%d] semaphore_post      0x%lx value %ld",
                 VG_(get_running_tid)(),
                 thread_get_running_tid(),
                 semaphore,
                 p->value);
  }

  if (p->value == 1)
  {
    p->last_sem_post_tid = tid;
    thread_new_segment(tid);
    thread_get_latest_segment(&p->last_sem_post_segment, tid);
    s_semaphore_segment_creation_count++;
  }
}

/** Called after sem_post() finished successfully. */
void semaphore_post_post(const DrdThreadId tid, const Addr semaphore,
                         const Bool waited)
{
  /* Note: it is hard to implement the sem_post() wrapper correctly in     */
  /* case sem_post() returns an error code. This is because handling this  */
  /* case correctly requires restoring the vector clock associated with    */
  /* the semaphore to its original value here. In order to do that without */
  /* introducing a race condition, extra locking has to be added around    */
  /* each semaphore call. Such extra locking would have to be added in     */
  /* drd_intercepts.c. However, it is hard to implement synchronization    */
  /* in drd_intercepts.c in a portable way without calling already         */
  /* redirected functions.                                                 */
}

void semaphore_thread_delete(const DrdThreadId threadid)
{ }

ULong get_semaphore_segment_creation_count(void)
{
  return s_semaphore_segment_creation_count;
}
