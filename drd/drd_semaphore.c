/* -*- mode: C; c-basic-offset: 3; indent-tabs-mode: nil; -*- */
/*
  This file is part of drd, a thread error detector.

  Copyright (C) 2006-2011 Bart Van Assche <bvanassche@acm.org>.

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
#include "pub_tool_mallocfree.h"  // VG_(malloc), VG_(free)
#include "pub_tool_threadstate.h" // VG_(get_running_tid)()


/* Local functions. */

static void semaphore_cleanup(struct semaphore_info* p);


/* Local variables. */

static Bool s_trace_semaphore;
static ULong s_semaphore_segment_creation_count;


/* Function definitions. */

/** Push a segment at the end of the queue 'p->last_sem_post_seg'. */
static void drd_segment_push(struct semaphore_info* p, Segment* sg)
{
   Word n;

   tl_assert(sg);
   n = VG_(addToXA)(p->last_sem_post_seg, &sg);
#if 0
   VG_(message)(Vg_DebugMsg, "0x%lx push: added at position %ld/%ld",
                p->a1, n, VG_(sizeXA)(p->last_sem_post_seg));
#endif
   tl_assert(*(Segment**)VG_(indexXA)(p->last_sem_post_seg, n) == sg);
}

/** Pop a segment from the beginning of the queue 'p->last_sem_post_seg'. */
static Segment* drd_segment_pop(struct semaphore_info* p)
{
   Word sz;
   Segment* sg;

   sz = VG_(sizeXA)(p->last_sem_post_seg);
#if 0
   VG_(message)(Vg_DebugMsg, "0x%lx pop:  removed from position %ld/%ld",
                p->a1, sz - 1, sz);
#endif
   sg = 0;
   if (sz > 0)
   {
      sg = *(Segment**)VG_(indexXA)(p->last_sem_post_seg, sz - 1);
      tl_assert(sg);
      VG_(dropTailXA)(p->last_sem_post_seg, 1);
   }
   return sg;
}

/** Enable or disable tracing of semaphore actions. */
void DRD_(semaphore_set_trace)(const Bool trace_semaphore)
{
   s_trace_semaphore = trace_semaphore;
}

/**
 * Initialize the memory 'p' points at as a semaphore_info structure for the
 * client semaphore at client addres 'semaphore'.
 */
static
void drd_semaphore_initialize(struct semaphore_info* const p,
                              const Addr semaphore)
{
   tl_assert(semaphore != 0);
   tl_assert(p->a1 == semaphore);
   tl_assert(p->type == ClientSemaphore);

   p->cleanup           = (void(*)(DrdClientobj*))semaphore_cleanup;
   p->delete_thread     = 0;
   p->waits_to_skip     = 0;
   p->value             = 0;
   p->waiters           = 0;
   p->last_sem_post_tid = DRD_INVALID_THREADID;
   p->last_sem_post_seg = VG_(newXA)(VG_(malloc), "drd.sg-stack",
                                     VG_(free), sizeof(Segment*));
}

/**
 * Free the memory that was allocated by semaphore_initialize(). Called by
 * DRD_(clientobj_remove)().
 */
static void semaphore_cleanup(struct semaphore_info* p)
{
   Segment* sg;

   if (p->waiters > 0)
   {
      SemaphoreErrInfo sei = { DRD_(thread_get_running_tid)(), p->a1 };
      VG_(maybe_record_error)(VG_(get_running_tid)(),
                              SemaphoreErr,
                              VG_(get_IP)(VG_(get_running_tid)()),
                              "Destruction of semaphore that is being waited"
                              " upon",
                              &sei);
   }
   while ((sg = drd_segment_pop(p)))
      DRD_(sg_put)(sg);
   VG_(deleteXA)(p->last_sem_post_seg);
}

/**
 * Return a pointer to the structure with information about the specified
 * client semaphore. Allocate a new structure if such a structure did not
 * yet exist.
 */
static
struct semaphore_info*
drd_semaphore_get_or_allocate(const Addr semaphore)
{
   struct semaphore_info *p;

   tl_assert(offsetof(DrdClientobj, semaphore) == 0);
   p = &(DRD_(clientobj_get)(semaphore, ClientSemaphore)->semaphore);
   if (p == 0)
   {
      tl_assert(offsetof(DrdClientobj, semaphore) == 0);
      p = &(DRD_(clientobj_add)(semaphore, ClientSemaphore)->semaphore);
      drd_semaphore_initialize(p, semaphore);
   }
   return p;
}

/**
 * Return a pointer to the structure with information about the specified
 * client semaphore, or null if no such structure was found.
 */
static struct semaphore_info* semaphore_get(const Addr semaphore)
{
   tl_assert(offsetof(DrdClientobj, semaphore) == 0);
   return &(DRD_(clientobj_get)(semaphore, ClientSemaphore)->semaphore);
}

/** Called before sem_init(). */
struct semaphore_info* DRD_(semaphore_init)(const Addr semaphore,
                                            const Word pshared,
                                            const UInt value)
{
   struct semaphore_info* p;
   Segment* sg;

   if (s_trace_semaphore)
      DRD_(trace_msg)("[%d] sem_init      0x%lx value %u",
                      DRD_(thread_get_running_tid)(), semaphore, value);

   p = semaphore_get(semaphore);
   if (p)
   {
      const ThreadId vg_tid = VG_(get_running_tid)();
      SemaphoreErrInfo SEI = { DRD_(thread_get_running_tid)(), semaphore };
      VG_(maybe_record_error)(vg_tid,
                              SemaphoreErr,
                              VG_(get_IP)(vg_tid),
                              "Semaphore reinitialization",
                              &SEI);
      // Remove all segments from the segment stack.
      while ((sg = drd_segment_pop(p)))
      {
         DRD_(sg_put)(sg);
      }
   }
   else
   {
#if defined(VGO_darwin)
      const ThreadId vg_tid = VG_(get_running_tid)();
      GenericErrInfo GEI = { DRD_(thread_get_running_tid)(), 0 };
      VG_(maybe_record_error)(vg_tid,
			      GenericErr,
			      VG_(get_IP)(vg_tid),
			      "sem_init() is not yet supported on Darwin",
			      &GEI);
      return NULL;
#else
      p = drd_semaphore_get_or_allocate(semaphore);
#endif
   }
   tl_assert(p);
   p->waits_to_skip = value;
   p->value         = value;
   return p;
}

/** Called after sem_destroy(). */
void DRD_(semaphore_destroy)(const Addr semaphore)
{
   struct semaphore_info* p;

   p = semaphore_get(semaphore);

   if (s_trace_semaphore)
      DRD_(trace_msg)("[%d] sem_destroy   0x%lx value %u",
                      DRD_(thread_get_running_tid)(), semaphore,
                      p ? p->value : 0);

   if (p == 0)
   {
      GenericErrInfo GEI = {
	 .tid  = DRD_(thread_get_running_tid)(),
	 .addr = semaphore,
      };
      VG_(maybe_record_error)(VG_(get_running_tid)(),
                              GenericErr,
                              VG_(get_IP)(VG_(get_running_tid)()),
                              "Not a semaphore",
                              &GEI);
      return;
   }

   DRD_(clientobj_remove)(semaphore, ClientSemaphore);
}

/** Called after sem_open(). */
struct semaphore_info* DRD_(semaphore_open)(const Addr semaphore,
                                            const Char* name, const Word oflag,
                                            const Word mode, const UInt value)
{
   struct semaphore_info* p;
   Segment* sg;

   if (s_trace_semaphore)
      DRD_(trace_msg)("[%d] sem_open      0x%lx name %s"
                      " oflag %#lx mode %#lo value %u",
                      DRD_(thread_get_running_tid)(),
                      semaphore, name, oflag, mode, value);

   /* Return if the sem_open() call failed. */
   if (! semaphore)
      return NULL;

   p = semaphore_get(semaphore);
   if (p)
   {
      const ThreadId vg_tid = VG_(get_running_tid)();
      SemaphoreErrInfo SEI = { DRD_(thread_get_running_tid)(), semaphore };
      VG_(maybe_record_error)(vg_tid,
                              SemaphoreErr,
                              VG_(get_IP)(vg_tid),
                              "Semaphore reinitialization",
                              &SEI);
      // Remove all segments from the segment stack.
      while ((sg = drd_segment_pop(p)))
      {
         DRD_(sg_put)(sg);
      }
   }
   else
   {
      p = drd_semaphore_get_or_allocate(semaphore);
   }
   tl_assert(p);
   p->waits_to_skip = value;
   p->value         = value;
   return p;
}

/** Called before sem_close(). */
void DRD_(semaphore_close)(const Addr semaphore)
{
   struct semaphore_info* p;

   p = semaphore_get(semaphore);

   if (s_trace_semaphore)
      DRD_(trace_msg)("[%d] sem_close     0x%lx value %u",
                      DRD_(thread_get_running_tid)(), semaphore,
                      p ? p->value : 0);

   if (p == 0)
   {
      GenericErrInfo GEI = {
	 .tid  = DRD_(thread_get_running_tid)(),
	 .addr = semaphore,
      };
      VG_(maybe_record_error)(VG_(get_running_tid)(),
                              GenericErr,
                              VG_(get_IP)(VG_(get_running_tid)()),
                              "Not a semaphore",
                              &GEI);
      return;
   }

   DRD_(clientobj_remove)(semaphore, ClientSemaphore);
}

/** Called before sem_wait(). */
void DRD_(semaphore_pre_wait)(const Addr semaphore)
{
   struct semaphore_info* p;

   tl_assert(semaphore < semaphore + 1);
   p = drd_semaphore_get_or_allocate(semaphore);
   tl_assert(p);
   p->waiters++;

   if ((Word)(p->waiters) <= 0)
   {
      SemaphoreErrInfo sei = { DRD_(thread_get_running_tid)(), semaphore };
      VG_(maybe_record_error)(VG_(get_running_tid)(),
                              SemaphoreErr,
                              VG_(get_IP)(VG_(get_running_tid)()),
                              "Invalid semaphore",
                              &sei);
   }
}

/**
 * Called after sem_wait() finished.
 * @note Do not rely on the value of 'waited' -- some glibc versions do
 *       not set it correctly.
 */
void DRD_(semaphore_post_wait)(const DrdThreadId tid, const Addr semaphore,
                               const Bool waited)
{
   struct semaphore_info* p;
   Segment* sg;

   p = semaphore_get(semaphore);
   if (s_trace_semaphore)
      DRD_(trace_msg)("[%d] sem_wait      0x%lx value %u -> %u",
                      DRD_(thread_get_running_tid)(), semaphore,
                      p ? p->value : 0, p ? p->value - 1 : 0);

   if (p)
   {
      p->waiters--;
      p->value--;
   }

   /*
    * Note: if another thread destroyed and reinitialized a semaphore while
    * the current thread was waiting in sem_wait, p->waiters may have been
    * set to zero by drd_semaphore_initialize() after
    * DRD_(semaphore_pre_wait)() has finished before
    * DRD_(semaphore_post_wait)() has been called.
    */
   if (p == NULL || (Int)(p->value) < 0 || (Word)(p->waiters) < 0)
   {
      SemaphoreErrInfo sei = { DRD_(thread_get_running_tid)(), semaphore };
      VG_(maybe_record_error)(VG_(get_running_tid)(),
                              SemaphoreErr,
                              VG_(get_IP)(VG_(get_running_tid)()),
                              "Invalid semaphore",
                              &sei);
      return;
   }

   if (p->waits_to_skip > 0)
      p->waits_to_skip--;
   else
   {
      sg = drd_segment_pop(p);
      tl_assert(sg);
      if (p->last_sem_post_tid != tid
          && p->last_sem_post_tid != DRD_INVALID_THREADID)
      {
         DRD_(thread_new_segment_and_combine_vc)(tid, sg);
      }
      else
         DRD_(thread_new_segment)(tid);
      s_semaphore_segment_creation_count++;
      DRD_(sg_put)(sg);
   }
}

/** Called before sem_post(). */
void DRD_(semaphore_pre_post)(const DrdThreadId tid, const Addr semaphore)
{
   struct semaphore_info* p;
   Segment* sg;

   p = drd_semaphore_get_or_allocate(semaphore);
   p->value++;

   if (s_trace_semaphore)
      DRD_(trace_msg)("[%d] sem_post      0x%lx value %u -> %u",
                      DRD_(thread_get_running_tid)(),
                      semaphore, p->value - 1, p->value);

   p->last_sem_post_tid = tid;
   sg = 0;
   DRD_(thread_get_latest_segment)(&sg, tid);
   tl_assert(sg);
   drd_segment_push(p, sg);
   DRD_(thread_new_segment)(tid);
   s_semaphore_segment_creation_count++;
}

/** Called after sem_post() finished. */
void DRD_(semaphore_post_post)(const DrdThreadId tid, const Addr semaphore,
                               const Bool succeeded)
{
   /*
    * Note: it is hard to implement the sem_post() wrapper correctly in
    * case sem_post() returns an error code. This is because handling this
    * case correctly requires restoring the vector clock associated with
    * the semaphore to its original value here. In order to do that without
    * introducing a race condition, extra locking has to be added around
    * each semaphore call. Such extra locking would have to be added in
    * drd_pthread_intercepts.c. However, it is hard to implement
    * synchronization in drd_pthread_intercepts.c in a portable way without
    * calling already redirected functions.
    */
}

ULong DRD_(get_semaphore_segment_creation_count)(void)
{
   return s_semaphore_segment_creation_count;
}
