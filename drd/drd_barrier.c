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


#include "drd_barrier.h"
#include "drd_clientobj.h"
#include "drd_error.h"
#include "drd_suppression.h"
#include "pub_tool_errormgr.h"    // VG_(maybe_record_error)()
#include "pub_tool_libcassert.h"  // tl_assert()
#include "pub_tool_libcprint.h"   // VG_(printf)()
#include "pub_tool_machine.h"     // VG_(get_IP)()
#include "pub_tool_mallocfree.h"  // VG_(malloc)(), VG_(free)()
#include "pub_tool_oset.h"
#include "pub_tool_threadstate.h" // VG_(get_running_tid)()


/* Type definitions. */

/** Information associated with one thread participating in a barrier. */
struct barrier_thread_info
{
   UWord       tid;           // A DrdThreadId declared as UWord because
                              // this member variable is the key of an OSet.
   Segment*    sg;            // Segment of the last pthread_barrier() call
                              // by thread tid.
   Segment*    post_wait_sg;  // Segment created after *_barrier_wait() finished
   ExeContext* wait_call_ctxt;// call stack for *_barrier_wait() call.
   Bool       thread_finished;// Whether thread 'tid' has finished.
};


/* Local functions. */

static void barrier_cleanup(struct barrier_info* p);
static void barrier_delete_thread(struct barrier_info* const p,
                                  const DrdThreadId tid);
static const char* barrier_get_typename(struct barrier_info* const p);
static const char* barrier_type_name(const BarrierT bt);
static
void barrier_report_wait_delete_race(const struct barrier_info* const p,
                                     const struct barrier_thread_info* const q);


/* Local variables. */

static Bool  s_trace_barrier = False;
static ULong s_barrier_segment_creation_count;


/* Function definitions. */

void DRD_(barrier_set_trace)(const Bool trace_barrier)
{
   s_trace_barrier = trace_barrier;
}

/**
 * Initialize the structure *p with the specified thread ID and iteration
 * information.
 */
static
void DRD_(barrier_thread_initialize)(struct barrier_thread_info* const p,
                                     const DrdThreadId tid)
{
   p->tid             = tid;
   p->sg              = NULL;
   p->post_wait_sg    = 0;
   p->wait_call_ctxt  = 0;
   p->thread_finished = False;
}

/**
 * Deallocate the memory that is owned by members of
 * struct barrier_thread_info.
 */
static void DRD_(barrier_thread_destroy)(struct barrier_thread_info* const p)
{
   tl_assert(p);
   DRD_(sg_put)(p->sg);
   DRD_(sg_put)(p->post_wait_sg);
}

/**
 * Initialize the structure *p with the specified client-side barrier address,
 * barrier object size and number of participants in each barrier.
 */
static
void DRD_(barrier_initialize)(struct barrier_info* const p,
                              const Addr barrier,
                              const BarrierT barrier_type,
                              const Word count)
{
   int i;

   tl_assert(barrier != 0);
   tl_assert(barrier_type == pthread_barrier || barrier_type == gomp_barrier);
   tl_assert(p->a1 == barrier);

   p->cleanup           = (void(*)(DrdClientobj*))barrier_cleanup;
   p->delete_thread
      = (void(*)(DrdClientobj*, DrdThreadId))barrier_delete_thread;
   p->barrier_type      = barrier_type;
   p->count             = count;
   p->pre_iteration     = 0;
   p->post_iteration    = 0;
   p->pre_waiters_left  = count;
   p->post_waiters_left = count;

   tl_assert(sizeof(((struct barrier_thread_info*)0)->tid) == sizeof(Word));
   tl_assert(sizeof(((struct barrier_thread_info*)0)->tid)
             >= sizeof(DrdThreadId));
   for (i = 0; i < 2; i++) {
      p->oset[i] = VG_(OSetGen_Create)(0, 0, VG_(malloc), "drd.barrier.bi.1",
                                       VG_(free));
   }
}

/**
 * Deallocate the memory owned by the struct barrier_info object and also
 * all the nodes in the OSet p->oset.
 *
 * Called by clientobj_destroy().
 */
static void barrier_cleanup(struct barrier_info* p)
{
   struct barrier_thread_info* q;
   Segment* latest_sg = 0;
   OSet* oset;
   int i;

   tl_assert(p);

   DRD_(thread_get_latest_segment)(&latest_sg, DRD_(thread_get_running_tid)());
   tl_assert(latest_sg);

   if (p->pre_waiters_left != p->count) {
      BarrierErrInfo bei = { DRD_(thread_get_running_tid)(), p->a1, 0, 0 };
      VG_(maybe_record_error)(VG_(get_running_tid)(),
                              BarrierErr,
                              VG_(get_IP)(VG_(get_running_tid)()),
                              "Destruction of barrier that is being waited"
                              " upon",
                              &bei);
   } else {
      oset = p->oset[1 - (p->pre_iteration & 1)];
      VG_(OSetGen_ResetIter)(oset);
      for ( ; (q = VG_(OSetGen_Next)(oset)) != 0; ) {
         if (q->post_wait_sg && !DRD_(vc_lte)(&q->post_wait_sg->vc,
                                              &latest_sg->vc))
         {
            barrier_report_wait_delete_race(p, q);
         }
         DRD_(barrier_thread_destroy)(q);
      }
   }

   for (i = 0; i < 2; i++) {
      VG_(OSetGen_Destroy)(p->oset[i]);
      p->oset[i] = NULL;
   }

   DRD_(sg_put)(latest_sg);
}

/**
 * Look up the client-side barrier address barrier in s_barrier[]. If not
 * found, add it.
 */
static
struct barrier_info*
DRD_(barrier_get_or_allocate)(const Addr barrier,
                              const BarrierT barrier_type, const Word count)
{
   struct barrier_info *p;

   tl_assert(barrier_type == pthread_barrier || barrier_type == gomp_barrier);

   tl_assert(offsetof(DrdClientobj, barrier) == 0);
   p = &(DRD_(clientobj_get)(barrier, ClientBarrier)->barrier);
   if (p == 0)
   {
      p = &(DRD_(clientobj_add)(barrier, ClientBarrier)->barrier);
      DRD_(barrier_initialize)(p, barrier, barrier_type, count);
   }
   return p;
}

/**
 * Look up the address of the struct barrier_info associated with the
 * client-side barrier object.
 */
static struct barrier_info* DRD_(barrier_get)(const Addr barrier)
{
   tl_assert(offsetof(DrdClientobj, barrier) == 0);
   return &(DRD_(clientobj_get)(barrier, ClientBarrier)->barrier);
}

/**
 * Initialize a barrier with given client address, barrier type and number of
 * participants. The 'reinitialization' argument indicates whether a barrier
 * object is being initialized or reinitialized.
 *
 * Called before pthread_barrier_init().
 */
void DRD_(barrier_init)(const Addr barrier,
                        const BarrierT barrier_type, const Word count,
                        const Bool reinitialization)
{
   struct barrier_info* p;

   tl_assert(barrier_type == pthread_barrier || barrier_type == gomp_barrier);

   if (count == 0)
   {
      BarrierErrInfo bei = { DRD_(thread_get_running_tid)(), barrier, 0, 0 };
      VG_(maybe_record_error)(VG_(get_running_tid)(),
                              BarrierErr,
                              VG_(get_IP)(VG_(get_running_tid)()),
                              "pthread_barrier_init: 'count' argument is zero",
                              &bei);
   }

   if (! reinitialization && barrier_type == pthread_barrier)
   {
      p = DRD_(barrier_get)(barrier);
      if (p)
      {
         BarrierErrInfo bei = { DRD_(thread_get_running_tid)(), barrier, 0, 0 };
         VG_(maybe_record_error)(VG_(get_running_tid)(),
                                 BarrierErr,
                                 VG_(get_IP)(VG_(get_running_tid)()),
                                 "Barrier reinitialization",
                                 &bei);
      }
   }

   p = DRD_(barrier_get_or_allocate)(barrier, barrier_type, count);

   if (s_trace_barrier) {
      if (reinitialization)
         DRD_(trace_msg)("[%d] barrier_reinit    %s 0x%lx count %ld -> %ld",
                         DRD_(thread_get_running_tid)(),
                         barrier_get_typename(p), barrier, p->count, count);
      else
         DRD_(trace_msg)("[%d] barrier_init      %s 0x%lx",
                         DRD_(thread_get_running_tid)(),
                         barrier_get_typename(p),
                         barrier);
   }

   if (reinitialization && p->count != count)
   {
      if (p->pre_waiters_left != p->count || p->post_waiters_left != p->count)
      {
         BarrierErrInfo bei = { DRD_(thread_get_running_tid)(), p->a1, 0, 0 };
         VG_(maybe_record_error)(VG_(get_running_tid)(),
                                 BarrierErr,
                                 VG_(get_IP)(VG_(get_running_tid)()),
                                 "Reinitialization of barrier with active"
                                 " waiters",
                                 &bei);
      }
      p->count = count;
   }
}

/** Called after pthread_barrier_destroy() / gomp_barrier_destroy(). */
void DRD_(barrier_destroy)(const Addr barrier, const BarrierT barrier_type)
{
   struct barrier_info* p;

   p = DRD_(barrier_get)(barrier);

   if (s_trace_barrier)
      DRD_(trace_msg)("[%d] barrier_destroy   %s 0x%lx",
                      DRD_(thread_get_running_tid)(),
                      barrier_get_typename(p), barrier);

   if (p == 0)
   {
      GenericErrInfo GEI = {
	 .tid = DRD_(thread_get_running_tid)(),
	 .addr = barrier,
      };
      VG_(maybe_record_error)(VG_(get_running_tid)(),
                              GenericErr,
                              VG_(get_IP)(VG_(get_running_tid)()),
                              "Not a barrier",
                              &GEI);
      return;
   }

   if (p->pre_waiters_left != p->count || p->post_waiters_left != p->count)
   {
      BarrierErrInfo bei = { DRD_(thread_get_running_tid)(), p->a1, 0, 0 };
      VG_(maybe_record_error)(VG_(get_running_tid)(),
                              BarrierErr,
                              VG_(get_IP)(VG_(get_running_tid)()),
                              "Destruction of a barrier with active waiters",
                              &bei);
   }

   DRD_(clientobj_remove)(p->a1, ClientBarrier);
}

/** Called before pthread_barrier_wait() / gomp_barrier_wait(). */
void DRD_(barrier_pre_wait)(const DrdThreadId tid, const Addr barrier,
                            const BarrierT barrier_type)
{
   struct barrier_info* p;
   struct barrier_thread_info* q;
   const UWord word_tid = tid;
   OSet* oset;

   p = DRD_(barrier_get)(barrier);
   if (p == 0 && barrier_type == gomp_barrier) {
      /*
       * gomp_barrier_wait() call has been intercepted but gomp_barrier_init()
       * not. The only cause I know of that can trigger this is that libgomp.so
       * has been compiled with --enable-linux-futex.
       */
      BarrierErrInfo bei = { DRD_(thread_get_running_tid)(), 0, 0, 0 };
      VG_(maybe_record_error)(VG_(get_running_tid)(),
                              BarrierErr,
                              VG_(get_IP)(VG_(get_running_tid)()),
                              "Please verify whether gcc has been configured"
                              " with option --disable-linux-futex. See also"
                              " the section about OpenMP in the DRD manual.",
                              &bei);
   }
   tl_assert(p);

   if (s_trace_barrier)
      DRD_(trace_msg)("[%d] barrier_pre_wait  %s 0x%lx iteration %ld",
                      DRD_(thread_get_running_tid)(),
                      barrier_get_typename(p), barrier, p->pre_iteration);

   /* Clean up nodes associated with finished threads. */
   oset = p->oset[p->pre_iteration & 1];
   tl_assert(oset);
   VG_(OSetGen_ResetIter)(oset);
   for ( ; (q = VG_(OSetGen_Next)(oset)) != 0; ) {
      if (q->thread_finished) {
         void* r = VG_(OSetGen_Remove)(oset, &q->tid);
         tl_assert(r == q);
         DRD_(barrier_thread_destroy)(q);
         VG_(OSetGen_FreeNode)(oset, q);
         VG_(OSetGen_ResetIterAt)(oset, &word_tid);
      }
   }
   /* Allocate the per-thread data structure if necessary. */
   q = VG_(OSetGen_Lookup)(oset, &word_tid);
   if (q == NULL) {
      q = VG_(OSetGen_AllocNode)(oset, sizeof(*q));
      DRD_(barrier_thread_initialize)(q, tid);
      VG_(OSetGen_Insert)(oset, q);
      tl_assert(VG_(OSetGen_Lookup)(oset, &word_tid) == q);
   }

   /* Record *_barrier_wait() call context. */
   q->wait_call_ctxt = VG_(record_ExeContext)(VG_(get_running_tid)(), 0);

   /*
    * Store a pointer to the latest segment of the current thread in the
    * per-thread data structure.
    */
   DRD_(thread_get_latest_segment)(&q->sg, tid);

   /*
    * If the same number of threads as the barrier count indicates have
    * called the pre *_barrier_wait() wrapper, toggle p->pre_iteration and
    * reset the p->pre_waiters_left counter.
    */
   if (--p->pre_waiters_left <= 0)
   {
      p->pre_iteration++;
      p->pre_waiters_left = p->count;
   }
}

/** Called after pthread_barrier_wait() / gomp_barrier_wait(). */
void DRD_(barrier_post_wait)(const DrdThreadId tid, const Addr barrier,
                             const BarrierT barrier_type, const Bool waited,
                             const Bool serializing)
{
   struct barrier_info* p;
   const UWord word_tid = tid;
   struct barrier_thread_info* q;
   struct barrier_thread_info* r;
   OSet* oset;

   p = DRD_(barrier_get)(barrier);

   if (s_trace_barrier)
      DRD_(trace_msg)("[%d] barrier_post_wait %s 0x%lx iteration %ld%s",
                      tid, p ? barrier_get_typename(p) : "(?)",
                      barrier, p ? p->post_iteration : -1,
                      serializing ? " (serializing)" : "");

   /*
    * If p == 0, this means that the barrier has been destroyed after
    * *_barrier_wait() returned and before this function was called. Just
    * return in that case -- race conditions between *_barrier_wait()
    * and *_barrier_destroy() are detected by the *_barrier_destroy() wrapper.
    */
   if (p == 0)
      return;

   /* If the *_barrier_wait() call returned an error code, exit. */
   if (! waited)
      return;

   oset = p->oset[p->post_iteration & 1];
   q = VG_(OSetGen_Lookup)(oset, &word_tid);
   if (p->pre_iteration - p->post_iteration > 1) {
      BarrierErrInfo bei = { DRD_(thread_get_running_tid)(), p->a1, 0, 0 };
      VG_(maybe_record_error)(VG_(get_running_tid)(),
                              BarrierErr,
                              VG_(get_IP)(VG_(get_running_tid)()),
                              "Number of concurrent pthread_barrier_wait()"
                              " calls exceeds the barrier count",
                              &bei);
   } else if (q == NULL) {
      BarrierErrInfo bei = { DRD_(thread_get_running_tid)(), p->a1, 0, 0 };
      VG_(maybe_record_error)(VG_(get_running_tid)(),
                              BarrierErr,
                              VG_(get_IP)(VG_(get_running_tid)()),
                              "Error in barrier implementation"
                              " -- barrier_wait() started before"
                              " barrier_destroy() and finished after"
                              " barrier_destroy()",
                              &bei);
   }
   if (q == NULL) {
      q = VG_(OSetGen_AllocNode)(oset, sizeof(*q));
      DRD_(barrier_thread_initialize)(q, tid);
      VG_(OSetGen_Insert)(oset, q);
      tl_assert(VG_(OSetGen_Lookup)(oset, &word_tid) == q);
      DRD_(thread_get_latest_segment)(&q->sg, tid);
   }

   /* Create a new segment and store a pointer to that segment. */
   DRD_(thread_new_segment)(tid);
   DRD_(thread_get_latest_segment)(&q->post_wait_sg, tid);
   s_barrier_segment_creation_count++;

   /*
    * Combine all vector clocks that were stored in the pre_barrier_wait
    * wrapper with the vector clock of the current thread.
    */
   {
      VectorClock old_vc;

      DRD_(vc_copy)(&old_vc, &DRD_(g_threadinfo)[tid].last->vc);
      VG_(OSetGen_ResetIter)(oset);
      for ( ; (r = VG_(OSetGen_Next)(oset)) != 0; )
      {
         if (r != q)
         {
            tl_assert(r->sg);
            DRD_(vc_combine)(&DRD_(g_threadinfo)[tid].last->vc,
                             &r->sg->vc);
         }
      }
      DRD_(thread_update_conflict_set)(tid, &old_vc);
      DRD_(vc_cleanup)(&old_vc);
   }

   /*
    * If the same number of threads as the barrier count indicates have
    * called the post *_barrier_wait() wrapper, toggle p->post_iteration and
    * reset the p->post_waiters_left counter.
    */
   if (--p->post_waiters_left <= 0)
   {
      p->post_iteration++;
      p->post_waiters_left = p->count;
   }
}

/** Called when thread tid stops to exist. */
static void barrier_delete_thread(struct barrier_info* const p,
                                  const DrdThreadId tid)
{
   struct barrier_thread_info* q;
   const UWord word_tid = tid;
   int i;

   for (i = 0; i < 2; i++) {
      q = VG_(OSetGen_Lookup)(p->oset[i], &word_tid);
      if (q)
         q->thread_finished = True;
   }
}

/**
 * Report that *_barrier_destroy() has been called but that this call was
 * not synchronized with the last *_barrier_wait() call on the same barrier.
 *
 * This topic has been discussed extensively on comp.programming.threads
 * (February 3, 2009). See also
 * <a href="http://groups.google.com/group/comp.programming.threads/browse_thread/thread/4f65535d6192aa50/a5f4bf1e3b437c4d">Immediately destroying pthread barriers</a>.
 */
static
void barrier_report_wait_delete_race(const struct barrier_info* const p,
                                     const struct barrier_thread_info* const q)
{
   tl_assert(p);
   tl_assert(q);

   {
      BarrierErrInfo bei
         = { DRD_(thread_get_running_tid)(), p->a1, q->tid, q->wait_call_ctxt };
      VG_(maybe_record_error)(VG_(get_running_tid)(),
                              BarrierErr,
                              VG_(get_IP)(VG_(get_running_tid)()),
                              "Destruction of barrier not synchronized with"
                              " barrier wait call",
                              &bei);
   }
}

static const char* barrier_get_typename(struct barrier_info* const p)
{
   tl_assert(p);

   return barrier_type_name(p->barrier_type);
}

static const char* barrier_type_name(const BarrierT bt)
{
   switch (bt)
   {
   case pthread_barrier:
      return "pthread barrier";
   case gomp_barrier:
      return "gomp barrier";
   }
   return "?";
}

ULong DRD_(get_barrier_segment_creation_count)(void)
{
   return s_barrier_segment_creation_count;
}
