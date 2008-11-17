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


// Type definitions.

/** Information associated with one thread participating in a barrier. */
struct barrier_thread_info
{
  UWord       tid;           // A DrdThreadId
  Word        iteration;     // iteration of last pthread_barrier_wait()
                             // call thread tid participated in.
  Segment*    sg[2];         // Segments of the last two
                             // pthread_barrier() calls by thread tid.
};


// Local functions.

static void barrier_cleanup(struct barrier_info* p);
static const char* barrier_get_typename(struct barrier_info* const p);
static const char* barrier_type_name(const BarrierT bt);


// Local variables.

static Bool s_trace_barrier = False;
static ULong s_barrier_segment_creation_count;


// Function definitions.

void barrier_set_trace(const Bool trace_barrier)
{
  s_trace_barrier = trace_barrier;
}

/** Initialize the structure *p with the specified thread ID and iteration
 *  information. */
static void barrier_thread_initialize(struct barrier_thread_info* const p,
                                      const DrdThreadId tid,
                                      const Word iteration)
{
  p->tid = tid;
  p->iteration = iteration;
  p->sg[0] = 0;
  p->sg[1] = 0;
}

/** Deallocate the memory that was allocated in barrier_thread_initialize(). */
static void barrier_thread_destroy(struct barrier_thread_info* const p)
{
  tl_assert(p);
  sg_put(p->sg[0]);
  sg_put(p->sg[1]);
}

/** Initialize the structure *p with the specified client-side barrier address,
 *  barrier object size and number of participants in each barrier. */
static
void barrier_initialize(struct barrier_info* const p,
                        const Addr barrier,
                        const BarrierT barrier_type,
                        const Word count)
{
  tl_assert(barrier != 0);
  tl_assert(barrier_type == pthread_barrier || barrier_type == gomp_barrier);
  tl_assert(p->a1 == barrier);

  p->cleanup           = (void(*)(DrdClientobj*))barrier_cleanup;
  p->barrier_type      = barrier_type;
  p->count             = count;
  p->pre_iteration     = 0;
  p->post_iteration    = 0;
  p->pre_waiters_left  = count;
  p->post_waiters_left = count;
  tl_assert(sizeof(((struct barrier_thread_info*)0)->tid) == sizeof(Word));
  tl_assert(sizeof(((struct barrier_thread_info*)0)->tid)
            >= sizeof(DrdThreadId));
  p->oset = VG_(OSetGen_Create)(0, 0, VG_(malloc), "drd.barrier.bi.1",
                                      VG_(free));
}

/** Deallocate the memory allocated by barrier_initialize() and in p->oset. 
 *  Called by clientobj_destroy().
 */
void barrier_cleanup(struct barrier_info* p)
{
  struct barrier_thread_info* q;

  tl_assert(p);

  if (p->pre_waiters_left != p->count)
  {
    BarrierErrInfo bei = { p->a1 };
    VG_(maybe_record_error)(VG_(get_running_tid)(),
                            BarrierErr,
                            VG_(get_IP)(VG_(get_running_tid)()),
                            "Destruction of barrier that is being waited"
                            " upon",
                            &bei);
  }

  VG_(OSetGen_ResetIter)(p->oset);
  for ( ; (q = VG_(OSetGen_Next)(p->oset)) != 0; )
  {
    barrier_thread_destroy(q);
  }
  VG_(OSetGen_Destroy)(p->oset);
}

/** Look up the client-side barrier address barrier in s_barrier[]. If not
 *  found, add it. */
static
struct barrier_info*
barrier_get_or_allocate(const Addr barrier,
                        const BarrierT barrier_type, const Word count)
{
  struct barrier_info *p;

  tl_assert(barrier_type == pthread_barrier || barrier_type == gomp_barrier);

  tl_assert(offsetof(DrdClientobj, barrier) == 0);
  p = &clientobj_get(barrier, ClientBarrier)->barrier;
  if (p == 0)
  {
    p = &clientobj_add(barrier, ClientBarrier)->barrier;
    barrier_initialize(p, barrier, barrier_type, count);
  }
  return p;
}

/** Look up the address of the information associated with the client-side
 *  barrier object. */
static struct barrier_info* barrier_get(const Addr barrier)
{
  tl_assert(offsetof(DrdClientobj, barrier) == 0);
  return &clientobj_get(barrier, ClientBarrier)->barrier;
}

/** Initialize a barrier with client address barrier, client size size, and
 *  where count threads participate in each barrier.
 *  Called before pthread_barrier_init().
 */
void barrier_init(const Addr barrier,
                  const BarrierT barrier_type, const Word count,
                  const Bool reinitialization)
{
  struct barrier_info* p;

  tl_assert(barrier_type == pthread_barrier || barrier_type == gomp_barrier);

  if (count == 0)
  {
    BarrierErrInfo bei = { barrier };
    VG_(maybe_record_error)(VG_(get_running_tid)(),
                            BarrierErr,
                            VG_(get_IP)(VG_(get_running_tid)()),
                            "pthread_barrier_init: 'count' argument is zero",
                            &bei);
  }

  if (! reinitialization && barrier_type == pthread_barrier)
  {
    p = barrier_get(barrier);
    if (p)
    {
      BarrierErrInfo bei = { barrier };
      VG_(maybe_record_error)(VG_(get_running_tid)(),
                              BarrierErr,
                              VG_(get_IP)(VG_(get_running_tid)()),
                              "Barrier reinitialization",
                              &bei);
    }
  }
  p = barrier_get_or_allocate(barrier, barrier_type, count);

  if (s_trace_barrier)
  {
    if (reinitialization)
    {
      VG_(message)(Vg_UserMsg,
                   "[%d/%d] barrier_reinit    %s 0x%lx count %ld -> %ld",
                   VG_(get_running_tid)(),
                   thread_get_running_tid(),
                   barrier_get_typename(p),
                   barrier,
                   p->count,
                   count);
    }
    else
    {
      VG_(message)(Vg_UserMsg,
                   "[%d/%d] barrier_init      %s 0x%lx",
                   VG_(get_running_tid)(),
                   thread_get_running_tid(),
                   barrier_get_typename(p),
                   barrier);
    }
  }

  if (reinitialization && p->count != count)
  {
    if (p->pre_waiters_left != p->count || p->post_waiters_left != p->count)
    {
      BarrierErrInfo bei = { p->a1 };
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

/** Called after pthread_barrier_destroy(). */
void barrier_destroy(const Addr barrier, const BarrierT barrier_type)
{
  struct barrier_info* p;

  p = barrier_get(barrier);

  if (s_trace_barrier)
  {
    VG_(message)(Vg_UserMsg,
                 "[%d/%d] barrier_destroy   %s 0x%lx",
                 VG_(get_running_tid)(),
                 thread_get_running_tid(),
                 barrier_get_typename(p),
                 barrier);
  }

  if (p == 0)
  {
    GenericErrInfo GEI;
    VG_(maybe_record_error)(VG_(get_running_tid)(),
                            GenericErr,
                            VG_(get_IP)(VG_(get_running_tid)()),
                            "Not a barrier",
                            &GEI);
    return;
  }

  if (p->pre_waiters_left != p->count || p->post_waiters_left != p->count)
  {
    BarrierErrInfo bei = { p->a1 };
    VG_(maybe_record_error)(VG_(get_running_tid)(),
                            BarrierErr,
                            VG_(get_IP)(VG_(get_running_tid)()),
                            "Destruction of a barrier with active waiters",
                            &bei);
  }

  clientobj_remove(p->a1, ClientBarrier);
}

/** Called before pthread_barrier_wait(). */
void barrier_pre_wait(const DrdThreadId tid, const Addr barrier,
                      const BarrierT barrier_type)
{
  struct barrier_info* p;
  struct barrier_thread_info* q;
  const UWord word_tid = tid;

  p = barrier_get(barrier);
  if (p == 0 && barrier_type == gomp_barrier)
  {
    VG_(message)(Vg_UserMsg, "");
    VG_(message)(Vg_UserMsg,
                 "Please verify whether gcc has been configured"
                 " with option --disable-linux-futex.");
    VG_(message)(Vg_UserMsg,
                 "See also the section about OpenMP in the DRD manual.");
    VG_(message)(Vg_UserMsg, "");
  }
  tl_assert(p);

  if (s_trace_barrier)
  {
    VG_(message)(Vg_UserMsg,
                 "[%d/%d] barrier_pre_wait  %s 0x%lx iteration %ld",
                 VG_(get_running_tid)(),
                 thread_get_running_tid(),
                 barrier_get_typename(p),
                 barrier,
                 p->pre_iteration);
  }

  q = VG_(OSetGen_Lookup)(p->oset, &word_tid);
  if (q == 0)
  {
    q = VG_(OSetGen_AllocNode)(p->oset, sizeof(*q));
    barrier_thread_initialize(q, tid, p->pre_iteration);
    VG_(OSetGen_Insert)(p->oset, q);
    tl_assert(VG_(OSetGen_Lookup)(p->oset, &word_tid) == q);
  }
  thread_get_latest_segment(&q->sg[p->pre_iteration], tid);

  if (--p->pre_waiters_left <= 0)
  {
    p->pre_iteration    = 1 - p->pre_iteration;
    p->pre_waiters_left = p->count;
  }
}

/** Called after pthread_barrier_wait(). */
void barrier_post_wait(const DrdThreadId tid, const Addr barrier,
                       const BarrierT barrier_type, const Bool waited)
{
  struct barrier_info* p;

  p = barrier_get(barrier);

  if (s_trace_barrier)
  {
    VG_(message)(Vg_UserMsg,
                 "[%d/%d] barrier_post_wait %s 0x%lx iteration %ld",
                 VG_(get_running_tid)(),
                 tid,
                 p ? barrier_get_typename(p) : "(?)",
                 barrier,
                 p ? p->post_iteration : -1);
  }

  /* If p == 0, this means that the barrier has been destroyed after     */
  /* *_barrier_wait() returned and before this function was called. Just */
  /* return in that case.                                                */
  if (p == 0)
    return;

  if (waited)
  {
    const UWord word_tid = tid;
    struct barrier_thread_info* q;
    struct barrier_thread_info* r;

    q = VG_(OSetGen_Lookup)(p->oset, &word_tid);
    if (q == 0)
    {
      BarrierErrInfo bei = { p->a1 };
      VG_(maybe_record_error)(VG_(get_running_tid)(),
                              BarrierErr,
                              VG_(get_IP)(VG_(get_running_tid)()),
                              "Error in barrier implementation"
                              " -- barrier_wait() started before"
                              " barrier_destroy() and finished after"
                              " barrier_destroy()",
                              &bei);

      q = VG_(OSetGen_AllocNode)(p->oset, sizeof(*q));
      barrier_thread_initialize(q, tid, p->pre_iteration);
      VG_(OSetGen_Insert)(p->oset, q);
      tl_assert(VG_(OSetGen_Lookup)(p->oset, &word_tid) == q);
    }
    VG_(OSetGen_ResetIter)(p->oset);
    for ( ; (r = VG_(OSetGen_Next)(p->oset)) != 0; )
    {
      if (r != q)
      {
        tl_assert(r->sg[p->post_iteration]);
        thread_combine_vc2(tid, &r->sg[p->post_iteration]->vc);
      }
    }

    thread_new_segment(tid);
    s_barrier_segment_creation_count++;

    if (--p->post_waiters_left <= 0)
    {
      p->post_iteration    = 1 - p->post_iteration;
      p->post_waiters_left = p->count;
    }
  }
}

/** Call this function when thread tid stops to exist. */
void barrier_thread_delete(const DrdThreadId tid)
{
  struct barrier_info* p;

  clientobj_resetiter();
  for ( ; (p = &clientobj_next(ClientBarrier)->barrier) != 0; )
  {
    struct barrier_thread_info* q;
    const UWord word_tid = tid;
    q = VG_(OSetGen_Remove)(p->oset, &word_tid);
    /* q is only non-zero if the barrier object has been used by thread tid
     * after the barrier_init() call and before the thread finished.
     */
    if (q)
    {
      barrier_thread_destroy(q);
      VG_(OSetGen_FreeNode)(p->oset, q);
    }
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

ULong get_barrier_segment_creation_count(void)
{
  return s_barrier_segment_creation_count;
}
