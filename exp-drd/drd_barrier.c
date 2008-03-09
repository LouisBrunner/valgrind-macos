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
#include "priv_drd_clientreq.h"
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
  VectorClock vc[2];         // vector clocks corresponding to the last two
                             // pthread_barrier() calls by thread tid.
};


// Local functions.

void barrier_cleanup(struct barrier_info* p);


// Local variables.

static Bool s_trace_barrier = False;


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
  vc_init(&p->vc[0], 0, 0);
  vc_init(&p->vc[1], 0, 0);
}

/** Deallocate the memory that was allocated in barrier_thread_initialize(). */
static void barrier_thread_destroy(struct barrier_thread_info* const p)
{
  vc_cleanup(&p->vc[0]);
  vc_cleanup(&p->vc[1]);
}

/** Initialize the structure *p with the specified client-side barrier address,
 *  barrier object size and number of participants in each barrier. */
static
void barrier_initialize(struct barrier_info* const p,
                        const Addr barrier,
                        const SizeT size,
                        const Word count)
{
  tl_assert(barrier != 0);
  tl_assert(size > 0);
  tl_assert(count > 0);
  tl_assert(p->a1 == barrier);
  tl_assert(p->a2 - p->a1 == size);

  p->cleanup           = (void(*)(DrdClientobj*))barrier_cleanup;
  p->count             = count;
  p->pre_iteration     = 0;
  p->post_iteration    = 0;
  p->pre_waiters_left  = count;
  p->post_waiters_left = count;
  tl_assert(sizeof(((struct barrier_thread_info*)0)->tid) == sizeof(Word));
  tl_assert(sizeof(((struct barrier_thread_info*)0)->tid)
            >= sizeof(DrdThreadId));
  p->oset = VG_(OSetGen_Create)(0, 0, VG_(malloc), VG_(free));
  vc_init(&p->finished_threads_vc, 0, 0);
}

/** Deallocate the memory allocated by barrier_initialize() and in p->oset. 
 *  Called by clientobj_destroy().
 */
void barrier_cleanup(struct barrier_info* p)
{
  struct barrier_thread_info* q;

  tl_assert(p);

  if (p->pre_waiters_left != p->count || p->post_waiters_left != p->count)
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
  vc_cleanup(&p->finished_threads_vc);
}

/** Look up the client-side barrier address barrier in s_barrier[]. If not
 *  found, add it. */
static
struct barrier_info*
barrier_get_or_allocate(const Addr barrier, const SizeT size, const Word count)
{
  struct barrier_info *p;

  tl_assert(offsetof(DrdClientobj, barrier) == 0);
  p = &clientobj_get(barrier, ClientBarrier)->barrier;
  if (p == 0)
  {
    p = &clientobj_add(barrier, barrier + size, ClientBarrier)->barrier;
    barrier_initialize(p, barrier, size, count);
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
struct barrier_info*
barrier_init(const Addr barrier, const SizeT size, const Word count)
{
  if (s_trace_barrier)
  {
    VG_(message)(Vg_UserMsg,
                 "[%d/%d] barrier_init 0x%lx",
                 VG_(get_running_tid)(),
                 thread_get_running_tid(),
                 barrier);
  }
  tl_assert(barrier_get(barrier) == 0);
  return barrier_get_or_allocate(barrier, size, count);
}

/** Called after pthread_barrier_destroy(). */
void barrier_destroy(const Addr barrier)
{
  struct barrier_info* p;

  if (s_trace_barrier)
  {
    VG_(message)(Vg_UserMsg,
                 "[%d/%d] barrier_destroy 0x%lx",
                 VG_(get_running_tid)(),
                 thread_get_running_tid(),
                 barrier);
  }

  p = barrier_get(barrier);
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

  clientobj_remove(p->a1, ClientBarrier);
}

/** Called before pthread_barrier_wait(). */
void barrier_pre_wait(const DrdThreadId tid, const Addr barrier)
{
  struct barrier_info* p;
  struct barrier_thread_info* q;
  const UWord word_tid = tid;

  p = barrier_get(barrier);
  tl_assert(p);

  if (s_trace_barrier)
  {
    VG_(message)(Vg_UserMsg,
                 "[%d/%d] barrier_pre_wait 0x%lx iteration %d",
                 VG_(get_running_tid)(),
                 thread_get_running_tid(),
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
  vc_assign(&q->vc[p->pre_iteration], &thread_get_segment(tid)->vc);
  tl_assert(q->vc[p->pre_iteration].size > 0);

  if (--p->pre_waiters_left <= 0)
  {
    p->pre_iteration    = 1 - p->pre_iteration;
    p->pre_waiters_left = p->count;
  }
}

/** Called after pthread_barrier_wait(). */
void barrier_post_wait(const DrdThreadId tid, const Addr barrier,
                       const Bool waited)
{
  struct barrier_info* p;

  p = barrier_get(barrier);
  tl_assert(p);

  if (s_trace_barrier)
  {
    VG_(message)(Vg_UserMsg,
                 "[%d/%d] barrier_post_wait 0x%lx iteration %d",
                 VG_(get_running_tid)(),
                 tid,
                 barrier,
                 p->post_iteration);
  }

  if (waited)
  {
    const UWord word_tid = tid;
    struct barrier_thread_info* q;
    struct barrier_thread_info* r;

    q = VG_(OSetGen_Lookup)(p->oset, &word_tid);
    tl_assert(q);
    VG_(OSetGen_ResetIter)(p->oset);
    for ( ; (r = VG_(OSetGen_Next)(p->oset)) != 0; )
    {
      if (r != q)
      {
        thread_combine_vc2(tid, &r->vc[p->post_iteration]);
      }
    }
    thread_combine_vc2(tid, &p->finished_threads_vc);

    thread_new_segment(tid);

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
    vc_combine(&p->finished_threads_vc, &q->vc[p->post_iteration]);
    barrier_thread_destroy(q);
    VG_(OSetGen_FreeNode)(p->oset, q);
  }
}
