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

struct barrier_thread_info
{
  UWord       tid;           // A DrdThreadId
  Word        iteration;     // barrier number corresponding to ongoing
                             // pthread_barrier() call modulo two.
  VectorClock vc[2];         // vector clocks corresponding to the last two
                             // pthread_barrier() calls.
};

struct barrier_info
{
  Addr  barrier;             // Client address of barrier.
  SizeT size;                // Size in bytes of client-side object.
  Word  count;               // Participant count in a barrier wait.
  Word  iteration;           // barrier number corresponding to ongoing
                             // pthread_barrier() call modulo two.
  Word  participants;        // Number of participants that still have to join
                             // the most recent barrier.
  OSet* oset;                // Information about specific threads.
};


// Local variables.

static Bool s_trace_barrier = False;
struct barrier_info s_barrier[4];


// Function definitions.

void barrier_set_trace(const Bool trace_barrier)
{
  s_trace_barrier = trace_barrier;
}

static void barrier_thread_initialize(struct barrier_thread_info* const p,
                                      const DrdThreadId tid,
                                      const Word iteration)
{
  p->tid = tid;
  p->iteration = iteration;
  vc_init(&p->vc[0], 0, 0);
  vc_init(&p->vc[1], 0, 0);
}

static void barrier_thread_destroy(struct barrier_thread_info* const p)
{
  vc_cleanup(&p->vc[0]);
  vc_cleanup(&p->vc[1]);
}

static
void barrier_initialize(struct barrier_info* const p,
                        const Addr barrier,
                        const SizeT size,
                        const Word count)
{
  tl_assert(barrier != 0);
  tl_assert(size > 0);
  tl_assert(count > 0);

  p->barrier = barrier;
  p->size    = size;
  p->count   = count;
  p->iteration = 0;
  p->participants = count;
  tl_assert(sizeof(((struct barrier_thread_info*)0)->tid) == sizeof(Word));
  tl_assert(sizeof(((struct barrier_thread_info*)0)->tid)
            >= sizeof(DrdThreadId));
  p->oset = VG_(OSetGen_Create)(0, 0, VG_(malloc), VG_(free));
}

void barrier_destroy(struct barrier_info* const p)
{
  struct barrier_thread_info* q;

  tl_assert(p);

  drd_finish_suppression(p->barrier, p->barrier + p->size);

  VG_(OSetGen_ResetIter)(p->oset);
  for ( ; (q = VG_(OSetGen_Next)(p->oset)) != 0; )
  {
    barrier_thread_destroy(q);
  }
  VG_(OSetGen_Destroy)(p->oset);
  p->barrier = 0;
  p->size = 0;
  p->count = 0;
  p->iteration = 0;
  p->participants = 0;
}

static
struct barrier_info*
barrier_get_or_allocate(const Addr barrier, const SizeT size, const Word count)
{
  int i;

  for (i = 0; i < sizeof(s_barrier)/sizeof(s_barrier[0]); i++)
  {
    if (s_barrier[i].barrier == barrier)
    {
      tl_assert(s_barrier[i].size == size);
      return &s_barrier[i];
    }
  }
  for (i = 0; i < sizeof(s_barrier)/sizeof(s_barrier[0]); i++)
  {
    if (s_barrier[i].barrier == 0)
    {
      barrier_initialize(&s_barrier[i], barrier, size, count);
      drd_start_suppression(barrier, barrier + size, "barrier");
      return &s_barrier[i];
    }
  }
  tl_assert(0);
  return 0;
}

struct barrier_info*
barrier_init(const Addr barrier, const SizeT size, const Word count)
{
  tl_assert(barrier_get(barrier) == 0);
  return barrier_get_or_allocate(barrier, size, count);
}

struct barrier_info* barrier_get(const Addr barrier)
{
  int i;
  for (i = 0; i < sizeof(s_barrier)/sizeof(s_barrier[0]); i++)
    if (s_barrier[i].barrier == barrier)
      return &s_barrier[i];
  return 0;
}

void barrier_pre_wait(const DrdThreadId tid, const Addr barrier)
{
  struct barrier_info* p;
  struct barrier_thread_info* q;
  const UWord word_tid = tid;

  p = barrier_get(barrier);
  tl_assert(p);

  if (s_trace_barrier)
  {
    VG_(message)(Vg_DebugMsg,
                 "[%d] barrier_pre_wait(%p) iteration %d / left %d/%d",
                 tid, barrier, p->iteration, p->participants, p->count);
  }

  if (--p->participants <= 0)
  {
    p->iteration    = ! p->iteration;
    p->participants = p->count;
  }
  q = VG_(OSetGen_Lookup)(p->oset, &word_tid);
  if (q == 0)
  {
    q = VG_(OSetGen_AllocNode)(p->oset, sizeof(*q));
    barrier_thread_initialize(q, tid, p->iteration);
    tl_assert(q->tid == tid);
    VG_(OSetGen_Insert)(p->oset, q);
    tl_assert(VG_(OSetGen_Lookup)(p->oset, &word_tid) == q);
  }
  tl_assert(VG_(OSetGen_Lookup)(p->oset, &word_tid) == q);
  vc_copy(&q->vc[p->iteration], &thread_get_segment(tid)->vc);
}

void barrier_post_wait(const DrdThreadId tid, const Addr barrier,
                       const Bool waited)
{
  struct barrier_info* const p = barrier_get(barrier);

  if (s_trace_barrier)
  {
    VG_(message)(Vg_DebugMsg, "[%d] barrier_post_wait(%p) iteration %d",
                 tid, barrier, p ? 1 - p->iteration : -1);
  }

  if (waited)
  {
    const UWord word_tid = tid;
    struct barrier_thread_info* q;
    struct barrier_thread_info* r;

    tl_assert(p);
    q = VG_(OSetGen_Lookup)(p->oset, &word_tid);
    tl_assert(q);
    VG_(OSetGen_ResetIter)(p->oset);
    for ( ; (r = VG_(OSetGen_Next)(p->oset)) != 0; )
    {
      if (r != q)
      {
        if (s_trace_barrier)
        {
          VG_(message)(Vg_DebugMsg,
                       "[%d] barrier_post_wait: combining vc of thread %d",
                       tid, r->tid);
        }
        thread_combine_vc2(tid, &r->vc[1 - q->iteration]);
      }
    }
  }
}

/**
 * Call this function when thread threadid stops to exist, such that the
 * "last owner" field can be cleared if it still refers to that thread.
 */
void barrier_thread_delete(const DrdThreadId tid)
{
  int i;
  struct barrier_thread_info* q;

  for (i = 0; i < sizeof(s_barrier)/sizeof(s_barrier[0]); i++)
  {
    struct barrier_info* const p = &s_barrier[i];
    if (p->barrier)
    {
      VG_(OSetGen_ResetIter)(p->oset);
      for ( ; (q = VG_(OSetGen_Next)(p->oset)) != 0; )
      {
      }
    }
  }
}

void barrier_stop_using_mem(const Addr a1, const Addr a2)
{
  unsigned i;
  for (i = 0; i < sizeof(s_barrier)/sizeof(s_barrier[0]); i++)
  {
    if (a1 <= s_barrier[i].barrier && s_barrier[i].barrier < a2)
    {
      tl_assert(s_barrier[i].barrier + s_barrier[i].size <= a2);
      barrier_destroy(&s_barrier[i]);
    }
  }
}
