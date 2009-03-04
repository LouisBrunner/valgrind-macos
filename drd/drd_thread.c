/*
  This file is part of drd, a thread error detector.

  Copyright (C) 2006-2009 Bart Van Assche <bart.vanassche@gmail.com>.

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


#include "drd_error.h"
#include "drd_barrier.h"
#include "drd_clientobj.h"
#include "drd_cond.h"
#include "drd_mutex.h"
#include "drd_segment.h"
#include "drd_semaphore.h"
#include "drd_suppression.h"
#include "drd_thread.h"
#include "pub_tool_vki.h"
#include "pub_tool_basics.h"      // Addr, SizeT
#include "pub_tool_errormgr.h"    // VG_(unique_error)()
#include "pub_tool_libcassert.h"  // tl_assert()
#include "pub_tool_libcbase.h"    // VG_(strlen)()
#include "pub_tool_libcprint.h"   // VG_(printf)()
#include "pub_tool_libcproc.h"    // VG_(getenv)()
#include "pub_tool_machine.h"
#include "pub_tool_mallocfree.h"  // VG_(malloc)(), VG_(free)()
#include "pub_tool_options.h"     // VG_(clo_backtrace_size)
#include "pub_tool_threadstate.h" // VG_(get_pthread_id)()



/* Local functions. */

static void thread_append_segment(const DrdThreadId tid, Segment* const sg);
static void thread_discard_segment(const DrdThreadId tid, Segment* const sg);
static void thread_compute_conflict_set(struct bitmap** conflict_set,
                                        const DrdThreadId tid);


/* Local variables. */

static ULong    s_context_switch_count;
static ULong    s_discard_ordered_segments_count;
static ULong    s_update_conflict_set_count;
static ULong    s_conflict_set_new_segment_count;
static ULong    s_conflict_set_combine_vc_count;
static ULong    s_conflict_set_bitmap_creation_count;
static ULong    s_conflict_set_bitmap2_creation_count;
static ThreadId s_vg_running_tid  = VG_INVALID_THREADID;
DrdThreadId     DRD_(g_drd_running_tid) = DRD_INVALID_THREADID;
ThreadInfo      DRD_(g_threadinfo)[DRD_N_THREADS];
struct bitmap*  DRD_(g_conflict_set);
static Bool     s_trace_context_switches = False;
static Bool     s_trace_conflict_set = False;
static Bool     s_trace_fork_join = False;
static Bool     s_segment_merging = True;


/* Function definitions. */

/** Enables/disables context switch tracing. */
void DRD_(thread_trace_context_switches)(const Bool t)
{
  tl_assert(t == False || t == True);
  s_trace_context_switches = t;
}

/** Enables/disables conflict set tracing. */
void DRD_(thread_trace_conflict_set)(const Bool t)
{
  tl_assert(t == False || t == True);
  s_trace_conflict_set = t;
}

/** Report whether fork/join tracing is enabled. */
Bool DRD_(thread_get_trace_fork_join)(void)
{
  return s_trace_fork_join;
}

/** Enables/disables fork/join tracing. */
void DRD_(thread_set_trace_fork_join)(const Bool t)
{
  tl_assert(t == False || t == True);
  s_trace_fork_join = t;
}

/** Enables/disables segment merging. */
void DRD_(thread_set_segment_merging)(const Bool m)
{
  tl_assert(m == False || m == True);
  s_segment_merging = m;
}

/**
 * Convert Valgrind's ThreadId into a DrdThreadId.
 *
 * @return DRD thread ID upon success and DRD_INVALID_THREADID if the passed
 *         Valgrind ThreadId does not yet exist.
 */
DrdThreadId DRD_(VgThreadIdToDrdThreadId)(const ThreadId tid)
{
  int i;

  if (tid == VG_INVALID_THREADID)
    return DRD_INVALID_THREADID;

  for (i = 1; i < DRD_N_THREADS; i++)
  {
    if (DRD_(g_threadinfo)[i].vg_thread_exists == True
        && DRD_(g_threadinfo)[i].vg_threadid == tid)
    {
      return i;
    }
  }

  return DRD_INVALID_THREADID;
}

/** Allocate a new DRD thread ID for the specified Valgrind thread ID. */
static DrdThreadId DRD_(VgThreadIdToNewDrdThreadId)(const ThreadId tid)
{
  int i;

  tl_assert(DRD_(VgThreadIdToDrdThreadId)(tid) == DRD_INVALID_THREADID);

  for (i = 1; i < DRD_N_THREADS; i++)
  {
    if (DRD_(g_threadinfo)[i].vg_thread_exists == False
        && DRD_(g_threadinfo)[i].posix_thread_exists == False
        && DRD_(g_threadinfo)[i].detached_posix_thread == False)
    {
      tl_assert(! DRD_(IsValidDrdThreadId)(i));

      DRD_(g_threadinfo)[i].vg_thread_exists = True;
      DRD_(g_threadinfo)[i].vg_threadid   = tid;
      DRD_(g_threadinfo)[i].pt_threadid   = INVALID_POSIX_THREADID;
      DRD_(g_threadinfo)[i].stack_min     = 0;
      DRD_(g_threadinfo)[i].stack_min_min = 0;
      DRD_(g_threadinfo)[i].stack_startup = 0;
      DRD_(g_threadinfo)[i].stack_max     = 0;
      DRD_(g_threadinfo)[i].is_recording  = True;
      DRD_(g_threadinfo)[i].synchr_nesting = 0;
      tl_assert(DRD_(g_threadinfo)[i].first == 0);
      tl_assert(DRD_(g_threadinfo)[i].last == 0);

      tl_assert(DRD_(IsValidDrdThreadId)(i));

      return i;
    }
  }

  tl_assert(False);

  return DRD_INVALID_THREADID;
}

/** Convert a POSIX thread ID into a DRD thread ID. */
DrdThreadId DRD_(PtThreadIdToDrdThreadId)(const PThreadId tid)
{
  int i;

  tl_assert(tid != INVALID_POSIX_THREADID);

  for (i = 1; i < DRD_N_THREADS; i++)
  {
    if (DRD_(g_threadinfo)[i].posix_thread_exists
        && DRD_(g_threadinfo)[i].pt_threadid == tid)
    {
      return i;
    }
  }
  return DRD_INVALID_THREADID;
}

/** Convert a DRD thread ID into a Valgrind thread ID. */
ThreadId DRD_(DrdThreadIdToVgThreadId)(const DrdThreadId tid)
{
  tl_assert(0 <= (int)tid && tid < DRD_N_THREADS
            && tid != DRD_INVALID_THREADID);
  return (DRD_(g_threadinfo)[tid].vg_thread_exists
          ? DRD_(g_threadinfo)[tid].vg_threadid
          : VG_INVALID_THREADID);
}

#if 0
/**
 * Sanity check of the doubly linked list of segments referenced by a
 * ThreadInfo struct.
 * @return True if sane, False if not.
 */
static Bool DRD_(sane_ThreadInfo)(const ThreadInfo* const ti)
{
  Segment* p;
  for (p = ti->first; p; p = p->next) {
    if (p->next && p->next->prev != p)
      return False;
    if (p->next == 0 && p != ti->last)
      return False;
  }
  for (p = ti->last; p; p = p->prev) {
    if (p->prev && p->prev->next != p)
      return False;
    if (p->prev == 0 && p != ti->first)
      return False;
  }
  return True;
}
#endif

/**
 * Create the first segment for a newly started thread.
 *
 * This function is called from the handler installed via
 * VG_(track_pre_thread_ll_create)(). The Valgrind core invokes this handler
 * from the context of the creator thread, before the new thread has been
 * created.
 *
 * @param[in] creator    DRD thread ID of the creator thread.
 * @param[in] vg_created Valgrind thread ID of the created thread.
 *
 * @return DRD thread ID of the created thread.
 */
DrdThreadId DRD_(thread_pre_create)(const DrdThreadId creator,
                                    const ThreadId vg_created)
{
  DrdThreadId created;

  tl_assert(DRD_(VgThreadIdToDrdThreadId)(vg_created) == DRD_INVALID_THREADID);
  created = DRD_(VgThreadIdToNewDrdThreadId)(vg_created);
  tl_assert(0 <= (int)created && created < DRD_N_THREADS
            && created != DRD_INVALID_THREADID);

  tl_assert(DRD_(g_threadinfo)[created].first == 0);
  tl_assert(DRD_(g_threadinfo)[created].last == 0);
  thread_append_segment(created, DRD_(sg_new)(creator, created));

  return created;
}

/**
 * Initialize DRD_(g_threadinfo)[] for a newly created thread. Must be called
 * after the thread has been created and before any client instructions are run
 * on the newly created thread, e.g. from the handler installed via
 * VG_(track_pre_thread_first_insn)().
 *
 * @param[in] vg_created Valgrind thread ID of the newly created thread.
 *
 * @return DRD thread ID for the new thread.
 */
DrdThreadId DRD_(thread_post_create)(const ThreadId vg_created)
{
  const DrdThreadId created = DRD_(VgThreadIdToDrdThreadId)(vg_created);

  tl_assert(0 <= (int)created && created < DRD_N_THREADS
            && created != DRD_INVALID_THREADID);

  DRD_(g_threadinfo)[created].stack_max     = VG_(thread_get_stack_max)(vg_created);
  DRD_(g_threadinfo)[created].stack_startup = DRD_(g_threadinfo)[created].stack_max;
  DRD_(g_threadinfo)[created].stack_min     = DRD_(g_threadinfo)[created].stack_max;
  DRD_(g_threadinfo)[created].stack_min_min = DRD_(g_threadinfo)[created].stack_max;
  DRD_(g_threadinfo)[created].stack_size    = VG_(thread_get_stack_size)(vg_created);
  tl_assert(DRD_(g_threadinfo)[created].stack_max != 0);

  return created;
}

/**
 * Process VG_USERREQ__POST_THREAD_JOIN. This client request is invoked just
 * after thread drd_joiner joined thread drd_joinee.
 */
void DRD_(thread_post_join)(DrdThreadId drd_joiner, DrdThreadId drd_joinee)
{
  tl_assert(DRD_(IsValidDrdThreadId)(drd_joiner));
  tl_assert(DRD_(IsValidDrdThreadId)(drd_joinee));
  DRD_(thread_new_segment)(drd_joinee);
  DRD_(thread_combine_vc)(drd_joiner, drd_joinee);
  DRD_(thread_new_segment)(drd_joiner);

  if (s_trace_fork_join)
  {
    const ThreadId joiner = DRD_(DrdThreadIdToVgThreadId)(drd_joiner);
    const ThreadId joinee = DRD_(DrdThreadIdToVgThreadId)(drd_joinee);
    const unsigned msg_size = 256;
    char* msg;

    msg = VG_(malloc)("drd.main.dptj.1", msg_size);
    tl_assert(msg);
    VG_(snprintf)(msg, msg_size,
                  "drd_post_thread_join joiner = %d/%d, joinee = %d/%d",
                  joiner, drd_joiner, joinee, drd_joinee);
    if (joiner)
    {
      VG_(snprintf)(msg + VG_(strlen)(msg), msg_size - VG_(strlen)(msg),
                    ", new vc: ");
      DRD_(vc_snprint)(msg + VG_(strlen)(msg), msg_size - VG_(strlen)(msg),
                       DRD_(thread_get_vc)(drd_joiner));
    }
    VG_(message)(Vg_DebugMsg, "%s", msg);
    VG_(free)(msg);
  }

  if (!  DRD_(get_check_stack_accesses)())
  {
    DRD_(finish_suppression)(DRD_(thread_get_stack_max)(drd_joinee)
                             - DRD_(thread_get_stack_size)(drd_joinee),
                             DRD_(thread_get_stack_max)(drd_joinee));
  }
  DRD_(clientobj_delete_thread)(drd_joinee);
  DRD_(thread_delete)(drd_joinee);
}

/**
 * NPTL hack: NPTL allocates the 'struct pthread' on top of the stack,
 * and accesses this data structure from multiple threads without locking.
 * Any conflicting accesses in the range stack_startup..stack_max will be
 * ignored.
 */
void DRD_(thread_set_stack_startup)(const DrdThreadId tid,
                                    const Addr stack_startup)
{
  tl_assert(0 <= (int)tid && tid < DRD_N_THREADS
            && tid != DRD_INVALID_THREADID);
  tl_assert(DRD_(g_threadinfo)[tid].stack_min <= stack_startup);
  tl_assert(stack_startup <= DRD_(g_threadinfo)[tid].stack_max);
  DRD_(g_threadinfo)[tid].stack_startup = stack_startup;
}

/** Return the stack pointer for the specified thread. */
Addr DRD_(thread_get_stack_min)(const DrdThreadId tid)
{
  tl_assert(0 <= (int)tid && tid < DRD_N_THREADS
            && tid != DRD_INVALID_THREADID);
  return DRD_(g_threadinfo)[tid].stack_min;
}

/**
 * Return the lowest value that was ever assigned to the stack pointer
 * for the specified thread.
 */
Addr DRD_(thread_get_stack_min_min)(const DrdThreadId tid)
{
  tl_assert(0 <= (int)tid && tid < DRD_N_THREADS
            && tid != DRD_INVALID_THREADID);
  return DRD_(g_threadinfo)[tid].stack_min_min;
}

/** Return the top address for the stack of the specified thread. */
Addr DRD_(thread_get_stack_max)(const DrdThreadId tid)
{
  tl_assert(0 <= (int)tid && tid < DRD_N_THREADS
            && tid != DRD_INVALID_THREADID);
  return DRD_(g_threadinfo)[tid].stack_max;
}

/** Return the maximum stack size for the specified thread. */
SizeT DRD_(thread_get_stack_size)(const DrdThreadId tid)
{
  tl_assert(0 <= (int)tid && tid < DRD_N_THREADS
            && tid != DRD_INVALID_THREADID);
  return DRD_(g_threadinfo)[tid].stack_size;
}

/**
 * Clean up thread-specific data structures. Call this just after 
 * pthread_join().
 */
void DRD_(thread_delete)(const DrdThreadId tid)
{
  Segment* sg;
  Segment* sg_prev;

  tl_assert(DRD_(IsValidDrdThreadId)(tid));

  tl_assert(DRD_(g_threadinfo)[tid].synchr_nesting >= 0);
  for (sg = DRD_(g_threadinfo)[tid].last; sg; sg = sg_prev)
  {
    sg_prev = sg->prev;
    sg->prev = 0;
    sg->next = 0;
    DRD_(sg_put)(sg);
  }
  DRD_(g_threadinfo)[tid].vg_thread_exists = False;
  DRD_(g_threadinfo)[tid].posix_thread_exists = False;
  tl_assert(DRD_(g_threadinfo)[tid].detached_posix_thread == False);
  DRD_(g_threadinfo)[tid].first = 0;
  DRD_(g_threadinfo)[tid].last = 0;

  tl_assert(! DRD_(IsValidDrdThreadId)(tid));
}

/**
 * Called after a thread performed its last memory access and before
 * thread_delete() is called. Note: thread_delete() is only called for
 * joinable threads, not for detached threads.
 */
void DRD_(thread_finished)(const DrdThreadId tid)
{
  tl_assert(0 <= (int)tid && tid < DRD_N_THREADS
            && tid != DRD_INVALID_THREADID);

  DRD_(g_threadinfo)[tid].vg_thread_exists = False;

  if (DRD_(g_threadinfo)[tid].detached_posix_thread)
  {
    /* Once a detached thread has finished, its stack is deallocated and   */
    /* should no longer be taken into account when computing the conflict set*/
    DRD_(g_threadinfo)[tid].stack_min = DRD_(g_threadinfo)[tid].stack_max;

    /* For a detached thread, calling pthread_exit() invalidates the     */
    /* POSIX thread ID associated with the detached thread. For joinable */
    /* POSIX threads however, the POSIX thread ID remains live after the */
    /* pthread_exit() call until pthread_join() is called.               */
    DRD_(g_threadinfo)[tid].posix_thread_exists = False;
  }
}

/** Called just before pthread_cancel(). */
void DRD_(thread_pre_cancel)(const DrdThreadId tid)
{
  tl_assert(0 <= (int)tid && tid < DRD_N_THREADS
            && tid != DRD_INVALID_THREADID);
  tl_assert(DRD_(g_threadinfo)[tid].pt_threadid != INVALID_POSIX_THREADID);

  DRD_(g_threadinfo)[tid].synchr_nesting = 0;
}

/** Store the POSIX thread ID for the specified thread. */
void DRD_(thread_set_pthreadid)(const DrdThreadId tid, const PThreadId ptid)
{
  tl_assert(0 <= (int)tid && tid < DRD_N_THREADS
            && tid != DRD_INVALID_THREADID);
  tl_assert(DRD_(g_threadinfo)[tid].pt_threadid == INVALID_POSIX_THREADID);
  tl_assert(ptid != INVALID_POSIX_THREADID);
  DRD_(g_threadinfo)[tid].posix_thread_exists = True;
  DRD_(g_threadinfo)[tid].pt_threadid         = ptid;
}

/** Returns true for joinable threads and false for detached threads. */
Bool DRD_(thread_get_joinable)(const DrdThreadId tid)
{
  tl_assert(0 <= (int)tid && tid < DRD_N_THREADS
            && tid != DRD_INVALID_THREADID);
  return ! DRD_(g_threadinfo)[tid].detached_posix_thread;
}

/** Store the thread mode: joinable or detached. */
void DRD_(thread_set_joinable)(const DrdThreadId tid, const Bool joinable)
{
  tl_assert(0 <= (int)tid && tid < DRD_N_THREADS
            && tid != DRD_INVALID_THREADID);
  tl_assert(!! joinable == joinable);
  tl_assert(DRD_(g_threadinfo)[tid].pt_threadid != INVALID_POSIX_THREADID);
#if 0
  VG_(message)(Vg_DebugMsg,
               "thread_set_joinable(%d/%d, %s)",
               tid,
               DRD_(g_threadinfo)[tid].vg_threadid,
               joinable ? "joinable" : "detached");
#endif
  DRD_(g_threadinfo)[tid].detached_posix_thread = ! joinable;
}

/**
 * Update s_vg_running_tid, DRD_(g_drd_running_tid) and recalculate the
 * conflict set.
 */
void DRD_(thread_set_vg_running_tid)(const ThreadId vg_tid)
{
  tl_assert(vg_tid != VG_INVALID_THREADID);

  if (vg_tid != s_vg_running_tid)
  {
    DRD_(thread_set_running_tid)(vg_tid,
                                 DRD_(VgThreadIdToDrdThreadId)(vg_tid));
  }

  tl_assert(s_vg_running_tid != VG_INVALID_THREADID);
  tl_assert(DRD_(g_drd_running_tid) != DRD_INVALID_THREADID);
}

/**
 * Update s_vg_running_tid, DRD_(g_drd_running_tid) and recalculate the
 * conflict set.
 */
void DRD_(thread_set_running_tid)(const ThreadId vg_tid,
                                  const DrdThreadId drd_tid)
{
  tl_assert(vg_tid != VG_INVALID_THREADID);
  tl_assert(drd_tid != DRD_INVALID_THREADID);
   
  if (vg_tid != s_vg_running_tid)
  {
    if (s_trace_context_switches
        && DRD_(g_drd_running_tid) != DRD_INVALID_THREADID)
    {
      VG_(message)(Vg_DebugMsg,
                   "Context switch from thread %d/%d to thread %d/%d;"
                   " segments: %llu",
                   s_vg_running_tid, DRD_(g_drd_running_tid),
                   DRD_(DrdThreadIdToVgThreadId)(drd_tid), drd_tid,
                   DRD_(sg_get_segments_alive_count)());
    }
    s_vg_running_tid = vg_tid;
    DRD_(g_drd_running_tid) = drd_tid;
    thread_compute_conflict_set(&DRD_(g_conflict_set), drd_tid);
    s_context_switch_count++;
  }

  tl_assert(s_vg_running_tid != VG_INVALID_THREADID);
  tl_assert(DRD_(g_drd_running_tid) != DRD_INVALID_THREADID);
}

/**
 * Increase the synchronization nesting counter. Must be called before the
 * client calls a synchronization function.
 */
int DRD_(thread_enter_synchr)(const DrdThreadId tid)
{
  tl_assert(DRD_(IsValidDrdThreadId)(tid));
  return DRD_(g_threadinfo)[tid].synchr_nesting++;
}

/**
 * Decrease the synchronization nesting counter. Must be called after the
 * client left a synchronization function.
 */
int DRD_(thread_leave_synchr)(const DrdThreadId tid)
{
  tl_assert(DRD_(IsValidDrdThreadId)(tid));
  tl_assert(DRD_(g_threadinfo)[tid].synchr_nesting >= 1);
  return --DRD_(g_threadinfo)[tid].synchr_nesting;
}

/** Returns the synchronization nesting counter. */
int DRD_(thread_get_synchr_nesting_count)(const DrdThreadId tid)
{
  tl_assert(DRD_(IsValidDrdThreadId)(tid));
  return DRD_(g_threadinfo)[tid].synchr_nesting;
}

/** Append a new segment at the end of the segment list. */
static
void thread_append_segment(const DrdThreadId tid, Segment* const sg)
{
  tl_assert(0 <= (int)tid && tid < DRD_N_THREADS
            && tid != DRD_INVALID_THREADID);
  // tl_assert(DRD_(sane_ThreadInfo)(&DRD_(g_threadinfo)[tid]));
  sg->prev = DRD_(g_threadinfo)[tid].last;
  sg->next = 0;
  if (DRD_(g_threadinfo)[tid].last)
    DRD_(g_threadinfo)[tid].last->next = sg;
  DRD_(g_threadinfo)[tid].last = sg;
  if (DRD_(g_threadinfo)[tid].first == 0)
    DRD_(g_threadinfo)[tid].first = sg;
  // tl_assert(DRD_(sane_ThreadInfo)(&DRD_(g_threadinfo)[tid]));
}

/**
 * Remove a segment from the segment list of thread threadid, and free the
 * associated memory.
 */
static
void thread_discard_segment(const DrdThreadId tid, Segment* const sg)
{
  tl_assert(0 <= (int)tid && tid < DRD_N_THREADS
            && tid != DRD_INVALID_THREADID);
  //tl_assert(DRD_(sane_ThreadInfo)(&DRD_(g_threadinfo)[tid]));

  if (sg->prev)
    sg->prev->next = sg->next;
  if (sg->next)
    sg->next->prev = sg->prev;
  if (sg == DRD_(g_threadinfo)[tid].first)
    DRD_(g_threadinfo)[tid].first = sg->next;
  if (sg == DRD_(g_threadinfo)[tid].last)
    DRD_(g_threadinfo)[tid].last = sg->prev;
  DRD_(sg_put)(sg);

  //tl_assert(DRD_(sane_ThreadInfo)(&DRD_(g_threadinfo)[tid]));
}

/**
 * Returns a pointer to the vector clock of the most recent segment associated
 * with thread 'tid'.
 */
VectorClock* DRD_(thread_get_vc)(const DrdThreadId tid)
{
  tl_assert(0 <= (int)tid && tid < DRD_N_THREADS
            && tid != DRD_INVALID_THREADID);
  tl_assert(DRD_(g_threadinfo)[tid].last);
  return &DRD_(g_threadinfo)[tid].last->vc;
}

/**
 * Return the latest segment of thread 'tid' and increment its reference count.
 */
void DRD_(thread_get_latest_segment)(Segment** sg, const DrdThreadId tid)
{
  tl_assert(sg);
  tl_assert(0 <= (int)tid && tid < DRD_N_THREADS
            && tid != DRD_INVALID_THREADID);
  tl_assert(DRD_(g_threadinfo)[tid].last);

  DRD_(sg_put)(*sg);
  *sg = DRD_(sg_get)(DRD_(g_threadinfo)[tid].last);
}

/**
 * Compute the minimum of all latest vector clocks of all threads
 * (Michiel Ronsse calls this "clock snooping" in his papers about DIOTA).
 *
 * @param vc pointer to a vectorclock, holds result upon return.
 */
static void DRD_(thread_compute_minimum_vc)(VectorClock* vc)
{
  unsigned i;
  Bool first;
  Segment* latest_sg;

  first = True;
  for (i = 0; i < sizeof(DRD_(g_threadinfo)) / sizeof(DRD_(g_threadinfo)[0]);
       i++)
  {
    latest_sg = DRD_(g_threadinfo)[i].last;
    if (latest_sg)
    {
      if (first)
        DRD_(vc_assign)(vc, &latest_sg->vc);
      else
        DRD_(vc_min)(vc, &latest_sg->vc);
      first = False;
    }
  }
}

/**
 * Compute the maximum of all latest vector clocks of all threads.
 *
 * @param vc pointer to a vectorclock, holds result upon return.
 */
static void DRD_(thread_compute_maximum_vc)(VectorClock* vc)
{
  unsigned i;
  Bool first;
  Segment* latest_sg;

  first = True;
  for (i = 0; i < sizeof(DRD_(g_threadinfo)) / sizeof(DRD_(g_threadinfo)[0]);
       i++)
  {
    latest_sg = DRD_(g_threadinfo)[i].last;
    if (latest_sg)
    {
      if (first)
        DRD_(vc_assign)(vc, &latest_sg->vc);
      else
        DRD_(vc_combine)(vc, &latest_sg->vc);
      first = False;
    }
  }
}

/**
 * Discard all segments that have a defined order against the latest vector
 * clock of all threads -- these segments can no longer be involved in a
 * data race.
 */
static void DRD_(thread_discard_ordered_segments)(void)
{
  unsigned i;
  VectorClock thread_vc_min;

  s_discard_ordered_segments_count++;

  DRD_(vc_init)(&thread_vc_min, 0, 0);
  DRD_(thread_compute_minimum_vc)(&thread_vc_min);
  if (DRD_(sg_get_trace)())
  {
    char msg[256];
    VectorClock thread_vc_max;

    DRD_(vc_init)(&thread_vc_max, 0, 0);
    DRD_(thread_compute_maximum_vc)(&thread_vc_max);
    VG_(snprintf)(msg, sizeof(msg),
                  "Discarding ordered segments -- min vc is ");
    DRD_(vc_snprint)(msg + VG_(strlen)(msg), sizeof(msg) - VG_(strlen)(msg),
                     &thread_vc_min);
    VG_(snprintf)(msg + VG_(strlen)(msg), sizeof(msg) - VG_(strlen)(msg),
                  ", max vc is ");
    DRD_(vc_snprint)(msg + VG_(strlen)(msg), sizeof(msg) - VG_(strlen)(msg),
                     &thread_vc_max);
    VG_(message)(Vg_UserMsg, "%s", msg);
    DRD_(vc_cleanup)(&thread_vc_max);
  }

  for (i = 0; i < sizeof(DRD_(g_threadinfo)) / sizeof(DRD_(g_threadinfo)[0]);
       i++)
  {
    Segment* sg;
    Segment* sg_next;
    for (sg = DRD_(g_threadinfo)[i].first;
         sg && (sg_next = sg->next) && DRD_(vc_lte)(&sg->vc, &thread_vc_min);
         sg = sg_next)
    {
      thread_discard_segment(i, sg);
    }
  }
  DRD_(vc_cleanup)(&thread_vc_min);
}

/**
 * Merge all segments that may be merged without triggering false positives
 * or discarding real data races. For the theoretical background of segment
 * merging, see also the following paper:
 * Mark Christiaens, Michiel Ronsse and Koen De Bosschere.
 * Bounding the number of segment histories during data race detection.
 * Parallel Computing archive, Volume 28, Issue 9, pp 1221-1238,
 * September 2002.
 */
static void thread_merge_segments(void)
{
  unsigned i;

  for (i = 0; i < sizeof(DRD_(g_threadinfo)) / sizeof(DRD_(g_threadinfo)[0]);
       i++)
  {
    Segment* sg;

    // tl_assert(DRD_(sane_ThreadInfo)(&DRD_(g_threadinfo)[i]));

    for (sg = DRD_(g_threadinfo)[i].first; sg; sg = sg->next)
    {
      if (DRD_(sg_get_refcnt)(sg) == 1
          && sg->next
          && DRD_(sg_get_refcnt)(sg->next) == 1
          && sg->next->next)
      {
        /* Merge sg and sg->next into sg. */
        DRD_(sg_merge)(sg, sg->next);
        thread_discard_segment(i, sg->next);
      }
    }

    // tl_assert(DRD_(sane_ThreadInfo)(&DRD_(g_threadinfo)[i]));
  }
}

/**
 * Create a new segment for the specified thread, and discard any segments
 * that cannot cause races anymore.
 */
void DRD_(thread_new_segment)(const DrdThreadId tid)
{
  Segment* new_sg;

  tl_assert(0 <= (int)tid && tid < DRD_N_THREADS
            && tid != DRD_INVALID_THREADID);

  new_sg = DRD_(sg_new)(tid, tid);
  thread_append_segment(tid, new_sg);

  thread_compute_conflict_set(&DRD_(g_conflict_set), DRD_(g_drd_running_tid));
  s_conflict_set_new_segment_count++;

  DRD_(thread_discard_ordered_segments)();

  if (s_segment_merging)
  {
    thread_merge_segments();
  }
}

/** Call this function after thread 'joiner' joined thread 'joinee'. */
void DRD_(thread_combine_vc)(DrdThreadId joiner, DrdThreadId joinee)
{
  tl_assert(joiner != joinee);
  tl_assert(0 <= (int)joiner && joiner < DRD_N_THREADS
            && joiner != DRD_INVALID_THREADID);
  tl_assert(0 <= (int)joinee && joinee < DRD_N_THREADS
            && joinee != DRD_INVALID_THREADID);
  tl_assert(DRD_(g_threadinfo)[joiner].last);
  tl_assert(DRD_(g_threadinfo)[joinee].last);
  DRD_(vc_combine)(&DRD_(g_threadinfo)[joiner].last->vc,
                   &DRD_(g_threadinfo)[joinee].last->vc);
  DRD_(thread_discard_ordered_segments)();

  if (joiner == DRD_(g_drd_running_tid))
  {
    thread_compute_conflict_set(&DRD_(g_conflict_set), joiner);
  }
}

/**
 * Call this function after thread 'tid' had to wait because of thread
 * synchronization until the memory accesses in the segment with vector clock
 * 'vc' finished.
 */
void DRD_(thread_combine_vc2)(DrdThreadId tid, const VectorClock* const vc)
{
  tl_assert(0 <= (int)tid && tid < DRD_N_THREADS
            && tid != DRD_INVALID_THREADID);
  tl_assert(DRD_(g_threadinfo)[tid].last);
  tl_assert(vc);
  DRD_(vc_combine)(&DRD_(g_threadinfo)[tid].last->vc, vc);
  thread_compute_conflict_set(&DRD_(g_conflict_set), tid);
  DRD_(thread_discard_ordered_segments)();
  s_conflict_set_combine_vc_count++;
}

/**
 * Call this function whenever a thread is no longer using the memory
 * [ a1, a2 [, e.g. because of a call to free() or a stack pointer
 * increase.
 */
void DRD_(thread_stop_using_mem)(const Addr a1, const Addr a2)
{
  DrdThreadId other_user;
  unsigned i;

  /* For all threads, mark the range [ a1, a2 [ as no longer in use. */
  other_user = DRD_INVALID_THREADID;
  for (i = 0; i < sizeof(DRD_(g_threadinfo)) / sizeof(DRD_(g_threadinfo)[0]);
       i++)
  {
    Segment* p;
    for (p = DRD_(g_threadinfo)[i].first; p; p = p->next)
    {
      if (other_user == DRD_INVALID_THREADID
          && i != DRD_(g_drd_running_tid))
      {
        if (UNLIKELY(DRD_(bm_test_and_clear)(p->bm, a1, a2)))
        {
          other_user = i;
        }
        continue;
      }
      DRD_(bm_clear)(p->bm, a1, a2);
    }
  }

  /*
   * If any other thread had accessed memory in [ a1, a2 [, update the
   * conflict set.
   */
  if (other_user != DRD_INVALID_THREADID
      && DRD_(bm_has_any_access)(DRD_(g_conflict_set), a1, a2))
  {
    thread_compute_conflict_set(&DRD_(g_conflict_set),
                                      DRD_(thread_get_running_tid)());
  }
}

/** Start recording memory access information. */
void DRD_(thread_start_recording)(const DrdThreadId tid)
{
  tl_assert(0 <= (int)tid && tid < DRD_N_THREADS
            && tid != DRD_INVALID_THREADID);
  tl_assert(! DRD_(g_threadinfo)[tid].is_recording);
  DRD_(g_threadinfo)[tid].is_recording = True;
}

/** Stop recording memory access information. */
void DRD_(thread_stop_recording)(const DrdThreadId tid)
{
  tl_assert(0 <= (int)tid && tid < DRD_N_THREADS
            && tid != DRD_INVALID_THREADID);
  tl_assert(DRD_(g_threadinfo)[tid].is_recording);
  DRD_(g_threadinfo)[tid].is_recording = False;
}

/**
 * Print the segment information for all threads.
 *
 * This function is only used for debugging purposes.
 */
void DRD_(thread_print_all)(void)
{
  unsigned i;
  Segment* p;

  for (i = 0; i < sizeof(DRD_(g_threadinfo)) / sizeof(DRD_(g_threadinfo)[0]);
       i++)
  {
    if (DRD_(g_threadinfo)[i].first)
    {
      VG_(printf)("**************\n"
                  "* thread %3d (%d/%d/%d/0x%lx/%d) *\n"
                  "**************\n",
                  i,
                  DRD_(g_threadinfo)[i].vg_thread_exists,
                  DRD_(g_threadinfo)[i].vg_threadid,
                  DRD_(g_threadinfo)[i].posix_thread_exists,
                  DRD_(g_threadinfo)[i].pt_threadid,
                  DRD_(g_threadinfo)[i].detached_posix_thread);
      for (p = DRD_(g_threadinfo)[i].first; p; p = p->next)
      {
        DRD_(sg_print)(p);
      }
    }
  }
}

/** Show a call stack involved in a data race. */
static void show_call_stack(const DrdThreadId tid,
                            const Char* const msg,
                            ExeContext* const callstack)
{
  const ThreadId vg_tid = DRD_(DrdThreadIdToVgThreadId)(tid);

  VG_(message)(Vg_UserMsg, "%s (thread %d/%d)", msg, vg_tid, tid);

  if (vg_tid != VG_INVALID_THREADID)
  {
    if (callstack)
    {
      VG_(pp_ExeContext)(callstack);
    }
    else
    {
      VG_(get_and_pp_StackTrace)(vg_tid, VG_(clo_backtrace_size));
    }
  }
  else
  {
    VG_(message)(Vg_UserMsg,
                 "   (thread finished, call stack no longer available)");
  }
}

/** Print information about the segments involved in a data race. */
static void
thread_report_conflicting_segments_segment(const DrdThreadId tid,
                                           const Addr addr,
                                           const SizeT size,
                                           const BmAccessTypeT access_type,
                                           const Segment* const p)
{
  unsigned i;

  tl_assert(0 <= (int)tid && tid < DRD_N_THREADS
            && tid != DRD_INVALID_THREADID);
  tl_assert(p);

  for (i = 0; i < sizeof(DRD_(g_threadinfo)) / sizeof(DRD_(g_threadinfo)[0]);
       i++)
  {
    if (i != tid)
    {
      Segment* q;
      for (q = DRD_(g_threadinfo)[i].last; q; q = q->prev)
      {
        /*
         * Since q iterates over the segments of thread i in order of 
         * decreasing vector clocks, if q->vc <= p->vc, then 
         * q->next->vc <= p->vc will also hold. Hence, break out of the
         * loop once this condition is met.
         */
        if (DRD_(vc_lte)(&q->vc, &p->vc))
          break;
        if (! DRD_(vc_lte)(&p->vc, &q->vc))
        {
          if (DRD_(bm_has_conflict_with)(q->bm, addr, addr + size,
                                         access_type))
          {
            tl_assert(q->stacktrace);
            show_call_stack(i,        "Other segment start",
                            q->stacktrace);
            show_call_stack(i,        "Other segment end",
                            q->next ? q->next->stacktrace : 0);
          }
        }
      }
    }
  }
}

/** Print information about all segments involved in a data race. */
void DRD_(thread_report_conflicting_segments)(const DrdThreadId tid,
                                              const Addr addr,
                                              const SizeT size,
                                              const BmAccessTypeT access_type)
{
  Segment* p;

  tl_assert(0 <= (int)tid && tid < DRD_N_THREADS
            && tid != DRD_INVALID_THREADID);

  for (p = DRD_(g_threadinfo)[tid].first; p; p = p->next)
  {
    if (DRD_(bm_has)(p->bm, addr, addr + size, access_type))
    {
      thread_report_conflicting_segments_segment(tid, addr, size,
                                                 access_type, p);
    }
  }
}

/**
 * Compute a bitmap that represents the union of all memory accesses of all
 * segments that are unordered to the current segment of the thread tid.
 */
static void thread_compute_conflict_set(struct bitmap** conflict_set,
                                        const DrdThreadId tid)
{
  Segment* p;

  tl_assert(0 <= (int)tid && tid < DRD_N_THREADS
            && tid != DRD_INVALID_THREADID);
  tl_assert(tid == DRD_(g_drd_running_tid));

  s_update_conflict_set_count++;
  s_conflict_set_bitmap_creation_count  -= DRD_(bm_get_bitmap_creation_count)();
  s_conflict_set_bitmap2_creation_count -= DRD_(bm_get_bitmap2_creation_count)();

  if (*conflict_set)
  {
    DRD_(bm_delete)(*conflict_set);
  }
  *conflict_set = DRD_(bm_new)();

  if (s_trace_conflict_set)
  {
    char msg[256];

    VG_(snprintf)(msg, sizeof(msg),
                  "computing conflict set for thread %d/%d with vc ",
                  DRD_(DrdThreadIdToVgThreadId)(tid), tid);
    DRD_(vc_snprint)(msg + VG_(strlen)(msg),
                     sizeof(msg) - VG_(strlen)(msg),
                     &DRD_(g_threadinfo)[tid].last->vc);
    VG_(message)(Vg_UserMsg, "%s", msg);
  }

  p = DRD_(g_threadinfo)[tid].last;
  {
    unsigned j;

    if (s_trace_conflict_set)
    {
      char msg[256];

      VG_(snprintf)(msg, sizeof(msg),
                    "conflict set: thread [%d] at vc ",
                    tid);
      DRD_(vc_snprint)(msg + VG_(strlen)(msg),
                       sizeof(msg) - VG_(strlen)(msg),
                       &p->vc);
      VG_(message)(Vg_UserMsg, "%s", msg);
    }

    for (j = 0; j < sizeof(DRD_(g_threadinfo)) / sizeof(DRD_(g_threadinfo)[0]); j++)
    {
      if (j != tid && DRD_(IsValidDrdThreadId)(j))
      {
        const Segment* q;
        for (q = DRD_(g_threadinfo)[j].last; q; q = q->prev)
        {
          if (! DRD_(vc_lte)(&q->vc, &p->vc) && ! DRD_(vc_lte)(&p->vc, &q->vc))
          {
            if (s_trace_conflict_set)
            {
              char msg[256];
              VG_(snprintf)(msg, sizeof(msg),
                            "conflict set: [%d] merging segment ", j);
              DRD_(vc_snprint)(msg + VG_(strlen)(msg),
                               sizeof(msg) - VG_(strlen)(msg),
                               &q->vc);
              VG_(message)(Vg_UserMsg, "%s", msg);
            }
            DRD_(bm_merge2)(*conflict_set, q->bm);
          }
          else
          {
            if (s_trace_conflict_set)
            {
              char msg[256];
              VG_(snprintf)(msg, sizeof(msg),
                            "conflict set: [%d] ignoring segment ", j);
              DRD_(vc_snprint)(msg + VG_(strlen)(msg),
                               sizeof(msg) - VG_(strlen)(msg),
                               &q->vc);
              VG_(message)(Vg_UserMsg, "%s", msg);
            }
          }
        }
      }
    }
  }

  s_conflict_set_bitmap_creation_count  += DRD_(bm_get_bitmap_creation_count)();
  s_conflict_set_bitmap2_creation_count += DRD_(bm_get_bitmap2_creation_count)();

  if (0 && s_trace_conflict_set)
  {
    VG_(message)(Vg_UserMsg, "[%d] new conflict set:", tid);
    DRD_(bm_print)(*conflict_set);
    VG_(message)(Vg_UserMsg, "[%d] end of new conflict set.", tid);
  }
}

/** Report the number of context switches performed. */
ULong DRD_(thread_get_context_switch_count)(void)
{
  return s_context_switch_count;
}

/** Report the number of ordered segments that have been discarded. */
ULong DRD_(thread_get_discard_ordered_segments_count)(void)
{
  return s_discard_ordered_segments_count;
}

/** Return how many times the conflict set has been updated. */
ULong DRD_(thread_get_update_conflict_set_count)(ULong* dsnsc, ULong* dscvc)
{
  tl_assert(dsnsc);
  tl_assert(dscvc);
  *dsnsc = s_conflict_set_new_segment_count;
  *dscvc = s_conflict_set_combine_vc_count;
  return s_update_conflict_set_count;
}

/**
 * Return the number of first-level bitmaps that have been created during
 * conflict set updates.
 */
ULong DRD_(thread_get_conflict_set_bitmap_creation_count)(void)
{
  return s_conflict_set_bitmap_creation_count;
}

/**
 * Return the number of second-level bitmaps that have been created during
 * conflict set updates.
 */
ULong DRD_(thread_get_conflict_set_bitmap2_creation_count)(void)
{
  return s_conflict_set_bitmap2_creation_count;
}
