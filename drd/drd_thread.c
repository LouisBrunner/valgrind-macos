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


#include "drd_error.h"
#include "drd_segment.h"
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



// Local functions.

static void thread_append_segment(const DrdThreadId tid,
                                  Segment* const sg);
static void thread_discard_segment(const DrdThreadId tid, Segment* const sg);
static Bool thread_conflict_set_up_to_date(const DrdThreadId tid);
static void thread_compute_conflict_set(struct bitmap** conflict_set,
                                        const DrdThreadId tid);


// Local variables.

static ULong s_context_switch_count;
static ULong s_discard_ordered_segments_count;
static ULong s_update_conflict_set_count;
static ULong s_conflict_set_new_segment_count;
static ULong s_conflict_set_combine_vc_count;
static ULong s_conflict_set_bitmap_creation_count;
static ULong s_conflict_set_bitmap2_creation_count;
static ThreadId    s_vg_running_tid  = VG_INVALID_THREADID;
DrdThreadId s_drd_running_tid = DRD_INVALID_THREADID;
ThreadInfo s_threadinfo[DRD_N_THREADS];
struct bitmap* s_conflict_set;
static Bool s_trace_context_switches = False;
static Bool s_trace_conflict_set = False;
static Bool s_segment_merging = True;


// Function definitions.

void thread_trace_context_switches(const Bool t)
{
  s_trace_context_switches = t;
}

void thread_trace_conflict_set(const Bool t)
{
  s_trace_conflict_set = t;
}

void thread_set_segment_merging(const Bool m)
{
  s_segment_merging = m;
}

/**
 * Convert Valgrind's ThreadId into a DrdThreadId. Report failure if
 * Valgrind's ThreadId does not yet exist.
 **/
DrdThreadId VgThreadIdToDrdThreadId(const ThreadId tid)
{
  int i;

  if (tid == VG_INVALID_THREADID)
    return DRD_INVALID_THREADID;

  for (i = 1; i < DRD_N_THREADS; i++)
  {
    if (s_threadinfo[i].vg_thread_exists == True
        && s_threadinfo[i].vg_threadid == tid)
    {
      return i;
    }
  }

  return DRD_INVALID_THREADID;
}

static
DrdThreadId VgThreadIdToNewDrdThreadId(const ThreadId tid)
{
  int i;

  tl_assert(VgThreadIdToDrdThreadId(tid) == DRD_INVALID_THREADID);

  for (i = 1; i < DRD_N_THREADS; i++)
  {
    if (s_threadinfo[i].vg_thread_exists == False
        && s_threadinfo[i].posix_thread_exists == False
        && s_threadinfo[i].detached_posix_thread == False)
    {
      s_threadinfo[i].vg_thread_exists = True;
      s_threadinfo[i].vg_threadid   = tid;
      s_threadinfo[i].pt_threadid   = INVALID_POSIX_THREADID;
      s_threadinfo[i].stack_min     = 0;
      s_threadinfo[i].stack_min_min = 0;
      s_threadinfo[i].stack_startup = 0;
      s_threadinfo[i].stack_max     = 0;
      s_threadinfo[i].is_recording  = True;
      s_threadinfo[i].synchr_nesting = 0;
      if (s_threadinfo[i].first != 0)
        VG_(printf)("drd thread id = %d\n", i);
      tl_assert(s_threadinfo[i].first == 0);
      tl_assert(s_threadinfo[i].last == 0);
      return i;
    }
  }

  tl_assert(False);

  return DRD_INVALID_THREADID;
}

DrdThreadId PtThreadIdToDrdThreadId(const PThreadId tid)
{
  int i;

  tl_assert(tid != INVALID_POSIX_THREADID);

  for (i = 1; i < DRD_N_THREADS; i++)
  {
    if (s_threadinfo[i].posix_thread_exists
        && s_threadinfo[i].pt_threadid == tid)
    {
      return i;
    }
  }
  return DRD_INVALID_THREADID;
}

ThreadId DrdThreadIdToVgThreadId(const DrdThreadId tid)
{
  tl_assert(0 <= (int)tid && tid < DRD_N_THREADS
            && tid != DRD_INVALID_THREADID);
  return (s_threadinfo[tid].vg_thread_exists
          ? s_threadinfo[tid].vg_threadid
          : VG_INVALID_THREADID);
}

#if 0
/** Sanity check of the doubly linked list of segments referenced by a
 *  ThreadInfo struct.
 *  @return True if sane, False if not.
 */
static Bool sane_ThreadInfo(const ThreadInfo* const ti)
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

DrdThreadId thread_pre_create(const DrdThreadId creator,
                              const ThreadId vg_created)
{
  DrdThreadId created;

  tl_assert(VgThreadIdToDrdThreadId(vg_created) == DRD_INVALID_THREADID);
  created = VgThreadIdToNewDrdThreadId(vg_created);
  tl_assert(0 <= (int)created && created < DRD_N_THREADS
            && created != DRD_INVALID_THREADID);

  tl_assert(s_threadinfo[created].first == 0);
  tl_assert(s_threadinfo[created].last == 0);
  thread_append_segment(created, sg_new(creator, created));

  return created;
}

/** Allocate the first segment for a thread. Call this just after
 *  pthread_create().
 */
DrdThreadId thread_post_create(const ThreadId vg_created)
{
  const DrdThreadId created = VgThreadIdToDrdThreadId(vg_created);

  tl_assert(0 <= (int)created && created < DRD_N_THREADS
            && created != DRD_INVALID_THREADID);

  s_threadinfo[created].stack_max     = VG_(thread_get_stack_max)(vg_created);
  s_threadinfo[created].stack_startup = s_threadinfo[created].stack_max;
  s_threadinfo[created].stack_min     = s_threadinfo[created].stack_max;
  s_threadinfo[created].stack_min_min = s_threadinfo[created].stack_max;
  s_threadinfo[created].stack_size    = VG_(thread_get_stack_size)(vg_created);
  tl_assert(s_threadinfo[created].stack_max != 0);

  return created;
}

/* NPTL hack: NPTL allocates the 'struct pthread' on top of the stack,     */
/* and accesses this data structure from multiple threads without locking. */
/* Any conflicting accesses in the range stack_startup..stack_max will be  */
/* ignored.                                                                */
void thread_set_stack_startup(const DrdThreadId tid, const Addr stack_startup)
{
  tl_assert(0 <= (int)tid && tid < DRD_N_THREADS
            && tid != DRD_INVALID_THREADID);
  tl_assert(s_threadinfo[tid].stack_min <= stack_startup);
  tl_assert(stack_startup <= s_threadinfo[tid].stack_max);
  s_threadinfo[tid].stack_startup = stack_startup;
}

Addr thread_get_stack_min(const DrdThreadId tid)
{
  tl_assert(0 <= (int)tid && tid < DRD_N_THREADS
            && tid != DRD_INVALID_THREADID);
  return s_threadinfo[tid].stack_min;
}

Addr thread_get_stack_min_min(const DrdThreadId tid)
{
  tl_assert(0 <= (int)tid && tid < DRD_N_THREADS
            && tid != DRD_INVALID_THREADID);
  return s_threadinfo[tid].stack_min_min;
}

Addr thread_get_stack_max(const DrdThreadId tid)
{
  tl_assert(0 <= (int)tid && tid < DRD_N_THREADS
            && tid != DRD_INVALID_THREADID);
  return s_threadinfo[tid].stack_max;
}

SizeT thread_get_stack_size(const DrdThreadId tid)
{
  tl_assert(0 <= (int)tid && tid < DRD_N_THREADS
            && tid != DRD_INVALID_THREADID);
  return s_threadinfo[tid].stack_size;
}

/** Clean up thread-specific data structures. Call this just after 
 *  pthread_join().
 */
void thread_delete(const DrdThreadId tid)
{
  Segment* sg;
  Segment* sg_prev;

  tl_assert(0 <= (int)tid && tid < DRD_N_THREADS
            && tid != DRD_INVALID_THREADID);
  tl_assert(s_threadinfo[tid].synchr_nesting == 0);
  for (sg = s_threadinfo[tid].last; sg; sg = sg_prev)
  {
    sg_prev = sg->prev;
    sg->prev = 0;
    sg->next = 0;
    sg_put(sg);
  }
  s_threadinfo[tid].vg_thread_exists = False;
  s_threadinfo[tid].posix_thread_exists = False;
  tl_assert(s_threadinfo[tid].detached_posix_thread == False);
  s_threadinfo[tid].first = 0;
  s_threadinfo[tid].last = 0;
}

/* Called after a thread performed its last memory access and before   */
/* thread_delete() is called. Note: thread_delete() is only called for */
/* joinable threads, not for detached threads.                         */
void thread_finished(const DrdThreadId tid)
{
  tl_assert(0 <= (int)tid && tid < DRD_N_THREADS
            && tid != DRD_INVALID_THREADID);

  s_threadinfo[tid].vg_thread_exists = False;

  if (s_threadinfo[tid].detached_posix_thread)
  {
    /* Once a detached thread has finished, its stack is deallocated and   */
    /* should no longer be taken into account when computing the conflict set*/
    s_threadinfo[tid].stack_min = s_threadinfo[tid].stack_max;

    /* For a detached thread, calling pthread_exit() invalidates the     */
    /* POSIX thread ID associated with the detached thread. For joinable */
    /* POSIX threads however, the POSIX thread ID remains live after the */
    /* pthread_exit() call until pthread_join() is called.               */
    s_threadinfo[tid].posix_thread_exists = False;
  }
}

/** Called just before pthread_cancel(). */
void thread_pre_cancel(const DrdThreadId tid)
{
  tl_assert(0 <= (int)tid && tid < DRD_N_THREADS
            && tid != DRD_INVALID_THREADID);
  tl_assert(s_threadinfo[tid].pt_threadid != INVALID_POSIX_THREADID);

  s_threadinfo[tid].synchr_nesting = 0;
}

void thread_set_pthreadid(const DrdThreadId tid, const PThreadId ptid)
{
  tl_assert(0 <= (int)tid && tid < DRD_N_THREADS
            && tid != DRD_INVALID_THREADID);
  tl_assert(s_threadinfo[tid].pt_threadid == INVALID_POSIX_THREADID);
  tl_assert(ptid != INVALID_POSIX_THREADID);
  s_threadinfo[tid].posix_thread_exists = True;
  s_threadinfo[tid].pt_threadid         = ptid;
}

Bool thread_get_joinable(const DrdThreadId tid)
{
  tl_assert(0 <= (int)tid && tid < DRD_N_THREADS
            && tid != DRD_INVALID_THREADID);
  return ! s_threadinfo[tid].detached_posix_thread;
}

void thread_set_joinable(const DrdThreadId tid, const Bool joinable)
{
  tl_assert(0 <= (int)tid && tid < DRD_N_THREADS
            && tid != DRD_INVALID_THREADID);
  tl_assert(!! joinable == joinable);
  tl_assert(s_threadinfo[tid].pt_threadid != INVALID_POSIX_THREADID);
#if 0
  VG_(message)(Vg_DebugMsg,
               "thread_set_joinable(%d/%d, %s)",
               tid,
               s_threadinfo[tid].vg_threadid,
               joinable ? "joinable" : "detached");
#endif
  s_threadinfo[tid].detached_posix_thread = ! joinable;
}

void thread_set_vg_running_tid(const ThreadId vg_tid)
{
  tl_assert(vg_tid != VG_INVALID_THREADID);

  if (vg_tid != s_vg_running_tid)
  {
    thread_set_running_tid(vg_tid, VgThreadIdToDrdThreadId(vg_tid));
  }

  tl_assert(s_vg_running_tid != VG_INVALID_THREADID);
  tl_assert(s_drd_running_tid != DRD_INVALID_THREADID);
}

void thread_set_running_tid(const ThreadId vg_tid, const DrdThreadId drd_tid)
{
  tl_assert(vg_tid != VG_INVALID_THREADID);
  tl_assert(drd_tid != DRD_INVALID_THREADID);
   
  if (vg_tid != s_vg_running_tid)
  {
    if (s_trace_context_switches
        && s_drd_running_tid != DRD_INVALID_THREADID)
    {
      VG_(message)(Vg_DebugMsg,
                   "Context switch from thread %d/%d to thread %d/%d;"
                   " segments: %llu",
                   s_vg_running_tid, s_drd_running_tid,
                   DrdThreadIdToVgThreadId(drd_tid), drd_tid,
                   sg_get_alive_segments_count());
    }
    s_vg_running_tid = vg_tid;
    s_drd_running_tid = drd_tid;
    thread_compute_conflict_set(&s_conflict_set, drd_tid);
    s_context_switch_count++;
  }

  tl_assert(s_vg_running_tid != VG_INVALID_THREADID);
  tl_assert(s_drd_running_tid != DRD_INVALID_THREADID);
}

int thread_enter_synchr(const DrdThreadId tid)
{
  tl_assert(IsValidDrdThreadId(tid));
  return s_threadinfo[tid].synchr_nesting++;
}

int thread_leave_synchr(const DrdThreadId tid)
{
  tl_assert(IsValidDrdThreadId(tid));
  tl_assert(s_threadinfo[tid].synchr_nesting >= 1);
  return --s_threadinfo[tid].synchr_nesting;
}

int thread_get_synchr_nesting_count(const DrdThreadId tid)
{
  tl_assert(IsValidDrdThreadId(tid));
  return s_threadinfo[tid].synchr_nesting;
}

/** Append a new segment at the end of the segment list. */
static void thread_append_segment(const DrdThreadId tid, Segment* const sg)
{
  tl_assert(0 <= (int)tid && tid < DRD_N_THREADS
            && tid != DRD_INVALID_THREADID);
  // tl_assert(sane_ThreadInfo(&s_threadinfo[tid]));
  sg->prev = s_threadinfo[tid].last;
  sg->next = 0;
  if (s_threadinfo[tid].last)
    s_threadinfo[tid].last->next = sg;
  s_threadinfo[tid].last = sg;
  if (s_threadinfo[tid].first == 0)
    s_threadinfo[tid].first = sg;
  // tl_assert(sane_ThreadInfo(&s_threadinfo[tid]));
}

/** Remove a segment from the segment list of thread threadid, and free the
 *  associated memory.
 */
static void thread_discard_segment(const DrdThreadId tid, Segment* const sg)
{
  tl_assert(0 <= (int)tid && tid < DRD_N_THREADS
            && tid != DRD_INVALID_THREADID);
  //tl_assert(sane_ThreadInfo(&s_threadinfo[tid]));

  if (sg->prev)
    sg->prev->next = sg->next;
  if (sg->next)
    sg->next->prev = sg->prev;
  if (sg == s_threadinfo[tid].first)
    s_threadinfo[tid].first = sg->next;
  if (sg == s_threadinfo[tid].last)
    s_threadinfo[tid].last = sg->prev;
  sg_put(sg);

  //tl_assert(sane_ThreadInfo(&s_threadinfo[tid]));
}

VectorClock* thread_get_vc(const DrdThreadId tid)
{
  tl_assert(0 <= (int)tid && tid < DRD_N_THREADS
            && tid != DRD_INVALID_THREADID);
  tl_assert(s_threadinfo[tid].last);
  return &s_threadinfo[tid].last->vc;
}

/** Return the latest segment of thread 'tid' and increment its reference
 *  count.
 */
void thread_get_latest_segment(Segment** sg, const DrdThreadId tid)
{
  tl_assert(sg);
  tl_assert(0 <= (int)tid && tid < DRD_N_THREADS
            && tid != DRD_INVALID_THREADID);
  tl_assert(s_threadinfo[tid].last);

  sg_put(*sg);
  *sg = sg_get(s_threadinfo[tid].last);
}

/**
 * Compute the minimum of all latest vector clocks of all threads
 * (Michiel Ronsse calls this "clock snooping" in his papers about DIOTA).
 * @param vc pointer to a vectorclock, holds result upon return.
 */
static void thread_compute_minimum_vc(VectorClock* vc)
{
  unsigned i;
  Bool first;
  Segment* latest_sg;

  first = True;
  for (i = 0; i < sizeof(s_threadinfo) / sizeof(s_threadinfo[0]); i++)
  {
    latest_sg = s_threadinfo[i].last;
    if (latest_sg)
    {
      if (first)
        vc_assign(vc, &latest_sg->vc);
      else
        vc_min(vc, &latest_sg->vc);
      first = False;
    }
  }
}

static void thread_compute_maximum_vc(VectorClock* vc)
{
  unsigned i;
  Bool first;
  Segment* latest_sg;

  first = True;
  for (i = 0; i < sizeof(s_threadinfo) / sizeof(s_threadinfo[0]); i++)
  {
    latest_sg = s_threadinfo[i].last;
    if (latest_sg)
    {
      if (first)
        vc_assign(vc, &latest_sg->vc);
      else
        vc_combine(vc, &latest_sg->vc);
      first = False;
    }
  }
}

/**
 * Discard all segments that have a defined order against the latest vector
 * clock of every thread -- these segments can no longer be involved in a
 * data race.
 */
static void thread_discard_ordered_segments(void)
{
  unsigned i;
  VectorClock thread_vc_min;

  s_discard_ordered_segments_count++;

  vc_init(&thread_vc_min, 0, 0);
  thread_compute_minimum_vc(&thread_vc_min);
  if (sg_get_trace())
  {
    char msg[256];
    VectorClock thread_vc_max;

    vc_init(&thread_vc_max, 0, 0);
    thread_compute_maximum_vc(&thread_vc_max);
    VG_(snprintf)(msg, sizeof(msg),
                  "Discarding ordered segments -- min vc is ");
    vc_snprint(msg + VG_(strlen)(msg), sizeof(msg) - VG_(strlen)(msg),
               &thread_vc_min);
    VG_(snprintf)(msg + VG_(strlen)(msg), sizeof(msg) - VG_(strlen)(msg),
                  ", max vc is ");
    vc_snprint(msg + VG_(strlen)(msg), sizeof(msg) - VG_(strlen)(msg),
               &thread_vc_max);
    VG_(message)(Vg_UserMsg, "%s", msg);
    vc_cleanup(&thread_vc_max);
  }

  for (i = 0; i < sizeof(s_threadinfo) / sizeof(s_threadinfo[0]); i++)
  {
    Segment* sg;
    Segment* sg_next;
    for (sg = s_threadinfo[i].first;
         sg && (sg_next = sg->next) && vc_lte(&sg->vc, &thread_vc_min);
         sg = sg_next)
    {
      thread_discard_segment(i, sg);
    }
  }
  vc_cleanup(&thread_vc_min);
}

/** Merge all segments that may be merged without triggering false positives
 *  or discarding real data races. For the theoretical background of segment
 *  merging, see also the following paper:
 *  Mark Christiaens, Michiel Ronsse and Koen De Bosschere.
 *  Bounding the number of segment histories during data race detection.
 *  Parallel Computing archive, Volume 28, Issue 9, pp 1221-1238,
 *  September 2002.
 */
static void thread_merge_segments(void)
{
  unsigned i;

  for (i = 0; i < sizeof(s_threadinfo) / sizeof(s_threadinfo[0]); i++)
  {
    Segment* sg;

    // tl_assert(sane_ThreadInfo(&s_threadinfo[i]));

    for (sg = s_threadinfo[i].first; sg; sg = sg->next)
    {
      if (sg_get_refcnt(sg) == 1
          && sg->next
          && sg_get_refcnt(sg->next) == 1
          && sg->next->next)
      {
        /* Merge sg and sg->next into sg. */
        sg_merge(sg, sg->next);
        thread_discard_segment(i, sg->next);
      }
    }

    // tl_assert(sane_ThreadInfo(&s_threadinfo[i]));
  }
}

/** Every change in the vector clock of a thread may cause segments that
 *  were previously ordered to this thread to become unordered. Hence,
 *  it may be necessary to recalculate the conflict set if the vector clock 
 *  of the current thread is updated. This function check whether such a
 *  recalculation is necessary.
 *
 *  @param tid    Thread ID of the thread to which a new segment has been
 *                appended.
 *  @param new_sg Pointer to the most recent segment of thread tid.
 */
static Bool conflict_set_update_needed(const DrdThreadId tid,
                                     const Segment* const new_sg)
{
#if 0
  unsigned i;
  const Segment* old_sg;

  tl_assert(new_sg);

  /* If a new segment was added to another thread than the running thread, */
  /* just tell the caller to update the conflict set.                        */
  if (tid != s_drd_running_tid)
    return True;

  /* Always let the caller update the conflict set after creation of the */
  /* first segment.                                                    */
  old_sg = new_sg->prev;
  if (old_sg == 0)
    return True;

  for (i = 0; i < sizeof(s_threadinfo) / sizeof(s_threadinfo[0]); i++)
  {
    Segment* q;

    if (i == s_drd_running_tid)
      continue;

    for (q = s_threadinfo[i].last; q; q = q->prev)
    {
      /* If the expression below evaluates to false, this expression will */
      /* also evaluate to false for all subsequent iterations. So stop    */
      /* iterating.                                                       */
      if (vc_lte(&q->vc, &old_sg->vc))
        break;
      /* If the vector clock of the 2nd the last segment is not ordered   */
      /* to the vector clock of segment q, and the last segment is, ask   */
      /* the caller to update the conflict set.                             */
      if (! vc_lte(&old_sg->vc, &q->vc))
      {
        return True;
      }
      /* If the vector clock of the last segment is not ordered to the    */
      /* vector clock of segment q, ask the caller to update the conflict   */
      /* set.                                                             */
      if (! vc_lte(&q->vc, &new_sg->vc) && ! vc_lte(&new_sg->vc, &q->vc))
      {
        return True;
      }
    }
  }

  return False;
#else
  return True;
#endif
}

/** Create a new segment for the specified thread, and discard any segments
 *  that cannot cause races anymore.
 */
void thread_new_segment(const DrdThreadId tid)
{
  Segment* new_sg;

  tl_assert(0 <= (int)tid && tid < DRD_N_THREADS
            && tid != DRD_INVALID_THREADID);

  new_sg = sg_new(tid, tid);
  thread_append_segment(tid, new_sg);

  if (conflict_set_update_needed(tid, new_sg))
  {
    thread_compute_conflict_set(&s_conflict_set, s_drd_running_tid);
    s_conflict_set_new_segment_count++;
  }
  else if (tid == s_drd_running_tid)
  {
    tl_assert(thread_conflict_set_up_to_date(s_drd_running_tid));
  }

  thread_discard_ordered_segments();

  if (s_segment_merging)
    thread_merge_segments();
}

/** Call this function after thread 'joiner' joined thread 'joinee'. */
void thread_combine_vc(DrdThreadId joiner, DrdThreadId joinee)
{
  tl_assert(joiner != joinee);
  tl_assert(0 <= (int)joiner && joiner < DRD_N_THREADS
            && joiner != DRD_INVALID_THREADID);
  tl_assert(0 <= (int)joinee && joinee < DRD_N_THREADS
            && joinee != DRD_INVALID_THREADID);
  tl_assert(s_threadinfo[joiner].last);
  tl_assert(s_threadinfo[joinee].last);
  vc_combine(&s_threadinfo[joiner].last->vc, &s_threadinfo[joinee].last->vc);
  thread_discard_ordered_segments();

  if (joiner == s_drd_running_tid)
  {
    thread_compute_conflict_set(&s_conflict_set, joiner);
  }
}

/** Call this function after thread 'tid' had to wait because of thread
 *  synchronization until the memory accesses in the segment with vector clock
 *  'vc' finished.
 */
void thread_combine_vc2(DrdThreadId tid, const VectorClock* const vc)
{
  tl_assert(0 <= (int)tid && tid < DRD_N_THREADS
            && tid != DRD_INVALID_THREADID);
  tl_assert(s_threadinfo[tid].last);
  tl_assert(vc);
  vc_combine(&s_threadinfo[tid].last->vc, vc);
  thread_compute_conflict_set(&s_conflict_set, tid);
  thread_discard_ordered_segments();
  s_conflict_set_combine_vc_count++;
}

/** Call this function whenever a thread is no longer using the memory
 *  [ a1, a2 [, e.g. because of a call to free() or a stack pointer
 *  increase.
 */
void thread_stop_using_mem(const Addr a1, const Addr a2)
{
  DrdThreadId other_user;
  unsigned i;

  /* For all threads, mark the range [ a1, a2 [ as no longer in use. */
  other_user = DRD_INVALID_THREADID;
  for (i = 0; i < sizeof(s_threadinfo) / sizeof(s_threadinfo[0]); i++)
  {
    Segment* p;
    for (p = s_threadinfo[i].first; p; p = p->next)
    {
      if (other_user == DRD_INVALID_THREADID
          && i != s_drd_running_tid)
      {
        if (UNLIKELY(bm_test_and_clear(p->bm, a1, a2)))
        {
          other_user = i;
        }
        continue;
      }
      bm_clear(p->bm, a1, a2);
    }
  }

  /* If any other thread had accessed memory in [ a1, a2 [, update the */
  /* conflict set. */
  if (other_user != DRD_INVALID_THREADID
      && bm_has_any_access(s_conflict_set, a1, a2))
  {
    thread_compute_conflict_set(&s_conflict_set, thread_get_running_tid());
  }
}

void thread_start_recording(const DrdThreadId tid)
{
  tl_assert(0 <= (int)tid && tid < DRD_N_THREADS
            && tid != DRD_INVALID_THREADID);
  tl_assert(! s_threadinfo[tid].is_recording);
  s_threadinfo[tid].is_recording = True;
}

void thread_stop_recording(const DrdThreadId tid)
{
  tl_assert(0 <= (int)tid && tid < DRD_N_THREADS
            && tid != DRD_INVALID_THREADID);
  tl_assert(s_threadinfo[tid].is_recording);
  s_threadinfo[tid].is_recording = False;
}

void thread_print_all(void)
{
  unsigned i;
  Segment* p;

  for (i = 0; i < sizeof(s_threadinfo) / sizeof(s_threadinfo[0]); i++)
  {
    if (s_threadinfo[i].first)
    {
      VG_(printf)("**************\n"
                  "* thread %3d (%d/%d/%d/0x%lx/%d) *\n"
                  "**************\n",
                  i,
                  s_threadinfo[i].vg_thread_exists,
                  s_threadinfo[i].vg_threadid,
                  s_threadinfo[i].posix_thread_exists,
                  s_threadinfo[i].pt_threadid,
                  s_threadinfo[i].detached_posix_thread);
      for (p = s_threadinfo[i].first; p; p = p->next)
      {
        sg_print(p);
      }
    }
  }
}

static void show_call_stack(const DrdThreadId tid,
                            const Char* const msg,
                            ExeContext* const callstack)
{
  const ThreadId vg_tid = DrdThreadIdToVgThreadId(tid);

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

  for (i = 0; i < sizeof(s_threadinfo) / sizeof(s_threadinfo[0]); i++)
  {
    if (i != tid)
    {
      Segment* q;
      for (q = s_threadinfo[i].last; q; q = q->prev)
      {
        // Since q iterates over the segments of thread i in order of 
        // decreasing vector clocks, if q->vc <= p->vc, then 
        // q->next->vc <= p->vc will also hold. Hence, break out of the
        // loop once this condition is met.
        if (vc_lte(&q->vc, &p->vc))
          break;
        if (! vc_lte(&p->vc, &q->vc))
        {
          if (bm_has_conflict_with(q->bm, addr, addr + size, access_type))
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

void thread_report_conflicting_segments(const DrdThreadId tid,
                                        const Addr addr,
                                        const SizeT size,
                                        const BmAccessTypeT access_type)
{
  Segment* p;

  tl_assert(0 <= (int)tid && tid < DRD_N_THREADS
            && tid != DRD_INVALID_THREADID);

  for (p = s_threadinfo[tid].first; p; p = p->next)
  {
    if (bm_has(p->bm, addr, addr + size, access_type))
    {
      thread_report_conflicting_segments_segment(tid, addr, size,
                                                 access_type, p);
    }
  }
}

/** Verify whether the conflict set for thread tid is up to date. Only perform
 *  the check if the environment variable DRD_VERIFY_CONFLICT_SET has been set.
 */
static Bool thread_conflict_set_up_to_date(const DrdThreadId tid)
{
  static int do_verify_conflict_set = -1;
  Bool result;
  struct bitmap* computed_conflict_set = 0;

  if (do_verify_conflict_set < 0)
  {
    //VG_(message)(Vg_DebugMsg, "%s", VG_(getenv)("DRD_VERIFY_CONFLICT_SET"));
    do_verify_conflict_set = VG_(getenv)("DRD_VERIFY_CONFLICT_SET") != 0;
  }
  if (do_verify_conflict_set == 0)
    return True;

  thread_compute_conflict_set(&computed_conflict_set, tid);
  result = bm_equal(s_conflict_set, computed_conflict_set);
  bm_delete(computed_conflict_set);
  return result;
}

/** Compute a bitmap that represents the union of all memory accesses of all
 *  segments that are unordered to the current segment of the thread tid.
 */
static void thread_compute_conflict_set(struct bitmap** conflict_set,
                                        const DrdThreadId tid)
{
  Segment* p;

  tl_assert(0 <= (int)tid && tid < DRD_N_THREADS
            && tid != DRD_INVALID_THREADID);
  tl_assert(tid == s_drd_running_tid);

  s_update_conflict_set_count++;
  s_conflict_set_bitmap_creation_count  -= bm_get_bitmap_creation_count();
  s_conflict_set_bitmap2_creation_count -= bm_get_bitmap2_creation_count();

  if (*conflict_set)
  {
    bm_delete(*conflict_set);
  }
  *conflict_set = bm_new();

  if (s_trace_conflict_set)
  {
    char msg[256];

    VG_(snprintf)(msg, sizeof(msg),
                  "computing conflict set for thread %d/%d with vc ",
                  DrdThreadIdToVgThreadId(tid), tid);
    vc_snprint(msg + VG_(strlen)(msg),
               sizeof(msg) - VG_(strlen)(msg),
               &s_threadinfo[tid].last->vc);
    VG_(message)(Vg_UserMsg, "%s", msg);
  }

  p = s_threadinfo[tid].last;
  {
    unsigned j;

    if (s_trace_conflict_set)
    {
      char msg[256];

      VG_(snprintf)(msg, sizeof(msg),
                    "conflict set: thread [%d] at vc ",
                    tid);
      vc_snprint(msg + VG_(strlen)(msg),
                 sizeof(msg) - VG_(strlen)(msg),
                 &p->vc);
      VG_(message)(Vg_UserMsg, "%s", msg);
    }

    for (j = 0; j < sizeof(s_threadinfo) / sizeof(s_threadinfo[0]); j++)
    {
      if (j != tid && IsValidDrdThreadId(j))
      {
        const Segment* q;
        for (q = s_threadinfo[j].last; q; q = q->prev)
        {
          if (! vc_lte(&q->vc, &p->vc) && ! vc_lte(&p->vc, &q->vc))
          {
            if (s_trace_conflict_set)
            {
              char msg[256];
              VG_(snprintf)(msg, sizeof(msg),
                            "conflict set: [%d] merging segment ", j);
              vc_snprint(msg + VG_(strlen)(msg),
                         sizeof(msg) - VG_(strlen)(msg),
                         &q->vc);
              VG_(message)(Vg_UserMsg, "%s", msg);
            }
            bm_merge2(*conflict_set, q->bm);
          }
          else
          {
            if (s_trace_conflict_set)
            {
              char msg[256];
              VG_(snprintf)(msg, sizeof(msg),
                            "conflict set: [%d] ignoring segment ", j);
              vc_snprint(msg + VG_(strlen)(msg),
                         sizeof(msg) - VG_(strlen)(msg),
                         &q->vc);
              VG_(message)(Vg_UserMsg, "%s", msg);
            }
          }
        }
      }
    }
  }

  s_conflict_set_bitmap_creation_count  += bm_get_bitmap_creation_count();
  s_conflict_set_bitmap2_creation_count += bm_get_bitmap2_creation_count();

  if (0 && s_trace_conflict_set)
  {
    VG_(message)(Vg_UserMsg, "[%d] new conflict set:", tid);
    bm_print(*conflict_set);
    VG_(message)(Vg_UserMsg, "[%d] end of new conflict set.", tid);
  }
}

ULong thread_get_context_switch_count(void)
{
  return s_context_switch_count;
}

ULong thread_get_discard_ordered_segments_count(void)
{
  return s_discard_ordered_segments_count;
}

ULong thread_get_update_conflict_set_count(ULong* dsnsc, ULong* dscvc)
{
  tl_assert(dsnsc);
  tl_assert(dscvc);
  *dsnsc = s_conflict_set_new_segment_count;
  *dscvc = s_conflict_set_combine_vc_count;
  return s_update_conflict_set_count;
}

ULong thread_get_conflict_set_bitmap_creation_count(void)
{
  return s_conflict_set_bitmap_creation_count;
}

ULong thread_get_conflict_set_bitmap2_creation_count(void)
{
  return s_conflict_set_bitmap2_creation_count;
}
