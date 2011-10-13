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
static Bool thread_conflict_set_up_to_date(const DrdThreadId tid);


/* Local variables. */

static ULong    s_context_switch_count;
static ULong    s_discard_ordered_segments_count;
static ULong    s_compute_conflict_set_count;
static ULong    s_update_conflict_set_count;
static ULong    s_update_conflict_set_new_sg_count;
static ULong    s_update_conflict_set_sync_count;
static ULong    s_update_conflict_set_join_count;
static ULong    s_conflict_set_bitmap_creation_count;
static ULong    s_conflict_set_bitmap2_creation_count;
static ThreadId s_vg_running_tid  = VG_INVALID_THREADID;
DrdThreadId     DRD_(g_drd_running_tid) = DRD_INVALID_THREADID;
ThreadInfo      DRD_(g_threadinfo)[DRD_N_THREADS];
struct bitmap*  DRD_(g_conflict_set);
static Bool     s_trace_context_switches = False;
static Bool     s_trace_conflict_set = False;
static Bool     s_trace_conflict_set_bm = False;
static Bool     s_trace_fork_join = False;
static Bool     s_segment_merging = True;
static Bool     s_new_segments_since_last_merge;
static int      s_segment_merge_interval = 10;
static unsigned s_join_list_vol = 10;
static unsigned s_deletion_head;
static unsigned s_deletion_tail;


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

/** Enables/disables conflict set bitmap tracing. */
void DRD_(thread_trace_conflict_set_bm)(const Bool t)
{
   tl_assert(t == False || t == True);
   s_trace_conflict_set_bm = t;
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

/** Get the segment merging interval. */
int DRD_(thread_get_segment_merge_interval)(void)
{
   return s_segment_merge_interval;
}

/** Set the segment merging interval. */
void DRD_(thread_set_segment_merge_interval)(const int i)
{
   s_segment_merge_interval = i;
}

void DRD_(thread_set_join_list_vol)(const int jlv)
{
   s_join_list_vol = jlv;
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
      if (!DRD_(g_threadinfo)[i].valid)
      {
         tl_assert(! DRD_(IsValidDrdThreadId)(i));

         DRD_(g_threadinfo)[i].valid         = True;
         DRD_(g_threadinfo)[i].vg_thread_exists = True;
         DRD_(g_threadinfo)[i].vg_threadid   = tid;
         DRD_(g_threadinfo)[i].pt_threadid   = INVALID_POSIX_THREADID;
         DRD_(g_threadinfo)[i].stack_min     = 0;
         DRD_(g_threadinfo)[i].stack_min_min = 0;
         DRD_(g_threadinfo)[i].stack_startup = 0;
         DRD_(g_threadinfo)[i].stack_max     = 0;
         DRD_(thread_set_name)(i, "");
         DRD_(g_threadinfo)[i].on_alt_stack        = False;
         DRD_(g_threadinfo)[i].is_recording_loads  = True;
         DRD_(g_threadinfo)[i].is_recording_stores = True;
         DRD_(g_threadinfo)[i].pthread_create_nesting_level = 0;
         DRD_(g_threadinfo)[i].synchr_nesting = 0;
         DRD_(g_threadinfo)[i].deletion_seq = s_deletion_tail - 1;
         tl_assert(DRD_(g_threadinfo)[i].first == 0);
         tl_assert(DRD_(g_threadinfo)[i].last == 0);

         tl_assert(DRD_(IsValidDrdThreadId)(i));

         return i;
      }
   }

   VG_(printf)(
"\nSorry, but the maximum number of threads supported by DRD has been exceeded."
"Aborting.\n");

   tl_assert(False);

   return DRD_INVALID_THREADID;
}

/** Convert a POSIX thread ID into a DRD thread ID. */
DrdThreadId DRD_(PtThreadIdToDrdThreadId)(const PThreadId tid)
{
   int i;

   if (tid != INVALID_POSIX_THREADID)
   {
      for (i = 1; i < DRD_N_THREADS; i++)
      {
         if (DRD_(g_threadinfo)[i].posix_thread_exists
             && DRD_(g_threadinfo)[i].pt_threadid == tid)
         {
            return i;
         }
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

#ifdef ENABLE_DRD_CONSISTENCY_CHECKS
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
   /* Create an initial segment for the newly created thread. */
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

   DRD_(g_threadinfo)[created].stack_max
      = VG_(thread_get_stack_max)(vg_created);
   DRD_(g_threadinfo)[created].stack_startup
      = DRD_(g_threadinfo)[created].stack_max;
   DRD_(g_threadinfo)[created].stack_min
      = DRD_(g_threadinfo)[created].stack_max;
   DRD_(g_threadinfo)[created].stack_min_min
      = DRD_(g_threadinfo)[created].stack_max;
   DRD_(g_threadinfo)[created].stack_size
      = VG_(thread_get_stack_size)(vg_created);
   tl_assert(DRD_(g_threadinfo)[created].stack_max != 0);

   return created;
}

static void DRD_(thread_delayed_delete)(const DrdThreadId tid)
{
   int j;

   DRD_(g_threadinfo)[tid].vg_thread_exists = False;
   DRD_(g_threadinfo)[tid].posix_thread_exists = False;
   DRD_(g_threadinfo)[tid].deletion_seq = s_deletion_head++;
#if 0
   VG_(message)(Vg_DebugMsg, "Adding thread %d to the deletion list\n", tid);
#endif
   if (s_deletion_head - s_deletion_tail >= s_join_list_vol) {
      for (j = 0; j < DRD_N_THREADS; ++j) {
         if (DRD_(IsValidDrdThreadId)(j)
             && DRD_(g_threadinfo)[j].deletion_seq == s_deletion_tail)
         {
            s_deletion_tail++;
#if 0
            VG_(message)(Vg_DebugMsg, "Delayed delete of thread %d\n", j);
#endif
            DRD_(thread_delete)(j, False);
            break;
         }
      }
   }
}

/**
 * Process VG_USERREQ__POST_THREAD_JOIN. This client request is invoked just
 * after thread drd_joiner joined thread drd_joinee.
 */
void DRD_(thread_post_join)(DrdThreadId drd_joiner, DrdThreadId drd_joinee)
{
   tl_assert(DRD_(IsValidDrdThreadId)(drd_joiner));
   tl_assert(DRD_(IsValidDrdThreadId)(drd_joinee));

   DRD_(thread_new_segment)(drd_joiner);
   DRD_(thread_combine_vc_join)(drd_joiner, drd_joinee);
   DRD_(thread_new_segment)(drd_joinee);

   if (s_trace_fork_join)
   {
      const ThreadId joiner = DRD_(DrdThreadIdToVgThreadId)(drd_joiner);
      const unsigned msg_size = 256;
      char* msg;

      msg = VG_(malloc)("drd.main.dptj.1", msg_size);
      tl_assert(msg);
      VG_(snprintf)(msg, msg_size,
                    "drd_post_thread_join joiner = %d, joinee = %d",
                    drd_joiner, drd_joinee);
      if (joiner)
      {
         char* vc;

         vc = DRD_(vc_aprint)(DRD_(thread_get_vc)(drd_joiner));
         VG_(snprintf)(msg + VG_(strlen)(msg), msg_size - VG_(strlen)(msg),
                       ", new vc: %s", vc);
         VG_(free)(vc);
      }
      DRD_(trace_msg)("%pS", msg);
      VG_(free)(msg);
   }

   if (!  DRD_(get_check_stack_accesses)())
   {
      DRD_(finish_suppression)(DRD_(thread_get_stack_max)(drd_joinee)
                               - DRD_(thread_get_stack_size)(drd_joinee),
                               DRD_(thread_get_stack_max)(drd_joinee));
   }
   DRD_(clientobj_delete_thread)(drd_joinee);
   DRD_(thread_delayed_delete)(drd_joinee);
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

Bool DRD_(thread_get_on_alt_stack)(const DrdThreadId tid)
{
   tl_assert(0 <= (int)tid && tid < DRD_N_THREADS
             && tid != DRD_INVALID_THREADID);
   return DRD_(g_threadinfo)[tid].on_alt_stack;
}

void DRD_(thread_set_on_alt_stack)(const DrdThreadId tid,
                                   const Bool on_alt_stack)
{
   tl_assert(0 <= (int)tid && tid < DRD_N_THREADS
             && tid != DRD_INVALID_THREADID);
   tl_assert(on_alt_stack == !!on_alt_stack);
   DRD_(g_threadinfo)[tid].on_alt_stack = on_alt_stack;
}

Int DRD_(thread_get_threads_on_alt_stack)(void)
{
   int i, n = 0;

   for (i = 1; i < DRD_N_THREADS; i++)
      n += DRD_(g_threadinfo)[i].on_alt_stack;
   return n;
}

/**
 * Clean up thread-specific data structures.
 */
void DRD_(thread_delete)(const DrdThreadId tid, const Bool detached)
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
   DRD_(g_threadinfo)[tid].valid = False;
   DRD_(g_threadinfo)[tid].vg_thread_exists = False;
   DRD_(g_threadinfo)[tid].posix_thread_exists = False;
   if (detached)
      DRD_(g_threadinfo)[tid].detached_posix_thread = False;
   else
      tl_assert(!DRD_(g_threadinfo)[tid].detached_posix_thread);
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
      /*
       * Once a detached thread has finished, its stack is deallocated and
       * should no longer be taken into account when computing the conflict set.
       */
      DRD_(g_threadinfo)[tid].stack_min = DRD_(g_threadinfo)[tid].stack_max;

      /*
       * For a detached thread, calling pthread_exit() invalidates the
       * POSIX thread ID associated with the detached thread. For joinable
       * POSIX threads however, the POSIX thread ID remains live after the
       * pthread_exit() call until pthread_join() is called.
       */
      DRD_(g_threadinfo)[tid].posix_thread_exists = False;
   }
}

/** Called just after fork() in the child process. */
void DRD_(drd_thread_atfork_child)(const DrdThreadId tid)
{
   unsigned i;

   for (i = 1; i < DRD_N_THREADS; i++)
   {
      if (i == tid)
	 continue;
      if (DRD_(IsValidDrdThreadId(i)))
	 DRD_(thread_delete)(i, True);
      tl_assert(!DRD_(IsValidDrdThreadId(i)));
   }   
}

/** Called just before pthread_cancel(). */
void DRD_(thread_pre_cancel)(const DrdThreadId tid)
{
   tl_assert(0 <= (int)tid && tid < DRD_N_THREADS
             && tid != DRD_INVALID_THREADID);
   tl_assert(DRD_(g_threadinfo)[tid].pt_threadid != INVALID_POSIX_THREADID);

   if (DRD_(thread_get_trace_fork_join)())
      DRD_(trace_msg)("[%d] drd_thread_pre_cancel %d",
                      DRD_(g_drd_running_tid), tid);
}

/**
 * Store the POSIX thread ID for the specified thread.
 *
 * @note This function can be called two times for the same thread -- see also
 * the comment block preceding the pthread_create() wrapper in
 * drd_pthread_intercepts.c.
 */
void DRD_(thread_set_pthreadid)(const DrdThreadId tid, const PThreadId ptid)
{
   tl_assert(0 <= (int)tid && tid < DRD_N_THREADS
             && tid != DRD_INVALID_THREADID);
   tl_assert(DRD_(g_threadinfo)[tid].pt_threadid == INVALID_POSIX_THREADID
             || DRD_(g_threadinfo)[tid].pt_threadid == ptid);
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

   DRD_(g_threadinfo)[tid].detached_posix_thread = ! joinable;
}

/** Tells DRD that the calling thread is about to enter pthread_create(). */
void DRD_(thread_entering_pthread_create)(const DrdThreadId tid)
{
   tl_assert(0 <= (int)tid && tid < DRD_N_THREADS
             && tid != DRD_INVALID_THREADID);
   tl_assert(DRD_(g_threadinfo)[tid].pt_threadid != INVALID_POSIX_THREADID);
   tl_assert(DRD_(g_threadinfo)[tid].pthread_create_nesting_level >= 0);

   DRD_(g_threadinfo)[tid].pthread_create_nesting_level++;
}

/** Tells DRD that the calling thread has left pthread_create(). */
void DRD_(thread_left_pthread_create)(const DrdThreadId tid)
{
   tl_assert(0 <= (int)tid && tid < DRD_N_THREADS
             && tid != DRD_INVALID_THREADID);
   tl_assert(DRD_(g_threadinfo)[tid].pt_threadid != INVALID_POSIX_THREADID);
   tl_assert(DRD_(g_threadinfo)[tid].pthread_create_nesting_level > 0);

   DRD_(g_threadinfo)[tid].pthread_create_nesting_level--;
}

/** Obtain the thread number and the user-assigned thread name. */
const char* DRD_(thread_get_name)(const DrdThreadId tid)
{
   tl_assert(0 <= (int)tid && tid < DRD_N_THREADS
             && tid != DRD_INVALID_THREADID);

   return DRD_(g_threadinfo)[tid].name;
}

/** Set the name of the specified thread. */
void DRD_(thread_set_name)(const DrdThreadId tid, const char* const name)
{
   tl_assert(0 <= (int)tid && tid < DRD_N_THREADS
             && tid != DRD_INVALID_THREADID);

   if (name == NULL || name[0] == 0)
      VG_(snprintf)(DRD_(g_threadinfo)[tid].name,
                    sizeof(DRD_(g_threadinfo)[tid].name),
                    "Thread %d",
                    tid);
   else
      VG_(snprintf)(DRD_(g_threadinfo)[tid].name,
                    sizeof(DRD_(g_threadinfo)[tid].name),
                    "Thread %d (%s)",
                    tid, name);
   DRD_(g_threadinfo)[tid].name[sizeof(DRD_(g_threadinfo)[tid].name) - 1] = 0;
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
                      "Context switch from thread %d to thread %d;"
                      " segments: %llu\n",
                      DRD_(g_drd_running_tid), drd_tid,
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

#ifdef ENABLE_DRD_CONSISTENCY_CHECKS
   tl_assert(DRD_(sane_ThreadInfo)(&DRD_(g_threadinfo)[tid]));
#endif

   sg->prev = DRD_(g_threadinfo)[tid].last;
   sg->next = 0;
   if (DRD_(g_threadinfo)[tid].last)
      DRD_(g_threadinfo)[tid].last->next = sg;
   DRD_(g_threadinfo)[tid].last = sg;
   if (DRD_(g_threadinfo)[tid].first == 0)
      DRD_(g_threadinfo)[tid].first = sg;

#ifdef ENABLE_DRD_CONSISTENCY_CHECKS
   tl_assert(DRD_(sane_ThreadInfo)(&DRD_(g_threadinfo)[tid]));
#endif
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

#ifdef ENABLE_DRD_CONSISTENCY_CHECKS
   tl_assert(DRD_(sane_ThreadInfo)(&DRD_(g_threadinfo)[tid]));
#endif

   if (sg->prev)
      sg->prev->next = sg->next;
   if (sg->next)
      sg->next->prev = sg->prev;
   if (sg == DRD_(g_threadinfo)[tid].first)
      DRD_(g_threadinfo)[tid].first = sg->next;
   if (sg == DRD_(g_threadinfo)[tid].last)
      DRD_(g_threadinfo)[tid].last = sg->prev;
   DRD_(sg_put)(sg);

#ifdef ENABLE_DRD_CONSISTENCY_CHECKS
   tl_assert(DRD_(sane_ThreadInfo)(&DRD_(g_threadinfo)[tid]));
#endif
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
   for (i = 0; i < DRD_N_THREADS; i++)
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
   for (i = 0; i < DRD_N_THREADS; i++)
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
static void thread_discard_ordered_segments(void)
{
   unsigned i;
   VectorClock thread_vc_min;

   s_discard_ordered_segments_count++;

   DRD_(vc_init)(&thread_vc_min, 0, 0);
   DRD_(thread_compute_minimum_vc)(&thread_vc_min);
   if (DRD_(sg_get_trace)())
   {
      char *vc_min, *vc_max;
      VectorClock thread_vc_max;

      DRD_(vc_init)(&thread_vc_max, 0, 0);
      DRD_(thread_compute_maximum_vc)(&thread_vc_max);
      vc_min = DRD_(vc_aprint)(&thread_vc_min);
      vc_max = DRD_(vc_aprint)(&thread_vc_max);
      VG_(message)(Vg_DebugMsg,
                   "Discarding ordered segments -- min vc is %s, max vc is %s\n",
                   vc_min, vc_max);
      VG_(free)(vc_min);
      VG_(free)(vc_max);
      DRD_(vc_cleanup)(&thread_vc_max);
   }

   for (i = 0; i < DRD_N_THREADS; i++)
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
 * An implementation of the property 'equiv(sg1, sg2)' as defined in the paper
 * by Mark Christiaens e.a. The property equiv(sg1, sg2) holds if and only if
 * all segments in the set CS are ordered consistently against both sg1 and
 * sg2. The set CS is defined as the set of segments that can immediately
 * precede future segments via inter-thread synchronization operations. In
 * DRD the set CS consists of the latest segment of each thread combined with
 * all segments for which the reference count is strictly greater than one.
 * The code below is an optimized version of the following:
 *
 * for (i = 0; i < DRD_N_THREADS; i++)
 * {
 *    Segment* sg;
 *
 *    for (sg = DRD_(g_threadinfo)[i].first; sg; sg = sg->next)
 *    {
 *       if (sg == DRD_(g_threadinfo)[i].last || DRD_(sg_get_refcnt)(sg) > 1)
 *       {
 *          if (   DRD_(vc_lte)(&sg1->vc, &sg->vc)
 *              != DRD_(vc_lte)(&sg2->vc, &sg->vc)
 *              || DRD_(vc_lte)(&sg->vc, &sg1->vc)
 *              != DRD_(vc_lte)(&sg->vc, &sg2->vc))
 *          {
 *             return False;
 *          }
 *       }
 *    }
 * }
 */
static Bool thread_consistent_segment_ordering(const DrdThreadId tid,
                                               Segment* const sg1,
                                               Segment* const sg2)
{
   unsigned i;

   tl_assert(sg1->next);
   tl_assert(sg2->next);
   tl_assert(sg1->next == sg2);
   tl_assert(DRD_(vc_lte)(&sg1->vc, &sg2->vc));

   for (i = 0; i < DRD_N_THREADS; i++)
   {
      Segment* sg;

      for (sg = DRD_(g_threadinfo)[i].first; sg; sg = sg->next)
      {
         if (! sg->next || DRD_(sg_get_refcnt)(sg) > 1)
         {
            if (DRD_(vc_lte)(&sg2->vc, &sg->vc))
               break;
            if (DRD_(vc_lte)(&sg1->vc, &sg->vc))
               return False;
         }
      }
      for (sg = DRD_(g_threadinfo)[i].last; sg; sg = sg->prev)
      {
         if (! sg->next || DRD_(sg_get_refcnt)(sg) > 1)
         {
            if (DRD_(vc_lte)(&sg->vc, &sg1->vc))
               break;
            if (DRD_(vc_lte)(&sg->vc, &sg2->vc))
               return False;
         }
      }
   }
   return True;
}

/**
 * Merge all segments that may be merged without triggering false positives
 * or discarding real data races. For the theoretical background of segment
 * merging, see also the following paper: Mark Christiaens, Michiel Ronsse
 * and Koen De Bosschere. Bounding the number of segment histories during
 * data race detection. Parallel Computing archive, Volume 28, Issue 9,
 * pp 1221-1238, September 2002. This paper contains a proof that merging
 * consecutive segments for which the property equiv(s1,s2) holds can be
 * merged without reducing the accuracy of datarace detection. Furthermore
 * it is also proven that the total number of all segments will never grow
 * unbounded if all segments s1, s2 for which equiv(s1, s2) holds are merged
 * every time a new segment is created. The property equiv(s1, s2) is defined
 * as follows: equiv(s1, s2) <=> for all segments in the set CS, the vector
 * clocks of segments s and s1 are ordered in the same way as those of segments
 * s and s2. The set CS is defined as the set of existing segments s that have
 * the potential to conflict with not yet created segments, either because the
 * segment s is the latest segment of a thread or because it can become the
 * immediate predecessor of a new segment due to a synchronization operation.
 */
static void thread_merge_segments(void)
{
   unsigned i;

   s_new_segments_since_last_merge = 0;

   for (i = 0; i < DRD_N_THREADS; i++)
   {
      Segment* sg;

#ifdef ENABLE_DRD_CONSISTENCY_CHECKS
      tl_assert(DRD_(sane_ThreadInfo)(&DRD_(g_threadinfo)[i]));
#endif

      for (sg = DRD_(g_threadinfo)[i].first; sg; sg = sg->next)
      {
         if (DRD_(sg_get_refcnt)(sg) == 1
             && sg->next
             && DRD_(sg_get_refcnt)(sg->next) == 1
             && sg->next->next
             && thread_consistent_segment_ordering(i, sg, sg->next))
         {
            /* Merge sg and sg->next into sg. */
            DRD_(sg_merge)(sg, sg->next);
            thread_discard_segment(i, sg->next);
         }
      }

#ifdef ENABLE_DRD_CONSISTENCY_CHECKS
      tl_assert(DRD_(sane_ThreadInfo)(&DRD_(g_threadinfo)[i]));
#endif
   }
}

/**
 * Create a new segment for the specified thread, and discard any segments
 * that cannot cause races anymore.
 */
void DRD_(thread_new_segment)(const DrdThreadId tid)
{
   Segment* last_sg;
   Segment* new_sg;

   tl_assert(0 <= (int)tid && tid < DRD_N_THREADS
             && tid != DRD_INVALID_THREADID);
   tl_assert(thread_conflict_set_up_to_date(DRD_(g_drd_running_tid)));

   last_sg = DRD_(g_threadinfo)[tid].last;
   new_sg = DRD_(sg_new)(tid, tid);
   thread_append_segment(tid, new_sg);
   if (tid == DRD_(g_drd_running_tid) && last_sg)
   {
      DRD_(thread_update_conflict_set)(tid, &last_sg->vc);
      s_update_conflict_set_new_sg_count++;
   }

   tl_assert(thread_conflict_set_up_to_date(DRD_(g_drd_running_tid)));

   if (s_segment_merging
       && ++s_new_segments_since_last_merge >= s_segment_merge_interval)
   {
      thread_discard_ordered_segments();
      thread_merge_segments();
   }
}

/** Call this function after thread 'joiner' joined thread 'joinee'. */
void DRD_(thread_combine_vc_join)(DrdThreadId joiner, DrdThreadId joinee)
{
   tl_assert(joiner != joinee);
   tl_assert(0 <= (int)joiner && joiner < DRD_N_THREADS
             && joiner != DRD_INVALID_THREADID);
   tl_assert(0 <= (int)joinee && joinee < DRD_N_THREADS
             && joinee != DRD_INVALID_THREADID);
   tl_assert(DRD_(g_threadinfo)[joiner].last);
   tl_assert(DRD_(g_threadinfo)[joinee].last);

   if (DRD_(sg_get_trace)())
   {
      char *str1, *str2;
      str1 = DRD_(vc_aprint)(&DRD_(g_threadinfo)[joiner].last->vc);
      str2 = DRD_(vc_aprint)(&DRD_(g_threadinfo)[joinee].last->vc);
      VG_(message)(Vg_DebugMsg, "Before join: joiner %s, joinee %s\n",
                   str1, str2);
      VG_(free)(str1);
      VG_(free)(str2);
   }
   if (joiner == DRD_(g_drd_running_tid))
   {
      VectorClock old_vc;

      DRD_(vc_copy)(&old_vc, &DRD_(g_threadinfo)[joiner].last->vc);
      DRD_(vc_combine)(&DRD_(g_threadinfo)[joiner].last->vc,
                       &DRD_(g_threadinfo)[joinee].last->vc);
      DRD_(thread_update_conflict_set)(joiner, &old_vc);
      s_update_conflict_set_join_count++;
      DRD_(vc_cleanup)(&old_vc);
   }
   else
   {
      DRD_(vc_combine)(&DRD_(g_threadinfo)[joiner].last->vc,
                       &DRD_(g_threadinfo)[joinee].last->vc);
   }

   thread_discard_ordered_segments();

   if (DRD_(sg_get_trace)())
   {
      char* str;
      str = DRD_(vc_aprint)(&DRD_(g_threadinfo)[joiner].last->vc);
      VG_(message)(Vg_DebugMsg, "After join: %s\n", str);
      VG_(free)(str);
   }
}

/**
 * Update the vector clock of the last segment of thread tid with the
 * the vector clock of segment sg.
 */
static void thread_combine_vc_sync(DrdThreadId tid, const Segment* sg)
{
   const VectorClock* const vc = &sg->vc;

   tl_assert(0 <= (int)tid && tid < DRD_N_THREADS
             && tid != DRD_INVALID_THREADID);
   tl_assert(DRD_(g_threadinfo)[tid].last);
   tl_assert(sg);
   tl_assert(vc);

   if (tid != sg->tid)
   {
      VectorClock old_vc;

      DRD_(vc_copy)(&old_vc, &DRD_(g_threadinfo)[tid].last->vc);
      DRD_(vc_combine)(&DRD_(g_threadinfo)[tid].last->vc, vc);
      if (DRD_(sg_get_trace)())
      {
         char *str1, *str2;
         str1 = DRD_(vc_aprint)(&old_vc);
         str2 = DRD_(vc_aprint)(&DRD_(g_threadinfo)[tid].last->vc);
         VG_(message)(Vg_DebugMsg, "thread %d: vc %s -> %s\n", tid, str1, str2);
         VG_(free)(str1);
         VG_(free)(str2);
      }

      thread_discard_ordered_segments();

      DRD_(thread_update_conflict_set)(tid, &old_vc);
      s_update_conflict_set_sync_count++;

      DRD_(vc_cleanup)(&old_vc);
   }
   else
   {
      tl_assert(DRD_(vc_lte)(vc, &DRD_(g_threadinfo)[tid].last->vc));
   }
}

/**
 * Create a new segment for thread tid and update the vector clock of the last
 * segment of this thread with the the vector clock of segment sg. Call this
 * function after thread tid had to wait because of thread synchronization
 * until the memory accesses in the segment sg finished.
 */
void DRD_(thread_new_segment_and_combine_vc)(DrdThreadId tid, const Segment* sg)
{
   tl_assert(0 <= (int)tid && tid < DRD_N_THREADS
             && tid != DRD_INVALID_THREADID);
   tl_assert(thread_conflict_set_up_to_date(DRD_(g_drd_running_tid)));
   tl_assert(sg);

   thread_append_segment(tid, DRD_(sg_new)(tid, tid));

   thread_combine_vc_sync(tid, sg);

   if (s_segment_merging
       && ++s_new_segments_since_last_merge >= s_segment_merge_interval)
   {
      thread_discard_ordered_segments();
      thread_merge_segments();
   }
}

/**
 * Call this function whenever a thread is no longer using the memory
 * [ a1, a2 [, e.g. because of a call to free() or a stack pointer
 * increase.
 */
void DRD_(thread_stop_using_mem)(const Addr a1, const Addr a2)
{
   unsigned i;
   Segment* p;

   for (i = 0; i < DRD_N_THREADS; i++)
      for (p = DRD_(g_threadinfo)[i].first; p; p = p->next)
         DRD_(bm_clear)(DRD_(sg_bm)(p), a1, a2);

   DRD_(bm_clear)(DRD_(g_conflict_set), a1, a2);
}

/** Specify whether memory loads should be recorded. */
void DRD_(thread_set_record_loads)(const DrdThreadId tid, const Bool enabled)
{
   tl_assert(0 <= (int)tid && tid < DRD_N_THREADS
             && tid != DRD_INVALID_THREADID);
   tl_assert(enabled == !! enabled);

   DRD_(g_threadinfo)[tid].is_recording_loads = enabled;
}

/** Specify whether memory stores should be recorded. */
void DRD_(thread_set_record_stores)(const DrdThreadId tid, const Bool enabled)
{
   tl_assert(0 <= (int)tid && tid < DRD_N_THREADS
             && tid != DRD_INVALID_THREADID);
   tl_assert(enabled == !! enabled);

   DRD_(g_threadinfo)[tid].is_recording_stores = enabled;
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

   for (i = 0; i < DRD_N_THREADS; i++)
   {
      if (DRD_(g_threadinfo)[i].first)
      {
         VG_(printf)("**************\n"
                     "* thread %3d (%d/%d/%d/%d/0x%lx/%d) *\n"
                     "**************\n",
                     i,
                     DRD_(g_threadinfo)[i].valid,
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
static void show_call_stack(const DrdThreadId tid, ExeContext* const callstack)
{
   const ThreadId vg_tid = DRD_(DrdThreadIdToVgThreadId)(tid);

   if (vg_tid != VG_INVALID_THREADID) {
      if (callstack)
         VG_(pp_ExeContext)(callstack);
      else
         VG_(get_and_pp_StackTrace)(vg_tid, VG_(clo_backtrace_size));
   } else {
      if (!VG_(clo_xml))
         VG_(message)(Vg_UserMsg,
                      "   (thread finished, call stack no longer available)\n");
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

   for (i = 0; i < DRD_N_THREADS; i++)
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
               if (DRD_(bm_has_conflict_with)(DRD_(sg_bm)(q), addr, addr + size,
                                              access_type))
               {
                  tl_assert(q->stacktrace);
                  if (VG_(clo_xml))
                     VG_(printf_xml)("  <other_segment_start>\n");
                  else
                     VG_(message)(Vg_UserMsg,
                                  "Other segment start (thread %d)\n", i);
                  show_call_stack(i, q->stacktrace);
                  if (VG_(clo_xml))
                     VG_(printf_xml)("  </other_segment_start>\n"
                                     "  <other_segment_end>\n");
                  else
                     VG_(message)(Vg_UserMsg,
                                  "Other segment end (thread %d)\n", i);
                  show_call_stack(i, q->next ? q->next->stacktrace : 0);
                  if (VG_(clo_xml))
                     VG_(printf_xml)("  </other_segment_end>\n");
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
      if (DRD_(bm_has)(DRD_(sg_bm)(p), addr, addr + size, access_type))
      {
         thread_report_conflicting_segments_segment(tid, addr, size,
                                                    access_type, p);
      }
   }
}

/**
 * Verify whether the conflict set for thread tid is up to date. Only perform
 * the check if the environment variable DRD_VERIFY_CONFLICT_SET has been set.
 */
static Bool thread_conflict_set_up_to_date(const DrdThreadId tid)
{
   static int do_verify_conflict_set = -1;
   Bool result;
   struct bitmap* computed_conflict_set = 0;

   if (do_verify_conflict_set < 0)
      do_verify_conflict_set = VG_(getenv)("DRD_VERIFY_CONFLICT_SET") != 0;

   if (do_verify_conflict_set == 0)
      return True;

   thread_compute_conflict_set(&computed_conflict_set, tid);
   result = DRD_(bm_equal)(DRD_(g_conflict_set), computed_conflict_set);
   if (! result)
   {
      VG_(printf)("actual conflict set:\n");
      DRD_(bm_print)(DRD_(g_conflict_set));
      VG_(printf)("\n");
      VG_(printf)("computed conflict set:\n");
      DRD_(bm_print)(computed_conflict_set);
      VG_(printf)("\n");
   }
   DRD_(bm_delete)(computed_conflict_set);
   return result;
}

/**
 * Compute the conflict set: a bitmap that represents the union of all memory
 * accesses of all segments that are unordered to the current segment of the
 * thread tid.
 */
static void thread_compute_conflict_set(struct bitmap** conflict_set,
                                        const DrdThreadId tid)
{
   Segment* p;

   tl_assert(0 <= (int)tid && tid < DRD_N_THREADS
             && tid != DRD_INVALID_THREADID);
   tl_assert(tid == DRD_(g_drd_running_tid));

   s_compute_conflict_set_count++;
   s_conflict_set_bitmap_creation_count
      -= DRD_(bm_get_bitmap_creation_count)();
   s_conflict_set_bitmap2_creation_count
      -= DRD_(bm_get_bitmap2_creation_count)();

   if (*conflict_set)
   {
      DRD_(bm_cleanup)(*conflict_set);
      DRD_(bm_init)(*conflict_set);
   }
   else
   {
      *conflict_set = DRD_(bm_new)();
   }

   if (s_trace_conflict_set)
   {
      char* str;

      str = DRD_(vc_aprint)(&DRD_(g_threadinfo)[tid].last->vc);
      VG_(message)(Vg_DebugMsg,
                   "computing conflict set for thread %d with vc %s\n",
                   tid, str);
      VG_(free)(str);
   }

   p = DRD_(g_threadinfo)[tid].last;
   {
      unsigned j;

      if (s_trace_conflict_set)
      {
         char* vc;

         vc = DRD_(vc_aprint)(&p->vc);
         VG_(message)(Vg_DebugMsg, "conflict set: thread [%d] at vc %s\n",
                      tid, vc);
         VG_(free)(vc);
      }

      for (j = 0; j < DRD_N_THREADS; j++)
      {
         if (j != tid && DRD_(IsValidDrdThreadId)(j))
         {
            Segment* q;
            for (q = DRD_(g_threadinfo)[j].last; q; q = q->prev)
            {
               if (! DRD_(vc_lte)(&q->vc, &p->vc)
                   && ! DRD_(vc_lte)(&p->vc, &q->vc))
               {
                  if (s_trace_conflict_set)
                  {
                     char* str;

                     str = DRD_(vc_aprint)(&q->vc);
                     VG_(message)(Vg_DebugMsg,
                                  "conflict set: [%d] merging segment %s\n",
                                  j, str);
                     VG_(free)(str);
                  }
                  DRD_(bm_merge2)(*conflict_set, DRD_(sg_bm)(q));
               }
               else
               {
                  if (s_trace_conflict_set)
                  {
                     char* str;

                     str = DRD_(vc_aprint)(&q->vc);
                     VG_(message)(Vg_DebugMsg,
                                  "conflict set: [%d] ignoring segment %s\n",
                                  j, str);
                     VG_(free)(str);
                  }
               }
            }
         }
      }
   }

   s_conflict_set_bitmap_creation_count
      += DRD_(bm_get_bitmap_creation_count)();
   s_conflict_set_bitmap2_creation_count
      += DRD_(bm_get_bitmap2_creation_count)();

   if (s_trace_conflict_set_bm)
   {
      VG_(message)(Vg_DebugMsg, "[%d] new conflict set:\n", tid);
      DRD_(bm_print)(*conflict_set);
      VG_(message)(Vg_DebugMsg, "[%d] end of new conflict set.\n", tid);
   }
}

/**
 * Update the conflict set after the vector clock of thread tid has been
 * updated from old_vc to its current value, either because a new segment has
 * been created or because of a synchronization operation.
 */
void DRD_(thread_update_conflict_set)(const DrdThreadId tid,
                                      const VectorClock* const old_vc)
{
   const VectorClock* new_vc;
   Segment* p;
   unsigned j;

   tl_assert(0 <= (int)tid && tid < DRD_N_THREADS
             && tid != DRD_INVALID_THREADID);
   tl_assert(old_vc);
   tl_assert(tid == DRD_(g_drd_running_tid));
   tl_assert(DRD_(g_conflict_set));

   if (s_trace_conflict_set)
   {
      char* str;

      str = DRD_(vc_aprint)(&DRD_(g_threadinfo)[tid].last->vc);
      VG_(message)(Vg_DebugMsg,
                   "updating conflict set for thread %d with vc %s\n",
                   tid, str);
      VG_(free)(str);
   }

   new_vc = &DRD_(g_threadinfo)[tid].last->vc;
   tl_assert(DRD_(vc_lte)(old_vc, new_vc));

   DRD_(bm_unmark)(DRD_(g_conflict_set));

   for (j = 0; j < DRD_N_THREADS; j++)
   {
      Segment* q;

      if (j == tid || ! DRD_(IsValidDrdThreadId)(j))
         continue;

      for (q = DRD_(g_threadinfo)[j].last;
           q && !DRD_(vc_lte)(&q->vc, new_vc);
           q = q->prev) {
         const Bool included_in_old_conflict_set
            = !DRD_(vc_lte)(old_vc, &q->vc);
         const Bool included_in_new_conflict_set
            = !DRD_(vc_lte)(new_vc, &q->vc);

         if (UNLIKELY(s_trace_conflict_set)) {
            char* str;

            str = DRD_(vc_aprint)(&q->vc);
            VG_(message)(Vg_DebugMsg,
                         "conflict set: [%d] %s segment %s\n", j,
                         included_in_old_conflict_set
                         != included_in_new_conflict_set
                         ? "merging" : "ignoring", str);
            VG_(free)(str);
         }
         if (included_in_old_conflict_set != included_in_new_conflict_set)
            DRD_(bm_mark)(DRD_(g_conflict_set), DRD_(sg_bm)(q));
      }

      for ( ; q && !DRD_(vc_lte)(&q->vc, old_vc); q = q->prev) {
         const Bool included_in_old_conflict_set
            = !DRD_(vc_lte)(old_vc, &q->vc);
         const Bool included_in_new_conflict_set
            = !DRD_(vc_lte)(&q->vc, new_vc)
            && !DRD_(vc_lte)(new_vc, &q->vc);

         if (UNLIKELY(s_trace_conflict_set)) {
            char* str;

            str = DRD_(vc_aprint)(&q->vc);
            VG_(message)(Vg_DebugMsg,
                         "conflict set: [%d] %s segment %s\n", j,
                         included_in_old_conflict_set
                         != included_in_new_conflict_set
                         ? "merging" : "ignoring", str);
            VG_(free)(str);
         }
         if (included_in_old_conflict_set != included_in_new_conflict_set)
            DRD_(bm_mark)(DRD_(g_conflict_set), DRD_(sg_bm)(q));
      }
   }

   DRD_(bm_clear_marked)(DRD_(g_conflict_set));

   p = DRD_(g_threadinfo)[tid].last;
   for (j = 0; j < DRD_N_THREADS; j++)
   {
      if (j != tid && DRD_(IsValidDrdThreadId)(j))
      {
         Segment* q;
         for (q = DRD_(g_threadinfo)[j].last;
              q && !DRD_(vc_lte)(&q->vc, &p->vc);
              q = q->prev) {
            if (!DRD_(vc_lte)(&p->vc, &q->vc))
               DRD_(bm_merge2_marked)(DRD_(g_conflict_set), DRD_(sg_bm)(q));
         }
      }
   }

   DRD_(bm_remove_cleared_marked)(DRD_(g_conflict_set));

   s_update_conflict_set_count++;

   if (s_trace_conflict_set_bm)
   {
      VG_(message)(Vg_DebugMsg, "[%d] updated conflict set:\n", tid);
      DRD_(bm_print)(DRD_(g_conflict_set));
      VG_(message)(Vg_DebugMsg, "[%d] end of updated conflict set.\n", tid);
   }

   tl_assert(thread_conflict_set_up_to_date(DRD_(g_drd_running_tid)));
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

/** Return how many times the conflict set has been updated entirely. */
ULong DRD_(thread_get_compute_conflict_set_count)()
{
   return s_compute_conflict_set_count;
}

/** Return how many times the conflict set has been updated partially. */
ULong DRD_(thread_get_update_conflict_set_count)(void)
{
   return s_update_conflict_set_count;
}

/**
 * Return how many times the conflict set has been updated partially
 * because a new segment has been created.
 */
ULong DRD_(thread_get_update_conflict_set_new_sg_count)(void)
{
   return s_update_conflict_set_new_sg_count;
}

/**
 * Return how many times the conflict set has been updated partially
 * because of combining vector clocks due to synchronization operations
 * other than reader/writer lock or barrier operations.
 */
ULong DRD_(thread_get_update_conflict_set_sync_count)(void)
{
   return s_update_conflict_set_sync_count;
}

/**
 * Return how many times the conflict set has been updated partially
 * because of thread joins.
 */
ULong DRD_(thread_get_update_conflict_set_join_count)(void)
{
   return s_update_conflict_set_join_count;
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
