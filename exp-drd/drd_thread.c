/*
  This file is part of drd, a data race detector.

  Copyright (C) 2006-2007 Bart Van Assche
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
#include "pub_core_options.h"     // VG_(clo_backtrace_size)
#include "pub_tool_basics.h"      // Addr, SizeT
#include "pub_tool_errormgr.h"    // VG_(unique_error)()
#include "pub_tool_libcassert.h"  // tl_assert()
#include "pub_tool_libcbase.h"    // VG_(strlen)()
#include "pub_tool_libcprint.h"   // VG_(printf)()
#include "pub_tool_machine.h"
#include "pub_tool_mallocfree.h"  // VG_(malloc)(), VG_(free)()
#include "pub_tool_threadstate.h" // VG_(get_pthread_id)()


// Defines.

#define DRD_N_THREADS VG_N_THREADS


// Type definitions.

typedef struct
{
   Segment*  first;
   Segment*  last;
   ThreadId  vg_threadid;
   PThreadId pt_threadid;
   Addr      stack_min_min;
   Addr      stack_min;
   Addr      stack_startup;
   Addr      stack_max;
   char      name[32];
   /// Indicates whether the Valgrind core knows about this thread.
   Bool      vg_thread_exists;
   /// Indicates whether there is an associated POSIX thread ID.
   Bool      posix_thread_exists;
   /// If true, indicates that there is a corresponding POSIX thread ID and
   /// a corresponding OS thread that is detached.
   Bool      detached_posix_thread;
   Bool      is_recording;
} ThreadInfo;


// Local functions.

static void thread_append_segment(const DrdThreadId tid,
                                  Segment* const sg);
static void thread_update_danger_set(const DrdThreadId tid);


// Local variables.

static ULong s_context_switch_count;
static ULong s_discard_ordered_segments_count;
#ifdef OLD_RACE_DETECTION_ALGORITHM
static ULong s_report_races_count;
#endif
static ULong s_update_danger_set_count;
static ULong s_danger_set_bitmap_creation_count;
static ULong s_danger_set_bitmap2_creation_count;
static ThreadId    s_vg_running_tid  = VG_INVALID_THREADID;
static DrdThreadId s_drd_running_tid = DRD_INVALID_THREADID;
static ThreadInfo s_threadinfo[DRD_N_THREADS];
static struct bitmap* s_danger_set;


// Function definitions.

__inline__ Bool IsValidDrdThreadId(const DrdThreadId tid)
{
   return (0 <= tid && tid < DRD_N_THREADS && tid != DRD_INVALID_THREADID
           && ! (s_threadinfo[tid].vg_thread_exists == False
                 && s_threadinfo[tid].posix_thread_exists == False
                 && s_threadinfo[tid].detached_posix_thread == False));
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
         s_threadinfo[i].stack_min_min = 0;
         s_threadinfo[i].stack_min     = 0;
         s_threadinfo[i].stack_startup = 0;
         s_threadinfo[i].stack_max     = 0;
         VG_(snprintf)(s_threadinfo[i].name, sizeof(s_threadinfo[i].name),
                       "thread %d", tid);
         s_threadinfo[i].name[sizeof(s_threadinfo[i].name) - 1] = 0;
         s_threadinfo[i].is_recording  = True;
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
   tl_assert(0 <= tid && tid < DRD_N_THREADS && tid != DRD_INVALID_THREADID);
   return (s_threadinfo[tid].vg_thread_exists
           ? s_threadinfo[tid].vg_threadid
           : VG_INVALID_THREADID);
}

/**
 * Sanity check of the doubly linked list of segments referenced by a ThreadInfo struct.
 * @return True if sane, False if not.
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

DrdThreadId thread_pre_create(const DrdThreadId creator,
                              const ThreadId vg_created)
{
   DrdThreadId created;

   tl_assert(VgThreadIdToDrdThreadId(vg_created) == DRD_INVALID_THREADID);
   created = VgThreadIdToNewDrdThreadId(vg_created);
   tl_assert(0 <= created && created < DRD_N_THREADS
             && created != DRD_INVALID_THREADID);

   tl_assert(s_threadinfo[created].first == 0);
   tl_assert(s_threadinfo[created].last == 0);
   thread_append_segment(created, sg_new(creator, created));

   return created;
}

/**
 * Allocate the first segment for a thread. Call this just after
 * pthread_create().
 */
DrdThreadId thread_post_create(const ThreadId vg_created)
{
   const DrdThreadId created = VgThreadIdToDrdThreadId(vg_created);

   tl_assert(0 <= created && created < DRD_N_THREADS
             && created != DRD_INVALID_THREADID);

   s_threadinfo[created].stack_max     = VG_(thread_get_stack_max)(vg_created);
   s_threadinfo[created].stack_startup = s_threadinfo[created].stack_max;
   s_threadinfo[created].stack_min     = s_threadinfo[created].stack_max;
   s_threadinfo[created].stack_min_min = s_threadinfo[created].stack_max;
   tl_assert(s_threadinfo[created].stack_max != 0);

   return created;
}

/* NPTL hack: NPTL allocates the 'struct pthread' on top of the stack,     */
/* and accesses this data structure from multiple threads without locking. */
/* Any conflicting accesses in the range stack_startup..stack_max will be  */
/* ignored.                                                                */
void thread_set_stack_startup(const DrdThreadId tid, const Addr stack_startup)
{
#if 0
   VG_(message)(Vg_DebugMsg, "thread_set_stack_startup: thread %d (%d)"
                " stack 0x%x .. 0x%lx (size %d)",
                s_threadinfo[tid].vg_threadid, tid,
                stack_startup,
                s_threadinfo[tid].stack_max,
                s_threadinfo[tid].stack_max - stack_startup);
#endif
   tl_assert(0 <= tid && tid < DRD_N_THREADS && tid != DRD_INVALID_THREADID);
   tl_assert(s_threadinfo[tid].stack_min <= stack_startup);
   tl_assert(stack_startup <= s_threadinfo[tid].stack_max);
   s_threadinfo[tid].stack_startup = stack_startup;
}

Addr thread_get_stack_min(const DrdThreadId tid)
{
   tl_assert(0 <= tid && tid < DRD_N_THREADS
             && tid != DRD_INVALID_THREADID);
   return s_threadinfo[tid].stack_min;
}

void thread_set_stack_min(const DrdThreadId tid, const Addr stack_min)
{
#if 0
   VG_(message)(Vg_DebugMsg, "thread %d (%d) stack_min = 0x%x"
                " (size %d, max %d, delta %d)",
                s_threadinfo[tid].vg_threadid, tid,
                stack_min,
                s_threadinfo[tid].stack_max - stack_min,
                s_threadinfo[tid].stack_max - s_threadinfo[tid].stack_min_min,
                s_threadinfo[tid].stack_min - stack_min);
#endif
   tl_assert(0 <= tid && tid < DRD_N_THREADS && tid != DRD_INVALID_THREADID);
   if (s_threadinfo[tid].stack_max)
   {
      s_threadinfo[tid].stack_min = stack_min;
      if (stack_min < s_threadinfo[tid].stack_min_min)
      {
         s_threadinfo[tid].stack_min_min = stack_min;
      }
      tl_assert(s_threadinfo[tid].stack_min_min
                <= s_threadinfo[tid].stack_min);
      tl_assert(s_threadinfo[tid].stack_min < s_threadinfo[tid].stack_max);
   }
}

DrdThreadId thread_lookup_stackaddr(const Addr a,
                                    Addr* const stack_min,
                                    Addr* const stack_max)
{
   unsigned i;
   for (i = 0; i < sizeof(s_threadinfo) / sizeof(s_threadinfo[0]); i++)
   {
      if (s_threadinfo[i].stack_min <= a && a <= s_threadinfo[i].stack_max)
      {
         *stack_min = s_threadinfo[i].stack_min;
         *stack_max = s_threadinfo[i].stack_max;
         return i;
      }
   }
   return DRD_INVALID_THREADID;
}

/**
 * Clean up thread-specific data structures. Call this just after 
 * pthread_join().
 */
void thread_delete(const DrdThreadId tid)
{
   Segment* sg;
   Segment* sg_prev;

   tl_assert(0 <= tid && tid < DRD_N_THREADS
             && tid != DRD_INVALID_THREADID);
   for (sg = s_threadinfo[tid].last; sg; sg = sg_prev)
   {
      sg_prev = sg->prev;
      sg_delete(sg);
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
   tl_assert(0 <= tid && tid < DRD_N_THREADS
             && tid != DRD_INVALID_THREADID);

   thread_stop_using_mem(s_threadinfo[tid].stack_min,
                         s_threadinfo[tid].stack_max);

   s_threadinfo[tid].vg_thread_exists = False;

   if (s_threadinfo[tid].detached_posix_thread)
   {
      /* Once a detached thread has finished, its stack is deallocated and   */
      /* should no longer be taken into account when computing the danger set*/
      s_threadinfo[tid].stack_min = s_threadinfo[tid].stack_max;

      /* For a detached thread, calling pthread_exit() invalidates the     */
      /* POSIX thread ID associated with the detached thread. For joinable */
      /* POSIX threads however, the POSIX thread ID remains live after the */
      /* pthread_exit() call until pthread_join() is called.               */
      s_threadinfo[tid].posix_thread_exists = False;
   }
}

void thread_set_pthreadid(const DrdThreadId tid, const PThreadId ptid)
{
   tl_assert(0 <= tid && tid < DRD_N_THREADS
             && tid != DRD_INVALID_THREADID);
   tl_assert(s_threadinfo[tid].pt_threadid == INVALID_POSIX_THREADID);
   tl_assert(ptid != INVALID_POSIX_THREADID);
   s_threadinfo[tid].posix_thread_exists = True;
   s_threadinfo[tid].pt_threadid         = ptid;
}

Bool thread_get_joinable(const DrdThreadId tid)
{
   tl_assert(0 <= tid && tid < DRD_N_THREADS
             && tid != DRD_INVALID_THREADID);
   return ! s_threadinfo[tid].detached_posix_thread;
}

void thread_set_joinable(const DrdThreadId tid, const Bool joinable)
{
   tl_assert(0 <= tid && tid < DRD_N_THREADS
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

const char* thread_get_name(const DrdThreadId tid)
{
   tl_assert(0 <= tid && tid < DRD_N_THREADS
             && tid != DRD_INVALID_THREADID);
   return s_threadinfo[tid].name;
}

void thread_set_name(const DrdThreadId tid, const char* const name)
{
   tl_assert(0 <= tid && tid < DRD_N_THREADS
             && tid != DRD_INVALID_THREADID);
   VG_(strncpy)(s_threadinfo[tid].name, name,
                sizeof(s_threadinfo[tid].name));
   s_threadinfo[tid].name[sizeof(s_threadinfo[tid].name) - 1] = 0;
}

void thread_set_name_fmt(const DrdThreadId tid, const char* const fmt,
                         const UWord arg)
{
   tl_assert(0 <= tid && tid < DRD_N_THREADS
             && tid != DRD_INVALID_THREADID);
   VG_(snprintf)(s_threadinfo[tid].name, sizeof(s_threadinfo[tid].name),
                 fmt, arg);
   s_threadinfo[tid].name[sizeof(s_threadinfo[tid].name) - 1] = 0;
}

DrdThreadId thread_get_running_tid(void)
{
   // HACK. To do: remove the if-statement and keep the assert.
   if (VG_(get_running_tid)() != VG_INVALID_THREADID)
      tl_assert(VG_(get_running_tid)() == s_vg_running_tid);
   tl_assert(s_drd_running_tid != DRD_INVALID_THREADID);
   return s_drd_running_tid;
}

void thread_set_vg_running_tid(const ThreadId vg_tid)
{
   // HACK. To do: uncomment the line below.
   // tl_assert(vg_tid != VG_INVALID_THREADID);

   if (vg_tid != s_vg_running_tid)
   {
      thread_set_running_tid(vg_tid, VgThreadIdToDrdThreadId(vg_tid));
   }

   tl_assert(s_vg_running_tid != VG_INVALID_THREADID);
   tl_assert(s_drd_running_tid != DRD_INVALID_THREADID);
}

void thread_set_running_tid(const ThreadId vg_tid, const DrdThreadId drd_tid)
{
   // HACK. To do: remove the next two lines.
   if (vg_tid == VG_INVALID_THREADID)
      return;

   tl_assert(vg_tid != VG_INVALID_THREADID);
   tl_assert(drd_tid != DRD_INVALID_THREADID);
   
   if (vg_tid != s_vg_running_tid)
   {
      s_vg_running_tid = vg_tid;
      s_drd_running_tid = drd_tid;
      thread_update_danger_set(drd_tid);
      s_context_switch_count++;
   }

   tl_assert(s_vg_running_tid != VG_INVALID_THREADID);
   tl_assert(s_drd_running_tid != DRD_INVALID_THREADID);
}

/**
 * Return a pointer to the latest segment for the specified thread.
 */
Segment* thread_get_segment(const DrdThreadId tid)
{
   tl_assert(0 <= tid && tid < DRD_N_THREADS
             && tid != DRD_INVALID_THREADID);
   if (s_threadinfo[tid].last == 0)
   {
      VG_(message)(Vg_DebugMsg, "threadid = %d", tid);
      thread_print_all();
   }
   tl_assert(s_threadinfo[tid].last);
   return s_threadinfo[tid].last;
}

/**
 * Insert a new segment at the end of the segment list.
 */
static void thread_append_segment(const DrdThreadId tid,
                                  Segment* const sg)
{
   tl_assert(0 <= tid && tid < DRD_N_THREADS
             && tid != DRD_INVALID_THREADID);
   tl_assert(sane_ThreadInfo(&s_threadinfo[tid]));
   sg->prev = s_threadinfo[tid].last;
   sg->next = 0;
   if (s_threadinfo[tid].last)
      s_threadinfo[tid].last->next = sg;
   s_threadinfo[tid].last = sg;
   if (s_threadinfo[tid].first == 0)
      s_threadinfo[tid].first = sg;
   tl_assert(sane_ThreadInfo(&s_threadinfo[tid]));
}

/**
 * Remove a segment from the segment list of thread threadid, and free the
 * associated memory.
 */
static void thread_discard_segment(const DrdThreadId tid,
                                   Segment* const sg)
{
   tl_assert(0 <= tid && tid < DRD_N_THREADS
             && tid != DRD_INVALID_THREADID);
   tl_assert(sane_ThreadInfo(&s_threadinfo[tid]));
   if (sg->prev)
      sg->prev->next = sg->next;
   if (sg->next)
      sg->next->prev = sg->prev;
   if (sg == s_threadinfo[tid].first)
      s_threadinfo[tid].first = sg->next;
   if (sg == s_threadinfo[tid].last)
      s_threadinfo[tid].last = sg->prev;
   sg_delete(sg);
   tl_assert(sane_ThreadInfo(&s_threadinfo[tid]));
}

VectorClock* thread_get_vc(const DrdThreadId tid)
{
   tl_assert(0 <= tid && tid < DRD_N_THREADS
             && tid != DRD_INVALID_THREADID);
   tl_assert(s_threadinfo[tid].last);
   return &s_threadinfo[tid].last->vc;
}

/**
 * Compute the minimum of all latest vector clocks of all threads
 * (Michiel Ronsse calls this "clock snooping" in his papers about DIOTA).
 * @param vc pointer to a vectorclock, holds result upon return.
 */
static void thread_compute_minimum_vc(VectorClock* vc)
{
   int i;
   Bool first;
   Segment* latest_sg;

   first = True;
   for (i = 0; i < sizeof(s_threadinfo) / sizeof(s_threadinfo[0]); i++)
   {
      latest_sg = s_threadinfo[i].last;
      if (latest_sg)
      {
         if (first)
         {
            vc_cleanup(vc);
            vc_copy(vc, &latest_sg->vc);
         }
         else
            vc_min(vc, &latest_sg->vc);
         first = False;
      }
   }
}

static void thread_compute_maximum_vc(VectorClock* vc)
{
   int i;
   Bool first;
   Segment* latest_sg;

   first = True;
   for (i = 0; i < sizeof(s_threadinfo) / sizeof(s_threadinfo[0]); i++)
   {
      latest_sg = s_threadinfo[i].last;
      if (latest_sg)
      {
         if (first)
         {
            vc_cleanup(vc);
            vc_copy(vc, &latest_sg->vc);
         }
         else
            vc_combine(vc, &latest_sg->vc);
         first = False;
      }
   }
}

/**
 * Discard all segments that have a defined ordered against the latest vector
 * clock of every thread -- these segments can no longer be involved in a
 * data race.
 */
static void thread_discard_ordered_segments(void)
{
   VectorClock thread_vc_min;
   int i;

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
      VG_(message)(Vg_DebugMsg, "%s", msg);
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
#if 0
         VG_(printf)("Discarding a segment of thread %d: ", i);
         vc_print(&sg->vc);
         VG_(printf)("\n");
#endif
         thread_discard_segment(i, sg);
      }
   }
   vc_cleanup(&thread_vc_min);
}

/**
 * Create a new segment for the specified thread, and report all data races
 * of the most recent thread segment with other threads.
 */
void thread_new_segment(const DrdThreadId tid)
{
   //static int s_calls_since_last_discard = 0;
   Segment* sg;

   tl_assert(0 <= tid && tid < DRD_N_THREADS
             && tid != DRD_INVALID_THREADID);

#ifdef OLD_RACE_DETECTION_ALGORITHM
   if (s_threadinfo[tid].last)
   {
      thread_report_races_segment(tid, s_threadinfo[tid].last);
   }
#endif

   sg = sg_new(tid, tid);
   thread_append_segment(tid, sg);

   thread_discard_ordered_segments();
}

void thread_combine_vc(DrdThreadId joiner, DrdThreadId joinee)
{
   tl_assert(joiner != joinee);
   tl_assert(0 <= joiner && joiner < DRD_N_THREADS
             && joiner != DRD_INVALID_THREADID);
   tl_assert(0 <= joinee && joinee < DRD_N_THREADS
             && joinee != DRD_INVALID_THREADID);
   tl_assert(s_threadinfo[joiner].last);
   tl_assert(s_threadinfo[joinee].last);
   vc_combine(&s_threadinfo[joiner].last->vc, &s_threadinfo[joinee].last->vc);
   thread_discard_ordered_segments();

   if (joiner == s_drd_running_tid)
   {
      thread_update_danger_set(joiner);
   }
}

void thread_combine_vc2(DrdThreadId tid, const VectorClock* const vc)
{
   tl_assert(0 <= tid && tid < DRD_N_THREADS && tid != DRD_INVALID_THREADID);
   tl_assert(s_threadinfo[tid].last);
   tl_assert(vc);
   vc_combine(&s_threadinfo[tid].last->vc, vc);
   thread_discard_ordered_segments();
}

void thread_stop_using_mem(const Addr a1, const Addr a2)
{
   DrdThreadId other_user = DRD_INVALID_THREADID;

   /* For all threads, mark the range [a,a+size[ as no longer in use. */

   unsigned i;
   for (i = 0; i < sizeof(s_threadinfo) / sizeof(s_threadinfo[0]); i++)
   {
      Segment* p;
      for (p = s_threadinfo[i].first; p; p = p->next)
      {
         if (other_user == DRD_INVALID_THREADID
             && i != s_drd_running_tid
             && bm_has_any_access(p->bm, a1, a2))
         {
            other_user = i;
         }
         bm_clear(p->bm, a1, a2);
      }
   }

   /* If any other thread had accessed memory in [a,a+size[, update the */
   /* danger set. */
   if (other_user != DRD_INVALID_THREADID
       && bm_has_any_access(s_danger_set, a1, a2))
   {
#if 0
      VG_(message)(Vg_DebugMsg,
                   "recalculating danger set because thread %d / %d stopped"
                   " using memory at 0x%x sz %d",
                   other_user,
                   s_threadinfo[other_user].vg_threadid,
                   a1,
                   a2 - a1);
#endif
      thread_update_danger_set(thread_get_running_tid());
   }
}

void thread_start_recording(const DrdThreadId tid)
{
   tl_assert(0 <= tid && tid < DRD_N_THREADS && tid != DRD_INVALID_THREADID);
   tl_assert(! s_threadinfo[tid].is_recording);
   s_threadinfo[tid].is_recording = True;
}

void thread_stop_recording(const DrdThreadId tid)
{
   tl_assert(0 <= tid && tid < DRD_N_THREADS && tid != DRD_INVALID_THREADID);
   tl_assert(s_threadinfo[tid].is_recording);
   s_threadinfo[tid].is_recording = False;
}

Bool thread_is_recording(const DrdThreadId tid)
{
   tl_assert(0 <= tid && tid < DRD_N_THREADS && tid != DRD_INVALID_THREADID);
   return s_threadinfo[tid].is_recording;
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
                     "* thread %3d (%d/%d/%d/0x%x/%d/%s) *\n"
                     "**************\n",
                     i,
                     s_threadinfo[i].vg_thread_exists,
                     s_threadinfo[i].vg_threadid,
                     s_threadinfo[i].posix_thread_exists,
                     s_threadinfo[i].pt_threadid,
                     s_threadinfo[i].detached_posix_thread,
                     s_threadinfo[i].name);
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

   VG_(message)(Vg_UserMsg,
                "%s (%s)",
                msg,
                thread_get_name(tid));

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

#ifdef OLD_RACE_DETECTION_ALGORITHM
void thread_report_races(const DrdThreadId threadid)
{
   Segment* p;

   s_report_races_count++;

   tl_assert(0 <= threadid && threadid < DRD_N_THREADS
             && threadid != DRD_INVALID_THREADID);

   for (p = s_threadinfo[threadid].first; p; p = p->next)
   {
      thread_report_races_segment(threadid, p);
   }
}

/**
 * Report all data races for segment p of thread threadid against other
 * threads.
 */
void thread_report_races_segment(const DrdThreadId threadid,
                                 Segment* const p)
{
   unsigned i;

   tl_assert(0 <= threadid && threadid < DRD_N_THREADS
             && threadid != DRD_INVALID_THREADID);
   tl_assert(p);

   for (i = 0; i < sizeof(s_threadinfo) / sizeof(s_threadinfo[0]); i++)
   {
      if (i != threadid)
      {
         Segment* q;
         for (q = s_threadinfo[i].last; q; q = q->prev)
         {
#if 0
            char msg[256];
            VG_(snprintf)(msg, sizeof(msg), "Examining thread %d (vc ", threadid);
            vc_snprint(msg + VG_(strlen)(msg), sizeof(msg) - VG_(strlen)(msg),
                       &p->vc);
            VG_(snprintf)(msg + VG_(strlen)(msg), sizeof(msg) - VG_(strlen)(msg),
                          ") versus thread %d (vc ", i);
            vc_snprint(msg + VG_(strlen)(msg), sizeof(msg) - VG_(strlen)(msg),
                       &q->vc);
            VG_(snprintf)(msg + VG_(strlen)(msg), sizeof(msg) - VG_(strlen)(msg),
                          ") %d %d",
                          vc_lte(&p->vc, &q->vc), vc_lte(&q->vc, &p->vc));
            VG_(message)(Vg_DebugMsg, "%s", msg);
#endif
            // Since q iterates over the segments of thread i in order of 
            // decreasing vector clocks, if q->vc <= p->vc, then 
            // q->next->vc <= p->vc will also hold. Hence, break out of the
            // loop once this condition is met.
            if (vc_lte(&q->vc, &p->vc))
               break;
            if (! vc_lte(&p->vc, &q->vc))
            {
               if (bm_has_races(p->bm, q->bm))
               {
                  VG_(message)(Vg_UserMsg, "----------------------------------------------------------------------");
                  tl_assert(p->stacktrace);
                  show_call_stack(threadid, "1st segment start",
                                  p->stacktrace);
                  show_call_stack(threadid, "1st segment end",
                                  p->next ? p->next->stacktrace : 0);
                  tl_assert(q->stacktrace);
                  show_call_stack(i,        "2nd segment start",
                                  q->stacktrace);
                  show_call_stack(i,        "2nd segment end",
                                  q->next ? q->next->stacktrace : 0);
                  bm_report_races(threadid, i, p->bm, q->bm);
               }
            }
         }
      }
   }
}

/**
 * Report all detected data races for all threads.
 */
void thread_report_all_races(void)
{
   unsigned i;

   for (i = 0; i < sizeof(s_threadinfo) / sizeof(s_threadinfo[0]); i++)
   {
      if (s_threadinfo[i].last)
      {
         thread_report_races(i);
      }
   }
}
#else
static void
thread_report_conflicting_segments_segment(const DrdThreadId tid,
                                           const Addr addr,
                                           const SizeT size,
                                           const BmAccessTypeT access_type,
                                           const Segment* const p)
{
   unsigned i;

   tl_assert(0 <= tid && tid < DRD_N_THREADS
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

   tl_assert(0 <= tid && tid < DRD_N_THREADS
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
#endif

/**
 * Compute a bitmap that represents the union of all memory accesses of all
 * segments that are unordered to the current segment of the thread tid.
 */
static void thread_update_danger_set(const DrdThreadId tid)
{
   Segment* p;

   tl_assert(0 <= tid && tid < DRD_N_THREADS
             && tid != DRD_INVALID_THREADID);
   tl_assert(tid == s_drd_running_tid);

   s_update_danger_set_count++;
   s_danger_set_bitmap_creation_count  -= bm_get_bitmap_creation_count();
   s_danger_set_bitmap2_creation_count -= bm_get_bitmap2_creation_count();

#if 0
   if (s_danger_set)
   {
      bm_delete(s_danger_set);
      s_danger_set = 0;
   }
   s_danger_set = bm_new();
#else
   // Marginally faster than the above code.
   if (s_danger_set)
   {
      bm_clear_all(s_danger_set);
   }
   else
   {
      s_danger_set = bm_new();
   }
#endif

   for (p = s_threadinfo[tid].first; p; p = p->next)
   {
      unsigned j;

      for (j = 0; j < sizeof(s_threadinfo) / sizeof(s_threadinfo[0]); j++)
      {
         if (IsValidDrdThreadId(j))
         {
            const Segment* const q = s_threadinfo[j].last;
            if (j != tid && q != 0
                && ! vc_lte(&q->vc, &p->vc) && ! vc_lte(&p->vc, &q->vc))
            {
               bm_merge2(s_danger_set, q->bm);
            }

         }
      }

      for (j = 0; j < sizeof(s_threadinfo) / sizeof(s_threadinfo[0]); j++)
      {
         if (IsValidDrdThreadId(j))
         {
            // NPTL hack: don't report data races on sizeof(struct pthread)
            // bytes at the top of the stack, since the NPTL functions access
            // this data without locking.
            if (s_threadinfo[j].stack_min != 0)
            {
               tl_assert(s_threadinfo[j].stack_startup != 0);
               if (s_threadinfo[j].stack_min < s_threadinfo[j].stack_startup)
               {
                  bm_clear(s_danger_set,
                           s_threadinfo[j].stack_min,
                           s_threadinfo[j].stack_startup);
               }
            }
         }
      }
   }

   s_danger_set_bitmap_creation_count  += bm_get_bitmap_creation_count();
   s_danger_set_bitmap2_creation_count += bm_get_bitmap2_creation_count();

#if 0
   VG_(message)(Vg_DebugMsg, "[%d] new danger set:", tid);
   bm_print(s_danger_set);
   VG_(message)(Vg_DebugMsg, "[%d] end of new danger set.", tid);
#endif
}

Bool thread_conflicting_access(const Addr a,
                               const SizeT size,
                               const BmAccessTypeT access_type)
{
   tl_assert(s_danger_set);
   return (bm_has_conflict_with(s_danger_set, a, a + size, access_type)
           && ! drd_is_suppressed(a, a + size));
}

ULong thread_get_context_switch_count(void)
{
   return s_context_switch_count;
}

#ifdef OLD_RACE_DETECTION_ALGORITHM
ULong thread_get_report_races_count(void)
{
   return s_report_races_count;
}
#endif

ULong thread_get_discard_ordered_segments_count(void)
{
   return s_discard_ordered_segments_count;
}

ULong thread_get_update_danger_set_count(void)
{
   return s_update_danger_set_count;
}

ULong thread_get_danger_set_bitmap_creation_count(void)
{
   return s_danger_set_bitmap_creation_count;
}

ULong thread_get_danger_set_bitmap2_creation_count(void)
{
   return s_danger_set_bitmap2_creation_count;
}

/*
 * Local variables:
 * c-basic-offset: 3
 * End:
 */
