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


#ifndef __THREAD_H
#define __THREAD_H


// Includes.

#include "drd_segment.h"
#include "pub_drd_bitmap.h"
#include "pub_tool_libcassert.h"  // tl_assert()
#include "pub_tool_stacktrace.h"  // StackTrace
#include "pub_tool_threadstate.h" // VG_N_THREADS


// Defines.

#define DRD_N_THREADS VG_N_THREADS

#define DRD_INVALID_THREADID 0

/* Note: the PThreadId typedef and the INVALID_POSIX_THREADID depend on the */
/* operating system and threading library in use. PThreadId must contain at */
/* least the same number of bits as pthread_t, and INVALID_POSIX_THREADID   */
/* must be a value that will never be returned by pthread_self().           */

#define INVALID_POSIX_THREADID ((PThreadId)0)


// Type definitions.

typedef UInt DrdThreadId;
typedef UWord PThreadId;

typedef struct
{
  Segment*  first;
  Segment*  last;
  ThreadId  vg_threadid;
  PThreadId pt_threadid;
  Addr      stack_min_min; /** Lowest value stack pointer ever had. */
  Addr      stack_min;     /** Current stack pointer. */
  Addr      stack_startup; /** Stack pointer after pthread_create() finished.*/
  Addr      stack_max;     /** Top of stack. */
  SizeT     stack_size;    /** Maximum size of stack. */
  /// Indicates whether the Valgrind core knows about this thread.
  Bool      vg_thread_exists;
  /// Indicates whether there is an associated POSIX thread ID.
  Bool      posix_thread_exists;
  /// If true, indicates that there is a corresponding POSIX thread ID and
  /// a corresponding OS thread that is detached.
  Bool      detached_posix_thread;
  /// Wether recording of memory accesses is active.
  Bool      is_recording;
  /// Nesting level of synchronization functions called by the client.
  Int       synchr_nesting;
} ThreadInfo;


// Local variables of drd_thread.c that are declared here such that these
// can be accessed by inline functions.

extern DrdThreadId s_drd_running_tid;
extern ThreadInfo s_threadinfo[DRD_N_THREADS];
extern struct bitmap* s_conflict_set;


// Function declarations.

void thread_trace_context_switches(const Bool t);
void thread_trace_conflict_set(const Bool t);
void thread_set_segment_merging(const Bool m);

DrdThreadId VgThreadIdToDrdThreadId(const ThreadId tid);
DrdThreadId NewVgThreadIdToDrdThreadId(const ThreadId tid);
DrdThreadId PtThreadIdToDrdThreadId(const PThreadId tid);
ThreadId DrdThreadIdToVgThreadId(const DrdThreadId tid);
DrdThreadId thread_pre_create(const DrdThreadId creator,
                              const ThreadId vg_created);
DrdThreadId thread_post_create(const ThreadId vg_created);
void thread_delete(const DrdThreadId tid);
void thread_finished(const DrdThreadId tid);
void thread_pre_cancel(const DrdThreadId tid);
void thread_set_stack_startup(const DrdThreadId tid, const Addr stack_startup);
Addr thread_get_stack_min(const DrdThreadId tid);
Addr thread_get_stack_min_min(const DrdThreadId tid);
Addr thread_get_stack_max(const DrdThreadId tid);
SizeT thread_get_stack_size(const DrdThreadId tid);
void thread_set_pthreadid(const DrdThreadId tid, const PThreadId ptid);
Bool thread_get_joinable(const DrdThreadId tid);
void thread_set_joinable(const DrdThreadId tid, const Bool joinable);
void thread_set_vg_running_tid(const ThreadId vg_tid);
void thread_set_running_tid(const ThreadId vg_tid,
                            const DrdThreadId drd_tid);
int thread_enter_synchr(const DrdThreadId tid);
int thread_leave_synchr(const DrdThreadId tid);
int thread_get_synchr_nesting_count(const DrdThreadId tid);
void thread_new_segment(const DrdThreadId tid);
VectorClock* thread_get_vc(const DrdThreadId tid);
void thread_get_latest_segment(Segment** sg, const DrdThreadId tid);
void thread_combine_vc(const DrdThreadId joiner, const DrdThreadId joinee);
void thread_combine_vc2(const DrdThreadId tid, const VectorClock* const vc);

void thread_stop_using_mem(const Addr a1, const Addr a2);
void thread_start_recording(const DrdThreadId tid);
void thread_stop_recording(const DrdThreadId tid);
void thread_print_all(void);
void thread_report_races(const DrdThreadId tid);
void thread_report_races_segment(const DrdThreadId tid,
                                 const Segment* const p);
void thread_report_all_races(void);
void thread_report_conflicting_segments(const DrdThreadId tid,
                                        const Addr addr,
                                        const SizeT size,
                                        const BmAccessTypeT access_type);
ULong thread_get_context_switch_count(void);
ULong thread_get_report_races_count(void);
ULong thread_get_discard_ordered_segments_count(void);
ULong thread_get_update_conflict_set_count(ULong* dsnsc, ULong* dscvc);
ULong thread_get_conflict_set_bitmap_creation_count(void);
ULong thread_get_conflict_set_bitmap2_creation_count(void);


static __inline__
Bool IsValidDrdThreadId(const DrdThreadId tid)
{
  return (0 <= (int)tid && tid < DRD_N_THREADS && tid != DRD_INVALID_THREADID
          && ! (s_threadinfo[tid].vg_thread_exists == False
                && s_threadinfo[tid].posix_thread_exists == False
                && s_threadinfo[tid].detached_posix_thread == False));
}

static __inline__
DrdThreadId thread_get_running_tid(void)
{
  tl_assert(s_drd_running_tid != DRD_INVALID_THREADID);
  return s_drd_running_tid;
}

static __inline__
struct bitmap* thread_get_conflict_set(void)
{
  return s_conflict_set;
}

static __inline__
Bool running_thread_is_recording(void)
{
#ifdef ENABLE_DRD_CONSISTENCY_CHECKS
  tl_assert(0 <= (int)s_drd_running_tid && s_drd_running_tid < DRD_N_THREADS
            && s_drd_running_tid != DRD_INVALID_THREADID);
#endif
  return (s_threadinfo[s_drd_running_tid].synchr_nesting == 0
          && s_threadinfo[s_drd_running_tid].is_recording);
}

static __inline__
void thread_set_stack_min(const DrdThreadId tid, const Addr stack_min)
{
#ifdef ENABLE_DRD_CONSISTENCY_CHECKS
  tl_assert(0 <= (int)tid
            && tid < DRD_N_THREADS
            && tid != DRD_INVALID_THREADID);
#endif
  s_threadinfo[tid].stack_min = stack_min;
#ifdef ENABLE_DRD_CONSISTENCY_CHECKS
  /* This function can be called after the thread has been created but */
  /* before drd_post_thread_create() has filled in stack_max.          */
  tl_assert(s_threadinfo[tid].stack_min < s_threadinfo[tid].stack_max
            || s_threadinfo[tid].stack_max == 0);
#endif
  if (UNLIKELY(stack_min < s_threadinfo[tid].stack_min_min))
  {
    s_threadinfo[tid].stack_min_min = stack_min;
  }
}

/** Return true if and only if the specified address is on the stack of the
 *  currently scheduled thread.
 */
static __inline__
Bool thread_address_on_stack(const Addr a)
{
  return (s_threadinfo[s_drd_running_tid].stack_min <= a
	  && a < s_threadinfo[s_drd_running_tid].stack_max);
}

/** Return a pointer to the latest segment for the specified thread. */
static __inline__
Segment* thread_get_segment(const DrdThreadId tid)
{
#ifdef ENABLE_DRD_CONSISTENCY_CHECKS
  tl_assert(0 <= (int)tid && tid < DRD_N_THREADS
            && tid != DRD_INVALID_THREADID);
  tl_assert(s_threadinfo[tid].last);
#endif
  return s_threadinfo[tid].last;
}

/** Return a pointer to the latest segment for the running thread. */
static __inline__
Segment* running_thread_get_segment(void)
{
  return thread_get_segment(s_drd_running_tid);
}

#endif // __THREAD_H
