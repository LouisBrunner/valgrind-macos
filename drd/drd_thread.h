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


#ifndef __THREAD_H
#define __THREAD_H


/* Include directives. */

#include "drd_basics.h"
#include "drd_segment.h"
#include "pub_drd_bitmap.h"
#include "pub_tool_libcassert.h"  /* tl_assert()        */
#include "pub_tool_stacktrace.h"  /* typedef StackTrace */
#include "pub_tool_threadstate.h" /* VG_N_THREADS       */


/* Defines. */

/** Maximum number of threads DRD keeps information about. */
#define DRD_N_THREADS VG_N_THREADS

/** A number different from any valid DRD thread ID. */
#define DRD_INVALID_THREADID 0

/**
 * A number different from any valid POSIX thread ID.
 *
 * @note The PThreadId typedef and the INVALID_POSIX_THREADID depend on the
 * operating system and threading library in use. PThreadId must contain at
 * least as many bits as pthread_t, and INVALID_POSIX_THREADID
 * must be a value that will never be returned by pthread_self().
 */
#define INVALID_POSIX_THREADID ((PThreadId)0)


/* Type definitions. */

/**
 * POSIX thread ID. The type PThreadId must be at least as wide as
 * pthread_t.
 */
typedef UWord PThreadId;

/** Per-thread information managed by DRD. */
typedef struct
{
   Segment*  first;         /**< Pointer to first segment. */
   Segment*  last;          /**< Pointer to last segment. */
   ThreadId  vg_threadid;   /**< Valgrind thread ID. */
   PThreadId pt_threadid;   /**< POSIX thread ID. */
   Addr      stack_min_min; /**< Lowest value stack pointer ever had. */
   Addr      stack_min;     /**< Current stack pointer. */
   Addr      stack_startup; /**<Stack pointer after pthread_create() finished.*/
   Addr      stack_max;     /**< Top of stack. */
   SizeT     stack_size;    /**< Maximum size of stack. */
   char      name[64];      /**< User-assigned thread name. */
   Bool      on_alt_stack;
   /** Indicates whether the Valgrind core knows about this thread. */
   Bool      vg_thread_exists;
   /** Indicates whether there is an associated POSIX thread ID. */
   Bool      posix_thread_exists;
   /**
    * If true, indicates that there is a corresponding POSIX thread ID and
    * a corresponding OS thread that is detached.
    */
   Bool      detached_posix_thread;
   /** Wether recording of memory load accesses is currently enabled. */
   Bool      is_recording_loads;
   /** Wether recording of memory load accesses is currently enabled. */
   Bool      is_recording_stores;
   /** pthread_create() nesting level. */
   Int       pthread_create_nesting_level;
   /** Nesting level of synchronization functions called by the client. */
   Int       synchr_nesting;
} ThreadInfo;


/*
 * Local variables of drd_thread.c that are declared here such that these
 * can be accessed by inline functions.
 */

/**
 * DRD thread ID of the currently running thread. It is crucial for correct
 * operation of DRD that this number is always in sync with
 * VG_(get_running_tid)().
 */
extern DrdThreadId    DRD_(g_drd_running_tid);
/** Per-thread information managed by DRD. */
extern ThreadInfo     DRD_(g_threadinfo)[DRD_N_THREADS];
/** Conflict set for the currently running thread. */
extern struct bitmap* DRD_(g_conflict_set);


/* Function declarations. */

void DRD_(thread_trace_context_switches)(const Bool t);
void DRD_(thread_trace_conflict_set)(const Bool t);
void DRD_(thread_trace_conflict_set_bm)(const Bool t);
Bool DRD_(thread_get_trace_fork_join)(void);
void DRD_(thread_set_trace_fork_join)(const Bool t);
void DRD_(thread_set_segment_merging)(const Bool m);
int DRD_(thread_get_segment_merge_interval)(void);
void DRD_(thread_set_segment_merge_interval)(const int i);

DrdThreadId DRD_(VgThreadIdToDrdThreadId)(const ThreadId tid);
DrdThreadId DRD_(NewVgThreadIdToDrdThreadId)(const ThreadId tid);
DrdThreadId DRD_(PtThreadIdToDrdThreadId)(const PThreadId tid);
ThreadId DRD_(DrdThreadIdToVgThreadId)(const DrdThreadId tid);
DrdThreadId DRD_(thread_pre_create)(const DrdThreadId creator,
                                    const ThreadId vg_created);
DrdThreadId DRD_(thread_post_create)(const ThreadId vg_created);
void DRD_(thread_post_join)(DrdThreadId drd_joiner, DrdThreadId drd_joinee);
void DRD_(thread_delete)(const DrdThreadId tid, Bool detached);
void DRD_(thread_finished)(const DrdThreadId tid);
void DRD_(drd_thread_atfork_child)(const DrdThreadId tid);
void DRD_(thread_pre_cancel)(const DrdThreadId tid);
void DRD_(thread_set_stack_startup)(const DrdThreadId tid,
                                    const Addr stack_startup);
Addr DRD_(thread_get_stack_min)(const DrdThreadId tid);
Addr DRD_(thread_get_stack_min_min)(const DrdThreadId tid);
Addr DRD_(thread_get_stack_max)(const DrdThreadId tid);
SizeT DRD_(thread_get_stack_size)(const DrdThreadId tid);
Bool DRD_(thread_get_on_alt_stack)(const DrdThreadId tid);
void DRD_(thread_set_on_alt_stack)(const DrdThreadId tid,
                                   const Bool on_alt_stack);
Int DRD_(thread_get_threads_on_alt_stack)(void);
void DRD_(thread_set_pthreadid)(const DrdThreadId tid, const PThreadId ptid);
Bool DRD_(thread_get_joinable)(const DrdThreadId tid);
void DRD_(thread_set_joinable)(const DrdThreadId tid, const Bool joinable);
void DRD_(thread_entering_pthread_create)(const DrdThreadId tid);
void DRD_(thread_left_pthread_create)(const DrdThreadId tid);
const char* DRD_(thread_get_name)(const DrdThreadId tid);
void DRD_(thread_set_name)(const DrdThreadId tid, const char* const name);
void DRD_(thread_set_vg_running_tid)(const ThreadId vg_tid);
void DRD_(thread_set_running_tid)(const ThreadId vg_tid,
                                  const DrdThreadId drd_tid);
int DRD_(thread_enter_synchr)(const DrdThreadId tid);
int DRD_(thread_leave_synchr)(const DrdThreadId tid);
int DRD_(thread_get_synchr_nesting_count)(const DrdThreadId tid);
void DRD_(thread_new_segment)(const DrdThreadId tid);
VectorClock* DRD_(thread_get_vc)(const DrdThreadId tid);
void DRD_(thread_get_latest_segment)(Segment** sg, const DrdThreadId tid);
void DRD_(thread_combine_vc_join)(const DrdThreadId joiner,
                                  const DrdThreadId joinee);
void DRD_(thread_new_segment_and_combine_vc)(DrdThreadId tid,
                                             const Segment* sg);
void DRD_(thread_update_conflict_set)(const DrdThreadId tid,
                                      const VectorClock* const old_vc);

void DRD_(thread_stop_using_mem)(const Addr a1, const Addr a2);
void DRD_(thread_set_record_loads)(const DrdThreadId tid, const Bool enabled);
void DRD_(thread_set_record_stores)(const DrdThreadId tid, const Bool enabled);
void DRD_(thread_print_all)(void);
void DRD_(thread_report_races)(const DrdThreadId tid);
void DRD_(thread_report_races_segment)(const DrdThreadId tid,
                                       const Segment* const p);
void DRD_(thread_report_all_races)(void);
void DRD_(thread_report_conflicting_segments)(const DrdThreadId tid,
                                              const Addr addr,
                                              const SizeT size,
                                              const BmAccessTypeT access_type);
ULong DRD_(thread_get_context_switch_count)(void);
ULong DRD_(thread_get_report_races_count)(void);
ULong DRD_(thread_get_discard_ordered_segments_count)(void);
ULong DRD_(thread_get_compute_conflict_set_count)(void);
ULong DRD_(thread_get_update_conflict_set_count)(void);
ULong DRD_(thread_get_update_conflict_set_new_sg_count)(void);
ULong DRD_(thread_get_update_conflict_set_sync_count)(void);
ULong DRD_(thread_get_update_conflict_set_join_count)(void);
ULong DRD_(thread_get_conflict_set_bitmap_creation_count)(void);
ULong DRD_(thread_get_conflict_set_bitmap2_creation_count)(void);


/* Inline function definitions. */

/**
 * Whether or not the specified DRD thread ID is valid.
 *
 * A DRD thread ID is valid if and only if the following conditions are met:
 * - The ID is a valid index of the DRD_(g_threadinfo)[] array.
 * - The ID is not equal to DRD_INVALID_THREADID.
 * - The ID refers either to a thread known by the Valgrind core, a joinable
 *   thread that has not yet been joined or a detached thread.
 */
static __inline__
Bool DRD_(IsValidDrdThreadId)(const DrdThreadId tid)
{
   return (0 <= (int)tid && tid < DRD_N_THREADS && tid != DRD_INVALID_THREADID
           && ! (DRD_(g_threadinfo)[tid].vg_thread_exists == False
                 && DRD_(g_threadinfo)[tid].posix_thread_exists == False
                 && DRD_(g_threadinfo)[tid].detached_posix_thread == False));
}

/** Returns the DRD thread ID of the currently running thread. */
static __inline__
DrdThreadId DRD_(thread_get_running_tid)(void)
{
#ifdef ENABLE_DRD_CONSISTENCY_CHECKS
   tl_assert(DRD_(g_drd_running_tid) != DRD_INVALID_THREADID);
#endif
   return DRD_(g_drd_running_tid);
}

/** Returns a pointer to the conflict set for the currently running thread. */
static __inline__
struct bitmap* DRD_(thread_get_conflict_set)(void)
{
   return DRD_(g_conflict_set);
}

/**
 * Reports whether or not the currently running client thread is executing code
 * inside the pthread_create() function.
 */
static __inline__
Bool DRD_(running_thread_inside_pthread_create)(void)
{
   return (DRD_(g_threadinfo)[DRD_(g_drd_running_tid)]
           .pthread_create_nesting_level > 0);
}

/**
 * Reports whether or not recording of memory loads is enabled for the
 * currently running client thread.
 */
static __inline__
Bool DRD_(running_thread_is_recording_loads)(void)
{
#ifdef ENABLE_DRD_CONSISTENCY_CHECKS
   tl_assert(0 <= (int)DRD_(g_drd_running_tid)
             && DRD_(g_drd_running_tid) < DRD_N_THREADS
             && DRD_(g_drd_running_tid) != DRD_INVALID_THREADID);
#endif
   return (DRD_(g_threadinfo)[DRD_(g_drd_running_tid)].synchr_nesting == 0
           && DRD_(g_threadinfo)[DRD_(g_drd_running_tid)].is_recording_loads);
}

/**
 * Reports whether or not recording memory stores is enabled for the
 * currently running client thread.
 */
static __inline__
Bool DRD_(running_thread_is_recording_stores)(void)
{
#ifdef ENABLE_DRD_CONSISTENCY_CHECKS
   tl_assert(0 <= (int)DRD_(g_drd_running_tid)
             && DRD_(g_drd_running_tid) < DRD_N_THREADS
             && DRD_(g_drd_running_tid) != DRD_INVALID_THREADID);
#endif
   return (DRD_(g_threadinfo)[DRD_(g_drd_running_tid)].synchr_nesting == 0
           && DRD_(g_threadinfo)[DRD_(g_drd_running_tid)].is_recording_stores);
}

/**
 * Update the information about the lowest stack address that has ever been
 * accessed by a thread.
 */
static __inline__
void DRD_(thread_set_stack_min)(const DrdThreadId tid, const Addr stack_min)
{
#ifdef ENABLE_DRD_CONSISTENCY_CHECKS
   tl_assert(0 <= (int)tid
             && tid < DRD_N_THREADS
             && tid != DRD_INVALID_THREADID);
#endif
   DRD_(g_threadinfo)[tid].stack_min = stack_min;
#ifdef ENABLE_DRD_CONSISTENCY_CHECKS
   /* This function can be called after the thread has been created but */
   /* before drd_post_thread_create() has filled in stack_max.          */
   tl_assert(DRD_(g_threadinfo)[tid].stack_min
             < DRD_(g_threadinfo)[tid].stack_max
             || DRD_(g_threadinfo)[tid].stack_max == 0);
#endif
   if (UNLIKELY(stack_min < DRD_(g_threadinfo)[tid].stack_min_min))
   {
      DRD_(g_threadinfo)[tid].stack_min_min = stack_min;
   }
}

/**
 * Return true if and only if the specified address is on the stack of the
 * currently scheduled thread.
 */
static __inline__
Bool DRD_(thread_address_on_stack)(const Addr a)
{
   return (DRD_(g_threadinfo)[DRD_(g_drd_running_tid)].stack_min <= a
           && a < DRD_(g_threadinfo)[DRD_(g_drd_running_tid)].stack_max);
}

/**
 * Return true if and only if the specified address is on the stack of any
 * thread.
 */
static __inline__
Bool DRD_(thread_address_on_any_stack)(const Addr a)
{
   int i;

   for (i = 1; i < DRD_N_THREADS; i++)
   {
      if (DRD_(g_threadinfo)[i].vg_thread_exists
          && DRD_(g_threadinfo)[i].stack_min <= a
          && a < DRD_(g_threadinfo)[i].stack_max)
      {
         return True;
      }
   }
   return False;
}

/** Return a pointer to the latest segment for the specified thread. */
static __inline__
Segment* DRD_(thread_get_segment)(const DrdThreadId tid)
{
#ifdef ENABLE_DRD_CONSISTENCY_CHECKS
   tl_assert(0 <= (int)tid && tid < DRD_N_THREADS
             && tid != DRD_INVALID_THREADID);
   tl_assert(DRD_(g_threadinfo)[tid].last);
#endif
   return DRD_(g_threadinfo)[tid].last;
}

/** Return a pointer to the latest segment for the running thread. */
static __inline__
Segment* DRD_(running_thread_get_segment)(void)
{
   return DRD_(thread_get_segment)(DRD_(g_drd_running_tid));
}

#endif /* __THREAD_H */
