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


#include "drd_clientobj.h"
#include "drd_error.h"
#include "drd_rwlock.h"
#include "pub_tool_vki.h"
#include "pub_tool_errormgr.h"    // VG_(maybe_record_error)()
#include "pub_tool_libcassert.h"  // tl_assert()
#include "pub_tool_libcprint.h"   // VG_(message)()
#include "pub_tool_libcproc.h"    // VG_(read_millisecond_timer)()
#include "pub_tool_machine.h"     // VG_(get_IP)()
#include "pub_tool_mallocfree.h"  // VG_(malloc)(), VG_(free)()
#include "pub_tool_threadstate.h" // VG_(get_running_tid)()


/* Local type definitions. */

struct rwlock_thread_info
{
   UWord    tid;                 // DrdThreadId.
   UInt     reader_nesting_count;
   UInt     writer_nesting_count;
   // Segment of last unlock call by this thread that unlocked a writer lock.
   Segment* latest_wrlocked_segment;
   // Segment of last unlock call by this thread that unlocked a reader lock.
   Segment* latest_rdlocked_segment;
};


/* Local functions. */

static void rwlock_cleanup(struct rwlock_info* p);
static void rwlock_delete_thread(struct rwlock_info* const p,
                                 const DrdThreadId tid);


/* Local variables. */

static Bool DRD_(s_trace_rwlock);
static UInt DRD_(s_exclusive_threshold_ms);
static UInt DRD_(s_shared_threshold_ms);
static ULong DRD_(s_rwlock_segment_creation_count);


/* Function definitions. */

void DRD_(rwlock_set_trace)(const Bool trace_rwlock)
{
   tl_assert(trace_rwlock == False || trace_rwlock == True);
   DRD_(s_trace_rwlock) = trace_rwlock;
}

void DRD_(rwlock_set_exclusive_threshold)(const UInt exclusive_threshold_ms)
{
   DRD_(s_exclusive_threshold_ms) = exclusive_threshold_ms;
}

void DRD_(rwlock_set_shared_threshold)(const UInt shared_threshold_ms)
{
   DRD_(s_shared_threshold_ms) = shared_threshold_ms;
}

static Bool DRD_(rwlock_is_rdlocked)(struct rwlock_info* p)
{
   struct rwlock_thread_info* q;

   VG_(OSetGen_ResetIter)(p->thread_info);
   for ( ; (q = VG_(OSetGen_Next)(p->thread_info)) != 0; )
   {
      return q->reader_nesting_count > 0;
   }
   return False;
}

static Bool DRD_(rwlock_is_wrlocked)(struct rwlock_info* p)
{
   struct rwlock_thread_info* q;

   VG_(OSetGen_ResetIter)(p->thread_info);
   for ( ; (q = VG_(OSetGen_Next)(p->thread_info)) != 0; )
   {
      return q->writer_nesting_count > 0;
   }
   return False;
}

static Bool DRD_(rwlock_is_locked)(struct rwlock_info* p)
{
   return DRD_(rwlock_is_rdlocked)(p) || DRD_(rwlock_is_wrlocked)(p);
}

static Bool DRD_(rwlock_is_rdlocked_by)(struct rwlock_info* p,
                                        const DrdThreadId tid)
{
   const UWord uword_tid = tid;
   struct rwlock_thread_info* q;

   q = VG_(OSetGen_Lookup)(p->thread_info, &uword_tid);
   return q && q->reader_nesting_count > 0;
}

static Bool DRD_(rwlock_is_wrlocked_by)(struct rwlock_info* p,
                                        const DrdThreadId tid)
{
   const UWord uword_tid = tid;
   struct rwlock_thread_info* q;

   q = VG_(OSetGen_Lookup)(p->thread_info, &uword_tid);
   return q && q->writer_nesting_count > 0;
}

static Bool DRD_(rwlock_is_locked_by)(struct rwlock_info* p,
                                      const DrdThreadId tid)
{
   return (DRD_(rwlock_is_rdlocked_by)(p, tid)
           || DRD_(rwlock_is_wrlocked_by)(p, tid));
}

/** Either look up or insert a node corresponding to DRD thread id 'tid'. */
static
struct rwlock_thread_info*
DRD_(lookup_or_insert_node)(OSet* oset, const UWord tid)
{
   struct rwlock_thread_info* q;

   q = VG_(OSetGen_Lookup)(oset, &tid);
   if (q == 0)
   {
      q = VG_(OSetGen_AllocNode)(oset, sizeof(*q));
      q->tid                       = tid;
      q->reader_nesting_count      = 0;
      q->writer_nesting_count      = 0;
      q->latest_wrlocked_segment   = 0;
      q->latest_rdlocked_segment   = 0;
      VG_(OSetGen_Insert)(oset, q);
   }
   tl_assert(q);
   return q;
}

/**
 * Combine the vector clock corresponding to the last unlock operation of
 * reader-writer lock p into the vector clock of thread 'tid'.
 */
static void DRD_(rwlock_combine_other_vc)(struct rwlock_info* const p,
                                          const DrdThreadId tid,
                                          const Bool readers_too)
{
   struct rwlock_thread_info* q;
   VectorClock old_vc;

   DRD_(vc_copy)(&old_vc, &DRD_(g_threadinfo)[tid].last->vc);
   VG_(OSetGen_ResetIter)(p->thread_info);
   for ( ; (q = VG_(OSetGen_Next)(p->thread_info)) != 0; )
   {
      if (q->tid != tid)
      {
         if (q->latest_wrlocked_segment)
         {
            DRD_(vc_combine)(&DRD_(g_threadinfo)[tid].last->vc,
                             &q->latest_wrlocked_segment->vc);
         }
         if (readers_too && q->latest_rdlocked_segment)
         {
            DRD_(vc_combine)(&DRD_(g_threadinfo)[tid].last->vc,
                             &q->latest_rdlocked_segment->vc);
         }
      }
   }
   DRD_(thread_update_conflict_set)(tid, &old_vc);
   DRD_(vc_cleanup)(&old_vc);
}

/**
 * Compare the type of the rwlock specified at initialization time with
 * the type passed as an argument, and complain if these two types do not
 * match.
 */
static Bool drd_rwlock_check_type(struct rwlock_info* const p,
                                  const RwLockT rwlock_type)
{
   tl_assert(p);
   /* The code below has to be updated if additional rwlock types are added. */
   tl_assert(rwlock_type == pthread_rwlock || rwlock_type == user_rwlock);
   tl_assert(p->rwlock_type == pthread_rwlock || p->rwlock_type == user_rwlock);

   if (p->rwlock_type == rwlock_type)
      return True;

   {
      RwlockErrInfo REI = { DRD_(thread_get_running_tid)(), p->a1 };
      VG_(maybe_record_error)
         (VG_(get_running_tid)(),
          RwlockErr,
          VG_(get_IP)(VG_(get_running_tid)()),
          rwlock_type == pthread_rwlock
          ? "Attempt to use a user-defined rwlock as a POSIX rwlock"
          : "Attempt to use a POSIX rwlock as a user-defined rwlock",
          &REI);
   }
   return False;
}

/** Initialize the rwlock_info data structure *p. */
static
void DRD_(rwlock_initialize)(struct rwlock_info* const p, const Addr rwlock,
                             const RwLockT rwlock_type)
{
   tl_assert(rwlock != 0);
   tl_assert(p->a1 == rwlock);
   tl_assert(p->type == ClientRwlock);

   p->cleanup         = (void(*)(DrdClientobj*))rwlock_cleanup;
   p->delete_thread
      = (void(*)(DrdClientobj*, DrdThreadId))rwlock_delete_thread;
   p->rwlock_type     = rwlock_type;
   p->thread_info     = VG_(OSetGen_Create)(
      0, 0, VG_(malloc), "drd.rwlock.ri.1", VG_(free));
   p->acquiry_time_ms = 0;
   p->acquired_at     = 0;
}

/** Deallocate the memory that was allocated by rwlock_initialize(). */
static void rwlock_cleanup(struct rwlock_info* p)
{
   struct rwlock_thread_info* q;

   tl_assert(p);

   if (DRD_(s_trace_rwlock))
      DRD_(trace_msg)("[%d] rwlock_destroy     0x%lx",
                      DRD_(thread_get_running_tid)(), p->a1);

   if (DRD_(rwlock_is_locked)(p))
   {
      RwlockErrInfo REI = { DRD_(thread_get_running_tid)(), p->a1 };
      VG_(maybe_record_error)(VG_(get_running_tid)(),
                              RwlockErr,
                              VG_(get_IP)(VG_(get_running_tid)()),
                              "Destroying locked rwlock",
                              &REI);
   }

   VG_(OSetGen_ResetIter)(p->thread_info);
   for ( ; (q = VG_(OSetGen_Next)(p->thread_info)) != 0; )
   {
      DRD_(sg_put)(q->latest_wrlocked_segment);
      DRD_(sg_put)(q->latest_rdlocked_segment);
   }

   VG_(OSetGen_Destroy)(p->thread_info);
}

static
struct rwlock_info*
DRD_(rwlock_get_or_allocate)(const Addr rwlock, const RwLockT rwlock_type)
{
   struct rwlock_info* p;

   tl_assert(offsetof(DrdClientobj, rwlock) == 0);
   p = &(DRD_(clientobj_get)(rwlock, ClientRwlock)->rwlock);
   if (p)
   {
      drd_rwlock_check_type(p, rwlock_type);
      return p;
   }

   if (DRD_(clientobj_present)(rwlock, rwlock + 1))
   {
      GenericErrInfo GEI = {
	 .tid  = DRD_(thread_get_running_tid)(),
	 .addr = rwlock,
      };
      VG_(maybe_record_error)(VG_(get_running_tid)(),
                              GenericErr,
                              VG_(get_IP)(VG_(get_running_tid)()),
                              "Not a reader-writer lock",
                              &GEI);
      return 0;
   }

   p = &(DRD_(clientobj_add)(rwlock, ClientRwlock)->rwlock);
   DRD_(rwlock_initialize)(p, rwlock, rwlock_type);
   return p;
}

static struct rwlock_info* DRD_(rwlock_get)(const Addr rwlock)
{
   tl_assert(offsetof(DrdClientobj, rwlock) == 0);
   return &(DRD_(clientobj_get)(rwlock, ClientRwlock)->rwlock);
}

/** Called before pthread_rwlock_init(). */
struct rwlock_info* DRD_(rwlock_pre_init)(const Addr rwlock,
                                          const RwLockT rwlock_type)
{
   struct rwlock_info* p;

   if (DRD_(s_trace_rwlock))
      DRD_(trace_msg)("[%d] rwlock_init        0x%lx",
                      DRD_(thread_get_running_tid)(), rwlock);

   p = DRD_(rwlock_get)(rwlock);

   if (p)
	drd_rwlock_check_type(p, rwlock_type);

   if (p)
   {
      const ThreadId vg_tid = VG_(get_running_tid)();
      RwlockErrInfo REI = { DRD_(thread_get_running_tid)(), p->a1 };
      VG_(maybe_record_error)(vg_tid,
                              RwlockErr,
                              VG_(get_IP)(vg_tid),
                              "Reader-writer lock reinitialization",
                              &REI);
      return p;
   }

   p = DRD_(rwlock_get_or_allocate)(rwlock, rwlock_type);

   return p;
}

/** Called after pthread_rwlock_destroy(). */
void DRD_(rwlock_post_destroy)(const Addr rwlock, const RwLockT rwlock_type)
{
   struct rwlock_info* p;

   p = DRD_(rwlock_get)(rwlock);
   if (p == 0)
   {
      GenericErrInfo GEI = {
	 .tid = DRD_(thread_get_running_tid)(),
	 .addr = rwlock,
      };
      VG_(maybe_record_error)(VG_(get_running_tid)(),
                              GenericErr,
                              VG_(get_IP)(VG_(get_running_tid)()),
                              "Not a reader-writer lock",
                              &GEI);
      return;
   }

   drd_rwlock_check_type(p, rwlock_type);

   DRD_(clientobj_remove)(rwlock, ClientRwlock);
}

/**
 * Called before pthread_rwlock_rdlock() is invoked. If a data structure for
 * the client-side object was not yet created, do this now. Also check whether
 * an attempt is made to lock recursively a synchronization object that must
 * not be locked recursively.
 */
void DRD_(rwlock_pre_rdlock)(const Addr rwlock, const RwLockT rwlock_type)
{
   struct rwlock_info* p;

   if (DRD_(s_trace_rwlock))
      DRD_(trace_msg)("[%d] pre_rwlock_rdlock  0x%lx",
                      DRD_(thread_get_running_tid)(), rwlock);

   p = DRD_(rwlock_get_or_allocate)(rwlock, rwlock_type);
   tl_assert(p);

   if (DRD_(rwlock_is_wrlocked_by)(p, DRD_(thread_get_running_tid)())) {
      RwlockErrInfo REI = { DRD_(thread_get_running_tid)(), p->a1 };
      VG_(maybe_record_error)(VG_(get_running_tid)(),
                              RwlockErr,
                              VG_(get_IP)(VG_(get_running_tid)()),
                              "Already locked for writing by calling thread",
                              &REI);
   }
}

/**
 * Update rwlock_info state when locking the pthread_rwlock_t mutex.
 * Note: this function must be called after pthread_rwlock_rdlock() has been
 * called, or a race condition is triggered !
 */
void DRD_(rwlock_post_rdlock)(const Addr rwlock, const RwLockT rwlock_type,
                              const Bool took_lock)
{
   const DrdThreadId drd_tid = DRD_(thread_get_running_tid)();
   struct rwlock_info* p;
   struct rwlock_thread_info* q;

   if (DRD_(s_trace_rwlock))
      DRD_(trace_msg)("[%d] post_rwlock_rdlock 0x%lx", drd_tid, rwlock);

   p = DRD_(rwlock_get)(rwlock);

   if (! p || ! took_lock)
      return;

   tl_assert(! DRD_(rwlock_is_wrlocked)(p));

   q = DRD_(lookup_or_insert_node)(p->thread_info, drd_tid);
   if (++q->reader_nesting_count == 1)
   {
      DRD_(thread_new_segment)(drd_tid);
      DRD_(s_rwlock_segment_creation_count)++;
      DRD_(rwlock_combine_other_vc)(p, drd_tid, False);

      p->acquiry_time_ms = VG_(read_millisecond_timer)();
      p->acquired_at     = VG_(record_ExeContext)(VG_(get_running_tid)(), 0);
   }
}

/**
 * Called before pthread_rwlock_wrlock() is invoked. If a data structure for
 * the client-side object was not yet created, do this now. Also check whether
 * an attempt is made to lock recursively a synchronization object that must
 * not be locked recursively.
 */
void DRD_(rwlock_pre_wrlock)(const Addr rwlock, const RwLockT rwlock_type)
{
   struct rwlock_info* p;

   p = DRD_(rwlock_get)(rwlock);

   if (DRD_(s_trace_rwlock))
      DRD_(trace_msg)("[%d] pre_rwlock_wrlock  0x%lx",
                      DRD_(thread_get_running_tid)(), rwlock);

   if (p == 0)
      p = DRD_(rwlock_get_or_allocate)(rwlock, rwlock_type);

   tl_assert(p);

   if (DRD_(rwlock_is_wrlocked_by)(p, DRD_(thread_get_running_tid)()))
   {
      RwlockErrInfo REI = { DRD_(thread_get_running_tid)(), p->a1 };
      VG_(maybe_record_error)(VG_(get_running_tid)(),
                              RwlockErr,
                              VG_(get_IP)(VG_(get_running_tid)()),
                              "Recursive writer locking not allowed",
                              &REI);
   }
}

/**
 * Update rwlock_info state when locking the pthread_rwlock_t rwlock.
 * Note: this function must be called after pthread_rwlock_wrlock() has
 * finished, or a race condition is triggered !
 */
void DRD_(rwlock_post_wrlock)(const Addr rwlock, const RwLockT rwlock_type,
                              const Bool took_lock)
{
   const DrdThreadId drd_tid = DRD_(thread_get_running_tid)();
   struct rwlock_info* p;
   struct rwlock_thread_info* q;

   p = DRD_(rwlock_get)(rwlock);

   if (DRD_(s_trace_rwlock))
      DRD_(trace_msg)("[%d] post_rwlock_wrlock 0x%lx", drd_tid, rwlock);

   if (! p || ! took_lock)
      return;

   q = DRD_(lookup_or_insert_node)(p->thread_info,
                                   DRD_(thread_get_running_tid)());
   tl_assert(q->writer_nesting_count == 0);
   q->writer_nesting_count++;
   tl_assert(q->writer_nesting_count == 1);
   DRD_(thread_new_segment)(drd_tid);
   DRD_(s_rwlock_segment_creation_count)++;
   DRD_(rwlock_combine_other_vc)(p, drd_tid, True);
   p->acquiry_time_ms = VG_(read_millisecond_timer)();
   p->acquired_at     = VG_(record_ExeContext)(VG_(get_running_tid)(), 0);
}

/**
 * Update rwlock_info state when unlocking the pthread_rwlock_t rwlock.
 *
 * @param rwlock Pointer to pthread_rwlock_t data structure in the client space.
 *
 * @return New value of the rwlock recursion count.
 *
 * @note This function must be called before pthread_rwlock_unlock() is called,
 *   or a race condition is triggered !
 */
void DRD_(rwlock_pre_unlock)(const Addr rwlock, const RwLockT rwlock_type)
{
   const DrdThreadId drd_tid = DRD_(thread_get_running_tid)();
   const ThreadId vg_tid = VG_(get_running_tid)();
   struct rwlock_info* p;
   struct rwlock_thread_info* q;

   if (DRD_(s_trace_rwlock))
      DRD_(trace_msg)("[%d] rwlock_unlock      0x%lx", drd_tid, rwlock);

   p = DRD_(rwlock_get)(rwlock);
   if (p == 0)
   {
      GenericErrInfo GEI = {
	 .tid = DRD_(thread_get_running_tid)(),
	 .addr = rwlock,
      };
      VG_(maybe_record_error)(VG_(get_running_tid)(),
                              GenericErr,
                              VG_(get_IP)(VG_(get_running_tid)()),
                              "Not a reader-writer lock",
                              &GEI);
      return;
   }

   drd_rwlock_check_type(p, rwlock_type);

   if (! DRD_(rwlock_is_locked_by)(p, drd_tid))
   {
      RwlockErrInfo REI = { DRD_(thread_get_running_tid)(), p->a1 };
      VG_(maybe_record_error)(vg_tid,
                              RwlockErr,
                              VG_(get_IP)(vg_tid),
                              "Reader-writer lock not locked by calling thread",
                              &REI);
      return;
   }
   q = DRD_(lookup_or_insert_node)(p->thread_info, drd_tid);
   tl_assert(q);
   if (q->reader_nesting_count > 0)
   {
      q->reader_nesting_count--;
      if (q->reader_nesting_count == 0 && DRD_(s_shared_threshold_ms) > 0)
      {
         Long held = VG_(read_millisecond_timer)() - p->acquiry_time_ms;
         if (held > DRD_(s_shared_threshold_ms))
         {
            HoldtimeErrInfo HEI
               = { DRD_(thread_get_running_tid)(),
                   rwlock, p->acquired_at, held, DRD_(s_shared_threshold_ms) };
            VG_(maybe_record_error)(vg_tid,
                                    HoldtimeErr,
                                    VG_(get_IP)(vg_tid),
                                    "rwlock",
                                    &HEI);
         }
      }
      if (q->reader_nesting_count == 0 && q->writer_nesting_count == 0)
      {
         /*
          * This pthread_rwlock_unlock() call really unlocks the rwlock. Save
          * the current vector clock of the thread such that it is available
          * when this rwlock is locked again.
          */
         DRD_(thread_get_latest_segment)(&q->latest_rdlocked_segment, drd_tid);
         DRD_(thread_new_segment)(drd_tid);
         DRD_(s_rwlock_segment_creation_count)++;
      }
   }
   else if (q->writer_nesting_count > 0)
   {
      q->writer_nesting_count--;
      if (q->writer_nesting_count == 0 && DRD_(s_exclusive_threshold_ms) > 0)
      {
         Long held = VG_(read_millisecond_timer)() - p->acquiry_time_ms;
         if (held > DRD_(s_exclusive_threshold_ms))
         {
            HoldtimeErrInfo HEI
               = { DRD_(thread_get_running_tid)(),
                   rwlock, p->acquired_at, held,
                   DRD_(s_exclusive_threshold_ms) };
            VG_(maybe_record_error)(vg_tid,
                                    HoldtimeErr,
                                    VG_(get_IP)(vg_tid),
                                    "rwlock",
                                    &HEI);
         }
      }
      if (q->reader_nesting_count == 0 && q->writer_nesting_count == 0)
      {
         /*
          * This pthread_rwlock_unlock() call really unlocks the rwlock. Save
          * the current vector clock of the thread such that it is available
          * when this rwlock is locked again.
          */
         DRD_(thread_get_latest_segment)(&q->latest_wrlocked_segment, drd_tid);
         DRD_(thread_new_segment)(drd_tid);
         DRD_(s_rwlock_segment_creation_count)++;
      }
   }
   else
   {
      tl_assert(False);
   }
}

/** Called when thread tid stops to exist. */
static void rwlock_delete_thread(struct rwlock_info* const p,
                                 const DrdThreadId tid)
{
   struct rwlock_thread_info* q;

   if (DRD_(rwlock_is_locked_by)(p, tid))
   {
      RwlockErrInfo REI = { DRD_(thread_get_running_tid)(), p->a1 };
      VG_(maybe_record_error)(VG_(get_running_tid)(),
                              RwlockErr,
                              VG_(get_IP)(VG_(get_running_tid)()),
                              "Reader-writer lock still locked at thread exit",
                              &REI);
      q = DRD_(lookup_or_insert_node)(p->thread_info, tid);
      q->reader_nesting_count = 0;
      q->writer_nesting_count = 0;
   }
}

ULong DRD_(get_rwlock_segment_creation_count)(void)
{
   return DRD_(s_rwlock_segment_creation_count);
}
