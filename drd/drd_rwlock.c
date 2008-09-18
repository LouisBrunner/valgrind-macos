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


// Type definitions.

struct rwlock_thread_info
{
  UWord    tid;                 // DrdThreadId.
  UInt     reader_nesting_count;
  UInt     writer_nesting_count;
  Segment* last_unlock_segment; // Segment of last unlock call by this thread.
  Bool     last_lock_was_writer_lock;
};


// Local functions.

static void rwlock_cleanup(struct rwlock_info* p);
static ULong s_rwlock_segment_creation_count;


// Local variables.

static Bool s_trace_rwlock;
static UInt s_exclusive_threshold_ms;
static UInt s_shared_threshold_ms;


// Function definitions.

void rwlock_set_trace(const Bool trace_rwlock)
{
  tl_assert(!! trace_rwlock == trace_rwlock);
  s_trace_rwlock = trace_rwlock;
}

void rwlock_set_exclusive_threshold(const UInt exclusive_threshold_ms)
{
  s_exclusive_threshold_ms = exclusive_threshold_ms;
}

void rwlock_set_shared_threshold(const UInt shared_threshold_ms)
{
  s_shared_threshold_ms = shared_threshold_ms;
}

static Bool rwlock_is_rdlocked(struct rwlock_info* p)
{
  struct rwlock_thread_info* q;

  VG_(OSetGen_ResetIter)(p->thread_info);
  for ( ; (q = VG_(OSetGen_Next)(p->thread_info)) != 0; )
  {
    return q->reader_nesting_count > 0;
  }
  return False;
}

static Bool rwlock_is_wrlocked(struct rwlock_info* p)
{
  struct rwlock_thread_info* q;

  VG_(OSetGen_ResetIter)(p->thread_info);
  for ( ; (q = VG_(OSetGen_Next)(p->thread_info)) != 0; )
  {
    return q->writer_nesting_count > 0;
  }
  return False;
}

static Bool rwlock_is_locked(struct rwlock_info* p)
{
  return rwlock_is_rdlocked(p) || rwlock_is_wrlocked(p);
}

static Bool rwlock_is_rdlocked_by(struct rwlock_info* p, const DrdThreadId tid)
{
  const UWord uword_tid = tid;
  struct rwlock_thread_info* q;

  q = VG_(OSetGen_Lookup)(p->thread_info, &uword_tid);
  return q && q->reader_nesting_count > 0;
}

static Bool rwlock_is_wrlocked_by(struct rwlock_info* p, const DrdThreadId tid)
{
  const UWord uword_tid = tid;
  struct rwlock_thread_info* q;

  q = VG_(OSetGen_Lookup)(p->thread_info, &uword_tid);
  return q && q->writer_nesting_count > 0;
}

static Bool rwlock_is_locked_by(struct rwlock_info* p, const DrdThreadId tid)
{
  return rwlock_is_rdlocked_by(p, tid) || rwlock_is_wrlocked_by(p, tid);
}

/** Either look up or insert a node corresponding to DRD thread id 'tid'. */
static
struct rwlock_thread_info* lookup_or_insert_node(OSet* oset, const UWord tid)
{
  struct rwlock_thread_info* q;

  q = VG_(OSetGen_Lookup)(oset, &tid);
  if (q == 0)
  {
    q = VG_(OSetGen_AllocNode)(oset, sizeof(*q));
    q->tid                       = tid;
    q->reader_nesting_count      = 0;
    q->writer_nesting_count      = 0;
    q->last_unlock_segment       = 0;
    q->last_lock_was_writer_lock = False;
    VG_(OSetGen_Insert)(oset, q);
  }
  tl_assert(q);
  return q;
}

/** Combine the vector clock corresponding to the last unlock operation of
 *  reader-writer lock p into the vector clock of thread 'tid'.
 */
static void rwlock_combine_other_vc(struct rwlock_info* const p,
                                    const DrdThreadId tid,
                                    const Bool readers_too)
{
  struct rwlock_thread_info* q;

  VG_(OSetGen_ResetIter)(p->thread_info);
  for ( ; (q = VG_(OSetGen_Next)(p->thread_info)) != 0; )
  {
    if (q->tid != tid && (readers_too || q->last_lock_was_writer_lock))
    {
      thread_combine_vc2(tid, &q->last_unlock_segment->vc);
    }
  }
}

/** Initialize the rwlock_info data structure *p. */
static
void rwlock_initialize(struct rwlock_info* const p, const Addr rwlock)
{
  tl_assert(rwlock != 0);
  tl_assert(p->a1 == rwlock);
  tl_assert(p->type == ClientRwlock);

  p->cleanup         = (void(*)(DrdClientobj*))&rwlock_cleanup;
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

  if (s_trace_rwlock)
  {
    VG_(message)(Vg_UserMsg,
                 "[%d/%d] rwlock_destroy     0x%lx",
                 VG_(get_running_tid)(),
                 thread_get_running_tid(),
                 p->a1);
  }

  if (rwlock_is_locked(p))
  {
    RwlockErrInfo REI = { p->a1 };
    VG_(maybe_record_error)(VG_(get_running_tid)(),
                            RwlockErr,
                            VG_(get_IP)(VG_(get_running_tid)()),
                            "Destroying locked rwlock",
                            &REI);
  }

  VG_(OSetGen_ResetIter)(p->thread_info);
  for ( ; (q = VG_(OSetGen_Next)(p->thread_info)) != 0; )
  {
    sg_put(q->last_unlock_segment);
  }
  VG_(OSetGen_Destroy)(p->thread_info);
}

static
struct rwlock_info*
rwlock_get_or_allocate(const Addr rwlock)
{
  struct rwlock_info* p;

  tl_assert(offsetof(DrdClientobj, rwlock) == 0);
  p = &clientobj_get(rwlock, ClientRwlock)->rwlock;
  if (p)
  {
    return p;
  }

  if (clientobj_present(rwlock, rwlock + 1))
  {
    GenericErrInfo GEI;
    VG_(maybe_record_error)(VG_(get_running_tid)(),
                            GenericErr,
                            VG_(get_IP)(VG_(get_running_tid)()),
                            "Not a reader-writer lock",
                            &GEI);
    return 0;
  }

  p = &clientobj_add(rwlock, ClientRwlock)->rwlock;
  rwlock_initialize(p, rwlock);
  return p;
}

static struct rwlock_info* rwlock_get(const Addr rwlock)
{
  tl_assert(offsetof(DrdClientobj, rwlock) == 0);
  return &clientobj_get(rwlock, ClientRwlock)->rwlock;
}

/** Called before pthread_rwlock_init(). */
struct rwlock_info* rwlock_pre_init(const Addr rwlock)
{
  struct rwlock_info* p;

  if (s_trace_rwlock)
  {
    VG_(message)(Vg_UserMsg,
                 "[%d/%d] rwlock_init        0x%lx",
                 VG_(get_running_tid)(),
                 thread_get_running_tid(),
                 rwlock);
  }

  p = rwlock_get(rwlock);

  if (p)
  {
    const ThreadId vg_tid = VG_(get_running_tid)();
    RwlockErrInfo REI
      = { p->a1 };
    VG_(maybe_record_error)(vg_tid,
                            RwlockErr,
                            VG_(get_IP)(vg_tid),
                            "Reader-writer lock reinitialization",
                            &REI);
    return p;
  }

  p = rwlock_get_or_allocate(rwlock);

  return p;
}

/** Called after pthread_rwlock_destroy(). */
void rwlock_post_destroy(const Addr rwlock)
{
  struct rwlock_info* p;

  p = rwlock_get(rwlock);
  if (p == 0)
  {
    GenericErrInfo GEI;
    VG_(maybe_record_error)(VG_(get_running_tid)(),
                            GenericErr,
                            VG_(get_IP)(VG_(get_running_tid)()),
                            "Not a reader-writer lock",
                            &GEI);
    return;
  }

  clientobj_remove(rwlock, ClientRwlock);
}

/** Called before pthread_rwlock_rdlock() is invoked. If a data structure for
 *  the client-side object was not yet created, do this now. Also check whether
 *  an attempt is made to lock recursively a synchronization object that must
 *  not be locked recursively.
 */
void rwlock_pre_rdlock(const Addr rwlock)
{
  struct rwlock_info* p;

  if (s_trace_rwlock)
  {
    VG_(message)(Vg_UserMsg,
                 "[%d/%d] pre_rwlock_rdlock  0x%lx",
                 VG_(get_running_tid)(),
                 thread_get_running_tid(),
                 rwlock);
  }

  p = rwlock_get_or_allocate(rwlock);
  tl_assert(p);

  if (rwlock_is_wrlocked_by(p, thread_get_running_tid()))
  {
    VG_(message)(Vg_UserMsg,
                 "reader-writer lock 0x%lx is already locked for"
                 " writing by calling thread",
                 p->a1);
  }
}

/** Update rwlock_info state when locking the pthread_rwlock_t mutex.
 *  Note: this function must be called after pthread_rwlock_rdlock() has been
 *  called, or a race condition is triggered !
 */
void rwlock_post_rdlock(const Addr rwlock, const Bool took_lock)
{
  const DrdThreadId drd_tid = thread_get_running_tid();
  struct rwlock_info* p;
  struct rwlock_thread_info* q;

  if (s_trace_rwlock)
  {
    VG_(message)(Vg_UserMsg,
                 "[%d/%d] post_rwlock_rdlock 0x%lx",
                 VG_(get_running_tid)(),
                 drd_tid,
                 rwlock);
  }

  p = rwlock_get(rwlock);

  if (! p || ! took_lock)
    return;

  tl_assert(! rwlock_is_wrlocked(p));

  q = lookup_or_insert_node(p->thread_info, drd_tid);
  if (++q->reader_nesting_count == 1)
  {
    rwlock_combine_other_vc(p, drd_tid, False);
    q->last_lock_was_writer_lock = False;
    thread_new_segment(drd_tid);
    s_rwlock_segment_creation_count++;

    p->acquiry_time_ms = VG_(read_millisecond_timer)();
    p->acquired_at     = VG_(record_ExeContext)(VG_(get_running_tid)(), 0);
  }
}

/** Called before pthread_rwlock_wrlock() is invoked. If a data structure for
 *  the client-side object was not yet created, do this now. Also check whether
 *  an attempt is made to lock recursively a synchronization object that must
 *  not be locked recursively.
 */
void rwlock_pre_wrlock(const Addr rwlock)
{
  struct rwlock_info* p;

  p = rwlock_get(rwlock);

  if (s_trace_rwlock)
  {
    VG_(message)(Vg_UserMsg,
                 "[%d/%d] pre_rwlock_wrlock  0x%lx",
                 VG_(get_running_tid)(),
                 thread_get_running_tid(),
                 rwlock);
  }

  if (p == 0)
  {
    p = rwlock_get_or_allocate(rwlock);
  }

  tl_assert(p);

  if (rwlock_is_wrlocked_by(p, thread_get_running_tid()))
  {
    RwlockErrInfo REI = { p->a1 };
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
void rwlock_post_wrlock(const Addr rwlock, const Bool took_lock)
{
  const DrdThreadId drd_tid = thread_get_running_tid();
  struct rwlock_info* p;
  struct rwlock_thread_info* q;

  p = rwlock_get(rwlock);

  if (s_trace_rwlock)
  {
    VG_(message)(Vg_UserMsg,
                 "[%d/%d] post_rwlock_wrlock 0x%lx",
                 VG_(get_running_tid)(),
                 drd_tid,
                 rwlock);
  }

  if (! p || ! took_lock)
    return;

  q = lookup_or_insert_node(p->thread_info, thread_get_running_tid());
  tl_assert(q->writer_nesting_count == 0);
  q->writer_nesting_count++;
  q->last_lock_was_writer_lock = True;
  tl_assert(q->writer_nesting_count == 1);
  rwlock_combine_other_vc(p, drd_tid, True);
  thread_new_segment(drd_tid);
  s_rwlock_segment_creation_count++;
  p->acquiry_time_ms = VG_(read_millisecond_timer)();
  p->acquired_at     = VG_(record_ExeContext)(VG_(get_running_tid)(), 0);
}

/**
 * Update rwlock_info state when unlocking the pthread_rwlock_t rwlock.
 * Note: this function must be called before pthread_rwlock_unlock() is called,
 * or a race condition is triggered !
 * @return New value of the rwlock recursion count.
 * @param rwlock Pointer to pthread_rwlock_t data structure in the client space.
 * @param tid ThreadId of the thread calling pthread_rwlock_unlock().
 * @param vc Pointer to the current vector clock of thread tid.
 */
void rwlock_pre_unlock(const Addr rwlock)
{
  const DrdThreadId drd_tid = thread_get_running_tid();
  const ThreadId vg_tid = VG_(get_running_tid)();
  struct rwlock_info* p;
  struct rwlock_thread_info* q;

  if (s_trace_rwlock)
  {
    VG_(message)(Vg_UserMsg,
                 "[%d/%d] rwlock_unlock      0x%lx",
                 vg_tid,
                 drd_tid,
                 rwlock);
  }

  p = rwlock_get(rwlock);
  if (p == 0)
  {
    GenericErrInfo GEI;
    VG_(maybe_record_error)(VG_(get_running_tid)(),
                            GenericErr,
                            VG_(get_IP)(VG_(get_running_tid)()),
                            "Not a reader-writer lock",
                            &GEI);
    return;
  }
  if (! rwlock_is_locked_by(p, drd_tid))
  {
    RwlockErrInfo REI = { p->a1 };
    VG_(maybe_record_error)(vg_tid,
                            RwlockErr,
                            VG_(get_IP)(vg_tid),
                            "Reader-writer lock not locked by calling thread",
                            &REI);
    return;
  }
  q = lookup_or_insert_node(p->thread_info, drd_tid);
  tl_assert(q);
  if (q->reader_nesting_count > 0)
  {
    q->reader_nesting_count--;
    if (q->reader_nesting_count == 0 && s_shared_threshold_ms > 0)
    {
      ULong held = VG_(read_millisecond_timer)() - p->acquiry_time_ms;
      if (held > s_shared_threshold_ms)
      {
        HoldtimeErrInfo HEI
          = { rwlock, p->acquired_at, held, s_shared_threshold_ms };
        VG_(maybe_record_error)(vg_tid,
                                HoldtimeErr,
                                VG_(get_IP)(vg_tid),
                                "rwlock",
                                &HEI);
      }
    }
  }
  else if (q->writer_nesting_count > 0)
  {
    q->writer_nesting_count--;
    if (q->writer_nesting_count == 0 && s_exclusive_threshold_ms > 0)
    {
      ULong held = VG_(read_millisecond_timer)() - p->acquiry_time_ms;
      if (held > s_exclusive_threshold_ms)
      {
        HoldtimeErrInfo HEI
          = { rwlock, p->acquired_at, held, s_exclusive_threshold_ms };
        VG_(maybe_record_error)(vg_tid,
                                HoldtimeErr,
                                VG_(get_IP)(vg_tid),
                                "rwlock",
                                &HEI);
      }
    }
  }
  else
  {
    tl_assert(False);
  }

  if (q->reader_nesting_count == 0 && q->writer_nesting_count == 0)
  {
    /* This pthread_rwlock_unlock() call really unlocks the rwlock. Save the */
    /* current vector clock of the thread such that it is available when  */
    /* this rwlock is locked again.                                        */

    thread_get_latest_segment(&q->last_unlock_segment, drd_tid);
    thread_new_segment(drd_tid);
    s_rwlock_segment_creation_count++;
  }
}

/**
 * Call this function when thread tid stops to exist, such that the
 * "last owner" field can be cleared if it still refers to that thread.
 */
void rwlock_thread_delete(const DrdThreadId tid)
{
  struct rwlock_info* p;

  clientobj_resetiter();
  for ( ; (p = &clientobj_next(ClientRwlock)->rwlock) != 0; )
  {
    struct rwlock_thread_info* q;
    if (rwlock_is_locked_by(p, tid))
    {
      RwlockErrInfo REI = { p->a1 };
      VG_(maybe_record_error)(VG_(get_running_tid)(),
                              RwlockErr,
                              VG_(get_IP)(VG_(get_running_tid)()),
                              "Reader-writer lock still locked at thread exit",
                              &REI);
      q = lookup_or_insert_node(p->thread_info, tid);
      q->reader_nesting_count = 0;
      q->writer_nesting_count = 0;
    }
  }
}

ULong get_rwlock_segment_creation_count(void)
{
  return s_rwlock_segment_creation_count;
}
