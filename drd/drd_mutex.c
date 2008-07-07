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
#include "drd_mutex.h"
#include "pub_tool_vki.h"
#include "pub_tool_errormgr.h"    // VG_(maybe_record_error)()
#include "pub_tool_libcassert.h"  // tl_assert()
#include "pub_tool_libcbase.h"    // VG_(strlen)
#include "pub_tool_libcprint.h"   // VG_(message)()
#include "pub_tool_libcproc.h"    // VG_(read_millisecond_timer)()
#include "pub_tool_machine.h"     // VG_(get_IP)()
#include "pub_tool_threadstate.h" // VG_(get_running_tid)()


// Local functions.

static void mutex_cleanup(struct mutex_info* p);
static Bool mutex_is_locked(struct mutex_info* const p);


// Local variables.

static Bool s_trace_mutex;
static ULong s_mutex_lock_count;
static ULong s_mutex_segment_creation_count;
static UInt s_mutex_lock_threshold_ms = 1000 * 1000;


// Function definitions.

void mutex_set_trace(const Bool trace_mutex)
{
  tl_assert(!! trace_mutex == trace_mutex);
  s_trace_mutex = trace_mutex;
}

void mutex_set_lock_threshold(const UInt lock_threshold_ms)
{
  s_mutex_lock_threshold_ms = lock_threshold_ms;
}

static
void mutex_initialize(struct mutex_info* const p,
                      const Addr mutex, const MutexT mutex_type)
{
  tl_assert(mutex);
  tl_assert(mutex_type != mutex_type_unknown);
  tl_assert(p->a1 == mutex);

  p->cleanup             = (void(*)(DrdClientobj*))&mutex_cleanup;
  p->mutex_type          = mutex_type;
  p->recursion_count     = 0;
  p->owner               = DRD_INVALID_THREADID;
  p->last_locked_segment = 0;
  p->acquiry_time_ms     = 0;
  p->acquired_at         = 0;
}

/** Deallocate the memory that was allocated by mutex_initialize(). */
static void mutex_cleanup(struct mutex_info* p)
{
  tl_assert(p);

  if (s_trace_mutex)
  {
    VG_(message)(Vg_UserMsg,
                 "[%d/%d] mutex_destroy   %s 0x%lx rc %d owner %d",
                 VG_(get_running_tid)(),
                 thread_get_running_tid(),
                 mutex_get_typename(p),
                 p->a1,
                 p ? p->recursion_count : -1,
                 p ? p->owner : DRD_INVALID_THREADID);
  }

  if (mutex_is_locked(p))
  {
    MutexErrInfo MEI = { p->a1, p->recursion_count, p->owner };
    VG_(maybe_record_error)(VG_(get_running_tid)(),
                            MutexErr,
                            VG_(get_IP)(VG_(get_running_tid)()),
                            "Destroying locked mutex",
                            &MEI);
  }

  sg_put(p->last_locked_segment);
  p->last_locked_segment = 0;
}

/** Let Valgrind report that there is no mutex object at address 'mutex'. */
void not_a_mutex(const Addr mutex)
{
  MutexErrInfo MEI = { mutex, -1, DRD_INVALID_THREADID };
  VG_(maybe_record_error)(VG_(get_running_tid)(),
                          MutexErr,
                          VG_(get_IP)(VG_(get_running_tid)()),
                          "Not a mutex",
                          &MEI);
}

static
struct mutex_info*
mutex_get_or_allocate(const Addr mutex, const MutexT mutex_type)
{
  struct mutex_info* p;

  tl_assert(offsetof(DrdClientobj, mutex) == 0);
  p = &clientobj_get(mutex, ClientMutex)->mutex;
  if (p)
  {
    return p;
  }

  if (clientobj_present(mutex, mutex + 1))
  {
    not_a_mutex(mutex);
    return 0;
  }

  tl_assert(mutex_type != mutex_type_unknown);

  p = &clientobj_add(mutex, ClientMutex)->mutex;
  mutex_initialize(p, mutex, mutex_type);
  return p;
}

struct mutex_info* mutex_get(const Addr mutex)
{
  tl_assert(offsetof(DrdClientobj, mutex) == 0);
  return &clientobj_get(mutex, ClientMutex)->mutex;
}

/** Called before pthread_mutex_init(). */
struct mutex_info*
mutex_init(const Addr mutex, const MutexT mutex_type)
{
  struct mutex_info* p;

  tl_assert(mutex_type != mutex_type_unknown);

  if (s_trace_mutex)
  {
    VG_(message)(Vg_UserMsg,
                 "[%d/%d] mutex_init      %s 0x%lx",
                 VG_(get_running_tid)(),
                 thread_get_running_tid(),
                 mutex_type_name(mutex_type),
                 mutex);
  }

  if (mutex_type == mutex_type_invalid_mutex)
  {
    not_a_mutex(mutex);
    return 0;
  }

  p = mutex_get(mutex);
  if (p)
  {
    const ThreadId vg_tid = VG_(get_running_tid)();
    MutexErrInfo MEI
      = { p->a1, p->recursion_count, p->owner };
    VG_(maybe_record_error)(vg_tid,
                            MutexErr,
                            VG_(get_IP)(vg_tid),
                            "Mutex reinitialization",
                            &MEI);
    return p;
  }
  p = mutex_get_or_allocate(mutex, mutex_type);

  return p;
}

/** Called after pthread_mutex_destroy(). */
void mutex_post_destroy(const Addr mutex)
{
  struct mutex_info* p;

  p = mutex_get(mutex);
  if (p == 0)
  {
    not_a_mutex(mutex);
    return;
  }

  clientobj_remove(mutex, ClientMutex);
}

/** Called before pthread_mutex_lock() is invoked. If a data structure for
 *  the client-side object was not yet created, do this now. Also check whether
 *  an attempt is made to lock recursively a synchronization object that must
 *  not be locked recursively.
 */
void mutex_pre_lock(const Addr mutex, MutexT mutex_type,
                    const Bool trylock)
{
  struct mutex_info* p;

  p = mutex_get_or_allocate(mutex, mutex_type);
  if (mutex_type == mutex_type_unknown)
    mutex_type = p->mutex_type;

  if (s_trace_mutex)
  {
    VG_(message)(Vg_UserMsg,
                 "[%d/%d] %s %s 0x%lx rc %d owner %d",
                 VG_(get_running_tid)(),
                 thread_get_running_tid(),
                 trylock ? "pre_mutex_lock " : "mutex_trylock  ",
                 p ? mutex_get_typename(p) : "(?)",
                 mutex,
                 p ? p->recursion_count : -1,
                 p ? p->owner : DRD_INVALID_THREADID);
  }

  if (p == 0)
  {
    not_a_mutex(mutex);
    return;
  }

  tl_assert(p);

  if (mutex_type == mutex_type_invalid_mutex)
  {
    not_a_mutex(mutex);
    return;
  }

  if (! trylock
      && p->owner == thread_get_running_tid()
      && p->recursion_count >= 1
      && mutex_type != mutex_type_recursive_mutex)
  {
    MutexErrInfo MEI = { p->a1, p->recursion_count, p->owner };
    VG_(maybe_record_error)(VG_(get_running_tid)(),
                            MutexErr,
                            VG_(get_IP)(VG_(get_running_tid)()),
                            "Recursive locking not allowed",
                            &MEI);
  }
}

/**
 * Update mutex_info state when locking the pthread_mutex_t mutex.
 * Note: this function must be called after pthread_mutex_lock() has been
 * called, or a race condition is triggered !
 */
void mutex_post_lock(const Addr mutex, const Bool took_lock,
                     const Bool post_cond_wait)
{
  const DrdThreadId drd_tid = thread_get_running_tid();
  struct mutex_info* p;

  p = mutex_get(mutex);

  if (s_trace_mutex)
  {
    VG_(message)(Vg_UserMsg,
                 "[%d/%d] %s %s 0x%lx rc %d owner %d%s",
                 VG_(get_running_tid)(),
                 drd_tid,
                 post_cond_wait ? "cond_post_wait " : "post_mutex_lock",
                 p ? mutex_get_typename(p) : "(?)",
                 mutex,
                 p ? p->recursion_count : 0,
                 p ? p->owner : VG_INVALID_THREADID,
                 took_lock ? "" : " (locking failed)");
  }

  if (! p || ! took_lock)
    return;

  if (p->recursion_count == 0)
  {
    const DrdThreadId last_owner = p->owner;

    if (last_owner != drd_tid && last_owner != DRD_INVALID_THREADID)
    {
      tl_assert(p->last_locked_segment);
      thread_combine_vc2(drd_tid, &p->last_locked_segment->vc);
    }
    thread_new_segment(drd_tid);
    s_mutex_segment_creation_count++;

    p->owner           = drd_tid;
    p->acquiry_time_ms = VG_(read_millisecond_timer)();
    p->acquired_at     = VG_(record_ExeContext)(VG_(get_running_tid)(), 0);
    s_mutex_lock_count++;
  }
  else if (p->owner != drd_tid)
  {
    VG_(message)(Vg_UserMsg,
                 "The impossible happened: mutex 0x%lx is locked"
                 " simultaneously by two threads (recursion count %d,"
                 " owners %d and %d) !",
                 p->a1, p->recursion_count, p->owner, drd_tid);
    p->owner = drd_tid;
  }
  p->recursion_count++;
}

/** Update mutex_info state when unlocking the pthread_mutex_t mutex.
 *
 *  @param mutex Pointer to pthread_mutex_t data structure in the client space.
 *  @param tid ThreadId of the thread calling pthread_mutex_unlock().
 *  @param vc Pointer to the current vector clock of thread tid.
 *
 *  @return New value of the mutex recursion count.
 *
 *  @note This function must be called before pthread_mutex_unlock() is called,
 *        or a race condition is triggered !
 */
void mutex_unlock(const Addr mutex, MutexT mutex_type)
{
  const DrdThreadId drd_tid = thread_get_running_tid();
  const ThreadId vg_tid = VG_(get_running_tid)();
  struct mutex_info* p;

  p = mutex_get(mutex);
  if (mutex_type == mutex_type_unknown)
    mutex_type = p->mutex_type;

  if (s_trace_mutex)
  {
    VG_(message)(Vg_UserMsg,
                 "[%d/%d] mutex_unlock    %s 0x%lx rc %d",
                 vg_tid,
                 drd_tid,
                 p ? mutex_get_typename(p) : "(?)",
                 mutex,
                 p ? p->recursion_count : 0);
  }

  if (p == 0 || mutex_type == mutex_type_invalid_mutex)
  {
    not_a_mutex(mutex);
    return;
  }

  if (p->owner == DRD_INVALID_THREADID)
  {
    MutexErrInfo MEI = { p->a1, p->recursion_count, p->owner };
    VG_(maybe_record_error)(vg_tid,
                            MutexErr,
                            VG_(get_IP)(vg_tid),
                            "Mutex not locked",
                            &MEI);
    return;
  }

  tl_assert(p);
  if (p->mutex_type != mutex_type)
  {
    VG_(message)(Vg_UserMsg, "??? mutex 0x%lx: type changed from %d into %d",
                 p->a1, p->mutex_type, mutex_type);
  }
  tl_assert(p->mutex_type == mutex_type);
  tl_assert(p->owner != DRD_INVALID_THREADID);

  if (p->owner != drd_tid || p->recursion_count <= 0)
  {
    MutexErrInfo MEI = { p->a1, p->recursion_count, p->owner };
    VG_(maybe_record_error)(vg_tid,
                            MutexErr,
                            VG_(get_IP)(vg_tid),
                            "Mutex not locked by calling thread",
                            &MEI);
    return;
  }
  tl_assert(p->recursion_count > 0);
  p->recursion_count--;
  tl_assert(p->recursion_count >= 0);

  if (p->recursion_count == 0)
  {
    if (s_mutex_lock_threshold_ms > 0)
    {
      ULong held = VG_(read_millisecond_timer)() - p->acquiry_time_ms;
      if (held > s_mutex_lock_threshold_ms)
      {
        HoldtimeErrInfo HEI
          = { mutex, p->acquired_at, held, s_mutex_lock_threshold_ms };
        VG_(maybe_record_error)(vg_tid,
                                HoldtimeErr,
                                VG_(get_IP)(vg_tid),
                                "mutex",
                                &HEI);
      }
    }

    /* This pthread_mutex_unlock() call really unlocks the mutex. Save the */
    /* current vector clock of the thread such that it is available when  */
    /* this mutex is locked again.                                        */

    thread_get_latest_segment(&p->last_locked_segment, drd_tid);
    thread_new_segment(drd_tid);
    p->acquired_at = 0;
    s_mutex_segment_creation_count++;
  }
}

const char* mutex_get_typename(struct mutex_info* const p)
{
  tl_assert(p);

  return mutex_type_name(p->mutex_type);
}

const char* mutex_type_name(const MutexT mt)
{
  switch (mt)
  {
  case mutex_type_invalid_mutex:
    return "invalid mutex";
  case mutex_type_recursive_mutex:
    return "recursive mutex";
  case mutex_type_errorcheck_mutex:
    return "error checking mutex";
  case mutex_type_default_mutex:
    return "mutex";
  case mutex_type_spinlock:
    return "spinlock";
  default:
    tl_assert(0);
  }
  return "?";
}

/** Return true if the specified mutex is locked by any thread. */
static Bool mutex_is_locked(struct mutex_info* const p)
{
  tl_assert(p);
  return (p->recursion_count > 0);
}

Bool mutex_is_locked_by(const Addr mutex, const DrdThreadId tid)
{
  struct mutex_info* const p = mutex_get(mutex);
  if (p)
  {
    return (p->recursion_count > 0 && p->owner == tid);
  }
  return False;
}

int mutex_get_recursion_count(const Addr mutex)
{
  struct mutex_info* const p = mutex_get(mutex);
  tl_assert(p);
  return p->recursion_count;
}

/**
 * Call this function when thread tid stops to exist, such that the
 * "last owner" field can be cleared if it still refers to that thread.
 */
void mutex_thread_delete(const DrdThreadId tid)
{
  struct mutex_info* p;

  clientobj_resetiter();
  for ( ; (p = &clientobj_next(ClientMutex)->mutex) != 0; )
  {
    if (p->owner == tid && p->recursion_count > 0)
    {
      MutexErrInfo MEI
        = { p->a1, p->recursion_count, p->owner };
      VG_(maybe_record_error)(VG_(get_running_tid)(),
                              MutexErr,
                              VG_(get_IP)(VG_(get_running_tid)()),
                              "Mutex still locked at thread exit",
                              &MEI);
      p->owner = VG_INVALID_THREADID;
    }
  }
}

ULong get_mutex_lock_count(void)
{
  return s_mutex_lock_count;
}

ULong get_mutex_segment_creation_count(void)
{
  return s_mutex_segment_creation_count;
}
