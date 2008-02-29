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
#include "priv_drd_clientreq.h"
#include "pub_tool_errormgr.h"    // VG_(maybe_record_error)()
#include "pub_tool_libcassert.h"  // tl_assert()
#include "pub_tool_libcprint.h"   // VG_(message)()
#include "pub_tool_machine.h"     // VG_(get_IP)()
#include "pub_tool_threadstate.h" // VG_(get_running_tid)()


// Local functions.

static void mutex_cleanup(struct mutex_info* p);
static Bool mutex_is_locked(struct mutex_info* const p);
static void mutex_destroy(struct mutex_info* const p);


// Local variables.

static Bool s_trace_mutex;
static ULong s_mutex_lock_count;


// Function definitions.

void mutex_set_trace(const Bool trace_mutex)
{
  tl_assert(!! trace_mutex == trace_mutex);
  s_trace_mutex = trace_mutex;
}

static
void mutex_initialize(struct mutex_info* const p,
                      const Addr mutex,
                      const SizeT size,
                      const MutexT mutex_type)
{
  tl_assert(mutex != 0);
  tl_assert(size > 0);

  tl_assert(p->a1 == mutex);
  tl_assert(p->a2 == mutex + size);
  p->cleanup         = (void(*)(DrdClientobj*))&mutex_cleanup;
  p->mutex_type      = mutex_type;
  p->recursion_count = 0;
  p->owner           = DRD_INVALID_THREADID;
  vc_init(&p->vc, 0, 0);
}

/** Deallocate the memory that was allocated by mutex_initialize(). */
static void mutex_cleanup(struct mutex_info* p)
{
  if (s_trace_mutex)
  {
    const ThreadId vg_tid = VG_(get_running_tid)();
    const DrdThreadId drd_tid = VgThreadIdToDrdThreadId(vg_tid);
    VG_(message)(Vg_DebugMsg,
                 "drd_pre_mutex_destroy tid = %d/%d, %s 0x%lx",
                 vg_tid, drd_tid,
                 mutex_get_typename(p),
                 p->a1);
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

  vc_cleanup(&p->vc);
}

static
struct mutex_info*
mutex_get_or_allocate(const Addr mutex,
                      const SizeT size,
                      const MutexT mutex_type)
{
  struct mutex_info* p;

  tl_assert(offsetof(DrdClientobj, mutex) == 0);
  p = &drd_clientobj_get(mutex, ClientMutex)->mutex;
  if (p)
  {
    tl_assert(p->mutex_type == mutex_type);
    tl_assert(p->a2 - p->a1 == size);
    return p;
  }

  if (drd_clientobj_present(mutex, mutex + size))
  {
     GenericErrInfo GEI;
     VG_(maybe_record_error)(VG_(get_running_tid)(),
                             GenericErr,
                             VG_(get_IP)(VG_(get_running_tid)()),
                             "Not a mutex",
                             &GEI);
     return 0;
  }

  p = &drd_clientobj_add(mutex, mutex + size, ClientMutex)->mutex;
  mutex_initialize(p, mutex, size, mutex_type);
  return p;
}

struct mutex_info*
mutex_init(const Addr mutex, const SizeT size, const MutexT mutex_type)
{
  struct mutex_info* mutex_p;

  if (s_trace_mutex)
  {
    const ThreadId vg_tid = VG_(get_running_tid)();
    const DrdThreadId drd_tid = VgThreadIdToDrdThreadId(vg_tid);
    VG_(message)(Vg_DebugMsg,
                 "drd_post_mutex_init  tid = %d/%d, %s 0x%lx",
                 vg_tid, drd_tid,
                 mutex_type_name(mutex_type),
                 mutex);
  }

  mutex_p = mutex_get(mutex);
  if (mutex_p)
  {
    const ThreadId vg_tid = VG_(get_running_tid)();
    MutexErrInfo MEI
      = { mutex_p->a1, mutex_p->recursion_count, mutex_p->owner };
    VG_(maybe_record_error)(vg_tid,
                            MutexErr,
                            VG_(get_IP)(vg_tid),
                            "Mutex reinitialization",
                            &MEI);
    return mutex_p;
  }
  mutex_p = mutex_get_or_allocate(mutex, size, mutex_type);

  return mutex_p;
}

static void mutex_destroy(struct mutex_info* const p)
{
  drd_clientobj_remove(p->a1, ClientMutex);
}

/** Called after pthread_mutex_destroy(). */
void mutex_post_destroy(const Addr mutex)
{
   struct mutex_info* p;

   p = mutex_get(mutex);
   tl_assert(p);
   if (p)
   {
      mutex_destroy(p);
   }
}

struct mutex_info* mutex_get(const Addr mutex)
{
  tl_assert(offsetof(DrdClientobj, mutex) == 0);
  return &drd_clientobj_get(mutex, ClientMutex)->mutex;
}

/** Called before pthread_mutex_lock() is invoked. If a data structure for
 *  the client-side object was not yet created, do this now. Also check whether
 *  an attempt is made to lock recursively a synchronization object that must
 *  not be locked recursively.
 */
void mutex_pre_lock(const Addr mutex, const SizeT size, MutexT mutex_type)
{
  struct mutex_info* p;

  p = mutex_get(mutex);
  if (p == 0)
  {
    mutex_init(mutex, size, mutex_type);
    p = mutex_get(mutex);
  }

  tl_assert(p);

  if (p->owner == thread_get_running_tid()
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
int mutex_post_lock(const Addr mutex, const SizeT size, MutexT mutex_type)
{
  const DrdThreadId drd_tid = VgThreadIdToDrdThreadId(VG_(get_running_tid)());
  struct mutex_info* const p = mutex_get_or_allocate(mutex, size, mutex_type);

  if (s_trace_mutex)
  {
    const ThreadId tid = DrdThreadIdToVgThreadId(drd_tid);
    VG_(message)(Vg_DebugMsg,
                 "drd_post_mutex_lock  tid = %d/%d, %s 0x%lx rc %d owner %d",
                 tid,
                 drd_tid,
                 mutex_get_typename(p),
                 mutex,
                 p ? p->recursion_count : 0,
                 p ? p->owner : VG_INVALID_THREADID);
  }

  if (mutex_type == mutex_type_invalid_mutex)
  {
    GenericErrInfo GEI;
    VG_(maybe_record_error)(VG_(get_running_tid)(),
                            GenericErr,
                            VG_(get_IP)(VG_(get_running_tid)()),
                            "Invalid mutex",
                            &GEI);
  }

  if (p == 0)
  {
     GenericErrInfo GEI;
     VG_(maybe_record_error)(VG_(get_running_tid)(),
                             GenericErr,
                             VG_(get_IP)(VG_(get_running_tid)()),
                             "Not a mutex",
                             &GEI);
     return 0;
  }

#if 0
  tl_assert(mutex_type == mutex_type_mutex
            || mutex_type == mutex_type_spinlock);
#endif

  tl_assert(p->mutex_type == mutex_type);
  tl_assert(p->a2 - p->a1 == size);

  if (p->recursion_count == 0)
  {
    p->owner = drd_tid;
    s_mutex_lock_count++;
  }
  else if (p->owner != drd_tid)
  {
    VG_(message)(Vg_DebugMsg,
                 "The impossible happened: mutex 0x%lx is locked"
                 " simultaneously by two threads (recursion count %d,"
                 " owners %d and %d) !",
                 p->a1, p->recursion_count, p->owner, drd_tid);
    p->owner = drd_tid;
  }
  p->recursion_count++;

  if (p->recursion_count == 1)
  {
     const DrdThreadId last_owner = p->owner;

    if (last_owner != drd_tid && last_owner != DRD_INVALID_THREADID)
      thread_combine_vc2(drd_tid, mutex_get_last_vc(mutex));
    thread_new_segment(drd_tid);
  }

  return p->recursion_count;
}

/**
 * Update mutex_info state when unlocking the pthread_mutex_t mutex.
 * Note: this function must be called before pthread_mutex_unlock() is called,
 * or a race condition is triggered !
 * @return New value of the mutex recursion count.
 * @param mutex Pointer to pthread_mutex_t data structure in the client space.
 * @param tid ThreadId of the thread calling pthread_mutex_unlock().
 * @param vc Pointer to the current vector clock of thread tid.
 */
int mutex_unlock(const Addr mutex, const MutexT mutex_type)
{
  const DrdThreadId drd_tid = thread_get_running_tid();
  const ThreadId vg_tid = VG_(get_running_tid)();
  const VectorClock* const vc = thread_get_vc(drd_tid);
  struct mutex_info* const p = mutex_get(mutex);

  if (s_trace_mutex && p != 0)
  {
    VG_(message)(Vg_DebugMsg,
                 "drd_pre_mutex_unlock tid = %d/%d, %s 0x%lx rc %d",
                 vg_tid, drd_tid,
                 mutex_get_typename(p),
                 mutex,
                 p->recursion_count,
                 p->owner);
  }

  if (mutex_type == mutex_type_invalid_mutex)
  {
    GenericErrInfo GEI;
    VG_(maybe_record_error)(VG_(get_running_tid)(),
                            GenericErr,
                            VG_(get_IP)(VG_(get_running_tid)()),
                            "Invalid mutex",
                            &GEI);
  }

  if (p == 0)
  {
     GenericErrInfo GEI;
     VG_(maybe_record_error)(vg_tid,
                             GenericErr,
                             VG_(get_IP)(vg_tid),
                             "Not a mutex",
                             &GEI);
     return 0;
  }

  if (p->owner == DRD_INVALID_THREADID)
  {
    MutexErrInfo MEI = { p->a1, p->recursion_count, p->owner };
    VG_(maybe_record_error)(vg_tid,
                            MutexErr,
                            VG_(get_IP)(vg_tid),
                            "Mutex not locked",
                            &MEI);
     return 0;
  }

  tl_assert(p);
  if (p->mutex_type != mutex_type)
  {
    VG_(message)(Vg_DebugMsg, "??? mutex %p: type changed from %d into %d",
	         p->a1, p->mutex_type, mutex_type);
  }
  tl_assert(p->mutex_type == mutex_type);
  tl_assert(p->owner != DRD_INVALID_THREADID);

  if (p->owner != drd_tid)
  {
    MutexErrInfo MEI = { p->a1, p->recursion_count, p->owner };
    VG_(maybe_record_error)(vg_tid,
                            MutexErr,
                            VG_(get_IP)(vg_tid),
                            "Mutex not unlocked by owner thread",
                            &MEI);
  }
  p->recursion_count--;
  if (p->recursion_count < 0)
  {
    MutexErrInfo MEI
      = { p->a1, p->recursion_count, p->owner };
    VG_(maybe_record_error)(vg_tid,
                            MutexErr,
                            VG_(get_IP)(vg_tid),
                            "Attempt to unlock a mutex that is not locked",
                            &MEI);
    p->recursion_count = 0;
  }

  if (p->recursion_count == 0)
  {
    /* This pthread_mutex_unlock() call really unlocks the mutex. Save the */
    /* current vector clock of the thread such that it is available when  */
    /* this mutex is locked again.                                        */
    vc_assign(&p->vc, vc);

    thread_new_segment(drd_tid);
  }
  return p->recursion_count;
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

const VectorClock* mutex_get_last_vc(const Addr mutex)
{
  struct mutex_info* const p = mutex_get(mutex);
  return p ? &p->vc : 0;
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

  drd_clientobj_resetiter();
  for ( ; (p = &drd_clientobj_next(ClientMutex)->mutex) != 0; )
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


/*
 * Local variables:
 * c-basic-offset: 3
 * End:
 */
