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
#include "drd_mutex.h"
#include "drd_suppression.h"
#include "priv_drd_clientreq.h"
#include "pub_tool_errormgr.h"    // VG_(maybe_record_error)()
#include "pub_tool_libcassert.h"  // tl_assert()
#include "pub_tool_libcprint.h"   // VG_(printf)()
#include "pub_tool_machine.h"     // VG_(get_IP)()
#include "pub_tool_threadstate.h" // VG_(get_running_tid)()


// Type definitions.

struct mutex_info
{
  Addr        mutex;           // Pointer to client mutex.
  SizeT       size;            // Size in bytes of client-side object.
  MutexT      mutex_type;      // pthread_mutex_t or pthread_spinlock_t.
  int         recursion_count; // 0 if free, >= 1 if locked.
  DrdThreadId owner;           // owner if locked, last owner if free.
  VectorClock vc;              // vector clock associated with last unlock.
};


// Local variables.

static Bool s_trace_mutex;
static ULong s_mutex_lock_count;
struct mutex_info s_mutex[256];


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
  tl_assert(mutex_type == mutex_type_mutex
            || mutex_type == mutex_type_spinlock);

  p->mutex           = mutex;
  p->size            = size;
  p->mutex_type      = mutex_type;
  p->recursion_count = 0;
  p->owner           = DRD_INVALID_THREADID;
  vc_init(&p->vc, 0, 0);
}

static
struct mutex_info*
mutex_get_or_allocate(const Addr mutex,
                      const SizeT size,
                      const MutexT mutex_type)
{
  int i;

  tl_assert(mutex_type == mutex_type_mutex
            || mutex_type == mutex_type_spinlock);

  for (i = 0; i < sizeof(s_mutex)/sizeof(s_mutex[0]); i++)
  {
    if (s_mutex[i].mutex == mutex)
    {
      tl_assert(s_mutex[i].mutex_type == mutex_type);
      tl_assert(s_mutex[i].size == size);
      return &s_mutex[i];
    }
  }
  for (i = 0; i < sizeof(s_mutex)/sizeof(s_mutex[0]); i++)
  {
    if (s_mutex[i].mutex == 0)
    {
      mutex_initialize(&s_mutex[i], mutex, size, mutex_type);
      drd_start_suppression(mutex, mutex + size,
                            mutex_get_typename(&s_mutex[i]));
      return &s_mutex[i];
    }
  }
  tl_assert(0);
  return 0;
}

struct mutex_info*
mutex_init(const Addr mutex, const SizeT size, const MutexT mutex_type)
{
  struct mutex_info* mutex_p;

  tl_assert(mutex_get(mutex) == 0);
  tl_assert(mutex_type == mutex_type_mutex
            || mutex_type == mutex_type_spinlock);
  mutex_p = mutex_get_or_allocate(mutex, size, mutex_type);

  if (s_trace_mutex)
  {
    const ThreadId vg_tid = VG_(get_running_tid)();
    const DrdThreadId drd_tid = VgThreadIdToDrdThreadId(vg_tid);
    VG_(message)(Vg_DebugMsg,
                 "drd_post_mutex_init  tid = %d/%d, %s 0x%lx",
                 vg_tid, drd_tid,
                 mutex_get_typename(mutex_p),
                 mutex);
  }

  return mutex_p;
}

void mutex_destroy(struct mutex_info* const p)
{
  if (s_trace_mutex)
  {
    const ThreadId vg_tid = VG_(get_running_tid)();
    const DrdThreadId drd_tid = VgThreadIdToDrdThreadId(vg_tid);
    VG_(message)(Vg_DebugMsg,
                 "drd_pre_mutex_destroy tid = %d/%d, %s 0x%lx",
                 vg_tid, drd_tid,
                 mutex_get_typename(p),
                 p->mutex);
  }

  drd_finish_suppression(p->mutex, p->mutex + p->size);

  vc_cleanup(&p->vc);
  p->mutex = 0;
}

struct mutex_info* mutex_get(const Addr mutex)
{
  int i;
  for (i = 0; i < sizeof(s_mutex)/sizeof(s_mutex[0]); i++)
    if (s_mutex[i].mutex == mutex)
      return &s_mutex[i];
  return 0;
}

/**
 * Update mutex_info state when locking the pthread_mutex_t mutex.
 * Note: this function must be called after pthread_mutex_lock() has been
 * called, or a race condition is triggered !
 */
int mutex_lock(const Addr mutex, const SizeT size, MutexT mutex_type)
{
  const DrdThreadId drd_tid = VgThreadIdToDrdThreadId(VG_(get_running_tid)());
  struct mutex_info* const p = mutex_get_or_allocate(mutex, size, mutex_type);
  const DrdThreadId last_owner = p->owner;

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

  tl_assert(mutex_type == mutex_type_mutex
            || mutex_type == mutex_type_spinlock);
  tl_assert(p->mutex_type == mutex_type);
  tl_assert(p->size == size);

  if (p->recursion_count >= 1 && mutex_type == mutex_type_spinlock)
  {
    // TO DO: tell the user in a more friendly way that it is not allowed to
    // lock spinlocks recursively.
    tl_assert(0);
  }

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
                 p->mutex, p->recursion_count, p->owner, drd_tid);
    tl_assert(0);
  }
  p->recursion_count++;

  if (p->recursion_count == 1)
  {
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
 * @param mutex Pointer to pthread_mutex_t data structure in the client space.
 * @param tid ThreadId of the thread calling pthread_mutex_unlock().
 * @param vc Pointer to the current vector clock of thread tid.
 */
int mutex_unlock(const Addr mutex, const MutexT mutex_type)
{
  const DrdThreadId drd_tid = VgThreadIdToDrdThreadId(VG_(get_running_tid)());
  const ThreadId vg_tid = DrdThreadIdToVgThreadId(drd_tid);
  const VectorClock* const vc = thread_get_vc(drd_tid);
  struct mutex_info* const p = mutex_get(mutex);

  if (s_trace_mutex)
  {
    VG_(message)(Vg_DebugMsg,
                 "drd_pre_mutex_unlock tid = %d/%d, %s 0x%lx rc %d",
                 vg_tid, drd_tid,
                 mutex_get_typename(p),
                 mutex,
                 p->recursion_count,
                 p->owner);
  }

  tl_assert(p);
  tl_assert(p->mutex_type == mutex_type);
  tl_assert(p->owner != DRD_INVALID_THREADID);
  tl_assert(mutex_type == mutex_type_mutex
            || mutex_type == mutex_type_spinlock);

  if (p->owner != drd_tid)
  {
    MutexErrInfo MEI = { p->mutex, p->recursion_count, p->owner };
    VG_(maybe_record_error)(vg_tid,
                            MutexErr,
                            VG_(get_IP)(vg_tid),
                            "Mutex not unlocked by owner thread",
                            &MEI);
  }
  p->recursion_count--;
  tl_assert(p->recursion_count >= 0);
  if (p->recursion_count == 0)
  {
    /* This pthread_mutex_unlock() call really unlocks the mutex. Save the */
    /* current vector clock of the thread such that it is available when  */
    /* this mutex is locked again.                                        */
    vc_copy(&p->vc, vc);

    thread_new_segment(drd_tid);
  }
  return p->recursion_count;
}

const char* mutex_get_typename(struct mutex_info* const p)
{
  tl_assert(p);

  switch (p->mutex_type)
  {
  case mutex_type_mutex:
    return "mutex";
  case mutex_type_spinlock:
    return "spinlock";
  default:
    tl_assert(0);
  }
  return "?";
}

Bool mutex_is_locked_by(const Addr mutex, const DrdThreadId tid)
{
  struct mutex_info* const p = mutex_get(mutex);
  tl_assert(p);
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
 * Call this function when thread threadid stops to exist, such that the
 * "last owner" field can be cleared if it still refers to that thread.
 * TO DO: print an error message if a thread exits while it still has some
 * mutexes locked.
 */
void mutex_thread_delete(const DrdThreadId threadid)
{
  int i;
  for (i = 0; i < sizeof(s_mutex)/sizeof(s_mutex[0]); i++)
  {
    struct mutex_info* const p = &s_mutex[i];
    if (p->mutex && p->owner == threadid)
    {
      p->owner = VG_INVALID_THREADID;
    }
  }
}

void mutex_stop_using_mem(const Addr a1, const Addr a2)
{
  unsigned i;
  for (i = 0; i < sizeof(s_mutex)/sizeof(s_mutex[0]); i++)
  {
    if (a1 <= s_mutex[i].mutex && s_mutex[i].mutex < a2)
    {
      tl_assert(s_mutex[i].mutex + s_mutex[i].size <= a2);
      mutex_destroy(&s_mutex[i]);
    }
  }
}

ULong get_mutex_lock_count(void)
{
  return s_mutex_lock_count;
}
