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


#include "drd_clientreq.h"
#include "drd_cond.h"
#include "drd_mutex.h"
#include "drd_semaphore.h"
#include "drd_suppression.h"      // drd_start_suppression()
#include "drd_thread.h"
#include "drd_track.h"
#include "drd_rwlock.h"
#include "priv_drd_clientreq.h"
#include "pub_tool_basics.h"      // Bool
#include "pub_tool_libcassert.h"
#include "pub_tool_libcassert.h"  // tl_assert()
#include "pub_tool_libcprint.h"   // VG_(message)()
#include "pub_tool_machine.h"     // VG_(get_SP)()
#include "pub_tool_threadstate.h"
#include "pub_tool_tooliface.h"   // VG_(needs_...)()


static void drd_spin_init_or_unlock(const Addr spinlock)
{
   struct mutex_info* mutex_p = mutex_get(spinlock);
   if (mutex_p)
   {
      mutex_unlock(spinlock, mutex_type_spinlock);
   }
   else
   {
      mutex_init(spinlock, mutex_type_spinlock);
   }
}

static void drd_pre_cond_wait(const Addr cond,
                              const Addr mutex, const MutexT mutex_type)
{
   mutex_unlock(mutex, mutex_type);
   cond_pre_wait(cond, mutex);
}

static void drd_post_cond_wait(const Addr cond,
                               const Addr mutex,
                               const Bool took_lock)
{
   cond_post_wait(cond);
   mutex_post_lock(mutex, took_lock);
}

static void drd_pre_cond_signal(const Addr cond)
{
   cond_pre_signal(cond);
}

static void drd_pre_cond_broadcast(const Addr cond)
{
   cond_pre_broadcast(cond);
}

static Bool drd_handle_client_request(ThreadId vg_tid, UWord* arg, UWord* ret)
{
   UWord result = 0;
   const DrdThreadId drd_tid = thread_get_running_tid();

   tl_assert(vg_tid == VG_(get_running_tid()));
   tl_assert(VgThreadIdToDrdThreadId(vg_tid) == drd_tid);

   switch (arg[0])
   {
   case VG_USERREQ__GET_THREAD_SELF:
      result = vg_tid;
      break;

   case VG_USERREQ__SET_THREAD_NAME:
      thread_set_name_fmt(drd_tid, (char*)arg[1], arg[2]);
      break;

   case VG_USERREQ__DRD_START_SUPPRESSION:
      drd_start_suppression(arg[1], arg[2], "client");
      break;

   case VG_USERREQ__DRD_FINISH_SUPPRESSION:
      drd_finish_suppression(arg[1], arg[2]);
      break;

   case VG_USERREQ__DRD_SUPPRESS_CURRENT_STACK:
      thread_set_stack_startup(drd_tid, VG_(get_SP)(vg_tid));
      break;

   case VG_USERREQ__DRD_START_NEW_SEGMENT:
      thread_new_segment(PtThreadIdToDrdThreadId(arg[1]));
      break;

   case VG_USERREQ__DRD_TRACE_ADDR:
      drd_trace_addr(arg[1]);
      break;

   case VG_USERREQ__SET_PTHREADID:
      thread_set_pthreadid(drd_tid, arg[1]);
      break;

   case VG_USERREQ__SET_JOINABLE:
      thread_set_joinable(PtThreadIdToDrdThreadId(arg[1]), (Bool)arg[2]);
      break;

   case VG_USERREQ__POST_THREAD_JOIN:
      tl_assert(arg[1]);
      drd_post_thread_join(drd_tid,
                           PtThreadIdToDrdThreadId(arg[1]));
      break;

   case VG_USERREQ__PRE_MUTEX_INIT:
      if (thread_enter_synchr(drd_tid) == 0)
         drd_pre_mutex_init(arg[1], arg[2]);
      break;

   case VG_USERREQ__POST_MUTEX_INIT:
      thread_leave_synchr(drd_tid);
      break;

   case VG_USERREQ__PRE_MUTEX_DESTROY:
      thread_enter_synchr(drd_tid);
      break;

   case VG_USERREQ__POST_MUTEX_DESTROY:
      if (thread_leave_synchr(drd_tid) == 0)
         drd_post_mutex_destroy(arg[1], arg[2]);
      break;

   case VG_USERREQ__PRE_MUTEX_LOCK:
      if (thread_enter_synchr(drd_tid) == 0)
         drd_pre_mutex_lock(arg[1], arg[2]);
      break;

   case VG_USERREQ__POST_MUTEX_LOCK:
      if (thread_leave_synchr(drd_tid) == 0)
         drd_post_mutex_lock(arg[1], arg[2]);
      break;

   case VG_USERREQ__PRE_MUTEX_UNLOCK:
      if (thread_enter_synchr(drd_tid) == 0)
         drd_pre_mutex_unlock(arg[1], arg[2]);
      break;

   case VG_USERREQ__POST_MUTEX_UNLOCK:
      thread_leave_synchr(drd_tid);
      break;

   case VG_USERREQ__SPIN_INIT_OR_UNLOCK:
      tl_assert(thread_get_synchr_nesting_count(drd_tid) == 0);
      drd_spin_init_or_unlock(arg[1]);
      break;

   case VG_USERREQ__PRE_COND_INIT:
      tl_assert(thread_get_synchr_nesting_count(drd_tid) == 0);
      drd_pre_cond_init(arg[1]);
      break;

   case VG_USERREQ__POST_COND_DESTROY:
      tl_assert(thread_get_synchr_nesting_count(drd_tid) == 0);
      drd_post_cond_destroy(arg[1]);
      break;

   case VG_USERREQ__PRE_COND_WAIT:
      if (thread_enter_synchr(drd_tid) == 0)
         drd_pre_cond_wait(arg[1], arg[2], arg[3]);
      break;

   case VG_USERREQ__POST_COND_WAIT:
      if (thread_leave_synchr(drd_tid) == 0)
         drd_post_cond_wait(arg[1], arg[2], arg[3]);
      break;

   case VG_USERREQ__PRE_COND_SIGNAL:
      tl_assert(thread_get_synchr_nesting_count(drd_tid) == 0);
      drd_pre_cond_signal(arg[1]);
      break;

   case VG_USERREQ__PRE_COND_BROADCAST:
      tl_assert(thread_get_synchr_nesting_count(drd_tid) == 0);
      drd_pre_cond_broadcast(arg[1]);
      break;

   case VG_USERREQ__PRE_SEM_INIT:
      if (thread_enter_synchr(drd_tid) == 0)
         drd_semaphore_init(arg[1], arg[2], arg[3]);
      break;

   case VG_USERREQ__POST_SEM_INIT:
      thread_leave_synchr(drd_tid);
      break;

   case VG_USERREQ__PRE_SEM_DESTROY:
      thread_enter_synchr(drd_tid);
      break;

   case VG_USERREQ__POST_SEM_DESTROY:
      if (thread_leave_synchr(drd_tid) == 0)
         drd_semaphore_destroy(arg[1]);
      break;

   case VG_USERREQ__PRE_SEM_WAIT:
      if (thread_enter_synchr(drd_tid) == 0)
         drd_semaphore_pre_wait(drd_tid, arg[1]);
      break;

   case VG_USERREQ__POST_SEM_WAIT:
      if (thread_leave_synchr(drd_tid) == 0)
         drd_semaphore_post_wait(drd_tid, arg[1], arg[2]);
      break;

   case VG_USERREQ__PRE_SEM_POST:
      if (thread_enter_synchr(drd_tid) == 0)
         drd_semaphore_pre_post(drd_tid, arg[1]);
      break;

   case VG_USERREQ__POST_SEM_POST:
      if (thread_leave_synchr(drd_tid) == 0)
         drd_semaphore_post_post(drd_tid, arg[1], arg[2]);
      break;

   case VG_USERREQ__PRE_BARRIER_INIT:
      if (thread_enter_synchr(drd_tid) == 0)
         drd_barrier_init(arg[1], arg[2], arg[3], arg[4]);
      break;

   case VG_USERREQ__POST_BARRIER_INIT:
      thread_leave_synchr(drd_tid);
      break;

   case VG_USERREQ__PRE_BARRIER_DESTROY:
      thread_enter_synchr(drd_tid);
      break;

   case VG_USERREQ__POST_BARRIER_DESTROY:
      if (thread_leave_synchr(drd_tid) == 0)
         drd_barrier_destroy(arg[1], arg[2]);
      break;

   case VG_USERREQ__PRE_BARRIER_WAIT:
      if (thread_enter_synchr(drd_tid) == 0)
         drd_barrier_pre_wait(drd_tid, arg[1], arg[2]);
      break;

   case VG_USERREQ__POST_BARRIER_WAIT:
      if (thread_leave_synchr(drd_tid) == 0)
         drd_barrier_post_wait(drd_tid, arg[1], arg[2], arg[3]);
      break;

   case VG_USERREQ__PRE_RWLOCK_INIT:
      rwlock_pre_init(arg[1]);
      break;

   case VG_USERREQ__POST_RWLOCK_DESTROY:
      rwlock_post_destroy(arg[1]);
      break;

   case VG_USERREQ__PRE_RWLOCK_RDLOCK:
      if (thread_enter_synchr(drd_tid) == 0)
         rwlock_pre_rdlock(arg[1]);
      break;

   case VG_USERREQ__POST_RWLOCK_RDLOCK:
      if (thread_leave_synchr(drd_tid) == 0)
         rwlock_post_rdlock(arg[1], arg[2]);
      break;

   case VG_USERREQ__PRE_RWLOCK_WRLOCK:
      if (thread_enter_synchr(drd_tid) == 0)
         rwlock_pre_wrlock(arg[1]);
      break;

   case VG_USERREQ__POST_RWLOCK_WRLOCK:
      if (thread_leave_synchr(drd_tid) == 0)
         rwlock_post_wrlock(arg[1], arg[2]);
      break;

   case VG_USERREQ__PRE_RWLOCK_UNLOCK:
      if (thread_enter_synchr(drd_tid) == 0)
         rwlock_pre_unlock(arg[1]);
      break;
      
   case VG_USERREQ__POST_RWLOCK_UNLOCK:
      thread_leave_synchr(drd_tid);
      break;

   default:
      VG_(message)(Vg_DebugMsg, "Unrecognized client request 0x%lx 0x%lx",
                   arg[0], arg[1]);
      tl_assert(0);
      return False;
   }

   *ret = result;
   return True;
}

void drd_clientreq_init(void)
{
   VG_(needs_client_requests)(drd_handle_client_request);
}

/*
 * Local variables:
 * c-basic-offset: 3
 * End:
 */
