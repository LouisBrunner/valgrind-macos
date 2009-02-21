/*
  This file is part of drd, a thread error detector.

  Copyright (C) 2006-2009 Bart Van Assche <bart.vanassche@gmail.com>.

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


#include "drd_barrier.h"
#include "drd_clientreq.h"
#include "drd_cond.h"
#include "drd_mutex.h"
#include "drd_rwlock.h"
#include "drd_semaphore.h"
#include "drd_suppression.h"      // drd_start_suppression()
#include "drd_thread.h"
#include "pub_tool_basics.h"      // Bool
#include "pub_tool_debuginfo.h"   // VG_(describe_IP)()
#include "pub_tool_libcassert.h"
#include "pub_tool_libcassert.h"  // tl_assert()
#include "pub_tool_libcprint.h"   // VG_(message)()
#include "pub_tool_machine.h"     // VG_(get_SP)()
#include "pub_tool_threadstate.h"
#include "pub_tool_tooliface.h"   // VG_(needs_...)()


/* Local function declarations. */

static
Bool DRD_(handle_client_request)(ThreadId vg_tid, UWord* arg, UWord* ret);
static Addr DRD_(highest_used_stack_address)(const ThreadId vg_tid);


/* Function definitions. */

/**
 * Tell the Valgrind core the address of the DRD function that processes
 * client requests. Must be called before any client code is run.
 */
void DRD_(clientreq_init)(void)
{
  VG_(needs_client_requests)(DRD_(handle_client_request));
}

/**
 * DRD's handler for Valgrind client requests. The code below handles both
 * DRD's public and tool-internal client requests.
 */
static
Bool DRD_(handle_client_request)(ThreadId vg_tid, UWord* arg, UWord* ret)
{
  UWord result = 0;
  const DrdThreadId drd_tid = DRD_(thread_get_running_tid)();

  tl_assert(vg_tid == VG_(get_running_tid()));
  tl_assert(DRD_(VgThreadIdToDrdThreadId)(vg_tid) == drd_tid);

  switch (arg[0])
  {
  case VG_USERREQ__DRD_GET_VALGRIND_THREAD_ID:
    result = vg_tid;
    break;

  case VG_USERREQ__DRD_GET_DRD_THREAD_ID:
    result = drd_tid;
    break;

  case VG_USERREQ__DRD_START_SUPPRESSION:
    DRD_(start_suppression)(arg[1], arg[1] + arg[2], "client");
    break;

  case VG_USERREQ__DRD_FINISH_SUPPRESSION:
    DRD_(finish_suppression)(arg[1], arg[1] + arg[2]);
    break;

  case VG_USERREQ__DRD_SUPPRESS_CURRENT_STACK:
  {
    const Addr topmost_sp = DRD_(highest_used_stack_address)(vg_tid);
#if 0
    UInt nframes;
    const UInt n_ips = 20;
    Addr ips[n_ips], sps[n_ips], fps[n_ips];
    Char desc[128];
    unsigned i;

    nframes = VG_(get_StackTrace)(vg_tid, ips, n_ips, sps, fps, 0);

    VG_(message)(Vg_DebugMsg, "thread %d/%d", vg_tid, drd_tid);
    for (i = 0; i < nframes; i++)
    {
      VG_(describe_IP)(ips[i], desc, sizeof(desc));
      VG_(message)(Vg_DebugMsg, "[%2d] sp 0x%09lx fp 0x%09lx ip %s",
                   i, sps[i], fps[i], desc);
    }
#endif
    DRD_(thread_set_stack_startup)(drd_tid, VG_(get_SP)(vg_tid));
    DRD_(start_suppression)(topmost_sp, VG_(thread_get_stack_max)(vg_tid),
                            "stack top");
    break;
  }

  case VG_USERREQ__DRD_START_NEW_SEGMENT:
    DRD_(thread_new_segment)(DRD_(PtThreadIdToDrdThreadId)(arg[1]));
    break;

  case VG_USERREQ__DRD_START_TRACE_ADDR:
    DRD_(start_tracing_address_range)(arg[1], arg[1] + arg[2]);
    break;

  case VG_USERREQ__DRD_STOP_TRACE_ADDR:
    DRD_(stop_tracing_address_range)(arg[1], arg[1] + arg[2]);
    break;

  case VG_USERREQ__DRD_STOP_RECORDING:
    DRD_(thread_stop_recording)(drd_tid);
    break;

  case VG_USERREQ__DRD_START_RECORDING:
    DRD_(thread_start_recording)(drd_tid);
    break;

  case VG_USERREQ__SET_PTHREADID:
    // pthread_self() returns 0 for programs not linked with libpthread.so.
    if (arg[1] != INVALID_POSIX_THREADID)
      DRD_(thread_set_pthreadid)(drd_tid, arg[1]);
    break;

  case VG_USERREQ__SET_JOINABLE:
    DRD_(thread_set_joinable)(DRD_(PtThreadIdToDrdThreadId)(arg[1]),
                              (Bool)arg[2]);
    break;

  case VG_USERREQ__POST_THREAD_JOIN:
    tl_assert(arg[1]);
    DRD_(thread_post_join)(drd_tid, DRD_(PtThreadIdToDrdThreadId)(arg[1]));
    break;

  case VG_USERREQ__PRE_THREAD_CANCEL:
    tl_assert(arg[1]);
    DRD_(thread_pre_cancel)(drd_tid);
    break;

  case VG_USERREQ__POST_THREAD_CANCEL:
    tl_assert(arg[1]);
    break;

  case VG_USERREQ__PRE_MUTEX_INIT:
    if (DRD_(thread_enter_synchr)(drd_tid) == 0)
      DRD_(mutex_init)(arg[1], arg[2]);
    break;

  case VG_USERREQ__POST_MUTEX_INIT:
    DRD_(thread_leave_synchr)(drd_tid);
    break;

  case VG_USERREQ__PRE_MUTEX_DESTROY:
    DRD_(thread_enter_synchr)(drd_tid);
    break;

  case VG_USERREQ__POST_MUTEX_DESTROY:
    if (DRD_(thread_leave_synchr)(drd_tid) == 0)
      DRD_(mutex_post_destroy)(arg[1]);
    break;

  case VG_USERREQ__PRE_MUTEX_LOCK:
    if (DRD_(thread_enter_synchr)(drd_tid) == 0)
      DRD_(mutex_pre_lock)(arg[1], arg[2], arg[3]);
    break;

  case VG_USERREQ__POST_MUTEX_LOCK:
    if (DRD_(thread_leave_synchr)(drd_tid) == 0)
      DRD_(mutex_post_lock)(arg[1], arg[2], False/*post_cond_wait*/);
    break;

  case VG_USERREQ__PRE_MUTEX_UNLOCK:
    if (DRD_(thread_enter_synchr)(drd_tid) == 0)
      DRD_(mutex_unlock)(arg[1], arg[2]);
    break;

  case VG_USERREQ__POST_MUTEX_UNLOCK:
    DRD_(thread_leave_synchr)(drd_tid);
    break;

  case VG_USERREQ__PRE_SPIN_INIT_OR_UNLOCK:
    if (DRD_(thread_enter_synchr)(drd_tid) == 0)
      DRD_(spinlock_init_or_unlock)(arg[1]);
    break;

  case VG_USERREQ__POST_SPIN_INIT_OR_UNLOCK:
    DRD_(thread_leave_synchr)(drd_tid);
    break;

  case VG_USERREQ__PRE_COND_INIT:
    if (DRD_(thread_enter_synchr)(drd_tid) == 0)
      DRD_(cond_pre_init)(arg[1]);
    break;

  case VG_USERREQ__POST_COND_INIT:
    DRD_(thread_leave_synchr)(drd_tid);
    break;

  case VG_USERREQ__PRE_COND_DESTROY:
    DRD_(thread_enter_synchr)(drd_tid);
    break;

  case VG_USERREQ__POST_COND_DESTROY:
    if (DRD_(thread_leave_synchr)(drd_tid) == 0)
      DRD_(cond_post_destroy)(arg[1]);
    break;

  case VG_USERREQ__PRE_COND_WAIT:
    if (DRD_(thread_enter_synchr)(drd_tid) == 0)
    {
      const Addr cond = arg[1];
      const Addr mutex = arg[2];
      const MutexT mutex_type = arg[3];
      DRD_(mutex_unlock)(mutex, mutex_type);
      DRD_(cond_pre_wait)(cond, mutex);
    }
    break;

  case VG_USERREQ__POST_COND_WAIT:
    if (DRD_(thread_leave_synchr)(drd_tid) == 0)
    {
      const Addr cond = arg[1];
      const Addr mutex = arg[2];
      const Bool took_lock = arg[3];
      DRD_(cond_post_wait)(cond);
      DRD_(mutex_post_lock)(mutex, took_lock, True);
    }
    break;

  case VG_USERREQ__PRE_COND_SIGNAL:
    if (DRD_(thread_enter_synchr)(drd_tid) == 0)
      DRD_(cond_pre_signal)(arg[1]);
    break;

  case VG_USERREQ__POST_COND_SIGNAL:
    DRD_(thread_leave_synchr)(drd_tid);
    break;

  case VG_USERREQ__PRE_COND_BROADCAST:
    if (DRD_(thread_enter_synchr)(drd_tid) == 0)
      DRD_(cond_pre_broadcast)(arg[1]);
    break;

  case VG_USERREQ__POST_COND_BROADCAST:
    DRD_(thread_leave_synchr)(drd_tid);
    break;

  case VG_USERREQ__PRE_SEM_INIT:
    if (DRD_(thread_enter_synchr)(drd_tid) == 0)
      DRD_(semaphore_init)(arg[1], arg[2], arg[3]);
    break;

  case VG_USERREQ__POST_SEM_INIT:
    DRD_(thread_leave_synchr)(drd_tid);
    break;

  case VG_USERREQ__PRE_SEM_DESTROY:
    DRD_(thread_enter_synchr)(drd_tid);
    break;

  case VG_USERREQ__POST_SEM_DESTROY:
    if (DRD_(thread_leave_synchr)(drd_tid) == 0)
      DRD_(semaphore_destroy)(arg[1]);
    break;

  case VG_USERREQ__PRE_SEM_WAIT:
    if (DRD_(thread_enter_synchr)(drd_tid) == 0)
      DRD_(semaphore_pre_wait)(arg[1]);
    break;

  case VG_USERREQ__POST_SEM_WAIT:
    if (DRD_(thread_leave_synchr)(drd_tid) == 0)
      DRD_(semaphore_post_wait)(drd_tid, arg[1], arg[2]);
    break;

  case VG_USERREQ__PRE_SEM_POST:
    if (DRD_(thread_enter_synchr)(drd_tid) == 0)
      DRD_(semaphore_pre_post)(drd_tid, arg[1]);
    break;

  case VG_USERREQ__POST_SEM_POST:
    if (DRD_(thread_leave_synchr)(drd_tid) == 0)
      DRD_(semaphore_post_post)(drd_tid, arg[1], arg[2]);
    break;

  case VG_USERREQ__PRE_BARRIER_INIT:
    if (DRD_(thread_enter_synchr)(drd_tid) == 0)
      DRD_(barrier_init)(arg[1], arg[2], arg[3], arg[4]);
    break;

  case VG_USERREQ__POST_BARRIER_INIT:
    DRD_(thread_leave_synchr)(drd_tid);
    break;

  case VG_USERREQ__PRE_BARRIER_DESTROY:
    DRD_(thread_enter_synchr)(drd_tid);
    break;

  case VG_USERREQ__POST_BARRIER_DESTROY:
    if (DRD_(thread_leave_synchr)(drd_tid) == 0)
      DRD_(barrier_destroy)(arg[1], arg[2]);
    break;

  case VG_USERREQ__PRE_BARRIER_WAIT:
    if (DRD_(thread_enter_synchr)(drd_tid) == 0)
      DRD_(barrier_pre_wait)(drd_tid, arg[1], arg[2]);
    break;

  case VG_USERREQ__POST_BARRIER_WAIT:
    if (DRD_(thread_leave_synchr)(drd_tid) == 0)
      DRD_(barrier_post_wait)(drd_tid, arg[1], arg[2], arg[3], arg[4]);
    break;

  case VG_USERREQ__PRE_RWLOCK_INIT:
    DRD_(rwlock_pre_init)(arg[1]);
    break;

  case VG_USERREQ__POST_RWLOCK_DESTROY:
    DRD_(rwlock_post_destroy)(arg[1]);
    break;

  case VG_USERREQ__PRE_RWLOCK_RDLOCK:
    if (DRD_(thread_enter_synchr)(drd_tid) == 0)
      DRD_(rwlock_pre_rdlock)(arg[1]);
    break;

  case VG_USERREQ__POST_RWLOCK_RDLOCK:
    if (DRD_(thread_leave_synchr)(drd_tid) == 0)
      DRD_(rwlock_post_rdlock)(arg[1], arg[2]);
    break;

  case VG_USERREQ__PRE_RWLOCK_WRLOCK:
    if (DRD_(thread_enter_synchr)(drd_tid) == 0)
      DRD_(rwlock_pre_wrlock)(arg[1]);
    break;

  case VG_USERREQ__POST_RWLOCK_WRLOCK:
    if (DRD_(thread_leave_synchr)(drd_tid) == 0)
      DRD_(rwlock_post_wrlock)(arg[1], arg[2]);
    break;

  case VG_USERREQ__PRE_RWLOCK_UNLOCK:
    if (DRD_(thread_enter_synchr)(drd_tid) == 0)
      DRD_(rwlock_pre_unlock)(arg[1]);
    break;
      
  case VG_USERREQ__POST_RWLOCK_UNLOCK:
    DRD_(thread_leave_synchr)(drd_tid);
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

/**
 * Walk the stack up to the highest stack frame, and return the stack pointer
 * of the highest stack frame. It is assumed that there are no more than
 * ten stack frames above the current frame. This should be no problem
 * since this function is either called indirectly from the _init() function
 * in vgpreload_exp-drd-*.so or from the thread wrapper for a newly created
 * thread. See also drd_pthread_intercepts.c.
 */
static Addr DRD_(highest_used_stack_address)(const ThreadId vg_tid)
{
    UInt nframes;
    const UInt n_ips = 10;
    UInt i;
    Addr ips[n_ips], sps[n_ips];
    Addr husa;

    nframes = VG_(get_StackTrace)(vg_tid, ips, n_ips, sps, 0, 0);
    tl_assert(1 <= nframes && nframes <= n_ips);

    /* A hack to work around VG_(get_StackTrace)()'s behavior that sometimes */
    /* the topmost stackframes it returns are bogus (this occurs sometimes   */
    /* at least on amd64, ppc32 and ppc64).                                  */

    husa = sps[0];

    tl_assert(VG_(thread_get_stack_max)(vg_tid)
              - VG_(thread_get_stack_size)(vg_tid) <= husa
              && husa < VG_(thread_get_stack_max)(vg_tid));

    for (i = 1; i < nframes; i++)
    {
      if (sps[i] == 0)
        break;
      if (husa < sps[i] && sps[i] < VG_(thread_get_stack_max)(vg_tid))
        husa = sps[i];
    }

    tl_assert(VG_(thread_get_stack_max)(vg_tid)
              - VG_(thread_get_stack_size)(vg_tid) <= husa
              && husa < VG_(thread_get_stack_max)(vg_tid));

    return husa;
}
