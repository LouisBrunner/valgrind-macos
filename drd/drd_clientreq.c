/* -*- mode: C; c-basic-offset: 3; -*- */
/*
  This file is part of drd, a thread error detector.

  Copyright (C) 2006-2010 Bart Van Assche <bvanassche@acm.org>.

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
#include "drd_error.h"
#include "drd_hb.h"
#include "drd_load_store.h"
#include "drd_malloc_wrappers.h"
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

static Bool handle_client_request(ThreadId vg_tid, UWord* arg, UWord* ret);


/* Function definitions. */

/**
 * Tell the Valgrind core the address of the DRD function that processes
 * client requests. Must be called before any client code is run.
 */
void DRD_(clientreq_init)(void)
{
   VG_(needs_client_requests)(handle_client_request);
}

/**
 * DRD's handler for Valgrind client requests. The code below handles both
 * DRD's public and tool-internal client requests.
 */
static Bool handle_client_request(ThreadId vg_tid, UWord* arg, UWord* ret)
{
   UWord result = 0;
   const DrdThreadId drd_tid = DRD_(thread_get_running_tid)();

   tl_assert(vg_tid == VG_(get_running_tid()));
   tl_assert(DRD_(VgThreadIdToDrdThreadId)(vg_tid) == drd_tid);

   switch (arg[0])
   {
   case VG_USERREQ__MALLOCLIKE_BLOCK:
      if (arg[1])
         DRD_(malloclike_block)(vg_tid, arg[1]/*addr*/, arg[2]/*size*/);
      break;

   case VG_USERREQ__FREELIKE_BLOCK:
      if (arg[1] && ! DRD_(freelike_block)(vg_tid, arg[1]/*addr*/))
      {
         GenericErrInfo GEI = {
	    .tid = DRD_(thread_get_running_tid)(),
	    .addr = 0,
	 };
         VG_(maybe_record_error)(vg_tid,
                                 GenericErr,
                                 VG_(get_IP)(vg_tid),
                                 "Invalid VG_USERREQ__FREELIKE_BLOCK request",
                                 &GEI);
      }
      break;

   case VG_USERREQ__DRD_GET_VALGRIND_THREAD_ID:
      result = vg_tid;
      break;

   case VG_USERREQ__DRD_GET_DRD_THREAD_ID:
      result = drd_tid;
      break;

   case VG_USERREQ__DRD_SET_THREAD_NAME:
      DRD_(thread_set_name)(drd_tid, (const char*)arg[1]);
      break;

   case VG_USERREQ__DRD_START_SUPPRESSION:
      /*_VG_USERREQ__HG_ARANGE_MAKE_UNTRACKED*/
   case VG_USERREQ_TOOL_BASE('H','G') + 256 + 39:
      DRD_(start_suppression)(arg[1], arg[1] + arg[2], "client");
      break;

   case VG_USERREQ__DRD_FINISH_SUPPRESSION:
      /*_VG_USERREQ__HG_ARANGE_MAKE_TRACKED*/
   case VG_USERREQ_TOOL_BASE('H','G') + 256 + 40:
      DRD_(finish_suppression)(arg[1], arg[1] + arg[2]);
      break;

   case VG_USERREQ__DRD_ANNOTATE_HAPPENS_BEFORE:
      DRD_(hb_happens_before)(drd_tid, arg[1]);
      break;

   case VG_USERREQ__DRD_ANNOTATE_HAPPENS_AFTER:
      DRD_(hb_happens_after)(drd_tid, arg[1]);
      break;

   case VG_USERREQ__DRD_ANNOTATE_RWLOCK_CREATE:
      if (arg[1])
      {
         struct mutex_info* const mutex_p = DRD_(mutex_get)(arg[1]);
         if (mutex_p && mutex_p->mutex_type == mutex_type_spinlock)
            break;
      }
      DRD_(rwlock_pre_init)(arg[1], user_rwlock);
      break;

   case VG_USERREQ__DRD_ANNOTATE_RWLOCK_DESTROY:
      if (arg[1])
      {
         struct mutex_info* const mutex_p = DRD_(mutex_get)(arg[1]);
         if (mutex_p && mutex_p->mutex_type == mutex_type_spinlock)
            break;
      }
      DRD_(rwlock_post_destroy)(arg[1], user_rwlock);
      break;

   case VG_USERREQ__DRD_ANNOTATE_RWLOCK_ACQUIRED:
      if (arg[1])
      {
         struct mutex_info* const mutex_p = DRD_(mutex_get)(arg[1]);
         if (mutex_p && mutex_p->mutex_type == mutex_type_spinlock)
            break;
      }
      tl_assert(arg[2] == !! arg[2]);
      if (arg[2])
      {
         DRD_(rwlock_pre_wrlock)(arg[1], user_rwlock);
         DRD_(rwlock_post_wrlock)(arg[1], user_rwlock, True);
      }
      else
      {
         DRD_(rwlock_pre_rdlock)(arg[1], user_rwlock);
         DRD_(rwlock_post_rdlock)(arg[1], user_rwlock, True);
      }
      break;

   case VG_USERREQ__DRD_ANNOTATE_RWLOCK_RELEASED:
      if (arg[1])
      {
         struct mutex_info* const mutex_p = DRD_(mutex_get)(arg[1]);
         if (mutex_p && mutex_p->mutex_type == mutex_type_spinlock)
            break;
      }
      tl_assert(arg[2] == !! arg[2]);
      DRD_(rwlock_pre_unlock)(arg[1], user_rwlock);
      break;

   case VG_USERREQ__SET_PTHREAD_COND_INITIALIZER:
      DRD_(pthread_cond_initializer) = (Addr)arg[1];
      DRD_(pthread_cond_initializer_size) = arg[2];
      break;

   case VG_USERREQ__DRD_START_NEW_SEGMENT:
      DRD_(thread_new_segment)(DRD_(PtThreadIdToDrdThreadId)(arg[1]));
      break;

   case VG_USERREQ__DRD_START_TRACE_ADDR:
      DRD_(start_tracing_address_range)(arg[1], arg[1] + arg[2]);
      break;

   case VG_USERREQ__DRD_STOP_TRACE_ADDR:
      DRD_(stop_tracing_address_range)(arg[1], arg[1] + arg[2]);
      break;

   case VG_USERREQ__DRD_RECORD_LOADS:
      DRD_(thread_set_record_loads)(drd_tid, arg[1]);
      break;

   case VG_USERREQ__DRD_RECORD_STORES:
      DRD_(thread_set_record_stores)(drd_tid, arg[1]);
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

   case VG_USERREQ__ENTERING_PTHREAD_CREATE:
      DRD_(thread_entering_pthread_create)(drd_tid);
      break;

   case VG_USERREQ__LEFT_PTHREAD_CREATE:
      DRD_(thread_left_pthread_create)(drd_tid);
      break;

   case VG_USERREQ__POST_THREAD_JOIN:
   {
      const DrdThreadId thread_to_join = DRD_(PtThreadIdToDrdThreadId)(arg[1]);
      if (thread_to_join == DRD_INVALID_THREADID)
      {
         InvalidThreadIdInfo ITI = { DRD_(thread_get_running_tid)(), arg[1] };
         VG_(maybe_record_error)(vg_tid,
                                 InvalidThreadId,
                                 VG_(get_IP)(vg_tid),
                                 "pthread_join(): invalid thread ID",
                                 &ITI);
      }
      else
      {
         DRD_(thread_post_join)(drd_tid, thread_to_join);
      }
      break;
   }

   case VG_USERREQ__PRE_THREAD_CANCEL:
   {
      const DrdThreadId thread_to_cancel =DRD_(PtThreadIdToDrdThreadId)(arg[1]);
      if (thread_to_cancel == DRD_INVALID_THREADID)
      {
         InvalidThreadIdInfo ITI = { DRD_(thread_get_running_tid)(), arg[1] };
         VG_(maybe_record_error)(vg_tid,
                                 InvalidThreadId,
                                 VG_(get_IP)(vg_tid),
                                 "pthread_cancel(): invalid thread ID",
                                 &ITI);
      }
      else
      {
         DRD_(thread_pre_cancel)(thread_to_cancel);
      }
      break;
   }

   case VG_USERREQ__POST_THREAD_CANCEL:
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

   case VG_USERREQ__PRE_SEM_OPEN:
      DRD_(thread_enter_synchr)(drd_tid);
      break;

   case VG_USERREQ__POST_SEM_OPEN:
      if (DRD_(thread_leave_synchr)(drd_tid) == 0)
         DRD_(semaphore_open)(arg[1], (Char*)arg[2], arg[3], arg[4], arg[5]);
      break;

   case VG_USERREQ__PRE_SEM_CLOSE:
      if (DRD_(thread_enter_synchr)(drd_tid) == 0)
         DRD_(semaphore_close)(arg[1]);
      break;

   case VG_USERREQ__POST_SEM_CLOSE:
      DRD_(thread_leave_synchr)(drd_tid);
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
      DRD_(rwlock_pre_init)(arg[1], pthread_rwlock);
      break;

   case VG_USERREQ__POST_RWLOCK_DESTROY:
      DRD_(rwlock_post_destroy)(arg[1], pthread_rwlock);
      break;

   case VG_USERREQ__PRE_RWLOCK_RDLOCK:
      if (DRD_(thread_enter_synchr)(drd_tid) == 0)
         DRD_(rwlock_pre_rdlock)(arg[1], pthread_rwlock);
      break;

   case VG_USERREQ__POST_RWLOCK_RDLOCK:
      if (DRD_(thread_leave_synchr)(drd_tid) == 0)
         DRD_(rwlock_post_rdlock)(arg[1], pthread_rwlock, arg[2]);
      break;

   case VG_USERREQ__PRE_RWLOCK_WRLOCK:
      if (DRD_(thread_enter_synchr)(drd_tid) == 0)
         DRD_(rwlock_pre_wrlock)(arg[1], pthread_rwlock);
      break;

   case VG_USERREQ__POST_RWLOCK_WRLOCK:
      if (DRD_(thread_leave_synchr)(drd_tid) == 0)
         DRD_(rwlock_post_wrlock)(arg[1], pthread_rwlock, arg[2]);
      break;

   case VG_USERREQ__PRE_RWLOCK_UNLOCK:
      if (DRD_(thread_enter_synchr)(drd_tid) == 0)
         DRD_(rwlock_pre_unlock)(arg[1], pthread_rwlock);
      break;

   case VG_USERREQ__POST_RWLOCK_UNLOCK:
      DRD_(thread_leave_synchr)(drd_tid);
      break;

   case VG_USERREQ__DRD_CLEAN_MEMORY:
      if (arg[2] > 0)
         DRD_(clean_memory)(arg[1], arg[2]);
      break;

   case VG_USERREQ__HELGRIND_ANNOTATION_UNIMP:
      {
         /* Note: it is assumed below that the text arg[1] points to is never
          * freed, e.g. because it points to static data.
          */
         UnimpClReqInfo UICR =
            { DRD_(thread_get_running_tid)(), (Char*)arg[1] };
         VG_(maybe_record_error)(vg_tid,
                                 UnimpHgClReq,
                                 VG_(get_IP)(vg_tid),
                                 "",
                                 &UICR);
      }
      break;

   case VG_USERREQ__DRD_ANNOTATION_UNIMP:
      {
         /* Note: it is assumed below that the text arg[1] points to is never
          * freed, e.g. because it points to static data.
          */
         UnimpClReqInfo UICR =
            { DRD_(thread_get_running_tid)(), (Char*)arg[1] };
         VG_(maybe_record_error)(vg_tid,
                                 UnimpDrdClReq,
                                 VG_(get_IP)(vg_tid),
                                 "",
                                 &UICR);
      }
      break;

   default:
#if 0
      VG_(message)(Vg_DebugMsg, "Unrecognized client request 0x%lx 0x%lx",
                   arg[0], arg[1]);
      tl_assert(0);
#endif
      return False;
   }

   *ret = result;
   return True;
}
