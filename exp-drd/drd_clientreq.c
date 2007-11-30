#include "drd_clientreq.h"
#include "drd_cond.h"
#include "drd_mutex.h"
#include "drd_suppression.h"      // drd_start_suppression()
#include "drd_thread.h"
#include "drd_track.h"
#include "priv_drd_clientreq.h"
#include "pub_core_tooliface.h"   // VG_TRACK()
#include "pub_tool_basics.h"      // Bool
#include "pub_tool_libcassert.h"
#include "pub_tool_libcassert.h"  // tl_assert()
#include "pub_tool_libcprint.h"   // VG_(message)()
#include "pub_tool_machine.h"     // VG_(get_SP)()
#include "pub_tool_threadstate.h"
#include "pub_tool_tooliface.h"   // VG_(needs_...)()


static void drd_spin_init_or_unlock(const Addr spinlock, const SizeT size)
{
   struct mutex_info* mutex_p = mutex_get(spinlock);
   if (mutex_p)
   {
      mutex_unlock(spinlock, mutex_type_spinlock);
   }
   else
   {
      mutex_init(spinlock, size, mutex_type_spinlock);
   }
}

static void drd_pre_cond_wait(const Addr cond, const SizeT cond_size,
                              const Addr mutex)
{
   mutex_unlock(mutex, mutex_type_mutex);
   cond_pre_wait(cond, cond_size, mutex);
}

static void drd_post_cond_wait(const Addr cond, const Addr mutex,
                               const SizeT size)
{
   cond_post_wait(cond);
   mutex_lock(mutex, size, mutex_type_mutex);
}

static void drd_pre_cond_signal(const Addr cond)
{
   cond_pre_signal(cond);
}

static void drd_pre_cond_broadcast(const Addr cond)
{
   cond_pre_broadcast(cond);
}

static Bool drd_handle_client_request(ThreadId tid, UWord* arg, UWord* ret)
{
   UWord result = 0;

   switch (arg[0])
   {
   case VG_USERREQ__GET_THREAD_SELF:
      result = tid;
      break;

   case VG_USERREQ__SET_THREAD_NAME:
      thread_set_name_fmt(VgThreadIdToDrdThreadId(VG_(get_running_tid)()),
                          (char*)arg[1], arg[2]);
      break;

   case VG_USERREQ__DRD_START_SUPPRESSION:
      drd_start_suppression(arg[1], arg[1] + arg[2], "client");
      break;

   case VG_USERREQ__DRD_FINISH_SUPPRESSION:
      drd_finish_suppression(arg[1], arg[1] + arg[2]);
      break;

   case VG_USERREQ__DRD_SUPPRESS_CURRENT_STACK:
      thread_set_stack_startup(thread_get_running_tid(),
                               VG_(get_SP)(VG_(get_running_tid)()));
      break;

   case VG_USERREQ__DRD_START_NEW_SEGMENT:
      thread_new_segment(PtThreadIdToDrdThreadId(arg[1]));
      break;

   case VG_USERREQ__DRD_START_RECORDING:
      thread_start_recording(PtThreadIdToDrdThreadId(arg[1]));
      break;

   case VG_USERREQ__DRD_STOP_RECORDING:
      thread_stop_recording(PtThreadIdToDrdThreadId(arg[1]));
      break;

   case VG_USERREQ__SET_PTHREADID:
      thread_set_pthreadid(thread_get_running_tid(), arg[1]);
      break;

   case VG_USERREQ__SET_JOINABLE:
      thread_set_joinable(PtThreadIdToDrdThreadId(arg[1]), (Bool)arg[2]);
      break;

   case VG_USERREQ__POST_THREAD_JOIN:
      tl_assert(arg[1]);
      drd_post_thread_join(thread_get_running_tid(),
                           PtThreadIdToDrdThreadId(arg[1]));
      break;

   case VG_USERREQ__PRE_MUTEX_INIT:
      drd_pre_mutex_init(arg[1], arg[2], arg[3]);
      break;

   case VG_USERREQ__POST_MUTEX_DESTROY:
      drd_post_mutex_destroy(arg[1], arg[2]);
      break;

   case VG_USERREQ__PRE_PTHREAD_MUTEX_LOCK:
      drd_pre_mutex_lock(thread_get_running_tid(), arg[1], arg[2], arg[3]);
      break;

   case VG_USERREQ__POST_PTHREAD_MUTEX_LOCK:
      drd_post_mutex_lock(thread_get_running_tid(), arg[1], arg[2], arg[3]);
      break;

   case VG_USERREQ__PRE_PTHREAD_MUTEX_UNLOCK:
      drd_pre_mutex_unlock(thread_get_running_tid(), arg[1], arg[3]);
      break;

   case VG_USERREQ__SPIN_INIT_OR_UNLOCK:
      drd_spin_init_or_unlock(arg[1], arg[2]);
      break;

   case VG_USERREQ__POST_PTHREAD_COND_INIT:
      drd_post_cond_init(arg[1], arg[2]);
      break;

   case VG_USERREQ__PRE_PTHREAD_COND_DESTROY:
      drd_pre_cond_destroy(arg[1]);
      break;

   case VG_USERREQ__PRE_PTHREAD_COND_WAIT:
      drd_pre_cond_wait(arg[1]/*cond*/, arg[2]/*cond_size*/, arg[3]/*mutex*/);
      break;

   case VG_USERREQ__POST_PTHREAD_COND_WAIT:
      drd_post_cond_wait(arg[1]/*cond*/, arg[3]/*mutex*/,
                         arg[4]/*mutex_size*/);
      break;

   case VG_USERREQ__PRE_PTHREAD_COND_SIGNAL:
      drd_pre_cond_signal(arg[1]);
      break;

   case VG_USERREQ__PRE_PTHREAD_COND_BROADCAST:
      drd_pre_cond_broadcast(arg[1]);
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
