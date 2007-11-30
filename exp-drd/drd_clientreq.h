#ifndef __DRD_CLIENTREQ_H
#define __DRD_CLIENTREQ_H


#include "valgrind.h" // VG_USERREQ_TOOL_BASE()


enum {
  /* Ask the core the thread ID assigned by Valgrind. */
  VG_USERREQ__GET_THREAD_SELF = VG_USERREQ_TOOL_BASE('D', 'R'),
  /* args: none. */
  /* Set the name of the thread that performs this client request. */
  VG_USERREQ__SET_THREAD_NAME,
  /* args: null-terminated character string. */

  /* To tell the drd tool to suppress data race detection on the specified */
  /* address range. */
  VG_USERREQ__DRD_START_SUPPRESSION,
  /* args: start address, size in bytes */
  /* To tell the drd tool no longer to suppress data race detection on the */
  /* specified address range. */
  VG_USERREQ__DRD_FINISH_SUPPRESSION,
  /* args: start address, size in bytes */
  /* Ask drd to suppress data race reports on all currently allocated stack */
  /* data of the current thread.                                            */
  VG_USERREQ__DRD_SUPPRESS_CURRENT_STACK,
  /* args: none */
  /* To ask the drd tool to start a new segment in the specified thread. */
  VG_USERREQ__DRD_START_NEW_SEGMENT,
  /* args: POSIX thread ID. */

  /* To tell the drd tool to start again recording memory accesses for the */
  /* specified thread. */
  VG_USERREQ__DRD_START_RECORDING,
  /* args: POSIX thread ID. */
  /* To tell the drd tool to stop recording memory accesses for the */
  /* specified thread. */
  VG_USERREQ__DRD_STOP_RECORDING,
  /* args: POSIX thread ID. */

  /* Tell the core the pthread_t of the running thread */
  VG_USERREQ__SET_PTHREADID,
  /* args: pthread_t. */
  /* Ask the core that a the thread's state transition from */
  /* VgTs_Zombie to VgTs_Empty is delayed until */
  /* VG_USERREQ__POST_THREAD_JOIN is performed. */
  VG_USERREQ__SET_JOINABLE,
  /* args: pthread_t, Bool */

  /* To notify drd that a thread finished because */
  /* pthread_thread_join() was called on it. */
  VG_USERREQ__POST_THREAD_JOIN,
  /* args: pthread_t (joinee) */

  /* To notify the core of a pthread_mutex_init call */
  VG_USERREQ__PRE_MUTEX_INIT,
  /* args: Addr, MutexT */
  /* To notify the core of a pthread_mutex_destroy call */
  VG_USERREQ__POST_MUTEX_DESTROY,
  /* args: Addr, SizeT, MutexT */
  /* To notify the core of pthread_mutex_lock calls */
  VG_USERREQ__PRE_PTHREAD_MUTEX_LOCK,
  /* args: Addr, SizeT, MutexT */
  /* To notify the core of pthread_mutex_lock calls */
  VG_USERREQ__POST_PTHREAD_MUTEX_LOCK,
  /* args: Addr, SizeT, MutexT */
  /* To notify the core of pthread_mutex_unlock calls */
  VG_USERREQ__PRE_PTHREAD_MUTEX_UNLOCK,
  /* args: Addr */
  VG_USERREQ__SPIN_INIT_OR_UNLOCK,
  /* args: Addr spinlock, SizeT size */


  /* To notify the core of a pthread_cond_init call */
  VG_USERREQ__POST_PTHREAD_COND_INIT,
  /* args: Addr */
  /* To notify the core of a pthread_cond_destroy call */
  VG_USERREQ__PRE_PTHREAD_COND_DESTROY,
  /* args: Addr cond, SizeT cond_size, Addr mutex, SizeT mutex_size */
  VG_USERREQ__PRE_PTHREAD_COND_WAIT,
  /* args: Addr cond, SizeT cond_size, Addr mutex, SizeT mutex_size */
  VG_USERREQ__POST_PTHREAD_COND_WAIT,
  /* args: Addr cond, SizeT cond_size, Addr mutex, SizeT mutex_size */
  VG_USERREQ__PRE_PTHREAD_COND_SIGNAL,
  /* args: Addr cond */
  VG_USERREQ__PRE_PTHREAD_COND_BROADCAST,
  /* args: Addr cond */

};

typedef enum
{
   mutex_type_mutex = 1,
   mutex_type_spinlock = 2,
} MutexT;


#endif //  __DRD_CLIENTREQ_H
