#ifndef __DRD_CLIENTREQ_H
#define __DRD_CLIENTREQ_H


#include "valgrind.h" // VG_USERREQ_TOOL_BASE()


enum {
  /* Ask the core the thread ID assigned by Valgrind. */
  VG_USERREQ__GET_THREAD_SELF = VG_USERREQ_TOOL_BASE('D', 'R'),
  /* args: none. */

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
  /* To ask the drd tool to trace all accesses to the specified address. */
  VG_USERREQ__DRD_TRACE_ADDR,
  /* args: Addr. */

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

  /* to notify the drd tool of a pthread_mutex_init call. */
  VG_USERREQ__PRE_MUTEX_INIT,
  /* args: Addr, MutexT */
  /* to notify the drd tool of a pthread_mutex_init call. */
  VG_USERREQ__POST_MUTEX_INIT,
  /* args: Addr */
  /* to notify the drd tool of a pthread_mutex_destroy call. */
  VG_USERREQ__PRE_MUTEX_DESTROY,
  /* args: Addr */
  /* to notify the drd tool of a pthread_mutex_destroy call. */
  VG_USERREQ__POST_MUTEX_DESTROY,
  /* args: Addr, MutexT */
  /* to notify the drd tool of pthread_mutex_lock calls */
  VG_USERREQ__PRE_MUTEX_LOCK,
  /* args: Addr, MutexT, Bool */
  /* to notify the drd tool of pthread_mutex_lock calls */
  VG_USERREQ__POST_MUTEX_LOCK,
  /* args: Addr, Bool */
  /* to notify the drd tool of pthread_mutex_unlock calls */
  VG_USERREQ__PRE_MUTEX_UNLOCK,
  /* args: Addr */
  /* to notify the drd tool of pthread_mutex_unlock calls */
  VG_USERREQ__POST_MUTEX_UNLOCK,
  /* args: Addr */
  VG_USERREQ__SPIN_INIT_OR_UNLOCK,
  /* args: Addr spinlock */


  /* to notify the drd tool of a pthread_cond_init call. */
  VG_USERREQ__PRE_COND_INIT,
  /* args: Addr */
  /* to notify the drd tool of a pthread_cond_destroy call. */
  VG_USERREQ__POST_COND_DESTROY,
  /* args: Addr cond */
  VG_USERREQ__PRE_COND_WAIT,
  /* args: Addr cond, Addr mutex, MutexT mt */
  VG_USERREQ__POST_COND_WAIT,
  /* args: Addr cond, Addr mutex, Bool took_lock*/
  VG_USERREQ__PRE_COND_SIGNAL,
  /* args: Addr cond */
  VG_USERREQ__PRE_COND_BROADCAST,
  /* args: Addr cond */

  /* To notify the drd tool of a sem_init call. */
  VG_USERREQ__PRE_SEM_INIT,
  /* args: Addr sem, Word pshared, Word value */
  /* To notify the drd tool of a sem_init call. */
  VG_USERREQ__POST_SEM_INIT,
  /* args: Addr sem */
  /* To notify the drd tool of a sem_destroy call. */
  VG_USERREQ__PRE_SEM_DESTROY,
  /* args: Addr sem */
  /* To notify the drd tool of a sem_destroy call. */
  VG_USERREQ__POST_SEM_DESTROY,
  /* args: Addr sem */
  /* To notify the drd tool of a sem_wait call. */
  VG_USERREQ__PRE_SEM_WAIT,
  /* args: Addr sem */
  /* To notify the drd tool of a sem_wait call. */
  VG_USERREQ__POST_SEM_WAIT,
  /* args: Addr sem, Bool waited */
  /* To notify the drd tool before a sem_post call. */
  VG_USERREQ__PRE_SEM_POST,
  /* args: Addr sem */
  /* To notify the drd tool after a sem_post call. */
  VG_USERREQ__POST_SEM_POST,
  /* args: Addr sem, Bool waited */

  /* To notify the drd tool of a pthread_barrier_init call. */
  VG_USERREQ__PRE_BARRIER_INIT,
  /* args: Addr barrier, BarrierT type, Word count, Bool reinit */
  /* To notify the drd tool of a pthread_barrier_init call. */
  VG_USERREQ__POST_BARRIER_INIT,
  /* args: Addr barrier, BarrierT type */
  /* To notify the drd tool of a pthread_barrier_destroy call. */
  VG_USERREQ__PRE_BARRIER_DESTROY,
  /* args: Addr barrier, BarrierT type. */
  /* To notify the drd tool of a pthread_barrier_destroy call. */
  VG_USERREQ__POST_BARRIER_DESTROY,
  /* args: Addr barrier, BarrierT type. */
  /* To notify the drd tool of a pthread_barrier_wait call. */
  VG_USERREQ__PRE_BARRIER_WAIT,
  /* args: Addr barrier, BarrierT type. */
  /* To notify the drd tool of a pthread_barrier_wait call. */
  VG_USERREQ__POST_BARRIER_WAIT,
  /* args: Addr barrier, BarrierT type, Word has_waited */

  /* To notify the drd tool of a pthread_rwlock_init call. */
  VG_USERREQ__PRE_RWLOCK_INIT,
  /* args: Addr rwlock */
  /* To notify the drd tool of a pthread_rwlock_destroy call. */
  VG_USERREQ__POST_RWLOCK_DESTROY,
  /* args: Addr rwlock */
  /* To notify the drd tool of a pthread_rwlock_rdlock call. */
  VG_USERREQ__PRE_RWLOCK_RDLOCK,
  /* args: Addr rwlock */
  /* To notify the drd tool of a pthread_rwlock_rdlock call. */
  VG_USERREQ__POST_RWLOCK_RDLOCK,
  /* args: Addr rwlock, Bool took_lock */
  /* To notify the drd tool of a pthread_rwlock_wrlock call. */
  VG_USERREQ__PRE_RWLOCK_WRLOCK,
  /* args: Addr rwlock */
  /* To notify the drd tool of a pthread_rwlock_wrlock call. */
  VG_USERREQ__POST_RWLOCK_WRLOCK,
  /* args: Addr rwlock, Bool took_lock */
  /* To notify the drd tool of a pthread_rwlock_unlock call. */
  VG_USERREQ__PRE_RWLOCK_UNLOCK,
  /* args: Addr rwlock */
  /* To notify the drd tool of a pthread_rwlock_unlock call. */
  VG_USERREQ__POST_RWLOCK_UNLOCK,
  /* args: Addr rwlock, Bool unlocked */

};

typedef enum
{
   mutex_type_invalid_mutex    = 0,
   mutex_type_recursive_mutex  = 1,
   mutex_type_errorcheck_mutex = 2,
   mutex_type_default_mutex    = 3,
   mutex_type_spinlock         = 4,
} MutexT;

typedef enum
  {
    pthread_barrier = 1,
    gomp_barrier = 2,
  } BarrierT;

#endif //  __DRD_CLIENTREQ_H
