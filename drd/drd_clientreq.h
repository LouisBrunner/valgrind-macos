/*
  This file is part of drd, a thread error detector.

  Copyright (C) 2006-2013 Bart Van Assche <bvanassche@acm.org>.

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


/*
 * This header file contains the tool-internal interface for the code that
 * processes client requests.
 */


#ifndef __DRD_CLIENTREQ_H
#define __DRD_CLIENTREQ_H


#include "drd.h"
#include "drd_basics.h" /* DRD_() */


/*
 * While the client requests defined in the header file "drd.h" define a
 * public interface between client programs and the DRD tool, the client
 * requests defined below are a tool-internal interface. These last client
 * requests must only be used by the source code in the various *_intercepts.c
 * source files.
 */
enum {
   /* Declare the address and size of a variable with value
    * PTHREAD_COND_INITIALIZER.
    */
   VG_USERREQ__SET_PTHREAD_COND_INITIALIZER = VG_USERREQ_TOOL_BASE('D', 'r'),
   /* args: address, size. */

   /* To ask the drd tool to start a new segment in the specified thread. */
   VG_USERREQ__DRD_START_NEW_SEGMENT,
   /* args: POSIX thread ID. */

   /* Tell drd the pthread_t of the running thread. */
   VG_USERREQ__SET_PTHREADID,
   /* args: pthread_t. */
   /* Ask drd that a the thread's state transition from */
   /* VgTs_Zombie to VgTs_Empty is delayed until */
   /* VG_USERREQ__POST_THREAD_JOIN is performed. */
   VG_USERREQ__SET_JOINABLE,
   /* args: pthread_t, Bool */

   /* Tell DRD that the calling thread is about to enter pthread_create(). */
   VG_USERREQ__ENTERING_PTHREAD_CREATE,
   /* args: (none) */
   /* Tell DRD that the calling thread has left pthread_create(). */
   VG_USERREQ__LEFT_PTHREAD_CREATE,
   /* args: (none) */

   /* To notify drd that a thread finished because */
   /* pthread_thread_join() was called on it. */
   VG_USERREQ__POST_THREAD_JOIN,
   /* args: pthread_t (joinee) */

   /* To notify drd before a pthread_cancel call. */
   VG_USERREQ__PRE_THREAD_CANCEL,
   /* args: pthread_t */
   /* To notify drd after a pthread_cancel call. */
   VG_USERREQ__POST_THREAD_CANCEL,
   /* args: pthread_t, Bool */

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
   /* to notify the drd tool of a pthread_spin_init/pthread_spin_unlock call */
   VG_USERREQ__PRE_SPIN_INIT_OR_UNLOCK,
   /* args: Addr */
   /* to notify the drd tool of a pthread_spin_init/pthread_spin_unlock call */
   VG_USERREQ__POST_SPIN_INIT_OR_UNLOCK,
   /* args: Addr */


   /* to notify the drd tool of a pthread_cond_init call. */
   VG_USERREQ__PRE_COND_INIT,
   /* args: Addr */
   /* to notify the drd tool of a pthread_cond_init call. */
   VG_USERREQ__POST_COND_INIT,
   /* args: Addr */
   /* to notify the drd tool of a pthread_cond_destroy call. */
   VG_USERREQ__PRE_COND_DESTROY,
   /* args: Addr */
   /* to notify the drd tool of a pthread_cond_destroy call. */
   VG_USERREQ__POST_COND_DESTROY,
   /* args: Addr cond, Bool destroy_succeeded */
   VG_USERREQ__PRE_COND_WAIT,
   /* args: Addr cond, Addr mutex, MutexT mt */
   VG_USERREQ__POST_COND_WAIT,
   /* args: Addr cond, Addr mutex, Bool took_lock*/
   VG_USERREQ__PRE_COND_SIGNAL,
   /* args: Addr cond */
   VG_USERREQ__POST_COND_SIGNAL,
   /* args: Addr cond */
   VG_USERREQ__PRE_COND_BROADCAST,
   /* args: Addr cond */
   VG_USERREQ__POST_COND_BROADCAST,
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
   /* To notify the drd tool of a sem_open call. */
   VG_USERREQ__PRE_SEM_OPEN,
   /* args: Addr name, Word oflag, Word mode, Word value */
   /* To notify the drd tool of a sem_open call. */
   VG_USERREQ__POST_SEM_OPEN,
   /* args: Addr sem, Word oflag, Word mode, Word value */
   /* To notify the drd tool of a sem_close call. */
   VG_USERREQ__PRE_SEM_CLOSE,
   /* args: Addr sem */
   /* To notify the drd tool of a sem_close call. */
   VG_USERREQ__POST_SEM_CLOSE,
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
   /* args: Addr barrier, BarrierT type, Word has_waited, Word serializing */

   /* To notify the drd tool of a pthread_rwlock_init call. */
   VG_USERREQ__PRE_RWLOCK_INIT,
   /* args: Addr rwlock */
   /* To notify the drd tool of a pthread_rwlock_init call. */
   VG_USERREQ__POST_RWLOCK_INIT,
   /* args: Addr rwlock */
   /* To notify the drd tool of a pthread_rwlock_destroy call. */
   VG_USERREQ__PRE_RWLOCK_DESTROY,
   /* args: Addr rwlock, RwLockT */
   /* To notify the drd tool of a pthread_rwlock_destroy call. */
   VG_USERREQ__POST_RWLOCK_DESTROY,
   /* args: Addr rwlock, RwLockT */
   /* To notify the drd tool of a pthread_rwlock_rdlock call. */
   VG_USERREQ__PRE_RWLOCK_RDLOCK,
   /* args: Addr rwlock, RwLockT */
   /* To notify the drd tool of a pthread_rwlock_rdlock call. */
   VG_USERREQ__POST_RWLOCK_RDLOCK,
   /* args: Addr rwlock, RwLockT, Bool took_lock */
   /* To notify the drd tool of a pthread_rwlock_wrlock call. */
   VG_USERREQ__PRE_RWLOCK_WRLOCK,
   /* args: Addr rwlock, RwLockT */
   /* To notify the drd tool of a pthread_rwlock_wrlock call. */
   VG_USERREQ__POST_RWLOCK_WRLOCK,
   /* args: Addr rwlock, RwLockT, Bool took_lock */
   /* To notify the drd tool of a pthread_rwlock_unlock call. */
   VG_USERREQ__PRE_RWLOCK_UNLOCK,
   /* args: Addr rwlock, RwLockT */
   /* To notify the drd tool of a pthread_rwlock_unlock call. */
   VG_USERREQ__POST_RWLOCK_UNLOCK
   /* args: Addr rwlock, RwLockT, Bool unlocked */

};

/**
 * Error checking on POSIX recursive mutexes, POSIX error checking mutexes,
 * POSIX default mutexes and POSIX spinlocks happens the code in drd_mutex.c.
 * The values defined below specify the mutex type.
 */
typedef enum {
   mutex_type_unknown          = -1,
   mutex_type_invalid_mutex    = 0,
   mutex_type_recursive_mutex  = 1,
   mutex_type_errorcheck_mutex = 2,
   mutex_type_default_mutex    = 3,
   mutex_type_spinlock         = 4,
} MutexT;

/**
 * Error checking on POSIX reader/writer locks and user-defined reader/writer
 * locks happens by the code in drd_rwlock.c. The values defined below specify
 * the rwlock type.
 */
typedef enum {
   pthread_rwlock = 1,
   user_rwlock    = 2,
} RwLockT;

/*
 * Error checking on POSIX barriers and GOMP barriers happens by the same
 * code. The integer values defined below specify the type of a barrier with
 * a given client address.
 */
typedef enum {
   pthread_barrier = 1,
   gomp_barrier    = 2,
} BarrierT;


extern Bool DRD_(g_free_is_write);

void DRD_(clientreq_init)(void);


#endif //  __DRD_CLIENTREQ_H
