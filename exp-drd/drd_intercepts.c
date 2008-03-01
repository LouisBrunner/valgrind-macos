
/*--------------------------------------------------------------------*/
/*--- Client-space code for drd.                  drd_intercepts.c ---*/
/*--------------------------------------------------------------------*/

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

/* ---------------------------------------------------------------------
   ALL THE CODE IN THIS FILE RUNS ON THE SIMULATED CPU. 

   These functions are not called directly - they're the targets of code
   redirection or load notifications (see pub_core_redir.h for info).
   They're named weirdly so that the intercept code can find them when the
   shared object is initially loaded.

   Note that this filename has the "drd_" prefix because it can appear
   in stack traces, and the "drd_" makes it a little clearer that it
   originates from Valgrind.
   ------------------------------------------------------------------ */

// Make sure pthread_spinlock_t is available when compiling with older glibc
// versions (2.3 or before).
#ifndef _GNU_SOURCE
#define _GNU_SOURCE
#endif

#include <assert.h>
#include <inttypes.h> // uintptr_t
#include <pthread.h>
#include <semaphore.h>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include "drd_clientreq.h"
#include "pub_tool_redir.h"


// Defines.

#define PTH_FUNC(ret_ty, f, args...) \
   ret_ty VG_WRAP_FUNCTION_ZZ(libpthreadZdsoZd0,f)(args); \
   ret_ty VG_WRAP_FUNCTION_ZZ(libpthreadZdsoZd0,f)(args)


// Local data structures.

typedef struct
{
   void* (*start)(void*);
   void* arg;
   int   detachstate;
#if 0
   pthread_mutex_t mutex;
   pthread_cond_t  cond;
#else
   int wrapper_started;
#endif
} VgPosixThreadArgs;


// Local variables.

static int vg_main_thread_state_is_set = 0;
static pid_t vg_main_thread_pid;


// Function definitions.

static MutexT pthread_to_drd_mutex_type(const int kind)
{
   switch (kind)
   {
   /* PTHREAD_MUTEX_RECURSIVE_NP */
   case PTHREAD_MUTEX_RECURSIVE:
      return mutex_type_recursive_mutex;
   /* PTHREAD_MUTEX_ERRORCHECK_NP */
   case PTHREAD_MUTEX_ERRORCHECK:
      return mutex_type_errorcheck_mutex;
   /* PTHREAD_MUTEX_TIMED_NP */
   /* PTHREAD_MUTEX_NORMAL */
   case PTHREAD_MUTEX_DEFAULT:
   case PTHREAD_MUTEX_ADAPTIVE_NP:
      return mutex_type_default_mutex;
   }
   return mutex_type_invalid_mutex;
}

static MutexT mutex_type(pthread_mutex_t* mutex)
{
#if defined(_PTHREAD_DESCR_DEFINED)
   // Linuxthreads.
   const int kind = mutex->__m_kind;
#elif defined(__SIZEOF_PTHREAD_MUTEX_T)
   // NPTL.
   const int kind = mutex->__data.__kind;
#else
   // Another POSIX threads implementation. Regression tests will fail.
   const int kind = PTHREAD_MUTEX_DEFAULT;
#endif
   return pthread_to_drd_mutex_type(kind);
}

static void vg_start_suppression(const void* const p, size_t const size)
{
   int res;
   VALGRIND_DO_CLIENT_REQUEST(res, 0, VG_USERREQ__DRD_START_SUPPRESSION,
                              p, (char*)p + size, 0, 0, 0);
}

static void vg_set_joinable(const pthread_t tid, const int joinable)
{
   int res;
   assert(joinable == 0 || joinable == 1);
   VALGRIND_DO_CLIENT_REQUEST(res, 0, VG_USERREQ__SET_JOINABLE,
                              tid, joinable, 0, 0, 0);
}

static void* vg_thread_wrapper(void* arg)
{
   int res;

   if (getpid() != vg_main_thread_pid)
   {
      fprintf(stderr,
	      "Detected the linuxthreads threading library.\n"
	      "Sorry, but DRD does not support linuxthreads.\n"
	      "Please try to run DRD on a system with NPTL instead.\n"
	      "Giving up.\n");
      abort();
   }

   VALGRIND_DO_CLIENT_REQUEST(res, 0, VG_USERREQ__DRD_SUPPRESS_CURRENT_STACK,
                              0, 0, 0, 0, 0);

   {
      VgPosixThreadArgs* const arg_ptr = (VgPosixThreadArgs*)arg;
      VgPosixThreadArgs const arg_copy = *arg_ptr;
      void* result;

#if 0
      pthread_mutex_lock(arg_ptr->mutex);
      pthread_cond_signal(arg_ptr->cond);
      pthread_mutex_unlock(arg_ptr->mutex);
#else
      arg_ptr->wrapper_started = 1;
#endif

      VALGRIND_DO_CLIENT_REQUEST(res, -1, VG_USERREQ__SET_PTHREADID,
                                 pthread_self(), 0, 0, 0, 0);
      vg_set_joinable(pthread_self(),
                      arg_copy.detachstate == PTHREAD_CREATE_JOINABLE);
      result = (arg_copy.start)(arg_copy.arg);
      return result;
   }
}

static void vg_set_main_thread_state(void)
{
   int res;

   vg_main_thread_pid = getpid();

   VALGRIND_DO_CLIENT_REQUEST(res, -1, VG_USERREQ__DRD_SUPPRESS_CURRENT_STACK,
                              0, 0, 0, 0, 0);

   // Make sure that DRD knows about the main thread's POSIX thread ID.
   VALGRIND_DO_CLIENT_REQUEST(res, -1, VG_USERREQ__SET_PTHREADID,
                              pthread_self(), 0, 0, 0, 0);

}

// pthread_create
PTH_FUNC(int, pthreadZucreateZa, // pthread_create*
              pthread_t *thread, const pthread_attr_t *attr,
              void *(*start) (void *), void *arg)
{
   int    ret;
   OrigFn fn;
   VgPosixThreadArgs vgargs;

   VALGRIND_GET_ORIG_FN(fn);

   if (vg_main_thread_state_is_set == 0)
   {
      vg_set_main_thread_state();
      vg_main_thread_state_is_set = 1;
   }
   vg_start_suppression(&vgargs.wrapper_started,
                        sizeof(vgargs.wrapper_started));
   vgargs.start = start;
   vgargs.arg   = arg;
   vgargs.wrapper_started = 0;
   vgargs.detachstate = PTHREAD_CREATE_JOINABLE;
   if (attr)
   {
      if (pthread_attr_getdetachstate(attr, &vgargs.detachstate) != 0)
      {
         assert(0);
      }
   }
   assert(vgargs.detachstate == PTHREAD_CREATE_JOINABLE
          || vgargs.detachstate == PTHREAD_CREATE_DETACHED);
#if 0
   pthread_mutex_init(&vgargs.mutex, 0);
   pthread_cond_init(&vgargs.cond, 0);
   pthread_mutex_lock(&vgargs.mutex);
#endif
   CALL_FN_W_WWWW(ret, fn, thread, attr, vg_thread_wrapper, &vgargs);
#if 0
   pthread_cond_wait(&vgargs.cond, &vgargs.mutex);
   pthread_mutex_unlock(&vgargs.mutex);
   pthread_cond_destroy(&vgargs.cond);
   pthread_mutex_destroy(&vgargs.mutex);
#else
   // Yes, you see it correctly, busy waiting ... The problem is that
   // POSIX threads functions cannot be called here -- the functions defined
   // in this file (drd_intercepts.c) would be called instead of those in
   // libpthread.so. This loop is necessary because vgargs is allocated on the
   // stack, and the created thread reads it.
   if (ret == 0)
   {
      while (! vgargs.wrapper_started)
      {
         sched_yield();
      }
   }
#endif
   return ret;
}

// pthread_join
PTH_FUNC(int, pthreadZujoin, // pthread_join
              pthread_t pt_joinee, void **thread_return)
{
   int      ret;
   int      res;
   OrigFn   fn;

   VALGRIND_GET_ORIG_FN(fn);
   CALL_FN_W_WW(ret, fn, pt_joinee, thread_return);
   if (ret == 0)
   {
      VALGRIND_DO_CLIENT_REQUEST(res, -1, VG_USERREQ__POST_THREAD_JOIN,
                                 pt_joinee, 0, 0, 0, 0);
   }
   return ret;
}

// pthread_detach
PTH_FUNC(int, pthreadZudetach, pthread_t pt_thread)
{
   int ret;
   OrigFn fn;
   VALGRIND_GET_ORIG_FN(fn);
   {
      CALL_FN_W_W(ret, fn, pt_thread);
      if (ret == 0)
      {
         vg_set_joinable(pt_thread, 0);
      }
   }
   return ret;
}

// pthread_mutex_init
PTH_FUNC(int, pthreadZumutexZuinit,
              pthread_mutex_t *mutex,
              const pthread_mutexattr_t* attr)
{
   int ret;
   int res;
   OrigFn fn;
   int mt = PTHREAD_MUTEX_DEFAULT;
   VALGRIND_GET_ORIG_FN(fn);
   if (attr)
      pthread_mutexattr_gettype(attr, &mt);
   VALGRIND_DO_CLIENT_REQUEST(res, -1, VG_USERREQ__PRE_MUTEX_INIT,
                              mutex, sizeof(*mutex),
                              pthread_to_drd_mutex_type(mt), 0, 0);
   CALL_FN_W_WW(ret, fn, mutex, attr);
   return ret;
}

// pthread_mutex_destroy
PTH_FUNC(int, pthreadZumutexZudestroy,
              pthread_mutex_t *mutex)
{
   int ret;
   int res;
   OrigFn fn;
   VALGRIND_GET_ORIG_FN(fn);
   CALL_FN_W_W(ret, fn, mutex);
   VALGRIND_DO_CLIENT_REQUEST(res, -1, VG_USERREQ__POST_MUTEX_DESTROY,
                              mutex, mutex_type(mutex), 0, 0, 0);
   return ret;
}

// pthread_mutex_lock
PTH_FUNC(int, pthreadZumutexZulock, // pthread_mutex_lock
              pthread_mutex_t *mutex)
{
   int   ret;
   int   res;
   OrigFn fn;
   VALGRIND_GET_ORIG_FN(fn);
   VALGRIND_DO_CLIENT_REQUEST(res, 0, VG_USERREQ__PRE_PTHREAD_MUTEX_LOCK,
                              mutex, sizeof(*mutex), mutex_type(mutex), 0, 0);
#if 1
   // The only purpose of the system call below is to make drd work on AMD64
   // systems. Without this system call, clients crash (SIGSEGV) in
   // std::locale::locale().
   write(1, "", 0);
#endif
   CALL_FN_W_W(ret, fn, mutex);
   VALGRIND_DO_CLIENT_REQUEST(res, 0, VG_USERREQ__POST_PTHREAD_MUTEX_LOCK,
                              mutex, ret == 0, 0, 0, 0);
   return ret;
}

// pthread_mutex_trylock
PTH_FUNC(int, pthreadZumutexZutrylock, // pthread_mutex_trylock
              pthread_mutex_t *mutex)
{
   int   ret;
   int   res;
   OrigFn fn;
   VALGRIND_GET_ORIG_FN(fn);
   VALGRIND_DO_CLIENT_REQUEST(res, 0, VG_USERREQ__PRE_PTHREAD_MUTEX_LOCK,
                              mutex, sizeof(*mutex), mutex_type(mutex), 0, 0);
   CALL_FN_W_W(ret, fn, mutex);
   VALGRIND_DO_CLIENT_REQUEST(res, -1, VG_USERREQ__POST_PTHREAD_MUTEX_LOCK,
                              mutex, ret == 0, 0, 0, 0);
   return ret;
}

// pthread_mutex_timedlock
PTH_FUNC(int, pthreadZumutexZutimedlock, // pthread_mutex_timedlock
              pthread_mutex_t *mutex,
              const struct timespec *abs_timeout)
{
   int   ret;
   int   res;
   OrigFn fn;
   VALGRIND_GET_ORIG_FN(fn);
   VALGRIND_DO_CLIENT_REQUEST(res, 0, VG_USERREQ__PRE_PTHREAD_MUTEX_LOCK,
                              mutex, sizeof(*mutex), mutex_type(mutex), 0, 0);
   CALL_FN_W_WW(ret, fn, mutex, abs_timeout);
   VALGRIND_DO_CLIENT_REQUEST(res, -1, VG_USERREQ__POST_PTHREAD_MUTEX_LOCK,
                              mutex, ret == 0, 0, 0, 0);
   return ret;
}

// pthread_mutex_unlock
PTH_FUNC(int, pthreadZumutexZuunlock, // pthread_mutex_unlock
              pthread_mutex_t *mutex)
{
   int ret;
   int   res;
   OrigFn fn;
   VALGRIND_GET_ORIG_FN(fn);
   VALGRIND_DO_CLIENT_REQUEST(res, -1,
                              VG_USERREQ__PRE_PTHREAD_MUTEX_UNLOCK,
                              mutex, sizeof(*mutex), mutex_type(mutex), 0, 0);
   CALL_FN_W_W(ret, fn, mutex);
   return ret;
}

// pthread_cond_init
PTH_FUNC(int, pthreadZucondZuinitZa, // pthread_cond_init*
              pthread_cond_t* cond,
              const pthread_condattr_t* attr)
{
   int ret;
   int res;
   OrigFn fn;
   VALGRIND_GET_ORIG_FN(fn);
   VALGRIND_DO_CLIENT_REQUEST(res, -1, VG_USERREQ__PRE_PTHREAD_COND_INIT,
                              cond, sizeof(*cond), 0, 0, 0);
   CALL_FN_W_WW(ret, fn, cond, attr);
   return ret;
}

// pthread_cond_destroy
PTH_FUNC(int, pthreadZucondZudestroyZa, // pthread_cond_destroy*
              pthread_cond_t* cond)
{
   int ret;
   int res;
   OrigFn fn;
   VALGRIND_GET_ORIG_FN(fn);
   CALL_FN_W_W(ret, fn, cond);
   VALGRIND_DO_CLIENT_REQUEST(res, -1, VG_USERREQ__POST_PTHREAD_COND_DESTROY,
                              cond, 0, 0, 0, 0);
   return ret;
}

// pthread_cond_wait
PTH_FUNC(int, pthreadZucondZuwaitZa, // pthread_cond_wait*
              pthread_cond_t *cond,
              pthread_mutex_t *mutex)
{
   int   ret;
   int   res;
   OrigFn fn;
   VALGRIND_GET_ORIG_FN(fn);
   VALGRIND_DO_CLIENT_REQUEST(res, -1, VG_USERREQ__PRE_PTHREAD_COND_WAIT,
                              cond, sizeof(*cond), mutex, mutex_type(mutex),
                              0);
   CALL_FN_W_WW(ret, fn, cond, mutex);
   VALGRIND_DO_CLIENT_REQUEST(res, -1, VG_USERREQ__POST_PTHREAD_COND_WAIT,
                              cond, mutex, ret == 0, 0, 0);
   return ret;
}

// pthread_cond_timedwait
PTH_FUNC(int, pthreadZucondZutimedwaitZa, // pthread_cond_timedwait*
              pthread_cond_t *cond,
              pthread_mutex_t *mutex,
              const struct timespec* abstime)
{
   int   ret;
   int   res;
   OrigFn fn;
   VALGRIND_GET_ORIG_FN(fn);
   VALGRIND_DO_CLIENT_REQUEST(res, -1, VG_USERREQ__PRE_PTHREAD_COND_WAIT,
                              cond, sizeof(*cond), mutex, mutex_type(mutex),
                              0);
   CALL_FN_W_WWW(ret, fn, cond, mutex, abstime);
   VALGRIND_DO_CLIENT_REQUEST(res, -1, VG_USERREQ__POST_PTHREAD_COND_WAIT,
                              cond, mutex, ret == 0, 0, 0);
   return ret;
}

// pthread_cond_signal
PTH_FUNC(int, pthreadZucondZusignalZa, // pthread_cond_signal*
              pthread_cond_t* cond)
{
   int   ret;
   int   res;
   OrigFn fn;
   VALGRIND_GET_ORIG_FN(fn);
   VALGRIND_DO_CLIENT_REQUEST(res, -1, VG_USERREQ__PRE_PTHREAD_COND_SIGNAL,
                              cond, 0, 0, 0, 0);
   CALL_FN_W_W(ret, fn, cond);
   return ret;
}

// pthread_cond_broadcast
PTH_FUNC(int, pthreadZucondZubroadcastZa, // pthread_cond_broadcast*
              pthread_cond_t* cond)
{
   int   ret;
   int   res;
   OrigFn fn;
   VALGRIND_GET_ORIG_FN(fn);
   VALGRIND_DO_CLIENT_REQUEST(res, -1, VG_USERREQ__PRE_PTHREAD_COND_BROADCAST,
                              cond, 0, 0, 0, 0);
   CALL_FN_W_W(ret, fn, cond);
   return ret;
}


// pthread_spin_init
PTH_FUNC(int, pthreadZuspinZuinit, // pthread_spin_init
              pthread_spinlock_t *spinlock,
              int pshared)
{
   int ret;
   int res;
   OrigFn fn;
   VALGRIND_GET_ORIG_FN(fn);
   VALGRIND_DO_CLIENT_REQUEST(res, -1, VG_USERREQ__SPIN_INIT_OR_UNLOCK,
                              spinlock, sizeof(*spinlock),
                              mutex_type_spinlock, 0, 0);
   CALL_FN_W_WW(ret, fn, spinlock, pshared);
   return ret;
}

// pthread_spin_destroy
PTH_FUNC(int, pthreadZuspinZudestroy, // pthread_spin_destroy
              pthread_spinlock_t *spinlock)
{
   int ret;
   int res;
   OrigFn fn;
   VALGRIND_GET_ORIG_FN(fn);
   CALL_FN_W_W(ret, fn, spinlock);
   VALGRIND_DO_CLIENT_REQUEST(res, -1, VG_USERREQ__POST_MUTEX_DESTROY,
                              spinlock, mutex_type_spinlock, 0, 0, 0);
   return ret;
}

// pthread_spin_lock
PTH_FUNC(int, pthreadZuspinZulock, // pthread_spin_lock
              pthread_spinlock_t *spinlock)
{
   int   ret;
   int   res;
   OrigFn fn;
   VALGRIND_GET_ORIG_FN(fn);
   VALGRIND_DO_CLIENT_REQUEST(res, 0, VG_USERREQ__PRE_PTHREAD_MUTEX_LOCK,
                              spinlock, sizeof(*spinlock), mutex_type_spinlock,
                              0, 0);
   CALL_FN_W_W(ret, fn, spinlock);
   VALGRIND_DO_CLIENT_REQUEST(res, -1, VG_USERREQ__POST_PTHREAD_MUTEX_LOCK,
                              spinlock, ret == 0, 0, 0, 0);
   return ret;
}

// pthread_spin_trylock
PTH_FUNC(int, pthreadZuspinZutrylock, // pthread_spin_trylock
              pthread_spinlock_t *spinlock)
{
   int   ret;
   int   res;
   OrigFn fn;
   VALGRIND_GET_ORIG_FN(fn);
   VALGRIND_DO_CLIENT_REQUEST(res, 0, VG_USERREQ__PRE_PTHREAD_MUTEX_LOCK,
                              spinlock, sizeof(*spinlock), mutex_type_spinlock,
                              0, 0);
   CALL_FN_W_W(ret, fn, spinlock);
   VALGRIND_DO_CLIENT_REQUEST(res, -1, VG_USERREQ__POST_PTHREAD_MUTEX_LOCK,
                              spinlock, ret == 0, 0, 0, 0);
   return ret;
}

// pthread_spin_unlock
PTH_FUNC(int, pthreadZuspinZuunlock, // pthread_spin_unlock
              pthread_spinlock_t *spinlock)
{
   int   ret;
   int   res;
   OrigFn fn;
   VALGRIND_GET_ORIG_FN(fn);
   VALGRIND_DO_CLIENT_REQUEST(res, -1, VG_USERREQ__SPIN_INIT_OR_UNLOCK,
                              spinlock, sizeof(*spinlock),
                              mutex_type_spinlock, 0, 0);
   CALL_FN_W_W(ret, fn, spinlock);
   return ret;
}

// pthread_barrier_init
PTH_FUNC(int, pthreadZubarrierZuinit, // pthread_barrier_init
              pthread_barrier_t* barrier,
              const pthread_barrierattr_t* attr,
              unsigned count)
{
   int   ret;
   int   res;
   OrigFn fn;
   VALGRIND_GET_ORIG_FN(fn);
   VALGRIND_DO_CLIENT_REQUEST(res, -1, VG_USERREQ__BARRIER_INIT,
                              barrier, sizeof(*barrier),
                              count, 0, 0);
   CALL_FN_W_WWW(ret, fn, barrier, attr, count);
   return ret;
}

// pthread_barrier_destroy
PTH_FUNC(int, pthreadZubarrierZudestroy, // pthread_barrier_destroy
              pthread_barrier_t* barrier)
{
   int   ret;
   int   res;
   OrigFn fn;
   VALGRIND_GET_ORIG_FN(fn);
   CALL_FN_W_W(ret, fn, barrier);
   VALGRIND_DO_CLIENT_REQUEST(res, -1, VG_USERREQ__BARRIER_DESTROY,
                              barrier, 0, 0, 0, 0);
   return ret;
}

// pthread_barrier_wait
PTH_FUNC(int, pthreadZubarrierZuwait, // pthread_barrier_wait
              pthread_barrier_t* barrier)
{
   int   ret;
   int   res;
   OrigFn fn;
   VALGRIND_GET_ORIG_FN(fn);
   VALGRIND_DO_CLIENT_REQUEST(res, -1, VG_USERREQ__PRE_BARRIER_WAIT,
                              barrier, 0, 0, 0, 0);
   CALL_FN_W_W(ret, fn, barrier);
   VALGRIND_DO_CLIENT_REQUEST(res, -1, VG_USERREQ__POST_BARRIER_WAIT,
                              barrier,
                              ret == 0 || ret == PTHREAD_BARRIER_SERIAL_THREAD,
                              0, 0, 0);
   return ret;
}


// From glibc 2.0 linuxthreads/sysdeps/pthread/cmpxchg/semaphorebits.h
typedef struct { long int sem_status; } sem_t_glibc_2_0;

// sem_init
PTH_FUNC(int, semZuinitZAGLIBCZu2Zd0, // sem_init@GLIBC_2.0
              sem_t_glibc_2_0 *sem,
              int pshared,
              unsigned int value)
{
   int   ret;
   int   res;
   OrigFn fn;
   VALGRIND_GET_ORIG_FN(fn);
   CALL_FN_W_WWW(ret, fn, sem, pshared, value);
   if (ret == 0)
   {
      VALGRIND_DO_CLIENT_REQUEST(res, -1, VG_USERREQ__SEM_INIT,
                                 sem, sizeof(*sem),
                                 pshared, value, 0);
   }
   return ret;
}

PTH_FUNC(int, semZuinitZa, // sem_init*
              sem_t *sem,
              int pshared,
              unsigned int value)
{
   int   ret;
   int   res;
   OrigFn fn;
   VALGRIND_GET_ORIG_FN(fn);
   CALL_FN_W_WWW(ret, fn, sem, pshared, value);
   if (ret == 0)
   {
      VALGRIND_DO_CLIENT_REQUEST(res, -1, VG_USERREQ__SEM_INIT,
                                 sem, sizeof(*sem),
                                 pshared, value, 0);
   }
   return ret;
}

// sem_destroy
PTH_FUNC(int, semZudestroyZAGLIBCZu2Zd0, // sem_destroy@GLIBC_2.0
              sem_t_glibc_2_0 *sem)
{
   int   ret;
   int   res;
   OrigFn fn;
   VALGRIND_GET_ORIG_FN(fn);
   CALL_FN_W_W(ret, fn, sem);
   if (ret == 0)
   {
      VALGRIND_DO_CLIENT_REQUEST(res, -1, VG_USERREQ__SEM_DESTROY,
                                 sem, 0, 0, 0, 0);
   }
   return ret;
}

PTH_FUNC(int, semZudestroyZa, // sem_destroy*
              sem_t *sem)
{
   int   ret;
   int   res;
   OrigFn fn;
   VALGRIND_GET_ORIG_FN(fn);
   CALL_FN_W_W(ret, fn, sem);
   if (ret == 0)
   {
      VALGRIND_DO_CLIENT_REQUEST(res, -1, VG_USERREQ__SEM_DESTROY,
                                 sem, 0, 0, 0, 0);
   }
   return ret;
}

// sem_wait
PTH_FUNC(int, semZuwaitZAGLIBCZu2Zd0, // sem_wait@GLIBC_2.0
              sem_t_glibc_2_0 *sem)
{
   int   ret;
   int   res;
   OrigFn fn;
   VALGRIND_GET_ORIG_FN(fn);
   VALGRIND_DO_CLIENT_REQUEST(res, -1, VG_USERREQ__PRE_SEM_WAIT,
                              sem, 0, 0, 0, 0);
   CALL_FN_W_W(ret, fn, sem);
   VALGRIND_DO_CLIENT_REQUEST(res, -1, VG_USERREQ__POST_SEM_WAIT,
                              sem, ret == 0, 0, 0, 0);
   return ret;
}

// sem_wait
PTH_FUNC(int, semZuwaitZa, // sem_wait*
              sem_t *sem)
{
   int   ret;
   int   res;
   OrigFn fn;
   VALGRIND_GET_ORIG_FN(fn);
   VALGRIND_DO_CLIENT_REQUEST(res, -1, VG_USERREQ__PRE_SEM_WAIT,
                              sem, 0, 0, 0, 0);
   CALL_FN_W_W(ret, fn, sem);
   VALGRIND_DO_CLIENT_REQUEST(res, -1, VG_USERREQ__POST_SEM_WAIT,
                              sem, ret == 0, 0, 0, 0);
   return ret;
}

// sem_trywait
PTH_FUNC(int, semZutrywaitZAGLIBCZu2Zd0, // sem_trywait@GLIBC_2.0
              sem_t_glibc_2_0 *sem)
{
   int   ret;
   int   res;
   OrigFn fn;
   VALGRIND_GET_ORIG_FN(fn);
   VALGRIND_DO_CLIENT_REQUEST(res, -1, VG_USERREQ__PRE_SEM_WAIT,
                              sem, 0, 0, 0, 0);
   CALL_FN_W_W(ret, fn, sem);
   VALGRIND_DO_CLIENT_REQUEST(res, -1, VG_USERREQ__POST_SEM_WAIT,
                              sem, ret == 0, 0, 0, 0);
   return ret;
}

PTH_FUNC(int, semZutrywaitZa, // sem_trywait*
              sem_t *sem)
{
   int   ret;
   int   res;
   OrigFn fn;
   VALGRIND_GET_ORIG_FN(fn);
   VALGRIND_DO_CLIENT_REQUEST(res, -1, VG_USERREQ__PRE_SEM_WAIT,
                              sem, 0, 0, 0, 0);
   CALL_FN_W_W(ret, fn, sem);
   VALGRIND_DO_CLIENT_REQUEST(res, -1, VG_USERREQ__POST_SEM_WAIT,
                              sem, ret == 0, 0, 0, 0);
   return ret;
}

// sem_timedwait
PTH_FUNC(int, semZutimedwait, // sem_timedwait
              sem_t *sem, const struct timespec *abs_timeout)
{
   int   ret;
   int   res;
   OrigFn fn;
   VALGRIND_GET_ORIG_FN(fn);
   VALGRIND_DO_CLIENT_REQUEST(res, -1, VG_USERREQ__PRE_SEM_WAIT,
                              sem, 0, 0, 0, 0);
   CALL_FN_W_WW(ret, fn, sem, abs_timeout);
   VALGRIND_DO_CLIENT_REQUEST(res, -1, VG_USERREQ__POST_SEM_WAIT,
                              sem, ret == 0, 0, 0, 0);
   return ret;
}

// sem_post
PTH_FUNC(int, semZupostZAGLIBCZu2Zd0, // sem_post@GLIBC_2.0
              sem_t_glibc_2_0 *sem)
{
   int   ret;
   int   res;
   OrigFn fn;
   VALGRIND_GET_ORIG_FN(fn);
   VALGRIND_DO_CLIENT_REQUEST(res, -1, VG_USERREQ__PRE_SEM_POST,
                              sem, sizeof(*sem), 0, 0, 0);
   CALL_FN_W_W(ret, fn, sem);
   VALGRIND_DO_CLIENT_REQUEST(res, -1, VG_USERREQ__POST_SEM_POST,
                              sem, sizeof(*sem), ret == 0, 0, 0);
   return ret;
}

// sem_post
PTH_FUNC(int, semZupostZa, // sem_post*
              sem_t *sem)
{
   int   ret;
   int   res;
   OrigFn fn;
   VALGRIND_GET_ORIG_FN(fn);
   VALGRIND_DO_CLIENT_REQUEST(res, -1, VG_USERREQ__PRE_SEM_POST,
                              sem, sizeof(*sem), 0, 0, 0);
   CALL_FN_W_W(ret, fn, sem);
   VALGRIND_DO_CLIENT_REQUEST(res, -1, VG_USERREQ__POST_SEM_POST,
                              sem, sizeof(*sem), ret == 0, 0, 0);
   return ret;
}

// pthread_rwlock_init
PTH_FUNC(int,
         pthreadZurwlockZuinitZa, // pthread_rwlock_init*
         pthread_rwlock_t* rwlock,
         const pthread_rwlockattr_t* attr)
{
   int   ret;
   int   res;
   OrigFn fn;
   VALGRIND_GET_ORIG_FN(fn);
   VALGRIND_DO_CLIENT_REQUEST(res, -1, VG_USERREQ__PRE_RWLOCK_INIT,
                              rwlock, sizeof(*rwlock), 0, 0, 0);
   CALL_FN_W_WW(ret, fn, rwlock, attr);
   return ret;
}

// pthread_rwlock_destroy
PTH_FUNC(int,
         pthreadZurwlockZudestroyZa, // pthread_rwlock_destroy*
         pthread_rwlock_t* rwlock)
{
   int   ret;
   int   res;
   OrigFn fn;
   VALGRIND_GET_ORIG_FN(fn);
   CALL_FN_W_W(ret, fn, rwlock);
   VALGRIND_DO_CLIENT_REQUEST(res, -1, VG_USERREQ__POST_RWLOCK_DESTROY,
                              rwlock, 0, 0, 0, 0);
   return ret;
}

// pthread_rwlock_rdlock
PTH_FUNC(int,
         pthreadZurwlockZurdlockZa, // pthread_rwlock_rdlock*
         pthread_rwlock_t* rwlock)
{
   int   ret;
   int   res;
   OrigFn fn;
   VALGRIND_GET_ORIG_FN(fn);
   VALGRIND_DO_CLIENT_REQUEST(res, -1, VG_USERREQ__PRE_RWLOCK_RDLOCK,
                              rwlock, sizeof(*rwlock), 0, 0, 0);
   CALL_FN_W_W(ret, fn, rwlock);
   VALGRIND_DO_CLIENT_REQUEST(res, -1, VG_USERREQ__POST_RWLOCK_RDLOCK,
                              rwlock, ret == 0, 0, 0, 0);
   return ret;
}

// pthread_rwlock_wrlock
PTH_FUNC(int,
         pthreadZurwlockZuwrlockZa, // pthread_rwlock_wrlock*
         pthread_rwlock_t* rwlock)
{
   int   ret;
   int   res;
   OrigFn fn;
   VALGRIND_GET_ORIG_FN(fn);
   VALGRIND_DO_CLIENT_REQUEST(res, -1, VG_USERREQ__PRE_RWLOCK_WRLOCK,
                              rwlock, sizeof(*rwlock), 0, 0, 0);
   CALL_FN_W_W(ret, fn, rwlock);
   VALGRIND_DO_CLIENT_REQUEST(res, -1, VG_USERREQ__POST_RWLOCK_WRLOCK,
                              rwlock, ret == 0, 0, 0, 0);
   return ret;
}

// pthread_rwlock_timedrdlock
PTH_FUNC(int,
         pthreadZurwlockZutimedrdlockZa, // pthread_rwlock_timedrdlock*
         pthread_rwlock_t* rwlock)
{
   int   ret;
   int   res;
   OrigFn fn;
   VALGRIND_GET_ORIG_FN(fn);
   VALGRIND_DO_CLIENT_REQUEST(res, -1, VG_USERREQ__PRE_RWLOCK_RDLOCK,
                              rwlock, sizeof(*rwlock), 0, 0, 0);
   CALL_FN_W_W(ret, fn, rwlock);
   VALGRIND_DO_CLIENT_REQUEST(res, -1, VG_USERREQ__POST_RWLOCK_RDLOCK,
                              rwlock, ret == 0, 0, 0, 0);
   return ret;
}

// pthread_rwlock_timedwrlock
PTH_FUNC(int,
         pthreadZurwlockZutimedwrlockZa, // pthread_rwlock_timedwrlock*
         pthread_rwlock_t* rwlock)
{
   int   ret;
   int   res;
   OrigFn fn;
   VALGRIND_GET_ORIG_FN(fn);
   VALGRIND_DO_CLIENT_REQUEST(res, -1, VG_USERREQ__PRE_RWLOCK_WRLOCK,
                              rwlock, sizeof(*rwlock), 0, 0, 0);
   CALL_FN_W_W(ret, fn, rwlock);
   VALGRIND_DO_CLIENT_REQUEST(res, -1, VG_USERREQ__POST_RWLOCK_WRLOCK,
                              rwlock, ret == 0, 0, 0, 0);
   return ret;
}

// pthread_rwlock_tryrdlock
PTH_FUNC(int,
         pthreadZurwlockZutryrdlockZa, // pthread_rwlock_tryrdlock*
         pthread_rwlock_t* rwlock)
{
   int   ret;
   int   res;
   OrigFn fn;
   VALGRIND_GET_ORIG_FN(fn);
   VALGRIND_DO_CLIENT_REQUEST(res, -1, VG_USERREQ__PRE_RWLOCK_RDLOCK,
                              rwlock, sizeof(*rwlock), 0, 0, 0);
   CALL_FN_W_W(ret, fn, rwlock);
   VALGRIND_DO_CLIENT_REQUEST(res, -1, VG_USERREQ__POST_RWLOCK_RDLOCK,
                              rwlock, ret == 0, 0, 0, 0);
   return ret;
}

// pthread_rwlock_trywrlock
PTH_FUNC(int,
         pthreadZurwlockZutrywrlockZa, // pthread_rwlock_trywrlock*
         pthread_rwlock_t* rwlock)
{
   int   ret;
   int   res;
   OrigFn fn;
   VALGRIND_GET_ORIG_FN(fn);
   VALGRIND_DO_CLIENT_REQUEST(res, -1, VG_USERREQ__PRE_RWLOCK_WRLOCK,
                              rwlock, sizeof(*rwlock), 0, 0, 0);
   CALL_FN_W_W(ret, fn, rwlock);
   VALGRIND_DO_CLIENT_REQUEST(res, -1, VG_USERREQ__POST_RWLOCK_WRLOCK,
                              rwlock, ret == 0, 0, 0, 0);
   return ret;
}

// pthread_rwlock_unlock
PTH_FUNC(int,
         pthreadZurwlockZuunlockZa, // pthread_rwlock_unlock*
         pthread_rwlock_t* rwlock)
{
   int   ret;
   int   res;
   OrigFn fn;
   VALGRIND_GET_ORIG_FN(fn);
   CALL_FN_W_W(ret, fn, rwlock);
   VALGRIND_DO_CLIENT_REQUEST(res, -1, VG_USERREQ__POST_RWLOCK_UNLOCK,
                              rwlock, ret == 0, 0, 0, 0);
   return ret;
}


/*
 * Local variables:
 * c-basic-offset: 3
 * End:
 */
