
/*--------------------------------------------------------------------*/
/*--- Client-space code for DRD.          drd_pthread_intercepts.c ---*/
/*--------------------------------------------------------------------*/

/*
  This file is part of DRD, a data race detector.

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

/*
 * Define _GNU_SOURCE to make sure that pthread_spinlock_t is available when
 * compiling with older glibc versions (2.3 or before).
 */
#ifndef _GNU_SOURCE
#define _GNU_SOURCE
#endif

#include <assert.h>         /* assert() */
#include <pthread.h>        /* pthread_mutex_t */
#include <semaphore.h>      /* sem_t */
#include <stdio.h>          /* fprintf() */
#include <stdlib.h>         /* malloc(), free() */
#include <unistd.h>         /* confstr() */
#include "config.h"         /* HAVE_PTHREAD_MUTEX_ADAPTIVE_NP etc. */
#include "drd_basics.h"     /* DRD_() */
#include "drd_clientreq.h"
#include "pub_tool_redir.h" /* VG_WRAP_FUNCTION_ZZ() */


/* Defines. */

#define PTH_FUNC(ret_ty, f, args...)                            \
  ret_ty VG_WRAP_FUNCTION_ZZ(libpthreadZdsoZd0,f)(args);        \
  ret_ty VG_WRAP_FUNCTION_ZZ(libpthreadZdsoZd0,f)(args)


/* Local data structures. */

typedef struct
{
  void* (*start)(void*);
  void* arg;
  int   detachstate;
  int   wrapper_started;
} DrdPosixThreadArgs;


/* Local function declarations. */

static void DRD_(init)(void) __attribute__((constructor));
static void DRD_(check_threading_library)(void);
static void DRD_(set_main_thread_state)(void);


/* Function definitions. */

/**
 * Shared library initialization function. The function init() is called after
 * dlopen() has loaded the shared library with DRD client intercepts because
 * the constructor attribute was specified in the declaration of this function.
 * Note: do specify the -nostdlib option to gcc when linking this code into a
 * shared library because doing so would cancel the effect of the constructor
 * attribute ! Using the gcc option -nodefaultlibs is fine because this last
 * option preserves the shared library initialization code that calls
 * constructor and destructor functions.
 */
static void DRD_(init)(void)
{
  DRD_(check_threading_library)();
  DRD_(set_main_thread_state)();
}

/**
 * POSIX threads and DRD each have their own mutex type identification.
 * Convert POSIX threads' mutex type to DRD's mutex type. In the code below
 * if-statements are used to test the value of 'kind' instead of a switch
 * statement because some of the PTHREAD_MUTEX_ macro's may have the same
 * value.
 */
static MutexT DRD_(pthread_to_drd_mutex_type)(const int kind)
{
  if (kind == PTHREAD_MUTEX_RECURSIVE)
    return mutex_type_recursive_mutex;
  else if (kind == PTHREAD_MUTEX_ERRORCHECK)
    return mutex_type_errorcheck_mutex;
  else if (kind == PTHREAD_MUTEX_NORMAL)
    return mutex_type_default_mutex;
  else if (kind == PTHREAD_MUTEX_DEFAULT)
    return mutex_type_default_mutex;
#if defined(HAVE_PTHREAD_MUTEX_ADAPTIVE_NP)
  else if (kind == PTHREAD_MUTEX_ADAPTIVE_NP)
    return mutex_type_default_mutex;
#endif
  else
  {
    return mutex_type_invalid_mutex;
  }
}

/**
 * Read the mutex type stored in the client memory used for the mutex
 * implementation.
 *
 * @note This function depends on the implementation of the POSIX threads
 *   library -- the POSIX standard does not define the name of the member in
 *   which the mutex type is stored.
 * @note The function mutex_type() has been declared inline in order
 *   to avoid that it shows up in call stacks (drd/tests/...exp* files).
 */
static __inline__ MutexT DRD_(mutex_type)(pthread_mutex_t* mutex)
{
#if defined(HAVE_PTHREAD_MUTEX_T__M_KIND)
  /* glibc + LinuxThreads. */
  const int kind = mutex->__m_kind;
#elif defined(HAVE_PTHREAD_MUTEX_T__DATA__KIND)
  /* glibc + NPTL. */
  const int kind = mutex->__data.__kind;
#else
  /* Another POSIX threads implementation. Regression tests will fail. */
  const int kind = PTHREAD_MUTEX_DEFAULT;
  fprintf(stderr,
          "Did not recognize your POSIX threads implementation. Giving up.\n");
  assert(0);
#endif
  return DRD_(pthread_to_drd_mutex_type)(kind);
}

/**
 * Tell DRD whether 'tid' is a joinable thread or a detached thread.
 */
static void DRD_(set_joinable)(const pthread_t tid, const int joinable)
{
  int res;
  assert(joinable == 0 || joinable == 1);
  VALGRIND_DO_CLIENT_REQUEST(res, 0, VG_USERREQ__SET_JOINABLE,
                             tid, joinable, 0, 0, 0);
}

/**
 * The function called from the thread created by pthread_create().
 */
static void* DRD_(thread_wrapper)(void* arg)
{
  int res;
  DrdPosixThreadArgs* arg_ptr;
  DrdPosixThreadArgs arg_copy;

  VALGRIND_DO_CLIENT_REQUEST(res, 0, VG_USERREQ__DRD_SUPPRESS_CURRENT_STACK,
                             0, 0, 0, 0, 0);

  arg_ptr = (DrdPosixThreadArgs*)arg;
  arg_copy = *arg_ptr;
  arg_ptr->wrapper_started = 1;

  VALGRIND_DO_CLIENT_REQUEST(res, -1, VG_USERREQ__SET_PTHREADID,
                             pthread_self(), 0, 0, 0, 0);

  DRD_(set_joinable)(pthread_self(),
                     arg_copy.detachstate == PTHREAD_CREATE_JOINABLE);

  return (arg_copy.start)(arg_copy.arg);
}

/**
 * Return 1 if the LinuxThread implementation has been detected, and 0
 * otherwise. For more information about the confstr() function, see also
 * http://www.opengroup.org/onlinepubs/009695399/functions/confstr.html
 */
static int DRD_(detected_linuxthreads)(void)
{
#if defined(linux)
#if defined(_CS_GNU_LIBPTHREAD_VERSION)
  /* Linux with a recent glibc. */
  char buffer[256];
  unsigned len;
  len = confstr(_CS_GNU_LIBPTHREAD_VERSION, buffer, sizeof(buffer));
  assert(len <= sizeof(buffer));
  return len > 0 && buffer[0] == 'l';
#else
  /* Linux without _CS_GNU_LIBPTHREAD_VERSION: most likely LinuxThreads. */
  return 1;
#endif
#else
  /* Another OS than Linux, hence no LinuxThreads. */
  return 0;
#endif
}

/**
 * Stop and print an error message in case a non-supported threading
 * library implementation (LinuxThreads) has been detected.
 */
static void DRD_(check_threading_library)(void)
{
  if (DRD_(detected_linuxthreads)())
  {
    if (getenv("LD_ASSUME_KERNEL"))
    {
      fprintf(stderr,
"Detected the LinuxThreads threading library. Sorry, but DRD only supports\n"
"the newer NPTL (Native POSIX Threads Library). Please try to rerun DRD\n"
"after having unset the environment variable LD_ASSUME_KERNEL. Giving up.\n"
              );
    }
    else
    {
      fprintf(stderr,
"Detected the LinuxThreads threading library. Sorry, but DRD only supports\n"
"the newer NPTL (Native POSIX Threads Library). Please try to rerun DRD\n"
"after having upgraded to a newer version of your Linux distribution.\n"
"Giving up.\n"
              );
    }
    abort();
  }
}

/**
 * The main thread is the only thread not created by pthread_create().
 * Update DRD's state information about the main thread.
 */
static void DRD_(set_main_thread_state)(void)
{
  int res;

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
  int    res;
  int    ret;
  OrigFn fn;
  DrdPosixThreadArgs thread_args;

  VALGRIND_GET_ORIG_FN(fn);

  DRD_IGNORE_VAR(thread_args.wrapper_started);
  thread_args.start           = start;
  thread_args.arg             = arg;
  thread_args.wrapper_started = 0;
  /*
   * Find out whether the thread will be started as a joinable thread
   * or as a detached thread. If no thread attributes have been specified,
   * the new thread will be started as a joinable thread.
   */
  thread_args.detachstate = PTHREAD_CREATE_JOINABLE;
  if (attr)
  {
    if (pthread_attr_getdetachstate(attr, &thread_args.detachstate) != 0)
    {
      assert(0);
    }
  }
  assert(thread_args.detachstate == PTHREAD_CREATE_JOINABLE
         || thread_args.detachstate == PTHREAD_CREATE_DETACHED);

  /* Suppress NPTL-specific conflicts between creator and created thread. */
  VALGRIND_DO_CLIENT_REQUEST(res, -1, VG_USERREQ__DRD_STOP_RECORDING,
                             0, 0, 0, 0, 0);

  CALL_FN_W_WWWW(ret, fn, thread, attr, DRD_(thread_wrapper), &thread_args);

  VALGRIND_DO_CLIENT_REQUEST(res, -1, VG_USERREQ__DRD_START_RECORDING,
                             0, 0, 0, 0, 0);

  if (ret == 0)
  {
    /*
     * Wait until the thread wrapper started.
     * @todo Find out why some regression tests fail if thread arguments are
     *   passed via dynamically allocated memory and if the loop below is
     *   removed.
     */
    while (! thread_args.wrapper_started)
    {
      sched_yield();
    }
  }

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
      DRD_(set_joinable)(pt_thread, 0);
    }
  }
  return ret;
}

// pthread_cancel
PTH_FUNC(int, pthreadZucancel, pthread_t pt_thread)
{
  int res;
  int ret;
  OrigFn fn;
  VALGRIND_GET_ORIG_FN(fn);
  VALGRIND_DO_CLIENT_REQUEST(res, -1, VG_USERREQ__PRE_THREAD_CANCEL,
                             pt_thread, 0, 0, 0, 0);
  CALL_FN_W_W(ret, fn, pt_thread);
  VALGRIND_DO_CLIENT_REQUEST(res, -1, VG_USERREQ__POST_THREAD_CANCEL,
                             pt_thread, ret==0, 0, 0, 0);
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
  int mt;
  VALGRIND_GET_ORIG_FN(fn);
  mt = PTHREAD_MUTEX_DEFAULT;
  if (attr)
    pthread_mutexattr_gettype(attr, &mt);
  VALGRIND_DO_CLIENT_REQUEST(res, -1, VG_USERREQ__PRE_MUTEX_INIT,
                             mutex, DRD_(pthread_to_drd_mutex_type)(mt),
                             0, 0, 0);
  CALL_FN_W_WW(ret, fn, mutex, attr);
  VALGRIND_DO_CLIENT_REQUEST(res, -1, VG_USERREQ__POST_MUTEX_INIT,
                             mutex, 0, 0, 0, 0);
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
  VALGRIND_DO_CLIENT_REQUEST(res, -1, VG_USERREQ__PRE_MUTEX_DESTROY,
                             mutex, 0, 0, 0, 0);
  CALL_FN_W_W(ret, fn, mutex);
  VALGRIND_DO_CLIENT_REQUEST(res, -1, VG_USERREQ__POST_MUTEX_DESTROY,
                             mutex, DRD_(mutex_type)(mutex), 0, 0, 0);
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
  VALGRIND_DO_CLIENT_REQUEST(res, 0, VG_USERREQ__PRE_MUTEX_LOCK,
                             mutex, DRD_(mutex_type)(mutex), 0, 0, 0);
  CALL_FN_W_W(ret, fn, mutex);
  VALGRIND_DO_CLIENT_REQUEST(res, 0, VG_USERREQ__POST_MUTEX_LOCK,
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
  VALGRIND_DO_CLIENT_REQUEST(res, 0, VG_USERREQ__PRE_MUTEX_LOCK,
                             mutex, DRD_(mutex_type)(mutex), 1, 0, 0);
  CALL_FN_W_W(ret, fn, mutex);
  VALGRIND_DO_CLIENT_REQUEST(res, -1, VG_USERREQ__POST_MUTEX_LOCK,
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
  VALGRIND_DO_CLIENT_REQUEST(res, 0, VG_USERREQ__PRE_MUTEX_LOCK,
                             mutex, DRD_(mutex_type)(mutex), 0, 0, 0);
  CALL_FN_W_WW(ret, fn, mutex, abs_timeout);
  VALGRIND_DO_CLIENT_REQUEST(res, -1, VG_USERREQ__POST_MUTEX_LOCK,
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
                             VG_USERREQ__PRE_MUTEX_UNLOCK,
                             mutex, DRD_(mutex_type)(mutex), 0, 0, 0);
  CALL_FN_W_W(ret, fn, mutex);
  VALGRIND_DO_CLIENT_REQUEST(res, -1,
                             VG_USERREQ__POST_MUTEX_UNLOCK,
                             mutex, 0, 0, 0, 0);
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
  VALGRIND_DO_CLIENT_REQUEST(res, -1, VG_USERREQ__PRE_COND_INIT,
                             cond, 0, 0, 0, 0);
  CALL_FN_W_WW(ret, fn, cond, attr);
  VALGRIND_DO_CLIENT_REQUEST(res, -1, VG_USERREQ__POST_COND_INIT,
                             cond, 0, 0, 0, 0);
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
  VALGRIND_DO_CLIENT_REQUEST(res, -1, VG_USERREQ__PRE_COND_DESTROY,
                             cond, 0, 0, 0, 0);
  CALL_FN_W_W(ret, fn, cond);
  VALGRIND_DO_CLIENT_REQUEST(res, -1, VG_USERREQ__POST_COND_DESTROY,
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
  VALGRIND_DO_CLIENT_REQUEST(res, -1, VG_USERREQ__PRE_COND_WAIT,
                             cond, mutex, DRD_(mutex_type)(mutex), 0, 0);
  CALL_FN_W_WW(ret, fn, cond, mutex);
  VALGRIND_DO_CLIENT_REQUEST(res, -1, VG_USERREQ__POST_COND_WAIT,
                             cond, mutex, 1, 0, 0);
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
  VALGRIND_DO_CLIENT_REQUEST(res, -1, VG_USERREQ__PRE_COND_WAIT,
                             cond, mutex, DRD_(mutex_type)(mutex), 0, 0);
  CALL_FN_W_WWW(ret, fn, cond, mutex, abstime);
  VALGRIND_DO_CLIENT_REQUEST(res, -1, VG_USERREQ__POST_COND_WAIT,
                             cond, mutex, 1, 0, 0);
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
  VALGRIND_DO_CLIENT_REQUEST(res, -1, VG_USERREQ__PRE_COND_SIGNAL,
                             cond, 0, 0, 0, 0);
  CALL_FN_W_W(ret, fn, cond);
  VALGRIND_DO_CLIENT_REQUEST(res, -1, VG_USERREQ__POST_COND_SIGNAL,
                             cond, 0, 0, 0, 0);
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
  VALGRIND_DO_CLIENT_REQUEST(res, -1, VG_USERREQ__PRE_COND_BROADCAST,
                             cond, 0, 0, 0, 0);
  CALL_FN_W_W(ret, fn, cond);
  VALGRIND_DO_CLIENT_REQUEST(res, -1, VG_USERREQ__POST_COND_BROADCAST,
                             cond, 0, 0, 0, 0);
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
  VALGRIND_DO_CLIENT_REQUEST(res, -1, VG_USERREQ__PRE_SPIN_INIT_OR_UNLOCK,
                             spinlock, 0, 0, 0, 0);
  CALL_FN_W_WW(ret, fn, spinlock, pshared);
  VALGRIND_DO_CLIENT_REQUEST(res, -1, VG_USERREQ__POST_SPIN_INIT_OR_UNLOCK,
                             spinlock, 0, 0, 0, 0);
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
  VALGRIND_DO_CLIENT_REQUEST(res, -1, VG_USERREQ__PRE_MUTEX_DESTROY,
                             spinlock, 0, 0, 0, 0);
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
  VALGRIND_DO_CLIENT_REQUEST(res, 0, VG_USERREQ__PRE_MUTEX_LOCK,
                             spinlock, mutex_type_spinlock, 0, 0, 0);
  CALL_FN_W_W(ret, fn, spinlock);
  VALGRIND_DO_CLIENT_REQUEST(res, -1, VG_USERREQ__POST_MUTEX_LOCK,
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
  VALGRIND_DO_CLIENT_REQUEST(res, 0, VG_USERREQ__PRE_MUTEX_LOCK,
                             spinlock, mutex_type_spinlock, 0, 0, 0);
  CALL_FN_W_W(ret, fn, spinlock);
  VALGRIND_DO_CLIENT_REQUEST(res, -1, VG_USERREQ__POST_MUTEX_LOCK,
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
  VALGRIND_DO_CLIENT_REQUEST(res, -1, VG_USERREQ__PRE_SPIN_INIT_OR_UNLOCK,
                             spinlock, mutex_type_spinlock, 0, 0, 0);
  CALL_FN_W_W(ret, fn, spinlock);
  VALGRIND_DO_CLIENT_REQUEST(res, -1, VG_USERREQ__POST_SPIN_INIT_OR_UNLOCK,
                             spinlock, 0, 0, 0, 0);
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
  VALGRIND_DO_CLIENT_REQUEST(res, -1, VG_USERREQ__PRE_BARRIER_INIT,
                             barrier, pthread_barrier, count, 0, 0);
  CALL_FN_W_WWW(ret, fn, barrier, attr, count);
  VALGRIND_DO_CLIENT_REQUEST(res, -1, VG_USERREQ__POST_BARRIER_INIT,
                             barrier, pthread_barrier, 0, 0, 0);
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
  VALGRIND_DO_CLIENT_REQUEST(res, -1, VG_USERREQ__PRE_BARRIER_DESTROY,
                             barrier, pthread_barrier, 0, 0, 0);
  CALL_FN_W_W(ret, fn, barrier);
  VALGRIND_DO_CLIENT_REQUEST(res, -1, VG_USERREQ__POST_BARRIER_DESTROY,
                             barrier, pthread_barrier, 0, 0, 0);
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
                             barrier, pthread_barrier, 0, 0, 0);
  CALL_FN_W_W(ret, fn, barrier);
  VALGRIND_DO_CLIENT_REQUEST(res, -1, VG_USERREQ__POST_BARRIER_WAIT,
                             barrier, pthread_barrier,
                             ret == 0 || ret == PTHREAD_BARRIER_SERIAL_THREAD,
                             0, 0);
  return ret;
}


// sem_init
PTH_FUNC(int, semZuinitZa, // sem_init*
         sem_t *sem,
         int pshared,
         unsigned int value)
{
  int   ret;
  int   res;
  OrigFn fn;
  VALGRIND_GET_ORIG_FN(fn);
  VALGRIND_DO_CLIENT_REQUEST(res, -1, VG_USERREQ__PRE_SEM_INIT,
                             sem, pshared, value, 0, 0);
  CALL_FN_W_WWW(ret, fn, sem, pshared, value);
  VALGRIND_DO_CLIENT_REQUEST(res, -1, VG_USERREQ__POST_SEM_INIT,
                             sem, 0, 0, 0, 0);
  return ret;
}

// sem_destroy
PTH_FUNC(int, semZudestroyZa, // sem_destroy*
         sem_t *sem)
{
  int   ret;
  int   res;
  OrigFn fn;
  VALGRIND_GET_ORIG_FN(fn);
  VALGRIND_DO_CLIENT_REQUEST(res, -1, VG_USERREQ__PRE_SEM_DESTROY,
                             sem, 0, 0, 0, 0);
  CALL_FN_W_W(ret, fn, sem);
  VALGRIND_DO_CLIENT_REQUEST(res, -1, VG_USERREQ__POST_SEM_DESTROY,
                             sem, 0, 0, 0, 0);
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
PTH_FUNC(int, semZupostZa, // sem_post*
         sem_t *sem)
{
  int   ret;
  int   res;
  OrigFn fn;
  VALGRIND_GET_ORIG_FN(fn);
  VALGRIND_DO_CLIENT_REQUEST(res, -1, VG_USERREQ__PRE_SEM_POST,
                             sem, 0, 0, 0, 0);
  CALL_FN_W_W(ret, fn, sem);
  VALGRIND_DO_CLIENT_REQUEST(res, -1, VG_USERREQ__POST_SEM_POST,
                             sem, ret == 0, 0, 0, 0);
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
                             rwlock, 0, 0, 0, 0);
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
                             rwlock, 0, 0, 0, 0);
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
                             rwlock, 0, 0, 0, 0);
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
                             rwlock, 0, 0, 0, 0);
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
                             rwlock, 0, 0, 0, 0);
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
                             rwlock, 0, 0, 0, 0);
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
                             rwlock, 0, 0, 0, 0);
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
  VALGRIND_DO_CLIENT_REQUEST(res, -1, VG_USERREQ__PRE_RWLOCK_UNLOCK,
                             rwlock, 0, 0, 0, 0);
  CALL_FN_W_W(ret, fn, rwlock);
  VALGRIND_DO_CLIENT_REQUEST(res, -1, VG_USERREQ__POST_RWLOCK_UNLOCK,
                             rwlock, ret == 0, 0, 0, 0);
  return ret;
}
