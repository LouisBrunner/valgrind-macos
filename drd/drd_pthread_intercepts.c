/* -*- mode: C; c-basic-offset: 3; indent-tabs-mode: nil; -*- */

/*--------------------------------------------------------------------*/
/*--- Client-space code for DRD.          drd_pthread_intercepts.c ---*/
/*--------------------------------------------------------------------*/

/*
  This file is part of DRD, a thread error detector.

  Copyright (C) 2006-2011 Bart Van Assche <bvanassche@acm.org>.

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
#include <stdint.h>         /* uintptr_t */
#include <stdio.h>          /* fprintf() */
#include <stdlib.h>         /* malloc(), free() */
#include <unistd.h>         /* confstr() */
#include "config.h"         /* HAVE_PTHREAD_MUTEX_ADAPTIVE_NP etc. */
#include "drd_basics.h"     /* DRD_() */
#include "drd_clientreq.h"
#include "pub_tool_redir.h" /* VG_WRAP_FUNCTION_ZZ() */


/*
 * Notes regarding thread creation:
 * - sg_init() runs on the context of the created thread and copies the vector
 *   clock of the creator thread. This only works reliably if the creator
 *   thread waits until this copy has been performed.
 * - DRD_(thread_compute_minimum_vc)() does not take the vector clocks into
 *   account that are involved in thread creation and for which the
 *   corresponding thread has not yet been created. So not waiting until the
 *   created thread has been started would make it possible that segments get
 *   discarded that should not yet be discarded. Or: some data races are not
 *   detected.
 */

/**
 * Macro for generating a Valgrind interception function.
 * @param[in] ret_ty Return type of the function to be generated.
 * @param[in] zf Z-encoded name of the interception function.
 * @param[in] implf Name of the function that implements the intercept.
 * @param[in] arg_decl Argument declaration list enclosed in parentheses.
 * @param[in] argl Argument list enclosed in parentheses.
 */
#ifdef VGO_darwin
static int never_true;
#define PTH_FUNC(ret_ty, zf, implf, argl_decl, argl)                    \
   ret_ty VG_WRAP_FUNCTION_ZZ(VG_Z_LIBPTHREAD_SONAME,zf) argl_decl;     \
   ret_ty VG_WRAP_FUNCTION_ZZ(VG_Z_LIBPTHREAD_SONAME,zf) argl_decl      \
   {									\
      ret_ty pth_func_result = implf argl;				\
      /* Apparently inserting a function call in wrapper functions */   \
      /* is sufficient to avoid misaligned stack errors.           */	\
      if (never_true)							\
	 fflush(stdout);						\
      return pth_func_result;						\
   }
#else
#define PTH_FUNC(ret_ty, zf, implf, argl_decl, argl)                    \
   ret_ty VG_WRAP_FUNCTION_ZZ(VG_Z_LIBPTHREAD_SONAME,zf) argl_decl;     \
   ret_ty VG_WRAP_FUNCTION_ZZ(VG_Z_LIBPTHREAD_SONAME,zf) argl_decl      \
   { return implf argl; }
#endif

/**
 * Macro for generating three Valgrind interception functions: one with the
 * Z-encoded name zf, one with ZAZa ("@*") appended to the name zf and one
 * with ZDZa ("$*") appended to the name zf. The second generated interception
 * function will intercept versioned symbols on Linux, and the third will
 * intercept versioned symbols on Darwin.
 */
#define PTH_FUNCS(ret_ty, zf, implf, argl_decl, argl)           \
   PTH_FUNC(ret_ty, zf, implf, argl_decl, argl);                \
   PTH_FUNC(ret_ty, zf ## ZAZa, implf, argl_decl, argl);        \
   PTH_FUNC(ret_ty, zf ## ZDZa, implf, argl_decl, argl);

/*
 * Not inlining one of the intercept functions will cause the regression
 * tests to fail because this would cause an additional stackfram to appear
 * in the output. The __always_inline macro guarantees that inlining will
 * happen, even when compiling with optimization disabled.
 */
#undef __always_inline /* since already defined in <cdefs.h> */
#if __GNUC__ > 3 || __GNUC__ == 3 && __GNUC_MINOR__ >= 2
#define __always_inline __inline__ __attribute__((always_inline))
#else
#define __always_inline __inline__
#endif

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

#define IS_ALIGNED(p) (((uintptr_t)(p) & (sizeof(*(p)) - 1)) == 0)

/**
 * Read the mutex type stored in the client memory used for the mutex
 * implementation.
 *
 * @note This function depends on the implementation of the POSIX threads
 *   library -- the POSIX standard does not define the name of the member in
 *   which the mutex type is stored.
 * @note The function mutex_type() has been declared inline in order
 *   to avoid that it shows up in call stacks (drd/tests/...exp* files).
 * @note glibc stores the mutex type in the lowest two bits, and uses the
 *   higher bits for flags like PTHREAD_MUTEXATTR_FLAG_ROBUST and
 *   PTHREAD_MUTEXATTR_FLAG_PSHARED.
 */
static __always_inline MutexT DRD_(mutex_type)(pthread_mutex_t* mutex)
{
#if defined(HAVE_PTHREAD_MUTEX_T__M_KIND)
   /* glibc + LinuxThreads. */
   if (IS_ALIGNED(&mutex->__m_kind))
   {
      const int kind = mutex->__m_kind & 3;
      return DRD_(pthread_to_drd_mutex_type)(kind);
   }
#elif defined(HAVE_PTHREAD_MUTEX_T__DATA__KIND)
   /* glibc + NPTL. */
   if (IS_ALIGNED(&mutex->__data.__kind))
   {
      const int kind = mutex->__data.__kind & 3;
      return DRD_(pthread_to_drd_mutex_type)(kind);
   }
#else
   /*
    * Another POSIX threads implementation. The mutex type won't be printed
    * when enabling --trace-mutex=yes.
    */
#endif
   return mutex_type_unknown;
}

/**
 * Tell DRD whether 'tid' is a joinable thread or a detached thread.
 */
static void DRD_(set_joinable)(const pthread_t tid, const int joinable)
{
   assert(joinable == 0 || joinable == 1);
   VALGRIND_DO_CLIENT_REQUEST_EXPR(0, VG_USERREQ__SET_JOINABLE,
                                   tid, joinable, 0, 0, 0);
}

/** Tell DRD that the calling thread is about to enter pthread_create(). */
static __always_inline void DRD_(entering_pthread_create)(void)
{
   VALGRIND_DO_CLIENT_REQUEST_EXPR(0, VG_USERREQ__ENTERING_PTHREAD_CREATE,
                                   0, 0, 0, 0, 0);
}

/** Tell DRD that the calling thread has left pthread_create(). */
static __always_inline void DRD_(left_pthread_create)(void)
{
   VALGRIND_DO_CLIENT_REQUEST_EXPR(0, VG_USERREQ__LEFT_PTHREAD_CREATE,
                                   0, 0, 0, 0, 0);
}

/**
 * Entry point for newly created threads. This function is called from the
 * thread created by pthread_create().
 */
static void* DRD_(thread_wrapper)(void* arg)
{
   DrdPosixThreadArgs* arg_ptr;
   DrdPosixThreadArgs arg_copy;

   arg_ptr = (DrdPosixThreadArgs*)arg;
   arg_copy = *arg_ptr;

   VALGRIND_DO_CLIENT_REQUEST_EXPR(-1, VG_USERREQ__SET_PTHREADID,
                                   pthread_self(), 0, 0, 0, 0);

   DRD_(set_joinable)(pthread_self(),
                      arg_copy.detachstate == PTHREAD_CREATE_JOINABLE);

   /*
    * Only set 'wrapper_started' after VG_USERREQ__SET_PTHREADID and
    * DRD_(set_joinable)() have been invoked to avoid a race with
    * a pthread_detach() invocation for this thread from another thread.
    */
   arg_ptr->wrapper_started = 1;

   return (arg_copy.start)(arg_copy.arg);
}

/**
 * Return 1 if the LinuxThreads implementation of POSIX Threads has been
 * detected, and 0 otherwise.
 *
 * @see For more information about the confstr() function, see also
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
   // Make sure that DRD knows about the main thread's POSIX thread ID.
   VALGRIND_DO_CLIENT_REQUEST_EXPR(-1, VG_USERREQ__SET_PTHREADID,
                                   pthread_self(), 0, 0, 0, 0);
}

/*
 * Note: as of today there exist three different versions of pthread_create
 * in Linux:
 * - pthread_create@GLIBC_2.0
 * - pthread_create@@GLIBC_2.1
 * - pthread_create@@GLIBC_2.2.5
 * As an example, in libpthread-2.3.4 both pthread_create@GLIBC_2.0 and
 * pthread_create@@GLIBC_2.1 are defined, while in libpthread-2.9 all three
 * versions have been implemented. In any glibc version where more than one
 * pthread_create function has been implemented, older versions call the
 * newer versions. Or: the pthread_create* wrapper defined below can be
 * called recursively. Any code in this wrapper should take this in account.
 * As an example, it is not safe to invoke the DRD_STOP_RECORDING
 * / DRD_START_RECORDING client requests from the pthread_create wrapper.
 * See also the implementation of pthread_create@GLIBC_2.0 in
 * glibc-2.9/nptl/pthread_create.c.
 */

static __always_inline
int pthread_create_intercept(pthread_t* thread, const pthread_attr_t* attr,
                             void* (*start)(void*), void* arg)
{
   int    ret;
   OrigFn fn;
   DrdPosixThreadArgs thread_args;

   VALGRIND_GET_ORIG_FN(fn);

   thread_args.start           = start;
   thread_args.arg             = arg;
   DRD_IGNORE_VAR(thread_args.wrapper_started);
   thread_args.wrapper_started = 0;
   /*
    * Find out whether the thread will be started as a joinable thread
    * or as a detached thread. If no thread attributes have been specified,
    * this means that the new thread will be started as a joinable thread.
    */
   thread_args.detachstate = PTHREAD_CREATE_JOINABLE;
   if (attr)
   {
      if (pthread_attr_getdetachstate(attr, &thread_args.detachstate) != 0)
         assert(0);
   }
   assert(thread_args.detachstate == PTHREAD_CREATE_JOINABLE
          || thread_args.detachstate == PTHREAD_CREATE_DETACHED);

   DRD_(entering_pthread_create)();
   CALL_FN_W_WWWW(ret, fn, thread, attr, DRD_(thread_wrapper), &thread_args);
   DRD_(left_pthread_create)();

   if (ret == 0)
   {
      /* Wait until the thread wrapper started. */
      while (!thread_args.wrapper_started)
         sched_yield();
   }

   VALGRIND_DO_CLIENT_REQUEST_EXPR(-1, VG_USERREQ__DRD_START_NEW_SEGMENT,
                                   pthread_self(), 0, 0, 0, 0);

   return ret;
}

PTH_FUNCS(int, pthreadZucreate, pthread_create_intercept,
          (pthread_t *thread, const pthread_attr_t *attr,
           void *(*start) (void *), void *arg),
          (thread, attr, start, arg));

static __always_inline
int pthread_join_intercept(pthread_t pt_joinee, void **thread_return)
{
   int      ret;
   OrigFn   fn;

   VALGRIND_GET_ORIG_FN(fn);
   CALL_FN_W_WW(ret, fn, pt_joinee, thread_return);
   if (ret == 0)
   {
      VALGRIND_DO_CLIENT_REQUEST_EXPR(-1, VG_USERREQ__POST_THREAD_JOIN,
                                      pt_joinee, 0, 0, 0, 0);
   }
   return ret;
}

PTH_FUNCS(int, pthreadZujoin, pthread_join_intercept,
          (pthread_t pt_joinee, void **thread_return),
          (pt_joinee, thread_return));

static __always_inline
int pthread_detach_intercept(pthread_t pt_thread)
{
   int ret;
   OrigFn fn;

   VALGRIND_GET_ORIG_FN(fn);
   CALL_FN_W_W(ret, fn, pt_thread);
   DRD_(set_joinable)(pt_thread, 0);

   return ret;
}

PTH_FUNCS(int, pthreadZudetach, pthread_detach_intercept,
          (pthread_t thread), (thread));

// NOTE: be careful to intercept only pthread_cancel() and not
// pthread_cancel_init() on Linux.

static __always_inline
int pthread_cancel_intercept(pthread_t pt_thread)
{
   int ret;
   OrigFn fn;
   VALGRIND_GET_ORIG_FN(fn);
   VALGRIND_DO_CLIENT_REQUEST_EXPR(-1, VG_USERREQ__PRE_THREAD_CANCEL,
                                   pt_thread, 0, 0, 0, 0);
   CALL_FN_W_W(ret, fn, pt_thread);
   VALGRIND_DO_CLIENT_REQUEST_EXPR(-1, VG_USERREQ__POST_THREAD_CANCEL,
                                   pt_thread, ret==0, 0, 0, 0);
   return ret;
}

PTH_FUNCS(int, pthreadZucancel, pthread_cancel_intercept,
          (pthread_t thread), (thread))

static __always_inline
int pthread_once_intercept(pthread_once_t *once_control,
                           void (*init_routine)(void))
{
   int ret;
   OrigFn fn;
   VALGRIND_GET_ORIG_FN(fn);
   /*
    * Ignore any data races triggered by the implementation of pthread_once().
    * Necessary for Darwin. This is not necessary for Linux but doesn't have
    * any known adverse effects.
    */
   DRD_IGNORE_VAR(*once_control);
   CALL_FN_W_WW(ret, fn, once_control, init_routine);
   DRD_STOP_IGNORING_VAR(*once_control);
   return ret;
}

PTH_FUNCS(int, pthreadZuonce, pthread_once_intercept,
          (pthread_once_t *once_control, void (*init_routine)(void)),
          (once_control, init_routine));

static __always_inline
int pthread_mutex_init_intercept(pthread_mutex_t *mutex,
                                 const pthread_mutexattr_t* attr)
{
   int ret;
   OrigFn fn;
   int mt;
   VALGRIND_GET_ORIG_FN(fn);
   mt = PTHREAD_MUTEX_DEFAULT;
   if (attr)
      pthread_mutexattr_gettype(attr, &mt);
   VALGRIND_DO_CLIENT_REQUEST_EXPR(-1, VG_USERREQ__PRE_MUTEX_INIT,
                                   mutex, DRD_(pthread_to_drd_mutex_type)(mt),
                                   0, 0, 0);
   CALL_FN_W_WW(ret, fn, mutex, attr);
   VALGRIND_DO_CLIENT_REQUEST_EXPR(-1, VG_USERREQ__POST_MUTEX_INIT,
                                   mutex, 0, 0, 0, 0);
   return ret;
}

PTH_FUNCS(int, pthreadZumutexZuinit, pthread_mutex_init_intercept,
          (pthread_mutex_t *mutex, const pthread_mutexattr_t* attr),
          (mutex, attr));

static __always_inline
int pthread_mutex_destroy_intercept(pthread_mutex_t* mutex)
{
   int ret;
   OrigFn fn;
   VALGRIND_GET_ORIG_FN(fn);
   VALGRIND_DO_CLIENT_REQUEST_EXPR(-1, VG_USERREQ__PRE_MUTEX_DESTROY,
                                   mutex, 0, 0, 0, 0);
   CALL_FN_W_W(ret, fn, mutex);
   VALGRIND_DO_CLIENT_REQUEST_EXPR(-1, VG_USERREQ__POST_MUTEX_DESTROY,
                                   mutex, DRD_(mutex_type)(mutex), 0, 0, 0);
   return ret;
}

PTH_FUNCS(int, pthreadZumutexZudestroy, pthread_mutex_destroy_intercept,
          (pthread_mutex_t *mutex), (mutex));

static __always_inline
int pthread_mutex_lock_intercept(pthread_mutex_t* mutex)
{
   int   ret;
   OrigFn fn;
   VALGRIND_GET_ORIG_FN(fn);
   VALGRIND_DO_CLIENT_REQUEST_EXPR(0, VG_USERREQ__PRE_MUTEX_LOCK,
                                   mutex, DRD_(mutex_type)(mutex), 0, 0, 0);
   CALL_FN_W_W(ret, fn, mutex);
   VALGRIND_DO_CLIENT_REQUEST_EXPR(0, VG_USERREQ__POST_MUTEX_LOCK,
                                   mutex, ret == 0, 0, 0, 0);
   return ret;
}

PTH_FUNCS(int, pthreadZumutexZulock, pthread_mutex_lock_intercept,
          (pthread_mutex_t *mutex), (mutex));

static __always_inline
int pthread_mutex_trylock_intercept(pthread_mutex_t* mutex)
{
   int   ret;
   OrigFn fn;
   VALGRIND_GET_ORIG_FN(fn);
   VALGRIND_DO_CLIENT_REQUEST_EXPR(0, VG_USERREQ__PRE_MUTEX_LOCK,
                                   mutex, DRD_(mutex_type)(mutex), 1, 0, 0);
   CALL_FN_W_W(ret, fn, mutex);
   VALGRIND_DO_CLIENT_REQUEST_EXPR(-1, VG_USERREQ__POST_MUTEX_LOCK,
                                   mutex, ret == 0, 0, 0, 0);
   return ret;
}

PTH_FUNCS(int, pthreadZumutexZutrylock, pthread_mutex_trylock_intercept,
          (pthread_mutex_t *mutex), (mutex));

static __always_inline
int pthread_mutex_timedlock_intercept(pthread_mutex_t *mutex,
                                      const struct timespec *abs_timeout)
{
   int   ret;
   OrigFn fn;
   VALGRIND_GET_ORIG_FN(fn);
   VALGRIND_DO_CLIENT_REQUEST_EXPR(0, VG_USERREQ__PRE_MUTEX_LOCK,
                                   mutex, DRD_(mutex_type)(mutex), 0, 0, 0);
   CALL_FN_W_WW(ret, fn, mutex, abs_timeout);
   VALGRIND_DO_CLIENT_REQUEST_EXPR(-1, VG_USERREQ__POST_MUTEX_LOCK,
                                   mutex, ret == 0, 0, 0, 0);
   return ret;
}

PTH_FUNCS(int, pthreadZumutexZutimedlock, pthread_mutex_timedlock_intercept,
          (pthread_mutex_t *mutex, const struct timespec *abs_timeout),
          (mutex, abs_timeout));

static __always_inline
int pthread_mutex_unlock_intercept(pthread_mutex_t *mutex)
{
   int ret;
   OrigFn fn;
   VALGRIND_GET_ORIG_FN(fn);
   VALGRIND_DO_CLIENT_REQUEST_EXPR(-1, VG_USERREQ__PRE_MUTEX_UNLOCK,
                                   mutex, DRD_(mutex_type)(mutex), 0, 0, 0);
   CALL_FN_W_W(ret, fn, mutex);
   VALGRIND_DO_CLIENT_REQUEST_EXPR(-1, VG_USERREQ__POST_MUTEX_UNLOCK,
                                   mutex, 0, 0, 0, 0);
   return ret;
}

PTH_FUNCS(int, pthreadZumutexZuunlock, pthread_mutex_unlock_intercept,
          (pthread_mutex_t *mutex), (mutex));

static __always_inline
int pthread_cond_init_intercept(pthread_cond_t* cond,
                                const pthread_condattr_t* attr)
{
   int ret;
   OrigFn fn;
   VALGRIND_GET_ORIG_FN(fn);
   VALGRIND_DO_CLIENT_REQUEST_EXPR(-1, VG_USERREQ__PRE_COND_INIT,
                                   cond, 0, 0, 0, 0);
   CALL_FN_W_WW(ret, fn, cond, attr);
   VALGRIND_DO_CLIENT_REQUEST_EXPR(-1, VG_USERREQ__POST_COND_INIT,
                                   cond, 0, 0, 0, 0);
   return ret;
}

PTH_FUNCS(int, pthreadZucondZuinit, pthread_cond_init_intercept,
          (pthread_cond_t* cond, const pthread_condattr_t* attr),
          (cond, attr));

static __always_inline
int pthread_cond_destroy_intercept(pthread_cond_t* cond)
{
   int ret;
   OrigFn fn;
   VALGRIND_GET_ORIG_FN(fn);
   VALGRIND_DO_CLIENT_REQUEST_EXPR(-1, VG_USERREQ__PRE_COND_DESTROY,
                                   cond, 0, 0, 0, 0);
   CALL_FN_W_W(ret, fn, cond);
   VALGRIND_DO_CLIENT_REQUEST_EXPR(-1, VG_USERREQ__POST_COND_DESTROY,
                                   cond, 0, 0, 0, 0);
   return ret;
}

PTH_FUNCS(int, pthreadZucondZudestroy, pthread_cond_destroy_intercept,
          (pthread_cond_t* cond), (cond));

static __always_inline
int pthread_cond_wait_intercept(pthread_cond_t *cond, pthread_mutex_t *mutex)
{
   int   ret;
   OrigFn fn;
   VALGRIND_GET_ORIG_FN(fn);
   VALGRIND_DO_CLIENT_REQUEST_EXPR(-1, VG_USERREQ__PRE_COND_WAIT,
                                   cond, mutex, DRD_(mutex_type)(mutex), 0, 0);
   CALL_FN_W_WW(ret, fn, cond, mutex);
   VALGRIND_DO_CLIENT_REQUEST_EXPR(-1, VG_USERREQ__POST_COND_WAIT,
                                   cond, mutex, 1, 0, 0);
   return ret;
}

PTH_FUNCS(int, pthreadZucondZuwait, pthread_cond_wait_intercept,
          (pthread_cond_t *cond, pthread_mutex_t *mutex),
          (cond, mutex));

static __always_inline
int pthread_cond_timedwait_intercept(pthread_cond_t *cond,
                                     pthread_mutex_t *mutex,
                                     const struct timespec* abstime)
{
   int   ret;
   OrigFn fn;
   VALGRIND_GET_ORIG_FN(fn);
   VALGRIND_DO_CLIENT_REQUEST_EXPR(-1, VG_USERREQ__PRE_COND_WAIT,
                                   cond, mutex, DRD_(mutex_type)(mutex), 0, 0);
   CALL_FN_W_WWW(ret, fn, cond, mutex, abstime);
   VALGRIND_DO_CLIENT_REQUEST_EXPR(-1, VG_USERREQ__POST_COND_WAIT,
                                   cond, mutex, 1, 0, 0);
   return ret;
}

PTH_FUNCS(int, pthreadZucondZutimedwait, pthread_cond_timedwait_intercept,
          (pthread_cond_t *cond, pthread_mutex_t *mutex,
           const struct timespec* abstime),
          (cond, mutex, abstime));

// NOTE: be careful to intercept only pthread_cond_signal() and not Darwin's
// pthread_cond_signal_thread_np(). The former accepts one argument; the latter
// two. Intercepting all pthread_cond_signal* functions will cause only one
// argument to be passed to pthread_cond_signal_np() and hence will cause this
// last function to crash.

static __always_inline
int pthread_cond_signal_intercept(pthread_cond_t* cond)
{
   int   ret;
   OrigFn fn;
   VALGRIND_GET_ORIG_FN(fn);
   VALGRIND_DO_CLIENT_REQUEST_EXPR(-1, VG_USERREQ__PRE_COND_SIGNAL,
                                   cond, 0, 0, 0, 0);
   CALL_FN_W_W(ret, fn, cond);
   VALGRIND_DO_CLIENT_REQUEST_EXPR(-1, VG_USERREQ__POST_COND_SIGNAL,
                                   cond, 0, 0, 0, 0);
   return ret;
}

PTH_FUNCS(int, pthreadZucondZusignal, pthread_cond_signal_intercept,
          (pthread_cond_t* cond), (cond));

static __always_inline
int pthread_cond_broadcast_intercept(pthread_cond_t* cond)
{
   int   ret;
   OrigFn fn;
   VALGRIND_GET_ORIG_FN(fn);
   VALGRIND_DO_CLIENT_REQUEST_EXPR(-1, VG_USERREQ__PRE_COND_BROADCAST,
                                   cond, 0, 0, 0, 0);
   CALL_FN_W_W(ret, fn, cond);
   VALGRIND_DO_CLIENT_REQUEST_EXPR(-1, VG_USERREQ__POST_COND_BROADCAST,
                                   cond, 0, 0, 0, 0);
   return ret;
}

PTH_FUNCS(int, pthreadZucondZubroadcast, pthread_cond_broadcast_intercept,
          (pthread_cond_t* cond), (cond));

#if defined(HAVE_PTHREAD_SPIN_LOCK)
static __always_inline
int pthread_spin_init_intercept(pthread_spinlock_t *spinlock, int pshared)
{
   int ret;
   OrigFn fn;
   VALGRIND_GET_ORIG_FN(fn);
   VALGRIND_DO_CLIENT_REQUEST_EXPR(-1, VG_USERREQ__PRE_SPIN_INIT_OR_UNLOCK,
                                   spinlock, 0, 0, 0, 0);
   CALL_FN_W_WW(ret, fn, spinlock, pshared);
   VALGRIND_DO_CLIENT_REQUEST_EXPR(-1, VG_USERREQ__POST_SPIN_INIT_OR_UNLOCK,
                                   spinlock, 0, 0, 0, 0);
   return ret;
}

PTH_FUNCS(int, pthreadZuspinZuinit, pthread_spin_init_intercept,
          (pthread_spinlock_t *spinlock, int pshared), (spinlock, pshared));

static __always_inline
int pthread_spin_destroy_intercept(pthread_spinlock_t *spinlock)
{
   int ret;
   OrigFn fn;
   VALGRIND_GET_ORIG_FN(fn);
   VALGRIND_DO_CLIENT_REQUEST_EXPR(-1, VG_USERREQ__PRE_MUTEX_DESTROY,
                                   spinlock, 0, 0, 0, 0);
   CALL_FN_W_W(ret, fn, spinlock);
   VALGRIND_DO_CLIENT_REQUEST_EXPR(-1, VG_USERREQ__POST_MUTEX_DESTROY,
                                   spinlock, mutex_type_spinlock, 0, 0, 0);
   return ret;
}

PTH_FUNCS(int, pthreadZuspinZudestroy, pthread_spin_destroy_intercept,
          (pthread_spinlock_t *spinlock), (spinlock));

static __always_inline
int pthread_spin_lock_intercept(pthread_spinlock_t *spinlock)
{
   int   ret;
   OrigFn fn;
   VALGRIND_GET_ORIG_FN(fn);
   VALGRIND_DO_CLIENT_REQUEST_EXPR(0, VG_USERREQ__PRE_MUTEX_LOCK,
                                   spinlock, mutex_type_spinlock, 0, 0, 0);
   CALL_FN_W_W(ret, fn, spinlock);
   VALGRIND_DO_CLIENT_REQUEST_EXPR(-1, VG_USERREQ__POST_MUTEX_LOCK,
                                   spinlock, ret == 0, 0, 0, 0);
   return ret;
}

PTH_FUNCS(int, pthreadZuspinZulock, pthread_spin_lock_intercept,
          (pthread_spinlock_t *spinlock), (spinlock));

static __always_inline
int pthread_spin_trylock_intercept(pthread_spinlock_t *spinlock)
{
   int   ret;
   OrigFn fn;
   VALGRIND_GET_ORIG_FN(fn);
   VALGRIND_DO_CLIENT_REQUEST_EXPR(0, VG_USERREQ__PRE_MUTEX_LOCK,
                                   spinlock, mutex_type_spinlock, 0, 0, 0);
   CALL_FN_W_W(ret, fn, spinlock);
   VALGRIND_DO_CLIENT_REQUEST_EXPR(-1, VG_USERREQ__POST_MUTEX_LOCK,
                                   spinlock, ret == 0, 0, 0, 0);
   return ret;
}

PTH_FUNCS(int, pthreadZuspinZutrylock, pthread_spin_trylock_intercept,
          (pthread_spinlock_t *spinlock), (spinlock));

static __always_inline
int pthread_spin_unlock_intercept(pthread_spinlock_t *spinlock)
{
   int   ret;
   OrigFn fn;
   VALGRIND_GET_ORIG_FN(fn);
   VALGRIND_DO_CLIENT_REQUEST_EXPR(-1, VG_USERREQ__PRE_SPIN_INIT_OR_UNLOCK,
                                   spinlock, mutex_type_spinlock, 0, 0, 0);
   CALL_FN_W_W(ret, fn, spinlock);
   VALGRIND_DO_CLIENT_REQUEST_EXPR(-1, VG_USERREQ__POST_SPIN_INIT_OR_UNLOCK,
                                   spinlock, 0, 0, 0, 0);
   return ret;
}

PTH_FUNCS(int, pthreadZuspinZuunlock, pthread_spin_unlock_intercept,
          (pthread_spinlock_t *spinlock), (spinlock));
#endif   // HAVE_PTHREAD_SPIN_LOCK


#if defined(HAVE_PTHREAD_BARRIER_INIT)
static __always_inline
int pthread_barrier_init_intercept(pthread_barrier_t* barrier,
                                   const pthread_barrierattr_t* attr,
                                   unsigned count)
{
   int   ret;
   OrigFn fn;
   VALGRIND_GET_ORIG_FN(fn);
   VALGRIND_DO_CLIENT_REQUEST_EXPR(-1, VG_USERREQ__PRE_BARRIER_INIT,
                                   barrier, pthread_barrier, count, 0, 0);
   CALL_FN_W_WWW(ret, fn, barrier, attr, count);
   VALGRIND_DO_CLIENT_REQUEST_EXPR(-1, VG_USERREQ__POST_BARRIER_INIT,
                                   barrier, pthread_barrier, 0, 0, 0);
   return ret;
}

PTH_FUNCS(int, pthreadZubarrierZuinit, pthread_barrier_init_intercept,
          (pthread_barrier_t* barrier, const pthread_barrierattr_t* attr,
           unsigned count), (barrier, attr, count));

static __always_inline
int pthread_barrier_destroy_intercept(pthread_barrier_t* barrier)
{
   int   ret;
   OrigFn fn;
   VALGRIND_GET_ORIG_FN(fn);
   VALGRIND_DO_CLIENT_REQUEST_EXPR(-1, VG_USERREQ__PRE_BARRIER_DESTROY,
                                   barrier, pthread_barrier, 0, 0, 0);
   CALL_FN_W_W(ret, fn, barrier);
   VALGRIND_DO_CLIENT_REQUEST_EXPR(-1, VG_USERREQ__POST_BARRIER_DESTROY,
                                   barrier, pthread_barrier, 0, 0, 0);
   return ret;
}

PTH_FUNCS(int, pthreadZubarrierZudestroy, pthread_barrier_destroy_intercept,
          (pthread_barrier_t* barrier), (barrier));

static __always_inline
int pthread_barrier_wait_intercept(pthread_barrier_t* barrier)
{
   int   ret;
   OrigFn fn;
   VALGRIND_GET_ORIG_FN(fn);
   VALGRIND_DO_CLIENT_REQUEST_EXPR(-1, VG_USERREQ__PRE_BARRIER_WAIT,
                                   barrier, pthread_barrier, 0, 0, 0);
   CALL_FN_W_W(ret, fn, barrier);
   VALGRIND_DO_CLIENT_REQUEST_EXPR(-1, VG_USERREQ__POST_BARRIER_WAIT,
                              barrier, pthread_barrier,
                              ret == 0 || ret == PTHREAD_BARRIER_SERIAL_THREAD,
                              ret == PTHREAD_BARRIER_SERIAL_THREAD, 0);
   return ret;
}

PTH_FUNCS(int, pthreadZubarrierZuwait, pthread_barrier_wait_intercept,
          (pthread_barrier_t* barrier), (barrier));
#endif   // HAVE_PTHREAD_BARRIER_INIT


static __always_inline
int sem_init_intercept(sem_t *sem, int pshared, unsigned int value)
{
   int   ret;
   OrigFn fn;
   VALGRIND_GET_ORIG_FN(fn);
   VALGRIND_DO_CLIENT_REQUEST_EXPR(-1, VG_USERREQ__PRE_SEM_INIT,
                                   sem, pshared, value, 0, 0);
   CALL_FN_W_WWW(ret, fn, sem, pshared, value);
   VALGRIND_DO_CLIENT_REQUEST_EXPR(-1, VG_USERREQ__POST_SEM_INIT,
                                   sem, 0, 0, 0, 0);
   return ret;
}

PTH_FUNCS(int, semZuinit, sem_init_intercept,
          (sem_t *sem, int pshared, unsigned int value), (sem, pshared, value));

static __always_inline
int sem_destroy_intercept(sem_t *sem)
{
   int   ret;
   OrigFn fn;
   VALGRIND_GET_ORIG_FN(fn);
   VALGRIND_DO_CLIENT_REQUEST_EXPR(-1, VG_USERREQ__PRE_SEM_DESTROY,
                                   sem, 0, 0, 0, 0);
   CALL_FN_W_W(ret, fn, sem);
   VALGRIND_DO_CLIENT_REQUEST_EXPR(-1, VG_USERREQ__POST_SEM_DESTROY,
                                   sem, 0, 0, 0, 0);
   return ret;
}

PTH_FUNCS(int, semZudestroy, sem_destroy_intercept, (sem_t *sem), (sem));

static __always_inline
sem_t* sem_open_intercept(const char *name, int oflag, mode_t mode,
                          unsigned int value)
{
   sem_t *ret;
   OrigFn fn;
   VALGRIND_GET_ORIG_FN(fn);
   VALGRIND_DO_CLIENT_REQUEST_EXPR(-1, VG_USERREQ__PRE_SEM_OPEN,
                                   name, oflag, mode, value, 0);
   CALL_FN_W_WWWW(ret, fn, name, oflag, mode, value);
   VALGRIND_DO_CLIENT_REQUEST_EXPR(-1, VG_USERREQ__POST_SEM_OPEN,
                                   ret != SEM_FAILED ? ret : 0,
                                   name, oflag, mode, value);
   return ret;
}

PTH_FUNCS(sem_t *, semZuopen, sem_open_intercept,
          (const char *name, int oflag, mode_t mode, unsigned int value),
          (name, oflag, mode, value));

static __always_inline int sem_close_intercept(sem_t *sem)
{
   int   ret;
   OrigFn fn;
   VALGRIND_GET_ORIG_FN(fn);
   VALGRIND_DO_CLIENT_REQUEST_EXPR(-1, VG_USERREQ__PRE_SEM_CLOSE,
                                   sem, 0, 0, 0, 0);
   CALL_FN_W_W(ret, fn, sem);
   VALGRIND_DO_CLIENT_REQUEST_EXPR(-1, VG_USERREQ__POST_SEM_CLOSE,
                                   sem, 0, 0, 0, 0);
   return ret;
}

PTH_FUNCS(int, semZuclose, sem_close_intercept, (sem_t *sem), (sem));

static __always_inline int sem_wait_intercept(sem_t *sem)
{
   int   ret;
   OrigFn fn;
   VALGRIND_GET_ORIG_FN(fn);
   VALGRIND_DO_CLIENT_REQUEST_EXPR(-1, VG_USERREQ__PRE_SEM_WAIT,
                                   sem, 0, 0, 0, 0);
   CALL_FN_W_W(ret, fn, sem);
   VALGRIND_DO_CLIENT_REQUEST_EXPR(-1, VG_USERREQ__POST_SEM_WAIT,
                                   sem, ret == 0, 0, 0, 0);
   return ret;
}

PTH_FUNCS(int, semZuwait, sem_wait_intercept, (sem_t *sem), (sem));

static __always_inline int sem_trywait_intercept(sem_t *sem)
{
   int   ret;
   OrigFn fn;
   VALGRIND_GET_ORIG_FN(fn);
   VALGRIND_DO_CLIENT_REQUEST_EXPR(-1, VG_USERREQ__PRE_SEM_WAIT,
                                   sem, 0, 0, 0, 0);
   CALL_FN_W_W(ret, fn, sem);
   VALGRIND_DO_CLIENT_REQUEST_EXPR(-1, VG_USERREQ__POST_SEM_WAIT,
                                   sem, ret == 0, 0, 0, 0);
   return ret;
}

PTH_FUNCS(int, semZutrywait, sem_trywait_intercept, (sem_t *sem), (sem));

static __always_inline
int sem_timedwait_intercept(sem_t *sem, const struct timespec *abs_timeout)
{
   int   ret;
   OrigFn fn;
   VALGRIND_GET_ORIG_FN(fn);
   VALGRIND_DO_CLIENT_REQUEST_EXPR(-1, VG_USERREQ__PRE_SEM_WAIT,
                                   sem, 0, 0, 0, 0);
   CALL_FN_W_WW(ret, fn, sem, abs_timeout);
   VALGRIND_DO_CLIENT_REQUEST_EXPR(-1, VG_USERREQ__POST_SEM_WAIT,
                                   sem, ret == 0, 0, 0, 0);
   return ret;
}

PTH_FUNCS(int, semZutimedwait, sem_timedwait_intercept,
          (sem_t *sem, const struct timespec *abs_timeout),
          (sem, abs_timeout));

static __always_inline int sem_post_intercept(sem_t *sem)
{
   int   ret;
   OrigFn fn;
   VALGRIND_GET_ORIG_FN(fn);
   VALGRIND_DO_CLIENT_REQUEST_EXPR(-1, VG_USERREQ__PRE_SEM_POST,
                                   sem, 0, 0, 0, 0);
   CALL_FN_W_W(ret, fn, sem);
   VALGRIND_DO_CLIENT_REQUEST_EXPR(-1, VG_USERREQ__POST_SEM_POST,
                                   sem, ret == 0, 0, 0, 0);
   return ret;
}

PTH_FUNCS(int, semZupost, sem_post_intercept, (sem_t *sem), (sem));

static __always_inline
int pthread_rwlock_init_intercept(pthread_rwlock_t* rwlock,
                                  const pthread_rwlockattr_t* attr)
{
   int   ret;
   OrigFn fn;
   VALGRIND_GET_ORIG_FN(fn);
   VALGRIND_DO_CLIENT_REQUEST_EXPR(-1, VG_USERREQ__PRE_RWLOCK_INIT,
                                   rwlock, 0, 0, 0, 0);
   CALL_FN_W_WW(ret, fn, rwlock, attr);
   return ret;
}

PTH_FUNCS(int,
          pthreadZurwlockZuinit, pthread_rwlock_init_intercept,
          (pthread_rwlock_t* rwlock, const pthread_rwlockattr_t* attr),
          (rwlock, attr));

static __always_inline
int pthread_rwlock_destroy_intercept(pthread_rwlock_t* rwlock)
{
   int   ret;
   OrigFn fn;
   VALGRIND_GET_ORIG_FN(fn);
   CALL_FN_W_W(ret, fn, rwlock);
   VALGRIND_DO_CLIENT_REQUEST_EXPR(-1, VG_USERREQ__POST_RWLOCK_DESTROY,
                                   rwlock, 0, 0, 0, 0);
   return ret;
}

PTH_FUNCS(int,
          pthreadZurwlockZudestroy, pthread_rwlock_destroy_intercept,
          (pthread_rwlock_t* rwlock), (rwlock));

static __always_inline
int pthread_rwlock_rdlock_intercept(pthread_rwlock_t* rwlock)
{
   int   ret;
   OrigFn fn;
   VALGRIND_GET_ORIG_FN(fn);
   VALGRIND_DO_CLIENT_REQUEST_EXPR(-1, VG_USERREQ__PRE_RWLOCK_RDLOCK,
                                   rwlock, 0, 0, 0, 0);
   CALL_FN_W_W(ret, fn, rwlock);
   VALGRIND_DO_CLIENT_REQUEST_EXPR(-1, VG_USERREQ__POST_RWLOCK_RDLOCK,
                                   rwlock, ret == 0, 0, 0, 0);
   return ret;
}

PTH_FUNCS(int,
          pthreadZurwlockZurdlock, pthread_rwlock_rdlock_intercept,
          (pthread_rwlock_t* rwlock), (rwlock));

static __always_inline
int pthread_rwlock_wrlock_intercept(pthread_rwlock_t* rwlock)
{
   int   ret;
   OrigFn fn;
   VALGRIND_GET_ORIG_FN(fn);
   VALGRIND_DO_CLIENT_REQUEST_EXPR(-1, VG_USERREQ__PRE_RWLOCK_WRLOCK,
                                   rwlock, 0, 0, 0, 0);
   CALL_FN_W_W(ret, fn, rwlock);
   VALGRIND_DO_CLIENT_REQUEST_EXPR(-1, VG_USERREQ__POST_RWLOCK_WRLOCK,
                                   rwlock, ret == 0, 0, 0, 0);
   return ret;
}

PTH_FUNCS(int,
          pthreadZurwlockZuwrlock, pthread_rwlock_wrlock_intercept,
          (pthread_rwlock_t* rwlock), (rwlock));

static __always_inline
int pthread_rwlock_timedrdlock_intercept(pthread_rwlock_t* rwlock)
{
   int   ret;
   OrigFn fn;
   VALGRIND_GET_ORIG_FN(fn);
   VALGRIND_DO_CLIENT_REQUEST_EXPR(-1, VG_USERREQ__PRE_RWLOCK_RDLOCK,
                                   rwlock, 0, 0, 0, 0);
   CALL_FN_W_W(ret, fn, rwlock);
   VALGRIND_DO_CLIENT_REQUEST_EXPR(-1, VG_USERREQ__POST_RWLOCK_RDLOCK,
                                   rwlock, ret == 0, 0, 0, 0);
   return ret;
}

PTH_FUNCS(int,
          pthreadZurwlockZutimedrdlock, pthread_rwlock_timedrdlock_intercept,
          (pthread_rwlock_t* rwlock), (rwlock));

static __always_inline
int pthread_rwlock_timedwrlock_intercept(pthread_rwlock_t* rwlock)
{
   int   ret;
   OrigFn fn;
   VALGRIND_GET_ORIG_FN(fn);
   VALGRIND_DO_CLIENT_REQUEST_EXPR(-1, VG_USERREQ__PRE_RWLOCK_WRLOCK,
                                   rwlock, 0, 0, 0, 0);
   CALL_FN_W_W(ret, fn, rwlock);
   VALGRIND_DO_CLIENT_REQUEST_EXPR(-1, VG_USERREQ__POST_RWLOCK_WRLOCK,
                                   rwlock, ret == 0, 0, 0, 0);
   return ret;
}

PTH_FUNCS(int,
          pthreadZurwlockZutimedwrlock, pthread_rwlock_timedwrlock_intercept,
          (pthread_rwlock_t* rwlock), (rwlock));

static __always_inline
int pthread_rwlock_tryrdlock_intercept(pthread_rwlock_t* rwlock)
{
   int   ret;
   OrigFn fn;
   VALGRIND_GET_ORIG_FN(fn);
   VALGRIND_DO_CLIENT_REQUEST_EXPR(-1, VG_USERREQ__PRE_RWLOCK_RDLOCK,
                                   rwlock, 0, 0, 0, 0);
   CALL_FN_W_W(ret, fn, rwlock);
   VALGRIND_DO_CLIENT_REQUEST_EXPR(-1, VG_USERREQ__POST_RWLOCK_RDLOCK,
                                   rwlock, ret == 0, 0, 0, 0);
   return ret;
}

PTH_FUNCS(int,
          pthreadZurwlockZutryrdlock, pthread_rwlock_tryrdlock_intercept,
          (pthread_rwlock_t* rwlock), (rwlock));

static __always_inline
int pthread_rwlock_trywrlock_intercept(pthread_rwlock_t* rwlock)
{
   int   ret;
   OrigFn fn;
   VALGRIND_GET_ORIG_FN(fn);
   VALGRIND_DO_CLIENT_REQUEST_EXPR(-1, VG_USERREQ__PRE_RWLOCK_WRLOCK,
                                   rwlock, 0, 0, 0, 0);
   CALL_FN_W_W(ret, fn, rwlock);
   VALGRIND_DO_CLIENT_REQUEST_EXPR(-1, VG_USERREQ__POST_RWLOCK_WRLOCK,
                                   rwlock, ret == 0, 0, 0, 0);
   return ret;
}

PTH_FUNCS(int,
          pthreadZurwlockZutrywrlock, pthread_rwlock_trywrlock_intercept,
          (pthread_rwlock_t* rwlock), (rwlock));

static __always_inline
int pthread_rwlock_unlock_intercept(pthread_rwlock_t* rwlock)
{
   int   ret;
   OrigFn fn;
   VALGRIND_GET_ORIG_FN(fn);
   VALGRIND_DO_CLIENT_REQUEST_EXPR(-1, VG_USERREQ__PRE_RWLOCK_UNLOCK,
                                   rwlock, 0, 0, 0, 0);
   CALL_FN_W_W(ret, fn, rwlock);
   VALGRIND_DO_CLIENT_REQUEST_EXPR(-1, VG_USERREQ__POST_RWLOCK_UNLOCK,
                                   rwlock, ret == 0, 0, 0, 0);
   return ret;
}

PTH_FUNCS(int,
          pthreadZurwlockZuunlock, pthread_rwlock_unlock_intercept,
          (pthread_rwlock_t* rwlock), (rwlock));
