/*--------------------------------------------------------------------*/
/*--- Client-space code for DRD.          drd_pthread_intercepts.c ---*/
/*--------------------------------------------------------------------*/

/*
  This file is part of DRD, a thread error detector.

  Copyright (C) 2006-2020 Bart Van Assche <bvanassche@acm.org>.

  This program is free software; you can redistribute it and/or
  modify it under the terms of the GNU General Public License as
  published by the Free Software Foundation; either version 2 of the
  License, or (at your option) any later version.

  This program is distributed in the hope that it will be useful, but
  WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
  General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with this program; if not, see <http://www.gnu.org/licenses/>.

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
#include <errno.h>
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

#if defined(VGO_freebsd)
#include <dlfcn.h>
#endif

#if defined(VGO_solaris)
/*
 * Solaris usually provides pthread_* functions on top of Solaris threading
 * and synchronization functions. Usually both need to be intercepted because
 * pthread_* ones might not call the Solaris ones (see for example sem_wait()).
 * Such approach is required to correctly report misuse of the POSIX threads
 * API.
 * Therefore DRD intercepts and instruments all such functions but due to
 * DRD_(thread_enter_synchr)() and DRD_(thread_leave_synchr)() guards in
 * handle_client_request(), only the top-most function is handled.
 * So the right thing(TM) happens, as expected.
 * The only exception is when pthread_* function is a weak alias to the Solaris
 * threading/synchronization function. In such case only one needs to be
 * intercepted to avoid redirection ambiguity.
 *
 * Intercepted functions rely on the fact that:
 *  - pthread_mutex_t  == mutex_t
 *  - pthread_cond_t   == cond_t
 *  - sem_t            == sema_t
 *  - pthread_rwlock_t == rwlock_t
 *
 * It is necessary to intercept also internal libc synchronization functions
 * for two reasons:
 *  - For read-write locks the unlocking function is shared
 *  - Functions lmutex_lock/lmutex_unlock guard many critical sections in libc
 *    which will be otherwise reported by DRD
 */
#include <synch.h>
#include <thread.h>
#include "pub_tool_vki.h"

/*
 * Solaris provides higher throughput, parallelism and scalability than other
 * operating systems, at the cost of more fine-grained locking activity.
 * This means for example that when a thread is created under Linux, just one
 * big lock in glibc is used for all thread setup. Solaris libc uses several
 * fine-grained locks and the creator thread resumes its activities as soon
 * as possible, leaving for example stack and TLS setup activities to the
 * created thread.
 *
 * This situation confuses DRD as it assumes there is some false ordering
 * in place between creator and created thread; and therefore many types of
 * race conditions in the application would not be reported. To prevent such
 * false ordering, command line option --ignore-thread-creation is set to
 * 'yes' by default on Solaris. All activity (loads, stores, client requests)
 * is therefore ignored during:
 * - pthread_create() call in the creator thread [libc.so]
 * - thread creation phase (stack and TLS setup) in the created thread [libc.so]
 *
 * As explained in the comments for _ti_bind_guard(), whenever the runtime
 * linker has to perform any activity (such as resolving a symbol), it protects
 * its data structures by calling into rt_bind_guard() which in turn invokes
 * _ti_bind_guard() in libc. Pointers to _ti_bind_guard() and _ti_bind_clear()
 * are passed from libc to runtime linker in _ld_libc() call during libc_init().
 * All activity is also ignored during:
 * - runtime dynamic linker work between rt_bind_guard() and rt_bind_clear()
 *   calls [ld.so]
 *
 * This also means that DRD does not report race conditions in libc (when
 * --ignore-thread-creation=yes) and runtime linker itself (unconditionally)
 * during these ignored sequences.
 */

/*
 * Original function pointers for _ti_bind_guard() and _ti_bind_clear()
 * from libc. They are intercepted in function wrapper of _ld_libc().
 */
typedef int (*drd_rtld_guard_fn)(int flags);
static drd_rtld_guard_fn DRD_(rtld_bind_guard) = NULL;
static drd_rtld_guard_fn DRD_(rtld_bind_clear) = NULL;
#endif


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
#if defined(VGO_darwin)
/*
 * Note here VGO_darwin is used rather than VG_WRAP_THREAD_FUNCTION_LIBPTHREAD_ONLY
 * because of the special-case code adding a function call
 */
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
#elif defined(VG_WRAP_THREAD_FUNCTION_LIBPTHREAD_ONLY)
#define PTH_FUNC(ret_ty, zf, implf, argl_decl, argl)                    \
ret_ty VG_WRAP_FUNCTION_ZZ(VG_Z_LIBPTHREAD_SONAME,zf) argl_decl;        \
ret_ty VG_WRAP_FUNCTION_ZZ(VG_Z_LIBPTHREAD_SONAME,zf) argl_decl         \
{	return implf argl; }
#elif defined(VG_WRAP_THREAD_FUNCTION_LIBC_ONLY)
#define PTH_FUNC(ret_ty, zf, implf, argl_decl, argl)                    \
   ret_ty VG_WRAP_FUNCTION_ZZ(VG_Z_LIBC_SONAME,zf) argl_decl;           \
   ret_ty VG_WRAP_FUNCTION_ZZ(VG_Z_LIBC_SONAME,zf) argl_decl            \
   { return implf argl; }
#elif defined(VG_WRAP_THREAD_FUNCTION_LIBC_AND_LIBPTHREAD)
#define PTH_FUNC(ret_ty, zf, implf, argl_decl, argl)                    \
   ret_ty VG_WRAP_FUNCTION_ZZ(VG_Z_LIBC_SONAME,zf) argl_decl;           \
   ret_ty VG_WRAP_FUNCTION_ZZ(VG_Z_LIBC_SONAME,zf) argl_decl            \
   { return implf argl; }                                               \
   ret_ty VG_WRAP_FUNCTION_ZZ(VG_Z_LIBPTHREAD_SONAME,zf) argl_decl;     \
   ret_ty VG_WRAP_FUNCTION_ZZ(VG_Z_LIBPTHREAD_SONAME,zf) argl_decl      \
   { return implf argl; }
#else
#  error "Unknown platform/thread wrapping"
#endif

#if defined(VGO_freebsd)
#define LIBC_FUNC(ret_ty, zf, implf, argl_decl, argl)                    \
   ret_ty VG_WRAP_FUNCTION_ZZ(VG_Z_LIBC_SONAME,zf) argl_decl;           \
   ret_ty VG_WRAP_FUNCTION_ZZ(VG_Z_LIBC_SONAME,zf) argl_decl            \
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

typedef struct {
   pthread_mutex_t mutex;
   pthread_cond_t cond;
   int counter;
} DrdSema;

typedef struct
{
   void* (*start)(void*);
   void* arg;
   int   detachstate;
   DrdSema* wrapper_started;
} DrdPosixThreadArgs;


/* Local function declarations. */

static void DRD_(init)(void) __attribute__((constructor));
static void DRD_(check_threading_library)(void);
static void DRD_(set_pthread_id)(void);
static void DRD_(sema_init)(DrdSema* sema);
static void DRD_(sema_destroy)(DrdSema* sema);
static void DRD_(sema_down)(DrdSema* sema);
static void DRD_(sema_up)(DrdSema* sema);


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
#if defined(VGO_freebsd)
   {
      /*
       * On FreeBSD, pthead functions are all in libthr.so
       * However libc.so contains stubs. In this ctor function,
       * calling DRD_(set_pthread_id)() results in a call to
       * pthread_self() resolving to the libc.so stub which
       * returns a junk value for the tid. Subsequent calls
       * to pthread_create() then also cause calls to
       * DRD_(set_pthread_id)(), but this time with pthread_self()
       * resolving to the good libthr.so version (since this is later
       * and libthr.so has been loaded). That causes an assert
       * since we expect the tid to either be INVALID_POSIX_THREADID
       * or the same as the current tid, and the junk value
       * is neither. So we force loading of libthr.so, which
       * avoids this junk tid value.
       */
      dlclose(dlopen("/lib/libthr.so.3", RTLD_NOW|RTLD_GLOBAL|RTLD_NODELETE));
   }
#endif

   DRD_(check_threading_library)();
   DRD_(set_pthread_id)();
#if defined(VGO_solaris)
   if ((DRD_(rtld_bind_guard) == NULL) || (DRD_(rtld_bind_clear) == NULL)) {
      fprintf(stderr,
"Bind guard functions for the runtime linker (ld.so.1) were not intercepted.\n"
"This means the interface between libc and runtime linker changed and DRD\n"
"needs to be ported properly. Giving up.\n");
      abort();
   }
#endif
}

static __always_inline void DRD_(ignore_mutex_ordering)(pthread_mutex_t *mutex)
{
   VALGRIND_DO_CLIENT_REQUEST_STMT(VG_USERREQ__DRD_IGNORE_MUTEX_ORDERING,
                                   mutex, 0, 0, 0, 0);
}

static void DRD_(sema_init)(DrdSema* sema)
{
   DRD_IGNORE_VAR(*sema);
   pthread_mutex_init(&sema->mutex, NULL);
   DRD_(ignore_mutex_ordering)(&sema->mutex);
   pthread_cond_init(&sema->cond, NULL);
   sema->counter = 0;
}

static void DRD_(sema_destroy)(DrdSema* sema)
{
   pthread_mutex_destroy(&sema->mutex);
   pthread_cond_destroy(&sema->cond);
}

static void DRD_(sema_down)(DrdSema* sema)
{
   pthread_mutex_lock(&sema->mutex);
   while (sema->counter == 0)
      pthread_cond_wait(&sema->cond, &sema->mutex);
   sema->counter--;
   pthread_mutex_unlock(&sema->mutex);
}

static void DRD_(sema_up)(DrdSema* sema)
{
   pthread_mutex_lock(&sema->mutex);
   sema->counter++;
   pthread_cond_signal(&sema->cond);
   pthread_mutex_unlock(&sema->mutex);
}

/**
 * POSIX threads and DRD each have their own mutex type identification.
 * Convert POSIX threads' mutex type to DRD's mutex type. In the code below
 * if-statements are used to test the value of 'kind' instead of a switch
 * statement because some of the PTHREAD_MUTEX_ macro's may have the same
 * value.
 */
static MutexT DRD_(pthread_to_drd_mutex_type)(int kind)
{
   /*
    * Static checkers don't like this as there are repeated branch
    * but because there is variation between different platforms
    * it's messy to make something without repetition.
    *
    * See also PTHREAD_MUTEX_KIND_MASK_NP in glibc source file
    * <nptl/pthreadP.h>.
    */
   kind &= PTHREAD_MUTEX_RECURSIVE | PTHREAD_MUTEX_ERRORCHECK |
      PTHREAD_MUTEX_NORMAL | PTHREAD_MUTEX_DEFAULT;

   if (kind == PTHREAD_MUTEX_RECURSIVE) {
      return mutex_type_recursive_mutex;
   }
   if (kind == PTHREAD_MUTEX_ERRORCHECK) {
      return mutex_type_errorcheck_mutex;
   }
   if (kind == PTHREAD_MUTEX_NORMAL) {
      return mutex_type_default_mutex;
   }
   if (kind == PTHREAD_MUTEX_DEFAULT) {
      // On FreeBSD PTHREAD_MUTEX_DEFAULT is the same as PTHREAD_MUTEX_ERRORCHECK
      // so this code is unreachable, but that's not true for all platforms
      // so just ignore the warning
      // coverity[DEADCODE:FALSE]
      return mutex_type_default_mutex;
   }
#if defined(HAVE_PTHREAD_MUTEX_ADAPTIVE_NP)
   if (kind == PTHREAD_MUTEX_ADAPTIVE_NP) {
      return mutex_type_default_mutex;
   }
#endif
   return mutex_type_invalid_mutex;
}

#if defined(VGO_solaris)
/**
 * Solaris threads and DRD each have their own mutex type identification.
 * Convert Solaris threads' mutex type to DRD's mutex type.
 */
static MutexT DRD_(thread_to_drd_mutex_type)(int type)
{
   if (type & LOCK_RECURSIVE) {
      return mutex_type_recursive_mutex;
   } else if (type & LOCK_ERRORCHECK) {
      return mutex_type_errorcheck_mutex;
   } else {
      return mutex_type_default_mutex;
   }
}
#endif /* VGO_solaris */

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
   MutexT mutex_type = mutex_type_unknown;

   ANNOTATE_IGNORE_READS_BEGIN();
#if defined(HAVE_PTHREAD_MUTEX_T__M_KIND)
   /* glibc + LinuxThreads. */
   if (IS_ALIGNED(&mutex->__m_kind))
   {
      const int kind = mutex->__m_kind & 3;
      mutex_type = DRD_(pthread_to_drd_mutex_type)(kind);
   }
#elif defined(HAVE_PTHREAD_MUTEX_T__DATA__KIND)
   /* glibc + NPTL. */
   if (IS_ALIGNED(&mutex->__data.__kind))
   {
      const int kind = mutex->__data.__kind & 3;
      mutex_type = DRD_(pthread_to_drd_mutex_type)(kind);
   }
#elif defined(VGO_solaris)
   {
      const int type = ((mutex_t *) mutex)->vki_mutex_type;
      mutex_type = DRD_(thread_to_drd_mutex_type)(type);
   }
#else
   /*
    * Another POSIX threads implementation. The mutex type won't be printed
    * when enabling --trace-mutex=yes.
    */
#endif
   ANNOTATE_IGNORE_READS_END();

   return mutex_type;
}

/**
 * Tell DRD whether 'tid' is a joinable thread or a detached thread.
 */
static void DRD_(set_joinable)(const pthread_t tid, const int joinable)
{
   assert(joinable == 0 || joinable == 1);
   VALGRIND_DO_CLIENT_REQUEST_STMT(VG_USERREQ_DRD_SET_JOINABLE,
                                   tid, joinable, 0, 0, 0);
}

/** Tell DRD that the calling thread is about to enter pthread_create(). */
static __always_inline void DRD_(entering_pthread_create)(void)
{
   VALGRIND_DO_CLIENT_REQUEST_STMT(VG_USERREQ_DRD_ENTERING_PTHREAD_CREATE,
                                   0, 0, 0, 0, 0);
}

/** Tell DRD that the calling thread has left pthread_create(). */
static __always_inline void DRD_(left_pthread_create)(void)
{
   VALGRIND_DO_CLIENT_REQUEST_STMT(VG_USERREQ_DRD_LEFT_PTHREAD_CREATE,
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

   VALGRIND_DO_CLIENT_REQUEST_STMT(VG_USERREQ_DRD_SET_PTHREADID,
                                   pthread_self(), 0, 0, 0, 0);

   DRD_(set_joinable)(pthread_self(),
                      arg_copy.detachstate == PTHREAD_CREATE_JOINABLE);

   /*
    * Only set 'wrapper_started' after VG_USERREQ__SET_PTHREADID and
    * DRD_(set_joinable)() have been invoked to avoid a race with
    * a pthread_detach() invocation for this thread from another thread.
    */
   DRD_(sema_up)(arg_copy.wrapper_started);

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
   HChar buffer[256];
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
 * Update DRD's state information about the current thread.
 */
static void DRD_(set_pthread_id)(void)
{
   VALGRIND_DO_CLIENT_REQUEST_STMT(VG_USERREQ_DRD_SET_PTHREADID,
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
   DrdSema wrapper_started;
   DrdPosixThreadArgs thread_args;

   VALGRIND_GET_ORIG_FN(fn);

   DRD_(sema_init)(&wrapper_started);
   thread_args.start           = start;
   thread_args.arg             = arg;
   thread_args.wrapper_started = &wrapper_started;
   /*
    * Find out whether the thread will be started as a joinable thread
    * or as a detached thread. If no thread attributes have been specified,
    * this means that the new thread will be started as a joinable thread.
    */
   thread_args.detachstate = PTHREAD_CREATE_JOINABLE;
   /* The C11 thrd_create() implementation passes -1 as 'attr' argument. */
   if (attr && (uintptr_t)attr + 1 != 0)
   {
      if (pthread_attr_getdetachstate(attr, &thread_args.detachstate) != 0)
         assert(0);
   }
   assert(thread_args.detachstate == PTHREAD_CREATE_JOINABLE
          || thread_args.detachstate == PTHREAD_CREATE_DETACHED);

   /*
    * The DRD_(set_pthread_id)() from DRD_(init)() may encounter that
    * pthread_self() == 0, e.g. when the main program is not linked with the
    * pthread library and when a pthread_create() call occurs from within a
    * shared library. Hence call DRD_(set_pthread_id)() again to ensure that
    * DRD knows the identity of the current thread. See also B.Z. 356374.
    */
   DRD_(set_pthread_id)();
   DRD_(entering_pthread_create)();
   CALL_FN_W_WWWW(ret, fn, thread, attr, DRD_(thread_wrapper), &thread_args);
   DRD_(left_pthread_create)();

   if (ret == 0) {
      /* Wait until the thread wrapper started. */
      DRD_(sema_down)(&wrapper_started);
   }

   DRD_(sema_destroy)(&wrapper_started);

   VALGRIND_DO_CLIENT_REQUEST_STMT(VG_USERREQ_DRD_START_NEW_SEGMENT,
                                   pthread_self(), 0, 0, 0, 0);

   return ret;
}

PTH_FUNCS(int, pthreadZucreate, pthread_create_intercept,
          (pthread_t *thread, const pthread_attr_t *attr,
           void *(*start) (void *), void *arg),
          (thread, attr, start, arg));

#if defined(VGO_solaris)
/* Solaris also provides thr_create() in addition to pthread_create().
 * Both pthread_create(3C) and thr_create(3C) are based on private
 * _thrp_create().
 */
static __always_inline
int thr_create_intercept(void *stk, size_t stksize, void *(*start)(void *),
                         void *arg, long flags, thread_t *new_thread)
{
   int                ret;
   OrigFn             fn;
   DrdSema            wrapper_started;
   DrdPosixThreadArgs thread_args;

   VALGRIND_GET_ORIG_FN(fn);

   DRD_(sema_init)(&wrapper_started);
   thread_args.start           = start;
   thread_args.arg             = arg;
   thread_args.wrapper_started = &wrapper_started;
   /*
    * Find out whether the thread will be started as a joinable thread
    * or as a detached thread.
    */
   if (flags & THR_DETACHED)
      thread_args.detachstate = PTHREAD_CREATE_DETACHED;
   else
      thread_args.detachstate = PTHREAD_CREATE_JOINABLE;

   DRD_(entering_pthread_create)();
   CALL_FN_W_6W(ret, fn, stk, stksize, DRD_(thread_wrapper), &thread_args,
                flags, new_thread);
   DRD_(left_pthread_create)();

   if (ret == 0) {
      /* Wait until the thread wrapper started. */
      DRD_(sema_down)(&wrapper_started);
   }

   DRD_(sema_destroy)(&wrapper_started);

   VALGRIND_DO_CLIENT_REQUEST_STMT(VG_USERREQ_DRD_START_NEW_SEGMENT,
                                   pthread_self(), 0, 0, 0, 0);

   return ret;
}

PTH_FUNCS(int, thrZucreate, thr_create_intercept,
          (void *stk, size_t stksize, void *(*start)(void *), void *arg,
           long flags, thread_t *new_thread),
          (stk, stksize, start, arg, flags, new_thread));
#endif /* VGO_solaris */

#if defined(VGO_solaris)
/*
 * Intercepts for _ti_bind_guard() and _ti_bind_clear() functions from libc.
 * These are intercepted during _ld_libc() call by identifying CI_BIND_GUARD
 * and CI_BIND_CLEAR, to provide resilience against function renaming.
 */
static __always_inline
int DRD_(_ti_bind_guard_intercept)(int flags) {
   VALGRIND_DO_CLIENT_REQUEST_STMT(VG_USERREQ_DRD_RTLD_BIND_GUARD,
                                   flags, 0, 0, 0, 0);
   return DRD_(rtld_bind_guard)(flags);
}

static __always_inline
int DRD_(_ti_bind_clear_intercept)(int flags) {
   int ret = DRD_(rtld_bind_clear)(flags);
   VALGRIND_DO_CLIENT_REQUEST_STMT(VG_USERREQ_DRD_RTLD_BIND_CLEAR,
                                   flags, 0, 0, 0, 0);
   return ret;
}

/*
 * Wrapped _ld_libc() from the runtime linker ld.so.1.
 */
void VG_WRAP_FUNCTION_ZZ(VG_Z_LD_SO_1, ZuldZulibc)(vki_Lc_interface *ptr);
void VG_WRAP_FUNCTION_ZZ(VG_Z_LD_SO_1, ZuldZulibc)(vki_Lc_interface *ptr)
{
   OrigFn fn;
   int    tag;

   VALGRIND_GET_ORIG_FN(fn);

   vki_Lc_interface *funcs = ptr;
   for (tag = funcs->ci_tag; tag != 0; tag = (++funcs)->ci_tag) {
      switch (tag) {
      case VKI_CI_BIND_GUARD:
         if (funcs->vki_ci_un.ci_func != DRD_(_ti_bind_guard_intercept)) {
            DRD_(rtld_bind_guard) = funcs->vki_ci_un.ci_func;
            funcs->vki_ci_un.ci_func = DRD_(_ti_bind_guard_intercept);
         }
         break;
      case VKI_CI_BIND_CLEAR:
         if (funcs->vki_ci_un.ci_func != DRD_(_ti_bind_clear_intercept)) {
            DRD_(rtld_bind_clear) = funcs->vki_ci_un.ci_func;
            funcs->vki_ci_un.ci_func = DRD_(_ti_bind_clear_intercept);
         }
         break;
      }
   }

   CALL_FN_v_W(fn, ptr);
}
#endif /* VGO_solaris */

static __always_inline
int pthread_join_intercept(pthread_t pt_joinee, void **thread_return)
{
   int      ret;
   OrigFn   fn;

   VALGRIND_GET_ORIG_FN(fn);
   /*
    * Avoid that the sys_futex(td->tid) call invoked by the NPTL pthread_join()
    * implementation triggers a (false positive) race report.
    */
   ANNOTATE_IGNORE_READS_AND_WRITES_BEGIN();
   CALL_FN_W_WW(ret, fn, pt_joinee, thread_return);
   if (ret == 0)
   {
      VALGRIND_DO_CLIENT_REQUEST_STMT(VG_USERREQ_DRD_POST_THREAD_JOIN,
                                      pt_joinee, 0, 0, 0, 0);
   }
   ANNOTATE_IGNORE_READS_AND_WRITES_END();
   return ret;
}

PTH_FUNCS(int, pthreadZujoin, pthread_join_intercept,
          (pthread_t pt_joinee, void **thread_return),
          (pt_joinee, thread_return));

#if defined(VGO_solaris)
/* Solaris also provides thr_join() in addition to pthread_join().
 * Both pthread_join(3C) and thr_join(3C) are based on private _thrp_join().
 *
 * :TODO: No functionality is currently provided for joinee == 0 and departed.
 *        This would require another client request, of course.
 */
static __always_inline
int thr_join_intercept(thread_t joinee, thread_t *departed, void **thread_return)
{
   int      ret;
   OrigFn   fn;

   VALGRIND_GET_ORIG_FN(fn);
   CALL_FN_W_WWW(ret, fn, joinee, departed, thread_return);
   if (ret == 0)
   {
      VALGRIND_DO_CLIENT_REQUEST_STMT(VG_USERREQ_DRD_POST_THREAD_JOIN,
                                      joinee, 0, 0, 0, 0);
   }
   return ret;
}

PTH_FUNCS(int, thrZujoin, thr_join_intercept,
          (thread_t joinee, thread_t *departed, void **thread_return),
          (joinee, departed, thread_return));
#endif /* VGO_solaris */

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
   VALGRIND_DO_CLIENT_REQUEST_STMT(VG_USERREQ_DRD_PRE_THREAD_CANCEL,
                                   pt_thread, 0, 0, 0, 0);
   CALL_FN_W_W(ret, fn, pt_thread);
   VALGRIND_DO_CLIENT_REQUEST_STMT(VG_USERREQ_DRD_POST_THREAD_CANCEL,
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
   ANNOTATE_IGNORE_READS_AND_WRITES_BEGIN();
   CALL_FN_W_WW(ret, fn, once_control, init_routine);
   ANNOTATE_IGNORE_READS_AND_WRITES_END();
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
   VALGRIND_DO_CLIENT_REQUEST_STMT(VG_USERREQ_DRD_PRE_MUTEX_INIT,
                                   mutex, DRD_(pthread_to_drd_mutex_type)(mt),
                                   0, 0, 0);
   CALL_FN_W_WW(ret, fn, mutex, attr);
   VALGRIND_DO_CLIENT_REQUEST_STMT(VG_USERREQ_DRD_POST_MUTEX_INIT,
                                   mutex, 0, 0, 0, 0);
   return ret;
}

PTH_FUNCS(int, pthreadZumutexZuinit, pthread_mutex_init_intercept,
          (pthread_mutex_t *mutex, const pthread_mutexattr_t* attr),
          (mutex, attr));

#if defined(VGO_solaris)
static __always_inline
int mutex_init_intercept(mutex_t *mutex, int type, void *arg)
{
   int ret;
   OrigFn fn;
   VALGRIND_GET_ORIG_FN(fn);

   VALGRIND_DO_CLIENT_REQUEST_STMT(VG_USERREQ_DRD_PRE_MUTEX_INIT,
                                   mutex, DRD_(thread_to_drd_mutex_type)(type),
                                   0, 0, 0);
   CALL_FN_W_WWW(ret, fn, mutex, type, arg);
   VALGRIND_DO_CLIENT_REQUEST_STMT(VG_USERREQ_DRD_POST_MUTEX_INIT,
                                   mutex, 0, 0, 0, 0);
   return ret;
}

PTH_FUNCS(int, mutexZuinit, mutex_init_intercept,
          (mutex_t *mutex, int type, void *arg),
          (mutex, type, arg));
#endif /* VGO_solaris */

static __always_inline
int pthread_mutex_destroy_intercept(pthread_mutex_t* mutex)
{
   int ret;
   OrigFn fn;
   VALGRIND_GET_ORIG_FN(fn);
   VALGRIND_DO_CLIENT_REQUEST_STMT(VG_USERREQ_DRD_PRE_MUTEX_DESTROY,
                                   mutex, 0, 0, 0, 0);
   CALL_FN_W_W(ret, fn, mutex);
   VALGRIND_DO_CLIENT_REQUEST_STMT(VG_USERREQ_DRD_POST_MUTEX_DESTROY,
                                   mutex, DRD_(mutex_type)(mutex), 0, 0, 0);
   return ret;
}

#if defined(VGO_solaris)
/* On Solaris, pthread_mutex_destroy is a weak alias to mutex_destroy. */
PTH_FUNCS(int, mutexZudestroy, pthread_mutex_destroy_intercept,
          (pthread_mutex_t *mutex), (mutex));
#else
PTH_FUNCS(int, pthreadZumutexZudestroy, pthread_mutex_destroy_intercept,
          (pthread_mutex_t *mutex), (mutex));
#endif /* VGO_solaris */

static __always_inline
int pthread_mutex_lock_intercept(pthread_mutex_t* mutex)
{
   int   ret;
   OrigFn fn;
   VALGRIND_GET_ORIG_FN(fn);
   VALGRIND_DO_CLIENT_REQUEST_STMT(VG_USERREQ_DRD_PRE_MUTEX_LOCK,
                                   mutex, DRD_(mutex_type)(mutex), 0, 0, 0);
   CALL_FN_W_W(ret, fn, mutex);
   VALGRIND_DO_CLIENT_REQUEST_STMT(VG_USERREQ_DRD_POST_MUTEX_LOCK,
                                   mutex, ret == 0, 0, 0, 0);
   return ret;
}

#if defined(VGO_solaris)
/* On Solaris, pthread_mutex_lock is a weak alias to mutex_lock. */
PTH_FUNCS(int, mutexZulock, pthread_mutex_lock_intercept,
          (pthread_mutex_t *mutex), (mutex));
#else
PTH_FUNCS(int, pthreadZumutexZulock, pthread_mutex_lock_intercept,
          (pthread_mutex_t *mutex), (mutex));
#endif /* VGO_solaris */

#if defined(VGO_solaris)
/* Internal to libc. Mutex is usually initialized only implicitly,
 * by zeroing mutex_t structure.
 */
static __always_inline
void lmutex_lock_intercept(mutex_t *mutex)
{
   OrigFn fn;
   VALGRIND_GET_ORIG_FN(fn);
   VALGRIND_DO_CLIENT_REQUEST_STMT(VG_USERREQ_DRD_PRE_MUTEX_LOCK,
                                   mutex,
                                   DRD_(mutex_type)((pthread_mutex_t *) mutex),
                                   False /* try_lock */, 0, 0);
   CALL_FN_v_W(fn, mutex);
   VALGRIND_DO_CLIENT_REQUEST_STMT(VG_USERREQ_DRD_POST_MUTEX_LOCK,
                                   mutex, True /* took_lock */, 0, 0, 0);
}

PTH_FUNCS(void, lmutexZulock, lmutex_lock_intercept,
          (mutex_t *mutex), (mutex));
#endif /* VGO_solaris */

static __always_inline
int pthread_mutex_trylock_intercept(pthread_mutex_t* mutex)
{
   int   ret;
   OrigFn fn;
   VALGRIND_GET_ORIG_FN(fn);
   VALGRIND_DO_CLIENT_REQUEST_STMT(VG_USERREQ_DRD_PRE_MUTEX_LOCK,
                                   mutex, DRD_(mutex_type)(mutex), 1, 0, 0);
   CALL_FN_W_W(ret, fn, mutex);
   VALGRIND_DO_CLIENT_REQUEST_STMT(VG_USERREQ_DRD_POST_MUTEX_LOCK,
                                   mutex, ret == 0, 0, 0, 0);
   return ret;
}

#if defined(VGO_solaris)
/* On Solaris, pthread_mutex_trylock is a weak alias to mutex_trylock. */
PTH_FUNCS(int, mutexZutrylock, pthread_mutex_trylock_intercept,
          (pthread_mutex_t *mutex), (mutex));
#else
PTH_FUNCS(int, pthreadZumutexZutrylock, pthread_mutex_trylock_intercept,
          (pthread_mutex_t *mutex), (mutex));
#endif /* VGO_solaris */

static __always_inline
int pthread_mutex_timedlock_intercept(pthread_mutex_t *mutex,
                                      const struct timespec *abs_timeout)
{
   int   ret;
   OrigFn fn;
   VALGRIND_GET_ORIG_FN(fn);
   VALGRIND_DO_CLIENT_REQUEST_STMT(VG_USERREQ_DRD_PRE_MUTEX_LOCK,
                                   mutex, DRD_(mutex_type)(mutex), 0, 0, 0);
   CALL_FN_W_WW(ret, fn, mutex, abs_timeout);
   VALGRIND_DO_CLIENT_REQUEST_STMT(VG_USERREQ_DRD_POST_MUTEX_LOCK,
                                   mutex, ret == 0, 0, 0, 0);
   return ret;
}

PTH_FUNCS(int, pthreadZumutexZutimedlock, pthread_mutex_timedlock_intercept,
          (pthread_mutex_t *mutex, const struct timespec *abs_timeout),
          (mutex, abs_timeout));
#if defined(VGO_solaris)
PTH_FUNCS(int,
          pthreadZumutexZureltimedlockZunp, pthread_mutex_timedlock_intercept,
          (pthread_mutex_t *mutex, const struct timespec *timeout),
          (mutex, timeout));
#endif /* VGO_solaris */

#if defined(HAVE_CLOCKID_T)
static __always_inline
int pthread_mutex_clocklock_intercept(pthread_mutex_t *mutex,
                                      clockid_t clockid,
                                      const struct timespec *abs_timeout)
{
   int   ret;
   OrigFn fn;
   VALGRIND_GET_ORIG_FN(fn);
   VALGRIND_DO_CLIENT_REQUEST_STMT(VG_USERREQ_DRD_PRE_MUTEX_LOCK,
                                   mutex, DRD_(mutex_type)(mutex), 0, 0, 0);
   CALL_FN_W_WWW(ret, fn, mutex, clockid, abs_timeout);
   VALGRIND_DO_CLIENT_REQUEST_STMT(VG_USERREQ_DRD_POST_MUTEX_LOCK,
                                   mutex, ret == 0, 0, 0, 0);
   return ret;
}

PTH_FUNCS(int, pthreadZumutexZuclocklock, pthread_mutex_clocklock_intercept,
          (pthread_mutex_t *mutex, clockid_t clockid, const struct timespec *abs_timeout),
          (mutex, clockid, abs_timeout));
#endif

static __always_inline
int pthread_mutex_unlock_intercept(pthread_mutex_t *mutex)
{
   int ret;
   OrigFn fn;
   VALGRIND_GET_ORIG_FN(fn);
   VALGRIND_DO_CLIENT_REQUEST_STMT(VG_USERREQ_DRD_PRE_MUTEX_UNLOCK,
                                   mutex, DRD_(mutex_type)(mutex), 0, 0, 0);
   CALL_FN_W_W(ret, fn, mutex);
   VALGRIND_DO_CLIENT_REQUEST_STMT(VG_USERREQ_DRD_POST_MUTEX_UNLOCK,
                                   mutex, 0, 0, 0, 0);
   return ret;
}

#if defined(VGO_solaris)
/* On Solaris, pthread_mutex_unlock is a weak alias to mutex_unlock. */
PTH_FUNCS(int, mutexZuunlock, pthread_mutex_unlock_intercept,
          (pthread_mutex_t *mutex), (mutex));
#else
PTH_FUNCS(int, pthreadZumutexZuunlock, pthread_mutex_unlock_intercept,
          (pthread_mutex_t *mutex), (mutex));
#endif /* VGO_solaris */

#if defined(VGO_solaris)
/* Internal to libc. */
static __always_inline
void lmutex_unlock_intercept(mutex_t *mutex)
{
   OrigFn fn;
   VALGRIND_GET_ORIG_FN(fn);
   VALGRIND_DO_CLIENT_REQUEST_STMT(VG_USERREQ_DRD_PRE_MUTEX_UNLOCK,
                                   mutex,
                                   DRD_(mutex_type)((pthread_mutex_t *) mutex),
                                   0, 0, 0);
   CALL_FN_v_W(fn, mutex);
   VALGRIND_DO_CLIENT_REQUEST_STMT(VG_USERREQ_DRD_POST_MUTEX_UNLOCK,
                                   mutex, 0, 0, 0, 0);
}

PTH_FUNCS(void, lmutexZuunlock, lmutex_unlock_intercept,
          (mutex_t *mutex), (mutex));
#endif /* VGO_solaris */

static __always_inline
int pthread_cond_init_intercept(pthread_cond_t* cond,
                                const pthread_condattr_t* attr)
{
   int ret;
   OrigFn fn;
   VALGRIND_GET_ORIG_FN(fn);
   VALGRIND_DO_CLIENT_REQUEST_STMT(VG_USERREQ_DRD_PRE_COND_INIT,
                                   cond, 0, 0, 0, 0);
   CALL_FN_W_WW(ret, fn, cond, attr);
   VALGRIND_DO_CLIENT_REQUEST_STMT(VG_USERREQ_DRD_POST_COND_INIT,
                                   cond, 0, 0, 0, 0);
   return ret;
}

PTH_FUNCS(int, pthreadZucondZuinit, pthread_cond_init_intercept,
          (pthread_cond_t* cond, const pthread_condattr_t* attr),
          (cond, attr));

#if defined(VGO_solaris)
static __always_inline
int cond_init_intercept(cond_t *cond, int type, void *arg)
{
   int ret;
   OrigFn fn;
   VALGRIND_GET_ORIG_FN(fn);
   VALGRIND_DO_CLIENT_REQUEST_STMT(VG_USERREQ_DRD_PRE_COND_INIT,
                                   cond, 0, 0, 0, 0);
   CALL_FN_W_WWW(ret, fn, cond, type, arg);
   VALGRIND_DO_CLIENT_REQUEST_STMT(VG_USERREQ_DRD_POST_COND_INIT,
                                   cond, 0, 0, 0, 0);
   return ret;
}

PTH_FUNCS(int, condZuinit, cond_init_intercept,
          (cond_t *cond, int type, void *arg),
          (cond, type, arg));
#endif /* VGO_solaris */

static __always_inline
int pthread_cond_destroy_intercept(pthread_cond_t* cond)
{
   int ret;
   OrigFn fn;
   VALGRIND_GET_ORIG_FN(fn);
   VALGRIND_DO_CLIENT_REQUEST_STMT(VG_USERREQ_DRD_PRE_COND_DESTROY,
                                   cond, 0, 0, 0, 0);
   CALL_FN_W_W(ret, fn, cond);
   VALGRIND_DO_CLIENT_REQUEST_STMT(VG_USERREQ_DRD_POST_COND_DESTROY,
                                   cond, ret==0, 0, 0, 0);
   return ret;
}

#if defined(VGO_solaris)
/* On Solaris, pthread_cond_destroy is a weak alias to cond_destroy. */
PTH_FUNCS(int, condZudestroy, pthread_cond_destroy_intercept,
          (pthread_cond_t *cond), (cond));
#else
PTH_FUNCS(int, pthreadZucondZudestroy, pthread_cond_destroy_intercept,
          (pthread_cond_t* cond), (cond));
#endif /* VGO_solaris */

static __always_inline
int pthread_cond_wait_intercept(pthread_cond_t *cond, pthread_mutex_t *mutex)
{
   int   ret;
   OrigFn fn;
   VALGRIND_GET_ORIG_FN(fn);
   VALGRIND_DO_CLIENT_REQUEST_STMT(VG_USERREQ_DRD_PRE_COND_WAIT,
                                   cond, mutex, DRD_(mutex_type)(mutex), 0, 0);
   CALL_FN_W_WW(ret, fn, cond, mutex);
   VALGRIND_DO_CLIENT_REQUEST_STMT(VG_USERREQ_DRD_POST_COND_WAIT,
                                   cond, mutex, 1, 0, 0);
   return ret;
}

PTH_FUNCS(int, pthreadZucondZuwait, pthread_cond_wait_intercept,
          (pthread_cond_t *cond, pthread_mutex_t *mutex),
          (cond, mutex));
#if defined(VGO_solaris)
PTH_FUNCS(int, condZuwait, pthread_cond_wait_intercept,
          (pthread_cond_t *cond, pthread_mutex_t *mutex),
          (cond, mutex));
#endif /* VGO_solaris */

static __always_inline
int pthread_cond_timedwait_intercept(pthread_cond_t *cond,
                                     pthread_mutex_t *mutex,
                                     const struct timespec* abstime)
{
   int   ret;
   OrigFn fn;
   VALGRIND_GET_ORIG_FN(fn);
   VALGRIND_DO_CLIENT_REQUEST_STMT(VG_USERREQ_DRD_PRE_COND_WAIT,
                                   cond, mutex, DRD_(mutex_type)(mutex), 0, 0);
   CALL_FN_W_WWW(ret, fn, cond, mutex, abstime);
   VALGRIND_DO_CLIENT_REQUEST_STMT(VG_USERREQ_DRD_POST_COND_WAIT,
                                   cond, mutex, 1, 0, 0);
   return ret;
}

PTH_FUNCS(int, pthreadZucondZutimedwait, pthread_cond_timedwait_intercept,
          (pthread_cond_t *cond, pthread_mutex_t *mutex,
           const struct timespec* abstime),
          (cond, mutex, abstime));
#if defined(VGO_solaris)
PTH_FUNCS(int, condZutimedwait, pthread_cond_timedwait_intercept,
          (pthread_cond_t *cond, pthread_mutex_t *mutex,
           const struct timespec *timeout),
          (cond, mutex, timeout));
PTH_FUNCS(int, condZureltimedwait, pthread_cond_timedwait_intercept,
          (pthread_cond_t *cond, pthread_mutex_t *mutex,
           const struct timespec *timeout),
          (cond, mutex, timeout));
#endif /* VGO_solaris */


#if defined(HAVE_CLOCKID_T)
static __always_inline
int pthread_cond_clockwait_intercept(pthread_cond_t *cond,
                                     pthread_mutex_t *mutex,
                                     clockid_t clockid,
                                     const struct timespec* abstime)
{
   int   ret;
   OrigFn fn;
   VALGRIND_GET_ORIG_FN(fn);
   VALGRIND_DO_CLIENT_REQUEST_STMT(VG_USERREQ_DRD_PRE_COND_WAIT,
                                   cond, mutex, DRD_(mutex_type)(mutex), 0, 0);
   CALL_FN_W_WWWW(ret, fn, cond, mutex, clockid, abstime);
   VALGRIND_DO_CLIENT_REQUEST_STMT(VG_USERREQ_DRD_POST_COND_WAIT,
                                   cond, mutex, 1, 0, 0);
   return ret;
}

PTH_FUNCS(int, pthreadZucondZuclockwait, pthread_cond_clockwait_intercept,
          (pthread_cond_t *cond, pthread_mutex_t *mutex,
            clockid_t clockid, const struct timespec* abstime),
          (cond, mutex, clockid, abstime));
#endif


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
   VALGRIND_DO_CLIENT_REQUEST_STMT(VG_USERREQ_DRD_PRE_COND_SIGNAL,
                                   cond, 0, 0, 0, 0);
   CALL_FN_W_W(ret, fn, cond);
   VALGRIND_DO_CLIENT_REQUEST_STMT(VG_USERREQ_DRD_POST_COND_SIGNAL,
                                   cond, 0, 0, 0, 0);
   return ret;
}

#if defined(VGO_solaris)
/* On Solaris, pthread_cond_signal is a weak alias to cond_signal. */
PTH_FUNCS(int, condZusignal, pthread_cond_signal_intercept,
          (pthread_cond_t *cond), (cond));
#else
PTH_FUNCS(int, pthreadZucondZusignal, pthread_cond_signal_intercept,
          (pthread_cond_t* cond), (cond));
#endif /* VGO_solaris */

static __always_inline
int pthread_cond_broadcast_intercept(pthread_cond_t* cond)
{
   int   ret;
   OrigFn fn;
   VALGRIND_GET_ORIG_FN(fn);
   VALGRIND_DO_CLIENT_REQUEST_STMT(VG_USERREQ_DRD_PRE_COND_BROADCAST,
                                   cond, 0, 0, 0, 0);
   CALL_FN_W_W(ret, fn, cond);
   VALGRIND_DO_CLIENT_REQUEST_STMT(VG_USERREQ_DRD_POST_COND_BROADCAST,
                                   cond, 0, 0, 0, 0);
   return ret;
}

#if defined(VGO_solaris)
/* On Solaris, pthread_cond_broadcast is a weak alias to cond_broadcast. */
PTH_FUNCS(int, condZubroadcast, pthread_cond_broadcast_intercept,
          (pthread_cond_t *cond), (cond));
#else
PTH_FUNCS(int, pthreadZucondZubroadcast, pthread_cond_broadcast_intercept,
          (pthread_cond_t* cond), (cond));
#endif /* VGO_solaris */

#if defined(HAVE_PTHREAD_SPIN_LOCK) \
    && !defined(DISABLE_PTHREAD_SPINLOCK_INTERCEPT)
static __always_inline
int pthread_spin_init_intercept(pthread_spinlock_t *spinlock, int pshared)
{
   int ret;
   OrigFn fn;
   VALGRIND_GET_ORIG_FN(fn);
   VALGRIND_DO_CLIENT_REQUEST_STMT(VG_USERREQ_DRD_PRE_SPIN_INIT_OR_UNLOCK,
                                   spinlock, 0, 0, 0, 0);
   CALL_FN_W_WW(ret, fn, spinlock, pshared);
   VALGRIND_DO_CLIENT_REQUEST_STMT(VG_USERREQ_DRD_POST_SPIN_INIT_OR_UNLOCK,
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
   VALGRIND_DO_CLIENT_REQUEST_STMT(VG_USERREQ_DRD_PRE_MUTEX_DESTROY,
                                   spinlock, 0, 0, 0, 0);
   CALL_FN_W_W(ret, fn, spinlock);
   VALGRIND_DO_CLIENT_REQUEST_STMT(VG_USERREQ_DRD_POST_MUTEX_DESTROY,
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
   VALGRIND_DO_CLIENT_REQUEST_STMT(VG_USERREQ_DRD_PRE_MUTEX_LOCK,
                                   spinlock, mutex_type_spinlock, 0, 0, 0);
   CALL_FN_W_W(ret, fn, spinlock);
   VALGRIND_DO_CLIENT_REQUEST_STMT(VG_USERREQ_DRD_POST_MUTEX_LOCK,
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
   VALGRIND_DO_CLIENT_REQUEST_STMT(VG_USERREQ_DRD_PRE_MUTEX_LOCK,
                                   spinlock, mutex_type_spinlock, 0, 0, 0);
   CALL_FN_W_W(ret, fn, spinlock);
   VALGRIND_DO_CLIENT_REQUEST_STMT(VG_USERREQ_DRD_POST_MUTEX_LOCK,
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
   VALGRIND_DO_CLIENT_REQUEST_STMT(VG_USERREQ_DRD_PRE_SPIN_INIT_OR_UNLOCK,
                                   spinlock, mutex_type_spinlock, 0, 0, 0);
   CALL_FN_W_W(ret, fn, spinlock);
   VALGRIND_DO_CLIENT_REQUEST_STMT(VG_USERREQ_DRD_POST_SPIN_INIT_OR_UNLOCK,
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
   VALGRIND_DO_CLIENT_REQUEST_STMT(VG_USERREQ_DRD_PRE_BARRIER_INIT,
                                   barrier, pthread_barrier, count, 0, 0);
   CALL_FN_W_WWW(ret, fn, barrier, attr, count);
   VALGRIND_DO_CLIENT_REQUEST_STMT(VG_USERREQ_DRD_POST_BARRIER_INIT,
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
   VALGRIND_DO_CLIENT_REQUEST_STMT(VG_USERREQ_DRD_PRE_BARRIER_DESTROY,
                                   barrier, pthread_barrier, 0, 0, 0);
   CALL_FN_W_W(ret, fn, barrier);
   VALGRIND_DO_CLIENT_REQUEST_STMT(VG_USERREQ_DRD_POST_BARRIER_DESTROY,
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
   VALGRIND_DO_CLIENT_REQUEST_STMT(VG_USERREQ_DRD_PRE_BARRIER_WAIT,
                                   barrier, pthread_barrier, 0, 0, 0);
   CALL_FN_W_W(ret, fn, barrier);
   VALGRIND_DO_CLIENT_REQUEST_STMT(VG_USERREQ_DRD_POST_BARRIER_WAIT,
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
   VALGRIND_DO_CLIENT_REQUEST_STMT(VG_USERREQ_DRD_PRE_SEM_INIT,
                                   sem, pshared, value, 0, 0);
   CALL_FN_W_WWW(ret, fn, sem, pshared, value);
   VALGRIND_DO_CLIENT_REQUEST_STMT(VG_USERREQ_DRD_POST_SEM_INIT,
                                   sem, 0, 0, 0, 0);
   return ret;
}

#if defined(VGO_freebsd)
LIBC_FUNC(int, semZuinit, sem_init_intercept,
          (sem_t *sem, int pshared, unsigned int value), (sem, pshared, value));
#else
PTH_FUNCS(int, semZuinit, sem_init_intercept,
          (sem_t *sem, int pshared, unsigned int value), (sem, pshared, value));
#endif

#if defined(VGO_solaris)
static __always_inline
int sema_init_intercept(sema_t *sem, unsigned int value, int type, void *arg)
{
   int   ret;
   OrigFn fn;
   VALGRIND_GET_ORIG_FN(fn);
   VALGRIND_DO_CLIENT_REQUEST_STMT(VG_USERREQ_DRD_PRE_SEM_INIT,
                                   sem, type == USYNC_PROCESS ? 1 : 0,
                                   value, 0, 0);
   CALL_FN_W_WWWW(ret, fn, sem, value, type, arg);
   VALGRIND_DO_CLIENT_REQUEST_STMT(VG_USERREQ_DRD_POST_SEM_INIT,
                                   sem, 0, 0, 0, 0);
   return ret;
}

PTH_FUNCS(int, semaZuinit, sema_init_intercept,
          (sema_t *sem, unsigned int value, int type, void *arg),
          (sem, value, type, arg));
#endif /* VGO_solaris */

static __always_inline
int sem_destroy_intercept(sem_t *sem)
{
   int   ret;
   OrigFn fn;
   VALGRIND_GET_ORIG_FN(fn);
   VALGRIND_DO_CLIENT_REQUEST_STMT(VG_USERREQ_DRD_PRE_SEM_DESTROY,
                                   sem, 0, 0, 0, 0);
   CALL_FN_W_W(ret, fn, sem);
   VALGRIND_DO_CLIENT_REQUEST_STMT(VG_USERREQ_DRD_POST_SEM_DESTROY,
                                   sem, 0, 0, 0, 0);
   return ret;
}

#if defined(VGO_freebsd)
LIBC_FUNC(int, semZudestroy, sem_destroy_intercept, (sem_t *sem), (sem));
#else
PTH_FUNCS(int, semZudestroy, sem_destroy_intercept, (sem_t *sem), (sem));
#endif

#if defined(VGO_solaris)
PTH_FUNCS(int, semaZudestroy, sem_destroy_intercept, (sem_t *sem), (sem));
#endif /* VGO_solaris */

static __always_inline
sem_t* sem_open_intercept(const char *name, int oflag, mode_t mode,
                          unsigned int value)
{
   sem_t *ret;
   OrigFn fn;
   VALGRIND_GET_ORIG_FN(fn);
   VALGRIND_DO_CLIENT_REQUEST_STMT(VG_USERREQ_DRD_PRE_SEM_OPEN,
                                   name, oflag, mode, value, 0);
   CALL_FN_W_WWWW(ret, fn, name, oflag, mode, value);
   // To do: figure out why gcc 9.2.1 miscompiles this function if the printf()
   // call below is left out.
   printf("");
   VALGRIND_DO_CLIENT_REQUEST_STMT(VG_USERREQ_DRD_POST_SEM_OPEN,
                                   ret != SEM_FAILED ? ret : 0,
                                   name, oflag, mode, value);
   return ret;
}

#if defined(VGO_freebsd)
LIBC_FUNC(sem_t *, semZuopen, sem_open_intercept,
          (const char *name, int oflag, mode_t mode, unsigned int value),
          (name, oflag, mode, value));
#else
PTH_FUNCS(sem_t *, semZuopen, sem_open_intercept,
          (const char *name, int oflag, mode_t mode, unsigned int value),
          (name, oflag, mode, value));
#endif

static __always_inline int sem_close_intercept(sem_t *sem)
{
   int   ret;
   OrigFn fn;
   VALGRIND_GET_ORIG_FN(fn);
   VALGRIND_DO_CLIENT_REQUEST_STMT(VG_USERREQ_DRD_PRE_SEM_CLOSE,
                                   sem, 0, 0, 0, 0);
   CALL_FN_W_W(ret, fn, sem);
   VALGRIND_DO_CLIENT_REQUEST_STMT(VG_USERREQ_DRD_POST_SEM_CLOSE,
                                   sem, 0, 0, 0, 0);
   return ret;
}

#if defined(VGO_freebsd)
LIBC_FUNC(int, semZuclose, sem_close_intercept, (sem_t *sem), (sem));
#else
PTH_FUNCS(int, semZuclose, sem_close_intercept, (sem_t *sem), (sem));
#endif

static __always_inline int sem_wait_intercept(sem_t *sem)
{
   int   ret;
   OrigFn fn;
   VALGRIND_GET_ORIG_FN(fn);
   VALGRIND_DO_CLIENT_REQUEST_STMT(VG_USERREQ_DRD_PRE_SEM_WAIT,
                                   sem, 0, 0, 0, 0);
   CALL_FN_W_W(ret, fn, sem);
   VALGRIND_DO_CLIENT_REQUEST_STMT(VG_USERREQ_DRD_POST_SEM_WAIT,
                                   sem, ret == 0, 0, 0, 0);
   return ret;
}

#if defined(VGO_freebsd)
LIBC_FUNC(int, semZuwait, sem_wait_intercept, (sem_t *sem), (sem));
#else
PTH_FUNCS(int, semZuwait, sem_wait_intercept, (sem_t *sem), (sem));
#endif

#if defined(VGO_solaris)
PTH_FUNCS(int, semaZuwait, sem_wait_intercept, (sem_t *sem), (sem));
#endif /* VGO_solaris */

static __always_inline int sem_trywait_intercept(sem_t *sem)
{
   int   ret;
   OrigFn fn;
   VALGRIND_GET_ORIG_FN(fn);
   VALGRIND_DO_CLIENT_REQUEST_STMT(VG_USERREQ_DRD_PRE_SEM_WAIT,
                                   sem, 0, 0, 0, 0);
   CALL_FN_W_W(ret, fn, sem);
   VALGRIND_DO_CLIENT_REQUEST_STMT(VG_USERREQ_DRD_POST_SEM_WAIT,
                                   sem, ret == 0, 0, 0, 0);
   return ret;
}

#if defined(VGO_freebsd)
LIBC_FUNC(int, semZutrywait, sem_trywait_intercept, (sem_t *sem), (sem));
#else
PTH_FUNCS(int, semZutrywait, sem_trywait_intercept, (sem_t *sem), (sem));
#endif
#if defined(VGO_solaris)
PTH_FUNCS(int, semaZutrywait, sem_trywait_intercept, (sem_t *sem), (sem));
#endif /* VGO_solaris */

static __always_inline
int sem_timedwait_intercept(sem_t *sem, const struct timespec *abs_timeout)
{
   int   ret;
   OrigFn fn;
   VALGRIND_GET_ORIG_FN(fn);
   VALGRIND_DO_CLIENT_REQUEST_STMT(VG_USERREQ_DRD_PRE_SEM_WAIT,
                                   sem, 0, 0, 0, 0);
   CALL_FN_W_WW(ret, fn, sem, abs_timeout);
   VALGRIND_DO_CLIENT_REQUEST_STMT(VG_USERREQ_DRD_POST_SEM_WAIT,
                                   sem, ret == 0, 0, 0, 0);
   return ret;
}

#if defined(VGO_freebsd)
LIBC_FUNC(int, semZutimedwait, sem_timedwait_intercept,
          (sem_t *sem, const struct timespec *abs_timeout),
          (sem, abs_timeout));
#else
PTH_FUNCS(int, semZutimedwait, sem_timedwait_intercept,
          (sem_t *sem, const struct timespec *abs_timeout),
          (sem, abs_timeout));
#endif
#if defined(VGO_solaris)
PTH_FUNCS(int, semaZutimedwait, sem_timedwait_intercept,
          (sem_t *sem, const struct timespec *timeout),
          (sem, timeout));
PTH_FUNCS(int, semaZureltimedwait, sem_timedwait_intercept,
          (sem_t *sem, const struct timespec *timeout),
          (sem, timeout));
#endif /* VGO_solaris */

static __always_inline int sem_post_intercept(sem_t *sem)
{
   int   ret;
   OrigFn fn;
   VALGRIND_GET_ORIG_FN(fn);
   VALGRIND_DO_CLIENT_REQUEST_STMT(VG_USERREQ_DRD_PRE_SEM_POST,
                                   sem, 0, 0, 0, 0);
   CALL_FN_W_W(ret, fn, sem);
   VALGRIND_DO_CLIENT_REQUEST_STMT(VG_USERREQ_DRD_POST_SEM_POST,
                                   sem, ret == 0, 0, 0, 0);
   return ret;
}

#if defined(VGO_freebsd)
LIBC_FUNC(int, semZupost, sem_post_intercept, (sem_t *sem), (sem));
#else
PTH_FUNCS(int, semZupost, sem_post_intercept, (sem_t *sem), (sem));
#endif
#if defined(VGO_solaris)
PTH_FUNCS(int, semaZupost, sem_post_intercept, (sem_t *sem), (sem));
#endif /* VGO_solaris */

/* Android's pthread.h doesn't say anything about rwlocks, hence these
   functions have to be conditionally compiled. */
#if defined(HAVE_PTHREAD_RWLOCK_T)

static __always_inline
int pthread_rwlock_init_intercept(pthread_rwlock_t* rwlock,
                                  const pthread_rwlockattr_t* attr)
{
   int   ret;
   OrigFn fn;
   VALGRIND_GET_ORIG_FN(fn);
   VALGRIND_DO_CLIENT_REQUEST_STMT(VG_USERREQ_DRD_PRE_RWLOCK_INIT,
                                   rwlock, 0, 0, 0, 0);
   CALL_FN_W_WW(ret, fn, rwlock, attr);
   VALGRIND_DO_CLIENT_REQUEST_STMT(VG_USERREQ_DRD_POST_RWLOCK_INIT,
                                   rwlock, 0, 0, 0, 0);
   return ret;
}

PTH_FUNCS(int,
          pthreadZurwlockZuinit, pthread_rwlock_init_intercept,
          (pthread_rwlock_t* rwlock, const pthread_rwlockattr_t* attr),
          (rwlock, attr));

#if defined(VGO_solaris)
static __always_inline
int rwlock_init_intercept(rwlock_t *rwlock, int type, void *arg)
{
   int   ret;
   OrigFn fn;
   VALGRIND_GET_ORIG_FN(fn);
   VALGRIND_DO_CLIENT_REQUEST_STMT(VG_USERREQ_DRD_PRE_RWLOCK_INIT,
                                   rwlock, 0, 0, 0, 0);
   CALL_FN_W_WWW(ret, fn, rwlock, type, arg);
   VALGRIND_DO_CLIENT_REQUEST_STMT(VG_USERREQ_DRD_POST_RWLOCK_INIT,
                                   rwlock, 0, 0, 0, 0);
   return ret;
}

PTH_FUNCS(int, rwlockZuinit, rwlock_init_intercept,
          (rwlock_t *rwlock, int type, void *arg),
          (rwlock, type, arg));
#endif /* VGO_solaris */

static __always_inline
int pthread_rwlock_destroy_intercept(pthread_rwlock_t* rwlock)
{
   int   ret;
   OrigFn fn;
   VALGRIND_GET_ORIG_FN(fn);
   VALGRIND_DO_CLIENT_REQUEST_STMT(VG_USERREQ_DRD_PRE_RWLOCK_DESTROY,
                                   rwlock, 0, 0, 0, 0);
   CALL_FN_W_W(ret, fn, rwlock);
   VALGRIND_DO_CLIENT_REQUEST_STMT(VG_USERREQ_DRD_POST_RWLOCK_DESTROY,
                                   rwlock, 0, 0, 0, 0);
   return ret;
}

#if defined(VGO_solaris)
/* On Solaris, pthread_rwlock_destroy is a weak alias to rwlock_destroy. */
PTH_FUNCS(int,
          rwlockZudestroy, pthread_rwlock_destroy_intercept,
          (pthread_rwlock_t *rwlock), (rwlock));
#else
PTH_FUNCS(int,
          pthreadZurwlockZudestroy, pthread_rwlock_destroy_intercept,
          (pthread_rwlock_t* rwlock), (rwlock));
#endif /* VGO_solaris */

static __always_inline
int pthread_rwlock_rdlock_intercept(pthread_rwlock_t* rwlock)
{
   int   ret;
   OrigFn fn;
   VALGRIND_GET_ORIG_FN(fn);
   VALGRIND_DO_CLIENT_REQUEST_STMT(VG_USERREQ_DRD_PRE_RWLOCK_RDLOCK,
                                   rwlock, 0, 0, 0, 0);
   CALL_FN_W_W(ret, fn, rwlock);
   VALGRIND_DO_CLIENT_REQUEST_STMT(VG_USERREQ_DRD_POST_RWLOCK_RDLOCK,
                                   rwlock, ret == 0, 0, 0, 0);
   return ret;
}

#if defined(VGO_solaris)
/* On Solaris, pthread_rwlock_rdlock is a weak alias to rw_rdlock. */
PTH_FUNCS(int,
          rwZurdlock, pthread_rwlock_rdlock_intercept,
          (pthread_rwlock_t *rwlock), (rwlock));
#else
PTH_FUNCS(int,
          pthreadZurwlockZurdlock, pthread_rwlock_rdlock_intercept,
          (pthread_rwlock_t* rwlock), (rwlock));
#endif /* VGO_solaris */

#if defined(VGO_solaris)
/* Internal to libc. */
static __always_inline
void lrw_rdlock_intercept(rwlock_t *rwlock)
{
   OrigFn fn;
   VALGRIND_GET_ORIG_FN(fn);
   VALGRIND_DO_CLIENT_REQUEST_STMT(VG_USERREQ_DRD_PRE_RWLOCK_RDLOCK,
                                   rwlock, 0, 0, 0, 0);
   CALL_FN_v_W(fn, rwlock);
   VALGRIND_DO_CLIENT_REQUEST_STMT(VG_USERREQ_DRD_POST_RWLOCK_RDLOCK,
                                   rwlock, True /* took_lock */, 0, 0, 0);
}

PTH_FUNCS(void, lrwZurdlock, lrw_rdlock_intercept,
          (rwlock_t *rwlock), (rwlock));
#endif /* VGO_solaris */

static __always_inline
int pthread_rwlock_wrlock_intercept(pthread_rwlock_t* rwlock)
{
   int   ret;
   OrigFn fn;
   VALGRIND_GET_ORIG_FN(fn);
   VALGRIND_DO_CLIENT_REQUEST_STMT(VG_USERREQ_DRD_PRE_RWLOCK_WRLOCK,
                                   rwlock, 0, 0, 0, 0);
   CALL_FN_W_W(ret, fn, rwlock);
   VALGRIND_DO_CLIENT_REQUEST_STMT(VG_USERREQ_DRD_POST_RWLOCK_WRLOCK,
                                   rwlock, ret == 0, 0, 0, 0);
   return ret;
}

#if defined(VGO_solaris)
/* On Solaris, pthread_rwlock_wrlock is a weak alias to rw_wrlock. */
PTH_FUNCS(int,
          rwZuwrlock, pthread_rwlock_wrlock_intercept,
          (pthread_rwlock_t *rwlock), (rwlock));
#else
PTH_FUNCS(int,
          pthreadZurwlockZuwrlock, pthread_rwlock_wrlock_intercept,
          (pthread_rwlock_t* rwlock), (rwlock));
#endif /* VGO_solaris */

#if defined(VGO_solaris)
/* Internal to libc. */
static __always_inline
void lrw_wrlock_intercept(rwlock_t *rwlock)
{
   OrigFn fn;
   VALGRIND_GET_ORIG_FN(fn);
   VALGRIND_DO_CLIENT_REQUEST_STMT(VG_USERREQ_DRD_PRE_RWLOCK_WRLOCK,
                                   rwlock, 0, 0, 0, 0);
   CALL_FN_v_W(fn, rwlock);
   VALGRIND_DO_CLIENT_REQUEST_STMT(VG_USERREQ_DRD_POST_RWLOCK_WRLOCK,
                                   rwlock, True /* took_lock */, 0, 0, 0);
}

PTH_FUNCS(void, lrwZuwrlock, lrw_wrlock_intercept,
          (rwlock_t *rwlock), (rwlock));
#endif /* VGO_solaris */

static __always_inline
int pthread_rwlock_timedrdlock_intercept(pthread_rwlock_t* rwlock,
                                         const struct timespec *timeout)
{
   int   ret;
   OrigFn fn;
   VALGRIND_GET_ORIG_FN(fn);
   VALGRIND_DO_CLIENT_REQUEST_STMT(VG_USERREQ_DRD_PRE_RWLOCK_RDLOCK,
                                   rwlock, 0, 0, 0, 0);
   CALL_FN_W_WW(ret, fn, rwlock, timeout);
   VALGRIND_DO_CLIENT_REQUEST_STMT(VG_USERREQ_DRD_POST_RWLOCK_RDLOCK,
                                   rwlock, ret == 0, 0, 0, 0);
   return ret;
}

PTH_FUNCS(int,
          pthreadZurwlockZutimedrdlock, pthread_rwlock_timedrdlock_intercept,
          (pthread_rwlock_t* rwlock, const struct timespec *timeout),
          (rwlock, timeout));
#if defined(VGO_solaris)
PTH_FUNCS(int, pthreadZurwlockZureltimedrdlockZunp,
          pthread_rwlock_timedrdlock_intercept,
          (pthread_rwlock_t *rwlock, const struct timespec *timeout),
          (rwlock, timeout));
#endif /* VGO_solaris */


#if defined(HAVE_CLOCKID_T)
static __always_inline
int pthread_rwlock_clockrdlock_intercept(pthread_rwlock_t* rwlock,
                                         clockid_t clockid,
                                         const struct timespec *timeout)
{
   int   ret;
   OrigFn fn;
   VALGRIND_GET_ORIG_FN(fn);
   VALGRIND_DO_CLIENT_REQUEST_STMT(VG_USERREQ_DRD_PRE_RWLOCK_RDLOCK,
                                   rwlock, 0, 0, 0, 0);
   CALL_FN_W_WWW(ret, fn, rwlock, clockid, timeout);
   VALGRIND_DO_CLIENT_REQUEST_STMT(VG_USERREQ_DRD_POST_RWLOCK_RDLOCK,
                                   rwlock, ret == 0, 0, 0, 0);
   return ret;
}

PTH_FUNCS(int,
          pthreadZurwlockZuclockrdlock, pthread_rwlock_clockrdlock_intercept,
          (pthread_rwlock_t* rwlock, clockid_t clockid, const struct timespec *timeout),
          (rwlock, clockid, timeout));
#endif

static __always_inline
int pthread_rwlock_timedwrlock_intercept(pthread_rwlock_t* rwlock,
                                         const struct timespec *timeout)
{
   int   ret;
   OrigFn fn;
   VALGRIND_GET_ORIG_FN(fn);
   VALGRIND_DO_CLIENT_REQUEST_STMT(VG_USERREQ_DRD_PRE_RWLOCK_WRLOCK,
                                   rwlock, 0, 0, 0, 0);
   CALL_FN_W_WW(ret, fn, rwlock, timeout);
   VALGRIND_DO_CLIENT_REQUEST_STMT(VG_USERREQ_DRD_POST_RWLOCK_WRLOCK,
                                   rwlock, ret == 0, 0, 0, 0);
   return ret;
}

PTH_FUNCS(int,
          pthreadZurwlockZutimedwrlock, pthread_rwlock_timedwrlock_intercept,
          (pthread_rwlock_t* rwlock, const struct timespec *timeout),
          (rwlock, timeout));
#if defined(VGO_solaris)
PTH_FUNCS(int, pthreadZurwlockZureltimedwrlockZunp,
          pthread_rwlock_timedwrlock_intercept,
          (pthread_rwlock_t *rwlock, const struct timespec *timeout),
          (rwlock, timeout));
#endif /* VGO_solaris */


#if defined(HAVE_CLOCKID_T)
static __always_inline
int pthread_rwlock_clockwrlock_intercept(pthread_rwlock_t* rwlock,
                                         clockid_t clockid,
                                         const struct timespec *timeout)
{
   int   ret;
   OrigFn fn;
   VALGRIND_GET_ORIG_FN(fn);
   VALGRIND_DO_CLIENT_REQUEST_STMT(VG_USERREQ_DRD_PRE_RWLOCK_WRLOCK,
                                   rwlock, 0, 0, 0, 0);
   CALL_FN_W_WWW(ret, fn, rwlock, clockid, timeout);
   VALGRIND_DO_CLIENT_REQUEST_STMT(VG_USERREQ_DRD_POST_RWLOCK_WRLOCK,
                                   rwlock, ret == 0, 0, 0, 0);
   return ret;
}

PTH_FUNCS(int,
          pthreadZurwlockZuclockwrlock, pthread_rwlock_clockwrlock_intercept,
          (pthread_rwlock_t* rwlock, clockid_t clockid, const struct timespec *timeout),
          (rwlock, clockid, timeout));
#endif


static __always_inline
int pthread_rwlock_tryrdlock_intercept(pthread_rwlock_t* rwlock)
{
   int   ret;
   OrigFn fn;
   VALGRIND_GET_ORIG_FN(fn);
   VALGRIND_DO_CLIENT_REQUEST_STMT(VG_USERREQ_DRD_PRE_RWLOCK_RDLOCK,
                                   rwlock, 0, 0, 0, 0);
   CALL_FN_W_W(ret, fn, rwlock);
   VALGRIND_DO_CLIENT_REQUEST_STMT(VG_USERREQ_DRD_POST_RWLOCK_RDLOCK,
                                   rwlock, ret == 0, 0, 0, 0);
   return ret;
}

#if defined(VGO_solaris)
/* On Solaris, pthread_rwlock_tryrdlock is a weak alias to rw_tryrdlock. */
PTH_FUNCS(int,
          rwZutryrdlock, pthread_rwlock_tryrdlock_intercept,
          (pthread_rwlock_t *rwlock), (rwlock));
#else
PTH_FUNCS(int,
          pthreadZurwlockZutryrdlock, pthread_rwlock_tryrdlock_intercept,
          (pthread_rwlock_t* rwlock), (rwlock));
#endif /* VGO_solaris */

static __always_inline
int pthread_rwlock_trywrlock_intercept(pthread_rwlock_t* rwlock)
{
   int   ret;
   OrigFn fn;
   VALGRIND_GET_ORIG_FN(fn);
   VALGRIND_DO_CLIENT_REQUEST_STMT(VG_USERREQ_DRD_PRE_RWLOCK_WRLOCK,
                                   rwlock, 0, 0, 0, 0);
   CALL_FN_W_W(ret, fn, rwlock);
   VALGRIND_DO_CLIENT_REQUEST_STMT(VG_USERREQ_DRD_POST_RWLOCK_WRLOCK,
                                   rwlock, ret == 0, 0, 0, 0);
   return ret;
}

#if defined(VGO_solaris)
/* On Solaris, pthread_rwlock_trywrlock is a weak alias to rw_trywrlock. */
PTH_FUNCS(int,
          rwZutrywrlock, pthread_rwlock_trywrlock_intercept,
          (pthread_rwlock_t *rwlock), (rwlock));
#else
PTH_FUNCS(int,
          pthreadZurwlockZutrywrlock, pthread_rwlock_trywrlock_intercept,
          (pthread_rwlock_t* rwlock), (rwlock));
#endif /* VGO_solaris */

static __always_inline
int pthread_rwlock_unlock_intercept(pthread_rwlock_t* rwlock)
{
   int   ret;
   OrigFn fn;
   VALGRIND_GET_ORIG_FN(fn);
   VALGRIND_DO_CLIENT_REQUEST_STMT(VG_USERREQ_DRD_PRE_RWLOCK_UNLOCK,
                                   rwlock, 0, 0, 0, 0);
   CALL_FN_W_W(ret, fn, rwlock);
   VALGRIND_DO_CLIENT_REQUEST_STMT(VG_USERREQ_DRD_POST_RWLOCK_UNLOCK,
                                   rwlock, ret == 0, 0, 0, 0);
   return ret;
}

#if defined(VGO_solaris)
/* On Solaris, pthread_rwlock_unlock is a weak alias to rw_unlock. */
PTH_FUNCS(int,
          rwZuunlock, pthread_rwlock_unlock_intercept,
          (pthread_rwlock_t *rwlock), (rwlock));
#else
PTH_FUNCS(int,
          pthreadZurwlockZuunlock, pthread_rwlock_unlock_intercept,
          (pthread_rwlock_t* rwlock), (rwlock));
#endif /* VGO_solaris */

#endif /* defined(HAVE_PTHREAD_RWLOCK_T) */
