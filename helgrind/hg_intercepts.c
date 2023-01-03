
/*--------------------------------------------------------------------*/
/*--- pthread intercepts for thread checking.                      ---*/
/*---                                              hg_intercepts.c ---*/
/*--------------------------------------------------------------------*/

/*
   This file is part of Helgrind, a Valgrind tool for detecting errors
   in threaded programs.

   Copyright (C) 2007-2017 OpenWorks LLP
      info@open-works.co.uk

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

   Neither the names of the U.S. Department of Energy nor the
   University of California nor the names of its contributors may be
   used to endorse or promote products derived from this software
   without prior written permission.
*/

/* RUNS ON SIMULATED CPU
   Interceptors for pthread_* functions, so that tc_main can see
   significant thread events. 

   Important: when adding a function wrapper to this file, remember to
   add a test case to tc20_verifywrap.c.  A common cause of failure is
   for wrappers to not engage on different distros, and
   tc20_verifywrap essentially checks that each wrapper is really
   doing something.
*/

// DDD: for Darwin, need to have non-"@*"-suffixed versions for all pthread
// functions that currently have them.
// Note also, in the comments and code below, all Darwin symbols start
// with a leading underscore, which is not shown either in the comments
// nor in the redirect specs.


#include "pub_tool_basics.h"
#include "pub_tool_redir.h"
#include "pub_tool_clreq.h"
#include "helgrind.h"
#include "config.h"


#if defined(VGO_solaris)
/* See porting comments in drd/drd_pthread_intercepts.c
   However when a POSIX threads API function (for example pthread_cond_init)
   is built upon the Solaris one (cond_init), intercept only the bottom one.
   Helgrind does not contain generic synchronization nesting like DRD
   and double intercept confuses it. */
#include <synch.h>
#include <thread.h>
#endif /* VGO_solaris */


#define TRACE_PTH_FNS 0
#define TRACE_QT4_FNS 0
#define TRACE_GNAT_FNS 0


/*----------------------------------------------------------------*/
/*---                                                          ---*/
/*----------------------------------------------------------------*/

#if defined(VGO_solaris)
/* pthread_t is typedef'd to 'unsigned int' but in DO_CREQ_* macros
   sizeof(Word) is expected. */
#define CREQ_PTHREAD_T Word
#define SEM_ERROR ret
#else
#define CREQ_PTHREAD_T pthread_t
#define SEM_ERROR errno
#endif /* VGO_solaris */

#define HG_EXPAND(tok) #tok
#define HG_STR(tok) HG_EXPAND(tok)
#define HG_WEAK_ALIAS(name, aliasname) \
  extern __typeof (name) aliasname __attribute__ ((weak, alias(HG_STR(name))))

#if defined(VG_WRAP_THREAD_FUNCTION_LIBPTHREAD_ONLY)
#define PTH_FUNC(ret_ty, f, args...) \
   ret_ty I_WRAP_SONAME_FNNAME_ZZ(VG_Z_LIBPTHREAD_SONAME,f)(args); \
   ret_ty I_WRAP_SONAME_FNNAME_ZZ(VG_Z_LIBPTHREAD_SONAME,f)(args)
#elif defined(VG_WRAP_THREAD_FUNCTION_LIBC_AND_LIBPTHREAD)
#define PTH_FUNC(ret_ty, f, args...) \
   ret_ty I_WRAP_SONAME_FNNAME_ZZ(VG_Z_LIBPTHREAD_SONAME,f)(args); \
   HG_WEAK_ALIAS(I_WRAP_SONAME_FNNAME_ZZ(VG_Z_LIBPTHREAD_SONAME,f), I_WRAP_SONAME_FNNAME_ZZ(VG_Z_LIBC_SONAME,f)); \
   ret_ty I_WRAP_SONAME_FNNAME_ZZ(VG_Z_LIBPTHREAD_SONAME,f)(args)
#elif defined(VG_WRAP_THREAD_FUNCTION_LIBC_ONLY)
#define PTH_FUNC(ret_ty, f, args...) \
   ret_ty I_WRAP_SONAME_FNNAME_ZZ(VG_Z_LIBC_SONAME,f)(args); \
   ret_ty I_WRAP_SONAME_FNNAME_ZZ(VG_Z_LIBC_SONAME,f)(args)
#else
#  error "Unknown platform/thread wrapping"
#endif

#if defined(VGO_freebsd)
#define LIBC_FUNC(ret_ty, f, args...) \
   ret_ty I_WRAP_SONAME_FNNAME_ZZ(VG_Z_LIBC_SONAME,f)(args); \
   ret_ty I_WRAP_SONAME_FNNAME_ZZ(VG_Z_LIBC_SONAME,f)(args)
#endif

// Do a client request.  These are macros rather than a functions so
// as to avoid having an extra frame in stack traces.

// NB: these duplicate definitions in helgrind.h.  But here, we
// can have better typing (Word etc) and assertions, whereas
// in helgrind.h we can't.  Obviously it's important the two
// sets of definitions are kept in sync.

// nuke the previous definitions
#undef DO_CREQ_v_W
#undef DO_CREQ_v_WW
#undef DO_CREQ_W_WW
#undef DO_CREQ_v_WWW

#define DO_CREQ_v_W(_creqF, _ty1F,_arg1F)                \
   do {                                                  \
      Word _arg1;                                        \
      assert(sizeof(_ty1F) == sizeof(Word));             \
      _arg1 = (Word)(_arg1F);                            \
      VALGRIND_DO_CLIENT_REQUEST_STMT((_creqF),          \
                                 _arg1, 0,0,0,0);        \
   } while (0)

#define DO_CREQ_v_WW(_creqF, _ty1F,_arg1F, _ty2F,_arg2F) \
   do {                                                  \
      Word _arg1, _arg2;                                 \
      assert(sizeof(_ty1F) == sizeof(Word));             \
      assert(sizeof(_ty2F) == sizeof(Word));             \
      _arg1 = (Word)(_arg1F);                            \
      _arg2 = (Word)(_arg2F);                            \
      VALGRIND_DO_CLIENT_REQUEST_STMT((_creqF),          \
                                 _arg1,_arg2,0,0,0);     \
   } while (0)

#define DO_CREQ_W_WW(_resF, _creqF, _ty1F,_arg1F,        \
                     _ty2F,_arg2F)                       \
   do {                                                  \
      Word _res, _arg1, _arg2;                           \
      assert(sizeof(_ty1F) == sizeof(Word));             \
      assert(sizeof(_ty2F) == sizeof(Word));             \
      _arg1 = (Word)(_arg1F);                            \
      _arg2 = (Word)(_arg2F);                            \
      _res = VALGRIND_DO_CLIENT_REQUEST_EXPR(2,          \
                                 (_creqF),               \
                                 _arg1,_arg2,0,0,0);     \
      _resF = _res;                                      \
   } while (0)

#define DO_CREQ_v_WWW(_creqF, _ty1F,_arg1F,              \
                      _ty2F,_arg2F, _ty3F, _arg3F)       \
   do {                                                  \
      Word _arg1, _arg2, _arg3;                          \
      assert(sizeof(_ty1F) == sizeof(Word));             \
      assert(sizeof(_ty2F) == sizeof(Word));             \
      assert(sizeof(_ty3F) == sizeof(Word));             \
      _arg1 = (Word)(_arg1F);                            \
      _arg2 = (Word)(_arg2F);                            \
      _arg3 = (Word)(_arg3F);                            \
      VALGRIND_DO_CLIENT_REQUEST_STMT((_creqF),          \
                                 _arg1,_arg2,_arg3,0,0); \
   } while (0)

#define DO_CREQ_v_WWWW(_creqF, _ty1F,_arg1F,             \
                       _ty2F, _arg2F, _ty3F, _arg3F,     \
                       _ty4F, _arg4F)                    \
   do {                                                  \
      Word _arg1, _arg2, _arg3, _arg4;                   \
      assert(sizeof(_ty1F) == sizeof(Word));             \
      assert(sizeof(_ty2F) == sizeof(Word));             \
      assert(sizeof(_ty3F) == sizeof(Word));             \
      assert(sizeof(_ty4F) == sizeof(Word));             \
      _arg1 = (Word)(_arg1F);                            \
      _arg2 = (Word)(_arg2F);                            \
      _arg3 = (Word)(_arg3F);                            \
      _arg4 = (Word)(_arg4F);                            \
      VALGRIND_DO_CLIENT_REQUEST_STMT((_creqF),          \
                             _arg1,_arg2,_arg3,_arg4,0); \
   } while (0)

#define DO_PthAPIerror(_fnnameF, _errF)                  \
   do {                                                  \
      const char* _fnname = (_fnnameF);                  \
      long  _err    = (long)(int)(_errF);                \
      const char* _errstr = lame_strerror(_err);         \
      DO_CREQ_v_WWW(_VG_USERREQ__HG_PTH_API_ERROR,       \
                    char*,_fnname,                       \
                    long,_err, char*,_errstr);           \
   } while (0)


/* Needed for older glibcs (2.3 and older, at least) who don't
   otherwise "know" about pthread_rwlock_anything or about
   PTHREAD_MUTEX_RECURSIVE (amongst things). */
#define _GNU_SOURCE 1

#include <stdio.h>
#include <assert.h>
#include <errno.h>
#include <pthread.h>

/* A standalone memcmp. */
__attribute__((noinline))
static int my_memcmp ( const void* ptr1, const void* ptr2, size_t size)
{
   const unsigned char* uchar_ptr1 = (const unsigned char*) ptr1;
   const unsigned char* uchar_ptr2 = (const unsigned char*) ptr2;
   size_t i;
   for (i = 0; i < size; ++i) {
      if (uchar_ptr1[i] != uchar_ptr2[i])
         return (uchar_ptr1[i] < uchar_ptr2[i]) ? -1 : 1;
   }
   return 0;
}

/* A lame version of strerror which doesn't use the real libc
   strerror_r, since using the latter just generates endless more
   threading errors (glibc goes off and does tons of crap w.r.t.
   locales etc) */
static const HChar* lame_strerror ( long err )
{
   switch (err) {
      case EPERM:       return "EPERM: Operation not permitted";
      case ENOENT:      return "ENOENT: No such file or directory";
      case ESRCH:       return "ESRCH: No such process";
      case EINTR:       return "EINTR: Interrupted system call";
      case EBADF:       return "EBADF: Bad file number";
      case EAGAIN:      return "EAGAIN: Try again";
      case ENOMEM:      return "ENOMEM: Out of memory";
      case EACCES:      return "EACCES: Permission denied";
      case EFAULT:      return "EFAULT: Bad address";
      case EEXIST:      return "EEXIST: File exists";
      case EINVAL:      return "EINVAL: Invalid argument";
      case EMFILE:      return "EMFILE: Too many open files";
      case ENOSYS:      return "ENOSYS: Function not implemented";
      case EOVERFLOW:   return "EOVERFLOW: Value too large "
                               "for defined data type";
      case EBUSY:       return "EBUSY: Device or resource busy";
      case ETIMEDOUT:   return "ETIMEDOUT: Connection timed out";
      case EDEADLK:     return "EDEADLK: Resource deadlock would occur";
      case EOPNOTSUPP:  return "EOPNOTSUPP: Operation not supported on "
                               "transport endpoint"; /* honest, guv */
#if !defined(VGO_freebsd)
      case ETIME:       return "ETIME: Timer expired";
#endif
      default:          return "hg_intercepts.c: lame_strerror(): "
                               "unhandled case -- please fix me!";
   }
}

#if defined(VGO_solaris)
/*
 * Solaris provides higher throughput, parallelism and scalability than other
 * operating systems, at the cost of more fine-grained locking activity.
 * This means for example that when a thread is created under Linux, just one
 * big lock in glibc is used for all thread setup. Solaris libc uses several
 * fine-grained locks and the creator thread resumes its activities as soon
 * as possible, leaving for example stack and TLS setup activities to the
 * created thread.
 *
 * This situation confuses Helgrind as it assumes there is some false ordering
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
 * This also means that Helgrind does not report race conditions in libc (when
 * --ignore-thread-creation=yes) and runtime linker itself (unconditionally)
 * during these ignored sequences.
 */

#include "pub_tool_libcassert.h"
#include "pub_tool_vki.h"

/*
 * Original function pointers for _ti_bind_guard() and _ti_bind_clear()
 * from libc. They are intercepted in function wrapper of _ld_libc().
 */
typedef int (*hg_rtld_guard_fn)(int flags);
static hg_rtld_guard_fn hg_rtld_bind_guard = NULL;
static hg_rtld_guard_fn hg_rtld_bind_clear = NULL;

static void hg_init(void) __attribute__((constructor));
static void hg_init(void)
{
   if ((hg_rtld_bind_guard == NULL) || (hg_rtld_bind_clear == NULL)) {
      fprintf(stderr,
"Bind guard functions for the runtime linker (ld.so.1) were not intercepted.\n"
"This means the interface between libc and runtime linker changed\n"
"and Helgrind needs to be ported properly. Giving up.\n");
      tl_assert(0);
   }
}

/*
 * Intercepts for _ti_bind_guard() and _ti_bind_clear() functions from libc.
 * These are intercepted during _ld_libc() call by identifying CI_BIND_GUARD
 * and CI_BIND_CLEAR, to provide resilience against function renaming.
 */
static int _ti_bind_guard_intercept_WRK(int flags)
{
   VALGRIND_DO_CLIENT_REQUEST_STMT(_VG_USERREQ__HG_RTLD_BIND_GUARD,
                                   flags, 0, 0, 0, 0);
   return hg_rtld_bind_guard(flags);
}

static int _ti_bind_clear_intercept_WRK(int flags)
{
   int ret = hg_rtld_bind_clear(flags);
   VALGRIND_DO_CLIENT_REQUEST_STMT(_VG_USERREQ__HG_RTLD_BIND_CLEAR,
                                   flags, 0, 0, 0, 0);
   return ret;
}

/*
 * Wrapped _ld_libc() from the runtime linker ld.so.1.
 */
void I_WRAP_SONAME_FNNAME_ZZ(VG_Z_LD_SO_1, ZuldZulibc)(vki_Lc_interface *ptr);
void I_WRAP_SONAME_FNNAME_ZZ(VG_Z_LD_SO_1, ZuldZulibc)(vki_Lc_interface *ptr)
{
   OrigFn fn;
   int    tag;

   VALGRIND_GET_ORIG_FN(fn);

   vki_Lc_interface *funcs = ptr;
   for (tag = funcs->ci_tag; tag != 0; tag = (++funcs)->ci_tag) {
      switch (tag) {
      case VKI_CI_BIND_GUARD:
         if (funcs->vki_ci_un.ci_func != _ti_bind_guard_intercept_WRK) {
            hg_rtld_bind_guard = funcs->vki_ci_un.ci_func;
            funcs->vki_ci_un.ci_func = _ti_bind_guard_intercept_WRK;
         }
         break;
      case VKI_CI_BIND_CLEAR:
         if (funcs->vki_ci_un.ci_func != _ti_bind_clear_intercept_WRK) {
            hg_rtld_bind_clear = funcs->vki_ci_un.ci_func;
            funcs->vki_ci_un.ci_func = _ti_bind_clear_intercept_WRK;
         }
         break;
      }
   }

   CALL_FN_v_W(fn, ptr);
}
#endif /* VGO_solaris */


/*----------------------------------------------------------------*/
/*--- pthread_create, pthread_join, pthread_exit               ---*/
/*----------------------------------------------------------------*/

static void* mythread_wrapper ( void* xargsV )
{
   volatile Word* xargs = (volatile Word*) xargsV;
   void*(*fn)(void*) = (void*(*)(void*))xargs[0];
   void* arg         = (void*)xargs[1];
   pthread_t me = pthread_self();
   /* Tell the tool what my pthread_t is. */
   DO_CREQ_v_W(_VG_USERREQ__HG_SET_MY_PTHREAD_T, CREQ_PTHREAD_T, me);
   /* allow the parent to proceed.  We can't let it proceed until
      we're ready because (1) we need to make sure it doesn't exit and
      hence deallocate xargs[] while we still need it, and (2) we
      don't want either parent nor child to proceed until the tool has
      been notified of the child's pthread_t.

      Note that parent and child access args[] without a lock,
      effectively using args[2] as a spinlock in order to get the
      parent to wait until the child passes this point.  The parent
      disables checking on xargs[] before creating the child and
      re-enables it once the child goes past this point, so the user
      never sees the race.  The previous approach (suppressing the
      resulting error) was flawed, because it could leave shadow
      memory for args[] in a state in which subsequent use of it by
      the parent would report further races. */
   xargs[2] = 0;
   /* Now we can no longer safely use xargs[]. */
   return (void*) fn( (void*)arg );
}

//-----------------------------------------------------------
// glibc:  pthread_create@GLIBC_2.0
// glibc:  pthread_create@@GLIBC_2.1
// glibc:  pthread_create@@GLIBC_2.2.5
// darwin: pthread_create
// darwin: pthread_create_suspended_np (trapped)
// FreeBSD: pthread_create
//
/* ensure this has its own frame, so as to make it more distinguishable
   in suppressions */
__attribute__((noinline))
static int pthread_create_WRK(pthread_t *thread, const pthread_attr_t *attr,
                              void *(*start) (void *), void *arg)
{
   int    ret;
   OrigFn fn;
   volatile Word xargs[3];

   VALGRIND_GET_ORIG_FN(fn);
   if (TRACE_PTH_FNS) {
      fprintf(stderr, "<< pthread_create wrapper"); fflush(stderr);
   }
   xargs[0] = (Word)start;
   xargs[1] = (Word)arg;
   xargs[2] = 1; /* serves as a spinlock -- sigh */
   /* Disable checking on the spinlock and the two words used to
      convey args to the child.  Basically we need to make it appear
      as if the child never accessed this area, since merely
      suppressing the resulting races does not address the issue that
      that piece of the parent's stack winds up in the "wrong" state
      and therefore may give rise to mysterious races when the parent
      comes to re-use this piece of stack in some other frame. */
   VALGRIND_HG_DISABLE_CHECKING(&xargs, sizeof(xargs));

   VALGRIND_DO_CLIENT_REQUEST_STMT(_VG_USERREQ__HG_PTHREAD_CREATE_BEGIN,
                                   0, 0, 0, 0, 0);
   CALL_FN_W_WWWW(ret, fn, thread,attr,mythread_wrapper,&xargs[0]);
   VALGRIND_DO_CLIENT_REQUEST_STMT(_VG_USERREQ__HG_PTHREAD_CREATE_END,
                                   0, 0, 0, 0, 0);

   if (ret == 0) {
      /* we have to wait for the child to notify the tool of its
         pthread_t before continuing */
      while (xargs[2] != 0) {
         /* Do nothing.  We need to spin until the child writes to
            xargs[2].  However, that can lead to starvation in the
            child and very long delays (eg, tc19_shadowmem on
            ppc64-linux Fedora Core 6).  So yield the cpu if we can,
            to let the child run at the earliest available
            opportunity. */
         sched_yield();
      }
   } else { 
      DO_PthAPIerror( "pthread_create", ret );
   }

   /* Reenable checking on the area previously used to communicate
      with the child. */
   VALGRIND_HG_ENABLE_CHECKING(&xargs, sizeof(xargs));

   if (TRACE_PTH_FNS) {
      fprintf(stderr, " :: pth_create -> %d >>\n", ret);
   }
   return ret;
}
#if defined(VGO_linux)
   PTH_FUNC(int, pthreadZucreateZAZa, // pthread_create@*
                 pthread_t *thread, const pthread_attr_t *attr,
                 void *(*start) (void *), void *arg) {
      return pthread_create_WRK(thread, attr, start, arg);
   }
#elif defined(VGO_freebsd)
   PTH_FUNC(int, pthreadZucreate, // pthread_create
                 pthread_t *thread, const pthread_attr_t *attr,
                 void *(*start) (void *), void *arg) {
      return pthread_create_WRK(thread, attr, start, arg);
   }
#elif defined(VGO_darwin)
   PTH_FUNC(int, pthreadZucreate, // pthread_create
                 pthread_t *thread, const pthread_attr_t *attr,
                 void *(*start) (void *), void *arg) {
      return pthread_create_WRK(thread, attr, start, arg);
   }
   PTH_FUNC(int, pthreadZucreateZuZa, // pthread_create_*
                 pthread_t *thread, const pthread_attr_t *attr,
                 void *(*start) (void *), void *arg) {
      // trap anything else
      assert(0);
   }
#elif defined(VGO_solaris)
   PTH_FUNC(int, pthreadZucreate, // pthread_create
                 pthread_t *thread, const pthread_attr_t *attr,
                 void *(*start) (void *), void *arg) {
      return pthread_create_WRK(thread, attr, start, arg);
   }
#else
#  error "Unsupported OS"
#endif

#if defined(VGO_solaris)
/* Solaris also provides thr_create() in addition to pthread_create().
 * Both pthread_create(3C) and thr_create(3C) are based on private
 * _thrp_create().
 */
__attribute__((noinline))
static int thr_create_WRK(void *stk, size_t stksize, void *(*start)(void *),
                          void *arg, long flags, thread_t *new_thread)
{
   int    ret;
   OrigFn fn;
   volatile Word xargs[3];

   VALGRIND_GET_ORIG_FN(fn);
   if (TRACE_PTH_FNS) {
      fprintf(stderr, "<< thr_create wrapper"); fflush(stderr);
   }
   xargs[0] = (Word)start;
   xargs[1] = (Word)arg;
   xargs[2] = 1; /* serves as a spinlock -- sigh */
   /* See comments in pthread_create_WRK() */
   VALGRIND_HG_DISABLE_CHECKING(&xargs, sizeof(xargs));

   VALGRIND_DO_CLIENT_REQUEST_STMT(_VG_USERREQ__HG_PTHREAD_CREATE_BEGIN,
                                   0, 0, 0, 0, 0);
   CALL_FN_W_6W(ret, fn, stk, stksize, mythread_wrapper, start, flags,
                new_thread);
   VALGRIND_DO_CLIENT_REQUEST_STMT(_VG_USERREQ__HG_PTHREAD_CREATE_END,
                                   0, 0, 0, 0, 0);

   if (ret == 0) {
      while (xargs[2] != 0) {
         /* See comments in pthread_create_WRK(). */
         sched_yield();
      }
   } else {
      DO_PthAPIerror("thr_create", ret);
   }

   VALGRIND_HG_ENABLE_CHECKING(&xargs, sizeof(xargs));

   if (TRACE_PTH_FNS) {
      fprintf(stderr, " :: thr_create -> %d >>\n", ret);
   }
   return ret;
}
   PTH_FUNC(int, thrZucreate, // thr_create
                 void *stk, size_t stksize, void *(*start)(void *),
                 void *arg, long flags, thread_t *new_thread) {
      return thr_create_WRK(stk, stksize, start, arg, flags, new_thread);
   }
#endif /* VGO_solaris */


//-----------------------------------------------------------
// glibc:  pthread_join
// darwin: pthread_join
// darwin: pthread_join$NOCANCEL$UNIX2003
// darwin  pthread_join$UNIX2003
// FreeBSD: pthread_join
__attribute__((noinline))
static int pthread_join_WRK(pthread_t thread, void** value_pointer)
{
   int ret;
   OrigFn fn;
   VALGRIND_GET_ORIG_FN(fn);
   if (TRACE_PTH_FNS) {
      fprintf(stderr, "<< pthread_join wrapper"); fflush(stderr);
   }

   CALL_FN_W_WW(ret, fn, thread,value_pointer);

   /* At least with NPTL as the thread library, this is safe because
      it is guaranteed (by NPTL) that the joiner will completely gone
      before pthread_join (the original) returns.  See email below.*/
   if (ret == 0 /*success*/) {
      DO_CREQ_v_W(_VG_USERREQ__HG_PTHREAD_JOIN_POST, CREQ_PTHREAD_T, thread);
   } else { 
      DO_PthAPIerror( "pthread_join", ret );
   }

   if (TRACE_PTH_FNS) {
      fprintf(stderr, " :: pth_join -> %d >>\n", ret);
   }
   return ret;
}
#if defined(VGO_linux)
   PTH_FUNC(int, pthreadZujoin, // pthread_join
            pthread_t thread, void** value_pointer) {
      return pthread_join_WRK(thread, value_pointer);
   }
#elif defined(VGO_freebsd)
   PTH_FUNC(int, pthreadZujoin, // pthread_join
            pthread_t thread, void** value_pointer) {
      return pthread_join_WRK(thread, value_pointer);
   }
#elif defined(VGO_darwin)
   PTH_FUNC(int, pthreadZujoinZa, // pthread_join*
            pthread_t thread, void** value_pointer) {
      return pthread_join_WRK(thread, value_pointer);
   }
#elif defined(VGO_solaris)
   PTH_FUNC(int, pthreadZujoin, // pthread_join
            pthread_t thread, void** value_pointer) {
      return pthread_join_WRK(thread, value_pointer);
   }
#else
#  error "Unsupported OS"
#endif


/* Behaviour of pthread_join on NPTL:

Me:
I have a question re the NPTL pthread_join implementation.

  Suppose I am the thread 'stayer'.  

  If I call pthread_join(quitter), is it guaranteed that the
  thread 'quitter' has really exited before pthread_join returns?

  IOW, is it guaranteed that 'quitter' will not execute any further
  instructions after pthread_join returns?

I believe this is true based on the following analysis of
glibc-2.5 sources.  However am not 100% sure and would appreciate
confirmation.

  'quitter' will be running start_thread() in nptl/pthread_create.c

  The last action of start_thread() is to exit via
  __exit_thread_inline(0), which simply does sys_exit 
  (nptl/pthread_create.c:403)

  'stayer' meanwhile is waiting for lll_wait_tid (pd->tid) 
  (call at nptl/pthread_join.c:89)

  As per comment at nptl/sysdeps/unix/sysv/linux/i386/lowlevellock.h:536,
  lll_wait_tid will not return until kernel notifies via futex
  wakeup that 'quitter' has terminated.

  Hence pthread_join cannot return until 'quitter' really has
  completely disappeared.

Drepper:
>   As per comment at nptl/sysdeps/unix/sysv/linux/i386/lowlevellock.h:536,
>   lll_wait_tid will not return until kernel notifies via futex
>   wakeup that 'quitter' has terminated.
That's the key.  The kernel resets the TID field after the thread is
done.  No way the joiner can return before the thread is gone.
*/

#if defined(VGO_solaris)
/* Solaris also provides thr_join() in addition to pthread_join().
 * Both pthread_join(3C) and thr_join(3C) are based on private _thrp_join().
 *
 * :TODO: No functionality is currently provided for joinee == 0 and departed.
 *        This would require another client request, of course.
 */
__attribute__((noinline))
static int thr_join_WRK(thread_t joinee, thread_t *departed, void **thread_return)
{
   int ret;
   OrigFn fn;
   VALGRIND_GET_ORIG_FN(fn);
   if (TRACE_PTH_FNS) {
      fprintf(stderr, "<< thr_join wrapper"); fflush(stderr);
   }

   CALL_FN_W_WWW(ret, fn, joinee, departed, thread_return);

   if (ret == 0 /*success*/) {
      DO_CREQ_v_W(_VG_USERREQ__HG_PTHREAD_JOIN_POST, CREQ_PTHREAD_T, joinee);
   } else {
      DO_PthAPIerror("thr_join", ret);
   }

   if (TRACE_PTH_FNS) {
      fprintf(stderr, " :: thr_join -> %d >>\n", ret);
   }
   return ret;
}
   PTH_FUNC(int, thrZujoin, // thr_join
            thread_t joinee, thread_t *departed, void **thread_return) {
      return thr_join_WRK(joinee, departed, thread_return);
   }
#endif /* VGO_solaris */


//-----------------------------------------------------------
// Ada gcc gnat runtime:
// The gnat gcc Ada runtime does not use pthread_join. Instead, it uses
// a combination of other pthread primitives to ensure a child thread
// is gone. This combination is somewhat functionally equivalent to a
// pthread_join.
// We wrap two hook procedures called by the gnat gcc Ada runtime
// that allows helgrind to understand the semantic of Ada task dependencies
// and termination.
//   procedure Master_Hook
//     (Dependent    : Task_Id;
//      Parent       : Task_Id;
//      Master_Level : Integer);
// where    type Task_Id is access all Ada_Task_Control_Block;
// System.Tasking.Debug.Master_Hook is called by a task Dependent to
// indicate that its master is identified by master+master_level.
void I_WRAP_SONAME_FNNAME_ZU
   (Za,
    system__tasking__debug__master_hook)
     (void *dependent, void *master, int master_level);
void I_WRAP_SONAME_FNNAME_ZU
   (Za,
    system__tasking__debug__master_hook)
     (void *dependent, void *master, int master_level)
{
   OrigFn fn;
   VALGRIND_GET_ORIG_FN(fn);
   if (TRACE_GNAT_FNS) {
     fprintf(stderr, "<< GNAT master_hook wrapper "
             "dependent %p master %p master_level %d\n",
             dependent, master, master_level); fflush(stderr);
   }

   // We call the wrapped function, even if it is a null body.
   CALL_FN_v_WWW(fn, dependent, master, master_level);

   DO_CREQ_v_WWW(_VG_USERREQ__HG_GNAT_MASTER_HOOK,
                 void*,dependent, void*,master, 
                 Word, (Word)master_level);

   if (TRACE_GNAT_FNS) {
      fprintf(stderr, " :: GNAT master_hook >>\n");
   }
}

// System.Tasking.Debug.Master_Completed_Hook is called by a task to
// indicate that it has completed a master.
//  procedure Master_Completed_Hook
//     (Self_ID      : Task_Id;
//      Master_Level : Integer);
// where    type Task_Id is access all Ada_Task_Control_Block;
// This indicates that all its Dependent tasks (that identified themselves
// with the Master_Hook call) are terminated. Helgrind can consider
// at this point that the equivalent of a 'pthread_join' has been done
// between self_id and all dependent tasks at master_level.
void I_WRAP_SONAME_FNNAME_ZU
   (Za,
    system__tasking__debug__master_completed_hook)
     (void *self_id, int master_level);
void I_WRAP_SONAME_FNNAME_ZU
   (Za,
    system__tasking__debug__master_completed_hook)
     (void *self_id, int master_level)
{
   OrigFn fn;
   VALGRIND_GET_ORIG_FN(fn);
   if (TRACE_GNAT_FNS) {
     fprintf(stderr, "<< GNAT master_completed_hook wrapper "
             "self_id %p master_level %d\n",
             self_id, master_level); fflush(stderr);
   }

   // We call the wrapped function, even if it is a null body.
   CALL_FN_v_WW(fn, self_id, master_level);

   DO_CREQ_v_WW(_VG_USERREQ__HG_GNAT_MASTER_COMPLETED_HOOK,
                 void*,self_id, Word,(Word)master_level);

   if (TRACE_GNAT_FNS) {
      fprintf(stderr, " :: GNAT master_completed_hook >>\n");
   }
}

/*----------------------------------------------------------------*/
/*--- pthread_mutex_t functions                                ---*/
/*----------------------------------------------------------------*/

/* Handled:   pthread_mutex_init pthread_mutex_destroy
              pthread_mutex_lock
              pthread_mutex_trylock pthread_mutex_timedlock
              pthread_mutex_unlock
*/

//-----------------------------------------------------------
#if !defined(VGO_solaris)
// glibc:  pthread_mutex_init
// darwin: pthread_mutex_init
// FreeBSD: pthread_mutex_init
PTH_FUNC(int, pthreadZumutexZuinit, // pthread_mutex_init
              pthread_mutex_t *mutex,
              pthread_mutexattr_t* attr)
{
   int    ret;
   long   mbRec;
   OrigFn fn;
   VALGRIND_GET_ORIG_FN(fn);
   if (TRACE_PTH_FNS) {
      fprintf(stderr, "<< pthread_mxinit %p", mutex); fflush(stderr);
   }

   mbRec = 0;
   if (attr) {
      int ty, zzz;
      zzz = pthread_mutexattr_gettype(attr, &ty);
      if (zzz == 0 && ty == PTHREAD_MUTEX_RECURSIVE)
         mbRec = 1;
   }

   CALL_FN_W_WW(ret, fn, mutex,attr);

   if (ret == 0 /*success*/) {
      DO_CREQ_v_WW(_VG_USERREQ__HG_PTHREAD_MUTEX_INIT_POST,
                   pthread_mutex_t*,mutex, long,mbRec);
   } else { 
      DO_PthAPIerror( "pthread_mutex_init", ret );
   }

   if (TRACE_PTH_FNS) {
      fprintf(stderr, " :: mxinit -> %d >>\n", ret);
   }
   return ret;
}

#else /* VGO_solaris */

// Solaris: mutex_init (pthread_mutex_init calls here)
PTH_FUNC(int, mutexZuinit, // mutex_init
              mutex_t *mutex, int type, void *arg)
{
   int    ret;
   long   mbRec;
   OrigFn fn;
   VALGRIND_GET_ORIG_FN(fn);
   if (TRACE_PTH_FNS) {
      fprintf(stderr, "<< mxinit %p", mutex); fflush(stderr);
   }

   mbRec = ((type & LOCK_RECURSIVE) != 0) ? 1 : 0;

   CALL_FN_W_WWW(ret, fn, mutex, type, arg);

   if (ret == 0 /*success*/) {
      DO_CREQ_v_WW(_VG_USERREQ__HG_PTHREAD_MUTEX_INIT_POST,
                   mutex_t *, mutex, long, mbRec);
   } else {
      DO_PthAPIerror("mutex_init", ret);
   }

   if (TRACE_PTH_FNS) {
      fprintf(stderr, " :: mxinit -> %d >>\n", ret);
   }
   return ret;
}
#endif /* VGO_solaris */


//-----------------------------------------------------------
// glibc:   pthread_mutex_destroy
// darwin:  pthread_mutex_destroy
// Solaris: mutex_destroy (pthread_mutex_destroy is a weak alias)
// FreeBSD: pthread_mutex_destroy
__attribute__((noinline))
static int mutex_destroy_WRK(pthread_mutex_t *mutex)
{
   int    ret;
   unsigned long mutex_is_init;
   OrigFn fn;

   VALGRIND_GET_ORIG_FN(fn);
   if (TRACE_PTH_FNS) {
      fprintf(stderr, "<< pthread_mxdestroy %p", mutex); fflush(stderr);
   }

   if (mutex != NULL) {
      static const pthread_mutex_t mutex_init = PTHREAD_MUTEX_INITIALIZER;
      VALGRIND_HG_DISABLE_CHECKING(mutex, sizeof(*mutex));
      mutex_is_init = my_memcmp(mutex, &mutex_init, sizeof(*mutex)) == 0;
      VALGRIND_HG_ENABLE_CHECKING(mutex, sizeof(*mutex));
   } else {
      mutex_is_init = 0;
   }

   DO_CREQ_v_WW(_VG_USERREQ__HG_PTHREAD_MUTEX_DESTROY_PRE,
                pthread_mutex_t*, mutex, unsigned long, mutex_is_init);

   CALL_FN_W_W(ret, fn, mutex);

   if (ret != 0) {
      DO_PthAPIerror( "pthread_mutex_destroy", ret );
   }

   if (TRACE_PTH_FNS) {
      fprintf(stderr, " :: mxdestroy -> %d >>\n", ret);
   }
   return ret;
}

#if defined(VGO_linux) || defined(VGO_darwin) || defined(VGO_freebsd)
   PTH_FUNC(int, pthreadZumutexZudestroy, // pthread_mutex_destroy
            pthread_mutex_t *mutex) {
      return mutex_destroy_WRK(mutex);
   }
#elif defined(VGO_solaris)
   PTH_FUNC(int, mutexZudestroy, // mutex_destroy
            pthread_mutex_t *mutex) {
      return mutex_destroy_WRK(mutex);
   }
#else
#  error "Unsupported OS"
#endif


//-----------------------------------------------------------
// glibc:   pthread_mutex_lock
// darwin:  pthread_mutex_lock
// Solaris: mutex_lock (pthread_mutex_lock is a weak alias)
// FreeBSD: pthread_mutex_lock
__attribute__((noinline))
static int mutex_lock_WRK(pthread_mutex_t *mutex)
{
   int    ret;
   OrigFn fn;
   VALGRIND_GET_ORIG_FN(fn);
   if (TRACE_PTH_FNS) {
      fprintf(stderr, "<< pthread_mxlock %p", mutex); fflush(stderr);
   }

   DO_CREQ_v_WW(_VG_USERREQ__HG_PTHREAD_MUTEX_LOCK_PRE,
                pthread_mutex_t*,mutex, long,0/*!isTryLock*/);

   CALL_FN_W_W(ret, fn, mutex);

   /* There's a hole here: libpthread now knows the lock is locked,
      but the tool doesn't, so some other thread could run and detect
      that the lock has been acquired by someone (this thread).  Does
      this matter?  Not sure, but I don't think so. */

   DO_CREQ_v_WW(_VG_USERREQ__HG_PTHREAD_MUTEX_LOCK_POST,
                pthread_mutex_t *, mutex, long, (ret == 0) ? True : False);

   if (ret != 0) {
      DO_PthAPIerror( "pthread_mutex_lock", ret );
   }

   if (TRACE_PTH_FNS) {
      fprintf(stderr, " :: mxlock -> %d >>\n", ret);
   }
   return ret;
}

#if defined(VGO_linux) || defined(VGO_darwin) || defined(VGO_freebsd)
   PTH_FUNC(int, pthreadZumutexZulock, // pthread_mutex_lock
            pthread_mutex_t *mutex) {
      return mutex_lock_WRK(mutex);
   }
#elif defined(VGO_solaris)
   PTH_FUNC(int, mutexZulock, // mutex_lock
            pthread_mutex_t *mutex) {
      return mutex_lock_WRK(mutex);
   }
#else
#  error "Unsupported OS"
#endif

#if defined(VGO_solaris)
/* Internal to libc. Mutex is usually initialized only implicitly,
 * by zeroing mutex_t structure.
 */
__attribute__((noinline))
PTH_FUNC(void, lmutexZulock, // lmutex_lock
               mutex_t *mutex)
{
   OrigFn fn;
   VALGRIND_GET_ORIG_FN(fn);
   if (TRACE_PTH_FNS) {
      fprintf(stderr, "<< lmxlock %p", mutex); fflush(stderr);
   }

   DO_CREQ_v_WW(_VG_USERREQ__HG_PTHREAD_MUTEX_LOCK_PRE,
                mutex_t *, mutex, long, 0 /*!isTryLock*/);
   CALL_FN_v_W(fn, mutex);
   DO_CREQ_v_WW(_VG_USERREQ__HG_PTHREAD_MUTEX_LOCK_POST,
                mutex_t *, mutex, long, True);

   if (TRACE_PTH_FNS) {
      fprintf(stderr, " :: lmxlock >>\n");
   }
}
#endif /* VGO_solaris */


//-----------------------------------------------------------
// glibc:   pthread_mutex_trylock
// darwin:  pthread_mutex_trylock
// Solaris: mutex_trylock (pthread_mutex_trylock is a weak alias)
// FreeBSD: pthread_mutext_trylock
//
// pthread_mutex_trylock.  The handling needed here is very similar
// to that for pthread_mutex_lock, except that we need to tell
// the pre-lock creq that this is a trylock-style operation, and
// therefore not to complain if the lock is nonrecursive and 
// already locked by this thread -- because then it'll just fail
// immediately with EBUSY.
__attribute__((noinline))
static int mutex_trylock_WRK(pthread_mutex_t *mutex)
{
   int    ret;
   OrigFn fn;
   VALGRIND_GET_ORIG_FN(fn);
   if (TRACE_PTH_FNS) {
      fprintf(stderr, "<< pthread_mxtrylock %p", mutex); fflush(stderr);
   }

   DO_CREQ_v_WW(_VG_USERREQ__HG_PTHREAD_MUTEX_LOCK_PRE,
                pthread_mutex_t*,mutex, long,1/*isTryLock*/);

   CALL_FN_W_W(ret, fn, mutex);

   /* There's a hole here: libpthread now knows the lock is locked,
      but the tool doesn't, so some other thread could run and detect
      that the lock has been acquired by someone (this thread).  Does
      this matter?  Not sure, but I don't think so. */

   DO_CREQ_v_WW(_VG_USERREQ__HG_PTHREAD_MUTEX_LOCK_POST,
               pthread_mutex_t *, mutex, long, (ret == 0) ? True : False);

   if (ret != 0) {
      if (ret != EBUSY)
         DO_PthAPIerror( "pthread_mutex_trylock", ret );
   }

   if (TRACE_PTH_FNS) {
      fprintf(stderr, " :: mxtrylock -> %d >>\n", ret);
   }
   return ret;
}

#if defined(VGO_linux) || defined(VGO_darwin) || defined(VGO_freebsd)
   PTH_FUNC(int, pthreadZumutexZutrylock, // pthread_mutex_trylock
            pthread_mutex_t *mutex) {
      return mutex_trylock_WRK(mutex);
   }
#elif defined(VGO_solaris)
   PTH_FUNC(int, mutexZutrylock, // mutex_trylock
            pthread_mutex_t *mutex) {
      return mutex_trylock_WRK(mutex);
   }
#else
#  error "Unsupported OS"
#endif


//-----------------------------------------------------------
// glibc:   pthread_mutex_timedlock
// darwin:  (doesn't appear to exist)
// Solaris: pthread_mutex_timedlock
// FreeBSD: pthread_mutex_timedlock
//
// pthread_mutex_timedlock.  Identical logic to pthread_mutex_trylock.
__attribute__((noinline))
static int mutex_timedlock_WRK(pthread_mutex_t *mutex,
                               void *timeout)
{
   int    ret;
   OrigFn fn;
   VALGRIND_GET_ORIG_FN(fn);
   if (TRACE_PTH_FNS) {
      fprintf(stderr, "<< pthread_mxtimedlock %p %p", mutex, timeout); 
      fflush(stderr);
   }

   DO_CREQ_v_WW(_VG_USERREQ__HG_PTHREAD_MUTEX_LOCK_PRE,
                pthread_mutex_t*,mutex, long,1/*isTryLock-ish*/);

   CALL_FN_W_WW(ret, fn, mutex,timeout);

   /* There's a hole here: libpthread now knows the lock is locked,
      but the tool doesn't, so some other thread could run and detect
      that the lock has been acquired by someone (this thread).  Does
      this matter?  Not sure, but I don't think so. */

   DO_CREQ_v_WW(_VG_USERREQ__HG_PTHREAD_MUTEX_LOCK_POST,
                pthread_mutex_t *, mutex, long, (ret == 0) ? True : False);

   if (ret != 0) {
      if (ret != ETIMEDOUT)
         DO_PthAPIerror( "pthread_mutex_timedlock", ret );
   }

   if (TRACE_PTH_FNS) {
      fprintf(stderr, " :: mxtimedlock -> %d >>\n", ret);
   }
   return ret;
}

PTH_FUNC(int, pthreadZumutexZutimedlock, // pthread_mutex_timedlock
         pthread_mutex_t *mutex,
         void *timeout) {
   return mutex_timedlock_WRK(mutex, timeout);
}
#if defined(VGO_solaris)
PTH_FUNC(int, pthreadZumutexZureltimedlock, // pthread_mutex_reltimedlock
         pthread_mutex_t *mutex,
         void *timeout) {
   return mutex_timedlock_WRK(mutex, timeout);
}
#endif

#if defined(VGO_linux)
//-----------------------------------------------------------
// glibc:   pthread_mutex_clocklock
//
// pthread_mutex_clocklock.  Identical logic to pthread_mutex_timedlock.
__attribute__((noinline))
static int mutex_clocklock_WRK(pthread_mutex_t *mutex,
                               clockid_t clockid,
                               void *timeout)
{
   int    ret;
   OrigFn fn;
   VALGRIND_GET_ORIG_FN(fn);
   if (TRACE_PTH_FNS) {
      fprintf(stderr, "<< pthread_mxclocklock %p %p", mutex, timeout);
      fflush(stderr);
   }

   DO_CREQ_v_WW(_VG_USERREQ__HG_PTHREAD_MUTEX_LOCK_PRE,
                pthread_mutex_t*,mutex, long,1/*isTryLock-ish*/);

   CALL_FN_W_WWW(ret, fn, mutex, clockid, timeout);

   /* There's a hole here: libpthread now knows the lock is locked,
      but the tool doesn't, so some other thread could run and detect
      that the lock has been acquired by someone (this thread).  Does
      this matter?  Not sure, but I don't think so. */

   DO_CREQ_v_WW(_VG_USERREQ__HG_PTHREAD_MUTEX_LOCK_POST,
                pthread_mutex_t *, mutex, long, (ret == 0) ? True : False);

   if (ret != 0) {
      if (ret != ETIMEDOUT)
         DO_PthAPIerror( "pthread_mutex_clocklock", ret );
   }

   if (TRACE_PTH_FNS) {
      fprintf(stderr, " :: mxclocklock -> %d >>\n", ret);
   }
   return ret;
}

PTH_FUNC(int, pthreadZumutexZuclocklock, // pthread_mutex_clocklock
         pthread_mutex_t *mutex,
         clockid_t clockid,
         void *timeout) {
   return mutex_clocklock_WRK(mutex, clockid, timeout);
}
#endif

//-----------------------------------------------------------
// glibc:   pthread_mutex_unlock
// darwin:  pthread_mutex_unlock
// Solaris: mutex_unlock (pthread_mutex_unlock is a weak alias)
// FreeBSD: pthread_mutex_unlock
__attribute__((noinline))
static int mutex_unlock_WRK(pthread_mutex_t *mutex)
{
   int    ret;
   OrigFn fn;
   VALGRIND_GET_ORIG_FN(fn);

   if (TRACE_PTH_FNS) {
      fprintf(stderr, "<< pthread_mxunlk %p", mutex); fflush(stderr);
   }

   DO_CREQ_v_W(_VG_USERREQ__HG_PTHREAD_MUTEX_UNLOCK_PRE,
               pthread_mutex_t*,mutex);

   CALL_FN_W_W(ret, fn, mutex);

   DO_CREQ_v_W(_VG_USERREQ__HG_PTHREAD_MUTEX_UNLOCK_POST,
               pthread_mutex_t*,mutex);

   if (ret != 0) {
      DO_PthAPIerror( "pthread_mutex_unlock", ret );
   }

   if (TRACE_PTH_FNS) {
      fprintf(stderr, " mxunlk -> %d >>\n", ret);
   }
   return ret;
}

#if defined(VGO_linux) || defined(VGO_darwin) || defined(VGO_freebsd)
   PTH_FUNC(int, pthreadZumutexZuunlock, // pthread_mutex_unlock
            pthread_mutex_t *mutex) {
      return mutex_unlock_WRK(mutex);
   }
#elif defined(VGO_solaris)
   PTH_FUNC(int, mutexZuunlock, // mutex_unlock
            pthread_mutex_t *mutex) {
      return mutex_unlock_WRK(mutex);
   }
#else
#  error "Unsupported OS"
#endif


#if defined(VGO_solaris)
/* Internal to libc. */
__attribute__((noinline))
PTH_FUNC(void, lmutexZuunlock, // lmutex_unlock
               mutex_t *mutex)
{
   OrigFn fn;
   VALGRIND_GET_ORIG_FN(fn);

   if (TRACE_PTH_FNS) {
      fprintf(stderr, "<< lmxunlk %p", mutex); fflush(stderr);
   }

   DO_CREQ_v_W(_VG_USERREQ__HG_PTHREAD_MUTEX_UNLOCK_PRE,
               mutex_t *, mutex);
   CALL_FN_v_W(fn, mutex);
   DO_CREQ_v_W(_VG_USERREQ__HG_PTHREAD_MUTEX_UNLOCK_POST,
               mutex_t*, mutex);

   if (TRACE_PTH_FNS) {
      fprintf(stderr, " lmxunlk >>\n");
   }
}
#endif /* VGO_solaris */


/*----------------------------------------------------------------*/
/*--- pthread_cond_t functions                                 ---*/
/*----------------------------------------------------------------*/

/* Handled:   pthread_cond_wait pthread_cond_timedwait
              pthread_cond_signal pthread_cond_broadcast
              pthread_cond_init
              pthread_cond_destroy
*/

//-----------------------------------------------------------
// glibc:   pthread_cond_wait@GLIBC_2.2.5
// glibc:   pthread_cond_wait@@GLIBC_2.3.2
// darwin:  pthread_cond_wait
// darwin:  pthread_cond_wait$NOCANCEL$UNIX2003
// darwin:  pthread_cond_wait$UNIX2003
// Solaris: cond_wait (pthread_cond_wait is built atop of cond_wait)
// FreeBSD: pthread_cond_wait
//
__attribute__((noinline))
static int pthread_cond_wait_WRK(pthread_cond_t* cond,
                                 pthread_mutex_t* mutex)
{
   int ret;
   OrigFn fn;
   unsigned long mutex_is_valid;

   VALGRIND_GET_ORIG_FN(fn);

   if (TRACE_PTH_FNS) {
      fprintf(stderr, "<< pthread_cond_wait %p %p", cond, mutex);
      fflush(stderr);
   }

   /* Tell the tool a cond-wait is about to happen, so it can check
      for bogus argument values.  In return it tells us whether it
      thinks the mutex is valid or not. */
   DO_CREQ_W_WW(mutex_is_valid,
                _VG_USERREQ__HG_PTHREAD_COND_WAIT_PRE,
                pthread_cond_t*,cond, pthread_mutex_t*,mutex);
   assert(mutex_is_valid == 1 || mutex_is_valid == 0);

   /* Tell the tool we're about to drop the mutex.  This reflects the
      fact that in a cond_wait, we show up holding the mutex, and the
      call atomically drops the mutex and waits for the cv to be
      signalled. */
   if (mutex_is_valid) {
      DO_CREQ_v_W(_VG_USERREQ__HG_PTHREAD_MUTEX_UNLOCK_PRE,
                  pthread_mutex_t*,mutex);
   }

   CALL_FN_W_WW(ret, fn, cond,mutex);

   /* this conditional look stupid, but compare w/ same logic for
      pthread_cond_timedwait below */
   if (mutex_is_valid) {
      /* and now we have the mutex again if (ret == 0) */
      DO_CREQ_v_WW(_VG_USERREQ__HG_PTHREAD_MUTEX_LOCK_POST,
                   pthread_mutex_t *, mutex, long, (ret == 0) ? True : False);
   }

   DO_CREQ_v_WWWW(_VG_USERREQ__HG_PTHREAD_COND_WAIT_POST,
                  pthread_cond_t*,cond, pthread_mutex_t*,mutex, long,0,
                  long, (ret == 0 && mutex_is_valid) ? True : False);

   if (ret != 0) {
      DO_PthAPIerror( "pthread_cond_wait", ret );
   }

   if (TRACE_PTH_FNS) {
      fprintf(stderr, " cowait -> %d >>\n", ret);
   }

   return ret;
}
#if defined(VGO_linux)
   PTH_FUNC(int, pthreadZucondZuwaitZAZa, // pthread_cond_wait@*
                 pthread_cond_t* cond, pthread_mutex_t* mutex) {
      return pthread_cond_wait_WRK(cond, mutex);
   }
#elif defined(VGO_freebsd)
   PTH_FUNC(int, pthreadZucondZuwait, // pthread_cond_wait
                 pthread_cond_t* cond, pthread_mutex_t* mutex) {
      return pthread_cond_wait_WRK(cond, mutex);
   }
#elif defined(VGO_darwin)
   PTH_FUNC(int, pthreadZucondZuwaitZa, // pthread_cond_wait*
                 pthread_cond_t* cond, pthread_mutex_t* mutex) {
      return pthread_cond_wait_WRK(cond, mutex);
   }
#elif defined(VGO_solaris)
   PTH_FUNC(int, condZuwait, // cond_wait
                 pthread_cond_t *cond, pthread_mutex_t *mutex) {
      return pthread_cond_wait_WRK(cond, mutex);
   }
#else
#  error "Unsupported OS"
#endif


//-----------------------------------------------------------
// glibc:   pthread_cond_timedwait@@GLIBC_2.3.2
// glibc:   pthread_cond_timedwait@GLIBC_2.2.5
// glibc:   pthread_cond_timedwait@GLIBC_2.0
// darwin:  pthread_cond_timedwait
// darwin:  pthread_cond_timedwait$NOCANCEL$UNIX2003
// darwin:  pthread_cond_timedwait$UNIX2003
// darwin:  pthread_cond_timedwait_relative_np (trapped)
// Solaris: cond_timedwait (pthread_cond_timedwait is built on cond_timedwait)
// Solaris: cond_reltimedwait (pthread_cond_reltimedwait_np is built on this)
// FreeBSD: pthread_cond_timedwait
//
__attribute__((noinline))
static int pthread_cond_timedwait_WRK(pthread_cond_t* cond,
                                      pthread_mutex_t* mutex, 
                                      struct timespec* abstime,
                                      int timeout_error)
{
   int ret;
   OrigFn fn;
   unsigned long mutex_is_valid;
   Bool abstime_is_valid;
   VALGRIND_GET_ORIG_FN(fn);

   if (TRACE_PTH_FNS) {
      fprintf(stderr, "<< pthread_cond_timedwait %p %p %p", 
                      cond, mutex, abstime);
      fflush(stderr);
   }

   /* Tell the tool a cond-wait is about to happen, so it can check
      for bogus argument values.  In return it tells us whether it
      thinks the mutex is valid or not. */
   DO_CREQ_W_WW(mutex_is_valid,
                _VG_USERREQ__HG_PTHREAD_COND_WAIT_PRE,
                pthread_cond_t*,cond, pthread_mutex_t*,mutex);
   assert(mutex_is_valid == 1 || mutex_is_valid == 0);

   abstime_is_valid = abstime->tv_nsec >= 0 && abstime->tv_nsec < 1000000000;

   /* Tell the tool we're about to drop the mutex.  This reflects the
      fact that in a cond_wait, we show up holding the mutex, and the
      call atomically drops the mutex and waits for the cv to be
      signalled. */
   if (mutex_is_valid && abstime_is_valid) {
      DO_CREQ_v_W(_VG_USERREQ__HG_PTHREAD_MUTEX_UNLOCK_PRE,
                  pthread_mutex_t*,mutex);
   }

   CALL_FN_W_WWW(ret, fn, cond,mutex,abstime);

   if (mutex_is_valid && !abstime_is_valid && ret != EINVAL) {
      DO_PthAPIerror("Bug in libpthread: pthread_cond_timedwait "
                     "invalid abstime did not cause"
                     " EINVAL", ret);
   }

   if (mutex_is_valid && abstime_is_valid) {
      /* and now we have the mutex again if (ret == 0 || ret == timeout) */
      DO_CREQ_v_WW(_VG_USERREQ__HG_PTHREAD_MUTEX_LOCK_POST,
                   pthread_mutex_t *, mutex,
                   long, (ret == 0 || ret == timeout_error) ? True : False);
   }

   DO_CREQ_v_WWWW(_VG_USERREQ__HG_PTHREAD_COND_WAIT_POST,
                  pthread_cond_t*,cond, pthread_mutex_t*,mutex,
                  long,ret == timeout_error,
                  long, (ret == 0 || ret == timeout_error) && mutex_is_valid
                        ? True : False);

   if (ret != 0 && ret != timeout_error) {
      DO_PthAPIerror( "pthread_cond_timedwait", ret );
   }

   if (TRACE_PTH_FNS) {
      fprintf(stderr, " cotimedwait -> %d >>\n", ret);
   }

   return ret;
}
#if defined(VGO_linux)
   PTH_FUNC(int, pthreadZucondZutimedwaitZAZa, // pthread_cond_timedwait@*
                 pthread_cond_t* cond, pthread_mutex_t* mutex, 
                 struct timespec* abstime) {
      return pthread_cond_timedwait_WRK(cond, mutex, abstime, ETIMEDOUT);
   }
#elif defined(VGO_freebsd)
   PTH_FUNC(int, pthreadZucondZutimedwait, // pthread_cond_timedwait
                 pthread_cond_t* cond, pthread_mutex_t* mutex, 
                 struct timespec* abstime) {
      return pthread_cond_timedwait_WRK(cond, mutex, abstime, ETIMEDOUT);
   }
#elif defined(VGO_darwin)
   PTH_FUNC(int, pthreadZucondZutimedwait, // pthread_cond_timedwait
                 pthread_cond_t* cond, pthread_mutex_t* mutex, 
                 struct timespec* abstime) {
      return pthread_cond_timedwait_WRK(cond, mutex, abstime, ETIMEDOUT);
   }
   PTH_FUNC(int, pthreadZucondZutimedwaitZDZa, // pthread_cond_timedwait$*
                 pthread_cond_t* cond, pthread_mutex_t* mutex, 
                 struct timespec* abstime) {
      return pthread_cond_timedwait_WRK(cond, mutex, abstime, ETIMEDOUT);
   }
   PTH_FUNC(int, pthreadZucondZutimedwaitZuZa, // pthread_cond_timedwait_*
                 pthread_cond_t* cond, pthread_mutex_t* mutex, 
                 struct timespec* abstime) {
      assert(0);
   }
#elif defined(VGO_solaris)
   PTH_FUNC(int, condZutimedwait, // cond_timedwait
                 pthread_cond_t *cond, pthread_mutex_t *mutex,
                 struct timespec *abstime) {
      return pthread_cond_timedwait_WRK(cond, mutex, abstime, ETIME);
   }
   PTH_FUNC(int, condZureltimedwait, // cond_reltimedwait
                 pthread_cond_t *cond, pthread_mutex_t *mutex,
                 struct timespec *reltime) {
      return pthread_cond_timedwait_WRK(cond, mutex, reltime, ETIME);
   }
#else
#  error "Unsupported OS"
#endif

#if defined(VGO_linux)
//-----------------------------------------------------------
// glibc:   pthread_cond_clockwait
//
__attribute__((noinline))
static int pthread_cond_clockwait_WRK(pthread_cond_t* cond,
                                      pthread_mutex_t* mutex,
                                      clockid_t clockid,
                                      struct timespec* abstime,
                                      int timeout_error)
{
   int ret;
   OrigFn fn;
   unsigned long mutex_is_valid;
   Bool abstime_is_valid;
   VALGRIND_GET_ORIG_FN(fn);

   if (TRACE_PTH_FNS) {
      fprintf(stderr, "<< pthread_cond_clockwait %p %p %p",
                      cond, mutex, abstime);
      fflush(stderr);
   }

   /* Tell the tool a cond-wait is about to happen, so it can check
      for bogus argument values.  In return it tells us whether it
      thinks the mutex is valid or not. */
   DO_CREQ_W_WW(mutex_is_valid,
                _VG_USERREQ__HG_PTHREAD_COND_WAIT_PRE,
                pthread_cond_t*,cond, pthread_mutex_t*,mutex);
   assert(mutex_is_valid == 1 || mutex_is_valid == 0);

   abstime_is_valid = abstime->tv_nsec >= 0 && abstime->tv_nsec < 1000000000;

   /* Tell the tool we're about to drop the mutex.  This reflects the
      fact that in a cond_wait, we show up holding the mutex, and the
      call atomically drops the mutex and waits for the cv to be
      signalled. */
   if (mutex_is_valid && abstime_is_valid) {
      DO_CREQ_v_W(_VG_USERREQ__HG_PTHREAD_MUTEX_UNLOCK_PRE,
                  pthread_mutex_t*,mutex);
   }

   CALL_FN_W_WWWW(ret, fn, cond,mutex,clockid,abstime);

   if (mutex_is_valid && !abstime_is_valid && ret != EINVAL) {
      DO_PthAPIerror("Bug in libpthread: pthread_cond_clockwait "
                     "invalid abstime did not cause"
                     " EINVAL", ret);
   }

   if (mutex_is_valid && abstime_is_valid) {
      /* and now we have the mutex again if (ret == 0 || ret == timeout) */
      DO_CREQ_v_WW(_VG_USERREQ__HG_PTHREAD_MUTEX_LOCK_POST,
                   pthread_mutex_t *, mutex,
                   long, (ret == 0 || ret == timeout_error) ? True : False);
   }

   DO_CREQ_v_WWWW(_VG_USERREQ__HG_PTHREAD_COND_WAIT_POST,
                  pthread_cond_t*,cond, pthread_mutex_t*,mutex,
                  long,ret == timeout_error,
                  long, (ret == 0 || ret == timeout_error) && mutex_is_valid
                        ? True : False);

   if (ret != 0 && ret != timeout_error) {
      DO_PthAPIerror( "pthread_cond_clockwait", ret );
   }

   if (TRACE_PTH_FNS) {
      fprintf(stderr, " cotimedwait -> %d >>\n", ret);
   }

   return ret;
}

   PTH_FUNC(int, pthreadZucondZuclockwait, // pthread_cond_clockwait
                 pthread_cond_t* cond, pthread_mutex_t* mutex,
                 clockid_t clockid,
                 struct timespec* abstime) {
      return pthread_cond_clockwait_WRK(cond, mutex, clockid, abstime, ETIMEDOUT);
   }
#endif


//-----------------------------------------------------------
// glibc:   pthread_cond_signal@GLIBC_2.0
// glibc:   pthread_cond_signal@GLIBC_2.2.5
// glibc:   pthread_cond_signal@@GLIBC_2.3.2
// darwin:  pthread_cond_signal
// darwin:  pthread_cond_signal_thread_np (don't intercept this)
// Solaris: cond_signal (pthread_cond_signal is a weak alias)
// FreeBSD: pthread_cond_signal
//
__attribute__((noinline))
static int pthread_cond_signal_WRK(pthread_cond_t* cond)
{
   int ret;
   OrigFn fn;
   VALGRIND_GET_ORIG_FN(fn);

   if (TRACE_PTH_FNS) {
      fprintf(stderr, "<< pthread_cond_signal %p", cond);
      fflush(stderr);
   }

   DO_CREQ_v_W(_VG_USERREQ__HG_PTHREAD_COND_SIGNAL_PRE,
               pthread_cond_t*,cond);

   CALL_FN_W_W(ret, fn, cond);

   DO_CREQ_v_W(_VG_USERREQ__HG_PTHREAD_COND_SIGNAL_POST,
               pthread_cond_t*,cond);

   if (ret != 0) {
      DO_PthAPIerror( "pthread_cond_signal", ret );
   }

   if (TRACE_PTH_FNS) {
      fprintf(stderr, " cosig -> %d >>\n", ret);
   }

   return ret;
}
#if defined(VGO_linux)
   PTH_FUNC(int, pthreadZucondZusignalZAZa, // pthread_cond_signal@*
                 pthread_cond_t* cond) {
      return pthread_cond_signal_WRK(cond);
   }
#elif defined(VGO_freebsd)
   PTH_FUNC(int, pthreadZucondZusignal, // pthread_cond_signal
                 pthread_cond_t* cond) {
      return pthread_cond_signal_WRK(cond);
   }
#elif defined(VGO_darwin)
   PTH_FUNC(int, pthreadZucondZusignal, // pthread_cond_signal
                 pthread_cond_t* cond) {
      return pthread_cond_signal_WRK(cond);
   }
#elif defined(VGO_solaris)
   PTH_FUNC(int, condZusignal, // cond_signal
                 pthread_cond_t *cond) {
      return pthread_cond_signal_WRK(cond);
   }
#else
#  error "Unsupported OS"
#endif


//-----------------------------------------------------------
// glibc:   pthread_cond_broadcast@GLIBC_2.0
// glibc:   pthread_cond_broadcast@GLIBC_2.2.5
// glibc:   pthread_cond_broadcast@@GLIBC_2.3.2
// darwin:  pthread_cond_broadcast
// Solaris: cond_broadcast (pthread_cond_broadcast is a weak alias)
// FreeBSD: pthread_cond_broadcast
//
// Note, this is pretty much identical, from a dependency-graph
// point of view, with cond_signal, so the code is duplicated.
// Maybe it should be commoned up.
//
__attribute__((noinline))
static int pthread_cond_broadcast_WRK(pthread_cond_t* cond)
{
   int ret;
   OrigFn fn;
   VALGRIND_GET_ORIG_FN(fn);

   if (TRACE_PTH_FNS) {
      fprintf(stderr, "<< pthread_cond_broadcast %p", cond);
      fflush(stderr);
   }

   DO_CREQ_v_W(_VG_USERREQ__HG_PTHREAD_COND_BROADCAST_PRE,
               pthread_cond_t*,cond);

   CALL_FN_W_W(ret, fn, cond);

   DO_CREQ_v_W(_VG_USERREQ__HG_PTHREAD_COND_BROADCAST_POST,
               pthread_cond_t*,cond);

   if (ret != 0) { 
      DO_PthAPIerror( "pthread_cond_broadcast", ret );
   }

   if (TRACE_PTH_FNS) {
      fprintf(stderr, " cobro -> %d >>\n", ret);
   }

   return ret;
}
#if defined(VGO_linux)
   PTH_FUNC(int, pthreadZucondZubroadcastZAZa, // pthread_cond_broadcast@*
                 pthread_cond_t* cond) {
      return pthread_cond_broadcast_WRK(cond);
   }
#elif defined(VGO_freebsd)
   PTH_FUNC(int, pthreadZucondZubroadcast, // pthread_cond_broadcast
                 pthread_cond_t* cond) {
      return pthread_cond_broadcast_WRK(cond);
   }
#elif defined(VGO_darwin)
   PTH_FUNC(int, pthreadZucondZubroadcast, // pthread_cond_broadcast
                 pthread_cond_t* cond) {
      return pthread_cond_broadcast_WRK(cond);
   }
#elif defined(VGO_solaris)
   PTH_FUNC(int, condZubroadcast, // cond_broadcast
                 pthread_cond_t *cond) {
      return pthread_cond_broadcast_WRK(cond);
   }
#else
#   error "Unsupported OS"
#endif

// glibc:   pthread_cond_init@GLIBC_2.0
// glibc:   pthread_cond_init@GLIBC_2.2.5
// glibc:   pthread_cond_init@@GLIBC_2.3.2
// darwin:  pthread_cond_init
// Solaris: cond_init (pthread_cond_init is built atop on this function)
// FreeBSD: pthread_cond_init
// Easy way out: Handling of attr could have been messier.
// It turns out that pthread_cond_init under linux ignores
// all information in cond_attr, so do we.
// FIXME: MacOS X?
#if !defined(VGO_solaris)
__attribute__((noinline))
static int pthread_cond_init_WRK(pthread_cond_t* cond, pthread_condattr_t *cond_attr)
{
   int ret;
   OrigFn fn;
   VALGRIND_GET_ORIG_FN(fn);

   if (TRACE_PTH_FNS) {
      fprintf(stderr, "<< pthread_cond_init %p", cond);
      fflush(stderr);
   }

   CALL_FN_W_WW(ret, fn, cond, cond_attr);

   if (ret == 0) {
      DO_CREQ_v_WW(_VG_USERREQ__HG_PTHREAD_COND_INIT_POST,
                   pthread_cond_t*,cond, pthread_condattr_t*, cond_attr);
   } else {
      DO_PthAPIerror( "pthread_cond_init", ret );
   }

   if (TRACE_PTH_FNS) {
      fprintf(stderr, " coinit -> %d >>\n", ret);
   }

   return ret;
}
#if defined(VGO_linux)
   PTH_FUNC(int, pthreadZucondZuinitZAZa, // pthread_cond_init@*
	    pthread_cond_t* cond, pthread_condattr_t* cond_attr) {
     return pthread_cond_init_WRK(cond, cond_attr);
   }
#elif defined(VGO_freebsd)
   PTH_FUNC(int, pthreadZucondZuinit, // pthread_cond_init@*
	    pthread_cond_t* cond, pthread_condattr_t* cond_attr) {
     return pthread_cond_init_WRK(cond, cond_attr);
   }
#elif defined(VGO_darwin)
   PTH_FUNC(int, pthreadZucondZuinit, // pthread_cond_init
	    pthread_cond_t* cond, pthread_condattr_t * cond_attr) {
     return pthread_cond_init_WRK(cond, cond_attr);
   }
#else
#  error "Unsupported OS"
#endif

#else /* VGO_solaris */
__attribute__((noinline))
PTH_FUNC(int, condZuinit, // cond_init
              cond_t *cond, int type, void *arg)
{
   int ret;
   OrigFn fn;
   VALGRIND_GET_ORIG_FN(fn);

   if (TRACE_PTH_FNS) {
      fprintf(stderr, "<< cond_init %p", cond); fflush(stderr);
   }

   CALL_FN_W_WWW(ret, fn, cond, type, arg);

   if (ret == 0) {
      /* Luckily evh__HG_PTHREAD_COND_INIT_POST() ignores cond_attr.
         See also comment for pthread_cond_init_WRK(). */
      DO_CREQ_v_WW(_VG_USERREQ__HG_PTHREAD_COND_INIT_POST,
                   cond_t *, cond, void *, NULL);
   } else {
      DO_PthAPIerror("cond_init", ret);
   }

   if (TRACE_PTH_FNS) {
      fprintf(stderr, " cond_init -> %d >>\n", ret);
   }

   return ret;
}
#endif /* VGO_solaris */


//-----------------------------------------------------------
// glibc:   pthread_cond_destroy@@GLIBC_2.3.2
// glibc:   pthread_cond_destroy@GLIBC_2.2.5
// glibc:   pthread_cond_destroy@GLIBC_2.0
// darwin:  pthread_cond_destroy
// Solaris: cond_destroy (pthread_cond_destroy is a weak alias)
// FreeBSD: pthread_cond_destroy
//
__attribute__((noinline))
static int pthread_cond_destroy_WRK(pthread_cond_t* cond)
{
   int ret;
   unsigned long cond_is_init;
   OrigFn fn;

   VALGRIND_GET_ORIG_FN(fn);

   if (TRACE_PTH_FNS) {
      fprintf(stderr, "<< pthread_cond_destroy %p", cond);
      fflush(stderr);
   }

   if (cond != NULL) {
      const pthread_cond_t cond_init = PTHREAD_COND_INITIALIZER;
      VALGRIND_HG_DISABLE_CHECKING(cond, sizeof(*cond));
      cond_is_init = my_memcmp(cond, &cond_init, sizeof(*cond)) == 0;
      VALGRIND_HG_ENABLE_CHECKING(cond, sizeof(*cond));
   } else {
     cond_is_init = 0;
   }

   DO_CREQ_v_WW(_VG_USERREQ__HG_PTHREAD_COND_DESTROY_PRE,
                pthread_cond_t*, cond, unsigned long, cond_is_init);

   CALL_FN_W_W(ret, fn, cond);

   if (ret != 0) {
      DO_PthAPIerror( "pthread_cond_destroy", ret );
   }

   if (TRACE_PTH_FNS) {
      fprintf(stderr, " codestr -> %d >>\n", ret);
   }

   return ret;
}
#if defined(VGO_linux)
   PTH_FUNC(int, pthreadZucondZudestroyZAZa, // pthread_cond_destroy@*
                 pthread_cond_t* cond) {
      return pthread_cond_destroy_WRK(cond);
   }
#elif defined(VGO_freebsd)
   PTH_FUNC(int, pthreadZucondZudestroy, // pthread_cond_destroy@*
                 pthread_cond_t* cond) {
      return pthread_cond_destroy_WRK(cond);
   }
#elif defined(VGO_darwin)
   PTH_FUNC(int, pthreadZucondZudestroy, // pthread_cond_destroy
                 pthread_cond_t* cond) {
      return pthread_cond_destroy_WRK(cond);
   }
#elif defined(VGO_solaris)
   PTH_FUNC(int, condZudestroy, // cond_destroy
                 pthread_cond_t *cond) {
      return pthread_cond_destroy_WRK(cond);
   }
#else
#  error "Unsupported OS"
#endif


/*----------------------------------------------------------------*/
/*--- pthread_barrier_t functions                              ---*/
/*----------------------------------------------------------------*/

#if defined(HAVE_PTHREAD_BARRIER_INIT)

/* Handled:   pthread_barrier_init
              pthread_barrier_wait
              pthread_barrier_destroy

   Unhandled: pthread_barrierattr_destroy
              pthread_barrierattr_getpshared
              pthread_barrierattr_init
              pthread_barrierattr_setpshared
              -- are these important?
*/

//-----------------------------------------------------------
// glibc:   pthread_barrier_init
// darwin:  (doesn't appear to exist)
// Solaris: pthread_barrier_init
// FreeBSD: pthread_barrier_init
PTH_FUNC(int, pthreadZubarrierZuinit, // pthread_barrier_init
         pthread_barrier_t* bar,
         pthread_barrierattr_t* attr, unsigned long count)
{
   int ret;
   OrigFn fn;
   VALGRIND_GET_ORIG_FN(fn);

   if (TRACE_PTH_FNS) {
      fprintf(stderr, "<< pthread_barrier_init %p %p %lu",
                      bar, attr, count);
      fflush(stderr);
   }

   DO_CREQ_v_WWW(_VG_USERREQ__HG_PTHREAD_BARRIER_INIT_PRE,
                 pthread_barrier_t*, bar,
                 unsigned long, count,
                 unsigned long, 0/*!resizable*/);

   CALL_FN_W_WWW(ret, fn, bar,attr,count);

   if (ret != 0) {
      DO_PthAPIerror( "pthread_barrier_init", ret );
   }

   if (TRACE_PTH_FNS) {
      fprintf(stderr, "  pthread_barrier_init -> %d >>\n", ret);
   }

   return ret;
}


//-----------------------------------------------------------
// glibc:   pthread_barrier_wait
// darwin:  (doesn't appear to exist)
// Solaris: pthread_barrier_wait
// FreeBSD: pthread_barrier_wait
PTH_FUNC(int, pthreadZubarrierZuwait, // pthread_barrier_wait
              pthread_barrier_t* bar)
{
   int ret;
   OrigFn fn;
   VALGRIND_GET_ORIG_FN(fn);

   if (TRACE_PTH_FNS) {
      fprintf(stderr, "<< pthread_barrier_wait %p", bar);
      fflush(stderr);
   }

   /* That this works correctly, and doesn't screw up when a thread
      leaving the barrier races round to the front and re-enters while
      other threads are still leaving it, is quite subtle.  See
      comments in the handler for PTHREAD_BARRIER_WAIT_PRE in
      hg_main.c. */
   DO_CREQ_v_W(_VG_USERREQ__HG_PTHREAD_BARRIER_WAIT_PRE,
               pthread_barrier_t*,bar);

   CALL_FN_W_W(ret, fn, bar);

   if (ret != 0 && ret != PTHREAD_BARRIER_SERIAL_THREAD) {
      DO_PthAPIerror( "pthread_barrier_wait", ret );
   }

   if (TRACE_PTH_FNS) {
      fprintf(stderr, "  pthread_barrier_wait -> %d >>\n", ret);
   }

   return ret;
}


//-----------------------------------------------------------
// glibc:   pthread_barrier_destroy
// darwin:  (doesn't appear to exist)
// Solaris: pthread_barrier_destroy
// FreeBSD: pthread_barrier_destroy
PTH_FUNC(int, pthreadZubarrierZudestroy, // pthread_barrier_destroy
         pthread_barrier_t* bar)
{
   int ret;
   OrigFn fn;
   VALGRIND_GET_ORIG_FN(fn);

   if (TRACE_PTH_FNS) {
      fprintf(stderr, "<< pthread_barrier_destroy %p", bar);
      fflush(stderr);
   }

   DO_CREQ_v_W(_VG_USERREQ__HG_PTHREAD_BARRIER_DESTROY_PRE,
               pthread_barrier_t*,bar);

   CALL_FN_W_W(ret, fn, bar);

   if (ret != 0) {
      DO_PthAPIerror( "pthread_barrier_destroy", ret );
   }

   if (TRACE_PTH_FNS) {
      fprintf(stderr, "  pthread_barrier_destroy -> %d >>\n", ret);
   }

   return ret;
}

#endif   // defined(HAVE_PTHREAD_BARRIER_INIT)


/*----------------------------------------------------------------*/
/*--- pthread_spinlock_t functions                             ---*/
/*----------------------------------------------------------------*/

#if defined(HAVE_PTHREAD_SPIN_LOCK) \
    && !defined(DISABLE_PTHREAD_SPINLOCK_INTERCEPT)

/* Handled:   pthread_spin_init pthread_spin_destroy
              pthread_spin_lock pthread_spin_trylock
              pthread_spin_unlock

   Unhandled:
*/

/* This is a nasty kludge, in that glibc "knows" that initialising a
   spin lock unlocks it, and pthread_spin_{init,unlock} are names for
   the same function.  Hence we have to have a wrapper which does both
   things, without knowing which the user intended to happen.
   Solaris has distinct functions for init/unlock but client requests
   are immutable in helgrind.h so follow the glibc lead. */

//-----------------------------------------------------------
// glibc:   pthread_spin_init
// glibc:   pthread_spin_unlock
// darwin:  (doesn't appear to exist)
// Solaris: pthread_spin_init
// Solaris: pthread_spin_unlock
// FreeBSD: pthread_spin_init
// FreeBSD: pthread_spin_unlock
__attribute__((noinline))
static int pthread_spin_init_or_unlock_WRK(pthread_spinlock_t* lock,
                                           int pshared) {
   int    ret;
   OrigFn fn;
   VALGRIND_GET_ORIG_FN(fn);
   if (TRACE_PTH_FNS) {
      fprintf(stderr, "<< pthread_spin_iORu %p", lock); fflush(stderr);
   }

   DO_CREQ_v_W(_VG_USERREQ__HG_PTHREAD_SPIN_INIT_OR_UNLOCK_PRE,
               pthread_spinlock_t*, lock);

   CALL_FN_W_WW(ret, fn, lock,pshared);

   if (ret == 0 /*success*/) {
      DO_CREQ_v_W(_VG_USERREQ__HG_PTHREAD_SPIN_INIT_OR_UNLOCK_POST,
                  pthread_spinlock_t*,lock);
   } else { 
      DO_PthAPIerror( "pthread_spinlock_{init,unlock}", ret );
   }

   if (TRACE_PTH_FNS) {
      fprintf(stderr, " :: spiniORu -> %d >>\n", ret);
   }
   return ret;
}
#if defined(VGO_linux)
   PTH_FUNC(int, pthreadZuspinZuinit, // pthread_spin_init
            pthread_spinlock_t* lock, int pshared) {
      return pthread_spin_init_or_unlock_WRK(lock, pshared);
   }
   PTH_FUNC(int, pthreadZuspinZuunlock, // pthread_spin_unlock
            pthread_spinlock_t* lock) {
      /* this is never actually called */
      return pthread_spin_init_or_unlock_WRK(lock, 0/*pshared*/);
   }
#elif defined(VGO_freebsd)
   PTH_FUNC(int, pthreadZuspinZuinit, // pthread_spin_init
            pthread_spinlock_t* lock, int pshared) {
      return pthread_spin_init_or_unlock_WRK(lock, pshared);
   }
   PTH_FUNC(int, pthreadZuspinZuunlock, // pthread_spin_unlock@*
            pthread_spinlock_t* lock) {
      /* this is never actually called */
      return pthread_spin_init_or_unlock_WRK(lock, 0/*pshared*/);
   }
#elif defined(VGO_darwin)
#elif defined(VGO_solaris)
   PTH_FUNC(int, pthreadZuspinZuinit, // pthread_spin_init
            pthread_spinlock_t *lock, int pshared) {
      return pthread_spin_init_or_unlock_WRK(lock, pshared);
   }
   PTH_FUNC(int, pthreadZuspinZuunlock, // pthread_spin_unlock
            pthread_spinlock_t *lock) {
      return pthread_spin_init_or_unlock_WRK(lock, 0/*pshared*/);
   }
#else
#  error "Unsupported OS"
#endif


//-----------------------------------------------------------
// glibc:   pthread_spin_destroy
// darwin:  (doesn't appear to exist)
// Solaris: pthread_spin_destroy
// FreeBSD: pthread_spin_destroy
__attribute__((noinline))
static int pthread_spin_destroy_WRK(pthread_spinlock_t *lock)
{
   int    ret;
   OrigFn fn;
   VALGRIND_GET_ORIG_FN(fn);
   if (TRACE_PTH_FNS) {
      fprintf(stderr, "<< pthread_spin_destroy %p", lock);
      fflush(stderr);
   }

   DO_CREQ_v_W(_VG_USERREQ__HG_PTHREAD_SPIN_DESTROY_PRE,
               pthread_spinlock_t*,lock);

   CALL_FN_W_W(ret, fn, lock);

   if (ret != 0) {
      DO_PthAPIerror( "pthread_spin_destroy", ret );
   }

   if (TRACE_PTH_FNS) {
      fprintf(stderr, " :: spindestroy -> %d >>\n", ret);
   }
   return ret;
}
#if defined(VGO_linux) || defined(VGO_freebsd)
   PTH_FUNC(int, pthreadZuspinZudestroy, // pthread_spin_destroy
            pthread_spinlock_t *lock) {
      return pthread_spin_destroy_WRK(lock);
   }
#elif defined(VGO_darwin)
#elif defined(VGO_solaris)
   PTH_FUNC(int, pthreadZuspinZudestroy, // pthread_spin_destroy
            pthread_spinlock_t *lock) {
      return pthread_spin_destroy_WRK(lock);
   }
#else
#  error "Unsupported OS"
#endif


//-----------------------------------------------------------
// glibc:   pthread_spin_lock
// darwin:  (doesn't appear to exist)
// Solaris: pthread_spin_lock
// FreeBSD: pthread_spin_lock
__attribute__((noinline))
static int pthread_spin_lock_WRK(pthread_spinlock_t *lock)
{
   int    ret;
   OrigFn fn;
   VALGRIND_GET_ORIG_FN(fn);
   if (TRACE_PTH_FNS) {
      fprintf(stderr, "<< pthread_spinlock %p", lock);
      fflush(stderr);
   }

   DO_CREQ_v_WW(_VG_USERREQ__HG_PTHREAD_SPIN_LOCK_PRE,
                pthread_spinlock_t*,lock, long,0/*!isTryLock*/);

   CALL_FN_W_W(ret, fn, lock);

   /* There's a hole here: libpthread now knows the lock is locked,
      but the tool doesn't, so some other thread could run and detect
      that the lock has been acquired by someone (this thread).  Does
      this matter?  Not sure, but I don't think so. */

   if (ret == 0 /*success*/) {
      DO_CREQ_v_W(_VG_USERREQ__HG_PTHREAD_SPIN_LOCK_POST,
                  pthread_spinlock_t*,lock);
   } else { 
      DO_PthAPIerror( "pthread_spin_lock", ret );
   }

   if (TRACE_PTH_FNS) {
      fprintf(stderr, " :: spinlock -> %d >>\n", ret);
   }
   return ret;
}
#if defined(VGO_linux) || defined(VGO_freebsd)
   PTH_FUNC(int, pthreadZuspinZulock, // pthread_spin_lock
                 pthread_spinlock_t *lock) {
      return pthread_spin_lock_WRK(lock);
   }
#elif defined(VGO_darwin)
#elif defined(VGO_solaris)
   PTH_FUNC(int, pthreadZuspinZulock, // pthread_spin_lock
                 pthread_spinlock_t *lock) {
      return pthread_spin_lock_WRK(lock);
   }
#else
#  error "Unsupported OS"
#endif


//-----------------------------------------------------------
// glibc:   pthread_spin_trylock
// darwin:  (doesn't appear to exist)
// Solaris: pthread_spin_trylock
// FreeBSD: pthread_spin_trylock
__attribute__((noinline))
static int pthread_spin_trylock_WRK(pthread_spinlock_t *lock)
{
   int    ret;
   OrigFn fn;
   VALGRIND_GET_ORIG_FN(fn);
   if (TRACE_PTH_FNS) {
      fprintf(stderr, "<< pthread_spin_trylock %p", lock);
      fflush(stderr);
   }

   DO_CREQ_v_WW(_VG_USERREQ__HG_PTHREAD_SPIN_LOCK_PRE,
                pthread_spinlock_t*,lock, long,1/*isTryLock*/);

   CALL_FN_W_W(ret, fn, lock);

   /* There's a hole here: libpthread now knows the lock is locked,
      but the tool doesn't, so some other thread could run and detect
      that the lock has been acquired by someone (this thread).  Does
      this matter?  Not sure, but I don't think so. */

   if (ret == 0 /*success*/) {
      DO_CREQ_v_W(_VG_USERREQ__HG_PTHREAD_SPIN_LOCK_POST,
                  pthread_spinlock_t*,lock);
   } else {
      if (ret != EBUSY)
         DO_PthAPIerror( "pthread_spin_trylock", ret );
   }

   if (TRACE_PTH_FNS) {
      fprintf(stderr, " :: spin_trylock -> %d >>\n", ret);
   }
   return ret;
}
#if defined(VGO_linux) || defined(VGO_freebsd)
   PTH_FUNC(int, pthreadZuspinZutrylock, // pthread_spin_trylock
                 pthread_spinlock_t *lock) {
      return pthread_spin_trylock_WRK(lock);
   }
#elif defined(VGO_darwin)
#elif defined(VGO_solaris)
   PTH_FUNC(int, pthreadZuspinZutrylock, // pthread_spin_trylock
                 pthread_spinlock_t *lock) {
      return pthread_spin_trylock_WRK(lock);
   }
#else
#  error "Unsupported OS"
#endif

#endif // defined(HAVE_PTHREAD_SPIN_LOCK)


/*----------------------------------------------------------------*/
/*--- pthread_rwlock_t functions                               ---*/
/*----------------------------------------------------------------*/

/* Android's pthread.h doesn't say anything about rwlocks, hence these
   functions have to be conditionally compiled. */
#if defined(HAVE_PTHREAD_RWLOCK_T)

/* Handled:   pthread_rwlock_init pthread_rwlock_destroy
              pthread_rwlock_rdlock 
              pthread_rwlock_wrlock
              pthread_rwlock_unlock
              pthread_rwlock_tryrdlock
              pthread_rwlock_trywrlock
*/

//-----------------------------------------------------------
// glibc:   pthread_rwlock_init
// darwin:  pthread_rwlock_init
// darwin:  pthread_rwlock_init$UNIX2003
// Solaris: rwlock_init (pthread_rwlock_init is built atop of rwlock_init)
// FreeBSD: pthread_rwlock_init
__attribute__((noinline))
static int pthread_rwlock_init_WRK(pthread_rwlock_t *rwl,
                                   pthread_rwlockattr_t* attr)
{
   int    ret;
   OrigFn fn;
   VALGRIND_GET_ORIG_FN(fn);
   if (TRACE_PTH_FNS) {
      fprintf(stderr, "<< pthread_rwl_init %p", rwl); fflush(stderr);
   }

   CALL_FN_W_WW(ret, fn, rwl,attr);

   if (ret == 0 /*success*/) {
      DO_CREQ_v_W(_VG_USERREQ__HG_PTHREAD_RWLOCK_INIT_POST,
                  pthread_rwlock_t*,rwl);
   } else { 
      DO_PthAPIerror( "pthread_rwlock_init", ret );
   }

   if (TRACE_PTH_FNS) {
      fprintf(stderr, " :: rwl_init -> %d >>\n", ret);
   }
   return ret;
}
#if defined(VGO_linux)
   PTH_FUNC(int, pthreadZurwlockZuinit, // pthread_rwlock_init
                 pthread_rwlock_t *rwl,
                 pthread_rwlockattr_t* attr) {
      return pthread_rwlock_init_WRK(rwl, attr);
   }
#elif defined(VGO_freebsd)
   PTH_FUNC(int, pthreadZurwlockZuinit, // pthread_rwlock_init
                 pthread_rwlock_t *rwl,
                 pthread_rwlockattr_t* attr) {
      return pthread_rwlock_init_WRK(rwl, attr);
   }
#elif defined(VGO_darwin)
   PTH_FUNC(int, pthreadZurwlockZuinitZa, // pthread_rwlock_init*
                 pthread_rwlock_t *rwl,
                 pthread_rwlockattr_t* attr) {
      return pthread_rwlock_init_WRK(rwl, attr);
   }
#elif defined(VGO_solaris)
static int pthread_rwlock_init_WRK(pthread_rwlock_t *rwl,
                                   pthread_rwlockattr_t* attr)
                                   __attribute__((unused));
#else
#  error "Unsupported OS"
#endif

#if defined(VGO_solaris)
PTH_FUNC(int, rwlockZuinit, // rwlock_init
              rwlock_t *rwlock,
              int type,
              void *arg)
{
   int    ret;
   OrigFn fn;
   VALGRIND_GET_ORIG_FN(fn);
   if (TRACE_PTH_FNS) {
      fprintf(stderr, "<< rwl_init %p", rwlock); fflush(stderr);
   }

   CALL_FN_W_WWW(ret, fn, rwlock, type, arg);

   if (ret == 0 /*success*/) {
      DO_CREQ_v_W(_VG_USERREQ__HG_PTHREAD_RWLOCK_INIT_POST,
                  rwlock_t *, rwlock);
   } else {
      DO_PthAPIerror("rwlock_init", ret);
   }

   if (TRACE_PTH_FNS) {
      fprintf(stderr, " :: rwl_init -> %d >>\n", ret);
   }
   return ret;
}
#endif /* VGO_solaris */


//-----------------------------------------------------------
// glibc:   pthread_rwlock_destroy
// darwin:  pthread_rwlock_destroy
// darwin:  pthread_rwlock_destroy$UNIX2003
// Solaris: rwlock_destroy (pthread_rwlock_destroy is a weak alias)
// FreeBSD: pthread_rwlock_destroy
//
__attribute__((noinline))
static int pthread_rwlock_destroy_WRK(pthread_rwlock_t* rwl)
{
   int    ret;
   OrigFn fn;
   VALGRIND_GET_ORIG_FN(fn);
   if (TRACE_PTH_FNS) {
      fprintf(stderr, "<< pthread_rwl_destroy %p", rwl); fflush(stderr);
   }

   DO_CREQ_v_W(_VG_USERREQ__HG_PTHREAD_RWLOCK_DESTROY_PRE,
               pthread_rwlock_t*,rwl);

   CALL_FN_W_W(ret, fn, rwl);

   if (ret != 0) {
      DO_PthAPIerror( "pthread_rwlock_destroy", ret );
   }

   if (TRACE_PTH_FNS) {
      fprintf(stderr, " :: rwl_destroy -> %d >>\n", ret);
   }
   return ret;
}
#if defined(VGO_linux)
   PTH_FUNC(int, pthreadZurwlockZudestroy, // pthread_rwlock_destroy
                 pthread_rwlock_t *rwl) {
      return pthread_rwlock_destroy_WRK(rwl);
   }
#elif defined(VGO_freebsd)
   PTH_FUNC(int, pthreadZurwlockZudestroy, // pthread_rwlock_destroy
                 pthread_rwlock_t *rwl) {
      return pthread_rwlock_destroy_WRK(rwl);
   }
#elif defined(VGO_darwin)
   PTH_FUNC(int, pthreadZurwlockZudestroyZa, // pthread_rwlock_destroy*
                 pthread_rwlock_t *rwl) {
      return pthread_rwlock_destroy_WRK(rwl);
   }
#elif defined(VGO_solaris)
   PTH_FUNC(int, rwlockZudestroy, // rwlock_destroy
                 pthread_rwlock_t *rwl) {
      return pthread_rwlock_destroy_WRK(rwl);
   }
#else
#  error "Unsupported OS"
#endif


//-----------------------------------------------------------
// glibc:   pthread_rwlock_wrlock
// darwin:  pthread_rwlock_wrlock
// darwin:  pthread_rwlock_wrlock$UNIX2003
// Solaris: rw_wrlock (pthread_rwlock_wrlock is a weak alias)
// FreeBSD: pthread_rwlock_wrlock
//
__attribute__((noinline))
static int pthread_rwlock_wrlock_WRK(pthread_rwlock_t* rwlock)
{
   int    ret;
   OrigFn fn;
   VALGRIND_GET_ORIG_FN(fn);
   if (TRACE_PTH_FNS) {
      fprintf(stderr, "<< pthread_rwl_wlk %p", rwlock); fflush(stderr);
   }

   DO_CREQ_v_WWW(_VG_USERREQ__HG_PTHREAD_RWLOCK_LOCK_PRE,
                 pthread_rwlock_t*,rwlock, 
                 long,1/*isW*/, long,0/*!isTryLock*/);

   CALL_FN_W_W(ret, fn, rwlock);

   DO_CREQ_v_WWW(_VG_USERREQ__HG_PTHREAD_RWLOCK_LOCK_POST,
                 pthread_rwlock_t*,rwlock, long,1/*isW*/,
                 long, (ret == 0) ? True : False);
   if (ret != 0) {
      DO_PthAPIerror( "pthread_rwlock_wrlock", ret );
   }

   if (TRACE_PTH_FNS) {
      fprintf(stderr, " :: rwl_wlk -> %d >>\n", ret);
   }
   return ret;
}
#if defined(VGO_linux)
   PTH_FUNC(int, pthreadZurwlockZuwrlock, // pthread_rwlock_wrlock
                 pthread_rwlock_t* rwlock) {
      return pthread_rwlock_wrlock_WRK(rwlock);
   }
#elif defined(VGO_freebsd)
   PTH_FUNC(int, pthreadZurwlockZuwrlock, // pthread_rwlock_wrlock
                 pthread_rwlock_t* rwlock) {
      return pthread_rwlock_wrlock_WRK(rwlock);
   }
#elif defined(VGO_darwin)
   PTH_FUNC(int, pthreadZurwlockZuwrlockZa, // pthread_rwlock_wrlock*
                 pthread_rwlock_t* rwlock) {
      return pthread_rwlock_wrlock_WRK(rwlock);
   }
#elif defined(VGO_solaris)
   PTH_FUNC(int, rwZuwrlock, // rw_wrlock
                 pthread_rwlock_t *rwlock) {
      return pthread_rwlock_wrlock_WRK(rwlock);
   }
#else
#  error "Unsupported OS"
#endif

#if defined(VGO_solaris)
/* Internal to libc. */
PTH_FUNC(void, lrwZuwrlock, // lrw_wrlock
               rwlock_t *rwlock)
{
   OrigFn fn;
   VALGRIND_GET_ORIG_FN(fn);
   if (TRACE_PTH_FNS) {
      fprintf(stderr, "<< lrw_wlk %p", rwlock); fflush(stderr);
   }

   DO_CREQ_v_WWW(_VG_USERREQ__HG_PTHREAD_RWLOCK_LOCK_PRE,
                 pthread_rwlock_t *, rwlock,
                 long, 1/*isW*/, long, 0/*!isTryLock*/);

   CALL_FN_v_W(fn, rwlock);

   DO_CREQ_v_WWW(_VG_USERREQ__HG_PTHREAD_RWLOCK_LOCK_POST,
                 pthread_rwlock_t *, rwlock, long, 1/*isW*/, long, True);

   if (TRACE_PTH_FNS) {
      fprintf(stderr, " :: lrw_wlk >>\n");
   }
}
#endif /* VGO_solaris */


//-----------------------------------------------------------
// glibc:   pthread_rwlock_rdlock
// darwin:  pthread_rwlock_rdlock
// darwin:  pthread_rwlock_rdlock$UNIX2003
// Solaris: rw_rdlock (pthread_rwlock_rdlock is a weak alias)
// FreeBSD: pthread_rwlock_rdlock
//
__attribute__((noinline))
static int pthread_rwlock_rdlock_WRK(pthread_rwlock_t* rwlock)
{
   int    ret;
   OrigFn fn;
   VALGRIND_GET_ORIG_FN(fn);
   if (TRACE_PTH_FNS) {
      fprintf(stderr, "<< pthread_rwl_rlk %p", rwlock); fflush(stderr);
   }

   DO_CREQ_v_WWW(_VG_USERREQ__HG_PTHREAD_RWLOCK_LOCK_PRE,
                 pthread_rwlock_t*,rwlock,
                 long,0/*!isW*/, long,0/*!isTryLock*/);

   CALL_FN_W_W(ret, fn, rwlock);

   DO_CREQ_v_WWW(_VG_USERREQ__HG_PTHREAD_RWLOCK_LOCK_POST,
                 pthread_rwlock_t*,rwlock, long,0/*!isW*/,
                 long, (ret == 0) ? True : False);
   if (ret != 0) {
      DO_PthAPIerror( "pthread_rwlock_rdlock", ret );
   }

   if (TRACE_PTH_FNS) {
      fprintf(stderr, " :: rwl_rlk -> %d >>\n", ret);
   }
   return ret;
}
#if defined(VGO_linux)
   PTH_FUNC(int, pthreadZurwlockZurdlock, // pthread_rwlock_rdlock
                 pthread_rwlock_t* rwlock) {
      return pthread_rwlock_rdlock_WRK(rwlock);
   }
#elif defined(VGO_freebsd)
   PTH_FUNC(int, pthreadZurwlockZurdlock, // pthread_rwlock_rdlock
                 pthread_rwlock_t* rwlock) {
      return pthread_rwlock_rdlock_WRK(rwlock);
   }
#elif defined(VGO_darwin)
   PTH_FUNC(int, pthreadZurwlockZurdlockZa, // pthread_rwlock_rdlock*
                 pthread_rwlock_t* rwlock) {
      return pthread_rwlock_rdlock_WRK(rwlock);
   }
#elif defined(VGO_solaris)
   PTH_FUNC(int, rwZurdlock, // rw_rdlock
                 pthread_rwlock_t *rwlock) {
      return pthread_rwlock_rdlock_WRK(rwlock);
   }
#else
#  error "Unsupported OS"
#endif

#if defined(VGO_solaris)
/* Internal to libc. */
PTH_FUNC(void, lrwZurdlock, // lrw_rdlock
               rwlock_t *rwlock)
{
   OrigFn fn;
   VALGRIND_GET_ORIG_FN(fn);
   if (TRACE_PTH_FNS) {
      fprintf(stderr, "<< lrw_rlk %p", rwlock); fflush(stderr);
   }

   DO_CREQ_v_WWW(_VG_USERREQ__HG_PTHREAD_RWLOCK_LOCK_PRE,
                 pthread_rwlock_t *, rwlock,
                 long, 0/*!isW*/, long, 0/*!isTryLock*/);

   CALL_FN_v_W(fn, rwlock);

   DO_CREQ_v_WWW(_VG_USERREQ__HG_PTHREAD_RWLOCK_LOCK_POST,
                 pthread_rwlock_t *, rwlock, long, 0/*!isW*/, long, True);

   if (TRACE_PTH_FNS) {
      fprintf(stderr, " :: lrw_rlk ->>\n");
   }
}
#endif /* VGO_solaris */


//-----------------------------------------------------------
// glibc:   pthread_rwlock_trywrlock
// darwin:  pthread_rwlock_trywrlock
// darwin:  pthread_rwlock_trywrlock$UNIX2003
// Solaris: rw_trywrlock (pthread_rwlock_trywrlock is a weak alias)
// FreeBSD: pthread_rwlock_trywrlock
//
__attribute__((noinline))
static int pthread_rwlock_trywrlock_WRK(pthread_rwlock_t* rwlock)
{
   int    ret;
   OrigFn fn;
   VALGRIND_GET_ORIG_FN(fn);
   if (TRACE_PTH_FNS) {
      fprintf(stderr, "<< pthread_rwl_trywlk %p", rwlock); fflush(stderr);
   }

   DO_CREQ_v_WWW(_VG_USERREQ__HG_PTHREAD_RWLOCK_LOCK_PRE,
                 pthread_rwlock_t*,rwlock, 
                 long,1/*isW*/, long,1/*isTryLock*/);

   CALL_FN_W_W(ret, fn, rwlock);

   /* There's a hole here: libpthread now knows the lock is locked,
      but the tool doesn't, so some other thread could run and detect
      that the lock has been acquired by someone (this thread).  Does
      this matter?  Not sure, but I don't think so. */

   DO_CREQ_v_WWW(_VG_USERREQ__HG_PTHREAD_RWLOCK_LOCK_POST,
                 pthread_rwlock_t*,rwlock, long,1/*isW*/,
                 long, (ret == 0) ? True : False);
   if (ret != 0) {
      if (ret != EBUSY)
         DO_PthAPIerror( "pthread_rwlock_trywrlock", ret );
   }

   if (TRACE_PTH_FNS) {
      fprintf(stderr, " :: rwl_trywlk -> %d >>\n", ret);
   }
   return ret;
}
#if defined(VGO_linux)
   PTH_FUNC(int, pthreadZurwlockZutrywrlock, // pthread_rwlock_trywrlock
                 pthread_rwlock_t* rwlock) {
      return pthread_rwlock_trywrlock_WRK(rwlock);
   }
#elif defined(VGO_freebsd)
   PTH_FUNC(int, pthreadZurwlockZutrywrlock, // pthread_rwlock_trywrlock
                 pthread_rwlock_t* rwlock) {
      return pthread_rwlock_trywrlock_WRK(rwlock);
   }
#elif defined(VGO_darwin)
   PTH_FUNC(int, pthreadZurwlockZutrywrlockZa, // pthread_rwlock_trywrlock*
                 pthread_rwlock_t* rwlock) {
      return pthread_rwlock_trywrlock_WRK(rwlock);
   }
#elif defined(VGO_solaris)
   PTH_FUNC(int, rwZutrywrlock, // rw_trywrlock
                 pthread_rwlock_t *rwlock) {
      return pthread_rwlock_trywrlock_WRK(rwlock);
   }
#else
#  error "Unsupported OS"
#endif


//-----------------------------------------------------------
// glibc:   pthread_rwlock_tryrdlock
// darwin:  pthread_rwlock_tryrdlock
// darwin:  pthread_rwlock_tryrdlock$UNIX2003
// Solaris: rw_tryrdlock (pthread_rwlock_tryrdlock is a weak alias)
// FreeBSD: pthread_rwlock_tryrdlock
//
__attribute__((noinline))
static int pthread_rwlock_tryrdlock_WRK(pthread_rwlock_t* rwlock)
{
   int    ret;
   OrigFn fn;
   VALGRIND_GET_ORIG_FN(fn);
   if (TRACE_PTH_FNS) {
      fprintf(stderr, "<< pthread_rwl_tryrlk %p", rwlock); fflush(stderr);
   }

   DO_CREQ_v_WWW(_VG_USERREQ__HG_PTHREAD_RWLOCK_LOCK_PRE,
                 pthread_rwlock_t*,rwlock, 
                 long,0/*!isW*/, long,1/*isTryLock*/);

   CALL_FN_W_W(ret, fn, rwlock);

   /* There's a hole here: libpthread now knows the lock is locked,
      but the tool doesn't, so some other thread could run and detect
      that the lock has been acquired by someone (this thread).  Does
      this matter?  Not sure, but I don't think so. */

   DO_CREQ_v_WWW(_VG_USERREQ__HG_PTHREAD_RWLOCK_LOCK_POST,
                pthread_rwlock_t*,rwlock, long,0/*!isW*/,
                long, (ret == 0) ? True : False);

   if (ret != 0) {
      if (ret != EBUSY)
         DO_PthAPIerror( "pthread_rwlock_tryrdlock", ret );
   }

   if (TRACE_PTH_FNS) {
      fprintf(stderr, " :: rwl_tryrlk -> %d >>\n", ret);
   }
   return ret;
}
#if defined(VGO_linux)
   PTH_FUNC(int, pthreadZurwlockZutryrdlock, // pthread_rwlock_tryrdlock
                 pthread_rwlock_t* rwlock) {
      return pthread_rwlock_tryrdlock_WRK(rwlock);
   }
#elif defined(VGO_freebsd)
   PTH_FUNC(int, pthreadZurwlockZutryrdlock, // pthread_rwlock_tryrdlock
                 pthread_rwlock_t* rwlock) {
      return pthread_rwlock_tryrdlock_WRK(rwlock);
   }
#elif defined(VGO_darwin)
   PTH_FUNC(int, pthreadZurwlockZutryrdlockZa, // pthread_rwlock_tryrdlock*
                 pthread_rwlock_t* rwlock) {
      return pthread_rwlock_tryrdlock_WRK(rwlock);
   }
#elif defined(VGO_solaris)
   PTH_FUNC(int, rwZutryrdlock, // rw_tryrdlock
                 pthread_rwlock_t *rwlock) {
      return pthread_rwlock_tryrdlock_WRK(rwlock);
   }
#else
#  error "Unsupported OS"
#endif


//-----------------------------------------------------------
// glibc:   pthread_rwlock_timedrdlock
// darwin:  Unhandled
// Solaris: pthread_rwlock_timedrdlock
// Solaris: pthread_rwlock_reltimedrdlock_np
// FreeBSD: pthread_rwlock_timedrdlock
//
__attribute__((noinline)) __attribute__((unused))
static int pthread_rwlock_timedrdlock_WRK(pthread_rwlock_t *rwlock,
                                          const struct timespec *timeout)
{
   int    ret;
   OrigFn fn;
   VALGRIND_GET_ORIG_FN(fn);
   if (TRACE_PTH_FNS) {
      fprintf(stderr, "<< pthread_rwl_timedrdl %p", rwlock); fflush(stderr);
   }

   DO_CREQ_v_WWW(_VG_USERREQ__HG_PTHREAD_RWLOCK_LOCK_PRE,
                 pthread_rwlock_t *, rwlock,
                 long, 0/*isW*/, long, 0/*isTryLock*/);

   CALL_FN_W_WW(ret, fn, rwlock, timeout);

   DO_CREQ_v_WWW(_VG_USERREQ__HG_PTHREAD_RWLOCK_LOCK_POST,
                 pthread_rwlock_t *, rwlock, long, 0/*isW*/,
                 long, (ret == 0) ? True : False);
   if (ret != 0) {
      DO_PthAPIerror("pthread_rwlock_timedrdlock", ret);
   }

   if (TRACE_PTH_FNS) {
      fprintf(stderr, " :: rwl_timedrdl -> %d >>\n", ret);
   }
   return ret;
}
#if defined(VGO_linux)
PTH_FUNC(int, pthreadZurwlockZutimedrdlock, // pthread_rwlock_timedrdlock
              pthread_rwlock_t *rwlock,
              const struct timespec *timeout) {
   return pthread_rwlock_timedrdlock_WRK(rwlock, timeout);
}
#elif defined(VGO_darwin)
#elif defined(VGO_freebsd)
PTH_FUNC(int, pthreadZurwlockZutimedrdlock, // pthread_rwlock_timedrdlock
              pthread_rwlock_t *rwlock,
              const struct timespec *timeout) {
   return pthread_rwlock_timedrdlock_WRK(rwlock, timeout);
}
#elif defined(VGO_solaris)
   PTH_FUNC(int, pthreadZurwlockZutimedrdlock, // pthread_rwlock_timedrdlock
                 pthread_rwlock_t *rwlock,
                 const struct timespec *timeout) {
      return pthread_rwlock_timedrdlock_WRK(rwlock, timeout);
   }
   PTH_FUNC(int, pthreadZurwlockZureltimedrdlockZunp, // pthread_rwlock_timedrdlock_np
                 pthread_rwlock_t *rwlock,
                 const struct timespec *timeout) {
      return pthread_rwlock_timedrdlock_WRK(rwlock, timeout);
   }
#else
#  error "Unsupported OS"
#endif

#if defined(VGO_linux)
//-----------------------------------------------------------
// glibc:   pthread_rwlock_clockrdlock
//
__attribute__((noinline)) __attribute__((unused))
static int pthread_rwlock_clockrdlock_WRK(pthread_rwlock_t *rwlock,
                                          clockid_t clockid,
                                          const struct timespec *timeout)
{
   int    ret;
   OrigFn fn;
   VALGRIND_GET_ORIG_FN(fn);
   if (TRACE_PTH_FNS) {
      fprintf(stderr, "<< pthread_rwl_clockrdl %p", rwlock); fflush(stderr);
   }

   DO_CREQ_v_WWW(_VG_USERREQ__HG_PTHREAD_RWLOCK_LOCK_PRE,
                 pthread_rwlock_t *, rwlock,
                 long, 0/*isW*/, long, 0/*isTryLock*/);

   CALL_FN_W_WWW(ret, fn, rwlock, clockid, timeout);

   DO_CREQ_v_WWW(_VG_USERREQ__HG_PTHREAD_RWLOCK_LOCK_POST,
                 pthread_rwlock_t *, rwlock, long, 0/*isW*/,
                 long, (ret == 0) ? True : False);
   if (ret != 0) {
      DO_PthAPIerror("pthread_rwlock_clockrdlock", ret);
   }

   if (TRACE_PTH_FNS) {
      fprintf(stderr, " :: rwl_clockrdl -> %d >>\n", ret);
   }
   return ret;
}

PTH_FUNC(int, pthreadZurwlockZuclockrdlock, // pthread_rwlock_clockrdlock
              pthread_rwlock_t *rwlock,
              clockid_t clockid,
              const struct timespec *timeout) {
   return pthread_rwlock_clockrdlock_WRK(rwlock, clockid, timeout);
}
#endif


//-----------------------------------------------------------
// glibc:   pthread_rwlock_timedwrlock
// Solaris: pthread_rwlock_timedwrlock
// Solaris: pthread_rwlock_reltimedwrlock_np
// FreeBSD: pthread_rwlock_timedwrlock
//
__attribute__((noinline)) __attribute__((unused))
static int pthread_rwlock_timedwrlock_WRK(pthread_rwlock_t *rwlock,
                                          const struct timespec *timeout)
{
   int    ret;
   OrigFn fn;
   VALGRIND_GET_ORIG_FN(fn);
   if (TRACE_PTH_FNS) {
      fprintf(stderr, "<< pthread_rwl_timedwrl %p", rwlock); fflush(stderr);
   }

   DO_CREQ_v_WWW(_VG_USERREQ__HG_PTHREAD_RWLOCK_LOCK_PRE,
                 pthread_rwlock_t *, rwlock,
                 long, 1/*isW*/, long, 0/*isTryLock*/);

   CALL_FN_W_WW(ret, fn, rwlock, timeout);

   DO_CREQ_v_WWW(_VG_USERREQ__HG_PTHREAD_RWLOCK_LOCK_POST,
                 pthread_rwlock_t *, rwlock, long, 1/*isW*/,
                 long, (ret == 0) ? True : False);
   if (ret != 0) {
      DO_PthAPIerror("pthread_rwlock_timedwrlock", ret);
   }

   if (TRACE_PTH_FNS) {
      fprintf(stderr, " :: rwl_timedwrl -> %d >>\n", ret);
   }
   return ret;
}
#if defined(VGO_linux)
PTH_FUNC(int, pthreadZurwlockZutimedwrlock, // pthread_rwlock_timedwrlock
              pthread_rwlock_t *rwlock,
              const struct timespec *timeout) {
   return pthread_rwlock_timedwrlock_WRK(rwlock, timeout);
}
#elif defined(VGO_darwin)
#elif defined(VGO_freebsd)
PTH_FUNC(int, pthreadZurwlockZutimedwrlock, // pthread_rwlock_timedwrlock
              pthread_rwlock_t *rwlock,
              const struct timespec *timeout) {
   return pthread_rwlock_timedwrlock_WRK(rwlock, timeout);
}
#elif defined(VGO_solaris)
   PTH_FUNC(int, pthreadZurwlockZutimedwrlock, // pthread_rwlock_timedwrlock
                 pthread_rwlock_t *rwlock,
                 const struct timespec *timeout) {
      return pthread_rwlock_timedwrlock_WRK(rwlock, timeout);
   }
   PTH_FUNC(int, pthreadZurwlockZureltimedwrlockZunp, // pthread_rwlock_timedwrlock_np
                 pthread_rwlock_t *rwlock,
                 const struct timespec *timeout) {
      return pthread_rwlock_timedwrlock_WRK(rwlock, timeout);
   }
#else
#  error "Unsupported OS"
#endif

#if defined(VGO_linux)
//-----------------------------------------------------------
// glibc:   pthread_rwlock_clockwrlock
//
__attribute__((noinline)) __attribute__((unused))
static int pthread_rwlock_clockwrlock_WRK(pthread_rwlock_t *rwlock,
                                          clockid_t clockid,
                                          const struct timespec *timeout)
{
   int    ret;
   OrigFn fn;
   VALGRIND_GET_ORIG_FN(fn);
   if (TRACE_PTH_FNS) {
      fprintf(stderr, "<< pthread_rwl_clockwrl %p", rwlock); fflush(stderr);
   }

   DO_CREQ_v_WWW(_VG_USERREQ__HG_PTHREAD_RWLOCK_LOCK_PRE,
                 pthread_rwlock_t *, rwlock,
                 long, 1/*isW*/, long, 0/*isTryLock*/);

   CALL_FN_W_WWW(ret, fn, rwlock, clockid, timeout);

   DO_CREQ_v_WWW(_VG_USERREQ__HG_PTHREAD_RWLOCK_LOCK_POST,
                 pthread_rwlock_t *, rwlock, long, 1/*isW*/,
                 long, (ret == 0) ? True : False);
   if (ret != 0) {
      DO_PthAPIerror("pthread_rwlock_clockwrlock", ret);
   }

   if (TRACE_PTH_FNS) {
      fprintf(stderr, " :: rwl_clockwrl -> %d >>\n", ret);
   }
   return ret;
}

PTH_FUNC(int, pthreadZurwlockZuclockwrlock, // pthread_rwlock_clockwrlock
              pthread_rwlock_t *rwlock,
              clockid_t clockid,
              const struct timespec *timeout) {
   return pthread_rwlock_clockwrlock_WRK(rwlock, clockid, timeout);
}
#endif


//-----------------------------------------------------------
// glibc:   pthread_rwlock_unlock
// darwin:  pthread_rwlock_unlock
// darwin:  pthread_rwlock_unlock$UNIX2003
// Solaris: rw_unlock (pthread_rwlock_unlock is a weak alias)
// FreeBSD: pthread_rwlock_unlock
__attribute__((noinline))
static int pthread_rwlock_unlock_WRK(pthread_rwlock_t* rwlock)
{
   int    ret;
   OrigFn fn;
   VALGRIND_GET_ORIG_FN(fn);
   if (TRACE_PTH_FNS) {
      fprintf(stderr, "<< pthread_rwl_unlk %p", rwlock); fflush(stderr);
   }

   DO_CREQ_v_W(_VG_USERREQ__HG_PTHREAD_RWLOCK_UNLOCK_PRE,
               pthread_rwlock_t*,rwlock);

   CALL_FN_W_W(ret, fn, rwlock);

   DO_CREQ_v_W(_VG_USERREQ__HG_PTHREAD_RWLOCK_UNLOCK_POST,
               pthread_rwlock_t*,rwlock);
   if (ret != 0) {
      DO_PthAPIerror( "pthread_rwlock_unlock", ret );
   }

   if (TRACE_PTH_FNS) {
      fprintf(stderr, " :: rwl_unlk -> %d >>\n", ret);
   }
   return ret;
}
#if defined(VGO_linux)
   PTH_FUNC(int, pthreadZurwlockZuunlock, // pthread_rwlock_unlock
                 pthread_rwlock_t* rwlock) {
      return pthread_rwlock_unlock_WRK(rwlock);
   }
#elif defined(VGO_freebsd)
   PTH_FUNC(int, pthreadZurwlockZuunlock, // pthread_rwlock_unlock
                 pthread_rwlock_t* rwlock) {
      return pthread_rwlock_unlock_WRK(rwlock);
   }
#elif defined(VGO_darwin)
   PTH_FUNC(int, pthreadZurwlockZuunlockZa, // pthread_rwlock_unlock*
                 pthread_rwlock_t* rwlock) {
      return pthread_rwlock_unlock_WRK(rwlock);
   }
#elif defined(VGO_solaris)
   PTH_FUNC(int, rwZuunlock, // rw_unlock
                 pthread_rwlock_t *rwlock) {
      return pthread_rwlock_unlock_WRK(rwlock);
   }
#else
#  error "Unsupported OS"
#endif

#endif /* defined(HAVE_PTHREAD_RWLOCK_T) */


/*----------------------------------------------------------------*/
/*--- POSIX semaphores                                         ---*/
/*----------------------------------------------------------------*/

#include <semaphore.h>
#include <fcntl.h>       /* O_CREAT */

#define TRACE_SEM_FNS 0

/* Handled: 
     int sem_init(sem_t *sem, int pshared, unsigned value);
     int sem_destroy(sem_t *sem);
     int sem_wait(sem_t *sem);
     int sem_post(sem_t *sem);
     sem_t* sem_open(const char *name, int oflag, 
                     ... [mode_t mode, unsigned value]);
        [complete with its idiotic semantics]
     int sem_close(sem_t* sem);

   Unhandled:
     int sem_trywait(sem_t *sem);
     int sem_timedwait(sem_t *restrict sem,
                       const struct timespec *restrict abs_timeout);
*/

//-----------------------------------------------------------
// glibc:   sem_init@@GLIBC_2.2.5
// glibc:   sem_init@@GLIBC_2.1
// glibc:   sem_init@GLIBC_2.0
// darwin:  sem_init
// Solaris: sema_init (sem_init is built on top of sem_init)
// FreeBSD: sem_init (libc)
//
#if !defined(VGO_solaris)
__attribute__((noinline))
static int sem_init_WRK(sem_t* sem, int pshared, unsigned long value)
{
   OrigFn fn;
   int    ret;
   VALGRIND_GET_ORIG_FN(fn);

   if (TRACE_SEM_FNS) {
      fprintf(stderr, "<< sem_init(%p,%d,%lu) ", sem,pshared,value);
      fflush(stderr);
   }

   CALL_FN_W_WWW(ret, fn, sem,pshared,value);

   if (ret == 0) {
      DO_CREQ_v_WW(_VG_USERREQ__HG_POSIX_SEM_INIT_POST,
                   sem_t*, sem, unsigned long, value);
   } else {
      DO_PthAPIerror( "sem_init", errno );
   }

   if (TRACE_SEM_FNS) {
      fprintf(stderr, " sem_init -> %d >>\n", ret);
      fflush(stderr);
   }

   return ret;
}
#if defined(VGO_linux)
   PTH_FUNC(int, semZuinitZAZa, // sem_init@*
                 sem_t* sem, int pshared, unsigned long value) {
      return sem_init_WRK(sem, pshared, value);
   }
#elif defined(VGO_darwin)
   PTH_FUNC(int, semZuinit, // sem_init
                 sem_t* sem, int pshared, unsigned long value) {
      return sem_init_WRK(sem, pshared, value);
   }
#elif defined(VGO_freebsd)
  LIBC_FUNC(int, semZuinit, // sem_init
                sem_t* sem, int pshared, unsigned long value) {
     return sem_init_WRK(sem, pshared, value);
  }
 #else
#  error "Unsupported OS"
#endif

#else /* VGO_solaris */
PTH_FUNC(int, semaZuinit, // sema_init
              sema_t *sem,
              unsigned int value,
              int type,
              void *arg)
{
   OrigFn fn;
   int    ret;
   VALGRIND_GET_ORIG_FN(fn);

   if (TRACE_SEM_FNS) {
      fprintf(stderr, "<< sema_init(%p, %d, %u) ", sem, type, value);
      fflush(stderr);
   }

   CALL_FN_W_WWWW(ret, fn, sem, value, type, arg);

   if (ret == 0) {
      DO_CREQ_v_WW(_VG_USERREQ__HG_POSIX_SEM_INIT_POST,
                   sema_t *, sem, Word, value);
   } else {
      DO_PthAPIerror("sema_init", ret);
   }

   if (TRACE_SEM_FNS) {
      fprintf(stderr, " sema_init -> %d >>\n", ret);
      fflush(stderr);
   }

   return ret;
}
#endif /* VGO_solaris */


//-----------------------------------------------------------
// glibc:   sem_destroy@GLIBC_2.0
// glibc:   sem_destroy@@GLIBC_2.1
// glibc:   sem_destroy@@GLIBC_2.2.5
// darwin:  sem_destroy
// Solaris: sema_destroy (sem_destroy is built on top of sema_destroy)
// FreeBSD: sem_destroy (libc)
__attribute__((noinline))
static int sem_destroy_WRK(sem_t* sem)
{
   OrigFn fn;
   int    ret;
   VALGRIND_GET_ORIG_FN(fn);

   if (TRACE_SEM_FNS) {
      fprintf(stderr, "<< sem_destroy(%p) ", sem);
      fflush(stderr);
   }

   DO_CREQ_v_W(_VG_USERREQ__HG_POSIX_SEM_DESTROY_PRE, sem_t*, sem);

   CALL_FN_W_W(ret, fn, sem);

   if (ret != 0) {
      DO_PthAPIerror( "sem_destroy", SEM_ERROR );
   }

   if (TRACE_SEM_FNS) {
      fprintf(stderr, " sem_destroy -> %d >>\n", ret);
      fflush(stderr);
   }

   return ret;
}
#if defined(VGO_linux)
   PTH_FUNC(int, semZudestroyZAZa,  // sem_destroy*
                 sem_t* sem) {
      return sem_destroy_WRK(sem);
   }
#elif defined(VGO_darwin)
   PTH_FUNC(int, semZudestroy,  // sem_destroy
                 sem_t* sem) {
      return sem_destroy_WRK(sem);
   }
#elif defined(VGO_freebsd)
   LIBC_FUNC(int, semZudestroy,  // sem_destroy
                 sem_t* sem) {
      return sem_destroy_WRK(sem);
   }
#elif defined(VGO_solaris)
   PTH_FUNC(int, semaZudestroy,  // sema_destroy
                 sem_t *sem) {
      return sem_destroy_WRK(sem);
   }
#else
#  error "Unsupported OS"
#endif


//-----------------------------------------------------------
// glibc:   sem_wait
// glibc:   sem_wait@GLIBC_2.0
// glibc:   sem_wait@@GLIBC_2.1
// darwin:  sem_wait
// darwin:  sem_wait$NOCANCEL$UNIX2003
// darwin:  sem_wait$UNIX2003
// Solaris: sema_wait (sem_wait is built on top of sema_wait)
// FreeBSD: sem_wait (libc)
//
/* wait: decrement semaphore - acquire lockage */
__attribute__((noinline))
static int sem_wait_WRK(sem_t* sem)
{
   OrigFn fn;
   int    ret;
   VALGRIND_GET_ORIG_FN(fn);

   if (TRACE_SEM_FNS) {
      fprintf(stderr, "<< sem_wait(%p) ", sem);
      fflush(stderr);
   }

   DO_CREQ_v_W(_VG_USERREQ__HG_POSIX_SEM_WAIT_PRE, sem_t*,sem);

   CALL_FN_W_W(ret, fn, sem);

   DO_CREQ_v_WW(_VG_USERREQ__HG_POSIX_SEM_WAIT_POST, sem_t*,sem,
                long, (ret == 0) ? True : False);

   if (ret != 0) {
      DO_PthAPIerror( "sem_wait", SEM_ERROR );
   }

   if (TRACE_SEM_FNS) {
      fprintf(stderr, " sem_wait -> %d >>\n", ret);
      fflush(stderr);
   }

   return ret;
}
#if defined(VGO_linux)
   PTH_FUNC(int, semZuwait, sem_t* sem) { /* sem_wait */
      return sem_wait_WRK(sem);
   }
   PTH_FUNC(int, semZuwaitZAZa, sem_t* sem) { /* sem_wait@* */
      return sem_wait_WRK(sem);
   }
#elif defined(VGO_darwin)
   PTH_FUNC(int, semZuwait, sem_t* sem) { /* sem_wait */
      return sem_wait_WRK(sem);
   }
   PTH_FUNC(int, semZuwaitZDZa, sem_t* sem) { /* sem_wait$* */
      return sem_wait_WRK(sem);
   }
#elif defined(VGO_freebsd)
   LIBC_FUNC(int, semZuwait, sem_t* sem) { /* sem_wait */
      return sem_wait_WRK(sem);
   }
#elif defined(VGO_solaris)
   PTH_FUNC(int, semaZuwait, sem_t *sem) { /* sema_wait */
      return sem_wait_WRK(sem);
   }
#else
#  error "Unsupported OS"
#endif


//-----------------------------------------------------------
// glibc:   sem_post
// glibc:   sem_post@GLIBC_2.0
// glibc:   sem_post@@GLIBC_2.1
// darwin:  sem_post
// Solaris: sema_post (sem_post is built on top of sema_post)
// FreeBSD: sem_post (libc)
//
/* post: increment semaphore - release lockage */
__attribute__((noinline))
static int sem_post_WRK(sem_t* sem)
{
   OrigFn fn;
   int    ret;

   VALGRIND_GET_ORIG_FN(fn);

   if (TRACE_SEM_FNS) {
      fprintf(stderr, "<< sem_post(%p) ", sem);
      fflush(stderr);
   }

   DO_CREQ_v_W(_VG_USERREQ__HG_POSIX_SEM_POST_PRE, sem_t*,sem);

   CALL_FN_W_W(ret, fn, sem);

   DO_CREQ_v_W(_VG_USERREQ__HG_POSIX_SEM_POST_POST, sem_t*,sem);

   if (ret != 0) {
      DO_PthAPIerror( "sem_post", SEM_ERROR );
   }

   if (TRACE_SEM_FNS) {
      fprintf(stderr, " sem_post -> %d >>\n", ret);
      fflush(stderr);
   }

   return ret;
}
#if defined(VGO_linux)
   PTH_FUNC(int, semZupost, sem_t* sem) { /* sem_post */
      return sem_post_WRK(sem);
   }
   PTH_FUNC(int, semZupostZAZa, sem_t* sem) { /* sem_post@* */
      return sem_post_WRK(sem);
   }
#elif defined(VGO_darwin)
   PTH_FUNC(int, semZupost, sem_t* sem) { /* sem_post */
      return sem_post_WRK(sem);
   }
#elif defined(VGO_freebsd)
   LIBC_FUNC(int, semZupost, sem_t* sem) { /* sem_post */
      return sem_post_WRK(sem);
   }
#elif defined(VGO_solaris)
   PTH_FUNC(int, semaZupost, sem_t *sem) { /* sema_post */
      return sem_post_WRK(sem);
   }
#else
#  error "Unsupported OS"
#endif


//-----------------------------------------------------------
// glibc:   sem_open
// darwin:  sem_open
// Solaris: sem_open
// FreeBSD: sem_open
//
#if defined(VGO_freebsd)
LIBC_FUNC(sem_t*, semZuopen,
                 const char* name, long oflag,
                 long mode, unsigned long value)
#else
PTH_FUNC(sem_t*, semZuopen,
                 const char* name, long oflag,
                 long mode, unsigned long value)
#endif
{
   /* A copy of sem_init_WRK (more or less).  Is this correct? */
   OrigFn fn;
   sem_t* ret;
   VALGRIND_GET_ORIG_FN(fn);

   if (TRACE_SEM_FNS) {
      fprintf(stderr, "<< sem_open(\"%s\",%ld,%lx,%lu) ",
                      name,oflag,mode,value);
      fflush(stderr);
   }

   CALL_FN_W_WWWW(ret, fn, name,oflag,mode,value);

   if (ret != SEM_FAILED && (oflag & O_CREAT)) {
      DO_CREQ_v_WW(_VG_USERREQ__HG_POSIX_SEM_INIT_POST,
                   sem_t*, ret, unsigned long, value);
   } 
   if (ret == SEM_FAILED) {
      DO_PthAPIerror( "sem_open", errno );
   }

   if (TRACE_SEM_FNS) {
      fprintf(stderr, " sem_open -> %p >>\n", ret);
      fflush(stderr);
   }

   return ret;
}


//-----------------------------------------------------------
// glibc:   sem_close
// darwin:  sem_close
// Solaris: sem_close
// FreeBSD: sem_close
#if defined (VGO_freebsd)
LIBC_FUNC(int, sem_close, sem_t* sem)
#else
PTH_FUNC(int, sem_close, sem_t* sem)
#endif
{
   OrigFn fn;
   int    ret;
   VALGRIND_GET_ORIG_FN(fn);

   if (TRACE_SEM_FNS) {
      fprintf(stderr, "<< sem_close(%p) ", sem);
      fflush(stderr);
   }

   DO_CREQ_v_W(_VG_USERREQ__HG_POSIX_SEM_DESTROY_PRE, sem_t*, sem);

   CALL_FN_W_W(ret, fn, sem);

   if (ret != 0) {
      DO_PthAPIerror( "sem_close", errno );
   }

   if (TRACE_SEM_FNS) {
      fprintf(stderr, " close -> %d >>\n", ret);
      fflush(stderr);
   }

   return ret;
}


/*----------------------------------------------------------------*/
/*--- Qt 4 threading functions (w/ GNU name mangling)          ---*/
/*----------------------------------------------------------------*/

/* Handled:
      QMutex::lock()
      QMutex::unlock()
      QMutex::tryLock()
      QMutex::tryLock(int)

      QMutex::QMutex(QMutex::RecursionMode)  _ZN6QMutexC1ENS_13RecursionModeE
      QMutex::QMutex(QMutex::RecursionMode)  _ZN6QMutexC2ENS_13RecursionModeE
      QMutex::~QMutex()                      _ZN6QMutexD1Ev
      QMutex::~QMutex()                      _ZN6QMutexD2Ev

   Unhandled:
      QReadWriteLock::lockForRead()
      QReadWriteLock::lockForWrite()
      QReadWriteLock::unlock()
      QReadWriteLock::tryLockForRead(int)
      QReadWriteLock::tryLockForRead()
      QReadWriteLock::tryLockForWrite(int)
      QReadWriteLock::tryLockForWrite()

      QWaitCondition::wait(QMutex*, unsigned long)
      QWaitCondition::wakeAll()
      QWaitCondition::wakeOne()

      QSemaphore::*
*/
/* More comments, 19 Nov 08, based on assessment of qt-4.5.0TP1,
   at least on Unix:

   It's apparently only necessary to intercept QMutex, since that is
   not implemented using pthread_mutex_t; instead Qt4 has its own
   implementation based on atomics (to check the non-contended case)
   and pthread_cond_wait (to wait in the contended case).

   QReadWriteLock is built on top of QMutex, counters, and a wait
   queue.  So we don't need to handle it specially once QMutex
   handling is correct -- presumably the dependencies through QMutex
   are sufficient to avoid any false race reports.  On the other hand,
   it is an open question whether too many dependencies are observed
   -- in which case we may miss races (false negatives).  I suspect
   this is likely to be the case, unfortunately.

   QWaitCondition is built on pthread_cond_t, pthread_mutex_t, QMutex
   and QReadWriteLock.  Same compositional-correctness justificiation
   and limitations as fro QReadWriteLock.

   Ditto QSemaphore (from cursory examination).

   Does it matter that only QMutex is handled directly?  Open
   question.  From testing with drd/tests/qt4_* and with KDE4 apps, it
   appears that no false errors are reported; however it is not clear
   if this is causing false negatives.

   Another problem with Qt4 is thread exiting.  Threads are created
   with pthread_create (fine); but they detach and simply exit when
   done.  There is no use of pthread_join, and the provided
   wait-for-a-thread-to-exit mechanism (QThread::wait, I believe)
   relies on a system of mutexes and flags.  I suspect this also
   causes too many dependencies to appear.  Consequently H sometimes
   fails to detect races at exit in some very short-lived racy
   programs, because it appears that a thread can exit _and_ have an
   observed dependency edge back to the main thread (presumably)
   before the main thread reaps the child (that is, calls
   QThread::wait).

   This theory is supported by the observation that if all threads are
   made to wait at a pthread_barrier_t immediately before they exit,
   then H's detection of races in such programs becomes reliable;
   without the barrier, it is varies from run to run, depending
   (according to investigation) on whether aforementioned
   exit-before-reaping behaviour happens or not.

   Finally, why is it necessary to intercept the QMutex constructors
   and destructors?  The constructors are intercepted only as a matter
   of convenience, so H can print accurate "first observed at"
   clauses.  However, it is actually necessary to intercept the
   destructors (as it is with pthread_mutex_destroy) in order that
   locks get removed from LAOG when they are destroyed.
*/

// soname is libQtCore.so.4 ; match against libQtCore.so*
#define QT4_FUNC(ret_ty, f, args...) \
   ret_ty I_WRAP_SONAME_FNNAME_ZU(libQtCoreZdsoZa,f)(args); \
   ret_ty I_WRAP_SONAME_FNNAME_ZU(libQtCoreZdsoZa,f)(args)

// soname is libQt5Core.so.4 ; match against libQt5Core.so*
#define QT5_FUNC(ret_ty, f, args...) \
   ret_ty I_WRAP_SONAME_FNNAME_ZU(libQt5CoreZdsoZa,f)(args); \
   ret_ty I_WRAP_SONAME_FNNAME_ZU(libQt5CoreZdsoZa,f)(args)

//-----------------------------------------------------------
// QMutex::lock()
__attribute__((noinline))
static void QMutex_lock_WRK(void* self)
{
   OrigFn fn;
   VALGRIND_GET_ORIG_FN(fn);
   if (TRACE_QT4_FNS) {
      fprintf(stderr, "<< QMutex::lock %p", self); fflush(stderr);
   }

   DO_CREQ_v_WW(_VG_USERREQ__HG_PTHREAD_MUTEX_LOCK_PRE,
                void*,self, long,0/*!isTryLock*/);

   CALL_FN_v_W(fn, self);

   DO_CREQ_v_WW(_VG_USERREQ__HG_PTHREAD_MUTEX_LOCK_POST,
                void *, self, long, True);

   if (TRACE_QT4_FNS) {
      fprintf(stderr, " :: Q::lock done >>\n");
   }
}

QT4_FUNC(void, _ZN6QMutex4lockEv, void* self) {
    QMutex_lock_WRK(self);
}
QT5_FUNC(void, _ZN6QMutex4lockEv, void* self) {
    QMutex_lock_WRK(self);
}

//-----------------------------------------------------------
// QMutex::unlock()
__attribute__((noinline))
static void QMutex_unlock_WRK(void* self)
{
   OrigFn fn;
   VALGRIND_GET_ORIG_FN(fn);

   if (TRACE_QT4_FNS) {
      fprintf(stderr, "<< QMutex::unlock %p", self); fflush(stderr);
   }

   DO_CREQ_v_W(_VG_USERREQ__HG_PTHREAD_MUTEX_UNLOCK_PRE,
               void*, self);

   CALL_FN_v_W(fn, self);

   DO_CREQ_v_W(_VG_USERREQ__HG_PTHREAD_MUTEX_UNLOCK_POST,
               void*, self);

   if (TRACE_QT4_FNS) {
      fprintf(stderr, " Q::unlock done >>\n");
   }
}

QT4_FUNC(void, _ZN6QMutex6unlockEv, void* self) {
    QMutex_unlock_WRK(self);
}
QT5_FUNC(void, _ZN6QMutex6unlockEv, void* self) {
    QMutex_unlock_WRK(self);
}

//-----------------------------------------------------------
// bool QMutex::tryLock()
// using 'long' to mimic C++ 'bool'
__attribute__((noinline))
static long QMutex_tryLock_WRK(void* self)
{
   OrigFn fn;
   long   ret;
   VALGRIND_GET_ORIG_FN(fn);
   if (TRACE_QT4_FNS) {
      fprintf(stderr, "<< QMutex::tryLock %p", self); fflush(stderr);
   }

   DO_CREQ_v_WW(_VG_USERREQ__HG_PTHREAD_MUTEX_LOCK_PRE,
                void*,self, long,1/*isTryLock*/);

   CALL_FN_W_W(ret, fn, self);

   // assumes that only the low 8 bits of the 'bool' are significant
   DO_CREQ_v_WW(_VG_USERREQ__HG_PTHREAD_MUTEX_LOCK_POST,
                void *, self, long, (ret & 0xFF) ? True : False);

   if (TRACE_QT4_FNS) {
      fprintf(stderr, " :: Q::tryLock -> %lu >>\n", ret);
   }
   
   return ret;
}

QT4_FUNC(long, _ZN6QMutex7tryLockEv, void* self) {
    return QMutex_tryLock_WRK(self);
}
QT5_FUNC(long, _ZN6QMutex7tryLockEv, void* self) {
    return QMutex_tryLock_WRK(self);
}

//-----------------------------------------------------------
// bool QMutex::tryLock(int)
// using 'long' to mimic C++ 'bool'
__attribute__((noinline))
static long QMutex_tryLock_int_WRK(void* self, long arg2)
{
   OrigFn fn;
   long   ret;
   VALGRIND_GET_ORIG_FN(fn);
   if (TRACE_QT4_FNS) {
      fprintf(stderr, "<< QMutex::tryLock(int) %p %d", self, (int)arg2);
      fflush(stderr);
   }

   DO_CREQ_v_WW(_VG_USERREQ__HG_PTHREAD_MUTEX_LOCK_PRE,
                void*,self, long,1/*isTryLock*/);

   CALL_FN_W_WW(ret, fn, self,arg2);

   // assumes that only the low 8 bits of the 'bool' are significant
   DO_CREQ_v_WW(_VG_USERREQ__HG_PTHREAD_MUTEX_LOCK_POST,
               void *, self, long, (ret & 0xFF) ? True : False);

   if (TRACE_QT4_FNS) {
      fprintf(stderr, " :: Q::tryLock(int) -> %lu >>\n", ret);
   }
   
   return ret;
}

QT4_FUNC(long, _ZN6QMutex7tryLockEi, void* self, long arg2) {
    return QMutex_tryLock_int_WRK(self, arg2);
}
QT5_FUNC(long, _ZN6QMutex7tryLockEi, void* self, long arg2) {
    return QMutex_tryLock_int_WRK(self, arg2);
}

//-----------------------------------------------------------
// It's not really very clear what the args are here.  But from
// a bit of dataflow analysis of the generated machine code of
// the original function, it appears this takes two args, and
// returns nothing.  Nevertheless preserve return value just in
// case.  A bit of debug printing indicates that the first arg
// is that of the mutex and the second is either zero or one,
// probably being the recursion mode, therefore.
// QMutex::QMutex(QMutex::RecursionMode)  ("C1ENS" variant)
__attribute__((noinline))
static void* QMutex_constructor_WRK(void* mutex, long recmode)
{
   OrigFn fn;
   long   ret;
   VALGRIND_GET_ORIG_FN(fn);
   CALL_FN_W_WW(ret, fn, mutex, recmode);
   //   fprintf(stderr, "QMutex constructor 1: %p <- %p %p\n", ret, arg1, arg2);
   DO_CREQ_v_WW(_VG_USERREQ__HG_PTHREAD_MUTEX_INIT_POST,
                void*,mutex, long,1/*mbRec*/);
   return (void*)ret;
}

QT4_FUNC(void*, _ZN6QMutexC1ENS_13RecursionModeE, void* self, long recmode) {
    return QMutex_constructor_WRK(self, recmode);
}
QT5_FUNC(void*, _ZN6QMutexC1ENS_13RecursionModeE, void* self, long recmode) {
    return QMutex_constructor_WRK(self, recmode);
}

//-----------------------------------------------------------
// QMutex::~QMutex()  ("D1Ev" variant)
__attribute__((noinline))
static void* QMutex_destructor_WRK(void* mutex)
{
   OrigFn fn;
   long   ret;
   VALGRIND_GET_ORIG_FN(fn);
   DO_CREQ_v_W(_VG_USERREQ__HG_PTHREAD_MUTEX_DESTROY_PRE,
               void*,mutex);
   CALL_FN_W_W(ret, fn, mutex);
   return (void*)ret;
}

QT4_FUNC(void*, _ZN6QMutexD1Ev, void* self) {
    return QMutex_destructor_WRK(self);
}
QT5_FUNC(void*, _ZN6QMutexD1Ev, void* self) {
    return QMutex_destructor_WRK(self);
}

//-----------------------------------------------------------
// QMutex::QMutex(QMutex::RecursionMode)  ("C2ENS" variant)
QT4_FUNC(void*, _ZN6QMutexC2ENS_13RecursionModeE,
         void* mutex,
         long  recmode)
{
   assert(0);
   /*NOTREACHED*/
   /* Android's gcc behaves like it doesn't know that assert(0)
      never returns.  Hence: */
   return NULL;
}

QT5_FUNC(void*, _ZN6QMutexC2ENS_13RecursionModeE, void* self, long recmode)
{
   assert(0);
   /*NOTREACHED*/
   return NULL;
}

//-----------------------------------------------------------
// QMutex::~QMutex()  ("D2Ev" variant)
QT4_FUNC(void*, _ZN6QMutexD2Ev, void* mutex)
{
   assert(0);
   /* Android's gcc behaves like it doesn't know that assert(0)
      never returns.  Hence: */
   return NULL;
}

QT5_FUNC(void*, _ZN6QMutexD2Ev, void* self)
{
   assert(0);
   /*NOTREACHED*/
   return NULL;
}

// QReadWriteLock is not intercepted directly.  See comments
// above.

//// QReadWriteLock::lockForRead()
//// _ZN14QReadWriteLock11lockForReadEv == QReadWriteLock::lockForRead()
//QT4_FUNC(void, ZuZZN14QReadWriteLock11lockForReadEv, 
//               // _ZN14QReadWriteLock11lockForReadEv
//               void* self)
//{
//   OrigFn fn;
//   VALGRIND_GET_ORIG_FN(fn);
//   if (TRACE_QT4_FNS) {
//      fprintf(stderr, "<< QReadWriteLock::lockForRead %p", self);
//      fflush(stderr);
//   }
//
//   DO_CREQ_v_WWW(_VG_USERREQ__HG_PTHREAD_RWLOCK_LOCK_PRE,
//                 void*,self,
//                 long,0/*!isW*/, long,0/*!isTryLock*/);
//
//   CALL_FN_v_W(fn, self);
//
//   DO_CREQ_v_WWW(_VG_USERREQ__HG_PTHREAD_RWLOCK_LOCK_POST,
//                 void*,self, long,0/*!isW*/, long, True);
//
//   if (TRACE_QT4_FNS) {
//      fprintf(stderr, " :: Q::lockForRead :: done >>\n");
//   }
//}
//
//// QReadWriteLock::lockForWrite()
//// _ZN14QReadWriteLock12lockForWriteEv == QReadWriteLock::lockForWrite()
//QT4_FUNC(void, ZuZZN14QReadWriteLock12lockForWriteEv, 
//               // _ZN14QReadWriteLock12lockForWriteEv
//               void* self)
//{
//   OrigFn fn;
//   VALGRIND_GET_ORIG_FN(fn);
//   if (TRACE_QT4_FNS) {
//      fprintf(stderr, "<< QReadWriteLock::lockForWrite %p", self);
//      fflush(stderr);
//   }
//
//   DO_CREQ_v_WWW(_VG_USERREQ__HG_PTHREAD_RWLOCK_LOCK_PRE,
//                 void*,self,
//                 long,1/*isW*/, long,0/*!isTryLock*/);
//
//   CALL_FN_v_W(fn, self);
//
//   DO_CREQ_v_WWW(_VG_USERREQ__HG_PTHREAD_RWLOCK_LOCK_POST,
//                 void*,self, long,1/*isW*/, long, True);
//
//   if (TRACE_QT4_FNS) {
//      fprintf(stderr, " :: Q::lockForWrite :: done >>\n");
//   }
//}
//
//// QReadWriteLock::unlock()
//// _ZN14QReadWriteLock6unlockEv == QReadWriteLock::unlock()
//QT4_FUNC(void, ZuZZN14QReadWriteLock6unlockEv,
//               // _ZN14QReadWriteLock6unlockEv
//               void* self)
//{
//   OrigFn fn;
//   VALGRIND_GET_ORIG_FN(fn);
//   if (TRACE_QT4_FNS) {
//      fprintf(stderr, "<< QReadWriteLock::unlock %p", self);
//      fflush(stderr);
//   }
//
//   DO_CREQ_v_W(_VG_USERREQ__HG_PTHREAD_RWLOCK_UNLOCK_PRE,
//               void*,self);
//
//   CALL_FN_v_W(fn, self);
//
//   DO_CREQ_v_W(_VG_USERREQ__HG_PTHREAD_RWLOCK_UNLOCK_POST,
//               void*,self);
//
//   if (TRACE_QT4_FNS) {
//      fprintf(stderr, " :: Q::unlock :: done >>\n");
//   }
//}


/*----------------------------------------------------------------*/
/*--- Replacements for basic string functions, that don't      ---*/
/*--- overrun the input arrays.                                ---*/
/*----------------------------------------------------------------*/

#include "../shared/vg_replace_strmem.c"

/*--------------------------------------------------------------------*/
/*--- end                                          hg_intercepts.c ---*/
/*--------------------------------------------------------------------*/
