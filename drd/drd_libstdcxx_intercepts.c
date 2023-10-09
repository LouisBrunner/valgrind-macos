/*--------------------------------------------------------------------*/
/*--- Client-space code for DRD.        drd_libstdcxx_intercepts.c ---*/
/*--------------------------------------------------------------------*/

/*
  This file is part of DRD, a thread error detector.

  Copyright (C) 2014-2017 Bart Van Assche <bvanassche@acm.org>.

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

#include "drd_basics.h"     /* DRD_() */
#include "drd_clientreq.h"
#include "pub_tool_redir.h" /* VG_WRAP_FUNCTION_ZZ() */

/* From <cxxabi.h> */
int __cxa_guard_acquire(void* guard);
void __cxa_guard_release(void* guard) __attribute__((__nothrow__));
void __cxa_guard_abort(void* guard) __attribute__((__nothrow__));

#define LIBSTDCXX_FUNC(ret_ty, zf, implf, argl_decl, argl)             \
   ret_ty VG_WRAP_FUNCTION_ZZ(VG_Z_LIBSTDCXX_SONAME,zf) argl_decl;     \
   ret_ty VG_WRAP_FUNCTION_ZZ(VG_Z_LIBSTDCXX_SONAME,zf) argl_decl      \
   { return implf argl; }

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

static __always_inline
int __cxa_guard_acquire_intercept(void *guard)
{
   int   ret;
   OrigFn fn;
   VALGRIND_GET_ORIG_FN(fn);
   VALGRIND_DO_CLIENT_REQUEST_STMT(VG_USERREQ_DRD_PRE_MUTEX_LOCK,
                                   guard, mutex_type_cxa_guard, 0, 0, 0);
   CALL_FN_W_W(ret, fn, guard);
   VALGRIND_DO_CLIENT_REQUEST_STMT(VG_USERREQ_DRD_POST_MUTEX_LOCK,
                                   guard, 1, 0, 0, 0);
   if (ret == 0) {
      VALGRIND_DO_CLIENT_REQUEST_STMT(VG_USERREQ_DRD_PRE_MUTEX_UNLOCK,
                                      guard, mutex_type_cxa_guard, 0, 0, 0);
      VALGRIND_DO_CLIENT_REQUEST_STMT(VG_USERREQ_DRD_POST_MUTEX_UNLOCK,
                                      guard, 0, 0, 0, 0);
   }
   return ret;
}

LIBSTDCXX_FUNC(int, ZuZucxaZuguardZuacquire, __cxa_guard_acquire_intercept,
               (void *guard), (guard));
LIBSTDCXX_FUNC(int, ZuZucxaZuguardZuacquireZAZACXXABIZu1Zd3,
               __cxa_guard_acquire_intercept, (void *guard), (guard));

static __always_inline
void __cxa_guard_abort_release_intercept(void *guard)
{
   int ret;
   OrigFn fn;
   VALGRIND_GET_ORIG_FN(fn);
   VALGRIND_DO_CLIENT_REQUEST_STMT(VG_USERREQ_DRD_PRE_MUTEX_UNLOCK,
                                   guard, mutex_type_cxa_guard, 0, 0, 0);
   CALL_FN_W_W(ret, fn, guard);
   VALGRIND_DO_CLIENT_REQUEST_STMT(VG_USERREQ_DRD_POST_MUTEX_UNLOCK,
                                   guard, 0, 0, 0, 0);
}

LIBSTDCXX_FUNC(void, ZuZucxaZuguardZurelease,
               __cxa_guard_abort_release_intercept, (void *guard), (guard));
LIBSTDCXX_FUNC(void, ZuZucxaZuguardZuabort,
               __cxa_guard_abort_release_intercept, (void *guard), (guard));
