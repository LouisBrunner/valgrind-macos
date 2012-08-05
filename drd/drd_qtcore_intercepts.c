/*--------------------------------------------------------------------*/
/*--- Client-space code for drd.           drd_qtcore_intercepts.c ---*/
/*--------------------------------------------------------------------*/

/*
  This file is part of drd, a thread error detector.

  Copyright (C) 2006-2012 Bart Van Assche <bvanassche@acm.org>.

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

#include <assert.h>
#include "drd_clientreq.h"
#include "pub_tool_redir.h"


// Defines.

#define QT4CORE_FUNC(ret_ty, f, args...)                        \
   ret_ty VG_WRAP_FUNCTION_ZU(libQtCoreZdsoZd4,f)(args);        \
   ret_ty VG_WRAP_FUNCTION_ZU(libQtCoreZdsoZd4,f)(args)



//////////////////////////////////////////////////////////////////
// QMutex intercepts.
//////////////////////////////////////////////////////////////////


typedef enum { qt_nonrecursive = 0, qt_recursive = 1 } qt_mutex_mode;


/** Convert a Qt4 mutex type to a DRD mutex type. */
static MutexT qt_to_drd_mutex_type(qt_mutex_mode mode)
{
   switch (mode)
   {
   case qt_nonrecursive:
      return mutex_type_default_mutex;
   case qt_recursive:
      return mutex_type_recursive_mutex;
   }
   return mutex_type_invalid_mutex;
}

/** Find out the type of a Qt4 mutex (recursive or not).
 *  Since it's not possible to do this in a portable way, return
 *  mutex_type_unknown and let drd_mutex.c look up the real mutex type.
 */
static MutexT mutex_type(void* qt4_mutex)
{
   return mutex_type_unknown;
}


// QMutex::QMutex(RecursionMode) -- _ZN6QMutexC1ENS_13RecursionModeE,
QT4CORE_FUNC(void, _ZN6QMutexC1ENS_13RecursionModeE,
             void* mutex,
             qt_mutex_mode mode)
{
   int    ret;
   OrigFn fn;
   VALGRIND_GET_ORIG_FN(fn);
   VALGRIND_DO_CLIENT_REQUEST_EXPR(-1, VG_USERREQ__PRE_MUTEX_INIT,
                                   mutex, qt_to_drd_mutex_type(mode), 0, 0, 0);
   CALL_FN_W_WW(ret, fn, mutex, mode);
   VALGRIND_DO_CLIENT_REQUEST_EXPR(-1, VG_USERREQ__POST_MUTEX_INIT,
                                   mutex, 0, 0, 0, 0);
}

// QMutex::QMutex(RecursionMode) -- _ZN6QMutexC2ENS_13RecursionModeE
QT4CORE_FUNC(void, _ZN6QMutexC2ENS_13RecursionModeE,
             void* mutex,
             qt_mutex_mode mode)
{
   int    ret;
   OrigFn fn;
   VALGRIND_GET_ORIG_FN(fn);
   VALGRIND_DO_CLIENT_REQUEST_EXPR(-1, VG_USERREQ__PRE_MUTEX_INIT,
                                   mutex, qt_to_drd_mutex_type(mode), 0, 0, 0);
   CALL_FN_W_WW(ret, fn, mutex, mode);
   VALGRIND_DO_CLIENT_REQUEST_EXPR(-1, VG_USERREQ__POST_MUTEX_INIT,
                                   mutex, 0, 0, 0, 0);
}

// QMutex::~QMutex() -- _ZN6QMutexD1Ev
QT4CORE_FUNC(void, _ZN6QMutexD1Ev,
             void* mutex)
{
   int    ret;
   OrigFn fn;
   VALGRIND_GET_ORIG_FN(fn);
   VALGRIND_DO_CLIENT_REQUEST_EXPR(-1, VG_USERREQ__PRE_MUTEX_DESTROY,
                                   mutex, 0, 0, 0, 0);
   CALL_FN_W_W(ret, fn, mutex);
   VALGRIND_DO_CLIENT_REQUEST_EXPR(-1, VG_USERREQ__POST_MUTEX_DESTROY,
                                   mutex, mutex_type(mutex), 0, 0, 0);
}

// QMutex::~QMutex() -- _ZN6QMutexD2Ev
QT4CORE_FUNC(void, _ZN6QMutexD2Ev,
             void** mutex)
{
   int    ret;
   OrigFn fn;
   VALGRIND_GET_ORIG_FN(fn);
   VALGRIND_DO_CLIENT_REQUEST_EXPR(-1, VG_USERREQ__PRE_MUTEX_DESTROY,
                                   mutex, 0, 0, 0, 0);
   CALL_FN_W_W(ret, fn, mutex);
   VALGRIND_DO_CLIENT_REQUEST_EXPR(-1, VG_USERREQ__POST_MUTEX_DESTROY,
                                   mutex, mutex_type(mutex), 0, 0, 0);
}

// QMutex::lock() -- _ZN6QMutex4lockEv
QT4CORE_FUNC(void, _ZN6QMutex4lockEv,
             void* mutex)
{
   int   ret;
   OrigFn fn;
   VALGRIND_GET_ORIG_FN(fn);
   VALGRIND_DO_CLIENT_REQUEST_EXPR(0, VG_USERREQ__PRE_MUTEX_LOCK,
                                   mutex, mutex_type(mutex), 0, 0, 0);
   CALL_FN_W_W(ret, fn, mutex);
   VALGRIND_DO_CLIENT_REQUEST_EXPR(0, VG_USERREQ__POST_MUTEX_LOCK,
                                   mutex, 1, 0, 0, 0);
}

// QMutex::tryLock() -- _ZN6QMutex7tryLockEv
QT4CORE_FUNC(int, _ZN6QMutex7tryLockEv,
             void* mutex)
{
   int    ret;
   OrigFn fn;
   VALGRIND_GET_ORIG_FN(fn);
   VALGRIND_DO_CLIENT_REQUEST_EXPR(0, VG_USERREQ__PRE_MUTEX_LOCK,
                                   mutex, mutex_type(mutex), 1, 0, 0);
   CALL_FN_W_W(ret, fn, mutex);
   VALGRIND_DO_CLIENT_REQUEST_EXPR(-1, VG_USERREQ__POST_MUTEX_LOCK,
                                   mutex, ret, 0, 0, 0);
   return ret;
}

// QMutex::tryLock(int) -- _ZN6QMutex7tryLockEi
QT4CORE_FUNC(int, _ZN6QMutex7tryLockEi,
             void* mutex,
             int timeout_ms)
{
   int    ret;
   OrigFn fn;
   VALGRIND_GET_ORIG_FN(fn);
   VALGRIND_DO_CLIENT_REQUEST_EXPR(0, VG_USERREQ__PRE_MUTEX_LOCK,
                                   mutex, mutex_type(mutex), 1, 0, 0);
   CALL_FN_W_WW(ret, fn, mutex, timeout_ms);
   VALGRIND_DO_CLIENT_REQUEST_EXPR(-1, VG_USERREQ__POST_MUTEX_LOCK,
                                   mutex, ret, 0, 0, 0);
   return ret;
}

// QMutex::unlock() -- _ZN6QMutex6unlockEv
QT4CORE_FUNC(void, _ZN6QMutex6unlockEv,
             void* mutex)
{
   int    ret;
   OrigFn fn;
   VALGRIND_GET_ORIG_FN(fn);
   VALGRIND_DO_CLIENT_REQUEST_EXPR(-1, VG_USERREQ__PRE_MUTEX_UNLOCK,
                                   mutex, mutex_type(mutex), 0, 0, 0);
   CALL_FN_W_W(ret, fn, mutex);
   VALGRIND_DO_CLIENT_REQUEST_EXPR(-1, VG_USERREQ__POST_MUTEX_UNLOCK,
                                   mutex, 0, 0, 0, 0);
}
