
/*--------------------------------------------------------------------*/
/*--- Client-space code for drd.             drd_gomp_intercepts.c ---*/
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

#include <assert.h>
#include "drd_clientreq.h"
#include "pub_tool_redir.h"


// Defines.

#define GOMP_FUNC(ret_ty, f, args...)                   \
  ret_ty VG_WRAP_FUNCTION_ZZ(libgompZdsoZd1Za,f)(args); \
  ret_ty VG_WRAP_FUNCTION_ZZ(libgompZdsoZd1Za,f)(args)


// Type definitions

typedef void* gomp_barrier_t;


// Function definitions.

GOMP_FUNC(void, gompZubarrierZuinit, // gomp_barrier_init
          gomp_barrier_t* barrier, unsigned count)
{
  int    ret;
  int    res;
  OrigFn fn;

  VALGRIND_DO_CLIENT_REQUEST(res, -1, VG_USERREQ__PRE_BARRIER_INIT,
                             barrier, gomp_barrier, count, 0, 0);
  VALGRIND_GET_ORIG_FN(fn);
  CALL_FN_W_WW(ret, fn, barrier, count);
  VALGRIND_DO_CLIENT_REQUEST(res, -1, VG_USERREQ__POST_BARRIER_INIT,
                             barrier, gomp_barrier, 0, 0, 0);
}

GOMP_FUNC(void, gompZubarrierZureinit, // gomp_barrier_reinit
          gomp_barrier_t* barrier, unsigned count)
{
  int    ret;
  int    res;
  OrigFn fn;
  VALGRIND_DO_CLIENT_REQUEST(res, -1, VG_USERREQ__PRE_BARRIER_INIT,
                             barrier, gomp_barrier, count, 1, 0);
  VALGRIND_GET_ORIG_FN(fn);
  CALL_FN_W_WW(ret, fn, barrier, count);
  VALGRIND_DO_CLIENT_REQUEST(res, -1, VG_USERREQ__POST_BARRIER_INIT,
                             barrier, gomp_barrier, 0, 0, 0);
}

GOMP_FUNC(void, gompZubarrierZudestroy, // gomp_barrier_destroy
          gomp_barrier_t* barrier)
{
  int    ret;
  int    res;
  OrigFn fn;
  VALGRIND_GET_ORIG_FN(fn);
  VALGRIND_DO_CLIENT_REQUEST(res, -1, VG_USERREQ__PRE_BARRIER_DESTROY,
                             barrier, gomp_barrier, 0, 0, 0);
  CALL_FN_W_W(ret, fn, barrier);
  VALGRIND_DO_CLIENT_REQUEST(res, -1, VG_USERREQ__POST_BARRIER_DESTROY,
                             barrier, gomp_barrier, 0, 0, 0);
}

GOMP_FUNC(void, gompZubarrierZuwait, // gomp_barrier_wait
          gomp_barrier_t* barrier)
{
  int    ret;
  int    res;
  OrigFn fn;
  VALGRIND_GET_ORIG_FN(fn);
  VALGRIND_DO_CLIENT_REQUEST(res, -1, VG_USERREQ__PRE_BARRIER_WAIT,
                             barrier, gomp_barrier, 0, 0, 0);
  CALL_FN_W_W(ret, fn, barrier);
  VALGRIND_DO_CLIENT_REQUEST(res, -1, VG_USERREQ__POST_BARRIER_WAIT,
                             barrier, gomp_barrier, 1, 0, 0);
}
