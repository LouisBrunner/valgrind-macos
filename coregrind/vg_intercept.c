
/*--------------------------------------------------------------------*/
/*--- Intercepts for various libc functions we want to capture     ---*/
/*--- (mostly for threading purposes).              vg_intercept.c ---*/
/*--------------------------------------------------------------------*/

/*
   This file is part of Valgrind, an extensible x86 protected-mode
   emulator for monitoring program execution on x86-Unixes.

   Copyright (C) 2000-2003 Julian Seward 
      jseward@acm.org

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
   ALL THE CODE IN THIS FILE RUNS ON THE SIMULATED CPU.  It is
   intended for various reasons as drop-in replacements for libc
   functions.  These functions have global visibility (obviously) and
   have no prototypes in vg_include.h, since they are not intended to
   be called from within Valgrind.
   ------------------------------------------------------------------ */

/* General idea (2003-Apr-26) is that master implementations of
   selected functions are done as VGR_(fnname).  Then we route
   all calls to the master, both here and in vg_libpthread.c.
   This means we no longer have to rely on the semantics of weak
   symbols, which seems to have changed in glibc >= 2.3.2 in such
   a way as to make the previous interception scheme stop working.
*/

/* Sidestep the normal check which disallows using valgrind.h
   directly. */
#define __VALGRIND_SOMESKIN_H
#include "valgrind.h"

#include "vg_include.h"
#include "vg_kerneliface.h"

/* This has some nasty duplication of stuff from vg_libpthread.c */

#include <errno.h>
#include <sys/types.h>
#include <stdio.h>
#include <sys/ipc.h>
#include <sys/msg.h>
#ifdef KERNEL_2_6
#include <linux/compiler.h>
#endif
#include <asm/ipc.h>		/* for ipc_kludge */
#include <sys/poll.h>
#include <sys/socket.h>
#include <sys/uio.h>
#ifdef HAVE_SYS_TIME_H
#include <sys/time.h>
#endif

/* --------------------------------------------------------------- */

/* Just start up Valgrind if it's not already going.  VG_(startup)()
   detects and ignores second and subsequent calls. */

/* We need this guy -- it's in valgrind.so. */
extern void VG_(startup) ( void );

static __inline__
void ensure_valgrind ( char* caller )
{
   VG_(startup)();
}

static __inline__
int is_kerror ( int res )
{
   if (res >= -4095 && res <= -1)
      return 1;
   else
      return 0;
}

/* --------------------------------------------------------------- */

/* Extract from Valgrind the value of VG_(clo_trace_pthread_level).
   Returns 0 (none) if not running on Valgrind. */
static
int get_pt_trace_level ( void )
{
   int res;
   VALGRIND_MAGIC_SEQUENCE(res, 0 /* default */,
                           VG_USERREQ__GET_PTHREAD_TRACE_LEVEL,
                           0, 0, 0, 0);
   return res;
}

static 
void cat_n_send ( char* pre, char* msg )
{
   char  buf[1000];
   if (get_pt_trace_level() >= 0) {
      snprintf(buf, sizeof(buf), "%s%s", pre, msg );
      buf[sizeof(buf)-1] = '\0';
      VALGRIND_NON_SIMD_CALL2(VG_(message), Vg_UserMsg, buf);
   }
}

static
void my_exit ( int arg )
{
   VG_(do_syscall)(__NR_exit_group, arg);
   VG_(do_syscall)(__NR_exit, arg);
}

static
void my_assert_fail ( const Char* expr, const Char* file, Int line, const Char* fn )
{
   char buf[1000];
   static Bool entered = False;
   if (entered) 
      my_exit(2);
   entered = True;
   sprintf(buf, "\n%s: %s:%d (%s): Assertion `%s' failed.\n",
                "valgrind", file, line, fn, expr );
   cat_n_send ( "", buf );
   sprintf(buf, "Please report this bug to me at: %s\n\n", 
                 VG_EMAIL_ADDR);
   cat_n_send ( "", buf );
   my_exit(1);
}

#define MY__STRING(__str)  #__str

#define my_assert(expr)                                               \
  ((void) ((expr) ? 0 :						      \
	   (my_assert_fail  (MY__STRING(expr),			      \
			      __FILE__, __LINE__,                     \
                              __PRETTY_FUNCTION__), 0)))

/* --------------------------------------------------------------- */

static __inline__
void __my_pthread_testcancel(void)
{
   int res;
   ensure_valgrind("__my_pthread_testcancel");
   VALGRIND_MAGIC_SEQUENCE(res, (-1) /* default */,
                           VG_USERREQ__TESTCANCEL,
                           0, 0, 0, 0);
   my_assert(res == 0);
}


/* ---------------------------------------------------------------------
   Hook for running __libc_freeres once the program exits.
   ------------------------------------------------------------------ */

void VG_(__libc_freeres_wrapper)( void )
{
   int res;
#ifndef __UCLIBC__
   extern void __libc_freeres(void);
   __libc_freeres();
#endif
   VALGRIND_MAGIC_SEQUENCE(res, 0 /* default */,
                           VG_USERREQ__LIBC_FREERES_DONE, 0, 0, 0, 0);
   /*NOTREACHED*/
   vg_assert(12345+54321 == 999999);
}

/* ---------------------------------------------------------------------
   Useful for skins that want to replace certain functions
   ------------------------------------------------------------------ */

Bool VG_(is_running_on_simd_CPU)(void)
{
   return VG_(running_on_simd_CPU);
}



/*--------------------------------------------------------------------*/
/*--- end                                           vg_intercept.c ---*/
/*--------------------------------------------------------------------*/

