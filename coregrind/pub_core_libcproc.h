
/*--------------------------------------------------------------------*/
/*--- Process-related libc stuff.              pub_core_libcproc.h ---*/
/*--------------------------------------------------------------------*/

/*
   This file is part of Valgrind, a dynamic binary instrumentation
   framework.

   Copyright (C) 2000-2017 Julian Seward
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
   along with this program; if not, see <http://www.gnu.org/licenses/>.

   The GNU General Public License is contained in the file COPYING.
*/

#ifndef __PUB_CORE_LIBCPROC_H
#define __PUB_CORE_LIBCPROC_H

//--------------------------------------------------------------------
// PURPOSE: This module contains libc code related to the process.
// It's a bit of a mixed bag.
//--------------------------------------------------------------------

#include "config.h"           // Crucial: ensure we get ENABLE_INNER
#include "pub_tool_libcproc.h"

/* The directory we look for all our auxiliary files in.  Useful for
   running Valgrind out of a build tree without having to do "make
   install".  Inner valgrinds require a different lib variable, else
   they end up picking up .so's etc intended for the outer
   valgrind. */
#ifdef ENABLE_INNER
#  define VALGRIND_LIB     "VALGRIND_LIB_INNER"
#else
#  define VALGRIND_LIB     "VALGRIND_LIB"
#endif

/* Additional command-line arguments; they are overridden by actual
   command-line option.  Each argument is separated by spaces.  There
   is no quoting mechanism.  */
#define VALGRIND_OPTS    "VALGRIND_OPTS"

/* The full name of Valgrind's stage1 (launcher) executable.  This is
   set by stage1 and read by stage2, and is used for recursive
   invocations of Valgrind on child processes. 
   
   For self-hosting, the inner and outer Valgrinds must use different
   names to avoid collisions.  */
#ifdef ENABLE_INNER
#  define VALGRIND_LAUNCHER  "VALGRIND_LAUNCHER_INNER"
#else
#  define VALGRIND_LAUNCHER  "VALGRIND_LAUNCHER"
#endif


// Environment manipulations
extern HChar **VG_(env_setenv)   ( HChar ***envp, const HChar* varname,
                                   const HChar *val );
extern void    VG_(env_unsetenv) ( HChar **env, const HChar *varname,
                                   void (*free_fn) ( void *) );
extern void    VG_(env_remove_valgrind_env_stuff) ( HChar** env,
                                                    Bool ro_strings,
                                                    void (*free_fn) (void *) );
extern HChar **VG_(env_clone)    ( HChar **env_clone );

// misc
extern Int  VG_(getgroups)( Int size, UInt* list );
extern Int  VG_(ptrace)( Int request, Int pid, void *addr, void *data );

// atfork
extern void VG_(do_atfork_pre)    ( ThreadId tid );
extern void VG_(do_atfork_parent) ( ThreadId tid );
extern void VG_(do_atfork_child)  ( ThreadId tid );

#if defined(VGO_freebsd)
// sysctlbyname, getosreldate, is32on64
extern Int VG_(sysctlbyname)(const HChar *name, void *oldp, SizeT *oldlenp, const void *newp, SizeT newlen);
extern Int VG_(getosreldate)(void);
extern Bool VG_(is32on64)(void);

struct auxv {
   Word a_type;
   union {
      void *a_ptr;
      Word a_val;
   } u;
};

#endif

// icache invalidation
extern void VG_(invalidate_icache) ( void *ptr, SizeT nbytes );

// dcache flushing
extern void VG_(flush_dcache) ( void *ptr, SizeT nbytes );

#endif   // __PUB_CORE_LIBCPROC_H

/*--------------------------------------------------------------------*/
/*--- end                                                          ---*/
/*--------------------------------------------------------------------*/
