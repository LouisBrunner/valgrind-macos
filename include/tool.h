/*-*- c -*- ----------------------------------------------------------*/
/*--- Header for lots of tool stuff.                        tool.h ---*/
/*--------------------------------------------------------------------*/

/*
   This file is part of Valgrind, a dynamic binary instrumentation
   framework.

   Copyright (C) 2000-2005 Julian Seward
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

#ifndef __TOOL_H
#define __TOOL_H

#include <stdarg.h>       /* ANSI varargs stuff  */

#include "basic_types.h"
#include "tool_asm.h"           /* asm stuff */

#if defined(VGO_linux)
#  include "vki-linux.h"
#else
#  error Unknown OS
#endif

/*====================================================================*/
/*=== Build options and table sizes.                               ===*/
/*====================================================================*/

/* The maximum number of pthreads that we support.  This is
   deliberately not very high since our implementation of some of the
   scheduler algorithms is surely O(N) in the number of threads, since
   that's simple, at least.  And (in practice) we hope that most
   programs do not need many threads. */
#define VG_N_THREADS 100

/*====================================================================*/
/*=== Useful stuff to call from generated code                     ===*/
/*====================================================================*/

/* ------------------------------------------------------------------ */
/* Thread-related stuff */

/* Get the TID of the thread which currently has the CPU. */
extern ThreadId VG_(get_running_tid) ( void );

/* Searches through all thread's stacks to see if any match.  Returns
   VG_INVALID_THREADID if none match. */
extern ThreadId VG_(first_matching_thread_stack)
                        ( Bool (*p) ( Addr stack_min, Addr stack_max, void* d ),
                          void* d );

/*====================================================================*/
/*=== Valgrind's version of libc                                   ===*/
/*====================================================================*/

/* ------------------------------------------------------------------ */
/* Register an interest in apparently internal faults; used code which
   wanders around dangerous memory (ie, leakcheck).  The catcher is
   not expected to return. */
extern void VG_(set_fault_catcher)(void (*catcher)(Int sig, Addr addr));

/* Calls "mark_addr" with register values (which may or may not be pointers) */
extern void VG_(mark_from_registers)(void (*mark_addr)(Addr addr));

/* ------------------------------------------------------------------ */
/* other, randomly useful functions */
extern Bool VG_(has_cpuid) ( void );

extern void VG_(cpuid) ( UInt eax,
                         UInt *eax_ret, UInt *ebx_ret,
                         UInt *ecx_ret, UInt *edx_ret );

#endif   /* __TOOL_H */


