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
/*=== Useful macros                                                ===*/
/*====================================================================*/

// 'a' -- the alignment -- must be a power of 2
#define VG_ROUNDDN(p, a)   ((Addr)(p) & ~((Addr)(a)-1))
#define VG_ROUNDUP(p, a)   VG_ROUNDDN((p)+(a)-1, (a))
#define VG_PGROUNDDN(p)    VG_ROUNDDN(p, VKI_PAGE_SIZE)
#define VG_PGROUNDUP(p)    VG_ROUNDUP(p, VKI_PAGE_SIZE)

/* Path to all our library/aux files */
extern const Char *VG_(libdir);

/* Client args */
extern Int    VG_(client_argc);
extern Char** VG_(client_argv);

/* Client environment.  Can be inspected with VG_(getenv)() */
extern Char** VG_(client_envp);


/*====================================================================*/
/*=== Useful stuff to call from generated code                     ===*/
/*====================================================================*/

/* ------------------------------------------------------------------ */
/* Thread-related stuff */

/* Special magic value for an invalid ThreadId.  It corresponds to
   LinuxThreads using zero as the initial value for
   pthread_mutex_t.__m_owner and pthread_cond_t.__c_waiting. */
#define VG_INVALID_THREADID ((ThreadId)(0))

/* Get the TID of the thread which currently has the CPU. */
extern ThreadId VG_(get_running_tid) ( void );

/* Searches through all thread's stacks to see if any match.  Returns
   VG_INVALID_THREADID if none match. */
extern ThreadId VG_(first_matching_thread_stack)
                        ( Bool (*p) ( Addr stack_min, Addr stack_max, void* d ),
                          void* d );

/* Get parts of the client's state. */
extern Addr VG_(get_SP) ( ThreadId tid );
extern Addr VG_(get_IP) ( ThreadId tid );


/*====================================================================*/
/*=== Valgrind's version of libc                                   ===*/
/*====================================================================*/

/* ------------------------------------------------------------------ */
/* stdlib.h */

/* terminate everything */
extern void VG_(exit)( Int status )
            __attribute__ ((__noreturn__));

/* Prints a panic message (a constant string), appends newline and bug
   reporting info, aborts. */
__attribute__ ((__noreturn__))
extern void  VG_(tool_panic) ( Char* str );

/* Looks up VG_(client_envp) */
extern Char* VG_(getenv) ( Char* name );

/* Get client resource limit*/
extern Int VG_(getrlimit) ( Int resource, struct vki_rlimit *rlim );

/* Set client resource limit*/
extern Int VG_(setrlimit) ( Int resource, const struct vki_rlimit *rlim );

/* Crude stand-in for the glibc system() call. */
extern Int   VG_(system) ( Char* cmd );

/* ------------------------------------------------------------------ */
/* unistd.h, fcntl.h, sys/stat.h */
extern Int  VG_(getpid)  ( void );
extern Int  VG_(getppid) ( void );
extern Int  VG_(getpgrp) ( void );
extern Int  VG_(gettid)	 ( void );
extern Int  VG_(setpgid) ( Int pid, Int pgrp );

/* ------------------------------------------------------------------ */
/* Register an interest in apparently internal faults; used code which
   wanders around dangerous memory (ie, leakcheck).  The catcher is
   not expected to return. */
extern void VG_(set_fault_catcher)(void (*catcher)(Int sig, Addr addr));

/* Calls "mark_addr" with register values (which may or may not be pointers) */
extern void VG_(mark_from_registers)(void (*mark_addr)(Addr addr));

extern Int VG_(waitpid)	    ( Int pid, Int *status, Int options );

/* ------------------------------------------------------------------ */
/* other, randomly useful functions */
extern UInt VG_(read_millisecond_timer) ( void );

extern Bool VG_(has_cpuid) ( void );

extern void VG_(cpuid) ( UInt eax,
                         UInt *eax_ret, UInt *ebx_ret,
                         UInt *ecx_ret, UInt *edx_ret );

/*====================================================================*/
/*=== Functions for shadow registers                               ===*/
/*====================================================================*/

// For get/set, 'area' is where the asked-for shadow state will be copied
// into/from.
extern void VG_(get_shadow_regs_area) ( ThreadId tid, OffT guest_state_offset,
                                        SizeT size, UChar* area );
extern void VG_(set_shadow_regs_area) ( ThreadId tid, OffT guest_state_offset,
                                        SizeT size, const UChar* area );

/*====================================================================*/
/*=== Arch-specific stuff                                          ===*/
/*====================================================================*/

/* VGA_STACK_REDZONE_SZB: how many bytes below the stack pointer are validly
 * addressible? */
#if defined(VGA_x86)
#  define VGA_REGPARM(n)            __attribute__((regparm(n)))
#  define VGA_MIN_INSTR_SZB         1
#  define VGA_MAX_INSTR_SZB        16
#  define VGA_STACK_REDZONE_SZB     0
#elif defined(VGA_amd64)
#  define VGA_REGPARM(n)            /* */
#  define VGA_MIN_INSTR_SZB         1
#  define VGA_MAX_INSTR_SZB        16
#  define VGA_STACK_REDZONE_SZB   128
#elif defined(VGA_arm)
#  define VGA_REGPARM(n)            /* */
#  define VGA_MIN_INSTR_SZB         4
#  define VGA_MAX_INSTR_SZB         4 
#  define VGA_STACK_REDZONE_SZB     0
#else
#  error Unknown platform
#endif

#endif   /* __TOOL_H */


