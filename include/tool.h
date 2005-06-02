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

/* No, really.  I _am_ that strange. */
#define OINK(nnn) VG_(message)(Vg_DebugMsg, "OINK %d",nnn)

/* Path to all our library/aux files */
extern const Char *VG_(libdir);

/* Client args */
extern Int    VG_(client_argc);
extern Char** VG_(client_argv);

/* Client environment.  Can be inspected with VG_(getenv)() */
extern Char** VG_(client_envp);


/*====================================================================*/
/*=== Printing messages for the user                               ===*/
/*====================================================================*/

/* Print a message prefixed by "??<pid>?? "; '?' depends on the VgMsgKind.
   Should be used for all user output. */

typedef
   enum { Vg_UserMsg,         /* '?' == '=' */
          Vg_DebugMsg,        /* '?' == '-' */
          Vg_DebugExtraMsg,   /* '?' == '+' */
          Vg_ClientMsg        /* '?' == '*' */
   }
   VgMsgKind;

/* Send a single-part message.  Appends a newline. */
extern UInt VG_(message)    ( VgMsgKind kind, const HChar* format, ... );
extern UInt VG_(vmessage)   ( VgMsgKind kind, const HChar* format, va_list vargs );


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

#if !defined(NULL)
#  define NULL ((void*)0)
#endif

/* ------------------------------------------------------------------ */
/* stdio.h
 *
 * Note that they all output to the file descriptor given by the
 * --log-fd/--log-file/--log-socket argument, which defaults to 2 (stderr).
 * Hence no need for VG_(fprintf)().
 */
extern UInt VG_(printf)  ( const HChar *format, ... );
extern UInt VG_(vprintf) ( const HChar *format, va_list vargs );
/* too noisy ...  __attribute__ ((format (printf, 1, 2))) ; */
extern UInt VG_(sprintf) ( Char* buf, const HChar* format, ... );
extern UInt VG_(vsprintf)( Char* buf, const HChar* format, va_list vargs );

extern Int  VG_(rename) ( Char* old_name, Char* new_name );

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
extern Int  VG_(getdents)( UInt fd, struct vki_dirent *dirp, UInt count );
extern Int  VG_(readlink)( Char* path, Char* buf, UInt bufsize );
extern Int  VG_(getpid)  ( void );
extern Int  VG_(getppid) ( void );
extern Int  VG_(getpgrp) ( void );
extern Int  VG_(gettid)	 ( void );
extern Int  VG_(setpgid) ( Int pid, Int pgrp );

extern Int  VG_(open)   ( const Char* pathname, Int flags, Int mode );
extern Int  VG_(read)   ( Int fd, void* buf, Int count);
extern Int  VG_(write)  ( Int fd, const void* buf, Int count);
extern OffT VG_(lseek)  ( Int fd, OffT offset, Int whence);
extern void VG_(close)  ( Int fd );

extern Int  VG_(pipe)   ( Int fd[2] );

/* Nb: VG_(rename)() declared in stdio.h section above */
extern Int  VG_(unlink) ( Char* file_name );
extern Int  VG_(stat)   ( Char* file_name, struct vki_stat* buf );
extern Int  VG_(fstat)  ( Int   fd,        struct vki_stat* buf );
extern Int  VG_(dup2)   ( Int oldfd, Int newfd );

extern Char* VG_(getcwd) ( Char* buf, SizeT size );

/* Easier to use than VG_(getcwd)() -- does the buffer fiddling itself.
   String put into 'cwd' is VG_(malloc)'d, and should be VG_(free)'d.
   Returns False if it fails.  Will fail if the pathname is > 65535 bytes. */
extern Bool VG_(getcwd_alloc) ( Char** cwd );

/* ------------------------------------------------------------------ */
/* assert.h */
/* Asserts permanently enabled -- no turning off with NDEBUG.  Hurrah! */

/* This odd definition lets us stringify VG_(x) function names to
   "vgPlain_x".  We need to do two macroexpansions to get the VG_ macro
   expanded before stringifying. */
#define VG_STRINGIFY_WRK(x)   #x
#define VG_STRINGIFY(x)       VG_STRINGIFY_WRK(x)

#define tl_assert(expr)                                                 \
  ((void) ((expr) ? 0 :                                                 \
           (VG_(assert_fail) (/*isCore?*/False, VG_STRINGIFY(expr),     \
                              __FILE__, __LINE__, __PRETTY_FUNCTION__,  \
                              ""),                                      \
                              0)))

#define tl_assert2(expr, format, args...)                               \
  ((void) ((expr) ? 0 :                                                 \
           (VG_(assert_fail) (/*isCore?*/False, VG_STRINGIFY(expr),     \
                              __FILE__, __LINE__, __PRETTY_FUNCTION__,  \
                              format, ##args),                          \
                              0)))

__attribute__ ((__noreturn__))
extern void VG_(assert_fail) ( Bool isCore, const Char* expr, const Char* file, 
                               Int line, const Char* fn, 
                               const HChar* format, ... );

/* ------------------------------------------------------------------ */
/* Get memory by anonymous mmap. */
extern void* VG_(get_memory_from_mmap) ( SizeT nBytes, Char* who );

extern Bool VG_(is_client_addr) (Addr a);

extern Bool VG_(is_shadow_addr) (Addr a);
extern Addr VG_(get_shadow_size)(void);

extern void *VG_(shadow_alloc)(UInt size);

extern Bool VG_(is_addressable)(Addr p, SizeT sz, UInt prot);

/* Register an interest in apparently internal faults; used code which
   wanders around dangerous memory (ie, leakcheck).  The catcher is
   not expected to return. */
extern void VG_(set_fault_catcher)(void (*catcher)(Int sig, Addr addr));

/* initialize shadow pages in the range [p, p+sz) This calls
   init_shadow_page for each one.  It should be a lot more efficient
   for bulk-initializing shadow pages than faulting on each one. 
*/
extern void VG_(init_shadow_range)(Addr p, UInt sz, Bool call_init);

/* Calls into the core used by leak-checking */

/* Calls "add_rootrange" with each range of memory which looks like a
   plausible source of root pointers. */
extern void VG_(find_root_memory)(void (*add_rootrange)(Addr addr, SizeT sz));

/* Calls "mark_addr" with register values (which may or may not be pointers) */
extern void VG_(mark_from_registers)(void (*mark_addr)(Addr addr));

/* ------------------------------------------------------------------ */
/* signal.h.

   Note that these use the vk_ (kernel) structure
   definitions, which are different in places from those that glibc
   defines.  Since we're operating right at the kernel interface, glibc's view
   of the world is entirely irrelevant. */

/* --- Signal set ops --- */
extern Int  VG_(sigfillset)  ( vki_sigset_t* set );
extern Int  VG_(sigemptyset) ( vki_sigset_t* set );

extern Bool VG_(isfullsigset)  ( const vki_sigset_t* set );
extern Bool VG_(isemptysigset) ( const vki_sigset_t* set );
extern Bool VG_(iseqsigset)    ( const vki_sigset_t* set1,
                                 const vki_sigset_t* set2 );

extern Int  VG_(sigaddset)   ( vki_sigset_t* set, Int signum );
extern Int  VG_(sigdelset)   ( vki_sigset_t* set, Int signum );
extern Int  VG_(sigismember) ( const vki_sigset_t* set, Int signum );

extern void VG_(sigaddset_from_set) ( vki_sigset_t* dst, vki_sigset_t* src );
extern void VG_(sigdelset_from_set) ( vki_sigset_t* dst, vki_sigset_t* src );

/* --- Mess with the kernel's sig state --- */
extern Int VG_(sigprocmask) ( Int how, const vki_sigset_t* set,
                              vki_sigset_t* oldset );
extern Int VG_(sigaction)   ( Int signum,
                              const struct vki_sigaction* act,
                              struct vki_sigaction* oldact );

extern Int VG_(sigtimedwait)( const vki_sigset_t *, vki_siginfo_t *, 
			      const struct vki_timespec * );

extern Int VG_(signal)      ( Int signum, void (*sighandler)(Int) );
extern Int VG_(sigaltstack) ( const vki_stack_t* ss, vki_stack_t* oss );

extern Int VG_(kill)        ( Int pid, Int signo );
extern Int VG_(tkill)       ( ThreadId tid, Int signo );
extern Int VG_(sigpending)  ( vki_sigset_t* set );

extern Int VG_(waitpid)	    ( Int pid, Int *status, Int options );

/* ------------------------------------------------------------------ */
/* socket.h. */

extern Int VG_(getsockname) ( Int sd, struct vki_sockaddr *name, Int *namelen);
extern Int VG_(getpeername) ( Int sd, struct vki_sockaddr *name, Int *namelen);
extern Int VG_(getsockopt) ( Int sd, Int level, Int optname, void *optval,
                             Int *optlen);

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


