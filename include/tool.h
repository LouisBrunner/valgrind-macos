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

/* No, really.  I _am_ that strange. */
#define OINK(nnn) VG_(message)(Vg_DebugMsg, "OINK %d",nnn)

/* Path to all our library/aux files */
extern const Char *VG_(libdir);


/* Use this for normal null-termination-style string comparison */
#define VG_STREQ(s1,s2) (s1 != NULL && s2 != NULL \
                         && VG_(strcmp)((s1),(s2))==0)

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
/*=== Profiling                                                    ===*/
/*====================================================================*/

/* Nb: VG_(register_profile_event)() relies on VgpUnc being the first one */
#define VGP_CORE_LIST \
   /* These ones depend on the core */                \
   VGP_PAIR(VgpUnc,         "unclassified"),          \
   VGP_PAIR(VgpStartup,     "startup"),               \
   VGP_PAIR(VgpRun,         "running"),               \
   VGP_PAIR(VgpSched,       "scheduler"),             \
   VGP_PAIR(VgpMalloc,      "low-lev malloc/free"),   \
   VGP_PAIR(VgpCliMalloc,   "client  malloc/free"),   \
   VGP_PAIR(VgpTranslate,   "translate-main"),        \
   VGP_PAIR(VgpToUCode,     "to-ucode"),              \
   VGP_PAIR(VgpFromUcode,   "from-ucode"),            \
   VGP_PAIR(VgpImprove,     "improve"),               \
   VGP_PAIR(VgpESPUpdate,   "ESP-update"),            \
   VGP_PAIR(VgpRegAlloc,    "reg-alloc"),             \
   VGP_PAIR(VgpLiveness,    "liveness-analysis"),     \
   VGP_PAIR(VgpDoLRU,       "do-lru"),                \
   VGP_PAIR(VgpSlowFindT,   "slow-search-transtab"),  \
   VGP_PAIR(VgpExeContext,  "exe-context"),           \
   VGP_PAIR(VgpReadSyms,    "read-syms"),             \
   VGP_PAIR(VgpSearchSyms,  "search-syms"),           \
   VGP_PAIR(VgpAddToT,      "add-to-transtab"),       \
   VGP_PAIR(VgpCoreSysWrap, "core-syscall-wrapper"),  \
   VGP_PAIR(VgpDemangle,    "demangle"),              \
   VGP_PAIR(VgpCoreCheapSanity,     "core-cheap-sanity"),     \
   VGP_PAIR(VgpCoreExpensiveSanity, "core-expensive-sanity"), \
   /* These ones depend on the tool */                \
   VGP_PAIR(VgpPreCloInit,  "pre-clo-init"),          \
   VGP_PAIR(VgpPostCloInit, "post-clo-init"),         \
   VGP_PAIR(VgpInstrument,  "instrument"),            \
   VGP_PAIR(VgpToolSysWrap, "tool-syscall-wrapper"),  \
   VGP_PAIR(VgpToolCheapSanity,     "tool-cheap-sanity"),     \
   VGP_PAIR(VgpToolExpensiveSanity, "tool-expensive-sanity"), \
   VGP_PAIR(VgpFini,        "fini")

#define VGP_PAIR(n,name) n
typedef enum { VGP_CORE_LIST } VgpCoreCC;
#undef  VGP_PAIR

/* When registering tool profiling events, ensure that the 'n' value is in
 * the range (VgpFini+1..) */
extern void VG_(register_profile_event) ( Int n, Char* name );

extern void VG_(pushcc) ( UInt cc );
extern void VG_(popcc)  ( UInt cc );

/* Define them only if they haven't already been defined by vg_profile.c */
#ifndef VGP_PUSHCC
#  define VGP_PUSHCC(x)
#endif
#ifndef VGP_POPCC
#  define VGP_POPCC(x)
#endif


/*====================================================================*/
/*=== Useful stuff to call from generated code                     ===*/
/*====================================================================*/

/* ------------------------------------------------------------------ */
/* General stuff */

/* Check if an address/whatever is aligned */
#define VG_IS_4_ALIGNED(aaa_p)    (0 == (((Addr)(aaa_p)) & ((Addr)0x3)))
#define VG_IS_8_ALIGNED(aaa_p)    (0 == (((Addr)(aaa_p)) & ((Addr)0x7)))
#define VG_IS_16_ALIGNED(aaa_p)   (0 == (((Addr)(aaa_p)) & ((Addr)0xf)))
#define VG_IS_WORD_ALIGNED(aaa_p) (0 == (((Addr)(aaa_p)) & ((Addr)(sizeof(Addr)-1))))
#define VG_IS_PAGE_ALIGNED(aaa_p) (0 == (((Addr)(aaa_p)) & ((Addr)(VKI_PAGE_SIZE-1))))


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

/* Valgrind doesn't use libc at all, for good reasons (trust us).  So here
   are its own versions of C library functions, but with VG_ prefixes.  Note
   that the types of some are slightly different to the real ones.  Some
   additional useful functions are provided too; descriptions of how they
   work are given below. */

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

extern Long  VG_(atoll)  ( Char* str );

/* Like atoll(), but converts a number of base 16 */
extern Long  VG_(atoll16) ( Char* str );

/* Like atoll(), but converts a number of base 2..36 */
extern Long  VG_(atoll36) ( UInt base, Char* str );

/* Like qsort(), but does shell-sort.  The size==1/2/4 cases are specialised. */
extern void VG_(ssort)( void* base, SizeT nmemb, SizeT size,
                        Int (*compar)(void*, void*) );


/* ------------------------------------------------------------------ */
/* ctype.h */
extern Bool VG_(isspace) ( Char c );
extern Bool VG_(isdigit) ( Char c );
extern Char VG_(toupper) ( Char c );


/* ------------------------------------------------------------------ */
/* string.h */
extern Int   VG_(strlen)         ( const Char* str );
extern Char* VG_(strcat)         ( Char* dest, const Char* src );
extern Char* VG_(strncat)        ( Char* dest, const Char* src, Int n );
extern Char* VG_(strpbrk)        ( const Char* s, const Char* accpt );
extern Char* VG_(strcpy)         ( Char* dest, const Char* src );
extern Char* VG_(strncpy)        ( Char* dest, const Char* src, Int ndest );
extern Int   VG_(strcmp)         ( const Char* s1, const Char* s2 );
extern Int   VG_(strncmp)        ( const Char* s1, const Char* s2, Int nmax );
extern Char* VG_(strstr)         ( const Char* haystack, Char* needle );
extern Char* VG_(strchr)         ( const Char* s, Char c );
extern Char* VG_(strrchr)        ( const Char* s, Char c );
extern Char* VG_(strdup)         ( const Char* s);
extern void* VG_(memcpy)         ( void *d, const void *s, Int sz );
extern void* VG_(memset)         ( void *s, Int c, Int sz );
extern Int   VG_(memcmp)         ( const void* s1, const void* s2, Int n );

/* Like strcmp() and strncmp(), but stop comparing at any whitespace. */
extern Int   VG_(strcmp_ws)      ( const Char* s1, const Char* s2 );
extern Int   VG_(strncmp_ws)     ( const Char* s1, const Char* s2, Int nmax );

/* Like strncpy(), but if 'src' is longer than 'ndest' inserts a '\0' as the
   last character. */
extern void  VG_(strncpy_safely) ( Char* dest, const Char* src, Int ndest );

/* Mini-regexp function.  Searches for 'pat' in 'str'.  Supports
 * meta-symbols '*' and '?'.  '\' escapes meta-symbols. */
extern Bool  VG_(string_match)   ( const Char* pat, const Char* str );


/* ------------------------------------------------------------------ */
/* math.h */
/* Returns the base-2 logarithm of x. */
extern Int VG_(log2) ( Int x );


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


