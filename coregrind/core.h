
/*--------------------------------------------------------------------*/
/*--- A header file for all private parts of Valgrind's core.      ---*/
/*--- Include no other! (more or less...)                          ---*/
/*---                                                       core.h ---*/
/*--------------------------------------------------------------------*/

/*
   This file is part of Valgrind, an extensible x86 protected-mode
   emulator for monitoring program execution on x86-Unixes.

   Copyright (C) 2000-2004 Julian Seward 
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

#ifndef __CORE_H
#define __CORE_H

/*
   Header hierarchy:

   - core C   files include core.h
   - core asm files include core_asm.h
   - tool C   files include tool.h
   - tool asm files include tool_asm.h

   - The hierarchy of the header files themselves is based around the
     following rules:

      - core headers     include  tool headers
      - generic headers  include  arch/OS/platform headers
      - C headers        include  asm headers

     This gives the following hierarchy (only showing 'arch' headers, not
     'os' or 'platform' headers), where arrows indicate inclusion, and
     $VG_ARCH==x86:


   (include/x86/tool_arch_asm.h?) <----- coregrind/x86/core_arch_asm.h
              ^   ^                          ^   ^
             /     \                        /     \
            /       \                      /       \
           /         \                    /         \
 include/tool_asm.h <-\---- coregrind/core_asm.h     \
           ^           \                  ^           \
            \  include/x86/tool_arch.h <--------coregrind/x86/core_arch.h
             \         ^                    \         ^
              \       /                      \       /
               \     /                        \     /
                \   /                          \   /
           include/tool.h <------------ coregrind/core.h


   Note that core.h contains the *declarations* of arch-specific functions
   and variables, which can be used by the core_arch.h file of any
   architecture.  (The functions/variables are *defined* within arch/.)
   However, arch-specific macros and types cannot go into core.h, because
   there is no separation between declaration and definition for
   macros/types, so they instead go into $VG_ARCH/core_arch.h.

   The tool-specific headers are all in include/ so they can be seen by any
   external tools.
*/


/* For system call numbers __NR_... */
#include "vki_unistd.h"

#include "core_asm.h"      // asm stuff
#include "tool.h"          // tool stuff
#include "core_arch.h"     // arch-specific stuff,  eg. x86/core_arch.h

// Ugly: this is needed by linux/core_os.h
typedef struct _ThreadState ThreadState;

#include "core_os.h"       // OS-specific stuff,    eg. linux/core_os.h
#include "core_platform.h" // platform-specific stuff,
                           //   eg. x86-linux/core_platform.h

#include "valgrind.h"

#undef TL_
#define TL_(x)	vgToolInternal_##x


/* ---------------------------------------------------------------------
   Build options and table sizes.  You should be able to change these
   options or sizes, recompile, and still have a working system.
   ------------------------------------------------------------------ */

/* Constants for the slow translation lookup cache. */
#define VG_TRANSTAB_SLOW_BITS 11
#define VG_TRANSTAB_SLOW_SIZE (1 << VG_TRANSTAB_SLOW_BITS)
#define VG_TRANSTAB_SLOW_MASK ((VG_TRANSTAB_SLOW_SIZE) - 1)

/* Size of a buffer used for creating messages. */
#define M_VG_MSGBUF 10000

/* Size of a smallish table used to read /proc/self/map entries. */
#define M_PROCMAP_BUF 50000

/* Max length of pathname to a .so/executable file. */
#define M_VG_LIBNAMESTR 100

/* Max length of a text fragment used to construct error messages. */
#define M_VG_ERRTXT 4096

/* Max length of the string copied from env var VG_ARGS at startup. */
#define M_VG_CMDLINE_STRLEN 1000

/* Max number of options for Valgrind which we can handle. */
#define M_VG_CMDLINE_OPTS 100

/* After this many different unsuppressed errors have been observed,
   be more conservative about collecting new ones. */
#define M_VG_COLLECT_ERRORS_SLOWLY_AFTER 50

/* After this many different unsuppressed errors have been observed,
   stop collecting errors at all, and tell the user their program is
   evidently a steaming pile of camel dung. */
#define M_VG_COLLECT_NO_ERRORS_AFTER_SHOWN 300

/* After this many total errors have been observed, stop collecting
   errors at all.  Counterpart to M_VG_COLLECT_NO_ERRORS_AFTER_SHOWN. */
#define M_VG_COLLECT_NO_ERRORS_AFTER_FOUND 30000

/* The maximum number of calls we're prepared to save in a
   backtrace. */
#define VG_DEEPEST_BACKTRACE 50

/* Number of lists in which we keep track of ExeContexts.  Should be
   prime. */
#define VG_N_EC_LISTS 4999 /* a prime number */

/* Defines the thread-scheduling timeslice, in terms of the number of
   basic blocks we attempt to run each thread for.  Smaller values
   give finer interleaving but much increased scheduling overheads. */
#define VG_SCHEDULING_QUANTUM   50000

/* Number of file descriptors that Valgrind tries to reserve for
   it's own use - two per thread plues a small number of extras. */
#define VG_N_RESERVED_FDS (VG_N_THREADS*2 + 4)

/* Stack size for a thread.  We try and check that they do not go
   beyond it. */
#define VG_PTHREAD_STACK_SIZE (1 << 20)

/* Number of entries in each thread's cleanup stack. */
#define VG_N_CLEANUPSTACK 16

/* Number of entries in each thread's fork-handler stack. */
#define VG_N_FORKHANDLERSTACK 4

/* Max number of callers for context in a suppression. */
#define VG_N_SUPP_CALLERS  4

/* Numer of entries in each thread's signal queue. */
#define VG_N_SIGNALQUEUE 8

/* Useful macros */
/* a - alignment - must be a power of 2 */
#define ROUNDDN(p, a)	((Addr)(p) & ~((a)-1))
#define ROUNDUP(p, a)	ROUNDDN((p)+(a)-1, (a))
#define PGROUNDDN(p)	ROUNDDN(p, VKI_PAGE_SIZE)
#define PGROUNDUP(p)	ROUNDUP(p, VKI_PAGE_SIZE)

/* ---------------------------------------------------------------------
   Environment variables
   ------------------------------------------------------------------ */

/* The directory we look for all our auxillary files in */
#define VALGRINDLIB	"VALGRINDLIB"

/* Additional command-line arguments; they are overridden by actual
   command-line option.  Each argument is separated by spaces.  There
   is no quoting mechanism.
 */
#define VALGRINDOPTS	"VALGRIND_OPTS"

/* If this variable is present in the environment, then valgrind will
   not parse the command line for options at all; all options come
   from this variable.  Arguments are terminated by ^A (\001).  There
   is no quoting mechanism.

   This variable is not expected to be set by anything other than
   Valgrind itself, as part of its handling of execve with
   --trace-children=yes.  This variable should not be present in the
   client environment.
 */
#define VALGRINDCLO	"_VALGRIND_CLO"


/* ---------------------------------------------------------------------
   Command-line-settable options
   ------------------------------------------------------------------ */

/* Default destination port to be used in logging over a network, if
   none specified. */
#define VG_CLO_DEFAULT_LOGPORT 1500

/* The max number of suppression files. */
#define VG_CLO_MAX_SFILES 10

/* Default debugger command. */
#define VG_CLO_DEFAULT_DBCOMMAND GDB_PATH " -nw %f %p"

/* Describes where logging output is to be sent. */
typedef
   enum {
      VgLogTo_Fd,
      VgLogTo_File,
      VgLogTo_Socket
   } VgLogTo;

/* pid of main process */
extern Int VG_(main_pid);

/* pgrp of process (global to all threads) */
extern Int VG_(main_pgrp);

/* Application-visible file descriptor limits */
extern Int VG_(fd_soft_limit);
extern Int VG_(fd_hard_limit);

/* Vex iropt control */
extern VexControl VG_(clo_vex_control);
/* Should we stop collecting errors if too many appear?  default: YES */
extern Bool  VG_(clo_error_limit);
/* Enquire about whether to attach to a debugger at errors?   default: NO */
extern Bool  VG_(clo_db_attach);
/* The debugger command?  default: whatever gdb ./configure found */
extern Char* VG_(clo_db_command);
/* Enquire about generating a suppression for each error?   default: NO */
extern Bool  VG_(clo_gen_suppressions);
/* Sanity-check level: 0 = none, 1 (default), > 1 = expensive. */
extern Int   VG_(clo_sanity_level);
/* Automatically attempt to demangle C++ names?  default: YES */
extern Bool  VG_(clo_demangle);
/* Simulate child processes? default: NO */
extern Bool  VG_(clo_trace_children);

/* Where logging output is to be sent to.

   When log_to == VgLogTo_Fd, clo_log_fd holds the file id, and is
   taken from the command line.  clo_log_name is irrelevant.

   When log_to == VgLogTo_File, clo_log_name holds the log-file
   name, and is taken from the command line.  clo_log_fd is then
   made to hold the relevant file id, by opening clo_log_name
   (concatenated with the process ID) for writing.

   When log_to == VgLogTo_Socket, clo_log_name holds the
   hostname:portnumber pair, and is taken from the command line.
   clo_log_fd is then made to hold the relevant file handle, by
   opening a connection to said hostname:portnumber pair. 

   Global default is to set log_to == VgLogTo_Fd and log_fd == 2
   (stderr). */
extern VgLogTo VG_(clo_log_to);
extern Int     VG_(clo_log_fd);
extern Char*   VG_(clo_log_name);

/* Add timestamps to log messages?  default: NO */
extern Bool  VG_(clo_time_stamp);

/* The file descriptor to read for input.  default: 0 == stdin */
extern Int   VG_(clo_input_fd);
/* The number of suppression files specified. */
extern Int   VG_(clo_n_suppressions);
/* The names of the suppression files. */
extern Char* VG_(clo_suppressions)[VG_CLO_MAX_SFILES];

/* Single stepping?  default: NO */
extern Bool  VG_(clo_single_step);
/* Code improvement?  default: YES */
extern Bool  VG_(clo_optimise);
/* DEBUG: print generated code?  default: 00000 ( == NO ) */
extern Bool  VG_(clo_trace_codegen);
/* DEBUG: print system calls?  default: NO */
extern Bool  VG_(clo_trace_syscalls);
/* DEBUG: print signal details?  default: NO */
extern Bool  VG_(clo_trace_signals);
/* DEBUG: print symtab details?  default: NO */
extern Bool  VG_(clo_trace_symtab);
/* DEBUG: print thread scheduling events?  default: NO */
extern Bool  VG_(clo_trace_sched);
/* DEBUG: print pthread (mutex etc) events?  default: 0 (none), 1
   (some), 2 (all) */
extern Int   VG_(clo_trace_pthread_level);
/* Display gory details for the k'th most popular error.  default:
   Infinity. */
extern Int   VG_(clo_dump_error);
/* Number of parents of a backtrace.  Default: 8.  */
extern Int   VG_(clo_backtrace_size);
/* Engage miscellaneous weird hacks needed for some progs. */
extern Char* VG_(clo_weird_hacks);
/* How often we should poll for signals, assuming we need to poll for
   signals. */
extern Int   VG_(clo_signal_polltime);

/* Low latency syscalls and signals */
extern Bool  VG_(clo_lowlat_syscalls);
extern Bool  VG_(clo_lowlat_signals);

/* Track open file descriptors? */
extern Bool  VG_(clo_track_fds);

/* Should we run __libc_freeres at exit?  Sometimes causes crashes.
   Default: YES.  Note this is subservient to VG_(needs).libc_freeres;
   if the latter says False, then the setting of VG_(clo_weird_hacks)
   is ignored.  Ie if a tool says no, I don't want this to run, that
   cannot be overridden from the command line. */
extern Bool  VG_(clo_run_libc_freeres);
/* Use the basic-block chaining optimisation?  Default: YES */
extern Bool VG_(clo_chain_bb);
/* Generate branch-prediction hints? */
extern Bool VG_(clo_branchpred);
/* Continue stack traces below main()?  Default: NO */
extern Bool VG_(clo_show_below_main);
/* Test each client pointer dereference to check it's within the
   client address space bounds */
extern Bool VG_(clo_pointercheck);

/* Set up the libc freeres wrapper */
extern void VG_(intercept_libc_freeres_wrapper)(Addr);

/* ---------------------------------------------------------------------
   Profiling stuff
   ------------------------------------------------------------------ */

extern void VGP_(init_profiling) ( void );
extern void VGP_(done_profiling) ( void );

#undef  VGP_PUSHCC
#undef  VGP_POPCC
#define VGP_PUSHCC(x)   if (VG_(clo_profile)) VGP_(pushcc)(x)
#define VGP_POPCC(x)    if (VG_(clo_profile)) VGP_(popcc)(x)

/* ---------------------------------------------------------------------
   Tool-related types
   ------------------------------------------------------------------ */
/* These structs are not exposed to tools to mitigate possibility of
   binary-incompatibilities when the core/tool interface changes.  Instead,
   set functions are provided (see include/tool.h). */
typedef
   struct {
      Char* name;
      Char* version;
      Char* description;
      Char* copyright_author;
      Char* bug_reports_to;
      UInt  avg_translation_sizeB;
   }
   VgDetails;

extern VgDetails VG_(details);

/* If new fields are added to this type, update:
 *  - vg_main.c:initialisation of VG_(needs)
 *  - vg_main.c:sanity_check_needs()
 *
 * If the name of this type or any of its fields change, update:
 *  - dependent comments (just search for "VG_(needs)"). 
 */
typedef
   struct {
      Bool libc_freeres;
      Bool core_errors;
      Bool tool_errors;
      Bool basic_block_discards;
      Bool shadow_regs;
      Bool command_line_options;
      Bool client_requests;
      Bool extended_UCode;
      Bool syscall_wrapper;
      Bool sanity_checks;
      Bool data_syms;
      Bool shadow_memory;
   } 
   VgNeeds;

extern VgNeeds VG_(needs);

extern void VG_(tool_init_dlsym)(void *dlhandle);

#include "vg_toolint.h"

/* ---------------------------------------------------------------------
   Exports of vg_needs.c
   ------------------------------------------------------------------ */

void VG_(sanity_check_needs)(void);

/* ---------------------------------------------------------------------
   Exports of vg_malloc2.c
   ------------------------------------------------------------------ */

/* Allocation arenas.  

      CORE      for the core's general use.
      TOOL      for the tool to use (and the only one it uses).
      SYMTAB    for Valgrind's symbol table storage.
      JITTER    for small storage during translation.
      CLIENT    for the client's mallocs/frees, if the tool replaces glibc's
                    malloc() et al -- redzone size is chosen by the tool.
      DEMANGLE  for the C++ demangler.
      EXECTXT   for storing ExeContexts.
      ERRORS    for storing CoreErrors.
      TRANSIENT for very short-term use.  It should be empty in between uses.

   When adding a new arena, remember also to add it to ensure_mm_init(). 
*/
typedef Int ArenaId;

#define VG_N_ARENAS        9 

#define VG_AR_CORE         0
#define VG_AR_TOOL         1
#define VG_AR_SYMTAB       2
#define VG_AR_JITTER       3
#define VG_AR_CLIENT       4
#define VG_AR_DEMANGLE     5
#define VG_AR_EXECTXT      6
#define VG_AR_ERRORS       7
#define VG_AR_TRANSIENT    8

// This is both the minimum payload size of a malloc'd block, and its
// minimum alignment.  Must be a power of 2 greater than 4, and should be
// greater than 8.
#define VG_MIN_MALLOC_SZB        8

// Round-up size for --sloppy-malloc=yes.
#define VG_SLOPPY_MALLOC_SZB     4

extern void* VG_(arena_malloc)  ( ArenaId arena, SizeT nbytes );
extern void  VG_(arena_free)    ( ArenaId arena, void* ptr );
extern void* VG_(arena_calloc)  ( ArenaId arena, SizeT alignment,
                                  SizeT nmemb, SizeT nbytes );
extern void* VG_(arena_realloc) ( ArenaId arena, void* ptr, SizeT alignment,
                                  SizeT size );
extern void* VG_(arena_malloc_aligned) ( ArenaId aid, SizeT req_alignB, 
                                         SizeT req_pszB );

extern SizeT VG_(arena_payload_szB) ( ArenaId aid, void* payload );

extern void  VG_(sanity_check_malloc_all) ( void );

extern void  VG_(print_all_arena_stats) ( void );

extern Bool  VG_(is_empty_arena) ( ArenaId aid );

/* ---------------------------------------------------------------------
   Exports of vg_intercept.c
   ------------------------------------------------------------------ */

/* This doesn't export code or data that valgrind.so needs to link
   against.  However, the scheduler does need to know the following
   request codes.  A few, publically-visible, request codes are also
   defined in valgrind.h, and similar headers for some tools. */

#define VG_USERREQ__MALLOC                  0x2001
#define VG_USERREQ__FREE                    0x2002

/* (Fn, Arg): Create a new thread and run Fn applied to Arg in it.  Fn
   MUST NOT return -- ever.  Eventually it will do either __QUIT or
   __WAIT_JOINER.  */
#define VG_USERREQ__APPLY_IN_NEW_THREAD     0x3001

/* ( no-args ): calling thread disappears from the system forever.
   Reclaim resources. */
#define VG_USERREQ__QUIT                    0x3002

/* ( void* ): calling thread waits for joiner and returns the void* to
   it. */
#define VG_USERREQ__WAIT_JOINER             0x3003

/* ( ThreadId, void** ): wait to join a thread. */
#define VG_USERREQ__PTHREAD_JOIN            0x3004

/* Set cancellation state and type for this thread. */
#define VG_USERREQ__SET_CANCELSTATE         0x3005
#define VG_USERREQ__SET_CANCELTYPE          0x3006

/* ( no-args ): Test if we are at a cancellation point. */
#define VG_USERREQ__TESTCANCEL              0x3007

/* ( ThreadId, &thread_exit_wrapper is the only allowable arg ): call
   with this arg to indicate that a cancel is now pending for the
   specified thread. */
#define VG_USERREQ__SET_CANCELPEND          0x3008

/* Set/get detach state for this thread. */
#define VG_USERREQ__SET_OR_GET_DETACH       0x3009

#define VG_USERREQ__PTHREAD_GET_THREADID    0x300A
#define VG_USERREQ__PTHREAD_MUTEX_LOCK      0x300B
#define VG_USERREQ__PTHREAD_MUTEX_TIMEDLOCK 0x300C
#define VG_USERREQ__PTHREAD_MUTEX_TRYLOCK   0x300D
#define VG_USERREQ__PTHREAD_MUTEX_UNLOCK    0x300E
#define VG_USERREQ__PTHREAD_COND_WAIT       0x300F
#define VG_USERREQ__PTHREAD_COND_TIMEDWAIT  0x3010
#define VG_USERREQ__PTHREAD_COND_SIGNAL     0x3011
#define VG_USERREQ__PTHREAD_COND_BROADCAST  0x3012
#define VG_USERREQ__PTHREAD_KEY_CREATE      0x3013
#define VG_USERREQ__PTHREAD_KEY_DELETE      0x3014
#define VG_USERREQ__PTHREAD_SETSPECIFIC_PTR 0x3015
#define VG_USERREQ__PTHREAD_GETSPECIFIC_PTR 0x3016
#define VG_USERREQ__READ_MILLISECOND_TIMER  0x3017
#define VG_USERREQ__PTHREAD_SIGMASK         0x3018
#define VG_USERREQ__SIGWAIT                 0x3019 /* unused */
#define VG_USERREQ__PTHREAD_KILL            0x301A
#define VG_USERREQ__PTHREAD_YIELD           0x301B
#define VG_USERREQ__PTHREAD_KEY_VALIDATE    0x301C

#define VG_USERREQ__CLEANUP_PUSH            0x3020
#define VG_USERREQ__CLEANUP_POP             0x3021
#define VG_USERREQ__GET_KEY_D_AND_S         0x3022

#define VG_USERREQ__NUKE_OTHER_THREADS      0x3023

/* Ask how many signal handler returns have happened to this
   thread. */
#define VG_USERREQ__GET_N_SIGS_RETURNED     0x3024 /* unused */

/* Get/set entries for a thread's pthread_atfork stack. */
#define VG_USERREQ__SET_FHSTACK_USED        0x3025
#define VG_USERREQ__GET_FHSTACK_USED        0x3026
#define VG_USERREQ__SET_FHSTACK_ENTRY       0x3027
#define VG_USERREQ__GET_FHSTACK_ENTRY       0x3028

/* Denote the finish of __libc_freeres_wrapper(). */
#define VG_USERREQ__LIBC_FREERES_DONE       0x3029

/* Allocate RT signals */
#define VG_USERREQ__GET_SIGRT_MIN	    0x302B
#define VG_USERREQ__GET_SIGRT_MAX	    0x302C
#define VG_USERREQ__ALLOC_RTSIG		    0x302D

/* Hook for replace_malloc.o to get malloc functions */
#define VG_USERREQ__GET_MALLOCFUNCS	    0x3030

/* Get stack information for a thread. */
#define VG_USERREQ__GET_STACK_INFO          0x3033

/* Cosmetic ... */
#define VG_USERREQ__GET_PTHREAD_TRACE_LEVEL 0x3101
/* Log a pthread error from client-space.  Cosmetic. */
#define VG_USERREQ__PTHREAD_ERROR           0x3102
/* Internal equivalent of VALGRIND_PRINTF . */
#define VG_USERREQ__INTERNAL_PRINTF         0x3103
/* Internal equivalent of VALGRIND_PRINTF_BACKTRACE . */
#define VG_USERREQ__INTERNAL_PRINTF_BACKTRACE 0x3104

/* 
In core_asm.h:
#define VG_USERREQ__SIGNAL_RETURNS          0x4001
*/

#define VG_INTERCEPT_PREFIX "_vgi__"
#define VG_INTERCEPT_PREFIX_LEN 6
#define VG_INTERCEPT(name) _vgi__##name
#define VG_INTERCEPT_ALIAS(name) "_vgi__" #name

#define VG_WRAPPER_PREFIX "_vgw__"
#define VG_WRAPPER_PREFIX_LEN 6
#define VG_WRAPPER(name) _vgw__##name
#define VG_WRAPPER_ALIAS(name) "_vgw__" #name


struct vg_mallocfunc_info {
   /* things vg_replace_malloc.o needs to know about */
   Addr	sk_malloc;
   Addr	sk_calloc;
   Addr	sk_realloc;
   Addr	sk_memalign;
   Addr	sk___builtin_new;
   Addr	sk___builtin_vec_new;
   Addr	sk_free;
   Addr	sk___builtin_delete;
   Addr	sk___builtin_vec_delete;

   Addr	arena_payload_szB;

   Bool	clo_sloppy_malloc;
   Bool	clo_trace_malloc;
};

/* ---------------------------------------------------------------------
   Exports of vg_defaults.c
   ------------------------------------------------------------------ */

extern Bool VG_(sk_malloc_called_by_scheduler);

/* ---------------------------------------------------------------------
   Exports of vg_libpthread.c
   ------------------------------------------------------------------ */

/* Replacements for pthread types, shared between vg_libpthread.c and
   vg_scheduler.c.  See comment in vg_libpthread.c above the other
   vg_pthread_*_t types for a description of how these are used. */

struct _vg_pthread_fastlock
{
   long int __vg_status;   /* "Free" or "taken" or head of waiting list */
   int __vg_spinlock;      /* Used by compare_and_swap emulation. Also,
                           adaptive SMP lock stores spin count here. */
};

typedef struct
{
   int __vg_m_reserved;               /* Reserved for future use */
   int __vg_m_count;                  /* Depth of recursive locking */
   /*_pthread_descr*/ void* __vg_m_owner;       /* Owner thread (if recursive or errcheck) */
   int __vg_m_kind;                   /* Mutex kind: fast, recursive or errcheck */
   struct _vg_pthread_fastlock __vg_m_lock; /* Underlying fast lock */
}  vg_pthread_mutex_t;

typedef struct
{
  struct _vg_pthread_fastlock __vg_c_lock; /* Protect against concurrent access */
  /*_pthread_descr*/ void* __vg_c_waiting; /* Threads waiting on this condition */

  // Nb: the following padding removed because it was missing from an
  // earlier glibc, so the size test in the CONVERT macro was failing.
  // --njn

  // Padding ensures the size is 48 bytes
  /*char __vg_padding[48 - sizeof(struct _vg_pthread_fastlock)
         - sizeof(void*) - sizeof(long long)];
  long long __vg_align;*/
} vg_pthread_cond_t;


/* ---------------------------------------------------------------------
   Exports of vg_scheduler.c
   ------------------------------------------------------------------ */

typedef
   enum ThreadStatus { 
      VgTs_Empty,      /* this slot is not in use */
      VgTs_Runnable,   /* waiting to be scheduled */
      VgTs_WaitJoiner, /* waiting for someone to do join on me */
      VgTs_WaitJoinee, /* waiting for the thread I did join on */
      VgTs_WaitMX,     /* waiting on a mutex */
      VgTs_WaitCV,     /* waiting on a condition variable */
      VgTs_WaitSys,    /* waiting for a syscall to complete */
      VgTs_Sleeping,   /* sleeping for a while */
   }
   ThreadStatus;

typedef
   enum CleanupType {
      VgCt_None,       /* this cleanup entry is not initialised */
      VgCt_Function,   /* an old-style function pointer cleanup */
      VgCt_Longjmp     /* a new-style longjmp based cleanup */
   }
   CleanupType;

/* Information on a thread's stack. */
typedef
   struct {
      Addr base;
      UInt size;
      UInt guardsize;
   }
   StackInfo;

/* An entry in a threads's cleanup stack. */
typedef
   struct {
      CleanupType type;
      union {
         struct {
            void (*fn)(void*);
            void* arg;
         } function;
         struct {
            void *ub;
            int ctype;
         } longjmp;
      } data;
   }
   CleanupEntry;

/* An entry in a thread's fork-handler stack. */
typedef
   struct {
      void (*prepare)(void);
      void (*parent)(void);
      void (*child)(void);
   }
   ForkHandlerEntry;

typedef struct ProxyLWP ProxyLWP;

//typedef
   struct _ThreadState {
   /* ThreadId == 0 (and hence vg_threads[0]) is NEVER USED.
      The thread identity is simply the index in vg_threads[].
      ThreadId == 1 is the root thread and has the special property
      that we don't try and allocate or deallocate its stack.  For
      convenience of generating error message, we also put the
      ThreadId in this tid field, but be aware that it should
      ALWAYS == the index in vg_threads[]. */
   ThreadId tid;

   /* Current scheduling status. 

      Complications: whenever this is set to VgTs_WaitMX, you
      should also set .m_edx to whatever the required return value
      is for pthread_mutex_lock / pthread_cond_timedwait for when
      the mutex finally gets unblocked. */
   ThreadStatus status;

   /* When .status == WaitMX, points to the mutex I am waiting for.
      When .status == WaitCV, points to the mutex associated with
      the condition variable indicated by the .associated_cv field.
      In all other cases, should be NULL. */
   vg_pthread_mutex_t* associated_mx;

   /* When .status == WaitCV, points to the condition variable I am
      waiting for.  In all other cases, should be NULL. */
   void* /*pthread_cond_t* */ associated_cv;

   /* If VgTs_Sleeping, this is when we should wake up, measured in
      milliseconds as supplied by VG_(read_millisecond_timer). 

      If VgTs_WaitCV, this indicates the time at which
      pthread_cond_timedwait should wake up.  If == 0xFFFFFFFF,
      this means infinitely far in the future, viz,
      pthread_cond_wait. */
   UInt awaken_at;

   /* If VgTs_WaitJoiner, return value, as generated by joinees. */
   void* joinee_retval;

   /* If VgTs_WaitJoinee, place to copy the return value to, and
      the identity of the thread we're waiting for. */
   void**   joiner_thread_return;
   ThreadId joiner_jee_tid;      

   /* If VgTs_WaitSys, this is the result of the pre-syscall check */
   void *sys_pre_res;

   /* If VgTs_WaitSys, this is the syscall we're currently running */
   Int syscallno;

   /* If VgTs_WaitSys, this is the syscall flags */
   UInt sys_flags;

   /* Details about this thread's proxy LWP */
   ProxyLWP *proxy;

   /* Whether or not detached. */
   Bool detached;

   /* Cancelability state and type. */
   Bool cancel_st; /* False==PTH_CANCEL_DISABLE; True==.._ENABLE */
   Bool cancel_ty; /* False==PTH_CANC_ASYNCH; True==..._DEFERRED */
  
   /* Pointer to fn to call to do cancellation.  Indicates whether
      or not cancellation is pending.  If NULL, not pending.  Else
      should be &thread_exit_wrapper(), indicating that
      cancallation is pending. */
   void (*cancel_pend)(void*);

   /* The cleanup stack. */
   Int          custack_used;
   CleanupEntry custack[VG_N_CLEANUPSTACK];

   /* A pointer to the thread's-specific-data.  This is handled almost
      entirely from vg_libpthread.c.  We just provide hooks to get and
      set this ptr.  This is either NULL, indicating the thread has
      read/written none of its specifics so far, OR points to a
      void*[VG_N_THREAD_KEYS], allocated and deallocated in
      vg_libpthread.c. */
   void** specifics_ptr;

   /* This thread's blocked-signals mask.  Semantics is that for a
      signal to be delivered to this thread, the signal must not be
      blocked by this signal mask.  If more than one thread accepts a
      signal, then it will be delivered to one at random.  If all
      threads block the signal, it will remain pending until either a
      thread unblocks it or someone uses sigwaitsig/sigtimedwait.

      sig_mask reflects what the client told us its signal mask should
      be, but isn't necessarily the current signal mask of the proxy
      LWP: it may have more signals blocked because of signal
      handling, or it may be different because of sigsuspend.
   */
   vki_sigset_t sig_mask;

   /* Effective signal mask.  This is the mask which currently
      applies; it may be different from sig_mask while a signal
      handler is running.
    */
   vki_sigset_t eff_sig_mask;

   /* Signal queue.  This is used when the kernel doesn't route
      signals properly in order to remember the signal information
      while we are routing the signal.  It is a circular queue with
      insertions performed at the head and removals at the tail.
    */
   vki_siginfo_t sigqueue[VG_N_SIGNALQUEUE];
   Int sigqueue_head;
   Int sigqueue_tail;

   /* Stacks.  When a thread slot is freed, we don't deallocate its
      stack; we just leave it lying around for the next use of the
      slot.  If the next use of the slot requires a larger stack,
      only then is the old one deallocated and a new one
      allocated. 

      For the main thread (threadid == 0), this mechanism doesn't
      apply.  We don't know the size of the stack since we didn't
      allocate it, and furthermore we never reallocate it. */

   /* The allocated size of this thread's stack (permanently zero
      if this is ThreadId == 0, since we didn't allocate its stack) */
   UInt stack_size;

   /* Address of the lowest word in this thread's stack.  NULL means
      not allocated yet.
   */
   Addr stack_base;

   /* The allocated size of this thread's stack's guard area (permanently
      zero if this is ThreadId == 0, since we didn't allocate its stack) */
   UInt stack_guard_size;

   /* Address of the highest legitimate word in this stack.  This is
      used for error messages only -- not critical for execution
      correctness.  Is is set for all stacks, specifically including
      ThreadId == 0 (the main thread). */
   Addr stack_highest_word;

   /* Alternate signal stack */
   vki_stack_t altstack;

   /* Architecture-specific thread state */
   arch_thread_t arch;
};
//ThreadState;


/* The thread table. */
extern ThreadState VG_(threads)[VG_N_THREADS];

/* Check that tid is in range and denotes a non-Empty thread. */
extern Bool VG_(is_valid_tid) ( ThreadId tid );

/* Determine if 'tid' is that of the current running thread (Nb: returns
   False if no thread is currently running. */
extern Bool VG_(is_running_thread)(ThreadId tid);

/* Get the ThreadState for a particular thread */
extern ThreadState *VG_(get_ThreadState)(ThreadId tid);

/* Similarly ... */
extern ThreadId VG_(get_current_tid) ( void );

/* Nuke all threads except tid. */
extern void VG_(nuke_all_threads_except) ( ThreadId me );

/* Give a hint to the scheduler that it may be a good time to find a
   new runnable thread.  If prefer_sched != VG_INVALID_THREADID, then
   try to schedule that thread.
*/
extern void VG_(need_resched) ( ThreadId prefer_sched );

/* Return codes from the scheduler. */
typedef
   enum { 
      VgSrc_Deadlock,    /* no runnable threads and no prospect of any
                            even if we wait for a long time */
      VgSrc_ExitSyscall, /* client called exit().  This is the normal
                            route out. */
      VgSrc_FatalSig	 /* Killed by the default action of a fatal
			    signal */
   }
   VgSchedReturnCode;


// The scheduler.  'fatal_sigNo' is only set if VgSrc_FatalSig is returned.
extern VgSchedReturnCode VG_(scheduler) 
            ( Int* exit_code, ThreadId* last_run_thread, Int* fatal_sigNo );

extern void VG_(scheduler_init) ( void );

extern void VG_(pp_sched_status) ( void );

// Longjmp back to the scheduler and thus enter the sighandler immediately.
extern void VG_(resume_scheduler) ( Int sigNo, vki_siginfo_t *info );

// Longjmp, ending the scheduler, when a fatal signal occurs in the client.
extern void VG_(scheduler_handle_fatal_signal)( Int sigNo );

/* The red-zone size which we put at the bottom (highest address) of
   thread stacks, for paranoia reasons.  This can be arbitrary, and
   doesn't really need to be set at compile time. */
#define VG_AR_CLIENT_STACKBASE_REDZONE_SZB   16

// Write a value to a client's thread register, and shadow (if necessary).
// Note that there are some further similar macros in the arch- and
// platform-specific parts;  these ones are the totally generic ones.
#define SET_THREAD_REG( zztid, zzval, zzGETREG, zzREG, zzevent, zzargs... ) \
   do { zzGETREG(VG_(threads)[zztid].arch) = (zzval);         \
        VG_TRACK( zzevent, zztid, zzREG, ##zzargs );          \
   } while (0)

#define SET_CLREQ_RETVAL(zztid, zzval) \
   SET_THREAD_REG(zztid, zzval, ARCH_CLREQ_RET, R_CLREQ_RET, \
                  post_reg_write_clientreq_return)

#define SET_CLCALL_RETVAL(zztid, zzval, f) \
   SET_THREAD_REG(zztid, zzval, ARCH_CLREQ_RET, R_CLREQ_RET, \
                  post_reg_write_clientcall_return, f)

#define SET_PTHREQ_ESP(zztid, zzval) \
   SET_THREAD_REG(zztid, zzval, ARCH_STACK_PTR, R_STACK_PTR, \
                  post_reg_write_pthread_return)

#define SET_PTHREQ_RETVAL(zztid, zzval) \
   SET_THREAD_REG(zztid, zzval, ARCH_PTHREQ_RET, R_PTHREQ_RET, \
                  post_reg_write_pthread_return)


/* ---------------------------------------------------------------------
   Exports of vg_signals.c
   ------------------------------------------------------------------ */

extern Bool VG_(do_signal_routing); /* whether scheduler LWP has to route signals */

/* RT signal allocation */
extern Int  VG_(sig_rtmin);
extern Int  VG_(sig_rtmax);
extern Int  VG_(sig_alloc_rtsig) ( Int high );

extern void VG_(sigstartup_actions) ( void );

extern void VG_(deliver_signal) ( ThreadId tid, const vki_siginfo_t *, Bool async );
extern void VG_(unblock_host_signal) ( Int sigNo );

extern Bool VG_(is_sig_ign) ( Int sigNo );

/* Route pending signals from the scheduler LWP to the appropriate
   thread LWP. */
extern void VG_(route_signals) ( void );

/* Fake system calls for signal handling. */
extern void VG_(do_sys_sigaltstack)   ( ThreadId tid );
extern void VG_(do_sys_sigaction)     ( ThreadId tid );
extern void VG_(do_sys_sigprocmask)   ( ThreadId tid, Int how, 
                                        vki_sigset_t* set,
                                        vki_sigset_t* oldset );
extern void VG_(do_pthread_sigmask_SCSS_upd) ( ThreadId tid, Int how, 
                                               vki_sigset_t* set,
                                               vki_sigset_t* oldset );

/* Modify the current thread's state once we have detected it is
   returning from a signal handler. */
extern Bool VG_(signal_returns) ( ThreadId tid );

/* Handy utilities to block/restore all host signals. */
extern void VG_(block_all_host_signals) 
                  ( /* OUT */ vki_sigset_t* saved_mask );
extern void VG_(restore_all_host_signals) 
                  ( /* IN */ vki_sigset_t* saved_mask );

extern void VG_(kill_self)(Int sigNo);

/* These function synthesize a fault, as if the running instruction
   had had a fault.  These functions do not return - they longjmp back
   into the scheduler so the signal can be delivered. */
extern void VG_(synth_fault)        (ThreadId tid);
extern void VG_(synth_fault_mapping)(ThreadId tid, Addr addr);
extern void VG_(synth_fault_perms)  (ThreadId tid, Addr addr);

extern void VG_(get_sigstack_bounds)( Addr* low, Addr* high );

/* ---------------------------------------------------------------------
   Exports of vg_mylibc.c
   ------------------------------------------------------------------ */

#define vg_assert(expr)                                               \
  ((void) ((expr) ? 0 :						      \
	   (VG_(core_assert_fail) (VG__STRING(expr),	              \
			           __FILE__, __LINE__,                \
                                   __PRETTY_FUNCTION__), 0)))
__attribute__ ((__noreturn__))
extern void VG_(core_assert_fail) ( const Char* expr, const Char* file, 
                                    Int line, const Char* fn );
__attribute__ ((__noreturn__))
extern void  VG_(core_panic)      ( Char* str );
__attribute__ ((__noreturn__))
extern void  VG_(core_panic_at)   ( Char* str, ExeContext *ec );

/* Tools use VG_(strdup)() which doesn't expose ArenaId */
extern Char* VG_(arena_strdup) ( ArenaId aid, const Char* s);

extern Int VG_(fcntl) ( Int fd, Int cmd, Int arg );
extern Int VG_(poll)( struct vki_pollfd *, UInt nfds, Int timeout);

/* system/mman.h */
extern void* VG_(mmap)( void* start, SizeT length, UInt prot, UInt flags,
                        UInt sf_flags, UInt fd, OffT offset );
extern Int  VG_(munmap)( void* start, SizeT length );
extern Int  VG_(mprotect)( void *start, SizeT length, UInt prot );


/* Move an fd into the Valgrind-safe range */
Int VG_(safe_fd)(Int oldfd);

extern Int VG_(write_socket)( Int sd, void *msg, Int count );

/* --- Connecting over the network --- */
extern Int VG_(connect_via_socket)( UChar* str );

/* Environment manipulations */
extern Char **VG_(env_setenv)   ( Char ***envp, const Char* varname,
                                  const Char *val );
extern void   VG_(env_unsetenv) ( Char **env, const Char *varname );
extern void   VG_(env_remove_valgrind_env_stuff) ( Char** env ); 

/* ---------------------------------------------------------------------
   Exports of vg_message.c
   ------------------------------------------------------------------ */

/* Low-level -- send bytes directly to the message sink.  Do not
   use. */
extern void VG_(send_bytes_to_logging_sink) ( Char* msg, Int nbytes );

// Functions for printing from code within Valgrind, but which runs on the
// sim'd CPU.  Defined here because needed for vg_libpthread.c,
// vg_replace_malloc.c, plus the rest of the core.  The weak attribute
// ensures the multiple definitions are not a problem.  They must be functions
// rather than macros so that va_list can be used.

__attribute__((weak))
int
VALGRIND_INTERNAL_PRINTF(char *format, ...)
{
   UWord _qzz_res = 0;
   va_list vargs;
   va_start(vargs, format);
   VALGRIND_MAGIC_SEQUENCE(_qzz_res, 0, VG_USERREQ__INTERNAL_PRINTF,
                           (UWord)format, (UWord)vargs, 0, 0);
   va_end(vargs);
   return _qzz_res;
}

__attribute__((weak))
int
VALGRIND_INTERNAL_PRINTF_BACKTRACE(char *format, ...)
{
   UWord _qzz_res = 0;
   va_list vargs;
   va_start(vargs, format);
   VALGRIND_MAGIC_SEQUENCE(_qzz_res, 0, VG_USERREQ__INTERNAL_PRINTF_BACKTRACE,
                           (UWord)format, (UWord)vargs, 0, 0);
   va_end(vargs);
   return _qzz_res;
}


/* ---------------------------------------------------------------------
   Exports of vg_demangle.c
   ------------------------------------------------------------------ */

extern void VG_(demangle) ( Char* orig, Char* result, Int result_size );

/* ---------------------------------------------------------------------
   Exports of vg_from_ucode.c
   ------------------------------------------------------------------ */

extern void   VG_(print_ccall_stats)      ( void );
extern void   VG_(print_UInstr_histogram) ( void );

extern void   VG_(unchain_jumpsite)	  ( Addr jumpsite );
extern Addr   VG_(get_jmp_dest)           ( Addr jumpsite );

/* ---------------------------------------------------------------------
   Exports of vg_to_ucode.c
   ------------------------------------------------------------------ */

Bool VG_(cpu_has_feature)(UInt feat);


/* ---------------------------------------------------------------------
   Exports of vg_translate.c
   ------------------------------------------------------------------ */

extern Bool VG_(translate)  ( ThreadId tid, Addr orig_addr, Bool debugging );


/* ---------------------------------------------------------------------
   Exports of vg_execontext.c.
   ------------------------------------------------------------------ */

/* Records the PC and a bit of the call chain.  The first 4 IP
   values are used in comparisons do remove duplicate errors, and for
   comparing against suppression specifications.  The rest are purely
   informational (but often important). */

struct _ExeContext {
   struct _ExeContext * next;
   /* Variable-length array.  The size is VG_(clo_backtrace_size); at
      least 1, at most VG_DEEPEST_BACKTRACE.  [0] is the current IP,
      [1] is its caller, [2] is the caller of [1], etc. */
   Addr ips[0];
};


/* Print stats (informational only). */
extern void VG_(print_ExeContext_stats) ( void );

/* Like VG_(get_ExeContext), but with a slightly different type */
extern ExeContext* VG_(get_ExeContext2) ( Addr ip, Addr fp,
                                          Addr fp_min, Addr fp_max );


/* ---------------------------------------------------------------------
   Exports of vg_errcontext.c.
   ------------------------------------------------------------------ */

extern void VG_(load_suppressions)    ( void );

extern void VG_(record_pthread_error) ( ThreadId tid, Char* msg );

extern void VG_(show_all_errors)      ( void );

extern Bool VG_(is_action_requested)  ( Char* action, Bool* clo );

extern UInt VG_(get_n_errs_found)     ( void );

/* ---------------------------------------------------------------------
   Exports of vg_procselfmaps.c
   ------------------------------------------------------------------ */

/* Reads /proc/self/maps into a static buffer which can be parsed by
   VG_(parse_procselfmaps)(). */
extern void VG_(read_procselfmaps) ( void );

/* Parses /proc/self/maps, calling `record_mapping' for each entry.  If
   `read_from_file' is True, /proc/self/maps is read directly, otherwise
   it's read from the buffer filled by VG_(read_procselfmaps_contents)(). */
extern 
void VG_(parse_procselfmaps) (
   void (*record_mapping)( Addr addr, SizeT len, Char rr, Char ww, Char xx, 
			   UInt dev, UInt ino, ULong foff,
                           const UChar *filename ) );


/* ---------------------------------------------------------------------
   Exports of vg_symtab2.c
   ------------------------------------------------------------------ */

typedef struct _Segment Segment;

extern Bool VG_(is_object_file)   ( const void *hdr );
extern void VG_(mini_stack_dump)  ( Addr ips[], UInt n_ips );
extern SegInfo * VG_(read_seg_symbols) ( Segment *seg );
extern void VG_(symtab_incref)	  ( SegInfo * );
extern void VG_(symtab_decref)	  ( SegInfo *, Addr a );

extern Bool VG_(get_fnname_nodemangle)( Addr a, Char* fnname, Int n_fnname );

/* Set up some default redirects */
extern void VG_(setup_code_redirect_table) ( void );

/* Redirection machinery */
extern Addr VG_(code_redirect) ( Addr orig );

/* ---------------------------------------------------------------------
   Exports of vg_main.c
   ------------------------------------------------------------------ */

/* Is this a SSE/SSE2-capable CPU?  If so, we had better save/restore
   the SSE state all over the place.  This is set up very early, in
   main().  We have to determine it early since we can't even
   correctly snapshot the startup machine state without it. */
extern Bool VG_(have_ssestate);

/* Tell the logging mechanism whether we are logging to a file
   descriptor or a socket descriptor. */
extern Bool VG_(logging_to_filedes);

/* Sanity checks which may be done at any time.  The scheduler decides when. */
extern void VG_(sanity_check_general) ( Bool force_expensive );

/* Address space */
extern Addr VG_(client_base);	/* client address space limits */
extern Addr VG_(client_end);
extern Addr VG_(client_mapbase); /* base of mappings */
extern Addr VG_(clstk_base);	/* client stack range */
extern Addr VG_(clstk_end);
extern Addr VG_(client_trampoline_code);

extern Addr VG_(brk_base);	/* start of brk */
extern Addr VG_(brk_limit);	/* current brk */
extern Addr VG_(shadow_base);	/* tool's shadow memory */
extern Addr VG_(shadow_end);
extern Addr VG_(valgrind_base);	/* valgrind's address range */
extern Addr VG_(valgrind_last); // Nb: last byte, rather than one past the end

extern struct vki_rlimit VG_(client_rlimit_data); /* client's original rlimit data */
extern struct vki_rlimit VG_(client_rlimit_stack); /* client's original rlimit stack */

/* client executable file descriptor */
extern Int  VG_(clexecfd);

// Help set up the child used when doing execve() with --trace-children=yes
Char* VG_(build_child_VALGRINDCLO) ( Char* exename );
Char* VG_(build_child_exename)     ( void );

/* Determine if SP adjustment must be noted */
extern Bool VG_(need_to_handle_SP_assignment) ( void );

/* Called when some unhandleable client behaviour is detected.
   Prints a msg and aborts. */
extern void VG_(unimplemented) ( Char* msg )
            __attribute__((__noreturn__));

/* Something of a function looking for a home ... start up debugger. */
extern void VG_(start_debugger) ( Int tid );

/* Counts downwards in vg_run_innerloop. */
extern UInt VG_(dispatch_ctr);

/* --- Counters, for informational purposes only. --- */

// These counters must be declared here because they're maintained by
// vg_dispatch.S.
extern UInt VG_(bb_enchain_count);     // Counts of chain operations done
extern UInt VG_(bb_dechain_count);     // Counts of unchain operations done
extern UInt VG_(unchained_jumps_done); // Number of unchained jumps performed

extern void VG_(print_scheduler_stats) ( void );

extern Int  VG_(alloc_BaB)( Int );      // Allocate slots in baseBlock
extern void VG_(align_BaB)( UInt );     // Align baseBlock offset
extern Int  VG_(alloc_BaB_1_set)( Addr ); // Allocate & init baseBlock slot

/* ---------------------------------------------------------------------
   Exports of vg_memory.c
   ------------------------------------------------------------------ */

/* A Segment is mapped piece of client memory.  This covers all kinds
   of mapped memory (exe, brk, mmap, .so, shm, stack, etc)

   We try to encode everything we know about a particular segment here.
*/
#define SF_FIXED    (1 <<  0) // client asked for MAP_FIXED
#define SF_SHARED   (1 <<  1) // shared
#define SF_SHM      (1 <<  2) // SYSV SHM (also SF_SHARED)
#define SF_MMAP     (1 <<  3) // mmap memory
#define SF_FILE     (1 <<  4) // mapping is backed by a file
#define SF_STACK    (1 <<  5) // is a stack
#define SF_GROWDOWN (1 <<  6) // segment grows down
#define SF_GROWUP   (1 <<  7) // segment grows up
#define SF_EXEC     (1 <<  8) // segment created by exec
#define SF_DYNLIB   (1 <<  9) // mapped from dynamic library
#define SF_NOSYMS   (1 << 10) // don't load syms, even if present
#define SF_BRK      (1 << 11) // brk segment
#define SF_CORE     (1 << 12) // allocated by core on behalf of the client
#define SF_VALGRIND (1 << 13) // a valgrind-internal mapping - not in client
#define SF_CODE     (1 << 14) // segment contains cached code

struct _Segment {
   UInt		prot;		/* VKI_PROT_*				*/
   UInt		flags;		/* SF_*					*/

   Addr		addr;		/* mapped addr (page aligned)		*/
   SizeT	len;		/* size of mapping (page aligned)	*/

   /* These are valid if (flags & SF_FILE) */
   ULong	offset;		/* file offset				*/
   const Char	*filename;	/* filename (NULL if unknown)		*/
   UInt		dev;		/* device				*/
   UInt		ino;		/* inode				*/

   SegInfo	*symtab;	/* symbol table				*/
};

/* segment mapped from a file descriptor */
extern void VG_(map_fd_segment)  (Addr addr, SizeT len, UInt prot, UInt flags, 
				  Int fd, ULong off, const Char *filename);

/* segment mapped from a file */
extern void VG_(map_file_segment)(Addr addr, SizeT len, UInt prot, UInt flags, 
				  UInt dev, UInt ino, ULong off, const Char *filename);

/* simple segment */
extern void VG_(map_segment)     (Addr addr, SizeT len, UInt prot, UInt flags);

extern void VG_(unmap_range)   (Addr addr, SizeT len);
extern void VG_(mprotect_range)(Addr addr, SizeT len, UInt prot);
extern Addr VG_(find_map_space)(Addr base, SizeT len, Bool for_client);

extern Segment *VG_(find_segment)(Addr a);
extern Segment *VG_(first_segment)(void);
extern Segment *VG_(next_segment)(Segment *);

extern Bool     VG_(seg_contains)(const Segment *s, Addr ptr, SizeT size);
extern Bool     VG_(seg_overlaps)(const Segment *s, Addr ptr, SizeT size);

extern void VG_(pad_address_space)  (void);
extern void VG_(unpad_address_space)(void);

extern REGPARM(1)
       void VG_(unknown_SP_update) ( Addr new_SP );

/* ---------------------------------------------------------------------
   Exports of vg_proxylwp.c
   ------------------------------------------------------------------ */

enum PXState
{
   PXS_BAD = -1,
   PXS_WaitReq,		/* waiting for a request */
   PXS_RunSyscall,	/* running a syscall */
   PXS_IntReply,	/* request interrupted - need to send reply */
   PXS_SysDone,		/* small window between syscall
			   complete and results written out */
   PXS_SigACK,		/* waiting for a signal ACK */
};


/* Issue a syscall for thread tid */
extern Int  VG_(sys_issue)(int tid);

extern void VG_(proxy_init)     ( void );
extern void VG_(proxy_create)   ( ThreadId tid );
extern void VG_(proxy_delete)   ( ThreadId tid, Bool force );
extern void VG_(proxy_results)  ( void );
extern void VG_(proxy_sendsig)  ( ThreadId tid, Int signo );
extern void VG_(proxy_setsigmask)(ThreadId tid);
extern void VG_(proxy_sigack)   ( ThreadId tid, const vki_sigset_t *);
extern void VG_(proxy_abort_syscall) ( ThreadId tid );
extern void VG_(proxy_waitsig)  ( void );
extern void VG_(proxy_wait_sys)	(ThreadId tid, Bool restart);

extern void VG_(proxy_shutdown) ( void ); // shut down the syscall workers
extern Int  VG_(proxy_resfd)    ( void ); // FD something can select on to know 
                                          //  a syscall finished

/* Sanity-check the whole proxy-LWP machinery */
void VG_(sanity_check_proxy)(void);

/* Send a signal from a thread's proxy to the thread.  This longjmps
   back into the proxy's main loop, so it doesn't return. */
__attribute__ ((__noreturn__))
extern void VG_(proxy_handlesig)( const vki_siginfo_t *siginfo, 
				  Addr ip, Int sysnum );

/* ---------------------------------------------------------------------
   Exports of vg_syscalls.c
   ------------------------------------------------------------------ */

extern Char *VG_(resolve_filename)(Int fd);

extern Bool VG_(pre_syscall) ( ThreadId tid );
extern void VG_(post_syscall)( ThreadId tid, Bool restart );

extern Bool VG_(is_kerror) ( Word res );

/* Internal atfork handlers */
typedef void (*vg_atfork_t)(ThreadId);
extern void VG_(atfork)(vg_atfork_t pre, vg_atfork_t parent, vg_atfork_t child);

/* fd leakage calls. */
extern void VG_(init_preopened_fds) ( void );
extern void VG_(show_open_fds) ( void );

// Return true if address range entirely contained within client
// address space.
Bool VG_(valid_client_addr)(Addr start, SizeT size, ThreadId tid,
                            const Char *syscallname);

// Return true if we're allowed to use or create this fd.
Bool VG_(fd_allowed)(Int fd, const Char *syscallname, ThreadId tid, Bool soft);

void VG_(record_fd_open)(Int tid, Int fd, char *pathname);
   
// Flags describing syscall wrappers
#define Special    (1 << 0)
#define MayBlock   (1 << 1)
#define NBRunInLWP (1 << 2)   // non-blocking, but must run in LWP context
#define PostOnFail (1 << 3)

// For each generic ("gen") wrapper, we declare the pre-wrapper, the
// post-wrapper (which is actually not always needed), and the associated
// flags.
#define GEN_SYSCALL_WRAPPER(x) \
   extern UInt VGA_(gen_##x##_flags); \
   extern void VGA_(gen_##x##_before)(ThreadId tid, ThreadState *tst); \
   extern void VGA_(gen_##x##_after) (ThreadId tid, ThreadState *tst)

// Generic (platform-independent) syscall wrappers.  These are generally
// POSIX or something like that;  those that are not POSIX are annotated
// with what standards they are part of, as stated in the Linux man pages.
// For many of them, it's unclear if they are generic, or Linux-specific, or
// x86/Linux-specific, or something else again.
//
// Nb: This list may change over time... ones thought at first to be generic
// may turn out not to be, and so be moved into OS-specific or
// platform-specific files.  If there's any doubt, I'm leaving them in here.
//
// Nb 2: if porting to a new OS, you should really check all these generic
// wrappers to make sure they match your OS, painful as it might be.

GEN_SYSCALL_WRAPPER(sys_ni_syscall);            // * P -- unimplemented
GEN_SYSCALL_WRAPPER(sys_exit);
GEN_SYSCALL_WRAPPER(sys_fork);
GEN_SYSCALL_WRAPPER(sys_read);
GEN_SYSCALL_WRAPPER(sys_write);
GEN_SYSCALL_WRAPPER(sys_open);
GEN_SYSCALL_WRAPPER(sys_close);
GEN_SYSCALL_WRAPPER(sys_waitpid);
GEN_SYSCALL_WRAPPER(sys_creat);
GEN_SYSCALL_WRAPPER(sys_link);
GEN_SYSCALL_WRAPPER(sys_unlink);
GEN_SYSCALL_WRAPPER(sys_execve);    // (*??) P
GEN_SYSCALL_WRAPPER(sys_chdir);
GEN_SYSCALL_WRAPPER(sys_time);
GEN_SYSCALL_WRAPPER(sys_mknod);
GEN_SYSCALL_WRAPPER(sys_chmod);
GEN_SYSCALL_WRAPPER(sys_lseek);
GEN_SYSCALL_WRAPPER(sys_getpid);
GEN_SYSCALL_WRAPPER(sys_alarm);
GEN_SYSCALL_WRAPPER(sys_pause);
GEN_SYSCALL_WRAPPER(sys_utime);
GEN_SYSCALL_WRAPPER(sys_access);
GEN_SYSCALL_WRAPPER(sys_kill);
GEN_SYSCALL_WRAPPER(sys_rename);
GEN_SYSCALL_WRAPPER(sys_mkdir);
GEN_SYSCALL_WRAPPER(sys_rmdir);
GEN_SYSCALL_WRAPPER(sys_dup);
GEN_SYSCALL_WRAPPER(sys_times);
GEN_SYSCALL_WRAPPER(sys_fcntl);        // POSIX (but complicated)
GEN_SYSCALL_WRAPPER(sys_setpgid);
GEN_SYSCALL_WRAPPER(sys_umask);
GEN_SYSCALL_WRAPPER(sys_dup2);
GEN_SYSCALL_WRAPPER(sys_getppid);
GEN_SYSCALL_WRAPPER(sys_getpgrp);
GEN_SYSCALL_WRAPPER(sys_setsid);
GEN_SYSCALL_WRAPPER(sys_munmap);
GEN_SYSCALL_WRAPPER(sys_truncate);
GEN_SYSCALL_WRAPPER(sys_ftruncate);
GEN_SYSCALL_WRAPPER(sys_fchmod);
GEN_SYSCALL_WRAPPER(sys_msync);
GEN_SYSCALL_WRAPPER(sys_readv);
GEN_SYSCALL_WRAPPER(sys_writev);
GEN_SYSCALL_WRAPPER(sys_getsid);
GEN_SYSCALL_WRAPPER(sys_fdatasync);
GEN_SYSCALL_WRAPPER(sys_mlock);
GEN_SYSCALL_WRAPPER(sys_munlock);
GEN_SYSCALL_WRAPPER(sys_mlockall);
GEN_SYSCALL_WRAPPER(sys_munlockall);
GEN_SYSCALL_WRAPPER(sys_sched_setparam);
GEN_SYSCALL_WRAPPER(sys_sched_getparam);
GEN_SYSCALL_WRAPPER(sys_sched_setscheduler);
GEN_SYSCALL_WRAPPER(sys_sched_getscheduler);
GEN_SYSCALL_WRAPPER(sys_sched_yield);
GEN_SYSCALL_WRAPPER(sys_sched_get_priority_max);
GEN_SYSCALL_WRAPPER(sys_sched_get_priority_min);
GEN_SYSCALL_WRAPPER(sys_nanosleep);
GEN_SYSCALL_WRAPPER(sys_mremap);    // POSIX, but Linux arg order may be odd
GEN_SYSCALL_WRAPPER(sys_getuid);
GEN_SYSCALL_WRAPPER(sys_getgid);
GEN_SYSCALL_WRAPPER(sys_geteuid);
GEN_SYSCALL_WRAPPER(sys_getegid);
GEN_SYSCALL_WRAPPER(sys_getpgid);
GEN_SYSCALL_WRAPPER(sys_fsync);
GEN_SYSCALL_WRAPPER(sys_wait4);
GEN_SYSCALL_WRAPPER(sys_mprotect);
GEN_SYSCALL_WRAPPER(sys_sigprocmask);
GEN_SYSCALL_WRAPPER(sys_timer_create);    // Linux: varies across archs?
GEN_SYSCALL_WRAPPER(sys_timer_settime);
GEN_SYSCALL_WRAPPER(sys_timer_gettime);
GEN_SYSCALL_WRAPPER(sys_timer_getoverrun);
GEN_SYSCALL_WRAPPER(sys_timer_delete);
GEN_SYSCALL_WRAPPER(sys_clock_settime);
GEN_SYSCALL_WRAPPER(sys_clock_gettime);
GEN_SYSCALL_WRAPPER(sys_clock_getres);
GEN_SYSCALL_WRAPPER(sys_getcwd);
GEN_SYSCALL_WRAPPER(sys_symlink);
GEN_SYSCALL_WRAPPER(sys_getgroups);
GEN_SYSCALL_WRAPPER(sys_setgroups);             // SVr4, SVID, X/OPEN, 4.3BSD
GEN_SYSCALL_WRAPPER(sys_chown);
GEN_SYSCALL_WRAPPER(sys_setuid);
GEN_SYSCALL_WRAPPER(sys_gettimeofday);
GEN_SYSCALL_WRAPPER(sys_madvise);
GEN_SYSCALL_WRAPPER(sys_sigpending);

// These ones aren't POSIX, but are in some standard and look reasonably
// generic, and are the same for all architectures under Linux.
GEN_SYSCALL_WRAPPER(sys_nice);      // SVr4, SVID EXT, AT&T, X/OPEN, BSD 4.3
GEN_SYSCALL_WRAPPER(sys_sync);      // SVr4, SVID, X/OPEN, BSD 4.3
GEN_SYSCALL_WRAPPER(sys_brk);       // 4.3BSD
GEN_SYSCALL_WRAPPER(sys_acct);      // SVR4, non-POSIX
GEN_SYSCALL_WRAPPER(sys_chroot);    // SVr4, SVID, 4.4BSD, X/OPEN
GEN_SYSCALL_WRAPPER(sys_readlink);  // X/OPEN, 4.4BSD
GEN_SYSCALL_WRAPPER(sys_fchdir);    // SVr4, SVID, POSIX, X/OPEN, 4.4BSD
GEN_SYSCALL_WRAPPER(sys_getdents);  // SVr4,SVID
GEN_SYSCALL_WRAPPER(sys_select);    // 4.4BSD
GEN_SYSCALL_WRAPPER(sys_flock);     // 4.4BSD
GEN_SYSCALL_WRAPPER(sys_poll);      // XPG4-UNIX
GEN_SYSCALL_WRAPPER(sys_getrusage); // SVr4, 4.3BSD
GEN_SYSCALL_WRAPPER(sys_settimeofday); // SVr4, 4.3BSD (non-POSIX)
GEN_SYSCALL_WRAPPER(sys_getpriority);  // SVr4, 4.4BSD
GEN_SYSCALL_WRAPPER(sys_setpriority);  // SVr4, 4.4BSD
GEN_SYSCALL_WRAPPER(sys_setitimer);    // SVr4, 4.4BSD
GEN_SYSCALL_WRAPPER(sys_getitimer);    // SVr4, 4.4BSD
GEN_SYSCALL_WRAPPER(sys_setreuid);     // 4.3BSD
GEN_SYSCALL_WRAPPER(sys_setregid);     // 4.3BSD
GEN_SYSCALL_WRAPPER(sys_fchown);       // SVr4,4.3BSD
GEN_SYSCALL_WRAPPER(sys_setgid);       // SVr4,SVID
GEN_SYSCALL_WRAPPER(sys_utimes);       // 4.3BSD

// These ones may be Linux specific... not sure.  They use 16-bit gid_t and
// uid_t types.  The similarly named (minus the "16" suffix) ones below use
// 32-bit versions of these types.
GEN_SYSCALL_WRAPPER(sys_setuid16);              // ## P
GEN_SYSCALL_WRAPPER(sys_getuid16);              // ## P
GEN_SYSCALL_WRAPPER(sys_setgid16);              // ## SVr4,SVID
GEN_SYSCALL_WRAPPER(sys_getgid16);              // ## P
GEN_SYSCALL_WRAPPER(sys_geteuid16);             // ## P
GEN_SYSCALL_WRAPPER(sys_getegid16);             // ## P
GEN_SYSCALL_WRAPPER(sys_setreuid16);            // ## BSD4.3
GEN_SYSCALL_WRAPPER(sys_setregid16);            // ## BSD4.3
GEN_SYSCALL_WRAPPER(sys_getgroups16);           // ## P
GEN_SYSCALL_WRAPPER(sys_setgroups16);           // ## SVr4, SVID, X/OPEN, 4.3BSD
GEN_SYSCALL_WRAPPER(sys_fchown16);              // ## SVr4,BSD4.3
GEN_SYSCALL_WRAPPER(sys_chown16);               // ## P

// Linux's funny many-in-one socketcall is certainly not generic, but I
// didn't want to move it until necessary because it's big and has a lot of
// associated junk.
GEN_SYSCALL_WRAPPER(sys_socketcall);

// Some archs on Linux do not match the generic wrapper for sys_pipe().
GEN_SYSCALL_WRAPPER(sys_pipe);

// May not be generic for every architecture under Linux.
GEN_SYSCALL_WRAPPER(sys_sigaction);             // (x86) P

// Funny names, not sure...
GEN_SYSCALL_WRAPPER(sys_newstat);               // * P
GEN_SYSCALL_WRAPPER(sys_newlstat);              // *
GEN_SYSCALL_WRAPPER(sys_newfstat);              // * P (SVr4,BSD4.3)

// For the remainder, not really sure yet
GEN_SYSCALL_WRAPPER(old_mmap);                  // x86, weird arg passing
GEN_SYSCALL_WRAPPER(sys_ptrace);                // (x86?) (almost-P)
GEN_SYSCALL_WRAPPER(sys_sigsuspend);            // POSIX, but L (proto varies across archs)
GEN_SYSCALL_WRAPPER(sys_setrlimit);             // SVr4, 4.3BSD
GEN_SYSCALL_WRAPPER(sys_ioctl);                 // x86? (various)
GEN_SYSCALL_WRAPPER(sys_old_getrlimit);         // SVr4, 4.3BSD L?
GEN_SYSCALL_WRAPPER(sys_statfs);                // * L?
GEN_SYSCALL_WRAPPER(sys_fstatfs);               // * L?
GEN_SYSCALL_WRAPPER(sys_iopl);                  // (x86/amd64) L
GEN_SYSCALL_WRAPPER(sys_ipc);                   // (x86) L
GEN_SYSCALL_WRAPPER(sys_newuname);              // * P
GEN_SYSCALL_WRAPPER(sys_init_module);           // * L?
GEN_SYSCALL_WRAPPER(sys_quotactl);              // * (?)
GEN_SYSCALL_WRAPPER(sys_rt_sigaction);          // (x86) ()
GEN_SYSCALL_WRAPPER(sys_rt_sigprocmask);        // * ?
GEN_SYSCALL_WRAPPER(sys_rt_sigpending);         // * ?
GEN_SYSCALL_WRAPPER(sys_rt_sigtimedwait);       // * ?
GEN_SYSCALL_WRAPPER(sys_rt_sigqueueinfo);       // * ?
GEN_SYSCALL_WRAPPER(sys_rt_sigsuspend);         // () ()
GEN_SYSCALL_WRAPPER(sys_pread64);               // * (Unix98?)
GEN_SYSCALL_WRAPPER(sys_pwrite64);              // * (Unix98?)
GEN_SYSCALL_WRAPPER(sys_capget);                // * L?
GEN_SYSCALL_WRAPPER(sys_capset);                // * L?
GEN_SYSCALL_WRAPPER(sys_sigaltstack);           // (x86) (XPG4-UNIX)
GEN_SYSCALL_WRAPPER(sys_getpmsg);               // (?) (?)
GEN_SYSCALL_WRAPPER(sys_putpmsg);               // (?) (?)
GEN_SYSCALL_WRAPPER(sys_getrlimit);             // * (?)
GEN_SYSCALL_WRAPPER(sys_mmap2);                 // (x86?) P?
GEN_SYSCALL_WRAPPER(sys_truncate64);            // %% (P?)
GEN_SYSCALL_WRAPPER(sys_ftruncate64);           // %% (P?)
GEN_SYSCALL_WRAPPER(sys_stat64);                // %% (?)
GEN_SYSCALL_WRAPPER(sys_lstat64);               // %% (?)
GEN_SYSCALL_WRAPPER(sys_fstat64);               // %% (?)
GEN_SYSCALL_WRAPPER(sys_lchown);                // * (L?)
GEN_SYSCALL_WRAPPER(sys_mincore);               // * L?
GEN_SYSCALL_WRAPPER(sys_getdents64);            // * (SVr4,SVID?)
GEN_SYSCALL_WRAPPER(sys_fcntl64);               // * P?
GEN_SYSCALL_WRAPPER(sys_setxattr);              // * L?
GEN_SYSCALL_WRAPPER(sys_lsetxattr);             // * L?
GEN_SYSCALL_WRAPPER(sys_fsetxattr);             // * L?
GEN_SYSCALL_WRAPPER(sys_getxattr);              // * L?
GEN_SYSCALL_WRAPPER(sys_lgetxattr);             // * L?
GEN_SYSCALL_WRAPPER(sys_fgetxattr);             // * L?
GEN_SYSCALL_WRAPPER(sys_listxattr);             // * L?
GEN_SYSCALL_WRAPPER(sys_llistxattr);            // * L?
GEN_SYSCALL_WRAPPER(sys_flistxattr);            // * L?
GEN_SYSCALL_WRAPPER(sys_removexattr);           // * L?
GEN_SYSCALL_WRAPPER(sys_lremovexattr);          // * L?
GEN_SYSCALL_WRAPPER(sys_fremovexattr);          // * L?
GEN_SYSCALL_WRAPPER(sys_sched_setaffinity);     // * L?
GEN_SYSCALL_WRAPPER(sys_sched_getaffinity);     // * L?
GEN_SYSCALL_WRAPPER(sys_exit_group);            // * ?
GEN_SYSCALL_WRAPPER(sys_lookup_dcookie);        // (*/32/64) L
GEN_SYSCALL_WRAPPER(sys_set_tid_address);       // * ?
GEN_SYSCALL_WRAPPER(sys_statfs64);              // * (?)
GEN_SYSCALL_WRAPPER(sys_fstatfs64);             // * (?)
GEN_SYSCALL_WRAPPER(sys_mq_open);               // * P?
GEN_SYSCALL_WRAPPER(sys_mq_unlink);             // * P?
GEN_SYSCALL_WRAPPER(sys_mq_timedsend);          // * P?
GEN_SYSCALL_WRAPPER(sys_mq_timedreceive);       // * P?
GEN_SYSCALL_WRAPPER(sys_mq_notify);             // * P?
GEN_SYSCALL_WRAPPER(sys_mq_getsetattr);         // * P?

#undef GEN_SYSCALL_WRAPPER

// Macros used in syscall wrappers
/* PRRAn == "pre-register-read-argument"
   PRRSN == "pre-register-read-syscall"
*/

#define PRRSN \
      TL_(pre_reg_read)(Vg_CoreSysCall, tid, "(syscallno)", \
                        R_SYSCALL_NUM, sizeof(UWord));
#define PRRAn(n,s,t,a) \
      TL_(pre_reg_read)(Vg_CoreSysCall, tid, s"("#a")", \
                        R_SYSCALL_ARG##n, sizeof(t));
#define PRE_REG_READ0(tr, s) \
   if (VG_(defined_pre_reg_read)()) { \
      PRRSN; \
   }
#define PRE_REG_READ1(tr, s, t1, a1) \
   if (VG_(defined_pre_reg_read)()) { \
      PRRSN; \
      PRRAn(1,s,t1,a1); \
   }
#define PRE_REG_READ2(tr, s, t1, a1, t2, a2) \
   if (VG_(defined_pre_reg_read)()) { \
      PRRSN; \
      PRRAn(1,s,t1,a1); PRRAn(2,s,t2,a2); \
   }
#define PRE_REG_READ3(tr, s, t1, a1, t2, a2, t3, a3) \
   if (VG_(defined_pre_reg_read)()) { \
      PRRSN; \
      PRRAn(1,s,t1,a1); PRRAn(2,s,t2,a2); PRRAn(3,s,t3,a3); \
   }
#define PRE_REG_READ4(tr, s, t1, a1, t2, a2, t3, a3, t4, a4) \
   if (VG_(defined_pre_reg_read)()) { \
      PRRSN; \
      PRRAn(1,s,t1,a1); PRRAn(2,s,t2,a2); PRRAn(3,s,t3,a3); \
      PRRAn(4,s,t4,a4); \
   }
#define PRE_REG_READ5(tr, s, t1, a1, t2, a2, t3, a3, t4, a4, t5, a5) \
   if (VG_(defined_pre_reg_read)()) { \
      PRRSN; \
      PRRAn(1,s,t1,a1); PRRAn(2,s,t2,a2); PRRAn(3,s,t3,a3); \
      PRRAn(4,s,t4,a4); PRRAn(5,s,t5,a5); \
   }
#define PRE_REG_READ6(tr, s, t1, a1, t2, a2, t3, a3, t4, a4, t5, a5, t6, a6) \
   if (VG_(defined_pre_reg_read)()) { \
      PRRSN; \
      PRRAn(1,s,t1,a1); PRRAn(2,s,t2,a2); PRRAn(3,s,t3,a3); \
      PRRAn(4,s,t4,a4); PRRAn(5,s,t5,a5); PRRAn(6,s,t6,a6); \
   }

#define PRE_MEM_READ(zzname, zzaddr, zzlen) \
   VG_TRACK( pre_mem_read, Vg_CoreSysCall, tid, zzname, zzaddr, zzlen)

#define PRE_MEM_RASCIIZ(zzname, zzaddr) \
   VG_TRACK( pre_mem_read_asciiz, Vg_CoreSysCall, tid, zzname, zzaddr)

#define PRE_MEM_WRITE(zzname, zzaddr, zzlen) \
   VG_TRACK( pre_mem_write, Vg_CoreSysCall, tid, zzname, zzaddr, zzlen)

#define POST_MEM_WRITE(zzaddr, zzlen) \
   VG_TRACK( post_mem_write, zzaddr, zzlen)

/* ---------------------------------------------------------------------
   Exports of vg_transtab.c
   ------------------------------------------------------------------ */

/* The fast-cache for tt-lookup. */
extern Addr VG_(tt_fast)[VG_TT_FAST_SIZE];

extern void VG_(init_tt_tc)       ( void );
extern void VG_(add_to_trans_tab) ( Addr orig_addr,  Int orig_size,
                                    Addr trans_addr, Int trans_size );
extern Addr VG_(search_transtab)  ( Addr original_addr );

extern void VG_(invalidate_translations) ( Addr start, UInt range,
                                           Bool unchain_blocks );

extern void VG_(sanity_check_tt_tc) ( void );

extern void VG_(print_tt_tc_stats) ( void );

extern Int  VG_(get_bbs_translated) ( void );

/* ---------------------------------------------------------------------
   Exports of vg_syscall.S
   ------------------------------------------------------------------ */

extern Int VG_(do_syscall) ( UInt, ... );
extern Int VG_(clone) ( Int (*fn)(void *), void *stack, Int flags, void *arg, 
			Int *child_tid, Int *parent_tid);
extern void VG_(sigreturn)(void);

/* ---------------------------------------------------------------------
   Exports of vg_dispatch.S
   ------------------------------------------------------------------ */

/* Run a thread for a (very short) while, until some event happens
   which means we need to defer to the scheduler. */
extern UInt VG_(run_innerloop) ( void );

/* The patching routing called when a BB wants to chain itself to
   another. */
extern UInt VG_(patch_me);

/* ---------------------------------------------------------------------
   Exports of vg_helpers.S
   ------------------------------------------------------------------ */

extern void VG_(helper_undefined_instruction);

/* Information about trampoline code (for signal return and syscalls) */
extern const Char VG_(trampoline_code_start);
extern const Int  VG_(trampoline_code_length);
extern const Int  VG_(tramp_sigreturn_offset);
extern const Int  VG_(tramp_syscall_offset);

/* ---------------------------------------------------------------------
   Things relating to the used tool
   ------------------------------------------------------------------ */

#define VG_TRACK(fn, args...) 			\
   do {						\
      if (VG_(defined_##fn)())			\
	 TL_(fn)(args);				\
   } while(0)

__attribute__ ((noreturn))
extern void VG_(missing_tool_func) ( const Char* fn );

/* ---------------------------------------------------------------------
   The baseBlock -- arch-neutral bits
   ------------------------------------------------------------------ */

#define INVALID_OFFSET (-1)

/* An array of words.  In generated code, %ebp always points to the
   start of this array.  Useful stuff, like the simulated CPU state,
   and the addresses of helper functions, can then be found by
   indexing off %ebp.  The following declares variables which, at
   startup time, are given values denoting offsets into baseBlock.
   These offsets are in *words* from the start of baseBlock. */

#define VG_BASEBLOCK_WORDS 400

extern UInt VG_(baseBlock)[VG_BASEBLOCK_WORDS];

// ---------------------------------------------------------------------
// Architecture-specific things defined in eg. x86/*.c
// ---------------------------------------------------------------------

// For setting up the baseBlock
extern void VGA_(init_low_baseBlock)  ( Addr client_ip, Addr sp_at_startup );
extern void VGA_(init_high_baseBlock) ( Addr client_ip, Addr sp_at_startup );

// Register state moving
extern void VGA_(load_state) ( arch_thread_t*, ThreadId tid );
extern void VGA_(save_state) ( arch_thread_t*, ThreadId tid );

// Register state access
extern void VGA_(set_thread_shadow_archreg) ( ThreadId tid, UInt archreg, UInt val );
extern UInt VGA_(get_thread_shadow_archreg) ( ThreadId tid, UInt archreg );
extern UInt VGA_(get_shadow_archreg) ( UInt archreg );

// Thread stuff
extern void VGA_(clear_thread)   ( arch_thread_t* );
extern void VGA_(init_thread)    ( arch_thread_t* );
extern void VGA_(cleanup_thread) ( arch_thread_t* );
extern void VGA_(setup_child)    ( arch_thread_t*, arch_thread_t* );

extern void VGA_(set_arg_and_bogus_ret) ( ThreadId tid, UWord arg, Addr ret );
extern void VGA_(thread_initial_stack)  ( ThreadId tid, UWord arg, Addr ret );

// Symtab stuff
extern UInt* VGA_(reg_addr_from_BB)  ( Int reg );
extern UInt* VGA_(reg_addr_from_tst) ( Int reg, arch_thread_t* );

// Pointercheck
extern Bool VGA_(setup_pointercheck) ( void );

// For attaching the debugger
extern Int  VGA_(ptrace_setregs_from_BB)  ( Int pid );
extern Int  VGA_(ptrace_setregs_from_tst) ( Int pid, arch_thread_t* arch );

// Making coredumps
extern void VGA_(fill_elfregs_from_BB)     ( struct vki_user_regs_struct* regs );
extern void VGA_(fill_elfregs_from_tst)    ( struct vki_user_regs_struct* regs,
                                             arch_thread_t* arch );
extern void VGA_(fill_elffpregs_from_BB)   ( vki_elf_fpregset_t* fpu );
extern void VGA_(fill_elffpregs_from_tst)  ( vki_elf_fpregset_t* fpu,
                                             const arch_thread_t* arch );
extern void VGA_(fill_elffpxregs_from_BB)  ( vki_elf_fpxregset_t* xfpu );
extern void VGA_(fill_elffpxregs_from_tst) ( vki_elf_fpxregset_t* xfpu,
                                             const arch_thread_t* arch );

// Signal stuff
extern void VGA_(push_signal_frame) ( ThreadId tid, Addr sp_top_of_frame,
                                      const vki_siginfo_t *siginfo,
                                      void *handler, UInt flags,
                                      const vki_sigset_t *mask);
extern Int  VGA_(pop_signal_frame)  ( ThreadId tid );

// libpthread stuff
typedef struct arch_thread_aux arch_thread_aux_t;

void VGA_(thread_create) ( arch_thread_aux_t *aux );
void VGA_(thread_wrapper)( arch_thread_aux_t *aux );
void VGA_(thread_exit)   ( void );

Bool VGA_(has_tls)       ( void );

#define MY__STRING(__str)  #__str

// Assertion to use in code running on the simulated CPU.
#define my_assert(expr)                                               \
  ((void) ((expr) ? 0 :						      \
	   (VG_(user_assert_fail) (MY__STRING(expr),		      \
			      __FILE__, __LINE__,                     \
                              __PRETTY_FUNCTION__), 0)))

extern void VG_(user_assert_fail) ( const Char* expr, const Char* file,
                                    Int line, const Char* fn );


// ---------------------------------------------------------------------
// Platform-specific things defined in eg. x86/*.c
// ---------------------------------------------------------------------

struct SyscallTableEntry {
   UInt  *flags_ptr;
   void        (*before)(ThreadId tid, ThreadState *tst);
   void        (*after) (ThreadId tid, ThreadState *tst);
};

/* This table is the mapping from __NR_xxx syscall numbers to the PRE/POST
   wrappers for the relevant syscalls used in the OS kernel for that number.
   Note that the constant names don't always match the wrapper names in a
   straightforward way.  For example, on x86/Linux: 
      
      __NR_lchown       --> sys_lchown16()
      __NR_lchown32     --> sys_lchown()
      __NR_select       --> old_select()
      __NR__newselect   --> sys_select()
*/
extern const struct SyscallTableEntry VGA_(syscall_table)[];

extern const UInt VGA_(syscall_table_size);
   

extern const Addr vga_sys_before, vga_sys_restarted,
                  vga_sys_after,  vga_sys_done;

extern void VGA_(restart_syscall)(arch_thread_t* arch);

extern void VGA_(thread_syscall)(Int syscallno, arch_thread_t* arch, 
                                 enum PXState* state, enum PXState poststate);

/* ---------------------------------------------------------------------
   Finally - autoconf-generated settings
   ------------------------------------------------------------------ */

#include "config.h"

#endif /* ndef __CORE_H */

/*--------------------------------------------------------------------*/
/*--- end                                                          ---*/
/*--------------------------------------------------------------------*/
