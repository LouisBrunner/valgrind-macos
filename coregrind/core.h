
/*--------------------------------------------------------------------*/
/*--- A header file for all private parts of Valgrind's core.      ---*/
/*--- Include no other! (more or less...)                          ---*/
/*---                                                       core.h ---*/
/*--------------------------------------------------------------------*/

/*
   This file is part of Valgrind, a dynamic binary instrumentation
   framework.

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

#include "core_platform.h" // platform-specific stuff,
                           //   eg. x86-linux/core_platform.h
#include "core_os.h"       // OS-specific stuff,    eg. linux/core_os.h

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
   it's own use - just a small constant. */
#define VG_N_RESERVED_FDS (10)

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
      VgLogTo_FileExactly,
      VgLogTo_Socket
   } VgLogTo;

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
/* Generating a suppression for each error?   default: 0 (NO)
   Other values: 1 (yes, but ask user), 2 (yes, don't ask user) */
extern Int  VG_(clo_gen_suppressions);
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

/* DEBUG: print generated code?  default: 00000000 ( == NO ) */
extern Bool  VG_(clo_trace_flags);
/* DEBUG: do bb profiling?  default: 00000000 ( == NO ) */
extern Bool  VG_(clo_profile_flags);
/* DEBUG: if tracing codegen, be quiet until after this bb ( 0 ) */
extern Int   VG_(clo_trace_notbelow);
/* DEBUG: print system calls?  default: NO */
extern Bool  VG_(clo_trace_syscalls);
/* DEBUG: print signal details?  default: NO */
extern Bool  VG_(clo_trace_signals);
/* DEBUG: print symtab details?  default: NO */
extern Bool  VG_(clo_trace_symtab);
/* DEBUG: print redirection details?  default: NO */
extern Bool  VG_(clo_trace_redir);
/* DEBUG: print thread scheduling events?  default: NO */
extern Bool  VG_(clo_trace_sched);
/* DEBUG: print pthreads calls?  default: NO */
extern Bool  VG_(clo_trace_pthreads);
/* Display gory details for the k'th most popular error.  default:
   Infinity. */
extern Int   VG_(clo_dump_error);
/* Number of parents of a backtrace.  Default: 8.  */
extern Int   VG_(clo_backtrace_size);
/* Engage miscellaneous weird hacks needed for some progs. */
extern Char* VG_(clo_weird_hacks);

/* Track open file descriptors? */
extern Bool  VG_(clo_track_fds);

/* Should we run __libc_freeres at exit?  Sometimes causes crashes.
   Default: YES.  Note this is subservient to VG_(needs).libc_freeres;
   if the latter says False, then the setting of VG_(clo_weird_hacks)
   is ignored.  Ie if a tool says no, I don't want this to run, that
   cannot be overridden from the command line. */
extern Bool  VG_(clo_run_libc_freeres);
/* Generate branch-prediction hints? */
extern Bool VG_(clo_branchpred);
/* Continue stack traces below main()?  Default: NO */
extern Bool VG_(clo_show_below_main);
/* Test each client pointer dereference to check it's within the
   client address space bounds */
extern Bool VG_(clo_pointercheck);
/* Model the pthread library */
extern Bool VG_(clo_model_pthreads);

/* HACK: Use hacked version of clone for Quadrics Elan3 drivers */
extern Bool VG_(clo_support_elan3);

/* Set up the libc freeres wrapper */
extern void VGA_(intercept_libc_freeres_wrapper)(Addr);

// Clean up the client by calling before the final reports
extern void VGA_(final_tidyup)(ThreadId tid);

// Arch-specific client requests
extern Bool VGA_(client_requests)(ThreadId tid, UWord *args);

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
      Bool no_longer_used_1;     // for backwards compatibility
      Bool command_line_options;
      Bool client_requests;
      Bool no_longer_used_0;     // for backwards compatibility
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
                                  SizeT nmemb, SizeT bytes_per_memb );
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

/* Obsolete pthread-related requests */
#define VG_USERREQ__APPLY_IN_NEW_THREAD     0x3001
#define VG_USERREQ__QUIT                    0x3002
#define VG_USERREQ__WAIT_JOINER             0x3003
#define VG_USERREQ__PTHREAD_JOIN            0x3004
#define VG_USERREQ__SET_CANCELSTATE         0x3005
#define VG_USERREQ__SET_CANCELTYPE          0x3006
#define VG_USERREQ__TESTCANCEL              0x3007
#define VG_USERREQ__SET_CANCELPEND          0x3008
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
#define VG_USERREQ__PTHREAD_SIGMASK         0x3018
#define VG_USERREQ__SIGWAIT                 0x3019
#define VG_USERREQ__PTHREAD_KILL            0x301A
#define VG_USERREQ__PTHREAD_YIELD           0x301B
#define VG_USERREQ__PTHREAD_KEY_VALIDATE    0x301C
#define VG_USERREQ__CLEANUP_PUSH            0x3020
#define VG_USERREQ__CLEANUP_POP             0x3021
#define VG_USERREQ__GET_KEY_D_AND_S         0x3022
#define VG_USERREQ__NUKE_OTHER_THREADS      0x3023
#define VG_USERREQ__GET_N_SIGS_RETURNED     0x3024
#define VG_USERREQ__SET_FHSTACK_USED        0x3025
#define VG_USERREQ__GET_FHSTACK_USED        0x3026
#define VG_USERREQ__SET_FHSTACK_ENTRY       0x3027
#define VG_USERREQ__GET_FHSTACK_ENTRY       0x3028
#define VG_USERREQ__GET_SIGRT_MIN	    0x302B
#define VG_USERREQ__GET_SIGRT_MAX	    0x302C
#define VG_USERREQ__ALLOC_RTSIG		    0x302D
#define VG_USERREQ__GET_MALLOCFUNCS	    0x3030
#define VG_USERREQ__GET_STACK_INFO          0x3033
#define VG_USERREQ__GET_PTHREAD_TRACE_LEVEL 0x3101
#define VG_USERREQ__PTHREAD_ERROR           0x3102


#define VG_USERREQ__READ_MILLISECOND_TIMER  0x3017

/* Internal equivalent of VALGRIND_PRINTF . */
#define VG_USERREQ__INTERNAL_PRINTF         0x3103
/* Internal equivalent of VALGRIND_PRINTF_BACKTRACE . */
#define VG_USERREQ__INTERNAL_PRINTF_BACKTRACE 0x3104

/* Denote the finish of __libc_freeres_wrapper(). 
   A synonym for exit. */
#define VG_USERREQ__LIBC_FREERES_DONE       0x3029

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
   Addr	tl_malloc;
   Addr	tl_calloc;
   Addr	tl_realloc;
   Addr	tl_memalign;
   Addr	tl___builtin_new;
   Addr	tl___builtin_vec_new;
   Addr	tl_free;
   Addr	tl___builtin_delete;
   Addr	tl___builtin_vec_delete;

   Addr	arena_payload_szB;

   Bool	clo_sloppy_malloc;
   Bool	clo_trace_malloc;
};


/* ---------------------------------------------------------------------
   Exports of vg_defaults.c
   ------------------------------------------------------------------ */

extern Bool VG_(tl_malloc_called_by_scheduler);



/* ---------------------------------------------------------------------
   Exports of vg_scheduler.c
   ------------------------------------------------------------------ */

/* 
   Thread state machine:

   Empty -> Init -> Runnable <=> WaitSys/Yielding
     ^                 |
     \---- Zombie -----/		       
 */
typedef
   enum ThreadStatus { 
      VgTs_Empty,      /* this slot is not in use */
      VgTs_Init,       /* just allocated */
      VgTs_Runnable,   /* ready to run */
      VgTs_WaitSys,    /* waiting for a syscall to complete */
      VgTs_Yielding,   /* temporarily yielding the CPU */
      VgTs_Zombie,     /* transient state just before exiting */
   }
   ThreadStatus;

/* Return codes from the scheduler. */
typedef
   enum { 
      VgSrc_None,	 /* not exiting yet */
      VgSrc_ExitSyscall, /* client called exit().  This is the normal
                            route out. */
      VgSrc_FatalSig	 /* Killed by the default action of a fatal
			    signal */
   }
   VgSchedReturnCode;

struct _ThreadState {
   /* ThreadId == 0 (and hence vg_threads[0]) is NEVER USED.
      The thread identity is simply the index in vg_threads[].
      ThreadId == 1 is the root thread and has the special property
      that we don't try and allocate or deallocate its stack.  For
      convenience of generating error message, we also put the
      ThreadId in this tid field, but be aware that it should
      ALWAYS == the index in vg_threads[]. */
   ThreadId tid;

   /* Current scheduling status. */
   ThreadStatus status;

   /* This is set if the thread is in the process of exiting for any
      reason.  The precise details of the exit are in the OS-specific
      state. */
   VgSchedReturnCode exitreason;

   /* Architecture-specific thread state. */
   ThreadArchState arch;

   /* This thread's blocked-signals mask.  Semantics is that for a
      signal to be delivered to this thread, the signal must not be
      blocked by this signal mask.  If more than one thread accepts a
      signal, then it will be delivered to one at random.  If all
      threads block the signal, it will remain pending until either a
      thread unblocks it or someone uses sigwaitsig/sigtimedwait. */
   vki_sigset_t sig_mask;

   /* tmp_sig_mask is usually the same as sig_mask, and is kept in
      sync whenever sig_mask is changed.  The only time they have
      different values is during the execution of a sigsuspend, where
      tmp_sig_mask is the temporary mask which sigsuspend installs.
      It is only consulted to compute the signal mask applied to a
      signal handler. */
   vki_sigset_t tmp_sig_mask;

   /* A little signal queue for signals we can't get the kernel to
      queue for us.  This is only allocated as needed, since it should
      be rare. */
   struct SigQueue *sig_queue;

   /* Syscall the Thread is currently running; -1 if none.  Should only
      be set while Thread is in VgTs_WaitSys. */
   Int syscallno;

   /* A value the Tool wants to pass from its pre-syscall to its
      post-syscall function. */
   void *tool_pre_syscall_value;

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

   /* Address of the highest legitimate word in this stack.  This is
      used for error messages only -- not critical for execution
      correctness.  Is is set for all stacks, specifically including
      ThreadId == 0 (the main thread). */
   Addr stack_highest_word;

   /* Alternate signal stack */
   vki_stack_t altstack;

   /* OS-specific thread state */
   os_thread_t os_state;

   /* Used in the syscall handlers.  Set to True to indicate that the
      PRE routine for a syscall has set the syscall result already and
      so the syscall does not need to be handed to the kernel. */
   Bool syscall_result_set;
   
   /* Per-thread jmp_buf to resume scheduler after a signal */
   Bool    sched_jmpbuf_valid;
   jmp_buf sched_jmpbuf;

   /* Info about the signal we just got */
   vki_siginfo_t	siginfo;
};
//ThreadState;

/* The thread table. */
extern ThreadState VG_(threads)[VG_N_THREADS];

/* Allocate a new ThreadState */
extern ThreadId VG_(alloc_ThreadState)(void);

/* A thread exits.  tid must currently be running. */
extern void VG_(exit_thread)(ThreadId tid);

/* Kill a thread.  This interrupts whatever a thread is doing, and
   makes it exit ASAP.  This does not set the exitreason or
   exitcode. */
extern void VG_(kill_thread)(ThreadId tid);

/* Check that tid is in range and denotes a non-Empty thread. */
extern Bool VG_(is_valid_tid) ( ThreadId tid );

/* Get the ThreadState for a particular thread */
extern ThreadState *VG_(get_ThreadState)(ThreadId tid);

/* Given an LWP id (ie, real kernel thread id), find the corresponding
   ThreadId */
extern ThreadId VG_(get_lwp_tid)(Int lwpid);

/* Returns true if a thread is currently running (ie, has the CPU lock) */
extern Bool VG_(is_running_thread)(ThreadId tid);

/* Returns true if the thread is in the process of exiting */
extern Bool VG_(is_exiting)(ThreadId tid);

/* Return the number of non-dead Threads */
extern Int VG_(count_living_threads)(void);

/* Nuke all threads except tid. */
extern void VG_(nuke_all_threads_except) ( ThreadId me, VgSchedReturnCode reason );

/* Make a thread the running thread.  The thread must previously been
   sleeping, and not holding the CPU semaphore. This will set the
   thread state to VgTs_Runnable, and the thread will attempt to take
   the CPU semaphore.  By the time it returns, tid will be the running
   thread. */
extern void VG_(set_running) ( ThreadId tid );

/* Set a thread into a sleeping state.  Before the call, the thread
   must be runnable, and holding the CPU semaphore.  When this call
   returns, the thread will be set to the specified sleeping state,
   and will not be holding the CPU semaphore.  Note that another
   thread could be running by the time this call returns, so the
   caller must be careful not to touch any shared state.  It is also
   the caller's responsibility to actually block until the thread is
   ready to run again. */
extern void VG_(set_sleeping) ( ThreadId tid, ThreadStatus state );

/* Yield the CPU for a while */
extern void VG_(vg_yield)(void);

// The scheduler.
extern VgSchedReturnCode VG_(scheduler) ( ThreadId tid );

// Do everything which needs doing before the process finally ends,
// like printing reports, etc
extern void VG_(shutdown_actions)(ThreadId tid);

extern void VG_(scheduler_init) ( void );

extern void VG_(pp_sched_status) ( void );

// Longjmp back to the scheduler and thus enter the sighandler immediately.
extern void VG_(resume_scheduler) ( ThreadId tid );

/* If true, a fault is Valgrind-internal (ie, a bug) */
extern Bool VG_(my_fault);

// Write a value to a client's thread register, and shadow (if necessary).
// Note that there are some further similar macros in the arch- and
// platform-specific parts;  these ones are the totally generic ones.
#define SET_THREAD_REG( zztid, zzval, zzGETREG, zzevent, zzargs... ) \
   do { zzGETREG(VG_(threads)[zztid].arch) = (zzval); \
        VG_TRACK( zzevent, ##zzargs ); \
   } while (0)

#define SET_CLREQ_RETVAL(zztid, zzval) \
   SET_THREAD_REG(zztid, zzval, CLREQ_RET, post_reg_write, \
                  Vg_CoreClientReq, zztid, O_CLREQ_RET, sizeof(UWord))

#define SET_CLCALL_RETVAL(zztid, zzval, f) \
   SET_THREAD_REG(zztid, zzval, CLREQ_RET, post_reg_write_clientcall_return, \
                  zztid, O_CLREQ_RET, sizeof(UWord), f)

#define SET_PTHREQ_ESP(zztid, zzval) \
   SET_THREAD_REG(zztid, zzval, STACK_PTR, post_reg_write, \
                  Vg_CorePThread, zztid, O_STACK_PTR, sizeof(Addr))

#define SET_PTHREQ_RETVAL(zztid, zzval) \
   SET_THREAD_REG(zztid, zzval, PTHREQ_RET, post_reg_write, \
                  Vg_CorePThread, zztid, O_PTHREQ_RET, sizeof(UWord))

/* ---------------------------------------------------------------------
   Exports of vg_signals.c
   ------------------------------------------------------------------ */

/* Set the standard set of blocked signals, used wheneever we're not
   running a client syscall. */
extern void VG_(block_signals)(ThreadId tid);

/* Highest signal the kernel will let us use */
extern Int VG_(max_signal);

extern void VG_(sigstartup_actions) ( void );

/* Modify a thread's state so that when it next runs it will be
   running in the signal handler (or doing the default action if there
   is none). */
extern void VG_(deliver_signal) ( ThreadId tid, const vki_siginfo_t * );

extern Bool VG_(is_sig_ign) ( Int sigNo );

/* Poll a thread's set of pending signals, and update the Thread's context to deliver one */
extern void VG_(poll_signals) ( ThreadId );

/* Fake system calls for signal handling. */
extern void VG_(do_sys_sigaltstack)   ( ThreadId tid );
extern Int  VG_(do_sys_sigaction)     ( Int signo, 
					const struct vki_sigaction *new_act, 
					struct vki_sigaction *old_act );
extern void VG_(do_sys_sigprocmask)   ( ThreadId tid, Int how, 
                                        vki_sigset_t* set,
                                        vki_sigset_t* oldset );
extern void VG_(do_pthread_sigmask_SCSS_upd) ( ThreadId tid, Int how, 
                                               vki_sigset_t* set,
                                               vki_sigset_t* oldset );

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
extern void VG_(synth_sigill)       (ThreadId tid, Addr addr);

/* Extend the stack to cover addr, if possible */
extern Bool VG_(extend_stack)(Addr addr, UInt maxsize);

/* Returns True if the signal is OK for the client to use */
extern Bool VG_(client_signal_OK)(Int sigNo);

/* Forces the client's signal handler to SIG_DFL - generally just
   before using that signal to kill the process. */
extern void VG_(set_default_handler)(Int sig);

/* Adjust a client's signal mask to match our internal requirements */
extern void VG_(sanitize_client_sigmask)(ThreadId tid, vki_sigset_t *mask);

/* Wait until a thread-related predicate is true */
extern void VG_(wait_for_threadstate)(Bool (*pred)(void *), void *arg);

/* ---------------------------------------------------------------------
   Exports of vg_mylibc.c
   ------------------------------------------------------------------ */

// Useful for making failing stubs, when certain things haven't yet been
// implemented.
#define I_die_here                                                    \
   VG_(core_assert_fail) ("Unimplemented functionality",              \
                           __FILE__, __LINE__, __PRETTY_FUNCTION__)

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
extern Int VG_(mprotect_native)( void *start, SizeT length, UInt prot );


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

extern void   VG_(nanosleep)(struct vki_timespec *);
/* ---------------------------------------------------------------------
   Exports of vg_message.c
   ------------------------------------------------------------------ */

/* Low-level -- send bytes directly to the message sink.  Do not
   use. */
extern void VG_(send_bytes_to_logging_sink) ( Char* msg, Int nbytes );

// Functions for printing from code within Valgrind, but which runs on the
// sim'd CPU.  Defined here because needed for vg_replace_malloc.c.  The
// weak attribute ensures the multiple definitions are not a problem.  They
// must be functions rather than macros so that va_list can be used.

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

extern void   VG_(reloc_abs_jump)	  ( UChar *jmp );

/* ---------------------------------------------------------------------
   Exports of vg_translate.c
   ------------------------------------------------------------------ */

extern 
Bool VG_(translate) ( ThreadId tid, 
                      Addr64   orig_addr,
                      Bool     debugging_translation,
                      Int      debugging_verbosity );

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

typedef
   enum { 
      ThreadErr      = -1,   // Thread error
      MutexErr       = -2,   // Mutex error
   }
   CoreErrorKind;

extern void VG_(load_suppressions)    ( void );

extern void VG_(show_all_errors)      ( void );

extern Bool VG_(is_action_requested)  ( Char* action, Bool* clo );

extern UInt VG_(get_n_errs_found)     ( void );


/* ---------------------------------------------------------------------
   Exports of vg_procselfmaps.c
   ------------------------------------------------------------------ */

/* Parses /proc/self/maps, calling `record_mapping' for each entry. */
extern 
void VG_(parse_procselfmaps) (
   void (*record_mapping)( Addr addr, SizeT len, UInt prot,
			   UInt dev, UInt ino, ULong foff,
                           const UChar *filename ) );


/* ---------------------------------------------------------------------
   Exports of vg_symtab2.c
   ------------------------------------------------------------------ */

typedef struct _Segment Segment;
typedef struct _CodeRedirect CodeRedirect;

extern Bool VG_(is_object_file)   ( const void *hdr );
extern void VG_(mini_stack_dump)  ( Addr ips[], UInt n_ips );
extern SegInfo * VG_(read_seg_symbols) ( Segment *seg );
extern void VG_(symtab_incref)	  ( SegInfo * );
extern void VG_(symtab_decref)	  ( SegInfo *, Addr a );

extern Bool VG_(get_fnname_nodemangle)( Addr a, Char* fnname, Int n_fnname );

extern Addr VG_(reverse_search_one_symtab) ( const SegInfo* si, const Char* name );

/* Set up some default redirects */
extern void VG_(setup_code_redirect_table) ( void );

extern Bool VG_(resolve_redir_allsegs)(CodeRedirect *redir);

/* ---------------------------------------------------------------------
   Exports of vg_redir.c
   ------------------------------------------------------------------ */
/* Redirection machinery */
extern Addr VG_(code_redirect) ( Addr orig );

extern void VG_(add_redirect_addr)(const Char *from_lib, const Char *from_sym,
				   Addr to_addr);
extern void VG_(resolve_seg_redirs)(SegInfo *si);
extern Bool VG_(resolve_redir)(CodeRedirect *redir, const SegInfo *si);

/* Wrapping machinery */
enum return_type {
   RT_RETURN,
   RT_LONGJMP,
   RT_EXIT,
};

typedef struct _FuncWrapper FuncWrapper;
struct _FuncWrapper {
   void *(*before)(va_list args);
   void  (*after) (void *nonce, enum return_type, Word retval);
};

extern void VG_(wrap_function)(Addr eip, const FuncWrapper *wrapper);
extern const FuncWrapper *VG_(is_wrapped)(Addr eip);
extern Bool VG_(is_wrapper_return)(Addr eip);

/* Primary interface for adding wrappers for client-side functions. */
extern CodeRedirect *VG_(add_wrapper)(const Char *from_lib, const Char *from_sym,
				      const FuncWrapper *wrapper);

extern Bool VG_(is_resolved)(const CodeRedirect *redir);

/* ---------------------------------------------------------------------
   Exports of vg_main.c
   ------------------------------------------------------------------ */

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

/* The master thread the one which will be responsible for mopping
   everything up at exit.  Normally it is tid 1, since that's the
   first thread created, but it may be something else after a
   fork(). */
extern ThreadId VG_(master_tid);

/* Called when some unhandleable client behaviour is detected.
   Prints a msg and aborts. */
extern void VG_(unimplemented) ( Char* msg )
            __attribute__((__noreturn__));

/* Something of a function looking for a home ... start up debugger. */
extern void VG_(start_debugger) ( ThreadId tid );

/* Counts downwards in vg_run_innerloop. */
extern UInt VG_(dispatch_ctr);

/* Instruction pointer guest state offset, used by $VG_ARCH/dispatch.S. */
extern OffT VG_(instr_ptr_offset);

/* Stats ... */
extern void VG_(print_scheduler_stats) ( void );

/* Indicates what arch and subarch we are running on. */
extern VexArch    VG_(vex_arch);
extern VexSubArch VG_(vex_subarch);


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
#define SF_DEVICE   (1 << 15) // device mapping; avoid careless touching

struct _Segment {
   UInt         prot;         // VKI_PROT_*
   UInt         flags;        // SF_*

   Addr         addr;         // mapped addr (page aligned)
   SizeT        len;          // size of mapping (page aligned)

   // These are valid if (flags & SF_FILE)
   OffT        offset;        // file offset
   const Char* filename;      // filename (NULL if unknown)
   Int         fnIdx;         // filename table index (-1 if unknown)
   UInt        dev;           // device
   UInt        ino;           // inode

   SegInfo*    symtab;        // symbol table
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

/* Find the segment containing a, or NULL if none. */
extern Segment *VG_(find_segment)(Addr a);

/* a is an unmapped address (is checked).  Find the next segment 
   along in the address space, or NULL if none. */
extern Segment *VG_(find_segment_above_unmapped)(Addr a);

/* a is a mapped address (in a segment, is checked).  Find the
   next segment along. */
extern Segment *VG_(find_segment_above_mapped)(Addr a);

extern Bool VG_(seg_contains)(const Segment *s, Addr ptr, SizeT size);
extern Bool VG_(seg_overlaps)(const Segment *s, Addr ptr, SizeT size);

extern Segment *VG_(split_segment)(Addr a);

extern void VG_(pad_address_space)  (Addr start);
extern void VG_(unpad_address_space)(Addr start);

extern REGPARM(2)
       void VG_(unknown_SP_update) ( Addr old_SP, Addr new_SP );

///* Search /proc/self/maps for changes which aren't reflected in the
//   segment list */
//extern void VG_(sync_segments)(UInt flags);

/* Return string for prot */
extern const HChar *VG_(prot_str)(UInt prot);

//extern void VG_(print_shadow_stats)();

/* ---------------------------------------------------------------------
   Exports of vg_syscalls.c
   ------------------------------------------------------------------ */

extern HChar* VG_(resolve_filename_nodup)(Int fd);
extern HChar* VG_(resolve_filename)(Int fd);

/* Simple Valgrind-internal atfork mechanism */
extern void VG_(do_atfork_pre)   (ThreadId tid);
extern void VG_(do_atfork_parent)(ThreadId tid);
extern void VG_(do_atfork_child) (ThreadId tid);


extern void VG_(client_syscall) ( ThreadId tid );

extern void VG_(post_syscall)   ( ThreadId tid );

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

void VG_(record_fd_open)(ThreadId tid, Int fd, char *pathname);
   
// Flags describing syscall wrappers
#define Special    (1 << 0)	/* handled specially			*/
#define MayBlock   (1 << 1)	/* may block				*/
#define PostOnFail (1 << 2)	/* call POST() function on failure	*/
#define PadAddr	   (1 << 3)	/* pad+unpad address space around syscall */
#define Done       (1 << 4)	/* used if a PRE() did the syscall	*/

// Templates for generating the PRE and POST macros.  For ones that must be
// publically visible, use an empty 'qual', 'prefix' should start with
// "vgArch_", and there should be corresponding global declarations (like
// the GEN_SYSCALL_WRAPPER ones below).  Otherwise, use "static" for 'qual',
// and "vgArch_" should not be in the 'prefix'.
#define PRE_TEMPLATE(qual, prefix, name, f) \
   qual UInt prefix##_##name##_flags = f; \
   qual void prefix##_##name##_before(ThreadId tid, ThreadState *tst)
#define POST_TEMPLATE(qual, prefix, name) \
   qual void prefix##_##name##_after (ThreadId tid, ThreadState *tst)

// This macro is used to write other macros which making writing syscall
// tables easier.
#define SYS_WRAPPER_ENTRY_X_(prefix, const, name) \
   [const] = { &prefix##_##name##_flags, \
                prefix##_##name##_before, NULL }
#define SYS_WRAPPER_ENTRY_XY(prefix, const, name) \
   [const] = { &prefix##_##name##_flags, \
                prefix##_##name##_before, \
                prefix##_##name##_after }

// Macros for adding generic wrappers to a syscall table.
#define GENX_(const, name)    SYS_WRAPPER_ENTRY_X_(vgArch_gen, const, name)
#define GENXY(const, name)    SYS_WRAPPER_ENTRY_XY(vgArch_gen, const, name)

// Space-saving macros for syscall wrappers
#define SYSNO   SYSCALL_NUM(tst->arch)    // in PRE(x)
#define RES     SYSCALL_RET(tst->arch)    // in POST(x)
#define ARG1    SYSCALL_ARG1(tst->arch)
#define ARG2    SYSCALL_ARG2(tst->arch)
#define ARG3    SYSCALL_ARG3(tst->arch)
#define ARG4    SYSCALL_ARG4(tst->arch)
#define ARG5    SYSCALL_ARG5(tst->arch)
#define ARG6    SYSCALL_ARG6(tst->arch)

#define SET_RESULT(val)                                \
   do { PLATFORM_SET_SYSCALL_RESULT(tst->arch, (val)); \
        tst->syscall_result_set = True;                \
   } while (0)

#define PRINT(format, args...)  \
   if (VG_(clo_trace_syscalls))        \
      VG_(printf)(format, ## args)

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
//
// For each generic ("gen") wrapper, we declare the pre-wrapper, the
// post-wrapper (which is actually not always needed), and the associated
// flags.
#define GEN_SYSCALL_WRAPPER(x) \
   extern UInt VGA_(gen_##x##_flags); \
   extern void VGA_(gen_##x##_before)(ThreadId tid, ThreadState *tst); \
   extern void VGA_(gen_##x##_after) (ThreadId tid, ThreadState *tst)

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
GEN_SYSCALL_WRAPPER(sys_sched_rr_get_interval);
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
GEN_SYSCALL_WRAPPER(sys_clock_nanosleep);
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
GEN_SYSCALL_WRAPPER(sys_stime);	    // SVr4, SVID, X/OPEN
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
GEN_SYSCALL_WRAPPER(sys_tkill);			// * L
GEN_SYSCALL_WRAPPER(sys_tgkill);		// * L
GEN_SYSCALL_WRAPPER(sys_gettid);		// * L?

#undef GEN_SYSCALL_WRAPPER

// Macros used in syscall wrappers
/* PRRAn == "pre-register-read-argument"
   PRRSN == "pre-register-read-syscall"
*/

#define PRRSN \
      TL_(pre_reg_read)(Vg_CoreSysCall, tid, "(syscallno)", \
                        O_SYSCALL_NUM, sizeof(UWord));
#define PRRAn(n,s,t,a) \
      TL_(pre_reg_read)(Vg_CoreSysCall, tid, s"("#a")", \
                        O_SYSCALL_ARG##n, sizeof(t));
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
   VG_TRACK( post_mem_write, Vg_CoreSysCall, tid, zzaddr, zzlen)


//////////////////////////////////////////////////////////

#define TId ThreadId
#define UW  UWord

extern void  VG_(generic_PRE_sys_socketpair)   ( TId, UW, UW, UW, UW );
extern UWord VG_(generic_POST_sys_socketpair)  ( TId, UW, UW, UW, UW, UW );
extern UWord VG_(generic_POST_sys_socket)      ( TId, UW );
extern void  VG_(generic_PRE_sys_bind)         ( TId, UW, UW, UW );
extern void  VG_(generic_PRE_sys_accept)       ( TId, UW, UW, UW );
extern UWord VG_(generic_POST_sys_accept)      ( TId, UW, UW, UW, UW );
extern void  VG_(generic_PRE_sys_sendto)       ( TId, UW, UW, UW, UW, UW, UW );
extern void  VG_(generic_PRE_sys_send)         ( TId, UW, UW, UW );
extern void  VG_(generic_PRE_sys_recvfrom)     ( TId, UW, UW, UW, UW, UW, UW );
extern void  VG_(generic_POST_sys_recvfrom)    ( TId, UW, UW, UW, UW, UW, UW, UW );
extern void  VG_(generic_PRE_sys_recv)         ( TId, UW, UW, UW );
extern void  VG_(generic_POST_sys_recv)        ( TId, UW, UW, UW, UW );
extern void  VG_(generic_PRE_sys_connect)      ( TId, UW, UW, UW );
extern void  VG_(generic_PRE_sys_setsockopt)   ( TId, UW, UW, UW, UW, UW );
extern void  VG_(generic_PRE_sys_getsockopt)   ( TId, UW, UW, UW, UW, UW );
extern void  VG_(generic_POST_sys_getsockopt)  ( TId, UW, UW, UW, UW, UW, UW );
extern void  VG_(generic_PRE_sys_getsockname)  ( TId, UW, UW, UW );
extern void  VG_(generic_POST_sys_getsockname) ( TId, UW, UW, UW, UW );
extern void  VG_(generic_PRE_sys_getpeername)  ( TId, UW, UW, UW );
extern void  VG_(generic_POST_sys_getpeername) ( TId, UW, UW, UW, UW );
extern void  VG_(generic_PRE_sys_sendmsg)      ( TId, UW, UW );
extern void  VG_(generic_PRE_sys_recvmsg)      ( TId, UW, UW );
extern void  VG_(generic_POST_sys_recvmsg)     ( TId, UW, UW, UW );

#undef TID
#undef UW


/* ---------------------------------------------------------------------
   Exports of vg_transtab.c
   ------------------------------------------------------------------ */

/* The fast-cache for tt-lookup, and for finding counters. */
extern ULong* VG_(tt_fast) [VG_TT_FAST_SIZE];
extern UInt*  VG_(tt_fastN)[VG_TT_FAST_SIZE];


extern void VG_(init_tt_tc)       ( void );

extern
void VG_(add_to_trans_tab)( VexGuestExtents* vge,
                            Addr64           entry,
                            AddrH            code,
                            UInt             code_len );

extern Bool VG_(search_transtab) ( /*OUT*/AddrH* result,
                                   Addr64        guest_addr, 
                                   Bool          upd_cache );

extern void VG_(discard_translations) ( Addr64 start, UInt range );

extern void VG_(sanity_check_tt_tc) ( Char* caller );

extern void VG_(print_tt_tc_stats) ( void );

extern UInt VG_(get_bbs_translated) ( void );

extern void VG_(show_BB_profile) ( void );


/* ---------------------------------------------------------------------
   Exports of vg_syscall.S
   ------------------------------------------------------------------ */

// We use a full prototype rather than "..." here to ensure that all
// arguments get converted to a UWord appropriately.  Not doing so can
// cause problems when passing 32-bit integers on 64-bit platforms, because
// the top 32-bits might not be zeroed appropriately, eg. as would happen
// with the 6th arg on AMD64 which is passed on the stack.
extern Word VG_(do_syscall) ( UInt, UWord, UWord, UWord, UWord, UWord, UWord );

// Macros make life easier.
#define vgPlain_do_syscall0(s)             VG_(do_syscall)((s),0,0,0,0,0,0)
#define vgPlain_do_syscall1(s,a)           VG_(do_syscall)((s),(a),0,0,0,0,0)
#define vgPlain_do_syscall2(s,a,b)         VG_(do_syscall)((s),(a),(b),0,0,0,0)
#define vgPlain_do_syscall3(s,a,b,c)       VG_(do_syscall)((s),(a),(b),(c),0,0,0)
#define vgPlain_do_syscall4(s,a,b,c,d)     VG_(do_syscall)((s),(a),(b),(c),(d),0,0)
#define vgPlain_do_syscall5(s,a,b,c,d,e)   VG_(do_syscall)((s),(a),(b),(c),(d),(e),0)
#define vgPlain_do_syscall6(s,a,b,c,d,e,f) VG_(do_syscall)((s),(a),(b),(c),(d),(e),(f))

extern Int VG_(clone) ( Int (*fn)(void *), void *stack, Int flags, void *arg, 
			Int *child_tid, Int *parent_tid, vki_modify_ldt_t * );
extern void VG_(sigreturn)(void);

/* ---------------------------------------------------------------------
   Exports of vg_dispatch.S
   ------------------------------------------------------------------ */

/* This subroutine is called from the C world.  It is passed
   a pointer to the VEX guest state (arch.vex).  It must run code
   from the instruction pointer in the guest state, and exit when
   VG_(dispatch_ctr) reaches zero, or we need to defer to the scheduler.
   The return value must indicate why it returned back to the scheduler.
   It can also be exited if the executing code throws a non-resumable
   signal, for example SIGSEGV, in which case control longjmp()s back past
   here.

   This code simply handles the common case fast -- when the translation
   address is found in the translation cache.  For anything else, the
   scheduler does the work.
*/
extern UInt VG_(run_innerloop) ( void* guest_state );

/* ---------------------------------------------------------------------
   Exports of vg_helpers.S
   ------------------------------------------------------------------ */

/* Information about trampoline code (for signal return and syscalls) */
extern const Char VG_(trampoline_code_start);
extern const Int  VG_(trampoline_code_length);
extern const Int  VG_(tramp_sigreturn_offset);
extern const Int  VG_(tramp_rt_sigreturn_offset);
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

// ---------------------------------------------------------------------
// Architecture-specific things defined in eg. x86/*.c
// ---------------------------------------------------------------------

// Returns the architecture and subarchitecture, or indicates
// that this subarchitecture is unable to run Valgrind
// Returns False to indicate we cannot proceed further.
extern Bool VGA_(getArchAndSubArch)( /*OUT*/VexArch*, 
                                     /*OUT*/VexSubArch* );
// Accessors for the ThreadArchState
#define INSTR_PTR(regs)    ((regs).vex.ARCH_INSTR_PTR)
#define STACK_PTR(regs)    ((regs).vex.ARCH_STACK_PTR)
#define FRAME_PTR(regs)    ((regs).vex.ARCH_FRAME_PTR)
#define CLREQ_ARGS(regs)   ((regs).vex.ARCH_CLREQ_ARGS)
#define PTHREQ_RET(regs)   ((regs).vex.ARCH_PTHREQ_RET)
#define CLREQ_RET(regs)    ((regs).vex.ARCH_CLREQ_RET)
// Offsets for the Vex state
#define O_STACK_PTR        (offsetof(VexGuestArchState, ARCH_STACK_PTR))
#define O_FRAME_PTR        (offsetof(VexGuestArchState, ARCH_FRAME_PTR))
#define O_CLREQ_RET        (offsetof(VexGuestArchState, ARCH_CLREQ_RET))
#define O_PTHREQ_RET       (offsetof(VexGuestArchState, ARCH_PTHREQ_RET))


// Setting up the initial thread (1) state
extern void 
       VGA_(init_thread1state) ( Addr client_eip, 
                                 Addr esp_at_startup,
                                 /*MOD*/ ThreadArchState* arch );

// Thread stuff
extern void VGA_(cleanup_thread) ( ThreadArchState* );
extern void VGA_(setup_child)    ( ThreadArchState*, ThreadArchState* );

extern void VGA_(set_arg_and_bogus_ret) ( ThreadId tid, UWord arg, Addr ret );
extern void VGA_(thread_initial_stack)  ( ThreadId tid, UWord arg, Addr ret );

// OS/Platform-specific thread clear (after thread exit)
extern void VGA_(os_state_clear)(ThreadState *);

// OS/Platform-specific thread init (at scheduler init time)
extern void VGA_(os_state_init)(ThreadState *);

// Run a thread from beginning to end.  Does not return if tid == VG_(master_tid).
void VGA_(thread_wrapper)(ThreadId tid);

// Like VGA_(thread_wrapper), but it allocates a stack before calling
// to VGA_(thread_wrapper) on that stack, as if it had been set up by
// clone()
void VGA_(main_thread_wrapper)(ThreadId tid) __attribute__ ((__noreturn__));

// Return how many bytes of a thread's Valgrind stack are unused
Int VGA_(stack_unused)(ThreadId tid);

// Terminate the process.  Does not return.
void VGA_(terminate)(ThreadId tid, VgSchedReturnCode src) __attribute__((__noreturn__));

// wait until all other threads are dead
extern void VGA_(reap_threads)(ThreadId self);

// handle an arch-specific client request
extern Bool VGA_(client_request)(ThreadId tid, UWord *args);

// Symtab stuff
extern UInt* VGA_(reg_addr_from_tst) ( Int regno, ThreadArchState* );

// Pointercheck
extern Bool VGA_(setup_pointercheck) ( void );

// For attaching the debugger
extern Int  VGA_(ptrace_setregs_from_tst) ( Int pid, ThreadArchState* arch );

// Used by leakcheck
extern void VGA_(mark_from_registers)(ThreadId tid, void (*marker)(Addr));

// Signal stuff
extern void VGA_(push_signal_frame) ( ThreadId tid, Addr sp_top_of_frame,
                                      const vki_siginfo_t *siginfo,
                                      void *handler, UInt flags,
                                      const vki_sigset_t *mask,
				      void *restorer );

////typedef struct _ThreadArchAux ThreadArchAux;
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

// Accessors for the ThreadArchState
#define SYSCALL_NUM(regs)  ((regs).vex.PLATFORM_SYSCALL_NUM)
#define SYSCALL_ARG1(regs) ((regs).vex.PLATFORM_SYSCALL_ARG1)
#define SYSCALL_ARG2(regs) ((regs).vex.PLATFORM_SYSCALL_ARG2)
#define SYSCALL_ARG3(regs) ((regs).vex.PLATFORM_SYSCALL_ARG3)
#define SYSCALL_ARG4(regs) ((regs).vex.PLATFORM_SYSCALL_ARG4)
#define SYSCALL_ARG5(regs) ((regs).vex.PLATFORM_SYSCALL_ARG5)
#define SYSCALL_ARG6(regs) ((regs).vex.PLATFORM_SYSCALL_ARG6)
#define SYSCALL_RET(regs)  ((regs).vex.PLATFORM_SYSCALL_RET)

// Offsets for the shadow state
#define O_SYSCALL_NUM   (offsetof(VexGuestArchState, PLATFORM_SYSCALL_NUM))
#define O_SYSCALL_ARG1  (offsetof(VexGuestArchState, PLATFORM_SYSCALL_ARG1))
#define O_SYSCALL_ARG2  (offsetof(VexGuestArchState, PLATFORM_SYSCALL_ARG2))
#define O_SYSCALL_ARG3  (offsetof(VexGuestArchState, PLATFORM_SYSCALL_ARG3))
#define O_SYSCALL_ARG4  (offsetof(VexGuestArchState, PLATFORM_SYSCALL_ARG4))
#define O_SYSCALL_ARG5  (offsetof(VexGuestArchState, PLATFORM_SYSCALL_ARG5))
#define O_SYSCALL_ARG6  (offsetof(VexGuestArchState, PLATFORM_SYSCALL_ARG6))
#define O_SYSCALL_RET   (offsetof(VexGuestArchState, PLATFORM_SYSCALL_RET))

struct SyscallTableEntry {
   UInt  *flags_ptr;
   void        (*before)(ThreadId tid, ThreadState *tst /*, UInt *flags*/);
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
   
extern void VGA_(restart_syscall)(ThreadArchState* arch);

/*
  Perform a syscall on behalf of a client thread, using a specific
  signal mask.  On completion, the signal mask is set to restore_mask
  (which presumably blocks almost everything).  If a signal happens
  during the syscall, the handler should call
  VGA_(interrupted_syscall)() to adjust the thread's context to do the
  right thing.
*/
extern void VGA_(client_syscall)(Int syscallno, ThreadState *tst,
				 const vki_sigset_t *syscall_mask);

/*
   Fix up the thread's state because a syscall may have been
   interrupted with a signal.  Returns True if the syscall completed
   (either interrupted or finished normally), or False if it was
   restarted (or the signal didn't actually interrupt a syscall).
 */
extern void VGA_(interrupted_syscall)(ThreadId tid,
                                      struct vki_ucontext *uc,
                                      Bool restart);


///* ---------------------------------------------------------------------
//   Thread modelling
//   ------------------------------------------------------------------ */
//extern void VG_(tm_thread_create)  (ThreadId creator, ThreadId tid, Bool detached);
//extern void VG_(tm_thread_exit)    (ThreadId tid);
//extern Bool VG_(tm_thread_exists)  (ThreadId tid);
//extern void VG_(tm_thread_detach)  (ThreadId tid);
//extern void VG_(tm_thread_join)    (ThreadId joiner, ThreadId joinee);
//extern void VG_(tm_thread_switchto)(ThreadId tid);
//
//extern void VG_(tm_mutex_init)   (ThreadId tid, Addr mutexp);
//extern void VG_(tm_mutex_destroy)(ThreadId tid, Addr mutexp);
//extern void VG_(tm_mutex_trylock)(ThreadId tid, Addr mutexp);
//extern void VG_(tm_mutex_giveup) (ThreadId tid, Addr mutexp);
//extern void VG_(tm_mutex_acquire)(ThreadId tid, Addr mutexp);
//extern void VG_(tm_mutex_tryunlock)(ThreadId tid, Addr mutexp);
//extern void VG_(tm_mutex_unlock) (ThreadId tid, Addr mutexp);
//extern Bool VG_(tm_mutex_exists) (Addr mutexp);
//
//extern UInt VG_(tm_error_update_extra) (Error *err);
//extern Bool VG_(tm_error_equal) (VgRes res, Error *e1, Error *e2);
//extern void VG_(tm_error_print) (Error *err);
//
//extern void VG_(tm_init) ();
//
//extern void VG_(tm_cond_init)    (ThreadId tid, Addr condp);
//extern void VG_(tm_cond_destroy) (ThreadId tid, Addr condp);
//extern void VG_(tm_cond_wait)    (ThreadId tid, Addr condp, Addr mutexp);
//extern void VG_(tm_cond_wakeup)  (ThreadId tid, Addr condp, Addr mutexp);
//extern void VG_(tm_cond_signal)  (ThreadId tid, Addr condp);
//
///* ----- pthreads ----- */
//extern void VG_(pthread_init)      ();
//extern void VG_(pthread_startfunc_wrapper)(Addr wrapper);
//
//struct vg_pthread_newthread_data {
//   void	*(*startfunc)(void *arg);
//   void *arg;
//};

/* ---------------------------------------------------------------------
   Finally - autoconf-generated settings
   ------------------------------------------------------------------ */

#include "config.h"

#endif /* ndef __CORE_H */

/*--------------------------------------------------------------------*/
/*--- end                                                          ---*/
/*--------------------------------------------------------------------*/
