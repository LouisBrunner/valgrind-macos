
/*--------------------------------------------------------------------*/
/*--- A header file for all private parts of Valgrind's core.      ---*/
/*--- Include no other! (more or less...)                          ---*/
/*---                                                       core.h ---*/
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

#ifndef __CORE_H
#define __CORE_H

/*
   [This comment is not longer accurate -- we're switching to an easier to
   understand module-based approach, with one or two headers per module,
   rather than having monolithic headers as described.  --njn 07-May-2005]
   
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

#include "pub_core_stacktrace.h"  // for type 'StackTrace'

#include "valgrind.h"

/* ---------------------------------------------------------------------
   Global macros.
   ------------------------------------------------------------------ */

/* Max length of a text fragment used to construct error messages. */
#define VG_ERRTXT_LEN 4096

/* The maximum number of calls we're prepared to save in a
   backtrace. */
#define VG_DEEPEST_BACKTRACE 50

/* Useful macros */
/* a - alignment - must be a power of 2 */
#define ROUNDDN(p, a)	((Addr)(p) & ~((Addr)(a)-1))
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
/* DEBUG: print call-frame-info details?  default: NO */
extern Bool  VG_(clo_trace_cfi);
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

/* Should we show VEX emulation warnings?  Default: NO */
extern Bool VG_(clo_show_emwarns);

/* How much does the stack pointer have to change before tools
   consider a stack switch to have happened?  Default: 2000000 bytes */
extern Int VG_(clo_max_stackframe);

/* Set up the libc freeres wrapper */
extern void VGA_(intercept_libc_freeres_wrapper)(Addr);

// Clean up the client by calling before the final reports
extern void VGA_(final_tidyup)(ThreadId tid);

// Arch-specific client requests
extern Bool VGA_(client_requests)(ThreadId tid, UWord *args);

/* ---------------------------------------------------------------------
   Profiling stuff
   ------------------------------------------------------------------ */

extern void VG_(init_profiling) ( void );
extern void VG_(done_profiling) ( void );

#undef  VGP_PUSHCC
#undef  VGP_POPCC
#define VGP_PUSHCC(x)   if (VG_(clo_profile)) VG_(pushcc)(x)
#define VGP_POPCC(x)    if (VG_(clo_profile)) VG_(popcc)(x)


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

typedef struct {
   // ---------------------------------------------
   // 'Needs' and related functions
   // ---------------------------------------------
   // Basic functions
   void  (*tool_pre_clo_init) (void);
   void  (*tool_post_clo_init)(void);
   IRBB* (*tool_instrument)   (IRBB*, VexGuestLayout*, IRType, IRType);
   void  (*tool_fini)         (Int);

   // VG_(needs).core_errors
   // (none)
   
   // VG_(needs).tool_errors
   Bool  (*tool_eq_Error)                    (VgRes, Error*, Error*);
   void  (*tool_pp_Error)                    (Error*);
   UInt  (*tool_update_extra)                (Error*);
   Bool  (*tool_recognised_suppression)      (Char*, Supp*);
   Bool  (*tool_read_extra_suppression_info) (Int, Char*, Int, Supp*);
   Bool  (*tool_error_matches_suppression)   (Error*, Supp*);
   Char* (*tool_get_error_name)              (Error*);
   void  (*tool_print_extra_suppression_info)(Error*);

   // VG_(needs).basic_block_discards
   void (*tool_discard_basic_block_info)(Addr, SizeT);

   // VG_(needs).command_line_options
   Bool (*tool_process_cmd_line_option)(Char*);
   void (*tool_print_usage)            (void);
   void (*tool_print_debug_usage)      (void);

   // VG_(needs).client_requests
   Bool (*tool_handle_client_request)(ThreadId, UWord*, UWord*);

   // VG_(needs).syscall_wrapper
   void (*tool_pre_syscall) (ThreadId, UInt);
   void (*tool_post_syscall)(ThreadId, UInt, Int);

   // VG_(needs).sanity_checks
   Bool (*tool_cheap_sanity_check)(void);
   Bool (*tool_expensive_sanity_check)(void);

   // ---------------------------------------------
   // Event tracking functions
   // ---------------------------------------------
   void (*track_new_mem_startup)     (Addr, SizeT, Bool, Bool, Bool);
   void (*track_new_mem_stack_signal)(Addr, SizeT);
   void (*track_new_mem_brk)         (Addr, SizeT);
   void (*track_new_mem_mmap)        (Addr, SizeT, Bool, Bool, Bool);

   void (*track_copy_mem_remap)      (Addr, Addr, SizeT);
   void (*track_change_mem_mprotect) (Addr, SizeT, Bool, Bool, Bool);
   void (*track_die_mem_stack_signal)(Addr, SizeT);
   void (*track_die_mem_brk)         (Addr, SizeT);
   void (*track_die_mem_munmap)      (Addr, SizeT);

   VGA_REGPARM(1) void (*track_new_mem_stack_4) (Addr);
   VGA_REGPARM(1) void (*track_new_mem_stack_8) (Addr);
   VGA_REGPARM(1) void (*track_new_mem_stack_12)(Addr);
   VGA_REGPARM(1) void (*track_new_mem_stack_16)(Addr);
   VGA_REGPARM(1) void (*track_new_mem_stack_32)(Addr);
   void (*track_new_mem_stack)(Addr, SizeT);

   VGA_REGPARM(1) void (*track_die_mem_stack_4) (Addr);
   VGA_REGPARM(1) void (*track_die_mem_stack_8) (Addr);
   VGA_REGPARM(1) void (*track_die_mem_stack_12)(Addr);
   VGA_REGPARM(1) void (*track_die_mem_stack_16)(Addr);
   VGA_REGPARM(1) void (*track_die_mem_stack_32)(Addr);
   void (*track_die_mem_stack)(Addr, SizeT);

   void (*track_ban_mem_stack)(Addr, SizeT);

   void (*track_pre_mem_read)       (CorePart, ThreadId, Char*, Addr, SizeT);
   void (*track_pre_mem_read_asciiz)(CorePart, ThreadId, Char*, Addr);
   void (*track_pre_mem_write)      (CorePart, ThreadId, Char*, Addr, SizeT);
   void (*track_post_mem_write)     (CorePart, ThreadId, Addr, SizeT);

   void (*track_pre_reg_read)  (CorePart, ThreadId, Char*, OffT, SizeT);
   void (*track_post_reg_write)(CorePart, ThreadId,        OffT, SizeT);
   void (*track_post_reg_write_clientcall_return)(ThreadId, OffT, SizeT, Addr);

   void (*track_thread_run)(ThreadId);

   void (*track_post_thread_create)(ThreadId, ThreadId);
   void (*track_post_thread_join)  (ThreadId, ThreadId);

   void (*track_pre_mutex_lock)   (ThreadId, void*);
   void (*track_post_mutex_lock)  (ThreadId, void*);
   void (*track_post_mutex_unlock)(ThreadId, void*);

   void (*track_pre_deliver_signal) (ThreadId, Int sigNo, Bool);
   void (*track_post_deliver_signal)(ThreadId, Int sigNo);

   void (*track_init_shadow_page)(Addr);

   // ---------------------------------------------
   // malloc/free replacements
   // ---------------------------------------------
   void* (*malloc_malloc)              (ThreadId, SizeT);
   void* (*malloc___builtin_new)       (ThreadId, SizeT);
   void* (*malloc___builtin_vec_new)   (ThreadId, SizeT);
   void* (*malloc_memalign)            (ThreadId, SizeT, SizeT);
   void* (*malloc_calloc)              (ThreadId, SizeT, SizeT);
   void  (*malloc_free)                (ThreadId, void*);
   void  (*malloc___builtin_delete)    (ThreadId, void*);
   void  (*malloc___builtin_vec_delete)(ThreadId, void*);
   void* (*malloc_realloc)             (ThreadId, void*, SizeT);

} VgToolInterface;

extern VgToolInterface VG_(tdict);



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
      CLIENT    for the client's mallocs/frees, if the tool replaces glibc's
                    malloc() et al -- redzone size is chosen by the tool.
      DEMANGLE  for the C++ demangler.
      EXECTXT   for storing ExeContexts.
      ERRORS    for storing CoreErrors.

   When adding a new arena, remember also to add it to ensure_mm_init(). 
*/
typedef Int ArenaId;

#define VG_N_ARENAS        7

#define VG_AR_CORE         0
#define VG_AR_TOOL         1
#define VG_AR_SYMTAB       2
#define VG_AR_CLIENT       3
#define VG_AR_DEMANGLE     4
#define VG_AR_EXECTXT      5
#define VG_AR_ERRORS       6

// This is both the minimum payload size of a malloc'd block, and its
// minimum alignment.  Must be a power of 2 greater than 4, and should be
// greater than 8.
#define VG_MIN_MALLOC_SZB        8

extern void* VG_(arena_malloc)  ( ArenaId arena, SizeT nbytes );
extern void  VG_(arena_free)    ( ArenaId arena, void* ptr );
extern void* VG_(arena_calloc)  ( ArenaId arena, 
                                  SizeT nmemb, SizeT bytes_per_memb );
extern void* VG_(arena_realloc) ( ArenaId arena, void* ptr, SizeT size );

/* Sets the size of the redzones at the start and end of heap blocks.  This
   must be called before any of VG_(malloc) and friends are called. */
extern void  VG_(set_client_malloc_redzone_szB) ( SizeT rz_szB );

extern SizeT VG_(arena_payload_szB) ( ArenaId aid, void* payload );

extern void  VG_(sanity_check_malloc_all) ( void );

extern void  VG_(print_all_arena_stats) ( void );


/* ---------------------------------------------------------------------
   Exports of vg_intercept.c
   ------------------------------------------------------------------ */

/* This doesn't export code or data that valgrind.so needs to link
   against.  However, the scheduler does need to know the following
   request codes.  A few, publically-visible, request codes are also
   defined in valgrind.h, and similar headers for some tools. */

/* Obsolete pthread-related requests */
#define VG_USERREQ__MALLOC                  0x2001
#define VG_USERREQ__FREE                    0x2002
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
#define VG_USERREQ__READ_MILLISECOND_TIMER  0x3017
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


/* Internal equivalent of VALGRIND_PRINTF . */
#define VG_USERREQ__INTERNAL_PRINTF         0x3103
/* Internal equivalent of VALGRIND_PRINTF_BACKTRACE . (no longer used) */
//#define VG_USERREQ__INTERNAL_PRINTF_BACKTRACE 0x3104

/* Denote the finish of __libc_freeres_wrapper(). 
   A synonym for exit. */
#define VG_USERREQ__LIBC_FREERES_DONE       0x3029

/* Intercept prefix stuff.  See coregrind/vg_replace_malloc.c for
   details.  Unfortunately the "_vgi_" literal is also hardcoded in
   that file, so if you change this one you must also change the other
   one. */
#define VG_INTERCEPT_PREFIX "_vgi_"
#define VG_INTERCEPT_PREFIX_LEN 5

/* Not sure what these are for.  Todo: clarify */
#define VG_WRAPPER_PREFIX "_vgw_"
#define VG_WRAPPER_PREFIX_LEN 5
#define VG_WRAPPER(name) _vgw_##name
#define VG_WRAPPER_ALIAS(name) "_vgw_" #name


struct vg_mallocfunc_info {
   /* things vg_replace_malloc.o needs to know about */
   void* (*tl_malloc)              (ThreadId tid, SizeT n);
   void* (*tl___builtin_new)       (ThreadId tid, SizeT n);
   void* (*tl___builtin_vec_new)   (ThreadId tid, SizeT n);
   void* (*tl_memalign)            (ThreadId tid, SizeT align, SizeT n);
   void* (*tl_calloc)              (ThreadId tid, SizeT nmemb, SizeT n);
   void  (*tl_free)                (ThreadId tid, void* p);
   void  (*tl___builtin_delete)    (ThreadId tid, void* p);
   void  (*tl___builtin_vec_delete)(ThreadId tid, void* p);
   void* (*tl_realloc)             (ThreadId tid, void* p, SizeT size);

   SizeT (*arena_payload_szB)      (ArenaId aid, void* payload);

   Bool	clo_trace_malloc;
};


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

   /* Client stacks.  When a thread slot is freed, we don't deallocate its
      stack; we just leave it lying around for the next use of the
      slot.  If the next use of the slot requires a larger stack,
      only then is the old one deallocated and a new one
      allocated. 

      For the main thread (threadid == 0), this mechanism doesn't
      apply.  We don't know the size of the stack since we didn't
      allocate it, and furthermore we never reallocate it. */

   /* The allocated size of this thread's stack (permanently zero
      if this is ThreadId == 0, since we didn't allocate its stack) */
   SizeT client_stack_szB;

   /* Address of the highest legitimate word in this stack.  This is
      used for error messages only -- not critical for execution
      correctness.  Is is set for all stacks, specifically including
      ThreadId == 0 (the main thread). */
   Addr client_stack_highest_word;

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
};

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

/* ---------------------------------------------------------------------
   Exports of vg_signals.c
   ------------------------------------------------------------------ */

/* Set the standard set of blocked signals, used wheneever we're not
   running a client syscall. */
extern void VG_(block_signals)(ThreadId tid);

/* Highest signal the kernel will let us use */
extern Int VG_(max_signal);

extern void VG_(sigstartup_actions) ( void );

extern Bool VG_(is_sig_ign) ( Int sigNo );

/* Poll a thread's set of pending signals, and update the Thread's context to deliver one */
extern void VG_(poll_signals) ( ThreadId );

/* Fake system calls for signal handling. */
extern Int VG_(do_sys_sigaltstack) ( ThreadId tid, vki_stack_t* ss,
                                                   vki_stack_t* oss );
extern Int VG_(do_sys_sigaction)   ( Int signo, 
                                     const struct vki_sigaction *new_act, 
                                     struct vki_sigaction *old_act );
extern Int VG_(do_sys_sigprocmask) ( ThreadId tid, Int how, 
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
#define I_die_here                                             \
   VG_(assert_fail) ("Unimplemented functionality",            \
                     __FILE__, __LINE__, __PRETTY_FUNCTION__,  \
                     "valgrind", VG_BUGS_TO, "")

#define vg_assert(expr)                                                 \
  ((void) ((expr) ? 0 :                                                 \
           (VG_(assert_fail) (/*isCore*/True, VG_STRINGIFY(expr),       \
                              __FILE__, __LINE__, __PRETTY_FUNCTION__,  \
                              ""),                                      \
                              0)))

#define vg_assert2(expr, format, args...)                               \
  ((void) ((expr) ? 0 :                                                 \
           (VG_(assert_fail) (/*isCore*/True, VG_STRINGIFY(expr),       \
                              __FILE__, __LINE__, __PRETTY_FUNCTION__,  \
                              format, ##args),                          \
                              0)))

__attribute__ ((__noreturn__))
extern void  VG_(core_panic)      ( Char* str );
__attribute__ ((__noreturn__))
extern void  VG_(core_panic_at)   ( Char* str, StackTrace ips );

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
   Exports of vg_symtab2.c
   ------------------------------------------------------------------ */

typedef struct _Segment Segment;
typedef struct _CodeRedirect CodeRedirect;

extern Bool VG_(is_object_file)   ( const void *hdr );
extern SegInfo * VG_(read_seg_symbols) ( Segment *seg );
extern void VG_(symtab_incref)	  ( SegInfo * );
extern void VG_(symtab_decref)	  ( SegInfo *, Addr a );

extern Bool VG_(get_fnname_nodemangle)( Addr a, Char* fnname, Int n_fnname );

extern Addr VG_(reverse_search_one_symtab) ( const SegInfo* si, const Char* name );

/* Set up some default redirects */
extern void VG_(setup_code_redirect_table) ( void );

extern Bool VG_(resolve_redir_allsegs)(CodeRedirect *redir);

extern Bool VG_(use_CFI_info) ( /*MOD*/Addr* ipP,
                                /*MOD*/Addr* spP,
                                /*MOD*/Addr* fpP,
                                Addr min_accessible,
                                Addr max_accessible );


/* ---------------------------------------------------------------------
   Exports of vg_redir.c
   ------------------------------------------------------------------ */
/* Redirection machinery */
extern Addr VG_(code_redirect) ( Addr orig );

extern void VG_(add_redirect_sym_to_addr)(const Char *from_lib,
					  const Char *from_sym,
					  Addr to_addr);
extern void VG_(add_redirect_addr_to_addr)(Addr from_addr, Addr to_addr);
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

/* Stats ... */
extern void VG_(print_scheduler_stats) ( void );

/* Indicates what arch and subarch we are running on. */
extern VexArch    VG_(vex_arch);
extern VexSubArch VG_(vex_subarch);

/* 64-bit counter for the number of basic blocks done. */
extern ULong VG_(bbs_done);


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
extern UWord VG_(run_innerloop) ( void* guest_state );

/* ---------------------------------------------------------------------
   Exports of vg_helpers.S
   ------------------------------------------------------------------ */

/* Information about trampoline code (for signal return and syscalls) */
extern const Char VG_(trampoline_code_start);
extern const Int  VG_(trampoline_code_length);
extern const Int  VG_(tramp_sigreturn_offset);
extern const Int  VG_(tramp_rt_sigreturn_offset);
extern const Int  VG_(tramp_syscall_offset);
extern const Int  VG_(tramp_gettimeofday_offset);
extern const Int  VG_(tramp_time_offset);

/* ---------------------------------------------------------------------
   Things relating to the used tool
   ------------------------------------------------------------------ */

// Note the use of C's comma operator here -- it means that we execute both
// statements, and the rvalue of the whole thing is the rvalue of the last
// statement.  This lets us say "x = VG_TDICT_CALL(...)" in the required
// places, while still checking the assertion.
#define VG_TDICT_CALL(fn, args...) \
   ( tl_assert2(VG_(tdict).fn, \
                "you forgot to set VgToolInterface function '" #fn "'"), \
     VG_(tdict).fn(args) )

#define VG_TRACK(fn, args...) 			\
   do {						\
      if (VG_(tdict).track_##fn)		\
	 VG_(tdict).track_##fn(args);           \
   } while(0)

// ---------------------------------------------------------------------
// Architecture-specific things defined in eg. x86/*.c
// ---------------------------------------------------------------------

// Returns the architecture and subarchitecture, or indicates
// that this subarchitecture is unable to run Valgrind
// Returns False to indicate we cannot proceed further.
extern Bool VGA_(getArchAndSubArch)( /*OUT*/VexArch*, 
                                     /*OUT*/VexSubArch* );
// Accessors for the ThreadArchState
#define INSTR_PTR(regs)    ((regs).vex.VGA_INSTR_PTR)
#define STACK_PTR(regs)    ((regs).vex.VGA_STACK_PTR)
#define FRAME_PTR(regs)    ((regs).vex.VGA_FRAME_PTR)
#define CLREQ_ARGS(regs)   ((regs).vex.VGA_CLREQ_ARGS)
#define CLREQ_RET(regs)    ((regs).vex.VGA_CLREQ_RET)
// Offsets for the Vex state
#define O_STACK_PTR        (offsetof(VexGuestArchState, VGA_STACK_PTR))
#define O_FRAME_PTR        (offsetof(VexGuestArchState, VGA_FRAME_PTR))
#define O_CLREQ_RET        (offsetof(VexGuestArchState, VGA_CLREQ_RET))


// Setting up the initial thread (1) state
extern void 
       VGA_(init_thread1state) ( Addr client_eip, 
                                 Addr esp_at_startup,
                                 /*MOD*/ ThreadArchState* arch );

// Thread stuff
extern void VGA_(cleanup_thread) ( ThreadArchState* );
extern void VGA_(setup_child)    ( ThreadArchState*, ThreadArchState* );

// OS/Platform-specific thread clear (after thread exit)
extern void VGA_(os_state_clear)(ThreadState *);

// OS/Platform-specific thread init (at scheduler init time)
extern void VGA_(os_state_init)(ThreadState *);

// Run a thread from beginning to end.  Does not return if tid == VG_(master_tid).
void VGA_(thread_wrapper)(Word /*ThreadId*/ tid);

// Like VGA_(thread_wrapper), but it allocates a stack before calling
// to VGA_(thread_wrapper) on that stack, as if it had been set up by
// clone()
void VGA_(main_thread_wrapper)(ThreadId tid) __attribute__ ((__noreturn__));

// Return how many bytes of a thread's Valgrind stack are unused
SSizeT VGA_(stack_unused)(ThreadId tid);

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

////typedef struct _ThreadArchAux ThreadArchAux;


// ---------------------------------------------------------------------
// Platform-specific things defined in eg. x86/*.c
// ---------------------------------------------------------------------

// Do any platform specific redirects.
extern void VGP_(setup_redirects)(void);

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
