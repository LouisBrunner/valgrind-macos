
/*--------------------------------------------------------------------*/
/*--- A header file for all private parts of Valgrind's core.      ---*/
/*--- Include no other!                                            ---*/
/*---                                                 vg_include.h ---*/
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

#ifndef __VG_INCLUDE_H
#define __VG_INCLUDE_H

/* ---------------------------------------------------------------------
   Build options and table sizes.  You should be able to change these
   options or sizes, recompile, and still have a working system.
   ------------------------------------------------------------------ */

/* For system call numbers __NR_... */
#include "vg_unistd.h"

#include "vg_constants.h"

/* All stuff visible to core and skins goes in vg_skin.h.  Things
 * visible to core but not visible to any skins should go in this
 * file, vg_include.h. */
#include "vg_skin.h"
#include "valgrind.h"

#undef SK_
#define SK_(x)	vgSkinInternal_##x

/* Total number of spill slots available for allocation, if a TempReg
   doesn't make it into a RealReg.  Just bomb the entire system if
   this value is too small; we don't expect it will ever get
   particularly high. */
#define VG_MAX_SPILLSLOTS 24


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
#define M_VG_ERRTXT 512

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

/* Number of entries in the semaphore-remapping table. */
#define VG_N_SEMAPHORES 50

/* Number of entries in the rwlock-remapping table. */
#define VG_N_RWLOCKS 500

/* Number of entries in each thread's cleanup stack. */
#define VG_N_CLEANUPSTACK 16

/* Number of entries in each thread's fork-handler stack. */
#define VG_N_FORKHANDLERSTACK 4

/* Max number of callers for context in a suppression. */
#define VG_N_SUPP_CALLERS  4

/* Valgrind's stack sizes, in words */
#define VG_STACK_SIZE_W       10000
#define VG_SIGSTACK_SIZE_W    10000

/* Useful macros */
/* a - alignment - must be a power of 2 */
#define ROUNDDN(p, a)	((Addr)(p) & ~((a)-1))
#define ROUNDUP(p, a)	ROUNDDN((p)+(a)-1, (a))
#define PGROUNDDN(p)	ROUNDDN(p, VKI_BYTES_PER_PAGE)
#define PGROUNDUP(p)	ROUNDUP(p, VKI_BYTES_PER_PAGE)


/* ---------------------------------------------------------------------
   Basic types
   ------------------------------------------------------------------ */

/* Just pray that gcc's constant folding works properly ... */
#define BITS(bit7,bit6,bit5,bit4,bit3,bit2,bit1,bit0)               \
   ( ((bit7) << 7) | ((bit6) << 6) | ((bit5) << 5) | ((bit4) << 4)  \
     | ((bit3) << 3) | ((bit2) << 2) | ((bit1) << 1) | (bit0))

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

/* Maximum allowed application-visible file descriptor */
extern Int VG_(max_fd);

/* Should we stop collecting errors if too many appear?  default: YES */
extern Bool  VG_(clo_error_limit);
/* Enquire about whether to attach to a debugger at errors?   default: NO */
extern Bool  VG_(clo_db_attach);
/* The debugger command?  default: whatever gdb ./configure found */
extern Char* VG_(clo_db_command);
/* Enquire about generating a suppression for each error?   default: NO */
extern Bool  VG_(clo_gen_suppressions);
/* Sanity-check level: 0 = none, 1 (default), > 1 = expensive. */
extern Int   VG_(sanity_level);
/* Automatically attempt to demangle C++ names?  default: YES */
extern Bool  VG_(clo_demangle);
/* Simulate child processes? default: NO */
extern Bool  VG_(clo_trace_children);

/* Where logging output is to be sent to.

   When log_to == VgLogTo_Fd, clo_logfile_fd holds the file id, and is
   taken from the command line.  clo_logfile_name is irrelevant.

   When log_to == VgLogTo_File, clo_logfile_name holds the logfile
   name, and is taken from the command line.  clo_logfile_fd is then
   made to hold the relevant file id, by opening clo_logfile_name
   (concatenated with the process ID) for writing.

   When log_to == VgLogTo_Socket, clo_logfile_name holds the
   hostname:portnumber pair, and is taken from the command line.
   clo_logfile_fd is then made to hold the relevant file handle, by
   opening a connection to said hostname:portnumber pair. 

   Global default is to set log_to == VgLogTo_Fd and logfile_fd == 2
   (stderr). */
extern VgLogTo VG_(clo_log_to);
extern Int     VG_(clo_logfile_fd);
extern Char*   VG_(clo_logfile_name);

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
/* Stop after this many basic blocks.  default: Infinity. */
extern ULong VG_(clo_stop_after);
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
   is ignored.  Ie if a skin says no, I don't want this to run, that
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

/* ---------------------------------------------------------------------
   Debugging and profiling stuff
   ------------------------------------------------------------------ */

/* Create a logfile into which messages can be dumped. */
extern void VG_(startup_logging) ( void );
extern void VG_(shutdown_logging)( void );

extern void VGP_(init_profiling) ( void );
extern void VGP_(done_profiling) ( void );

#undef  VGP_PUSHCC
#undef  VGP_POPCC
#define VGP_PUSHCC(x)   if (VG_(clo_profile)) VGP_(pushcc)(x)
#define VGP_POPCC(x)    if (VG_(clo_profile)) VGP_(popcc)(x)

/* ---------------------------------------------------------------------
   Skin-related types
   ------------------------------------------------------------------ */
/* These structs are not exposed to skins to mitigate possibility of
   binary-incompatibilities when the core/skin interface changes.  Instead,
   set functions are provided (see include/vg_skin.h). */
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

      Bool skin_errors;
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
      CLIENT    for the client's mallocs/frees, if the skin replaces glibc's
                    malloc() et al -- redzone size is chosen by the skin.
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

extern void* VG_(arena_malloc)  ( ArenaId arena, Int nbytes );
extern void  VG_(arena_free)    ( ArenaId arena, void* ptr );
extern void* VG_(arena_calloc)  ( ArenaId arena, Int alignment,
                                  Int nmemb, Int nbytes );
extern void* VG_(arena_realloc) ( ArenaId arena, void* ptr, Int alignment,
                                  Int size );
extern void* VG_(arena_malloc_aligned) ( ArenaId aid, Int req_alignB, 
                                                Int req_pszB );

extern Int   VG_(arena_payload_szB) ( ArenaId aid, void* payload );

extern void  VG_(mallocSanityCheckAll)   ( void );

extern void  VG_(show_all_arena_stats) ( void );
extern Bool  VG_(is_empty_arena) ( ArenaId aid );

/* Returns True if aa is inside any block mmap'd /dev/zero
   by our low-level memory manager. */
extern Bool  VG_(is_inside_segment_mmapd_by_low_level_MM)( Addr aa );


/* ---------------------------------------------------------------------
   Exports of vg_intercept.c
   ------------------------------------------------------------------ */

/* This doesn't export code or data that valgrind.so needs to link
   against.  However, the scheduler does need to know the following
   request codes.  A few, publically-visible, request codes are also
   defined in valgrind.h, and similar headers for some skins. */

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

#define VG_USERREQ__PTHREAD_GET_THREADID    0x300B
#define VG_USERREQ__PTHREAD_MUTEX_LOCK      0x300C
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

/* Denote the finish of VG_(__libc_freeres_wrapper). */
#define VG_USERREQ__LIBC_FREERES_DONE       0x3029
#define VG_USERREQ__REGISTER_LIBC_FREERES   0x302A

/* Allocate RT signals */
#define VG_USERREQ__GET_SIGRT_MIN	    0x302B
#define VG_USERREQ__GET_SIGRT_MAX	    0x302C
#define VG_USERREQ__ALLOC_RTSIG		    0x302D

/* Hook for replace_malloc.o to get malloc functions */
#define VG_USERREQ__GET_MALLOCFUNCS	    0x3030

/* Hook for interface to vg_inject.so */
#define VG_USERREQ__REGISTER_REDIRECT_SYM   0x3031
#define VG_USERREQ__REGISTER_REDIRECT_ADDR  0x3032

/* Cosmetic ... */
#define VG_USERREQ__GET_PTHREAD_TRACE_LEVEL 0x3101
/* Log a pthread error from client-space.  Cosmetic. */
#define VG_USERREQ__PTHREAD_ERROR           0x3102
/* Internal equivalent of VALGRIND_PRINTF . */
#define VG_USERREQ__INTERNAL_PRINTF         0x3103
/* Internal equivalent of VALGRIND_PRINTF_BACKTRACE . */
#define VG_USERREQ__INTERNAL_PRINTF_BACKTRACE 0x3104

/* 
In vg_constants.h:
#define VG_USERREQ__SIGNAL_RETURNS          0x4001
*/


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

__attribute__((weak))
int
VALGRIND_INTERNAL_PRINTF(char *format, ...)
{
   unsigned int _qzz_res = 0;
   va_list vargs;
   va_start(vargs, format);
   VALGRIND_MAGIC_SEQUENCE(_qzz_res, 0, VG_USERREQ__INTERNAL_PRINTF,
                           (unsigned int)format, (unsigned int)vargs, 0, 0);
   va_end(vargs);
   return _qzz_res;
}

__attribute__((weak))
int
VALGRIND_INTERNAL_PRINTF_BACKTRACE(char *format, ...)
{
   unsigned int _qzz_res = 0;
   va_list vargs;
   va_start(vargs, format);
   VALGRIND_MAGIC_SEQUENCE(_qzz_res, 0, VG_USERREQ__INTERNAL_PRINTF_BACKTRACE,
                           (unsigned int)format, (unsigned int)vargs, 0, 0);
   va_end(vargs);
   return _qzz_res;
}


/* ---------------------------------------------------------------------
   Constants pertaining to the simulated CPU state, VG_(baseBlock),
   which need to go here to avoid ugly circularities.
   ------------------------------------------------------------------ */

/* How big is the saved SSE/SSE2 state?  Note that this subsumes the
   FPU state.  On machines without SSE, we just save/restore the FPU
   state into the first part of this area. */
/* A general comment about SSE save/restore: It appears that the 7th
   word (which is the MXCSR) has to be &ed with 0x0000FFBF in order
   that restoring from it later does not cause a GP fault (which is
   delivered as a segfault).  I guess this will have to be done
   any time we do fxsave :-(  7th word means word offset 6 or byte
   offset 24 from the start address of the save area.
 */
#define VG_SIZE_OF_SSESTATE 512
/* ... and in words ... */
#define VG_SIZE_OF_SSESTATE_W ((VG_SIZE_OF_SSESTATE+3)/4)


/* ---------------------------------------------------------------------
   Exports of vg_defaults.c
   ------------------------------------------------------------------ */

extern Bool VG_(sk_malloc_called_by_scheduler);


/* ---------------------------------------------------------------------
   Exports of vg_ldt.c
   ------------------------------------------------------------------ */

/* This is the hardware-format for a segment descriptor, ie what the
   x86 actually deals with.  It is 8 bytes long.  It's ugly.  */

typedef struct _LDT_ENTRY {
    union {
       struct {
          UShort      LimitLow;
          UShort      BaseLow;
          unsigned    BaseMid         : 8;
          unsigned    Type            : 5;
          unsigned    Dpl             : 2;
          unsigned    Pres            : 1;
          unsigned    LimitHi         : 4;
          unsigned    Sys             : 1;
          unsigned    Reserved_0      : 1;
          unsigned    Default_Big     : 1;
          unsigned    Granularity     : 1;
          unsigned    BaseHi          : 8;
       } Bits;
       struct {
          UInt word1;
          UInt word2;
       } Words;
    } 
    LdtEnt;
} VgLdtEntry;

/* Maximum number of LDT entries supported (by the x86). */
#define VG_M_LDT_ENTRIES     8192
/* The size of each LDT entry == sizeof(VgLdtEntry) */
#define VG_LDT_ENTRY_SIZE  8

/* Alloc & copy, and dealloc. */
extern VgLdtEntry* 
         VG_(allocate_LDT_for_thread) ( VgLdtEntry* parent_ldt );
extern void       
         VG_(deallocate_LDT_for_thread) ( VgLdtEntry* ldt );
extern void       
         VG_(clear_TLS_for_thread) ( VgLdtEntry* tls );

/* Simulate the modify_ldt syscall. */
extern Int VG_(sys_modify_ldt) ( ThreadId tid,
                                 Int func, void* ptr, UInt bytecount );

/* Simulate the {get,set}_thread_area syscalls. */
extern Int VG_(sys_set_thread_area) ( ThreadId tid,
                                      struct vki_modify_ldt_ldt_s* info );
extern Int VG_(sys_get_thread_area) ( ThreadId tid,
                                      struct vki_modify_ldt_ldt_s* info );

/* Called from generated code.  Given a segment selector and a virtual
   address, return a linear address, and do limit checks too. */
extern Addr VG_(do_useseg) ( UInt seg_selector, Addr virtual_addr );


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

/* An entry in a threads's cleanup stack. */
typedef
   struct {
      void (*fn)(void*);
      void* arg;
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

typedef
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
   void* /*pthread_mutex_t* */ associated_mx;

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
   vki_ksigset_t sig_mask;

   /* Effective signal mask.  This is the mask which currently
      applies; it may be different from sig_mask while a signal
      handler is running.
    */
   vki_ksigset_t eff_sig_mask;

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

   /* Pointer to this thread's Local (Segment) Descriptor Table.
      Starts out as NULL, indicating there is no table, and we hope to
      keep it that way.  If the thread does __NR_modify_ldt to create
      entries, we allocate a 8192-entry table at that point.  This is
      a straight copy of the Linux kernel's scheme.  Don't forget to
      deallocate this at thread exit. */
   VgLdtEntry* ldt;

   /* TLS table. This consists of a small number (currently 3) of
      entries from the Global Descriptor Table. */
   VgLdtEntry tls[VKI_GDT_TLS_ENTRIES];

   /* Saved machine context.  Note the FPU state, %EIP and segment
      registers are not shadowed.

      Although the segment registers are 16 bits long, storage
      management here, in VG_(baseBlock) and in VG_(m_state_static) is
      simplified if we pretend they are 32 bits. */
   UInt m_cs;
   UInt m_ss;
   UInt m_ds;
   UInt m_es;
   UInt m_fs;
   UInt m_gs;

   UInt m_eax;
   UInt m_ebx;
   UInt m_ecx;
   UInt m_edx;
   UInt m_esi;
   UInt m_edi;
   UInt m_ebp;
   UInt m_esp;
   UInt m_eflags;
   UInt m_eip;

   /* The SSE/FPU state.  This array does not (necessarily) have the
      required 16-byte alignment required to get stuff in/out by
      fxsave/fxrestore.  So we have to do it "by hand".
   */
   UInt m_sse[VG_SIZE_OF_SSESTATE_W];

   UInt sh_eax;
   UInt sh_ebx;
   UInt sh_ecx;
   UInt sh_edx;
   UInt sh_esi;
   UInt sh_edi;
   UInt sh_ebp;
   UInt sh_esp;
   UInt sh_eflags;
} 
ThreadState;


/* The thread table. */
extern ThreadState VG_(threads)[VG_N_THREADS];

/* Check that tid is in range and denotes a non-Empty thread. */
extern Bool VG_(is_valid_tid) ( ThreadId tid );

/* Check that tid is in range. */
extern Bool VG_(is_valid_or_empty_tid) ( ThreadId tid );

/* Determine if 'tid' is that of the current running thread (Nb: returns
   False if no thread is currently running. */
extern Bool VG_(is_running_thread)(ThreadId tid);

/* Get the ThreadState for a particular thread */
extern ThreadState *VG_(get_ThreadState)(ThreadId tid);

/* Copy the specified thread's state into VG_(baseBlock) in
   preparation for running it. */
extern void VG_(load_thread_state)( ThreadId );

/* Save the specified thread's state back in VG_(baseBlock), and fill
   VG_(baseBlock) with junk, for sanity-check reasons. */
extern void VG_(save_thread_state)( ThreadId );

/* And for the currently running one, if valid. */
extern ThreadState* VG_(get_current_thread_state) ( void );

/* Similarly ... */
extern ThreadId VG_(get_current_tid) ( void );

/* Nuke all threads except tid. */
extern void VG_(nuke_all_threads_except) ( ThreadId me );

/* Give a hint to the scheduler that it may be a good time to find a
   new runnable thread.  If prefer_sched != VG_INVALID_THREADID, then
   try to schedule that thread.
*/
extern void VG_(need_resched) ( ThreadId prefer_sched );

/* Add a new timeout event for a thread*/
extern void VG_(add_timeout) ( ThreadId tid, UInt time );

/* Return codes from the scheduler. */
typedef
   enum { 
      VgSrc_Deadlock,    /* no runnable threads and no prospect of any
                            even if we wait for a long time */
      VgSrc_ExitSyscall, /* client called exit().  This is the normal
                            route out. */
      VgSrc_BbsDone,     /* In a debugging run, the specified number of
                            bbs has been completed. */
      VgSrc_FatalSig	 /* Killed by the default action of a fatal
			    signal */
   }
   VgSchedReturnCode;


/* The scheduler. */
extern VgSchedReturnCode VG_(scheduler) ( void );

extern void VG_(scheduler_init) ( void );

extern void VG_(pp_sched_status) ( void );

/* vg_oursignalhandler() might longjmp().  Here's the jmp_buf. */
extern jmp_buf VG_(scheduler_jmpbuf);
/* This says whether scheduler_jmpbuf is actually valid.  Needed so
   that our signal handler doesn't longjmp when the buffer isn't
   actually valid. */
extern Bool    VG_(scheduler_jmpbuf_valid);
/* ... and if so, here's the signal which caused it to do so. */
extern Int     VG_(longjmpd_on_signal);


/* The red-zone size which we put at the bottom (highest address) of
   thread stacks, for paranoia reasons.  This can be arbitrary, and
   doesn't really need to be set at compile time. */
#define VG_AR_CLIENT_STACKBASE_REDZONE_SZW 4

#define VG_AR_CLIENT_STACKBASE_REDZONE_SZB \
   (VG_AR_CLIENT_STACKBASE_REDZONE_SZW * VKI_BYTES_PER_WORD)

/* Junk to fill up a thread's shadow regs with when shadow regs aren't
   being used. */
#define VG_UNUSED_SHADOW_REG_VALUE  0x27182818
/* For sanity checking:  if this ends up in a thread's shadow regs when
   shadow regs aren't being used, something went wrong. */
#define   VG_USED_SHADOW_REG_VALUE  0x31415927

/* Write a value to a client's thread register, and shadow (if necessary) */
#define SET_THREAD_REG( zztid, zzval, zzreg, zzREG, zzevent, zzargs... ) \
   do { VG_(threads)[zztid].m_##zzreg = (zzval);                  \
        VG_TRACK( zzevent, zztid, R_##zzREG, ##zzargs );          \
   } while (0)

#define SET_SYSCALL_RETVAL(zztid, zzval) \
   SET_THREAD_REG(zztid, zzval, eax, EAX, post_reg_write_syscall_return)

#define SET_SIGNAL_EDX(zztid, zzval) \
   SET_THREAD_REG(zztid, zzval, edx, EDX, post_reg_write_deliver_signal)

#define SET_SIGNAL_ESP(zztid, zzval) \
   SET_THREAD_REG(zztid, zzval, esp, ESP, post_reg_write_deliver_signal)

#define SET_CLREQ_RETVAL(zztid, zzval) \
   SET_THREAD_REG(zztid, zzval, edx, EDX, post_reg_write_clientreq_return)

#define SET_CLCALL_RETVAL(zztid, zzval, f) \
   SET_THREAD_REG(zztid, zzval, edx, EDX, post_reg_write_clientcall_return, f)

#define SET_PTHREQ_ESP(zztid, zzval) \
   SET_THREAD_REG(zztid, zzval, esp, ESP, post_reg_write_pthread_return)

#define SET_PTHREQ_RETVAL(zztid, zzval) \
   SET_THREAD_REG(zztid, zzval, edx, EDX, post_reg_write_pthread_return)


/* This is or'd into a pthread mutex's __m_kind field if it is used
   before Valgrind is up and running (prehistory).  This is used so
   that if some early code (like the dynamic linker) takes a lock
   before Valgrind starts and then releases it afterwards, we can work
   out what's happening. */
#define VG_PTHREAD_PREHISTORY		0x80000000

/* ---------------------------------------------------------------------
   Exports of vg_signals.c
   ------------------------------------------------------------------ */

extern Bool VG_(do_signal_routing); /* whether scheduler LWP has to route signals */

/* RT signal allocation */
extern Int  VG_(sig_rtmin);
extern Int  VG_(sig_rtmax);
extern Int  VG_(sig_alloc_rtsig) ( Int high );

extern void VG_(sigstartup_actions) ( void );
extern void VG_(sigshutdown_actions) ( void );

extern void VG_(deliver_signal) ( ThreadId tid, const vki_ksiginfo_t *, Bool async );
extern void VG_(unblock_host_signal) ( Int sigNo );
extern void VG_(handle_SCSS_change) ( Bool force_update );

extern Bool VG_(is_sig_ign) ( Int sigNo );

/* Route pending signals from the scheduler LWP to the appropriate
   thread LWP. */
extern void VG_(route_signals) ( void );

/* Fake system calls for signal handling. */
extern void VG_(do__NR_sigaltstack)   ( ThreadId tid );
extern void VG_(do__NR_sigaction)     ( ThreadId tid );
extern void VG_(do__NR_sigprocmask)   ( ThreadId tid,
                                        Int how, 
                                        vki_ksigset_t* set,
                                        vki_ksigset_t* oldset );
extern void VG_(do_pthread_sigmask_SCSS_upd) ( ThreadId tid,
                                               Int how, 
                                               vki_ksigset_t* set,
                                               vki_ksigset_t* oldset );
extern void VG_(send_signal_to_thread) ( ThreadId thread,
                                         Int signo );

extern void VG_(do_sigpending) ( ThreadId tid, vki_ksigset_t* set );


/* Modify the current thread's state once we have detected it is
   returning from a signal handler. */
extern Bool VG_(signal_returns) ( ThreadId );

/* Handy utilities to block/restore all host signals. */
extern void VG_(block_all_host_signals) 
                  ( /* OUT */ vki_ksigset_t* saved_mask );
extern void VG_(restore_all_host_signals) 
                  ( /* IN */ vki_ksigset_t* saved_mask );

extern vki_ksiginfo_t VG_(unresumable_siginfo);

extern void VG_(kill_self)(Int sigNo);

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

/* VG_(brk) not public so skins cannot screw with curr_dataseg_end */
extern void* VG_(brk) ( void* end_data_segment );

/* Skins use VG_(strdup)() which doesn't expose ArenaId */
extern Char* VG_(arena_strdup) ( ArenaId aid, const Char* s);

extern Int VG_(fcntl) ( Int fd, Int cmd, Int arg );
extern Int VG_(select)( Int n, 
                        vki_fd_set* readfds, 
                        vki_fd_set* writefds, 
                        vki_fd_set* exceptfds, 
                        struct vki_timeval * timeout );
extern Int VG_(poll)( struct vki_pollfd *, UInt nfds, Int timeout);
extern Int VG_(nanosleep)( const struct vki_timespec *req, 
                           struct vki_timespec *rem );

/* system/mman.h */
extern void* VG_(mmap)( void* start, UInt length,
                        UInt prot, UInt flags, UInt fd, UInt offset );
extern Int  VG_(munmap)( void* start, Int length );
extern Int  VG_(mprotect)( void *start, Int length, UInt prot );


/* Move an fd into the Valgrind-safe range */
Int VG_(safe_fd)(Int oldfd);

extern Int VG_(write_socket)( Int sd, void *msg, Int count );

/* --- Connecting over the network --- */
extern Int VG_(connect_via_socket)( UChar* str );

/* Environment manipulations */
extern Char **VG_(env_clone) ( Char **oldenv );
extern Char* VG_(env_getenv) ( Char **env, Char* varname );
extern Char **VG_(env_setenv) ( Char ***envp, const Char* varname, const Char *val );
extern void  VG_(env_unsetenv) ( Char **env, const Char *varname );

/* ---------------------------------------------------------------------
   Exports of vg_message.c
   ------------------------------------------------------------------ */

/* Low-level -- send bytes directly to the message sink.  Do not
   use. */
extern void VG_(send_bytes_to_logging_sink) ( Char* msg, Int nbytes );


/* ---------------------------------------------------------------------
   Definitions for the JITter (vg_translate.c, vg_to_ucode.c,
   vg_from_ucode.c).
   ------------------------------------------------------------------ */

#define VG_IS_FLAG_SUBSET(set1,set2) \
   (( ((FlagSet)set1) & ((FlagSet)set2) ) == ((FlagSet)set1) )

#define VG_UNION_FLAG_SETS(set1,set2) \
   ( ((FlagSet)set1) | ((FlagSet)set2) )

/* ---------------------------------------------------------------------
   Exports of vg_demangle.c
   ------------------------------------------------------------------ */

extern void VG_(demangle) ( Char* orig, Char* result, Int result_size );

/* ---------------------------------------------------------------------
   Exports of vg_from_ucode.c
   ------------------------------------------------------------------ */

extern UChar* VG_(emit_code) ( UCodeBlock* cb, Int* nbytes, UShort jumps[VG_MAX_JUMPS] );

extern void   VG_(print_ccall_stats)      ( void );
extern void   VG_(print_UInstr_histogram) ( void );

extern void   VG_(unchain_jumpsite)	  ( Addr jumpsite );
extern Addr   VG_(get_jmp_dest)           ( Addr jumpsite );
extern Bool   VG_(is_unchained_jumpsite)  ( Addr jumpsite );
extern Bool   VG_(is_chained_jumpsite)    ( Addr jumpsite );

/* ---------------------------------------------------------------------
   Exports of vg_to_ucode.c
   ------------------------------------------------------------------ */

Bool VG_(cpu_has_feature)(UInt feat);

extern Int   VG_(disBB)          ( UCodeBlock* cb, Addr eip0 );

/* ---------------------------------------------------------------------
   Exports of vg_translate.c
   ------------------------------------------------------------------ */

/* Expandable arrays of uinstrs. */
struct _UCodeBlock { 
   Addr	   orig_eip;
   Int     used; 
   Int     size; 
   UInstr* instrs;
   Int     nextTemp;
};

extern UCodeBlock* VG_(alloc_UCodeBlock) ( void );

extern void  VG_(translate)  ( ThreadId tid,
                               Addr  orig_addr,
                               UInt* orig_size,
                               Addr* trans_addr,
                               UInt* trans_size,
			       UShort jumps[VG_MAX_JUMPS]);

extern Bool  VG_(saneUInstr)          ( Bool beforeRA, Bool beforeLiveness,
                                        UInstr* u );
extern void  VG_(saneUCodeBlock)      ( UCodeBlock* cb );
extern Bool  VG_(saneUCodeBlockCalls) ( UCodeBlock* cb );


/* ---------------------------------------------------------------------
   Exports of vg_execontext.c.
   ------------------------------------------------------------------ */

/* Records the PC and a bit of the call chain.  The first 4 %eip
   values are used in comparisons do remove duplicate errors, and for
   comparing against suppression specifications.  The rest are purely
   informational (but often important). */

struct _ExeContext {
   struct _ExeContext * next;
   /* Variable-length array.  The size is VG_(clo_backtrace_size); at
      least 1, at most VG_DEEPEST_BACKTRACE.  [0] is the current %eip,
      [1] is its caller, [2] is the caller of [1], etc. */
   Addr eips[0];
};


/* Print stats (informational only). */
extern void VG_(show_ExeContext_stats) ( void );

/* Like VG_(get_ExeContext), but with a slightly different type */
extern ExeContext* VG_(get_ExeContext2) ( Addr eip, Addr ebp,
                                          Addr ebp_min, Addr ebp_max );


/* ---------------------------------------------------------------------
   Exports of vg_errcontext.c.
   ------------------------------------------------------------------ */

/* Note: it is imperative this doesn't overlap with (0..) at all, as skins
 * effectively extend it by defining their own enums in the (0..) range. */
typedef
   enum {
      PThreadSupp = -1,    /* Matches PThreadErr */
   }
   CoreSuppKind;

/* For each caller specified for a suppression, record the nature of
   the caller name.  Not of interest to skins. */
typedef
   enum { 
      ObjName,    /* Name is of an shared object file. */
      FunName     /* Name is of a function. */
   }
   SuppLocTy;

/* Suppressions.  Skins can get/set skin-relevant parts with functions
   declared in include/vg_skin.h.  Extensible via the 'extra' field. 
   Skins can use a normal enum (with element values in the normal range
   (0..)) for `skind'. */
struct _Supp {
   struct _Supp* next;
   /* The number of times this error has been suppressed. */
   Int count;
   /* The name by which the suppression is referred to. */
   Char* sname;
   /* First two (name of fn where err occurs, and immediate caller)
    * are mandatory;  extra two are optional. */
   SuppLocTy caller_ty[VG_N_SUPP_CALLERS];
   Char*     caller   [VG_N_SUPP_CALLERS];

   /* The skin-specific part */
   /* What kind of suppression.  Must use the range (0..) */
   SuppKind skind;
   /* String -- use is optional.  NULL by default. */
   Char* string;
   /* Anything else -- use is optional.  NULL by default. */
   void* extra;
};

/* Note: it is imperative this doesn't overlap with (0..) at all, as skins
 * effectively extend it by defining their own enums in the (0..) range. */
typedef
   enum { 
      PThreadErr      = -1,   /* Pthreading error */
   }
   CoreErrorKind;

/* Errors.  Extensible (via the 'extra' field).  Skins can use a normal
   enum (with element values in the normal range (0..)) for `ekind'. 
   Functions for getting/setting the skin-relevant fields are in
   include/vg_skin.h.

   When errors are found and recorded with VG_(maybe_record_error)(), all
   the skin must do is pass in the four parameters;  core will
   allocate/initialise the error record.
*/
struct _Error {
   struct _Error* next;
   /* NULL if unsuppressed; or ptr to suppression record. */
   Supp* supp;
   Int count;
   ThreadId tid;

   /* The skin-specific part */
   /* Initialised by core */
   ExeContext* where;
   /* Used by ALL.  Must be in the range (0..) */
   Int ekind;
   /* Used frequently */
   Addr addr;
   /* Used frequently */
   Char* string;
   /* For any skin-specific extras */
   void* extra;
};


extern void VG_(load_suppressions)    ( void );

extern void VG_(record_pthread_error) ( ThreadId tid, Char* msg );

extern void VG_(show_all_errors)      ( void );

extern Bool VG_(is_action_requested) ( Char* action, Bool* clo );

extern UInt VG_(n_errs_found);

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
   void (*record_mapping)( Addr addr, UInt len, Char rr, Char ww, Char xx, 
			   UInt dev, UInt ino, ULong foff, const UChar *filename )
);


/* ---------------------------------------------------------------------
   Exports of vg_symtab2.c
   ------------------------------------------------------------------ */

typedef struct _Segment Segment;

extern Bool VG_(is_object_file)   ( const void *hdr );
extern void VG_(mini_stack_dump)  ( Addr eips[], UInt n_eips );
extern SegInfo * VG_(read_seg_symbols) ( Segment *seg );
extern void VG_(unload_symbols)   ( Addr start, UInt length );
extern void VG_(symtab_incref)	  ( SegInfo * );
extern void VG_(symtab_decref)	  ( SegInfo *, Addr a, UInt len );

extern Bool VG_(get_fnname_nodemangle)( Addr a, Char* fnname, Int n_fnname );

/* Set up some default redirects */
extern void VG_(setup_code_redirect_table) ( void );

/* Redirection machinery */
extern void VG_(add_redirect_sym)(const Char *from_lib, const Char *from_sym,
				  const Char *to_lib, const Char *to_sym);
extern void VG_(add_redirect_addr)(const Char *from_lib, const Char *from_sym,
				   Addr to_addr);
extern Addr VG_(code_redirect)	  (Addr orig);

/* ---------------------------------------------------------------------
   Exports of vg_main.c
   ------------------------------------------------------------------ */

/* Is this a SSE/SSE2-capable CPU?  If so, we had better save/restore
   the SSE state all over the place.  This is set up very early, in
   vg_startup.S.  We have to determine it early since we can't even
   correctly snapshot the startup machine state without it. */
extern Bool VG_(have_ssestate);

/* Tell the logging mechanism whether we are logging to a file
   descriptor or a socket descriptor. */
extern Bool VG_(logging_to_filedes);

/* Sanity checks which may be done at any time.  The scheduler decides when. */
extern void VG_(do_sanity_checks) ( Bool force_expensive );

/* Address space */
extern Addr VG_(client_base);	/* client address space limits */
extern Addr VG_(client_end);
extern Addr VG_(client_mapbase); /* base of mappings */
extern Addr VG_(clstk_base);	/* client stack range */
extern Addr VG_(clstk_end);
extern Addr VG_(client_trampoline_code);

extern Addr VG_(brk_base);	/* start of brk */
extern Addr VG_(brk_limit);	/* current brk */
extern Addr VG_(shadow_base);	/* skin's shadow memory */
extern Addr VG_(shadow_end);
extern Addr VG_(valgrind_base);	/* valgrind's address range */
extern Addr VG_(valgrind_mmap_end);
extern Addr VG_(valgrind_end);

/* stage1 executable file descriptor */
extern Int  VG_(vgexecfd);

/* client executable file descriptor */
extern Int  VG_(clexecfd);

/* Path to all our library/aux files */
extern const Char *VG_(libdir);

/* A structure used as an intermediary when passing the simulated
   CPU's state to VG_(switch_to_real_CPU)(), for --stop-after=yes.
   Stuff is copied from baseBlock to here, because it's much easier
   to copy the state into the real registers from this structure than
   the baseBlock, because it's layout is simpler.
   Alignment: the SSE state must be 16-byte aligned.  We ask for the whole
   struct to be 16-byte aligned, and the SSE state starts at the 6+8+1+1th
   == 16th word, so it too must be 16-byte aligned.  Consequence: change
   this struct only _very carefully_ !  See also above comment re masking
   MXCSR. 
*/
__attribute__ ((aligned (16)))
extern UInt VG_(m_state_static) [6 /* segment regs, Intel order */
                                 + 8 /* int regs, in Intel order */ 
                                 + 1 /* %eflags */ 
                                 + 1 /* %eip */
                                 + VG_SIZE_OF_SSESTATE_W /* SSE state */
                                ];

/* Determine if %esp adjustment must be noted */
extern Bool VG_(need_to_handle_esp_assignment) ( void );

/* Called when some unhandleable client behaviour is detected.
   Prints a msg and aborts. */
extern void VG_(unimplemented) ( Char* msg )
            __attribute__((__noreturn__));

/* Similarly, we have to ask for signals to be delivered on an alternative
   stack, since it is possible, although unlikely, that we'll have to run
   client code from inside the Valgrind-installed signal handler.  If this
   happens it will be done by vg_deliver_signal_immediately(). */
extern UInt VG_(sigstack)[VG_SIGSTACK_SIZE_W];

/* Valgrind's argc and argv */
extern Int    VG_(vg_argc);
extern Char **VG_(vg_argv);

/* Indicates presence, and holds address of client's sysinfo page, a
   feature of some modern kernels used to provide vsyscalls, etc. */
extern Bool VG_(sysinfo_page_exists);
extern Addr VG_(sysinfo_page_addr);

/* Walk through a colon separated list variable, removing entries
   which match pattern. */
extern void VG_(mash_colon_env)(Char *varp, const Char *pattern);

/* Something of a function looking for a home ... start up debugger. */
extern void VG_(start_debugger) ( Int tid );

/* VG_(bbs_done) in include/vg_skin.h */

/* 64-bit counter for the number of bbs to go before a debug exit. */
extern ULong VG_(bbs_to_go);

/* Counts downwards in vg_run_innerloop. */
extern UInt VG_(dispatch_ctr);

/* This is the ThreadId of the last thread the scheduler ran. */
extern ThreadId VG_(last_run_tid);

/* This is the argument to __NR_exit() supplied by the first thread to
   call that syscall.  We eventually pass that to __NR_exit() for
   real. */
extern Int VG_(exitcode);

/* If we're doing the default action of a fatal signal */
extern jmp_buf VG_(fatal_signal_jmpbuf);
extern Bool    VG_(fatal_signal_set);		/* jmp_buf is valid */
extern Int     VG_(fatal_sigNo);		/* the fatal signal */

/* --- Counters, for informational purposes only. --- */

/* Number of lookups which miss the fast tt helper. */
extern UInt VG_(tt_fast_misses);

/* Counts for TT/TC informational messages. */

/* Number and total o/t size of translations overall. */
extern UInt VG_(overall_in_count);
extern UInt VG_(overall_in_osize);
extern UInt VG_(overall_in_tsize);
/* Number and total o/t size of discards overall. */
extern UInt VG_(overall_out_count);
extern UInt VG_(overall_out_osize);
extern UInt VG_(overall_out_tsize);
/* The number of discards of TT/TC. */
extern UInt VG_(number_of_tc_discards);
/* Counts of chain and unchain operations done. */
extern UInt VG_(bb_enchain_count);
extern UInt VG_(bb_dechain_count);
/* Number of unchained jumps performed. */
extern UInt VG_(unchained_jumps_done);


/* Counts pertaining to the register allocator. */

/* total number of uinstrs input to reg-alloc */
extern UInt VG_(uinstrs_prealloc);

/* total number of uinstrs added due to spill code */
extern UInt VG_(uinstrs_spill);

/* number of bbs requiring spill code */
extern UInt VG_(translations_needing_spill);

/* total of register ranks over all translations */
extern UInt VG_(total_reg_rank);

/* Counts pertaining to internal sanity checking. */
extern UInt VG_(sanity_fast_count);
extern UInt VG_(sanity_slow_count);

/* Counts pertaining to the scheduler. */
extern UInt VG_(num_scheduling_events_MINOR);
extern UInt VG_(num_scheduling_events_MAJOR);

/* Insert and extract the D flag from eflags */
UInt VG_(insertDflag)(UInt eflags, Int d);
Int VG_(extractDflag)(UInt eflags);

/* ---------------------------------------------------------------------
   Exports of vg_memory.c
   ------------------------------------------------------------------ */

/* A Segment is mapped piece of client memory.  This covers all kinds
   of mapped memory (exe, brk, mmap, .so, shm, stack, etc)

   We try to encode everything we know about a particular segment here.
*/
#define SF_FIXED	(1 <<  0) /* client asked for MAP_FIXED			*/
#define SF_SHARED	(1 <<  1) /* shared					*/
#define SF_SHM		(1 <<  2) /* SYSV SHM (also SF_SHARED)			*/
#define SF_MMAP		(1 <<  3) /* mmap memory				*/
#define SF_FILE		(1 <<  4) /* mapping is backed by a file		*/
#define SF_STACK	(1 <<  5) /* is a stack					*/
#define SF_GROWDOWN	(1 <<  6) /* segment grows down				*/
#define SF_GROWUP	(1 <<  7) /* segment grows up				*/
#define SF_EXEC		(1 <<  8) /* segment created by exec			*/
#define SF_DYNLIB	(1 <<  9) /* mapped from dynamic library		*/
#define SF_NOSYMS	(1 << 10) /* don't load syms, even if present           */
#define SF_BRK		(1 << 11) /* brk segment                                */
#define SF_CORE		(1 << 12) /* allocated by core on behalf of the client  */
#define SF_VALGRIND	(1 << 13) /* a valgrind-internal mapping - not in client*/
#define SF_CODE		(1 << 14) /* segment contains cached code               */

struct _Segment {
   UInt		prot;		/* VKI_PROT_*				*/
   UInt		flags;		/* SF_*					*/

   Addr		addr;		/* mapped addr (page aligned)		*/
   UInt		len;		/* size of mapping (page aligned)	*/

   /* These are valid if (flags & SF_FILE) */
   ULong	offset;		/* file offset				*/
   const Char	*filename;	/* filename (NULL if unknown)		*/
   UInt		dev;		/* device				*/
   UInt		ino;		/* inode				*/

   SegInfo	*symtab;	/* symbol table				*/
};

/* segment mapped from a file descriptor */
extern void VG_(map_fd_segment)  (Addr addr, UInt len, UInt prot, UInt flags, 
				  Int fd, ULong off, const Char *filename);

/* segment mapped from a file */
extern void VG_(map_file_segment)(Addr addr, UInt len, UInt prot, UInt flags, 
				  UInt dev, UInt ino, ULong off, const Char *filename);

/* simple segment */
extern void VG_(map_segment)     (Addr addr, UInt len, UInt prot, UInt flags);

extern void VG_(unmap_range)   (Addr addr, UInt len);
extern void VG_(mprotect_range)(Addr addr, UInt len, UInt prot);
extern Addr VG_(find_map_space)(Addr base, UInt len, Bool for_client);

extern Segment *VG_(find_segment)(Addr a);
extern Segment *VG_(next_segment)(Segment *);

extern Bool     VG_(seg_contains)(const Segment *s, Addr ptr, UInt size);
extern Bool     VG_(seg_overlaps)(const Segment *s, Addr ptr, UInt size);

extern __attribute__((regparm(1))) 
       void VG_(unknown_esp_update) ( Addr new_ESP );

/* ---------------------------------------------------------------------
   Exports of vg_proxylwp.c
   ------------------------------------------------------------------ */

/* Issue a syscall for thread tid */
extern Int  VG_(sys_issue)(int tid);

extern void VG_(proxy_init)     ( void );
extern void VG_(proxy_create)   ( ThreadId tid );
extern void VG_(proxy_delete)   ( ThreadId tid, Bool force );
extern void VG_(proxy_results)  ( void );
extern void VG_(proxy_sendsig)  ( ThreadId tid, Int signo );
extern void VG_(proxy_setsigmask)(ThreadId tid);
extern void VG_(proxy_sigack)   ( ThreadId tid, const vki_ksigset_t *);
extern void VG_(proxy_abort_syscall) ( ThreadId tid );
extern void VG_(proxy_waitsig)  ( void );
extern void VG_(proxy_wait_sys)	(ThreadId tid, Bool restart);

extern void VG_(proxy_shutdown) ( void );	/* shut down the syscall workers */
extern Int  VG_(proxy_resfd)    ( void );	/* FD something can select on to know 
						   a syscall finished */

/* Sanity-check the whole proxy-LWP machinery */
void VG_(proxy_sanity)(void);

/* Send a signal from a thread's proxy to the thread.  This longjmps
   back into the proxy's main loop, so it doesn't return. */
__attribute__ ((__noreturn__))
extern void VG_(proxy_handlesig)( const vki_ksiginfo_t *siginfo, 
				  const struct vki_sigcontext *sigcontext );

/* Get the PID/TID of the ProxyLWP. */
extern Int VG_(proxy_id)(ThreadId tid);


/* ---------------------------------------------------------------------
   Exports of vg_syscalls.c
   ------------------------------------------------------------------ */

extern Char *VG_(resolve_filename)(Int fd);

extern Bool VG_(pre_syscall) ( ThreadId tid );
extern void VG_(post_syscall)( ThreadId tid, Bool restart );
extern void VG_(restart_syscall) ( ThreadId tid );

extern Bool VG_(is_kerror) ( Int res );

/* Internal atfork handlers */
typedef void (*vg_atfork_t)(ThreadId);
extern void VG_(atfork)(vg_atfork_t pre, vg_atfork_t parent, vg_atfork_t child);

/* fd leakage calls. */
extern void VG_(init_preopened_fds) ( void );
extern void VG_(fd_stats) ( void );

/* ---------------------------------------------------------------------
   Exports of vg_transtab.c
   ------------------------------------------------------------------ */

/* The fast-cache for tt-lookup. */
extern Addr VG_(tt_fast)[VG_TT_FAST_SIZE];

extern void VG_(get_tt_tc_used) ( UInt* tt_used, UInt* tc_used );

extern void VG_(add_to_trans_tab) ( Addr orig_addr,  Int orig_size,
                                    Addr trans_addr, Int trans_size,
				    UShort jumps[VG_MAX_JUMPS]);

extern void VG_(invalidate_translations) ( Addr start, UInt range, Bool unchain_blocks );

extern void VG_(init_tt_tc) ( void );

extern void VG_(sanity_check_tc_tt) ( void );
extern Addr VG_(search_transtab) ( Addr original_addr );



/* ---------------------------------------------------------------------
   Exports of vg_syscall.S
   ------------------------------------------------------------------ */

extern Int VG_(do_syscall) ( UInt, ... );
extern Int VG_(clone) ( Int (*fn)(void *), void *stack, Int flags, void *arg, 
			Int *child_tid, Int *parent_tid);

/* ---------------------------------------------------------------------
   Exports of vg_startup.S
   ------------------------------------------------------------------ */

extern void VG_(switch_to_real_CPU) ( void );

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

/* Mul, div, etc, -- we don't codegen these directly. */
extern void VG_(helper_idiv_64_32);
extern void VG_(helper_div_64_32);
extern void VG_(helper_idiv_32_16);
extern void VG_(helper_div_32_16);
extern void VG_(helper_idiv_16_8);
extern void VG_(helper_div_16_8);

extern void VG_(helper_imul_32_64);
extern void VG_(helper_mul_32_64);
extern void VG_(helper_imul_16_32);
extern void VG_(helper_mul_16_32);
extern void VG_(helper_imul_8_16);
extern void VG_(helper_mul_8_16);

extern void VG_(helper_CLD);
extern void VG_(helper_STD);
extern void VG_(helper_get_dirflag);

extern void VG_(helper_CLC);
extern void VG_(helper_STC);

extern void VG_(helper_shldl);
extern void VG_(helper_shldw);
extern void VG_(helper_shrdl);
extern void VG_(helper_shrdw);

extern void VG_(helper_IN);
extern void VG_(helper_OUT);

extern void VG_(helper_RDTSC);
extern void VG_(helper_CPUID);

extern void VG_(helper_bsf);
extern void VG_(helper_bsr);

extern void VG_(helper_fstsw_AX);
extern void VG_(helper_SAHF);
extern void VG_(helper_LAHF);
extern void VG_(helper_DAS);
extern void VG_(helper_DAA);

extern void VG_(helper_cmpxchg8b);

extern void VG_(helper_undefined_instruction);

/* Information about trampoline code (for signal return and syscalls) */
extern const Char VG_(trampoline_code_start);
extern const Int  VG_(trampoline_code_length);
extern const Int  VG_(tramp_sigreturn_offset);
extern const Int  VG_(tramp_syscall_offset);

/* ---------------------------------------------------------------------
   Things relating to the used skin
   ------------------------------------------------------------------ */

#define VG_TRACK(fn, args...) 			\
   do {						\
      if (VG_(defined_##fn)())			\
	 SK_(fn)(args);				\
   } while(0)

__attribute__ ((noreturn))
extern void VG_(missing_tool_func) ( const Char* fn );

/* ---------------------------------------------------------------------
   The state of the simulated CPU.
   ------------------------------------------------------------------ */

/* ---------------------------------------------------------------------
   Offsets into baseBlock for everything which needs to referred to
   from generated code.  The order of these decls does not imply 
   what the order of the actual offsets is.  The latter is important
   and is set up in vg_main.c.
   ------------------------------------------------------------------ */

/* An array of words.  In generated code, %ebp always points to the
   start of this array.  Useful stuff, like the simulated CPU state,
   and the addresses of helper functions, can then be found by
   indexing off %ebp.  The following declares variables which, at
   startup time, are given values denoting offsets into baseBlock.
   These offsets are in *words* from the start of baseBlock. */

#define VG_BASEBLOCK_WORDS 400

extern UInt VG_(baseBlock)[VG_BASEBLOCK_WORDS];


/* -----------------------------------------------------
   Read-write parts of baseBlock.
   -------------------------------------------------- */

/* State of the simulated CPU. */
extern Int VGOFF_(m_eax);
extern Int VGOFF_(m_ecx);
extern Int VGOFF_(m_edx);
extern Int VGOFF_(m_ebx);
extern Int VGOFF_(m_esp);
extern Int VGOFF_(m_ebp);
extern Int VGOFF_(m_esi);
extern Int VGOFF_(m_edi);
extern Int VGOFF_(m_eflags);
extern Int VGOFF_(m_ssestate);
extern Int VGOFF_(m_eip);

extern Int VGOFF_(m_dflag);	/* D flag is handled specially */

extern Int VGOFF_(m_cs);
extern Int VGOFF_(m_ss);
extern Int VGOFF_(m_ds);
extern Int VGOFF_(m_es);
extern Int VGOFF_(m_fs);
extern Int VGOFF_(m_gs);

/* Reg-alloc spill area (VG_MAX_SPILLSLOTS words long). */
extern Int VGOFF_(spillslots);

/* Records the valid bits for the 8 integer regs & flags reg. */
extern Int VGOFF_(sh_eax);
extern Int VGOFF_(sh_ecx);
extern Int VGOFF_(sh_edx);
extern Int VGOFF_(sh_ebx);
extern Int VGOFF_(sh_esp);
extern Int VGOFF_(sh_ebp);
extern Int VGOFF_(sh_esi);
extern Int VGOFF_(sh_edi);
extern Int VGOFF_(sh_eflags);

/* -----------------------------------------------------
   Read-only parts of baseBlock.
   -------------------------------------------------- */

/* This thread's LDT pointer. */
extern Int VGOFF_(ldt);

/* This thread's TLS pointer. */
extern Int VGOFF_(tls);

/* Nb: Most helper offsets are in include/vg_skin.h, for use by skins */

extern Int VGOFF_(helper_undefined_instruction);

/* For storing extension-specific helpers, determined at runtime.  The addr 
 * and offset arrays together form a (addr, offset) map that allows a 
 * helper's baseBlock offset to be computed from its address.  It's done 
 * like this so CCALL_M_Ns and other helper calls can use the function 
 * address rather than having to much around with offsets. */
extern UInt VG_(n_compact_helpers);
extern UInt VG_(n_noncompact_helpers);

extern Addr VG_(compact_helper_addrs)  [];
extern Int  VG_(compact_helper_offsets)[];

extern Addr VG_(noncompact_helper_addrs)  [];
extern Int  VG_(noncompact_helper_offsets)[];

#endif /* ndef __VG_INCLUDE_H */


/* ---------------------------------------------------------------------
   Finally - autoconf-generated settings
   ------------------------------------------------------------------ */

#include "config.h"

/*--------------------------------------------------------------------*/
/*--- end                                             vg_include.h ---*/
/*--------------------------------------------------------------------*/
