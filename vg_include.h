
/*--------------------------------------------------------------------*/
/*--- A header file for all parts of Valgrind.                     ---*/
/*--- Include no other!                                            ---*/
/*---                                                 vg_include.h ---*/
/*--------------------------------------------------------------------*/

/*
   This file is part of Valgrind, an x86 protected-mode emulator 
   designed for debugging and profiling binaries on x86-Unixes.

   Copyright (C) 2000-2002 Julian Seward 
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

   The GNU General Public License is contained in the file LICENSE.
*/

#ifndef __VG_INCLUDE_H
#define __VG_INCLUDE_H


#include <stdarg.h>       /* ANSI varargs stuff  */
#include <setjmp.h>       /* for jmp_buf         */


/* ---------------------------------------------------------------------
   Build options and table sizes.  You should be able to change these
   options or sizes, recompile, and still have a working system.
   ------------------------------------------------------------------ */

#include "vg_constants.h"


/* Set to 1 to enable time profiling.  Since this uses SIGPROF, we
   don't want this permanently enabled -- only for profiling
   builds. */
#if 0
#  define VG_PROFILE
#endif


/* Total number of integer registers available for allocation.  That's
   all of them except %esp, %edi and %ebp.  %edi is a general spare
   temporary.  %ebp permanently points at VG_(baseBlock).  Note that
   it's important that this tie in with what rankToRealRegNo() says.
   DO NOT CHANGE THIS VALUE FROM 5. !  */
#define VG_MAX_REALREGS 5

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
#define M_PROCMAP_BUF 20000

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
#define M_VG_COLLECT_NO_ERRORS_AFTER 500

/* These many bytes below %ESP are considered addressible if we're
   doing the --workaround-gcc296-bugs hack. */
#define VG_GCC296_BUG_STACK_SLOP 256

/* The maximum number of calls we're prepared to save in a
   backtrace. */
#define VG_DEEPEST_BACKTRACE 50

/* Number of lists in which we keep track of malloc'd but not free'd
   blocks.  Should be prime. */
#define VG_N_MALLOCLISTS 997

/* Number of lists in which we keep track of ExeContexts.  Should be
   prime. */
#define VG_N_EC_LISTS /*997*/ 4999

/* Defines the thread-scheduling timeslice, in terms of the number of
   basic blocks we attempt to run each thread for.  Smaller values
   give finer interleaving but much increased scheduling overheads. */
#define VG_SCHEDULING_QUANTUM   10000

/* The maximum number of pthreads that we support.  This is
   deliberately not very high since our implementation of some of the
   scheduler algorithms is surely O(N^2) in the number of threads,
   since that's simple, at least.  And (in practice) we hope that most
   programs do not need many threads. */
#define VG_N_THREADS 20

/* Number of file descriptors that can simultaneously be waited on for
   I/O to complete.  Perhaps this should be the same as VG_N_THREADS
   (surely a thread can't wait on more than one fd at once?.  Who
   knows.) */
#define VG_N_WAITING_FDS 10

/* Maximum number of mutexes allowed. */
#define VG_N_MUTEXES 10


/* ---------------------------------------------------------------------
   Basic types
   ------------------------------------------------------------------ */

typedef unsigned char          UChar;
typedef unsigned short         UShort;
typedef unsigned int           UInt;
typedef unsigned long long int ULong;

typedef signed char          Char;
typedef signed short         Short;
typedef signed int           Int;
typedef signed long long int Long;

typedef unsigned int Addr;

typedef unsigned char Bool;
#define False ((Bool)0)
#define True ((Bool)1)

#define mycat_wrk(aaa,bbb) aaa##bbb
#define mycat(aaa,bbb) mycat_wrk(aaa,bbb)

/* Just pray that gcc's constant folding works properly ... */
#define BITS(bit7,bit6,bit5,bit4,bit3,bit2,bit1,bit0)               \
   ( ((bit7) << 7) | ((bit6) << 6) | ((bit5) << 5) | ((bit4) << 4)  \
     | ((bit3) << 3) | ((bit2) << 2) | ((bit1) << 1) | (bit0))


/* ---------------------------------------------------------------------
   Now the basic types are set up, we can haul in the kernel-interface
   definitions.
   ------------------------------------------------------------------ */

#include "./vg_kerneliface.h"


/* ---------------------------------------------------------------------
   Command-line-settable options
   ------------------------------------------------------------------ */

#define VG_CLO_SMC_NONE 0
#define VG_CLO_SMC_SOME 1
#define VG_CLO_SMC_ALL  2

#define VG_CLO_MAX_SFILES 10

/* Shall we V-check addrs (they are always A checked too): default: YES */
extern Bool VG_(clo_check_addrVs);
/* Enquire about whether to attach to GDB at errors?   default: NO */
extern Bool  VG_(clo_GDB_attach);
/* Sanity-check level: 0 = none, 1 (default), > 1 = expensive. */
extern Int   VG_(sanity_level);
/* Verbosity level: 0 = silent, 1 (default), > 1 = more verbose. */
extern Int   VG_(clo_verbosity);
/* Automatically attempt to demangle C++ names?  default: YES */
extern Bool  VG_(clo_demangle);
/* Do leak check at exit?  default: NO */
extern Bool  VG_(clo_leak_check);
/* In leak check, show reachable-but-not-freed blocks?  default: NO */
extern Bool  VG_(clo_show_reachable);
/* How closely should we compare ExeContexts in leak records? default: 2 */
extern Int   VG_(clo_leak_resolution);
/* Round malloc sizes upwards to integral number of words? default:
   NO */
extern Bool  VG_(clo_sloppy_malloc);
/* Allow loads from partially-valid addresses?  default: YES */
extern Bool  VG_(clo_partial_loads_ok);
/* Simulate child processes? default: NO */
extern Bool  VG_(clo_trace_children);
/* The file id on which we send all messages.  default: 2 (stderr). */
extern Int   VG_(clo_logfile_fd);
/* Max volume of the freed blocks queue. */
extern Int   VG_(clo_freelist_vol);
/* Assume accesses immediately below %esp are due to gcc-2.96 bugs.
   default: NO */
extern Bool  VG_(clo_workaround_gcc296_bugs);

/* The number of suppression files specified. */
extern Int   VG_(clo_n_suppressions);
/* The names of the suppression files. */
extern Char* VG_(clo_suppressions)[VG_CLO_MAX_SFILES];

/* Single stepping?  default: NO */
extern Bool  VG_(clo_single_step);
/* Code improvement?  default: YES */
extern Bool  VG_(clo_optimise);
/* Memory-check instrumentation?  default: YES */
extern Bool  VG_(clo_instrument);
/* DEBUG: clean up instrumented code?  default: YES */
extern Bool  VG_(clo_cleanup);
/* SMC write checks?  default: SOME (1,2,4 byte movs to mem) */
extern Int   VG_(clo_smc_check);
/* DEBUG: print system calls?  default: NO */
extern Bool  VG_(clo_trace_syscalls);
/* DEBUG: print signal details?  default: NO */
extern Bool  VG_(clo_trace_signals);
/* DEBUG: print symtab details?  default: NO */
extern Bool  VG_(clo_trace_symtab);
/* DEBUG: print malloc details?  default: NO */
extern Bool  VG_(clo_trace_malloc);
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


/* ---------------------------------------------------------------------
   Debugging and profiling stuff
   ------------------------------------------------------------------ */

/* No, really.  I _am_ that strange. */
#define OINK(nnn) VG_(message)(Vg_DebugMsg, "OINK %d",nnn)

/* Tools for building messages from multiple parts. */
typedef
   enum { Vg_UserMsg, Vg_DebugMsg, Vg_DebugExtraMsg }
   VgMsgKind;

extern void VG_(start_msg)  ( VgMsgKind kind );
extern void VG_(add_to_msg) ( Char* format, ... );
extern void VG_(end_msg)    ( void );

/* Send a simple, single-part message. */
extern void VG_(message)    ( VgMsgKind kind, Char* format, ... );

/* Create a logfile into which messages can be dumped. */
extern void VG_(startup_logging) ( void );
extern void VG_(shutdown_logging) ( void );


/* Profiling stuff */
#ifdef VG_PROFILE

#define VGP_M_STACK 10

#define VGP_M_CCS 20  /* == the # of elems in VGP_LIST */
#define VGP_LIST \
   VGP_PAIR(VgpRun=0,      "running"),                \
   VGP_PAIR(VgpMalloc,     "low-lev malloc/free"),    \
   VGP_PAIR(VgpCliMalloc,  "client  malloc/free"),    \
   VGP_PAIR(VgpTranslate,  "translate-main"),         \
   VGP_PAIR(VgpToUCode,    "to-ucode"),               \
   VGP_PAIR(VgpFromUcode,  "from-ucode"),             \
   VGP_PAIR(VgpImprove,    "improve"),                \
   VGP_PAIR(VgpInstrument, "instrument"),             \
   VGP_PAIR(VgpCleanup,    "cleanup"),                \
   VGP_PAIR(VgpRegAlloc,   "reg-alloc"),              \
   VGP_PAIR(VgpDoLRU,      "do-lru"),                 \
   VGP_PAIR(VgpSlowFindT,  "slow-search-transtab"),   \
   VGP_PAIR(VgpInitAudit,  "init-mem-audit"),         \
   VGP_PAIR(VgpExeContext, "exe-context"),            \
   VGP_PAIR(VgpReadSyms,   "read-syms"),              \
   VGP_PAIR(VgpAddToT,     "add-to-transtab"),        \
   VGP_PAIR(VgpSARP,       "set-addr-range-perms"),   \
   VGP_PAIR(VgpSyscall,    "syscall wrapper"),        \
   VGP_PAIR(VgpSpare1,     "spare 1"),                \
   VGP_PAIR(VgpSpare2,     "spare 2")

#define VGP_PAIR(enumname,str) enumname
typedef enum { VGP_LIST } VgpCC;
#undef VGP_PAIR

extern void VGP_(init_profiling) ( void );
extern void VGP_(done_profiling) ( void );
extern void VGP_(pushcc) ( VgpCC );
extern void VGP_(popcc) ( void );

#define VGP_PUSHCC(cc) VGP_(pushcc)(cc)
#define VGP_POPCC      VGP_(popcc)()

#else

#define VGP_PUSHCC(cc) /* */
#define VGP_POPCC      /* */

#endif /* VG_PROFILE */


/* ---------------------------------------------------------------------
   Exports of vg_malloc2.c
   ------------------------------------------------------------------ */

/* Allocation arenas.  
      SYMTAB    is for Valgrind's symbol table storage.
      CLIENT    is for the client's mallocs/frees.
      DEMANGLE  is for the C++ demangler.
      EXECTXT   is for storing ExeContexts.
      ERRCTXT   is for storing ErrContexts.
      PRIVATE   is for Valgrind general stuff.
      TRANSIENT is for very short-term use.  It should be empty
                in between uses.
   When adding a new arena, remember also to add it
   to ensure_mm_init(). 
*/
typedef Int ArenaId;

#define VG_N_ARENAS 7

#define VG_AR_PRIVATE   0    /* :: ArenaId */
#define VG_AR_SYMTAB    1    /* :: ArenaId */
#define VG_AR_CLIENT    2    /* :: ArenaId */
#define VG_AR_DEMANGLE  3    /* :: ArenaId */
#define VG_AR_EXECTXT   4    /* :: ArenaId */
#define VG_AR_ERRCTXT   5    /* :: ArenaId */
#define VG_AR_TRANSIENT 6    /* :: ArenaId */

extern void* VG_(malloc)  ( ArenaId arena, Int nbytes );
extern void  VG_(free)    ( ArenaId arena, void* ptr );
extern void* VG_(calloc)  ( ArenaId arena, Int nmemb, Int nbytes );
extern void* VG_(realloc) ( ArenaId arena, void* ptr, Int size );
extern void* VG_(malloc_aligned) ( ArenaId aid, Int req_alignB, 
                                                Int req_pszB );

extern void  VG_(mallocSanityCheckArena) ( ArenaId arena );
extern void  VG_(mallocSanityCheckAll)   ( void );

extern void  VG_(show_all_arena_stats) ( void );
extern Bool  VG_(is_empty_arena) ( ArenaId aid );


/* The red-zone size for the client.  This can be arbitrary, but
   unfortunately must be set at compile time. */
#define VG_AR_CLIENT_REDZONE_SZW 4

#define VG_AR_CLIENT_REDZONE_SZB \
   (VG_AR_CLIENT_REDZONE_SZW * VKI_BYTES_PER_WORD)


/* ---------------------------------------------------------------------
   Exports of vg_clientfuns.c
   ------------------------------------------------------------------ */

/* This doesn't export code or data that valgrind.so needs to link
   against.  However, the scheduler does need to know the following
   request codes.  A few, publically-visible, request codes are also
   defined in valgrind.h. */

#define VG_USERREQ__MALLOC              0x2001
#define VG_USERREQ__BUILTIN_NEW         0x2002
#define VG_USERREQ__BUILTIN_VEC_NEW     0x2003

#define VG_USERREQ__FREE                0x2004
#define VG_USERREQ__BUILTIN_DELETE      0x2005
#define VG_USERREQ__BUILTIN_VEC_DELETE  0x2006

#define VG_USERREQ__CALLOC              0x2007
#define VG_USERREQ__REALLOC             0x2008
#define VG_USERREQ__MEMALIGN            0x2009


#define VG_USERREQ__PTHREAD_CREATE          0x3001
#define VG_USERREQ__PTHREAD_JOIN            0x3002
#define VG_USERREQ__PTHREAD_GET_THREADID    0x3003
#define VG_USERREQ__PTHREAD_MUTEX_INIT      0x3004
#define VG_USERREQ__PTHREAD_MUTEX_LOCK      0x3005
#define VG_USERREQ__PTHREAD_MUTEX_UNLOCK    0x3006
#define VG_USERREQ__PTHREAD_MUTEX_DESTROY   0x3007
#define VG_USERREQ__PTHREAD_CANCEL          0x3008

/* Cosmetic ... */
#define VG_USERREQ__GET_PTHREAD_TRACE_LEVEL 0x3101

/* 
In vg_constants.h:
#define VG_USERREQ__SIGNAL_RETURNS          0x4001
#define VG_USERREQ__PTHREAD_RETURNS         0x4002
#define VG_USERREQ__SHUTDOWN_VALGRIND       0x4003
*/


/* ---------------------------------------------------------------------
   Constants pertaining to the simulated CPU state, VG_(baseBlock),
   which need to go here to avoid ugly circularities.
   ------------------------------------------------------------------ */

/* How big is the saved FPU state? */
#define VG_SIZE_OF_FPUSTATE 108
/* ... and in words ... */
#define VG_SIZE_OF_FPUSTATE_W ((VG_SIZE_OF_FPUSTATE+3)/4)


/* ---------------------------------------------------------------------
   Exports of vg_scheduler.c
   ------------------------------------------------------------------ */

/* ThreadIds are simply indices into the vg_threads[] array. */
typedef 
   UInt 
   ThreadId;

/* MutexIds are simply indices into the vg_mutexes[] array. */
typedef
   UInt
   MutexId;


#define VG_INVALID_THREADID ((ThreadId)(-1))

typedef
   enum { 
      VgTs_Empty,      /* this slot is not in use */
      VgTs_Runnable,   /* waiting to be scheduled */
      VgTs_WaitJoiner, /* waiting for someone to do join on me */
      VgTs_WaitJoinee, /* waiting for the thread I did join on */
      VgTs_WaitFD,     /* waiting for I/O completion on a fd */
      VgTs_WaitMX,     /* waiting on a mutex */
      VgTs_Sleeping    /* sleeping for a while */
   }
   ThreadStatus;
 
typedef
   struct {
      /* The thread identity is simply the index in vg_threads[].
         ThreadId == 0 is the root thread and has the special property
         that we don't try and allocate or deallocate its stack.  */

      /* Current scheduling status. */
      ThreadStatus status;

      /* Identity of joiner (thread who called join on me), or
         VG_INVALID_THREADID if no one asked to join yet. */
      ThreadId joiner;

      /* Identity of mutex we are waiting on, if .status == WaitMX. */
      MutexId waited_on_mid;

      /* If VgTs_Sleeping, this is when we should wake up. */
      ULong awaken_at;

      /* return value */
      void* retval;

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

      /* Saved machine context. */
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
      UInt m_fpu[VG_SIZE_OF_FPUSTATE_W];

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


/* Copy the specified thread's state into VG_(baseBlock) in
   preparation for running it. */
extern void VG_(load_thread_state)( ThreadId );

/* Save the specified thread's state back in VG_(baseBlock), and fill
   VG_(baseBlock) with junk, for sanity-check reasons. */
extern void VG_(save_thread_state)( ThreadId );

/* Get the thread state block for the specified thread. */
extern ThreadState* VG_(get_thread_state)( ThreadId );


/* Create, and add to TT/TC, the translation of a client basic
   block. */
extern void VG_(create_translation_for) ( Addr orig_addr );

/* Return codes from the scheduler. */
typedef
   enum { VgSrc_Deadlock, VgSrc_Shutdown, VgSrc_BbsDone }
   VgSchedReturnCode;

/* The scheduler. */
extern VgSchedReturnCode VG_(scheduler) ( void );

extern void VG_(scheduler_init) ( void );

extern void VG_(pp_sched_status) ( void );

/* vg_oursignalhandler() might longjmp().  Here's the jmp_buf. */
extern jmp_buf VG_(scheduler_jmpbuf);
/* ... and if so, here's the signal which caused it to do so. */
extern Int     VG_(longjmpd_on_signal);


/* We check that the initial stack, which we can't move, is allocated
   here.  VG_(scheduler_init) checks this.  
*/
#define VG_STARTUP_STACK_MASK  (Addr)0xBFFF8000


/* The red-zone size which we put at the bottom (highest address) of
   thread stacks, for paranoia reasons.  This can be arbitrary, and
   doesn't really need to be set at compile time. */
#define VG_AR_CLIENT_STACKBASE_REDZONE_SZW 4

#define VG_AR_CLIENT_STACKBASE_REDZONE_SZB \
   (VG_AR_CLIENT_STACKBASE_REDZONE_SZW * VKI_BYTES_PER_WORD)



/* ---------------------------------------------------------------------
   Exports of vg_signals.c
   ------------------------------------------------------------------ */

extern void VG_(sigstartup_actions) ( void );

extern void VG_(deliver_signals) ( ThreadId );
extern void VG_(unblock_host_signal) ( Int sigNo );


/* Fake system calls for signal handling. */
extern void VG_(do__NR_sigaction)     ( ThreadId tid );
extern void VG_(do__NR_sigprocmask)   ( Int how, vki_ksigset_t* set );

/* Modify the current thread's state once we have detected it is
   returning from a signal handler. */
extern Bool VG_(signal_returns) ( ThreadId );

/* Handy utilities to block/restore all host signals. */
extern void VG_(block_all_host_signals) 
                  ( /* OUT */ vki_ksigset_t* saved_mask );
extern void VG_(restore_host_signals) 
                  ( /* IN */ vki_ksigset_t* saved_mask );

/* ---------------------------------------------------------------------
   Exports of vg_mylibc.c
   ------------------------------------------------------------------ */


#define NULL ((void*)0)

extern void VG_(exit)( Int status )
            __attribute__ ((__noreturn__));

extern void VG_(printf) ( const char *format, ... );
/* too noisy ...  __attribute__ ((format (printf, 1, 2))) ; */

extern void VG_(sprintf) ( Char* buf, Char *format, ... );

extern void VG_(vprintf) ( void(*send)(Char), 
                          const Char *format, va_list vargs );

extern Bool VG_(isspace) ( Char c );

extern Int VG_(strlen) ( const Char* str );

extern Long VG_(atoll) ( Char* str );

extern Char* VG_(strcat) ( Char* dest, const Char* src );
extern Char* VG_(strncat) ( Char* dest, const Char* src, Int n );
extern Char* VG_(strpbrk) ( const Char* s, const Char* accept );

extern Char* VG_(strcpy) ( Char* dest, const Char* src );

extern Int VG_(strcmp)    ( const Char* s1, const Char* s2 );
extern Int VG_(strcmp_ws) ( const Char* s1, const Char* s2 );

extern Int VG_(strncmp)    ( const Char* s1, const Char* s2, Int nmax );
extern Int VG_(strncmp_ws) ( const Char* s1, const Char* s2, Int nmax );

extern Char* VG_(strstr) ( const Char* haystack, Char* needle );
extern Char* VG_(strchr) ( const Char* s, Char c );
extern Char* VG_(strdup) ( ArenaId aid, const Char* s);

extern Char* VG_(getenv) ( Char* name );
extern Int   VG_(getpid) ( void );
extern ULong VG_(read_microsecond_timer)( void );


extern Char VG_(toupper) ( Char c );

extern void VG_(strncpy_safely) ( Char* dest, const Char* src, Int ndest );

extern void VG_(strncpy) ( Char* dest, const Char* src, Int ndest );

extern Bool VG_(stringMatch) ( Char* pat, Char* str );


#define __STRING(x)  #x

/* Asserts are permanently enabled.  Hurrah! */
#define vg_assert(expr)                                               \
  ((void) ((expr) ? 0 :						      \
	   (VG_(assert_fail) (__STRING(expr),			      \
			      __FILE__, __LINE__,                     \
                              __PRETTY_FUNCTION__), 0)))

extern void VG_(assert_fail) ( Char* expr, Char* file, 
                               Int line, Char* fn )
            __attribute__ ((__noreturn__));

/* Reading files. */
extern Int  VG_(open_read) ( Char* pathname );
extern void VG_(close)     ( Int fd );
extern Int  VG_(read)      ( Int fd, void* buf, Int count);
extern Int  VG_(write)     ( Int fd, void* buf, Int count);

extern Int  VG_(fcntl) ( Int fd, Int cmd, Int arg );

extern Int VG_(select)( Int n, 
                        vki_fd_set* readfds, 
                        vki_fd_set* writefds, 
                        vki_fd_set* exceptfds, 
                        struct vki_timeval * timeout );
extern Int VG_(nanosleep)( const struct vki_timespec *req, 
                           struct vki_timespec *rem );


/* mmap-ery ... */
extern void* VG_(mmap)( void* start, UInt length, 
                        UInt prot, UInt flags, UInt fd, UInt offset );

extern Int  VG_(munmap)( void* start, Int length );


/* Print a (panic) message, and abort. */
extern void VG_(panic) ( Char* str )
            __attribute__ ((__noreturn__));

/* Get memory by anonymous mmap. */
void* VG_(get_memory_from_mmap) ( Int nBytes );

/* Signal stuff.  Note that these use the vk_ (kernel) structure
   definitions, which are different in places from those that glibc
   defines.  Since we're operating right at the kernel interface,
   glibc's view of the world is entirely irrelevant. */
extern Int VG_(ksigfillset)( vki_ksigset_t* set );
extern Int VG_(ksigemptyset)( vki_ksigset_t* set );
extern Int VG_(ksigaddset)( vki_ksigset_t* set, Int signum );

extern Int VG_(ksigprocmask)( Int how, const vki_ksigset_t* set, 
                                       vki_ksigset_t* oldset );
extern Int VG_(ksigaction) ( Int signum,  
                             const vki_ksigaction* act,  
                             vki_ksigaction* oldact );
extern Int VG_(ksigismember) ( vki_ksigset_t* set, Int signum );

extern Int VG_(ksignal)(Int signum, void (*sighandler)(Int));

extern Int VG_(ksigaltstack)( const vki_kstack_t* ss, vki_kstack_t* oss );



/* ---------------------------------------------------------------------
   Definitions for the JITter (vg_translate.c, vg_to_ucode.c,
   vg_from_ucode.c).
   ------------------------------------------------------------------ */

/* Tags which describe what operands are. */
typedef
   enum { TempReg=0, ArchReg=1, RealReg=2, 
          SpillNo=3, Literal=4, Lit16=5, 
          NoValue=6 }
   Tag;


/* Microinstruction opcodes. */
typedef
   enum {
      NOP,
      GET,
      PUT,
      LOAD,
      STORE,
      MOV,
      CMOV, /* Used for cmpxchg and cmov */
      WIDEN,
      JMP,

      /* Read/write the %EFLAGS register into a TempReg. */
      GETF, PUTF,

      ADD, ADC, AND, OR,  XOR, SUB, SBB,
      SHL, SHR, SAR, ROL, ROR, RCL, RCR,
      NOT, NEG, INC, DEC, BSWAP,
      CC2VAL,

      /* Not strictly needed, but useful for making better
         translations of address calculations. */
      LEA1,  /* reg2 := const + reg1 */
      LEA2,  /* reg3 := const + reg1 + reg2 * 1,2,4 or 8 */

      /* not for translating x86 calls -- only to call helpers */
      CALLM_S, CALLM_E, /* Mark start and end of push/pop sequences
                           for CALLM. */
      PUSH, POP, CLEAR, /* Add/remove/zap args for helpers. */
      CALLM,  /* call to a machine-code helper */

      /* Hack for translating string (REP-) insns.  Jump to literal if
         TempReg/RealReg is zero. */
      JIFZ,

      /* FPU ops which read/write mem or don't touch mem at all. */
      FPU_R,
      FPU_W,
      FPU,

      /* Advance the simulated %eip by some small (< 128) number. */
      INCEIP,

      /* uinstrs which are not needed for mere translation of x86 code,
         only for instrumentation of it. */
      LOADV,
      STOREV,
      GETV,
      PUTV,
      TESTV,
      SETV,
      /* Get/set the v-bit (and it is only one bit) for the simulated
         %eflags register. */
      GETVF,
      PUTVF,

      /* Do a unary or binary tag op.  Only for post-instrumented
         code.  For TAG1, first and only arg is a TempReg, and is both
         arg and result reg.  For TAG2, first arg is src, second is
         dst, in the normal way; both are TempRegs.  In both cases,
         3rd arg is a RiCHelper with a Lit16 tag.  This indicates
         which tag op to do. */
      TAG1,
      TAG2
   }
   Opcode;


/* Condition codes, observing the Intel encoding.  CondAlways is an
   extra. */
typedef
   enum {
      CondO      = 0,  /* overflow           */
      CondNO     = 1,  /* no overflow        */
      CondB      = 2,  /* below              */
      CondNB     = 3,  /* not below          */
      CondZ      = 4,  /* zero               */
      CondNZ     = 5,  /* not zero           */
      CondBE     = 6,  /* below or equal     */
      CondNBE    = 7,  /* not below or equal */
      CondS      = 8,  /* negative           */
      ConsNS     = 9,  /* not negative       */
      CondP      = 10, /* parity even        */
      CondNP     = 11, /* not parity even    */
      CondL      = 12, /* jump less          */
      CondNL     = 13, /* not less           */
      CondLE     = 14, /* less or equal      */
      CondNLE    = 15, /* not less or equal  */
      CondAlways = 16  /* Jump always        */
   } 
   Condcode;


/* Descriptions of additional properties of *unconditional* jumps. */
typedef
   enum {
     JmpBoring=0,   /* boring unconditional jump */
     JmpCall=1,     /* jump due to an x86 call insn */
     JmpRet=2,      /* jump due to an x86 ret insn */
     JmpSyscall=3,  /* do a system call, then jump */
     JmpClientReq=4 /* do a client request, then jump */
   }
   JmpKind;


/* Flags.  User-level code can only read/write O(verflow), S(ign),
   Z(ero), A(ux-carry), C(arry), P(arity), and may also write
   D(irection).  That's a total of 7 flags.  A FlagSet is a bitset,
   thusly: 
      76543210
       DOSZACP
   and bit 7 must always be zero since it is unused.
*/
typedef UChar FlagSet;

#define FlagD (1<<6)
#define FlagO (1<<5)
#define FlagS (1<<4)
#define FlagZ (1<<3)
#define FlagA (1<<2)
#define FlagC (1<<1)
#define FlagP (1<<0)

#define FlagsOSZACP (FlagO | FlagS | FlagZ | FlagA | FlagC | FlagP)
#define FlagsOSZAP  (FlagO | FlagS | FlagZ | FlagA |         FlagP)
#define FlagsOSZCP  (FlagO | FlagS | FlagZ |         FlagC | FlagP)
#define FlagsOSACP  (FlagO | FlagS |         FlagA | FlagC | FlagP)
#define FlagsSZACP  (        FlagS | FlagZ | FlagA | FlagC | FlagP)
#define FlagsSZAP   (        FlagS | FlagZ | FlagA |         FlagP)
#define FlagsZCP    (                FlagZ         | FlagC | FlagP)
#define FlagsOC     (FlagO |                         FlagC        )
#define FlagsAC     (                        FlagA | FlagC        )

#define FlagsALL    (FlagsOSZACP | FlagD)
#define FlagsEmpty  (FlagSet)0

#define VG_IS_FLAG_SUBSET(set1,set2) \
   (( ((FlagSet)set1) & ((FlagSet)set2) ) == ((FlagSet)set1) )

#define VG_UNION_FLAG_SETS(set1,set2) \
   ( ((FlagSet)set1) | ((FlagSet)set2) )



/* A Micro (u)-instruction. */
typedef
   struct {
      /* word 1 */
      UInt    lit32;      /* 32-bit literal */

      /* word 2 */
      UShort  val1;       /* first operand */
      UShort  val2;       /* second operand */

      /* word 3 */
      UShort  val3;       /* third operand */
      UChar   opcode;     /* opcode */
      UChar   size;       /* data transfer size */

      /* word 4 */
      FlagSet flags_r;    /* :: FlagSet */
      FlagSet flags_w;    /* :: FlagSet */
      UChar   tag1:4;     /* first  operand tag */
      UChar   tag2:4;     /* second operand tag */
      UChar   tag3:4;     /* third  operand tag */
      UChar   extra4b:4;  /* Spare field, used by WIDEN for src
                             -size, and by LEA2 for scale 
                             (1,2,4 or 8) */

      /* word 5 */
      UChar   cond;            /* condition, for jumps */
      Bool    smc_check:1;     /* do a smc test, if writes memory. */
      Bool    signed_widen:1;  /* signed or unsigned WIDEN ? */
      JmpKind jmpkind:3;       /* additional properties of unconditional JMP */
   }
   UInstr;


/* Expandable arrays of uinstrs. */
typedef 
   struct { 
      Int     used; 
      Int     size; 
      UInstr* instrs;
      Int     nextTemp;
   }
   UCodeBlock;

/* Refer to `the last instruction stuffed in', including as an
   lvalue. */
#define LAST_UINSTR(cb) (cb)->instrs[(cb)->used-1]

/* An invalid temporary number :-) */
#define INVALID_TEMPREG 999999999


/* ---------------------------------------------------------------------
   Exports of vg_demangle.c
   ------------------------------------------------------------------ */

extern void VG_(demangle) ( Char* orig, Char* result, Int result_size );


/* ---------------------------------------------------------------------
   Exports of vg_from_ucode.c
   ------------------------------------------------------------------ */

extern UChar* VG_(emit_code) ( UCodeBlock* cb, Int* nbytes );


/* ---------------------------------------------------------------------
   Exports of vg_to_ucode.c
   ------------------------------------------------------------------ */

extern Int   VG_(disBB)          ( UCodeBlock* cb, Addr eip0 );
extern Char* VG_(nameOfIntReg)   ( Int size, Int reg );
extern Char  VG_(nameOfIntSize)  ( Int size );
extern UInt  VG_(extend_s_8to32) ( UInt x );
extern Int   VG_(getNewTemp)     ( UCodeBlock* cb );
extern Int   VG_(getNewShadow)   ( UCodeBlock* cb );

#define SHADOW(tempreg)  ((tempreg)+1)


/* ---------------------------------------------------------------------
   Exports of vg_translate.c
   ------------------------------------------------------------------ */

extern void  VG_(translate)  ( Addr  orig_addr,
                               UInt* orig_size,
                               Addr* trans_addr,
                               UInt* trans_size );

extern void  VG_(emptyUInstr) ( UInstr* u );
extern void  VG_(newUInstr0) ( UCodeBlock* cb, Opcode opcode, Int sz );
extern void  VG_(newUInstr1) ( UCodeBlock* cb, Opcode opcode, Int sz,
                               Tag tag1, UInt val1 );
extern void  VG_(newUInstr2) ( UCodeBlock* cb, Opcode opcode, Int sz,
                               Tag tag1, UInt val1,
                               Tag tag2, UInt val2 );
extern void  VG_(newUInstr3) ( UCodeBlock* cb, Opcode opcode, Int sz,
                               Tag tag1, UInt val1,
                               Tag tag2, UInt val2,
                               Tag tag3, UInt val3 );
extern void VG_(setFlagRW) ( UInstr* u, 
                             FlagSet fr, FlagSet fw );

extern void VG_(setLiteralField) ( UCodeBlock* cb, UInt lit32 );
extern Bool VG_(anyFlagUse) ( UInstr* u );



extern void  VG_(ppUInstr)        ( Int instrNo, UInstr* u );
extern void  VG_(ppUCodeBlock)    ( UCodeBlock* cb, Char* title );

extern Char* VG_(nameCondcode)    ( Condcode cond );
extern Bool  VG_(saneUInstr)      ( Bool beforeRA, UInstr* u );
extern Bool  VG_(saneUCodeBlock)  ( UCodeBlock* cb );
extern Char* VG_(nameUOpcode)     ( Bool upper, Opcode opc );
extern Int   VG_(rankToRealRegNo) ( Int rank );

extern void* VG_(jitmalloc) ( Int nbytes );
extern void  VG_(jitfree)   ( void* ptr );


/* ---------------------------------------------------------------------
   Exports of vg_execontext.c.
   ------------------------------------------------------------------ */

/* Records the PC and a bit of the call chain.  The first 4 %eip
   values are used in comparisons do remove duplicate errors, and for
   comparing against suppression specifications.  The rest are purely
   informational (but often important). */

typedef
   struct _ExeContextRec {
      struct _ExeContextRec * next;
      /* The size of this array is VG_(clo_backtrace_size); at least
         2, at most VG_DEEPEST_BACKTRACE.  [0] is the current %eip,
         [1] is its caller, [2] is the caller of [1], etc. */
      Addr eips[0];
   }
   ExeContext;


/* Initialise the ExeContext storage mechanism. */
extern void VG_(init_ExeContext_storage) ( void );

/* Print stats (informational only). */
extern void VG_(show_ExeContext_stats) ( void );


/* Take a snapshot of the client's stack.  Search our collection of
   ExeContexts to see if we already have it, and if not, allocate a
   new one.  Either way, return a pointer to the context. */
extern ExeContext* VG_(get_ExeContext) ( Bool skip_top_frame,
                                         Addr eip, Addr ebp );

/* Print an ExeContext. */
extern void VG_(pp_ExeContext) ( ExeContext* );

/* Compare two ExeContexts, just comparing the top two callers. */
extern Bool VG_(eq_ExeContext_top2) ( ExeContext* e1, ExeContext* e2 );

/* Compare two ExeContexts, just comparing the top four callers. */
extern Bool VG_(eq_ExeContext_top4) ( ExeContext* e1, ExeContext* e2 );

/* Compare two ExeContexts, comparing all callers. */
extern Bool VG_(eq_ExeContext_all) ( ExeContext* e1, ExeContext* e2 );



/* ---------------------------------------------------------------------
   Exports of vg_errcontext.c.
   ------------------------------------------------------------------ */

extern void VG_(load_suppressions)    ( void );
extern void VG_(show_all_errors)      ( void );
extern void VG_(record_value_error)   ( Int size );
extern void VG_(record_free_error)    ( Addr a );
extern void VG_(record_freemismatch_error)    ( Addr a );
extern void VG_(record_address_error) ( Addr a, Int size, 
                                        Bool isWrite );
extern void VG_(record_jump_error) ( Addr a );

extern void VG_(record_param_err) ( ThreadState* tst,
                                    Addr a, 
                                    Bool isWriteLack, 
                                    Char* msg );
extern void VG_(record_user_err) ( ThreadState* tst,
                                   Addr a, Bool isWriteLack );


/* The classification of a faulting address. */
typedef 
   enum { Stack, Unknown, Freed, Mallocd, UserG, UserS }
   AddrKind;

/* Records info about a faulting address. */
typedef
   struct {
      /* ALL */
      AddrKind akind;
      /* Freed, Mallocd */
      Int blksize;
      /* Freed, Mallocd */
      Int rwoffset;
      /* Freed, Mallocd */
      ExeContext* lastchange;
   }
   AddrInfo;


/* ---------------------------------------------------------------------
   Exports of vg_clientperms.c
   ------------------------------------------------------------------ */

extern Bool VG_(client_perm_maybe_describe)( Addr a, AddrInfo* ai );

extern UInt VG_(handle_client_request) ( ThreadState* tst, UInt* arg_block );

extern void VG_(delete_client_stack_blocks_following_ESP_change) ( void );

extern void VG_(show_client_block_stats) ( void );


/* ---------------------------------------------------------------------
   Exports of vg_procselfmaps.c
   ------------------------------------------------------------------ */

extern 
void VG_(read_procselfmaps) (
   void (*record_mapping)( Addr, UInt, Char, Char, Char, UInt, UChar* )
);


/* ---------------------------------------------------------------------
   Exports of vg_symtab2.c
   ------------------------------------------------------------------ */

/* We assume the executable is loaded here ... can't really find
   out.  There is a hacky sanity check in vg_init_memory_audit()
   which should trip up most stupidities.
*/
#define VG_ASSUMED_EXE_BASE  (Addr)0x8048000

extern void VG_(read_symbols) ( void );
extern void VG_(mini_stack_dump) ( ExeContext* ec );
extern void VG_(what_obj_and_fun_is_this)
                                     ( Addr a,
                                       Char* obj_buf, Int n_obj_buf,
                                       Char* fun_buf, Int n_fun_buf );

extern void VG_(symtab_notify_munmap) ( Addr start, UInt length );


/* ---------------------------------------------------------------------
   Exports of vg_clientmalloc.c
   ------------------------------------------------------------------ */

typedef
   enum { 
      Vg_AllocMalloc = 0,
      Vg_AllocNew    = 1,
      Vg_AllocNewVec = 2 
   }
   VgAllocKind;

/* Description of a malloc'd chunk. */
typedef 
   struct _ShadowChunk {
      struct _ShadowChunk* next;
      ExeContext*   where;          /* where malloc'd/free'd */
      UInt          size : 30;      /* size requested.       */
      VgAllocKind   allockind : 2;  /* which wrapper did the allocation */
      Addr          data;           /* ptr to actual block.  */
   } 
   ShadowChunk;

extern void          VG_(clientmalloc_done) ( void );
extern void          VG_(describe_addr) ( Addr a, AddrInfo* ai );
extern ShadowChunk** VG_(get_malloc_shadows) ( /*OUT*/ UInt* n_shadows );

/* These are called from the scheduler, when it intercepts a user
   request. */
extern void* VG_(client_malloc)   ( ThreadState* tst, 
                                    UInt size, VgAllocKind kind );
extern void* VG_(client_memalign) ( ThreadState* tst, 
                                    UInt align, UInt size );
extern void  VG_(client_free)     ( ThreadState* tst, 
                                    void* ptrV, VgAllocKind  kind );
extern void* VG_(client_calloc)   ( ThreadState* tst, 
                                    UInt nmemb, UInt size1 );
extern void* VG_(client_realloc)  ( ThreadState* tst, 
                                    void* ptrV, UInt size_new );


/* ---------------------------------------------------------------------
   Exports of vg_main.c
   ------------------------------------------------------------------ */

/* A structure used as an intermediary when passing the simulated
   CPU's state to some assembly fragments, particularly system calls.
   Stuff is copied from baseBlock to here, the assembly magic runs,
   and then the inverse copy is done. */

extern UInt VG_(m_state_static) [8 /* int regs, in Intel order */ 
                                 + 1 /* %eflags */ 
                                 + 1 /* %eip */
                                 + VG_SIZE_OF_FPUSTATE_W /* FPU state */
                                ];

/* Handy fns for doing the copy back and forth. */
extern void VG_(copy_baseBlock_to_m_state_static) ( void );
extern void VG_(copy_m_state_static_to_baseBlock) ( void );

/* Called when some unhandleable client behaviour is detected.
   Prints a msg and aborts. */
extern void VG_(unimplemented) ( Char* msg );

/* The stack on which Valgrind runs.  We can't use the same stack as the
   simulatee -- that's an important design decision.  */
extern UInt VG_(stack)[10000];

/* Similarly, we have to ask for signals to be delivered on an
   alternative stack, since it is possible, although unlikely, that
   we'll have to run client code from inside the Valgrind-installed
   signal handler.  If this happens it will be done by
   vg_deliver_signal_immediately(). */
extern UInt VG_(sigstack)[10000];

/* Holds client's %esp at the point we gained control.  From this the
   client's argc, argv and envp are deduced. */
extern Addr   VG_(esp_at_startup);
extern Int    VG_(client_argc);
extern Char** VG_(client_argv);
extern Char** VG_(client_envp);

/* Remove valgrind.so from a LD_PRELOAD=... string so child processes
   don't get traced into. */
extern void   VG_(mash_LD_PRELOAD_string)( Char* ld_preload_str );

/* Something of a function looking for a home ... start up GDB.  This
   is called from VG_(swizzle_esp_then_start_GDB) and so runs on the
   *client's* stack.  This is necessary to give GDB the illusion that
   the client program really was running on the real cpu. */
extern void VG_(start_GDB_whilst_on_client_stack) ( void );

/* Spew out vast amounts of junk during JITting? */
extern Bool  VG_(disassemble);

/* 64-bit counter for the number of basic blocks done. */
extern ULong VG_(bbs_done);
/* 64-bit counter for the number of bbs to go before a debug exit. */
extern ULong VG_(bbs_to_go);

/* Counts downwards in vg_run_innerloop. */
extern UInt VG_(dispatch_ctr);

/* Is the client running on the simulated CPU or the real one? */
extern Bool VG_(running_on_simd_CPU); /* Initially False */

/* The current LRU epoch. */
extern UInt VG_(current_epoch);


/* --- Counters, for informational purposes only. --- */

/* Number of lookups which miss the fast tt helper. */
extern UInt VG_(tt_fast_misses);

/* Counts for LRU informational messages. */

/* Number and total o/t size of new translations this epoch. */
extern UInt VG_(this_epoch_in_count);
extern UInt VG_(this_epoch_in_osize);
extern UInt VG_(this_epoch_in_tsize);
/* Number and total o/t size of discarded translations this epoch. */
extern UInt VG_(this_epoch_out_count);
extern UInt VG_(this_epoch_out_osize);
extern UInt VG_(this_epoch_out_tsize);
/* Number and total o/t size of translations overall. */
extern UInt VG_(overall_in_count);
extern UInt VG_(overall_in_osize);
extern UInt VG_(overall_in_tsize);
/* Number and total o/t size of discards overall. */
extern UInt VG_(overall_out_count);
extern UInt VG_(overall_out_osize);
extern UInt VG_(overall_out_tsize);

/* The number of LRU-clearings of TT/TC. */
extern UInt VG_(number_of_lrus);

/* Counts pertaining to the register allocator. */

/* total number of uinstrs input to reg-alloc */
extern UInt VG_(uinstrs_prealloc);

/* total number of uinstrs added due to spill code */
extern UInt VG_(uinstrs_spill);

/* number of bbs requiring spill code */
extern UInt VG_(translations_needing_spill);

/* total of register ranks over all translations */
extern UInt VG_(total_reg_rank);

/* Counts pertaining to the self-modifying-code detection machinery. */

/* Total number of writes checked. */
//extern UInt VG_(smc_total_check4s);

/* Number of writes which the fast smc check couldn't show were
   harmless. */
extern UInt VG_(smc_cache_passed);

/* Numnber of writes which really did write on original code. */
extern UInt VG_(smc_fancy_passed);

/* Number of translations discarded as a result. */
//extern UInt VG_(smc_discard_count);

/* Counts pertaining to internal sanity checking. */
extern UInt VG_(sanity_fast_count);
extern UInt VG_(sanity_slow_count);

/* Counts pertaining to the scheduler. */
extern UInt VG_(num_scheduling_events_MINOR);
extern UInt VG_(num_scheduling_events_MAJOR);


/* ---------------------------------------------------------------------
   Exports of vg_memory.c
   ------------------------------------------------------------------ */

extern void VGM_(init_memory_audit) ( void );
extern Addr VGM_(curr_dataseg_end);
extern void VG_(show_reg_tags) ( void );
extern void VG_(detect_memory_leaks) ( void );
extern void VG_(done_prof_mem) ( void );

/* Set permissions for an address range.  Not speed-critical. */
extern void VGM_(make_noaccess) ( Addr a, UInt len );
extern void VGM_(make_writable) ( Addr a, UInt len );
extern void VGM_(make_readable) ( Addr a, UInt len );
/* Use with care! (read: use for shmat only) */
extern void VGM_(make_readwritable) ( Addr a, UInt len );
extern void VGM_(copy_address_range_perms) ( Addr src, Addr dst,
                                             UInt len );

/* Check permissions for an address range.  Not speed-critical. */
extern Bool VGM_(check_writable) ( Addr a, UInt len, Addr* bad_addr );
extern Bool VGM_(check_readable) ( Addr a, UInt len, Addr* bad_addr );
extern Bool VGM_(check_readable_asciiz) ( Addr a, Addr* bad_addr );

/* Sanity checks which may be done at any time.  Doing them at
   signal-delivery time turns out to be convenient. */
extern void VG_(do_sanity_checks) ( ThreadId tid, Bool force_expensive );
/* Very cheap ... */
extern Bool VG_(first_and_last_secondaries_look_plausible) ( void );

/* These functions are called from generated code. */
extern void VG_(helperc_STOREV4) ( UInt, Addr );
extern void VG_(helperc_STOREV2) ( UInt, Addr );
extern void VG_(helperc_STOREV1) ( UInt, Addr );

extern UInt VG_(helperc_LOADV1) ( Addr );
extern UInt VG_(helperc_LOADV2) ( Addr );
extern UInt VG_(helperc_LOADV4) ( Addr );

extern void VGM_(handle_esp_assignment) ( Addr new_espA );
extern void VGM_(fpu_write_check) ( Addr addr, Int size );
extern void VGM_(fpu_read_check)  ( Addr addr, Int size );

/* Safely (avoiding SIGSEGV / SIGBUS) scan the entire valid address
   space and pass the addresses and values of all addressible,
   defined, aligned words to notify_word.  This is the basis for the
   leak detector.  Returns the number of calls made to notify_word.  */
UInt VG_(scan_all_valid_memory) ( void (*notify_word)( Addr, UInt ) );

/* Is this address within some small distance below %ESP?  Used only
   for the --workaround-gcc296-bugs kludge. */
extern Bool VG_(is_just_below_ESP)( Addr esp, Addr aa );

/* Nasty kludgery to deal with applications which switch stacks,
   like netscape. */
#define VG_STACK_STARTS_AT      0xC0000000
#define VG_PLAUSIBLE_STACK_SIZE 8000000

extern Bool VG_(is_plausible_stack_addr) ( Addr );


/* ---------------------------------------------------------------------
   Exports of vg_syscall_mem.c
   ------------------------------------------------------------------ */

extern void VG_(perform_assumed_nonblocking_syscall) ( ThreadId tid );

extern void VG_(check_known_blocking_syscall) ( ThreadId tid, 
                                                Int syscallno,
                                                Int* /*IN*/ res );

extern Bool VG_(is_kerror) ( Int res );

#define KERNEL_DO_SYSCALL(thread_id, result_lvalue)      \
         VG_(load_thread_state)(thread_id);              \
         VG_(copy_baseBlock_to_m_state_static)();        \
         VG_(do_syscall)();                              \
         VG_(copy_m_state_static_to_baseBlock)();        \
         VG_(save_thread_state)(thread_id);              \
         result_lvalue = VG_(get_thread_state)(thread_id)->m_eax;


/* ---------------------------------------------------------------------
   Exports of vg_transtab.c
   ------------------------------------------------------------------ */

/* An entry in the translation table (TT). */
typedef
   struct {
      /* +0 */  Addr   orig_addr;
      /* +4 */  Addr   trans_addr;
      /* +8 */  UInt   mru_epoch;
      /* +12 */ UShort orig_size;
      /* +14 */ UShort trans_size;
   }
   TTEntry;

/* The number of basic blocks in an epoch (one age-step). */
#define VG_BBS_PER_EPOCH 20000

extern void VG_(get_tt_tc_used) ( UInt* tt_used, UInt* tc_used );
extern void VG_(maybe_do_lru_pass) ( void );
extern void VG_(flush_transtab) ( void );
extern Addr VG_(copy_to_transcache) ( Addr trans_addr, Int trans_size );
extern void VG_(add_to_trans_tab) ( TTEntry* tte );

extern void VG_(smc_mark_original) ( Addr original_addr, 
                                     Int original_len );

extern void VG_(init_transtab_and_SMC) ( void );

extern void VG_(sanity_check_tc_tt) ( void );
extern Addr VG_(search_transtab) ( Addr original_addr );

extern void VG_(invalidate_tt_fast)( void );


/* ---------------------------------------------------------------------
   Exports of vg_vtagops.c
   ------------------------------------------------------------------ */

/* Lists the names of value-tag operations used in instrumented
   code.  These are the third argument to TAG1 and TAG2 uinsns. */

typedef
   enum { 
     /* Unary. */
     VgT_PCast40, VgT_PCast20, VgT_PCast10,
     VgT_PCast01, VgT_PCast02, VgT_PCast04,

     VgT_PCast14, VgT_PCast12, VgT_PCast11,

     VgT_Left4, VgT_Left2, VgT_Left1,

     VgT_SWiden14, VgT_SWiden24, VgT_SWiden12,
     VgT_ZWiden14, VgT_ZWiden24, VgT_ZWiden12,

     /* Binary; 1st is rd; 2nd is rd+wr */
     VgT_UifU4, VgT_UifU2, VgT_UifU1, VgT_UifU0,
     VgT_DifD4, VgT_DifD2, VgT_DifD1,

     VgT_ImproveAND4_TQ, VgT_ImproveAND2_TQ, VgT_ImproveAND1_TQ, 
     VgT_ImproveOR4_TQ, VgT_ImproveOR2_TQ, VgT_ImproveOR1_TQ,
     VgT_DebugFn
   }
   VgTagOp;

extern Char* VG_(nameOfTagOp) ( VgTagOp );
extern UInt VG_(DebugFn) ( UInt a1, UInt a2 );


/* ---------------------------------------------------------------------
   Exports of vg_syscall.S
   ------------------------------------------------------------------ */

extern void VG_(do_syscall) ( void );


/* ---------------------------------------------------------------------
   Exports of vg_startup.S
   ------------------------------------------------------------------ */

extern void VG_(shutdown);
extern void VG_(switch_to_real_CPU) ( void );

extern void VG_(swizzle_esp_then_start_GDB) ( void );


/* ---------------------------------------------------------------------
   Exports of vg_dispatch.S
   ------------------------------------------------------------------ */

/* Run a thread for a (very short) while, until some event happens
   which means we need to defer to the scheduler. */
extern UInt VG_(run_innerloop) ( void );


/* ---------------------------------------------------------------------
   Exports of vg_helpers.S
   ------------------------------------------------------------------ */

/* SMC fast checks. */
extern void VG_(helper_smc_check4);

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

extern void VG_(helper_shldl);
extern void VG_(helper_shldw);
extern void VG_(helper_shrdl);
extern void VG_(helper_shrdw);

extern void VG_(helper_RDTSC);
extern void VG_(helper_CPUID);

extern void VG_(helper_bsf);
extern void VG_(helper_bsr);

extern void VG_(helper_fstsw_AX);
extern void VG_(helper_SAHF);
extern void VG_(helper_DAS);
extern void VG_(helper_DAA);

extern void VG_(helper_value_check4_fail);
extern void VG_(helper_value_check2_fail);
extern void VG_(helper_value_check1_fail);
extern void VG_(helper_value_check0_fail);

/* NOT FUNCTIONS; these are bogus RETURN ADDRESS. */
extern void VG_(signalreturn_bogusRA)( void );
extern void VG_(pthreadreturn_bogusRA)( void );


/* ---------------------------------------------------------------------
   The state of the simulated CPU.
   ------------------------------------------------------------------ */

/* This is the Intel register encoding. */
#define R_EAX 0
#define R_ECX 1
#define R_EDX 2
#define R_EBX 3
#define R_ESP 4
#define R_EBP 5
#define R_ESI 6
#define R_EDI 7

#define R_AL (0+R_EAX)
#define R_CL (0+R_ECX)
#define R_DL (0+R_EDX)
#define R_BL (0+R_EBX)
#define R_AH (4+R_EAX)
#define R_CH (4+R_ECX)
#define R_DH (4+R_EDX)
#define R_BH (4+R_EBX)


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

#define VG_BASEBLOCK_WORDS 200

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
extern Int VGOFF_(m_fpustate);
extern Int VGOFF_(m_eip);

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

/* Offsets of addresses of helper functions.  A "helper" function is
   one which is called from generated code. */

extern Int VGOFF_(helper_idiv_64_32);
extern Int VGOFF_(helper_div_64_32);
extern Int VGOFF_(helper_idiv_32_16);
extern Int VGOFF_(helper_div_32_16);
extern Int VGOFF_(helper_idiv_16_8);
extern Int VGOFF_(helper_div_16_8);

extern Int VGOFF_(helper_imul_32_64);
extern Int VGOFF_(helper_mul_32_64);
extern Int VGOFF_(helper_imul_16_32);
extern Int VGOFF_(helper_mul_16_32);
extern Int VGOFF_(helper_imul_8_16);
extern Int VGOFF_(helper_mul_8_16);

extern Int VGOFF_(helper_CLD);
extern Int VGOFF_(helper_STD);
extern Int VGOFF_(helper_get_dirflag);

extern Int VGOFF_(helper_shldl);
extern Int VGOFF_(helper_shldw);
extern Int VGOFF_(helper_shrdl);
extern Int VGOFF_(helper_shrdw);

extern Int VGOFF_(helper_RDTSC);
extern Int VGOFF_(helper_CPUID);

extern Int VGOFF_(helper_bsf);
extern Int VGOFF_(helper_bsr);

extern Int VGOFF_(helper_fstsw_AX);
extern Int VGOFF_(helper_SAHF);
extern Int VGOFF_(helper_DAS);
extern Int VGOFF_(helper_DAA);

extern Int VGOFF_(helper_value_check4_fail);
extern Int VGOFF_(helper_value_check2_fail);
extern Int VGOFF_(helper_value_check1_fail);
extern Int VGOFF_(helper_value_check0_fail);

extern Int VGOFF_(helperc_STOREV4); /* :: UInt -> Addr -> void */
extern Int VGOFF_(helperc_STOREV2); /* :: UInt -> Addr -> void */
extern Int VGOFF_(helperc_STOREV1); /* :: UInt -> Addr -> void */

extern Int VGOFF_(helperc_LOADV4); /* :: Addr -> UInt -> void */
extern Int VGOFF_(helperc_LOADV2); /* :: Addr -> UInt -> void */
extern Int VGOFF_(helperc_LOADV1); /* :: Addr -> UInt -> void */

extern Int VGOFF_(handle_esp_assignment); /* :: Addr -> void */
extern Int VGOFF_(fpu_write_check);       /* :: Addr -> Int -> void */
extern Int VGOFF_(fpu_read_check);        /* :: Addr -> Int -> void */



#endif /* ndef __VG_INCLUDE_H */


/* ---------------------------------------------------------------------
   Finally - autoconf-generated settings
   ------------------------------------------------------------------ */

#include "config.h"

/*--------------------------------------------------------------------*/
/*--- end                                             vg_include.h ---*/
/*--------------------------------------------------------------------*/
