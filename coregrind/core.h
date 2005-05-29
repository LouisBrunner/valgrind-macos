
/*--------------------------------------------------------------------*/
/*--- A header file for various private parts of Valgrind's core.  ---*/
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

#include "tool.h"          // tool stuff
#include "core_arch.h"     // arch-specific stuff,  eg. x86/core_arch.h

#include "core_platform.h" // platform-specific stuff,
                           //   eg. x86-linux/core_platform.h
#include "core_os.h"       // OS-specific stuff,    eg. linux/core_os.h

#include "pub_core_mallocfree.h"  // for type 'ArenaId'
#include "pub_core_stacktrace.h"  // for type 'StackTrace'

#include <setjmp.h>       /* for jmp_buf         */

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


/* Application-visible file descriptor limits */
extern Int VG_(fd_soft_limit);
extern Int VG_(fd_hard_limit);

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
   Exports of vg_intercept.c
   ------------------------------------------------------------------ */

/* These are the internal client request codes.  The publically-visible
   request codes are also defined in valgrind.h, and similar headers for
   some tools. */

/* Get the tool's malloc-wrapping functions */
#define VG_USERREQ__GET_MALLOCFUNCS	    0x3030

/* Internal equivalent of VALGRIND_PRINTF . */
#define VG_USERREQ__INTERNAL_PRINTF         0x3103

/* Denote the finish of __libc_freeres_wrapper(). 
   A synonym for exit. */
#define VG_USERREQ__LIBC_FREERES_DONE       0x3029

/* Intercept prefix stuff.  See
   coregrind/m_replace_malloc/vg_replace_malloc.c for details.
   Unfortunately the "_vgi_" literal is also hardcoded in that file, so if
   you change this one you must also change the other one. */
#define VG_INTERCEPT_PREFIX "_vgi_"
#define VG_INTERCEPT_PREFIX_LEN 5

/* Not sure what these are for.  Todo: clarify */
#define VG_WRAPPER_PREFIX "_vgw_"
#define VG_WRAPPER_PREFIX_LEN 5
#define VG_WRAPPER(name) _vgw_##name
#define VG_WRAPPER_ALIAS(name) "_vgw_" #name


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


#if defined(VGA_x86)
   typedef VexGuestX86State   VexGuestArchState;
#elif defined(VGA_amd64)
   typedef VexGuestAMD64State VexGuestArchState;
#elif defined(VGA_arm)
   typedef VexGuestARMState   VexGuestArchState;
#else
#  error Unknown architecture
#endif


typedef 
   struct {
      /* --- BEGIN vex-mandated guest state --- */

      /* Saved machine context. */
      VexGuestArchState vex;

      /* Saved shadow context. */
      VexGuestArchState vex_shadow;

      /* Spill area. */
      UChar vex_spill[LibVEX_N_SPILL_BYTES];

      /* --- END vex-mandated guest state --- */
   } 
   ThreadArchState;


typedef struct {
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
}
ThreadState;


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
extern void VG_(nuke_all_threads_except) ( ThreadId me,
                                           VgSchedReturnCode reason );

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

/* Highest signal the kernel will let us use */
extern Int VG_(max_signal);

extern void VG_(sigstartup_actions) ( void );

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

extern void VG_(clear_out_queued_signals) 
                  ( ThreadId tid, /* OUT */ vki_sigset_t* saved_mask );

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

/* ---------------------------------------------------------------------
   Exports of vg_mylibc.c
   ------------------------------------------------------------------ */

// Useful for making failing stubs, when certain things haven't yet been
// implemented.
#define I_die_here                                             \
   VG_(assert_fail) (/*isCore*//*BOGUS*/True,                  \
                     "Unimplemented functionality",            \
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
   Exports of vg_symtab2.c
   ------------------------------------------------------------------ */

typedef struct _Segment Segment;
typedef struct _CodeRedirect CodeRedirect;

extern Bool VG_(is_object_file)   ( const void *hdr );
extern SegInfo * VG_(read_seg_symbols) ( Segment *seg );
extern void VG_(seginfo_incref)   ( SegInfo * );
extern void VG_(seginfo_decref)   ( SegInfo *, Addr a );

extern Bool VG_(get_fnname_nodemangle)( Addr a, Char* fnname, Int n_fnname );

extern Addr VG_(reverse_search_one_symtab) ( const SegInfo* si, const Char* name );

extern Bool VG_(use_CFI_info) ( /*MOD*/Addr* ipP,
                                /*MOD*/Addr* spP,
                                /*MOD*/Addr* fpP,
                                Addr min_accessible,
                                Addr max_accessible );


/* ---------------------------------------------------------------------
   Exports of vg_main.c
   ------------------------------------------------------------------ */

/* Tell the logging mechanism whether we are logging to a file
   descriptor or a socket descriptor. */
extern Bool VG_(logging_to_socket);

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

/* Stats ... */
extern void VG_(print_scheduler_stats) ( void );

/* 64-bit counter for the number of basic blocks done. */
extern ULong VG_(bbs_done);


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

extern void VG_(sigreturn)(void);

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

// wait until all other threads are dead
extern void VGA_(reap_threads)(ThreadId self);

// handle an arch-specific client request
extern Bool VGA_(client_request)(ThreadId tid, UWord *args);

// Pointercheck
extern Bool VGA_(setup_pointercheck) ( void );

// For attaching the debugger
extern Int  VGA_(ptrace_setregs_from_tst) ( Int pid, ThreadArchState* arch );

// Used by leakcheck
extern void VGA_(mark_from_registers)(ThreadId tid, void (*marker)(Addr));

// Set up the libc freeres wrapper
extern void VGA_(intercept_libc_freeres_wrapper)(Addr);

// Clean up the client by calling before the final reports
extern void VGA_(final_tidyup)(ThreadId tid);

// Arch-specific client requests
extern Bool VGA_(client_requests)(ThreadId tid, UWord *args);


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
