
/*--------------------------------------------------------------------*/
/*--- The thread state.                     pub_core_threadstate.h ---*/
/*--------------------------------------------------------------------*/

/*
   This file is part of Valgrind, a dynamic binary instrumentation
   framework.

   Copyright (C) 2000-2013 Julian Seward
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

#ifndef __PUB_CORE_THREADSTATE_H
#define __PUB_CORE_THREADSTATE_H

//--------------------------------------------------------------------
// PURPOSE: This module defines the ThreadState type and the
// VG_(threads)[] data structure which holds all the important thread
// state.  It also defines some simple operations on the data structure
// that don't require any external help.  (m_scheduler does the complex
// stuff).
//--------------------------------------------------------------------

#include "pub_tool_threadstate.h"
#include "pub_core_libcsetjmp.h"   // VG_MINIMAL_JMP_BUF
#include "pub_core_vki.h"          // vki_sigset_t

/*------------------------------------------------------------*/
/*--- Types                                                ---*/
/*------------------------------------------------------------*/

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
      VgSrc_ExitThread,  /* just this thread is exiting */
      VgSrc_ExitProcess, /* this thread is exiting due to another thread
                            calling exit() */
      VgSrc_FatalSig	 /* Killed by the default action of a fatal
			    signal */
   }
   VgSchedReturnCode;


#if defined(VGA_x86)
   typedef VexGuestX86State   VexGuestArchState;
#elif defined(VGA_amd64)
   typedef VexGuestAMD64State VexGuestArchState;
#elif defined(VGA_ppc32)
   typedef VexGuestPPC32State VexGuestArchState;
#elif defined(VGA_ppc64)
   typedef VexGuestPPC64State VexGuestArchState;
#elif defined(VGA_arm)
   typedef VexGuestARMState   VexGuestArchState;
#elif defined(VGA_arm64)
   typedef VexGuestARM64State VexGuestArchState;
#elif defined(VGA_s390x)
   typedef VexGuestS390XState VexGuestArchState;
#elif defined(VGA_mips32)
   typedef VexGuestMIPS32State VexGuestArchState;
#elif defined(VGA_mips64)
   typedef VexGuestMIPS64State VexGuestArchState;
#else
#  error Unknown architecture
#endif

/* Forward declarations */
struct SyscallStatus;
struct SyscallArgs;

/* Architecture-specific thread state */
typedef 
   struct {
      /* --- BEGIN vex-mandated guest state --- */

      /* Note that for code generation reasons, we require that the
         guest state area, its two shadows, and the spill area, are
         16-aligned and have 16-aligned sizes, and there are no holes
         in between.  This is checked by do_pre_run_checks() in
         scheduler.c. */

      /* Saved machine context. */
      VexGuestArchState vex __attribute__((aligned(16)));

      /* Saved shadow context (2 copies). */
      VexGuestArchState vex_shadow1 __attribute__((aligned(16)));
      VexGuestArchState vex_shadow2 __attribute__((aligned(16)));

      /* Spill area. */
      UChar vex_spill[LibVEX_N_SPILL_BYTES] __attribute__((aligned(16)));

      /* --- END vex-mandated guest state --- */
   } 
   ThreadArchState;


/* OS-specific thread state.  IMPORTANT: if you add fields to this,
   you _must_ add code to os_state_clear() to initialise those
   fields. */
typedef
   struct {
      /* who we are */
      Int lwpid;        // PID of kernel task  (Darwin: Mach thread)
      Int threadgroup;  // thread group id

      ThreadId parent;  // parent tid (if any)

      /* runtime details */
      Addr valgrind_stack_base;    // Valgrind's stack (VgStack*)
      Addr valgrind_stack_init_SP; // starting value for SP

      /* exit details */
      Word exitcode; // in the case of exitgroup, set by someone else
      Int  fatalsig; // fatal signal

#     if defined(VGO_darwin)
      // Mach trap POST handler as chosen by PRE
      void (*post_mach_trap_fn)(ThreadId tid,
                                struct SyscallArgs *, struct SyscallStatus *);
    
      // This thread's pthread
      Addr pthread;
    
      // Argument passed when thread started
      Addr func_arg;

      // Synchronization between child thread and parent thread's POST wrapper
      semaphore_t child_go;
      semaphore_t child_done;

      // Workqueue re-entry 
      // (setjmp in PRE(workq_ops), longjmp in wqthread_hijack)
      // DDD: JRS fixme: this comment is no longer correct; wq_jmpbuf is
      // never used, and there is no such setjmp or longjmp pair.
      // I guess we could leave wq_jmpbuf_valid in place though, since
      // it does allow for an assertion in ML_(wqthread_continue_NORETURN).
      Bool wq_jmpbuf_valid;
      //jmp_buf wq_jmpbuf;

      // Values saved from transient Mach RPC messages
      Addr remote_port;  // destination for original message
      Int msgh_id;       // outgoing message id
      union {
         struct {
            Addr port;
         } mach_port;
         struct {
            Int right;
         } mach_port_allocate;
         struct {
            Addr port;
            Int right;
            Int delta;
         } mach_port_mod_refs;
         struct {
            Addr task;
            Addr name;
            Int disposition;
         } mach_port_insert_right;
         struct {
            Addr size;
            int flags;
         } vm_allocate;
         struct {
            Addr address;
            Addr size;
         } vm_deallocate;
         struct {
            Addr src;
            Addr dst;
            Addr size;
         } vm_copy;
         struct {
            Addr address;
            Addr size;
            int set_maximum;
            UWord new_protection;
         } vm_protect;
         struct {
            Addr addr;
            SizeT size;
         } vm_read;
         struct {
            ULong addr;
            ULong size;
         } mach_vm_read;
         struct {
            Addr addr;
            SizeT size;
            Addr data;
         } vm_read_overwrite;
         struct {
            Addr size;
            int copy;
            UWord protection;
         } vm_map;
         struct {
            Addr size;
         } vm_remap;
         struct {
            ULong size;
            int flags;
         } mach_vm_allocate;
         struct {
            ULong address;
            ULong size;
         } mach_vm_deallocate;
         struct {
            ULong address;
            ULong size;
            int set_maximum;
            unsigned int new_protection;
         } mach_vm_protect;
         struct {
            ULong size;
            int copy;
            UWord protection;
         } mach_vm_map;
         struct {
            Addr thread;
            UWord flavor;
         } thread_get_state;
         struct {
            Addr address;
         } io_connect_unmap_memory;
         struct {
            int which_port;
         } task_get_special_port;
         struct {
            char *service_name;
         } bootstrap_look_up;
         struct {
            vki_size_t size;
         } WindowServer_29828;
         struct {
            Int access_rights;
         } WindowServer_29831;
         struct {
            char *path;
         } io_registry_entry_from_path;
      } mach_args;
#     endif

   }
   ThreadOSstate;


/* Overall thread state */
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

   /* Client stacks.  When a thread slot is freed, we don't deallocate its
      stack; we just leave it lying around for the next use of the
      slot.  If the next use of the slot requires a larger stack,
      only then is the old one deallocated and a new one
      allocated. 

      For the main thread (threadid == 1), this mechanism doesn't
      apply.  We don't know the size of the stack since we didn't
      allocate it, and furthermore we never reallocate it. */

   /* The allocated size of this thread's stack */
   SizeT client_stack_szB;

   /* Address of the highest legitimate word in this stack.  This is
      used for error messages only -- not critical for execution
      correctness.  Is is set for all stacks, specifically including
      ThreadId == 1 (the main thread). */
   Addr client_stack_highest_word;

   /* Alternate signal stack */
   vki_stack_t altstack;

   /* OS-specific thread state */
   ThreadOSstate os_state;

   /* Error disablement level.  A counter which allows selectively
      disabling error reporting in threads.  When zero, reporting is
      enabled.  When nonzero, it is disabled.  This is controlled by
      the client request 'VG_USERREQ__CHANGE_ERR_DISABLEMENT'.  New
      threads are always created with this as zero (errors
      enabled). */
   UInt err_disablement_level;

   /* Per-thread jmp_buf to resume scheduler after a signal */
   Bool               sched_jmpbuf_valid;
   VG_MINIMAL_JMP_BUF(sched_jmpbuf);

   /* This thread's name. NULL, if no name. */
   HChar *thread_name;
}
ThreadState;


/*------------------------------------------------------------*/
/*--- The thread table.                                    ---*/
/*------------------------------------------------------------*/

/* A statically allocated array of threads.  NOTE: [0] is
   never used, to simplify the simulation of initialisers for
   LinuxThreads. */
extern ThreadState VG_(threads)[VG_N_THREADS];

// The running thread.  m_scheduler should be the only other module
// to write to this.
extern ThreadId VG_(running_tid);


/*------------------------------------------------------------*/
/*--- Basic operations on the thread table.                ---*/
/*------------------------------------------------------------*/

/* Initialize the m_threadstate module. */
void VG_(init_Threads)(void);

// Convert a ThreadStatus to a string.
const HChar* VG_(name_of_ThreadStatus) ( ThreadStatus status );

// Convert a VgSchedReturnCode to a string.
const HChar* VG_(name_of_VgSchedReturnCode) ( VgSchedReturnCode retcode );

/* Get the ThreadState for a particular thread */
extern ThreadState *VG_(get_ThreadState) ( ThreadId tid );

/* Check that tid is in range and denotes a non-Empty thread. */
extern Bool VG_(is_valid_tid) ( ThreadId tid );

/* Returns true if a thread is currently running (ie, has the CPU lock) */
extern Bool VG_(is_running_thread)(ThreadId tid);

/* Returns true if the thread is in the process of exiting */
extern Bool VG_(is_exiting)(ThreadId tid);

/* Return the number of non-dead Threads */
extern Int VG_(count_living_threads)(void);

/* Return the number of threads in VgTs_Runnable state */
extern Int VG_(count_runnable_threads)(void);

/* Given an LWP id (ie, real kernel thread id), find the corresponding
   ThreadId */
extern ThreadId VG_(lwpid_to_vgtid)(Int lwpid);

#endif   // __PUB_CORE_THREADSTATE_H

/*--------------------------------------------------------------------*/
/*--- end                                                          ---*/
/*--------------------------------------------------------------------*/
