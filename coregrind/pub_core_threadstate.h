
/*--------------------------------------------------------------------*/
/*--- The thread state.                     pub_core_threadstate.h ---*/
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

/* OS-specific thread state */
typedef struct {
   /* who we are */
   Int	lwpid;			// PID of kernel task
   Int	threadgroup;		// thread group id

   ThreadId parent;		// parent tid (if any)

   /* runtime details */
   Addr  valgrind_stack_base;	// Valgrind's stack base
   SizeT valgrind_stack_szB;	// stack size in bytes

   /* exit details */
   Int  exitcode;		// in the case of exitgroup, set by someone else
   Int  fatalsig;		// fatal signal
} os_thread_t;


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

// Convert a ThreadStatus to a string.
const HChar* VG_(name_of_ThreadStatus) ( ThreadStatus status );

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

/* Given an LWP id (ie, real kernel thread id), find the corresponding
   ThreadId */
extern ThreadId VG_(get_lwp_tid)(Int lwpid);

#endif   // __PUB_CORE_THREADSTATE_H

/*--------------------------------------------------------------------*/
/*--- end                                                          ---*/
/*--------------------------------------------------------------------*/
