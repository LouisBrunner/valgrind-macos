
/*--------------------------------------------------------------------*/
/*--- Implementation of POSIX signals.                             ---*/
/*---                                                 vg_signals.c ---*/
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


#include "vg_include.h"
#include "vg_constants.h"
#include "vg_unsafe.h"
#include "valgrind.h"  /* for VALGRIND_MAGIC_SEQUENCE */

/* ---------------------------------------------------------------------
   Signal state for this process.
   ------------------------------------------------------------------ */

/* For each signal, the current action.  Is NULL if the client hasn't
   asked to handle the signal.  Consequently, we expect never to
   receive a signal for which the corresponding handler is NULL. */
void* VG_(sighandler)[VKI_KNSIG];


/* For each signal, either:
   -- VG_SIGIDLE if not pending and not running
   -- Handler address if pending
   -- VG_SIGRUNNING if the handler is running and hasn't (returned or 
      unblocked the signal using sigprocmask following a longjmp out 
      of the handler).
 */
#define VG_SIGIDLE    ((void*)0)
#define VG_SIGRUNNING ((void*)1)

void* VG_(sigpending)[VKI_KNSIG];


/* For each signal that we have a handler for (ie, for those for which
   the VG_(sighandler) entry is non-NULL), record whether or not the
   client asked for syscalls to be restartable (SA_RESTART) if
   interrupted by this signal.  We need to consult this when a signal
   returns, if it should happen that the signal which we delivered has
   interrupted a system call. */
Bool vg_sig_sarestart[VKI_KNSIG];


/* ---------------------------------------------------------------------
   Handy utilities to block/restore all host signals.
   ------------------------------------------------------------------ */

/* Block all host signals, dumping the old mask in *saved_mask. */
void VG_(block_all_host_signals) ( /* OUT */ vki_ksigset_t* saved_mask )
{
   Int           ret;
   vki_ksigset_t block_procmask;
   VG_(ksigfillset)(&block_procmask);
   ret = VG_(ksigprocmask)
            (VKI_SIG_SETMASK, &block_procmask, saved_mask);
   vg_assert(ret == 0);
}

/* Restore the blocking mask using the supplied saved one. */
void VG_(restore_host_signals) ( /* IN */ vki_ksigset_t* saved_mask )
{
   Int ret;
   ret = VG_(ksigprocmask)(VKI_SIG_SETMASK, saved_mask, NULL);
   vg_assert(ret == 0);
}


/* ---------------------------------------------------------------------
   The signal simulation proper.  A simplified version of what the 
   Linux kernel does.
   ------------------------------------------------------------------ */

/* A structure in which to save the application's registers
   during the execution of signal handlers. */

typedef
   struct {
      /* These are parameters to the signal handler. */
      UInt retaddr;   /* Sig handler's (bogus) return address */
      Int  sigNo;     /* The arg to the sig handler.  */
      Addr psigInfo;  /* ptr to siginfo_t; NULL for now. */
      Addr puContext; /* ptr to ucontext; NULL for now. */
      /* Sanity check word. */
      UInt magicPI;
      /* Saved processor state. */
      UInt fpustate[VG_SIZE_OF_FPUSTATE_W];
      UInt eax;
      UInt ecx;
      UInt edx;
      UInt ebx;
      UInt ebp;
      UInt esp;
      UInt esi;
      UInt edi;
      Addr eip;
      UInt eflags;
      /* Scheduler-private stuff: what was the thread's status prior to
         delivering this signal? */
      ThreadStatus status;
      /* Sanity check word.  Is the highest-addressed word; do not
         move!*/
      UInt magicE;
   }
   VgSigFrame;



/* Set up a stack frame (VgSigContext) for the client's signal
   handler.  This includes the signal number and a bogus return
   address.  */
static
void vg_push_signal_frame ( ThreadId tid, int sigNo )
{
   Int          i;
   Addr         esp;
   VgSigFrame*  frame;
   ThreadState* tst;

   tst = VG_(get_thread_state)(tid);
   esp = tst->m_esp;

   esp -= sizeof(VgSigFrame);
   frame = (VgSigFrame*)esp;
   /* Assert that the frame is placed correctly. */
   vg_assert( (sizeof(VgSigFrame) & 0x3) == 0 );
   vg_assert( ((Char*)(&frame->magicE)) + sizeof(UInt) 
              == ((Char*)(tst->m_esp)) );

   frame->retaddr    = (UInt)(&VG_(signalreturn_bogusRA));
   frame->sigNo      = sigNo;
   frame->psigInfo   = (Addr)NULL;
   frame->puContext  = (Addr)NULL;
   frame->magicPI    = 0x31415927;

   for (i = 0; i < VG_SIZE_OF_FPUSTATE_W; i++)
      frame->fpustate[i] = tst->m_fpu[i];

   frame->eax        = tst->m_eax;
   frame->ecx        = tst->m_ecx;
   frame->edx        = tst->m_edx;
   frame->ebx        = tst->m_ebx;
   frame->ebp        = tst->m_ebp;
   frame->esp        = tst->m_esp;
   frame->esi        = tst->m_esi;
   frame->edi        = tst->m_edi;
   frame->eip        = tst->m_eip;
   frame->eflags     = tst->m_eflags;

   frame->status     = tst->status;

   frame->magicE     = 0x27182818;

   /* Set the thread so it will next run the handler. */
   tst->m_esp  = esp;
   tst->m_eip  = (Addr)VG_(sigpending)[sigNo];
   /* This thread needs to be marked runnable, but we leave that the
      caller to do. */

   /* Make sigNo and retaddr fields readable -- at 0(%ESP) and 4(%ESP) */
   if (VG_(clo_instrument)) {
      VGM_(make_readable) ( ((Addr)esp)+0 ,4 );
      VGM_(make_readable) ( ((Addr)esp)+4 ,4 );
   }

   /* 
   VG_(printf)("pushed signal frame; %%ESP now = %p, next %%EBP = %p\n", 
               esp, tst->m_eip);
   */
}


/* Clear the signal frame created by vg_push_signal_frame, restore the
   simulated machine state, and return the signal number that the
   frame was for. */
static
Int vg_pop_signal_frame ( ThreadId tid )
{
   Addr          esp;
   Int           sigNo, i;
   VgSigFrame*   frame;
   ThreadState*  tst;

   tst = VG_(get_thread_state)(tid);

   /* Correctly reestablish the frame base address. */
   esp   = tst->m_esp;
   frame = (VgSigFrame*)
              (esp -4 /* because the handler's RET pops the RA */
                  +20 /* because signalreturn_bogusRA pushes 5 words */);

   vg_assert(frame->magicPI == 0x31415927);
   vg_assert(frame->magicE  == 0x27182818);
   if (VG_(clo_trace_signals))
      VG_(message)(Vg_DebugMsg, "vg_pop_signal_frame: valid magic");

   /* restore machine state */
   for (i = 0; i < VG_SIZE_OF_FPUSTATE_W; i++)
      tst->m_fpu[i] = frame->fpustate[i];

   /* Mark the frame structure as nonaccessible.  Has to happen
      _before_ vg_m_state.m_esp is given a new value.
      handle_esp_assignment reads %ESP from baseBlock, so we park it
      there first.  Re-place the junk there afterwards. */
   if (VG_(clo_instrument)) {
      vg_assert(VG_(baseBlock)[VGOFF_(m_esp)] == 0xDEADBEEF);
      VG_(baseBlock)[VGOFF_(m_esp)] = tst->m_esp;
      VGM_(handle_esp_assignment) ( frame->esp );
      VG_(baseBlock)[VGOFF_(m_esp)] = 0xDEADBEEF;
   }

   /* Restore machine state from the saved context. */
   tst->m_eax     = frame->eax;
   tst->m_ecx     = frame->ecx;
   tst->m_edx     = frame->edx;
   tst->m_ebx     = frame->ebx;
   tst->m_ebp     = frame->ebp;
   tst->m_esp     = frame->esp;
   tst->m_esi     = frame->esi;
   tst->m_edi     = frame->edi;
   tst->m_eflags  = frame->eflags;
   tst->m_eip     = frame->eip;
   sigNo          = frame->sigNo;

   /* And restore the thread's status to what it was before the signal
      was delivered. */
   tst->status    = frame->status;

   return sigNo;
}


/* A handler is returning.  Restore the machine state from the stacked
   VgSigContext and continue with whatever was going on before the
   handler ran.  Returns the SA_RESTART syscall-restartability-status
   of the delivered signal. */

Bool VG_(signal_returns) ( ThreadId tid )
{
   Int            sigNo;
   vki_ksigset_t  saved_procmask;

   /* Block host signals ... */
   VG_(block_all_host_signals)( &saved_procmask );

   /* Pop the signal frame and restore tid's status to what it was
      before the signal was delivered. */
   sigNo = vg_pop_signal_frame(tid);

   /* You would have thought that the following assertion made sense
      here:

         vg_assert(vg_sigpending[sigNo] == VG_SIGRUNNING);

      Alas, you would be wrong.  If a sigprocmask has been intercepted
      and it unblocks this signal, then vg_sigpending[sigNo] will
      either be VG_SIGIDLE, or (worse) another instance of it will
      already have arrived, so that the stored value is that of the
      handler.

      Note that these anomalies can only occur when a signal handler
      unblocks its own signal inside itself AND THEN RETURNS anyway
      (which seems a bizarre thing to do).

      Ho Hum.  This seems like a race condition which surely isn't
      handled correctly.  */

   vg_assert(sigNo >= 1 && sigNo < VKI_KNSIG);
   VG_(sigpending)[sigNo] = VG_SIGIDLE;

   /* Unlock and return. */
   VG_(restore_host_signals)( &saved_procmask );

   /* Scheduler now can resume this thread, or perhaps some other.
      Tell the scheduler whether or not any syscall interrupted by
      this signal should be restarted, if possible, or no. */
   return vg_sig_sarestart[sigNo];
}


/* Deliver all pending signals, by building stack frames for their
   handlers.  Return True if any signals were delivered. */
Bool VG_(deliver_signals) ( ThreadId tid )
{
   vki_ksigset_t  saved_procmask;
   Int            sigNo;
   Bool           found;
 
   /* A cheap check.  We don't need to have exclusive access
      to the queue, because in the worst case, vg_oursignalhandler
      will add signals, causing us to return, thinking there
      are no signals to deliver, when in fact there are some.
      A subsequent call here will handle the signal(s) we missed.
   */
   found = False;
   for (sigNo = 1; sigNo < VKI_KNSIG; sigNo++)
      if (VG_(sigpending)[sigNo] != VG_SIGIDLE &&
          VG_(sigpending)[sigNo] != VG_SIGRUNNING) found = True;

   if (!found) return False;

   /* Now we have to do it properly.  Get exclusive access by
      blocking all the host's signals.  That means vg_oursignalhandler
      can't run whilst we are messing with stuff.
   */
   VG_(block_all_host_signals)( &saved_procmask );

   /* Look for signals to deliver ... */
   for (sigNo = 1; sigNo < VKI_KNSIG; sigNo++) {
      if (VG_(sigpending)[sigNo] == VG_SIGIDLE ||
          VG_(sigpending)[sigNo] == VG_SIGRUNNING) continue;

      if (VG_(clo_trace_signals))
         VG_(message)(Vg_DebugMsg,"delivering signal %d", sigNo );

      /* Create a signal delivery frame, and set the client's %ESP and
         %EIP so that when execution continues, we will enter the
         signal handler with the frame on top of the client's stack,
         as it expects. */
      vg_push_signal_frame ( tid, sigNo );
      VG_(get_thread_state)(tid)->status = VgTs_Runnable;
      
      /* Signify that the signal has been delivered. */
      VG_(sigpending)[sigNo] = VG_SIGRUNNING;
   }

   /* Unlock and return. */
   VG_(restore_host_signals)( &saved_procmask );
   return True;
}


/* Receive a signal from the host, and either discard it or park it in
   the queue of pending signals.  All other signals will be blocked
   when this handler runs.  Runs with all host signals blocked, so as
   to have mutual exclusion when adding stuff to the queue. */

static void VG_(oursignalhandler) ( Int sigNo )
{
   Int           dummy_local;
   vki_ksigset_t saved_procmask;

   if (VG_(clo_trace_signals)) {
      VG_(start_msg)(Vg_DebugMsg);
      VG_(add_to_msg)("signal %d arrived ... ", sigNo );
   }
   vg_assert(sigNo >= 1 && sigNo < VKI_KNSIG);

   /* Sanity check.  Ensure we're really running on the signal stack
      we asked for. */
   if ( !(
            ((Char*)(&(VG_(sigstack)[0])) <= (Char*)(&dummy_local))
            &&
            ((Char*)(&dummy_local) < (Char*)(&(VG_(sigstack)[10000])))
         )
        ) {
     VG_(message)(Vg_DebugMsg, 
        "FATAL: signal delivered on the wrong stack?!");
     VG_(message)(Vg_DebugMsg, 
        "A possible workaround follows.  Please tell me");
     VG_(message)(Vg_DebugMsg, 
        "(jseward@acm.org) if the suggested workaround doesn't help.");
     VG_(unimplemented)
        ("support for progs compiled with -p/-pg; "
         "rebuild your prog without -p/-pg");
   }

   vg_assert((Char*)(&(VG_(sigstack)[0])) <= (Char*)(&dummy_local));
   vg_assert((Char*)(&dummy_local) < (Char*)(&(VG_(sigstack)[10000])));

   if (sigNo == VKI_SIGABRT && VG_(sighandler)[sigNo] == NULL) {
      /* We get here if SIGABRT is delivered and the client hasn't
         asked to catch it.  The aim is to exit in a controlled
         manner. */
      if (VG_(clo_trace_signals)) {
         VG_(add_to_msg)("catching SIGABRT");
         VG_(end_msg)();
      }
      VG_(ksignal)(VKI_SIGABRT, VKI_SIG_DFL);
      VG_(longjmpd_on_signal) = VKI_SIGABRT;
      __builtin_longjmp(VG_(scheduler_jmpbuf),1);
   }

   VG_(block_all_host_signals)( &saved_procmask );

   if (VG_(sighandler)[sigNo] == NULL) {
      if (VG_(clo_trace_signals)) {
         VG_(add_to_msg)("unexpected!");
         VG_(end_msg)();
      }
      /* Note: we panic with all signals blocked here.  Don't think
         that matters. */
      VG_(panic)("vg_oursignalhandler: unexpected signal");
   }

   /* Decide what to do with it. */
   if (VG_(sigpending)[sigNo] == VG_SIGRUNNING) {
       /* Already running; ignore it. */
      if (VG_(clo_trace_signals)) {
         VG_(add_to_msg)("already running; discarded" );
         VG_(end_msg)();
      }
   }
   else
   if (VG_(sigpending)[sigNo] != VG_SIGRUNNING && 
       VG_(sigpending)[sigNo] != VG_SIGIDLE) {
      /* Not running and not idle == pending; ignore it. */
      if (VG_(clo_trace_signals)) {
         VG_(add_to_msg)("already pending; discarded" );
         VG_(end_msg)();
      }
   } 
   else {
      /* Ok, we'd better deliver it to the client. */
      vg_assert(VG_(sigpending)[sigNo] == VG_SIGIDLE);
      /* Queue it up for delivery at some point in the future. */
      VG_(sigpending)[sigNo] = VG_(sighandler)[sigNo];
      if (VG_(clo_trace_signals)) {
         VG_(add_to_msg)("queued" );
         VG_(end_msg)();
      }
   }

   /* We've finished messing with the queue, so re-enable host
      signals. */
   VG_(restore_host_signals)( &saved_procmask );

   if (sigNo == VKI_SIGSEGV || sigNo == VKI_SIGBUS 
       || sigNo == VKI_SIGFPE || sigNo == VKI_SIGILL) {
      /* Can't continue; must longjmp back to the scheduler and thus
         enter the sighandler immediately. */
      VG_(longjmpd_on_signal) = sigNo;
      __builtin_longjmp(VG_(scheduler_jmpbuf),1);
   }
}


/* The outer insn loop calls here to reenable a host signal if
   vg_oursighandler longjmp'd.
*/
void VG_(unblock_host_signal) ( Int sigNo )
{
   Int ret;
   vki_ksigset_t set;
   VG_(ksigemptyset)(&set);
   ret = VG_(ksigaddset)(&set,sigNo);
   vg_assert(ret == 0);
   ret = VG_(ksigprocmask)(VKI_SIG_UNBLOCK,&set,NULL);
   vg_assert(ret == 0);
}


static __attribute((unused))
void pp_vg_ksigaction ( vki_ksigaction* sa )
{
   Int i;
   VG_(printf)("vg_ksigaction: handler %p, flags 0x%x, restorer %p\n", 
               sa->ksa_handler, (UInt)sa->ksa_flags, sa->ksa_restorer);
   VG_(printf)("vg_ksigaction: { ");
   for (i = 1; i < VKI_KNSIG; i++)
      if (VG_(ksigismember(&(sa->ksa_mask),i)))
         VG_(printf)("%d ", i);
   VG_(printf)("}\n");
}


/* Copy the process' real signal state to the sim state.  Whilst
   doing this, block all real signals.
*/
void VG_(sigstartup_actions) ( void )
{
   Int i, ret;

   vki_ksigset_t  saved_procmask;
   vki_kstack_t   altstack_info;
   vki_ksigaction sa;

   /* VG_(printf)("SIGSTARTUP\n"); */
   /* Block all signals.  
      saved_procmask remembers the previous mask. */
   VG_(block_all_host_signals)( &saved_procmask );

   /* Register an alternative stack for our own signal handler to run
      on. */
   altstack_info.ss_sp = &(VG_(sigstack)[0]);
   altstack_info.ss_size = 10000 * sizeof(UInt);
   altstack_info.ss_flags = 0;
   ret = VG_(ksigaltstack)(&altstack_info, NULL);
   if (ret != 0) {
      VG_(panic)(
         "vg_sigstartup_actions: couldn't install alternative sigstack");
   }
   if (VG_(clo_trace_signals)) {
      VG_(message)(Vg_DebugExtraMsg, 
         "vg_sigstartup_actions: sigstack installed ok");
   }

   /* Set initial state for the signal simulation. */
   for (i = 1; i < VKI_KNSIG; i++) {
      VG_(sighandler)[i] = NULL;
      VG_(sigpending)[i] = NULL;
      vg_sig_sarestart[i] = True; /* An easy default */
   }

   for (i = 1; i < VKI_KNSIG; i++) {

      /* Get the old host action */
      ret = VG_(ksigaction)(i, NULL, &sa);
      vg_assert(ret == 0);

      /* If there's already a handler set, record it, then route the
         signal through to our handler. */
      if (sa.ksa_handler != VKI_SIG_IGN && sa.ksa_handler != VKI_SIG_DFL) {
         if (VG_(clo_trace_signals))
            VG_(printf)("snaffling handler 0x%x for signal %d\n", 
                        (Addr)(sa.ksa_handler), i );
         if ((sa.ksa_flags & VKI_SA_ONSTACK) != 0)
            VG_(unimplemented)
               ("signals on an alternative stack (SA_ONSTACK)");

         VG_(sighandler)[i] = sa.ksa_handler;
         sa.ksa_handler = &VG_(oursignalhandler);
	 /* Save the restart status, then set it to restartable. */
	 vg_sig_sarestart[i] 
            = (sa.ksa_flags & VKI_SA_RESTART) ? True : False;
         sa.ksa_flags |= VKI_SA_RESTART;

         ret = VG_(ksigaction)(i, &sa, NULL);
         vg_assert(ret == 0);
      }
   }

   VG_(ksignal)(VKI_SIGABRT, &VG_(oursignalhandler));

   /* Finally, restore the blocking mask. */
   VG_(restore_host_signals)( &saved_procmask );
}


/* Copy the process' sim signal state to the real state,
   for when we transfer from the simulated to real CPU.
   PROBLEM: what if we're running a signal handler when we
   get here?  Hmm.
   I guess we wind up in vg_signalreturn_bogusRA, *or* the
   handler has done/will do a longjmp, in which case we're ok.

   It is important (see vg_startup.S) that this proc does not
   change the state of the real FPU, since it is called when
   running the program on the real CPU.
*/
void VG_(sigshutdown_actions) ( void )
{
   Int i, ret;

   vki_ksigset_t  saved_procmask;
   vki_ksigaction sa;

   VG_(block_all_host_signals)( &saved_procmask );

   /* copy the sim signal actions to the real ones. */
   /* Hmm, this isn't accurate.  Doesn't properly restore the
      SA_RESTART flag nor SA_ONSTACK. */
   for (i = 1; i < VKI_KNSIG; i++) {
      if (i == VKI_SIGKILL || i == VKI_SIGSTOP) continue;
      if (VG_(sighandler)[i] == NULL) continue;
      ret = VG_(ksigaction)(i, NULL, &sa);
      vg_assert(ret == 0);
      sa.ksa_handler = VG_(sighandler)[i];
      ret = VG_(ksigaction)(i, &sa, NULL);      
   }

   VG_(restore_host_signals)( &saved_procmask );
}


/* ---------------------------------------------------------------------
   Handle signal-related syscalls from the simulatee.
   ------------------------------------------------------------------ */

/* Do more error checking? */
void VG_(do__NR_sigaction) ( ThreadId tid )
{
   UInt res;
   void* our_old_handler;
   vki_ksigaction* new_action;
   vki_ksigaction* old_action;
   ThreadState* tst = VG_(get_thread_state)( tid );
   UInt param1 = tst->m_ebx; /* int sigNo */
   UInt param2 = tst->m_ecx; /* k_sigaction* new_action */
   UInt param3 = tst->m_edx; /* k_sigaction* old_action */
   new_action  = (vki_ksigaction*)param2;
   old_action  = (vki_ksigaction*)param3;

   if (VG_(clo_trace_signals))
      VG_(message)(Vg_DebugExtraMsg, 
         "__NR_sigaction: sigNo %d, "
         "new 0x%x, old 0x%x, new flags 0x%x",
         param1,(UInt)new_action,(UInt)old_action,
         (UInt)(new_action ? new_action->ksa_flags : 0) );
   /* VG_(ppSigProcMask)(); */

   /* Rule out various error conditions.  The aim is to ensure that if
      the call is passed to the kernel it will definitely succeed. */

   /* Reject out-of-range signal numbers. */
   if (param1 < 1 || param1 >= VKI_KNSIG) goto bad_signo;

   /* Reject attempts to set a handler (or set ignore) for SIGKILL. */
   if ( (param1 == VKI_SIGKILL || param1 == VKI_SIGSTOP)
       && new_action
       && new_action->ksa_handler != VKI_SIG_DFL)
      goto bad_sigkill;

   our_old_handler = VG_(sighandler)[param1];
   /* VG_(printf)("old handler = 0x%x\n", our_old_handler); */
   /* If a new handler has been specified, mess with its handler. */
   if (new_action) {
      if (new_action->ksa_handler == VKI_SIG_IGN ||
          new_action->ksa_handler == VKI_SIG_DFL) {
         VG_(sighandler)[param1] = NULL; 
         VG_(sigpending)[param1] = NULL;
         /* Dangerous!  Could lose signals like this. */
      } else {
         /* VG_(printf)("new handler = 0x%x\n", new_action->ksa_handler); */
         /* The client isn't allowed to use an alternative signal
            stack.  We, however, must. */
         if ((new_action->ksa_flags & VKI_SA_ONSTACK) != 0)
            VG_(unimplemented)
               ("signals on an alternative stack (SA_ONSTACK)");
         new_action->ksa_flags |= VKI_SA_ONSTACK;
         VG_(sighandler)[param1] = new_action->ksa_handler;
	 vg_sig_sarestart[param1] 
            = (new_action->ksa_flags & VKI_SA_RESTART) ? True : False;
         new_action->ksa_flags |= VKI_SA_RESTART;
         new_action->ksa_handler = &VG_(oursignalhandler);
      }
   }

   KERNEL_DO_SYSCALL(tid,res);
   /* VG_(printf)("RES = %d\n", res); */

   /* If the client asks for the old handler, maintain our fiction
      by stuffing in the handler it thought it asked for ... */
   if (old_action) {
      if (old_action->ksa_handler == VKI_SIG_IGN ||
          old_action->ksa_handler == VKI_SIG_DFL) {
         /* No old action; we should have a NULL handler. */
         vg_assert(our_old_handler == NULL);
      } else {
         /* There's a handler. */
         if (param1 != VKI_SIGKILL && param1 != VKI_SIGABRT) {
            vg_assert(old_action->ksa_handler == &VG_(oursignalhandler));
	    vg_assert((old_action->ksa_flags & VKI_SA_ONSTACK) != 0);
         }
         old_action->ksa_handler = our_old_handler;
         /* Since the client is not allowed to ask for an alternative
            sig stack, unset the bit for anything we pass back to
            it. */
         old_action->ksa_flags &= ~VKI_SA_ONSTACK;
	 /* Restore the SA_RESTART flag to whatever we snaffled. */
	 if (vg_sig_sarestart[param1])
            old_action->ksa_flags |= VKI_SA_RESTART;
         else 
            old_action->ksa_flags &= ~VKI_SA_RESTART;
      }
   }

   VG_(ksignal)(VKI_SIGABRT, &VG_(oursignalhandler));
   goto good;

  good:
   tst->m_eax = (UInt)0;
   return;

  bad_signo:
   VG_(message)(Vg_UserMsg,
                "Warning: bad signal number %d in __NR_sigaction.", 
                param1);
   VG_(baseBlock)[VGOFF_(m_eax)] = (UInt)(-VKI_EINVAL);
   return;

  bad_sigkill:
   VG_(message)(Vg_UserMsg,
                "Warning: attempt to set SIGKILL handler in __NR_sigaction.", 
                param1);
   VG_(baseBlock)[VGOFF_(m_eax)] = (UInt)(-VKI_EINVAL);
   return;
}


/* The kernel handles sigprocmask in the usual way, but we also need
   to inspect it, so as to spot requests to unblock signals.  We then
   inspect vg_sigpending, which records the current state of signal
   delivery to the client.  The problematic case is when a signal is
   delivered to the client, in which case the relevant vg_sigpending
   slot is set to VG_SIGRUNNING.  This inhibits further signal
   deliveries.  This mechanism implements the POSIX requirement that a
   signal is blocked in its own handler.

   If the handler returns normally, the slot is changed back to
   VG_SIGIDLE, so that further instances of the signal can be
   delivered.  The problem occurs when the handler never returns, but
   longjmps.  POSIX mandates that you then have to do an explicit
   setprocmask to re-enable the signal.  That is what we try and spot
   here.  Although the call is passed to the kernel, we also need to
   spot unblocked signals whose state is VG_SIGRUNNING, and change it
   back to VG_SIGIDLE.  
*/
void VG_(do__NR_sigprocmask) ( Int how, vki_ksigset_t* set )
{
   Int i;
   if (VG_(clo_trace_signals))
      VG_(message)(Vg_DebugMsg, 
                   "vg_do__NR_sigprocmask: how = %d (%s), set = %p", 
                   how,
                   how==VKI_SIG_BLOCK ? "SIG_BLOCK" : (
                      how==VKI_SIG_UNBLOCK ? "SIG_UNBLOCK" : (
                      how==VKI_SIG_SETMASK ? "SIG_SETMASK" : "???")),
                   set
                  );

   /* Sometimes this happens.  I don't know what it signifies. */
   if (set == NULL) 
      return;

   /* Not interested in blocking of signals. */
   if (how == VKI_SIG_BLOCK) 
      return;

   /* Detect and ignore unknown action. */
   if (how != VKI_SIG_UNBLOCK && how != VKI_SIG_SETMASK) {
      VG_(message)(Vg_DebugMsg, 
                  "sigprocmask: unknown `how' field %d", how);
      return;
   }

   for (i = 1; i < VKI_KNSIG; i++) {
      Bool unblock_me = False;
      if (how == VKI_SIG_SETMASK) {
         if (!VG_(ksigismember)(set,i))
            unblock_me = True;
      } else { /* how == SIG_UNBLOCK */
         if (VG_(ksigismember)(set,i))
            unblock_me = True;
      }
      if (unblock_me && VG_(sigpending)[i] == VG_SIGRUNNING) {
         VG_(sigpending)[i] = VG_SIGIDLE;
	 if (VG_(clo_verbosity) > 1)
            VG_(message)(Vg_UserMsg, 
                         "Warning: unblocking signal %d "
                         "due to sigprocmask", i );
      }
   }
}



/*--------------------------------------------------------------------*/
/*--- end                                             vg_signals.c ---*/
/*--------------------------------------------------------------------*/
