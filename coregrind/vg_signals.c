
/*--------------------------------------------------------------------*/
/*--- Implementation of POSIX signals.                             ---*/
/*---                                                 vg_signals.c ---*/
/*--------------------------------------------------------------------*/

/*
   This file is part of Valgrind, an x86 protected-mode emulator 
   designed for debugging and profiling binaries on x86-Unixes.

   Copyright (C) 2000-2002 Julian Seward 
      jseward@acm.org
      Julian_Seward@muraroa.demon.co.uk

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


/* ---------------------------------------------------------------------
   An implementation of signal sets and other grunge, identical to 
   that in the target kernels (Linux 2.2.X and 2.4.X).
   ------------------------------------------------------------------ */



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

/* See decl in vg_include.h for explanation. */
Int VG_(syscall_depth) = 0;


/* ---------------------------------------------------------------------
   The signal simulation proper.  A simplified version of what the 
   Linux kernel does.
   ------------------------------------------------------------------ */

/* A structure in which to save the application's registers
   during the execution of signal handlers. */

typedef
   struct {
      UInt retaddr;  /* Sig handler's (bogus) return address */
      Int  sigNo;    /* The arg to the sig handler.  */
      UInt magicPI;
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
      UInt magicE;
   }
   VgSigContext;



/* This is the bogus return address which the implementation
   of RET in vg_cpu.c checks for.  If it spots a return to 
   here, it calls vg_signal_returns().  We should never actually
   enter this procedure, neither on the real nor simulated CPU.
*/
void VG_(signalreturn_bogusRA) ( void )
{
   VG_(panic) ( "vg_signalreturn_bogusRA -- something is badly wrong" );
}


/* Set up a stack frame (VgSigContext) for the client's signal
   handler.  This includes the signal number and a bogus return
   address.  */
static
void vg_push_signal_frame ( int sigNo )
{
   Int          i;
   UInt         esp;
   VgSigContext sigctx;
   for (i = 0; i < VG_SIZE_OF_FPUSTATE_W; i++)
      sigctx.fpustate[i] = VG_(baseBlock)[VGOFF_(m_fpustate) + i];

   sigctx.magicPI    = 0x31415927;
   sigctx.magicE     = 0x27182818;
   sigctx.eax        = VG_(baseBlock)[VGOFF_(m_eax)];
   sigctx.ecx        = VG_(baseBlock)[VGOFF_(m_ecx)];
   sigctx.edx        = VG_(baseBlock)[VGOFF_(m_edx)];
   sigctx.ebx        = VG_(baseBlock)[VGOFF_(m_ebx)];
   sigctx.ebp        = VG_(baseBlock)[VGOFF_(m_ebp)];
   sigctx.esp        = VG_(baseBlock)[VGOFF_(m_esp)];
   sigctx.esi        = VG_(baseBlock)[VGOFF_(m_esi)];
   sigctx.edi        = VG_(baseBlock)[VGOFF_(m_edi)];
   sigctx.eflags     = VG_(baseBlock)[VGOFF_(m_eflags)];
   sigctx.eip        = VG_(baseBlock)[VGOFF_(m_eip)];
   sigctx.retaddr    = (UInt)(&VG_(signalreturn_bogusRA));
   sigctx.sigNo      = sigNo;

   esp = VG_(baseBlock)[VGOFF_(m_esp)];
   vg_assert((sizeof(VgSigContext) & 0x3) == 0);

   esp -= sizeof(VgSigContext);
   for (i = 0; i < sizeof(VgSigContext)/4; i++)
      ((UInt*)esp)[i] = ((UInt*)(&sigctx))[i];

   /* Make sigNo and retaddr fields readable -- at 0(%ESP) and 4(%ESP) */
   if (VG_(clo_instrument)) {
      VGM_(make_readable) ( ((Addr)esp)+0 ,4 );
      VGM_(make_readable) ( ((Addr)esp)+4 ,4 );
   }

   VG_(baseBlock)[VGOFF_(m_esp)] = esp;
   VG_(baseBlock)[VGOFF_(m_eip)] = (Addr)VG_(sigpending)[sigNo];
   /* 
   VG_(printf)("pushed signal frame; %%ESP now = %p, next %%EBP = %p\n", 
               esp, VG_(baseBlock)[VGOFF_(m_eip)]);
   */
}


/* Clear the signal frame created by vg_push_signal_frame, restore the
   simulated machine state, and return the signal number that the
   frame was for. */
static
Int vg_pop_signal_frame ( void )
{
   UInt          esp;
   Int           sigNo, i;
   VgSigContext* sigctx;
   /* esp is now pointing at the magicPI word on the stack, viz,
      eight bytes above the bottom of the vg_sigcontext.
   */
   esp    = VG_(baseBlock)[VGOFF_(m_esp)];
   sigctx = (VgSigContext*)(esp-4);

   vg_assert(sigctx->magicPI == 0x31415927);
   vg_assert(sigctx->magicE  == 0x27182818);
   if (VG_(clo_trace_signals))
      VG_(message)(Vg_DebugMsg, "vg_pop_signal_frame: valid magic");

   /* restore machine state */
   for (i = 0; i < VG_SIZE_OF_FPUSTATE_W; i++)
      VG_(baseBlock)[VGOFF_(m_fpustate) + i] = sigctx->fpustate[i];

   /* Mark the sigctx structure as nonaccessible.  Has to happen
      _before_ vg_m_state.m_esp is given a new value.*/
   if (VG_(clo_instrument)) 
      VGM_(handle_esp_assignment) ( sigctx->esp );

   /* Restore machine state from the saved context. */
   VG_(baseBlock)[VGOFF_(m_eax)]     = sigctx->eax;
   VG_(baseBlock)[VGOFF_(m_ecx)]     = sigctx->ecx;
   VG_(baseBlock)[VGOFF_(m_edx)]     = sigctx->edx;
   VG_(baseBlock)[VGOFF_(m_ebx)]     = sigctx->ebx;
   VG_(baseBlock)[VGOFF_(m_ebp)]     = sigctx->ebp;
   VG_(baseBlock)[VGOFF_(m_esp)]     = sigctx->esp;
   VG_(baseBlock)[VGOFF_(m_esi)]     = sigctx->esi;
   VG_(baseBlock)[VGOFF_(m_edi)]     = sigctx->edi;
   VG_(baseBlock)[VGOFF_(m_eflags)]  = sigctx->eflags;
   VG_(baseBlock)[VGOFF_(m_eip)]     = sigctx->eip;
   sigNo                             = sigctx->sigNo;
   return sigNo;
}


/* A handler is returning.  Restore the machine state from the stacked
   VgSigContext and continue with whatever was going on before the
   handler ran.  */

void VG_(signal_returns) ( void )
{
   Int            sigNo, ret;
   vki_ksigset_t  block_procmask;
   vki_ksigset_t  saved_procmask;

   /* Block host signals ... */
   VG_(ksigfillset)(&block_procmask);
   ret = VG_(ksigprocmask)(VKI_SIG_SETMASK, &block_procmask, &saved_procmask);
   vg_assert(ret == 0);

   sigNo = vg_pop_signal_frame();

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
   ret = VG_(ksigprocmask)(VKI_SIG_SETMASK, &saved_procmask, NULL);
   vg_assert(ret == 0);

   /* The main dispatch loop now continues at vg_m_eip. */
}


/* Restore the default host behaviour of SIGABRT, and unblock it,
   so we can exit the simulator cleanly by doing exit/abort/assert fail.
*/
void VG_(restore_SIGABRT) ( void )
{
   vki_ksigset_t   set;
   vki_ksigaction  act;
   act.ksa_flags   = VKI_SA_RESTART;
   act.ksa_handler = VKI_SIG_DFL;
   VG_(ksigemptyset)(&act.ksa_mask);

   VG_(ksigemptyset)(&set);
   VG_(ksigaddset)(&set,VKI_SIGABRT);

   /* If this doesn't work, tough.  Don't check return code. */
   VG_(ksigaction)(VKI_SIGABRT, &act, NULL);
   VG_(ksigprocmask)(VKI_SIG_UNBLOCK, &set, NULL);   
}


/* Deliver all pending signals, by building stack frames for their
   handlers. */
void VG_(deliver_signals) ( void )
{
   vki_ksigset_t  block_procmask;
   vki_ksigset_t  saved_procmask;
   Int            ret, sigNo;
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

   if (!found) return;

   /* Now we have to do it properly.  Get exclusive access by
      blocking all the host's signals.  That means vg_oursignalhandler
      can't run whilst we are messing with stuff.
   */
   VG_(ksigfillset)(&block_procmask);
   ret = VG_(ksigprocmask)(VKI_SIG_SETMASK, &block_procmask, &saved_procmask);
   vg_assert(ret == 0);

   for (sigNo = 1; sigNo < VKI_KNSIG; sigNo++) {
      if (VG_(sigpending)[sigNo] == VG_SIGIDLE ||
          VG_(sigpending)[sigNo] == VG_SIGRUNNING) continue;

      if (VG_(clo_trace_signals))
         VG_(message)(Vg_DebugMsg,"delivering signal %d", sigNo );

      /* Create a signal delivery frame, and set the client's %ESP and
         %EIP so that when execution continues, we will enter the
         signal handler with the frame on top of the client's stack,
         as it expects. */
      vg_push_signal_frame ( sigNo );

      /* Signify that the signal has been delivered. */
      VG_(sigpending)[sigNo] = VG_SIGRUNNING;
   }

   /* Unlock and return. */
   ret = VG_(ksigprocmask)(VKI_SIG_SETMASK, &saved_procmask, NULL);
   vg_assert(ret == 0);
   return;
}


/* ----------- HACK ALERT ----------- */
/* Note carefully that this runs with all host signals disabled! */
static
void vg_deliver_signal_immediately ( Int sigNo )
{
   Int   n_bbs_done;
   Int   sigNo2;
   Addr  next_orig_addr;
   Addr  next_trans_addr;

   if (VG_(clo_verbosity) > 0
       && (True || VG_(clo_trace_signals)))
      VG_(message)(Vg_DebugExtraMsg,
         "deliver signal %d immediately: BEGIN", sigNo );
   /* VG_(printf)("resumption addr is %p\n", 
      VG_(baseBlock)[VGOFF_(m_eip)]); */

   vg_push_signal_frame ( sigNo );
   n_bbs_done = 0;

   /* Single-step the client (ie, run the handler) until it jumps to
      VG_(signalreturn_bogusRA) */

   while (True) {

      if (n_bbs_done >= VG_MAX_BBS_IN_IMMEDIATE_SIGNAL)
         VG_(unimplemented)(
            "handling signal whilst client blocked in syscall: "
            "handler runs too long"
         );

      next_orig_addr = VG_(baseBlock)[VGOFF_(m_eip)];

      if (next_orig_addr == (Addr)(&VG_(trap_here)))
         VG_(unimplemented)(
            "handling signal whilst client blocked in syscall: "
            "handler calls malloc (et al)"
         );

      /* VG_(printf)("next orig addr = %p\n", next_orig_addr); */
      if (next_orig_addr == (Addr)(&VG_(signalreturn_bogusRA)))
         break;

      next_trans_addr = VG_(search_transtab) ( next_orig_addr );
      if (next_trans_addr == (Addr)NULL) {
         VG_(create_translation_for) ( next_orig_addr );
         next_trans_addr = VG_(search_transtab) ( next_orig_addr );
      }

      vg_assert(next_trans_addr != (Addr)NULL);
      next_orig_addr = VG_(run_singleton_translation)(next_trans_addr);
      VG_(baseBlock)[VGOFF_(m_eip)] = next_orig_addr;
      n_bbs_done++;
   }

   sigNo2 = vg_pop_signal_frame();
   vg_assert(sigNo2 == sigNo);

   if (VG_(clo_verbosity) > 0
       && (True || VG_(clo_trace_signals)))
     VG_(message)(Vg_DebugExtraMsg,
         "deliver signal %d immediately: END, %d bbs done", 
         sigNo, n_bbs_done );

   /* Invalidate the tt_fast cache.  We've been (potentially) adding
      translations and even possibly doing LRUs without keeping it up
      to date, so we'd better nuke it before going any further, to
      avoid inconsistencies with the main TT/TC structure. */
   VG_(invalidate_tt_fast)();
}


/* ----------- end of HACK ALERT ----------- */


/* Receive a signal from the host, and either discard it or park it in
   the queue of pending signals.  All other signals will be blocked
   when this handler runs.  Runs with all host signals blocked, so as
   to have mutual exclusion when adding stuff to the queue. */

static void VG_(oursignalhandler) ( Int sigNo )
{
   Int           ret;
   vki_ksigset_t block_procmask;
   vki_ksigset_t saved_procmask;

   if (VG_(clo_trace_signals)) {
      VG_(start_msg)(Vg_DebugMsg);
      VG_(add_to_msg)("signal %d arrived ... ", sigNo );
   }
   vg_assert(sigNo >= 1 && sigNo < VKI_KNSIG);

   /* Sanity check.  Ensure we're really running on the signal stack
      we asked for. */
   if ( !(
            ((Char*)(&(VG_(sigstack)[0])) <= (Char*)(&ret))
            &&
            ((Char*)(&ret) < (Char*)(&(VG_(sigstack)[10000])))
         )
        ) {
     VG_(message)(Vg_DebugMsg, "FATAL: signal delivered on the wrong stack?!");
     VG_(message)(Vg_DebugMsg, "A possible workaround follows.  Please tell me");
     VG_(message)(Vg_DebugMsg, "(jseward@acm.org) if the suggested workaround doesn't help.");
     VG_(unimplemented)
        ("support for progs compiled with -p/-pg; rebuild your prog without -p/-pg");
   }

   vg_assert((Char*)(&(VG_(sigstack)[0])) <= (Char*)(&ret));
   vg_assert((Char*)(&ret) < (Char*)(&(VG_(sigstack)[10000])));

   if (sigNo == VKI_SIGABRT && VG_(sighandler)[sigNo] == NULL) {
      /* We get here if SIGABRT is delivered and the client hasn't
         asked to catch it.  The aim is to exit in a controlled
         manner. */
      if (VG_(clo_trace_signals)) {
         VG_(add_to_msg)("catching SIGABRT");
         VG_(end_msg)();
      }
      VG_(ksignal)(VKI_SIGABRT, VKI_SIG_DFL);
      VG_(interrupt_reason) = VG_Y_EXIT;
      VG_(longjmpd_on_signal) = VKI_SIGABRT;
      __builtin_longjmp(VG_(toploop_jmpbuf),1);
   }

   /* Block all host signals. */
   VG_(ksigfillset)(&block_procmask);
   ret = VG_(ksigprocmask)(VKI_SIG_SETMASK, &block_procmask, &saved_procmask);
   vg_assert(ret == 0);

   if (VG_(sighandler)[sigNo] == NULL) {
      if (VG_(clo_trace_signals)) {
         VG_(add_to_msg)("unexpected!");
         VG_(end_msg)();
      }
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
      /* Ok, we'd better deliver it to the client, one way or another. */
      vg_assert(VG_(sigpending)[sigNo] == VG_SIGIDLE);

      if (VG_(syscall_depth) == 0) {
         /* The usual case; delivering a signal to the client, and the
            client is not currently in a syscall.  Queue it up for
            delivery at some point in the future. */
         VG_(sigpending)[sigNo] = VG_(sighandler)[sigNo];
         if (VG_(clo_trace_signals)) {
            VG_(add_to_msg)("queued" );
            VG_(end_msg)();
         }
      } else {
         /* The nasty case, which was causing kmail to freeze up: the
            client is (presumably blocked) in a syscall.  We have to
            deliver the signal right now, because it may be that
            running the sighandler is the only way that the syscall
            will be able to return.  In which case, if we don't do
            that, the client will deadlock. */
         if (VG_(clo_trace_signals)) {
            VG_(add_to_msg)("delivering immediately" );
            VG_(end_msg)();
         }
         /* Note that this runs with all host signals blocked. */
         VG_(sigpending)[sigNo] = VG_(sighandler)[sigNo];
         vg_deliver_signal_immediately(sigNo);
         VG_(sigpending)[sigNo] = VG_SIGIDLE;
         /* VG_(printf)("resuming at %p\n", VG_(baseBlock)[VGOFF_(m_eip)]); */
      }
   }

   /* We've finished messing with the queue, so re-enable host signals. */
   ret = VG_(ksigprocmask)(VKI_SIG_SETMASK, &saved_procmask, NULL);

   vg_assert(ret == 0);
   if (sigNo == VKI_SIGSEGV || sigNo == VKI_SIGBUS 
       || sigNo == VKI_SIGFPE || sigNo == VKI_SIGILL) {
      /* Can't continue; must longjmp and thus enter the sighandler
         immediately. */
      VG_(longjmpd_on_signal) = sigNo;
      __builtin_longjmp(VG_(toploop_jmpbuf),1);
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
               sa->ksa_handler, sa->ksa_flags, sa->ksa_restorer);
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

   vki_ksigset_t  block_procmask;
   vki_ksigset_t  saved_procmask;
   vki_kstack_t   altstack_info;
   vki_ksigaction sa;

   /*  VG_(printf)("SIGSTARTUP\n"); */
   /* Block all signals.  
      saved_procmask remembers the previous mask. */
   VG_(ksigfillset)(&block_procmask);
   ret = VG_(ksigprocmask)(VKI_SIG_SETMASK, &block_procmask, &saved_procmask);
   vg_assert(ret == 0);

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
   for (i = 1; i < VKI_KNSIG; i++)
      VG_(sighandler[i]) = VG_(sigpending[i]) = NULL;

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
         VG_(sighandler[i]) = sa.ksa_handler;
         sa.ksa_handler = &VG_(oursignalhandler);
         ret = VG_(ksigaction)(i, &sa, NULL);
         vg_assert(ret == 0);
      }
   }

   VG_(ksignal)(VKI_SIGABRT, &VG_(oursignalhandler));

   /* Finally, restore the blocking mask. */
   ret = VG_(ksigprocmask)(VKI_SIG_SETMASK, &saved_procmask, NULL);
   vg_assert(ret == 0);   
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

   vki_ksigset_t  block_procmask;
   vki_ksigset_t  saved_procmask;
   vki_ksigaction sa;

   /* Block all signals. */
   VG_(ksigfillset)(&block_procmask);
   ret = VG_(ksigprocmask)(VKI_SIG_SETMASK, &block_procmask, &saved_procmask);
   vg_assert(ret == 0);

   /* copy the sim signal actions to the real ones. */
   for (i = 1; i < VKI_KNSIG; i++) {
      if (i == VKI_SIGKILL || i == VKI_SIGSTOP) continue;
      if (VG_(sighandler)[i] == NULL) continue;
      ret = VG_(ksigaction)(i, NULL, &sa);
      vg_assert(ret == 0);
      sa.ksa_handler = VG_(sighandler)[i];
      ret = VG_(ksigaction)(i, &sa, NULL);      
   }

   /* Finally, copy the simulated process mask to the real one. */
   ret = VG_(ksigprocmask)(VKI_SIG_SETMASK, &saved_procmask, NULL);
   vg_assert(ret == 0);
}


/* ---------------------------------------------------------------------
   Handle signal-related syscalls from the simulatee.
   ------------------------------------------------------------------ */

/* Do more error checking? */
void VG_(do__NR_sigaction) ( void )
{
   UInt res;
   void* our_old_handler;
   vki_ksigaction* new_action;
   vki_ksigaction* old_action;
   UInt param1
      = VG_(baseBlock)[VGOFF_(m_ebx)]; /* int sigNo */
   UInt param2 
      = VG_(baseBlock)[VGOFF_(m_ecx)]; /* k_sigaction* new_action */
   UInt param3 
      = VG_(baseBlock)[VGOFF_(m_edx)]; /* k_sigaction* old_action */
   new_action  = (vki_ksigaction*)param2;
   old_action  = (vki_ksigaction*)param3;

   if (VG_(clo_trace_signals))
      VG_(message)(Vg_DebugExtraMsg, 
         "__NR_sigaction: sigNo %d, "
         "new 0x%x, old 0x%x, new flags 0x%x",
         param1,(UInt)new_action,(UInt)old_action,
         (UInt)(new_action ? new_action->ksa_flags : 0) );
   /* VG_(ppSigProcMask)(); */

   if (param1 < 1 || param1 >= VKI_KNSIG) goto bad;

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
         new_action->ksa_handler = &VG_(oursignalhandler);
      }
   }

   KERNEL_DO_SYSCALL(res);
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
      }
   }

   VG_(ksignal)(VKI_SIGABRT, &VG_(oursignalhandler));
   goto good;

  good:
   VG_(baseBlock)[VGOFF_(m_eax)] = (UInt)0;
   return;

  bad:
   VG_(message)(Vg_UserMsg,
                "Warning: bad signal number %d in __NR_sigaction.", 
                param1);
   VG_(baseBlock)[VGOFF_(m_eax)] = (UInt)(-1);
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
