
/*--------------------------------------------------------------------*/
/*--- Implementation of POSIX signals.                vg_signals.c ---*/
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

/* 
   Signal handling.

   There are 4 distinct classes of signal:

   1. Synchronous, instruction-generated (SIGILL, FPE, BUS, SEGV and
   TRAP): these are signals as a result of an instruction fault.  If
   we get one while running client code, then we just do the
   appropriate thing.  If it happens while running Valgrind code, then
   it indicates a Valgrind bug.  Note that we "manually" implement
   automatic stack growth, such that if a fault happens near the
   client process stack, it is extended in the same way the kernel
   would, and the fault is never reported to the client program.

   2. Asynchronous varients of the above signals: If the kernel tries
   to deliver a sync signal while it is blocked, it just kills the
   process.  Therefore, we can't block those signals if we want to be
   able to report on bugs in Valgrind.  This means that we're also
   open to receiving those signals from other processes, sent with
   kill.  We could get away with just dropping them, since they aren't
   really signals that processes send to each other.

   3. Synchronous, general signals.  If a thread/process sends itself
   a signal with kill, its expected to be synchronous: ie, the signal
   will have been delivered by the time the syscall finishes.
   
   4. Asyncronous, general signals.  All other signals, sent by
   another process with kill.  These are generally blocked, except for
   two special cases: we poll for them each time we're about to run a
   thread for a time quanta, and while running blocking syscalls.


   In addition, we define two signals for internal use: SIGVGCHLD and
   SIGVGKILL.  SIGVGCHLD is used to indicate thread death to any
   reaping thread (the master thread).  It is always blocked and never
   delivered as a signal; it is always polled with sigtimedwait.

   SIGVGKILL is used to terminate threads.  When one thread wants
   another to exit, it will set its exitreason and send it SIGVGKILL
   if it appears to be blocked in a syscall.


   We use a kernel thread for each application thread.  When the
   thread allows itself to be open to signals, it sets the thread
   signal mask to what the client application set it to.  This means
   that we get the kernel to do all signal routing: under Valgrind,
   signals get delivered in the same way as in the non-Valgrind case
   (the exception being for the sync signal set, since they're almost
   always unblocked).
 */

#include "core.h"

/* Define to give more sanity checking for signals. */
#define DEBUG_SIGNALS


/* ---------------------------------------------------------------------
   Forwards decls.
   ------------------------------------------------------------------ */

static void vg_sync_signalhandler  ( Int sigNo, vki_siginfo_t *info, struct vki_ucontext * );
static void vg_async_signalhandler ( Int sigNo, vki_siginfo_t *info, struct vki_ucontext * );
static void sigvgkill_handler	   ( Int sigNo, vki_siginfo_t *info, struct vki_ucontext * );

static const Char *signame(Int sigNo);

/* Maximum usable signal. */
Int VG_(max_signal) = _VKI_NSIG;

#define N_QUEUED_SIGNALS	8

typedef struct SigQueue {
   Int	next;
   vki_siginfo_t sigs[N_QUEUED_SIGNALS];
} SigQueue;

/* ---------------------------------------------------------------------
   HIGH LEVEL STUFF TO DO WITH SIGNALS: POLICY (MOSTLY)
   ------------------------------------------------------------------ */

/* ---------------------------------------------------------------------
   Signal state for this process.
   ------------------------------------------------------------------ */


/* Base-ment of these arrays[_VKI_NSIG].

   Valid signal numbers are 1 .. _VKI_NSIG inclusive.
   Rather than subtracting 1 for indexing these arrays, which
   is tedious and error-prone, they are simply dimensioned 1 larger,
   and entry [0] is not used. 
 */


/* -----------------------------------------------------
   Static client signal state (SCSS).  This is the state
   that the client thinks it has the kernel in.  
   SCSS records verbatim the client's settings.  These 
   are mashed around only when SKSS is calculated from it.
   -------------------------------------------------- */

typedef 
   struct {
      void* scss_handler;  /* VKI_SIG_DFL or VKI_SIG_IGN or ptr to
                              client's handler */
      UInt  scss_flags;
      vki_sigset_t scss_mask;
      void* scss_restorer; /* where sigreturn goes */
   }
   SCSS_Per_Signal;

typedef 
   struct {
      /* per-signal info */
      SCSS_Per_Signal scss_per_sig[1+_VKI_NSIG];

      /* Additional elements to SCSS not stored here:
         - for each thread, the thread's blocking mask
         - for each thread in WaitSIG, the set of waited-on sigs
      */
      } 
      SCSS;

static SCSS vg_scss;


/* -----------------------------------------------------
   Static kernel signal state (SKSS).  This is the state
   that we have the kernel in.  It is computed from SCSS.
   -------------------------------------------------- */

/* Let's do: 
     sigprocmask assigns to all thread masks
     so that at least everything is always consistent
   Flags:
     SA_SIGINFO -- we always set it, and honour it for the client
     SA_NOCLDSTOP -- passed to kernel
     SA_ONESHOT or SA_RESETHAND -- pass through
     SA_RESTART -- we observe this but set our handlers to always restart
     SA_NOMASK or SA_NODEFER -- we observe this, but our handlers block everything
     SA_ONSTACK -- pass through
     SA_NOCLDWAIT -- pass through
*/


typedef 
   struct {
      void* skss_handler;  /* VKI_SIG_DFL or VKI_SIG_IGN 
                              or ptr to our handler */
      UInt skss_flags;
      /* There is no skss_mask, since we know that we will always ask
         for all signals to be blocked in our sighandlers. */
      /* Also there is no skss_restorer. */
   }
   SKSS_Per_Signal;

typedef 
   struct {
      SKSS_Per_Signal skss_per_sig[1+_VKI_NSIG];
   } 
   SKSS;

static SKSS vg_skss;

Bool VG_(is_sig_ign)(Int sigNo)
{
   vg_assert(sigNo >= 1 && sigNo <= _VKI_NSIG);

   return vg_scss.scss_per_sig[sigNo].scss_handler == VKI_SIG_IGN;
}

/* ---------------------------------------------------------------------
   Compute the SKSS required by the current SCSS.
   ------------------------------------------------------------------ */

static 
void pp_SKSS ( void )
{
   Int sig;
   VG_(printf)("\n\nSKSS:\n");
   for (sig = 1; sig <= _VKI_NSIG; sig++) {
      VG_(printf)("sig %d:  handler 0x%x,  flags 0x%x\n", sig,
                  vg_skss.skss_per_sig[sig].skss_handler,
                  vg_skss.skss_per_sig[sig].skss_flags );

   }
}

/* This is the core, clever bit.  Computation is as follows:

   For each signal
      handler = if client has a handler, then our handler
                else if client is DFL, then our handler as well
                else (client must be IGN)
			then hander is IGN
*/
static
void calculate_SKSS_from_SCSS ( SKSS* dst )
{
   Int   sig;
   UInt  scss_flags;
   UInt  skss_flags;

   for (sig = 1; sig <= _VKI_NSIG; sig++) {
      void *skss_handler;
      void *scss_handler;
      
      scss_handler = vg_scss.scss_per_sig[sig].scss_handler;
      scss_flags   = vg_scss.scss_per_sig[sig].scss_flags;

      switch(sig) {
      case VKI_SIGSEGV:
      case VKI_SIGBUS:
      case VKI_SIGFPE:
      case VKI_SIGILL:
      case VKI_SIGTRAP:
	 /* For these, we always want to catch them and report, even
	    if the client code doesn't. */
	 skss_handler = vg_sync_signalhandler;
	 break;

      case VKI_SIGCONT:
	 /* Let the kernel handle SIGCONT unless the client is actually
	    catching it. */
	 if (vg_scss.scss_per_sig[sig].scss_handler == VKI_SIG_DFL)
	    skss_handler = VKI_SIG_DFL;
	 else if (vg_scss.scss_per_sig[sig].scss_handler == VKI_SIG_IGN)
	    skss_handler = VKI_SIG_IGN;
	 else
	    skss_handler = vg_async_signalhandler;
	 break;

      default:
	 if (sig == VKI_SIGVGKILL)
	    skss_handler = sigvgkill_handler;
	 else if (sig == VKI_SIGVGCHLD)
	    skss_handler = VKI_SIG_IGN;	/* we only poll for it */
	 else {
	    if (scss_handler == VKI_SIG_IGN)
	       skss_handler = VKI_SIG_IGN;
	    else 
	       skss_handler = vg_async_signalhandler;
	 }
	 break;
      }

      /* Flags */

      skss_flags = 0;

      /* SA_NOCLDSTOP, SA_NOCLDWAIT: pass to kernel */
      skss_flags |= scss_flags & (VKI_SA_NOCLDSTOP | VKI_SA_NOCLDWAIT);

      /* SA_ONESHOT: ignore client setting */
      
      /* SA_RESTART: ignore client setting and always set it for us
	 (even though we never rely on the kernel to restart a
	 syscall, we observe whether it wanted to restart the syscall
	 or not, which helps VGA_(interrupted_syscall)()) */
      skss_flags |= VKI_SA_RESTART;

      /* SA_NOMASK: ignore it */

      /* SA_ONSTACK: client setting is irrelevant here */
      /* We don't set a signal stack, so ignore */

      /* always ask for SA_SIGINFO */
      skss_flags |= VKI_SA_SIGINFO;

      /* use our own restorer */
      skss_flags |= VKI_SA_RESTORER;

      /* Create SKSS entry for this signal. */
      if (sig != VKI_SIGKILL && sig != VKI_SIGSTOP)
         dst->skss_per_sig[sig].skss_handler = skss_handler;
      else
         dst->skss_per_sig[sig].skss_handler = VKI_SIG_DFL;

      dst->skss_per_sig[sig].skss_flags   = skss_flags;
   }

   /* Sanity checks. */
   vg_assert(dst->skss_per_sig[VKI_SIGKILL].skss_handler == VKI_SIG_DFL);
   vg_assert(dst->skss_per_sig[VKI_SIGSTOP].skss_handler == VKI_SIG_DFL);

   if (0)
      pp_SKSS();
}


/* ---------------------------------------------------------------------
   After a possible SCSS change, update SKSS and the kernel itself.
   ------------------------------------------------------------------ */

static void handle_SCSS_change ( Bool force_update )
{
   Int  res, sig;
   SKSS skss_old;
   struct vki_sigaction ksa, ksa_old;

   /* Remember old SKSS and calculate new one. */
   skss_old = vg_skss;
   calculate_SKSS_from_SCSS ( &vg_skss );

   /* Compare the new SKSS entries vs the old ones, and update kernel
      where they differ. */
   for (sig = 1; sig <= VG_(max_signal); sig++) {

      /* Trying to do anything with SIGKILL is pointless; just ignore
         it. */
      if (sig == VKI_SIGKILL || sig == VKI_SIGSTOP)
         continue;

      if (!force_update) {
         if ((skss_old.skss_per_sig[sig].skss_handler
              == vg_skss.skss_per_sig[sig].skss_handler)
             && (skss_old.skss_per_sig[sig].skss_flags
                 == vg_skss.skss_per_sig[sig].skss_flags))
            /* no difference */
            continue;
      }

      ksa.ksa_handler = vg_skss.skss_per_sig[sig].skss_handler;
      ksa.sa_flags    = vg_skss.skss_per_sig[sig].skss_flags;
      ksa.sa_restorer = VG_(sigreturn);

      /* block all signals in handler */
      VG_(sigfillset)( &ksa.sa_mask );
      VG_(sigdelset)( &ksa.sa_mask, VKI_SIGKILL );
      VG_(sigdelset)( &ksa.sa_mask, VKI_SIGSTOP );

      if (VG_(clo_trace_signals) && VG_(clo_verbosity) > 2)
         VG_(message)(Vg_DebugMsg, 
            "setting ksig %d to: hdlr 0x%x, flags 0x%x, "
            "mask(63..0) 0x%x 0x%x",
            sig, ksa.ksa_handler,
            ksa.sa_flags,
            ksa.sa_mask.sig[1], 
            ksa.sa_mask.sig[0] 
         );

      res = VG_(sigaction)( sig, &ksa, &ksa_old );
      vg_assert(res == 0);

      /* Since we got the old sigaction more or less for free, might
         as well extract the maximum sanity-check value from it. */
      if (!force_update) {
         vg_assert(ksa_old.ksa_handler 
                   == skss_old.skss_per_sig[sig].skss_handler);
         vg_assert(ksa_old.sa_flags 
                   == skss_old.skss_per_sig[sig].skss_flags);
         vg_assert(ksa_old.sa_restorer 
                   == VG_(sigreturn));
         VG_(sigaddset)( &ksa_old.sa_mask, VKI_SIGKILL );
         VG_(sigaddset)( &ksa_old.sa_mask, VKI_SIGSTOP );
         vg_assert(VG_(isfullsigset)( &ksa_old.sa_mask ));
      }
   }
}


/* ---------------------------------------------------------------------
   Update/query SCSS in accordance with client requests.
   ------------------------------------------------------------------ */

/* Logic for this alt-stack stuff copied directly from do_sigaltstack
   in kernel/signal.[ch] */

/* True if we are on the alternate signal stack.  */
static Bool on_sig_stack ( ThreadId tid, Addr m_SP )
{
   ThreadState *tst = VG_(get_ThreadState)(tid);

   return (m_SP - (Addr)tst->altstack.ss_sp < tst->altstack.ss_size);
}

static Int sas_ss_flags ( ThreadId tid, Addr m_SP )
{
   ThreadState *tst = VG_(get_ThreadState)(tid);

   return (tst->altstack.ss_size == 0 
              ? VKI_SS_DISABLE
              : on_sig_stack(tid, m_SP) ? VKI_SS_ONSTACK : 0);
}


void VG_(do_sys_sigaltstack) ( ThreadId tid )
{
   vki_stack_t* ss;
   vki_stack_t* oss;
   Addr         m_SP;

   vg_assert(VG_(is_valid_tid)(tid));
   ss    = (vki_stack_t*)SYSCALL_ARG1(VG_(threads)[tid].arch);
   oss   = (vki_stack_t*)SYSCALL_ARG2(VG_(threads)[tid].arch);
   m_SP  = STACK_PTR(VG_(threads)[tid].arch);

   if (VG_(clo_trace_signals))
      VG_(message)(Vg_DebugExtraMsg, 
         "sys_sigaltstack: tid %d, "
         "ss %p, oss %p (current SP %p)",
         tid, (void*)ss, (void*)oss, (void*)m_SP );

   if (oss != NULL) {
      oss->ss_sp    = VG_(threads)[tid].altstack.ss_sp;
      oss->ss_size  = VG_(threads)[tid].altstack.ss_size;
      oss->ss_flags = VG_(threads)[tid].altstack.ss_flags | sas_ss_flags(tid, m_SP);
   }

   if (ss != NULL) {
      if (on_sig_stack(tid, STACK_PTR(VG_(threads)[tid].arch))) {
         SET_SYSCALL_RETVAL(tid, -VKI_EPERM);
         return;
      }
      if (ss->ss_flags != VKI_SS_DISABLE 
          && ss->ss_flags != VKI_SS_ONSTACK 
          && ss->ss_flags != 0) {
         SET_SYSCALL_RETVAL(tid, -VKI_EINVAL);
         return;
      }
      if (ss->ss_flags == VKI_SS_DISABLE) {
         VG_(threads)[tid].altstack.ss_flags = VKI_SS_DISABLE;
      } else {
         if (ss->ss_size < VKI_MINSIGSTKSZ) {
            SET_SYSCALL_RETVAL(tid, -VKI_ENOMEM);
            return;
         }

	 VG_(threads)[tid].altstack.ss_sp    = ss->ss_sp;
	 VG_(threads)[tid].altstack.ss_size  = ss->ss_size;
	 VG_(threads)[tid].altstack.ss_flags = 0;
      }
   }
   SET_SYSCALL_RETVAL(tid, 0);
}


Int VG_(do_sys_sigaction) ( Int signo, 
			    const struct vki_sigaction *new_act, 
			    struct vki_sigaction *old_act )
{
   if (VG_(clo_trace_signals))
      VG_(message)(Vg_DebugExtraMsg, 
         "sys_sigaction: sigNo %d, "
         "new %p, old %p, new flags 0x%llx",
         signo, (UWord)new_act, (UWord)old_act,
         (ULong)(new_act ? new_act->sa_flags : 0) );

   /* Rule out various error conditions.  The aim is to ensure that if
      when the call is passed to the kernel it will definitely
      succeed. */

   /* Reject out-of-range signal numbers. */
   if (signo < 1 || signo > VG_(max_signal)) goto bad_signo;

   /* don't let them use our signals */
   if ( (signo > VKI_SIGVGRTUSERMAX)
	&& new_act
	&& !(new_act->ksa_handler == VKI_SIG_DFL || new_act->ksa_handler == VKI_SIG_IGN) )
      goto bad_signo_reserved;

   /* Reject attempts to set a handler (or set ignore) for SIGKILL. */
   if ( (signo == VKI_SIGKILL || signo == VKI_SIGSTOP)
       && new_act
       && new_act->ksa_handler != VKI_SIG_DFL)
      goto bad_sigkill_or_sigstop;

   /* If the client supplied non-NULL old_act, copy the relevant SCSS
      entry into it. */
   if (old_act) {
      old_act->ksa_handler = vg_scss.scss_per_sig[signo].scss_handler;
      old_act->sa_flags    = vg_scss.scss_per_sig[signo].scss_flags;
      old_act->sa_mask     = vg_scss.scss_per_sig[signo].scss_mask;
      old_act->sa_restorer = vg_scss.scss_per_sig[signo].scss_restorer;
   }

   /* And now copy new SCSS entry from new_act. */
   if (new_act) {
      vg_scss.scss_per_sig[signo].scss_handler  = new_act->ksa_handler;
      vg_scss.scss_per_sig[signo].scss_flags    = new_act->sa_flags;
      vg_scss.scss_per_sig[signo].scss_mask     = new_act->sa_mask;
      vg_scss.scss_per_sig[signo].scss_restorer = new_act->sa_restorer;

      VG_(sigdelset)(&vg_scss.scss_per_sig[signo].scss_mask, VKI_SIGKILL);
      VG_(sigdelset)(&vg_scss.scss_per_sig[signo].scss_mask, VKI_SIGSTOP);
   }

   /* All happy bunnies ... */
   if (new_act) {
      handle_SCSS_change( False /* lazy update */ );
   }
   return 0;

  bad_signo:
   if (VG_(needs).core_errors && VG_(clo_verbosity) >= 1)
      VG_(message)(Vg_UserMsg,
                   "Warning: bad signal number %d in sigaction()", 
                   signo);
   return -VKI_EINVAL;

  bad_signo_reserved:
   if (VG_(needs).core_errors && VG_(clo_verbosity) >= 1) {
      VG_(message)(Vg_UserMsg,
		   "Warning: ignored attempt to set %s handler in sigaction();",
		   signame(signo));
      VG_(message)(Vg_UserMsg,
		   "         the %s signal is used internally by Valgrind", 
		   signame(signo));
   }
   return -VKI_EINVAL;

  bad_sigkill_or_sigstop:
   if (VG_(needs).core_errors && VG_(clo_verbosity) >= 1)
      VG_(message)(Vg_UserMsg,
		   "Warning: ignored attempt to set %s handler in sigaction();",
		   signame(signo));
      VG_(message)(Vg_UserMsg,
		   "         the %s signal is uncatchable", 
		   signame(signo));
   return -VKI_EINVAL;
}


static
void do_sigprocmask_bitops ( Int vki_how, 
			     vki_sigset_t* orig_set,
			     vki_sigset_t* modifier )
{
   switch (vki_how) {
      case VKI_SIG_BLOCK: 
         VG_(sigaddset_from_set)( orig_set, modifier );
         break;
      case VKI_SIG_UNBLOCK:
         VG_(sigdelset_from_set)( orig_set, modifier );
         break;
      case VKI_SIG_SETMASK:
         *orig_set = *modifier;
         break;
      default:
         VG_(core_panic)("do_sigprocmask_bitops");
	 break;
   }
}

static void sigvgchld_handler(Int sig)
{
   VG_(printf)("got a sigvgchld?\n");
}

/* 
   Wait until some predicate about threadstates is satisfied.

   This uses SIGVGCHLD as a notification that it is now worth
   re-evaluating the predicate.
 */
void VG_(wait_for_threadstate)(Bool (*pred)(void *), void *arg)
{
   vki_sigset_t set, saved;
   struct vki_sigaction sa, old_sa;

   /* 
      SIGVGCHLD is set to be ignored, and is unblocked by default.
      This means all such signals are simply discarded.

      In this loop, we actually block it, and then poll for it with
      sigtimedwait.
    */
   VG_(sigemptyset)(&set);
   VG_(sigaddset)(&set, VKI_SIGVGCHLD);

   VG_(set_sleeping)(VG_(master_tid), VgTs_Yielding);
   VG_(sigprocmask)(VKI_SIG_BLOCK, &set, &saved);

   /* It shouldn't be necessary to set a handler, since the signal is
      always blocked, but it seems to be necessary to convice the
      kernel not to just toss the signal... */
   sa.ksa_handler = sigvgchld_handler;
   sa.sa_flags = 0;
   VG_(sigfillset)(&sa.sa_mask);
   VG_(sigaction)(VKI_SIGVGCHLD, &sa, &old_sa);

   vg_assert(old_sa.ksa_handler == VKI_SIG_IGN);

   while(!(*pred)(arg)) {
      struct vki_siginfo si;
      Int ret = VG_(sigtimedwait)(&set, &si, NULL);

      if (ret > 0 && VG_(clo_trace_signals))
	 VG_(message)(Vg_DebugMsg, "Got %d (code=%d) from tid lwp %d",
		      ret, si.si_code, si._sifields._kill._pid);
   }

   VG_(sigaction)(VKI_SIGVGCHLD, &old_sa, NULL);
   VG_(sigprocmask)(VKI_SIG_SETMASK, &saved, NULL);
   VG_(set_running)(VG_(master_tid));
}

/* Add and remove signals from mask so that we end up telling the
   kernel the state we actually want rather than what the client
   wants. */
void VG_(sanitize_client_sigmask)(ThreadId tid, vki_sigset_t *mask)
{
   VG_(sigdelset)(mask, VKI_SIGKILL);
   VG_(sigdelset)(mask, VKI_SIGSTOP);

   VG_(sigdelset)(mask, VKI_SIGVGKILL); /* never block */

   /* SIGVGCHLD is used by threads to indicate their state changes to
      the master thread.  Mostly it doesn't care, so it leaves the
      signal ignored and unblocked.  Everyone else should have it
      blocked, so there's at most 1 thread with it unblocked. */
   if (tid == VG_(master_tid))
      VG_(sigdelset)(mask, VKI_SIGVGCHLD);
   else
      VG_(sigaddset)(mask, VKI_SIGVGCHLD);
}

/* 
   This updates the thread's signal mask.  There's no such thing as a
   process-wide signal mask.

   Note that the thread signal masks are an implicit part of SCSS,
   which is why this routine is allowed to mess with them.  
*/
static
void do_setmask ( ThreadId tid,
                  Int how,
                  vki_sigset_t* newset,
		  vki_sigset_t* oldset )
{
   if (VG_(clo_trace_signals))
      VG_(message)(Vg_DebugExtraMsg, 
		   "do_setmask: tid = %d how = %d (%s), set = %p %08x%08x", 
		   tid, how,
		   how==VKI_SIG_BLOCK ? "SIG_BLOCK" : (
		      how==VKI_SIG_UNBLOCK ? "SIG_UNBLOCK" : (
			 how==VKI_SIG_SETMASK ? "SIG_SETMASK" : "???")),
		   newset, newset ? newset->sig[1] : 0, newset ? newset->sig[0] : 0
	 );

   /* Just do this thread. */
   vg_assert(VG_(is_valid_tid)(tid));
   if (oldset) {
      *oldset = VG_(threads)[tid].sig_mask;
      if (VG_(clo_trace_signals))
	      VG_(message)(Vg_DebugExtraMsg, 
			   "\toldset=%p %08x%08x",
			   oldset, oldset->sig[1], oldset->sig[0]);
   }
   if (newset) {
      do_sigprocmask_bitops (how, &VG_(threads)[tid].sig_mask, newset );
      VG_(sigdelset)(&VG_(threads)[tid].sig_mask, VKI_SIGKILL);
      VG_(sigdelset)(&VG_(threads)[tid].sig_mask, VKI_SIGSTOP);
      VG_(threads)[tid].tmp_sig_mask = VG_(threads)[tid].sig_mask;
   }
}


void VG_(do_sys_sigprocmask) ( ThreadId tid,
                               Int how, 
                               vki_sigset_t* set,
                               vki_sigset_t* oldset )
{
   switch(how) {
   case VKI_SIG_BLOCK:
   case VKI_SIG_UNBLOCK:
   case VKI_SIG_SETMASK:
      vg_assert(VG_(is_valid_tid)(tid));
      do_setmask ( tid, how, set, oldset );
      SET_SYSCALL_RETVAL(tid, 0);
      VG_(poll_signals)(tid);	/* look for any newly deliverable signals */
      break;

   default:
      VG_(message)(Vg_DebugMsg, 
                  "sigprocmask: unknown `how' field %d", how);
      SET_SYSCALL_RETVAL(tid, -VKI_EINVAL);
      break;
   }
}


void VG_(do_pthread_sigmask_SCSS_upd) ( ThreadId tid,
                                        Int how, 
                                        vki_sigset_t* set,
                                        vki_sigset_t* oldset )
{
   /* Assume that how has been validated by caller. */
   vg_assert(how == VKI_SIG_BLOCK || how == VKI_SIG_UNBLOCK 
                                  || how == VKI_SIG_SETMASK);
   vg_assert(VG_(is_valid_tid)(tid));
   do_setmask ( tid, how, set, oldset );
   /* The request return code is set in do_pthread_sigmask */
}


/* ---------------------------------------------------------------------
   LOW LEVEL STUFF TO DO WITH SIGNALS: IMPLEMENTATION
   ------------------------------------------------------------------ */

/* ---------------------------------------------------------------------
   Handy utilities to block/restore all host signals.
   ------------------------------------------------------------------ */

/* Block all host signals, dumping the old mask in *saved_mask. */
void VG_(block_all_host_signals) ( /* OUT */ vki_sigset_t* saved_mask )
{
   Int           ret;
   vki_sigset_t block_procmask;
   VG_(sigfillset)(&block_procmask);
   ret = VG_(sigprocmask)
            (VKI_SIG_SETMASK, &block_procmask, saved_mask);
   vg_assert(ret == 0);
}

/* Restore the blocking mask using the supplied saved one. */
void VG_(restore_all_host_signals) ( /* IN */ vki_sigset_t* saved_mask )
{
   Int ret;
   ret = VG_(sigprocmask)(VKI_SIG_SETMASK, saved_mask, NULL);
   vg_assert(ret == 0);
}

Bool VG_(client_signal_OK)(Int sigNo)
{
   /* signal 0 is OK for kill */
   Bool ret = sigNo >= 0 && sigNo <= VKI_SIGVGRTUSERMAX;

   //VG_(printf)("client_signal_OK(%d) -> %d\n", sigNo, ret);

   return ret;
}

/* ---------------------------------------------------------------------
   The signal simulation proper.  A simplified version of what the 
   Linux kernel does.
   ------------------------------------------------------------------ */

/* Set up a stack frame (VgSigContext) for the client's signal
   handler. */
void vg_push_signal_frame ( ThreadId tid, const vki_siginfo_t *siginfo )
{
   Addr         esp_top_of_frame;
   ThreadState* tst;
   Int		sigNo = siginfo->si_signo;

   vg_assert(sigNo >= 1 && sigNo <= VG_(max_signal));
   vg_assert(VG_(is_valid_tid)(tid));
   tst = & VG_(threads)[tid];

   if (VG_(clo_trace_signals))
      VG_(message)(Vg_DebugMsg, 
         "vg_push_signal_frame (thread %d): signal %d", tid, sigNo);

   if (/* this signal asked to run on an alt stack */
       (vg_scss.scss_per_sig[sigNo].scss_flags & VKI_SA_ONSTACK )
       && /* there is a defined and enabled alt stack, which we're not
             already using.  Logic from get_sigframe in
             arch/i386/kernel/signal.c. */
          sas_ss_flags(tid, STACK_PTR(tst->arch)) == 0
      ) {
      esp_top_of_frame 
         = (Addr)(tst->altstack.ss_sp) + tst->altstack.ss_size;
      if (VG_(clo_trace_signals))
         VG_(message)(Vg_DebugMsg,
		      "delivering signal %d (%s) to thread %d: on ALT STACK (%p-%p; %d bytes)", 
		      sigNo, signame(sigNo), tid, 
		      tst->altstack.ss_sp,
		      tst->altstack.ss_sp + tst->altstack.ss_size,
		      tst->altstack.ss_size );

      /* Signal delivery to tools */
      VG_TRACK( pre_deliver_signal, tid, sigNo, /*alt_stack*/True );
      
   } else {
      esp_top_of_frame = STACK_PTR(tst->arch);

      /* Signal delivery to tools */
      VG_TRACK( pre_deliver_signal, tid, sigNo, /*alt_stack*/False );
   }

   vg_assert(vg_scss.scss_per_sig[sigNo].scss_handler != VKI_SIG_IGN);
   vg_assert(vg_scss.scss_per_sig[sigNo].scss_handler != VKI_SIG_DFL);

   /* This may fail if the client stack is busted; if that happens,
      the whole process will exit rather than simply calling the
      signal handler. */
   VGA_(push_signal_frame)(tid, esp_top_of_frame, siginfo,
                           vg_scss.scss_per_sig[sigNo].scss_handler,
                           vg_scss.scss_per_sig[sigNo].scss_flags,
			  &tst->sig_mask,
			   vg_scss.scss_per_sig[sigNo].scss_restorer);
}


static const Char *signame(Int sigNo)
{
   static Char buf[10];

   switch(sigNo) {
#define S(x)	case VKI_##x: return #x
      S(SIGHUP);
      S(SIGINT);
      S(SIGQUIT);
      S(SIGILL);
      S(SIGTRAP);
      S(SIGABRT);
      S(SIGBUS);
      S(SIGFPE);
      S(SIGKILL);
      S(SIGUSR1);
      S(SIGUSR2);
      S(SIGSEGV);
      S(SIGPIPE);
      S(SIGALRM);
      S(SIGTERM);
      S(SIGSTKFLT);
      S(SIGCHLD);
      S(SIGCONT);
      S(SIGSTOP);
      S(SIGTSTP);
      S(SIGTTIN);
      S(SIGTTOU);
      S(SIGURG);
      S(SIGXCPU);
      S(SIGXFSZ);
      S(SIGVTALRM);
      S(SIGPROF);
      S(SIGWINCH);
      S(SIGIO);
      S(SIGPWR);
      S(SIGUNUSED);
#undef S

   case VKI_SIGRTMIN ... VKI_SIGRTMAX:
      VG_(sprintf)(buf, "SIGRT%d", sigNo-VKI_SIGRTMIN);
      return buf;

   default:
      VG_(sprintf)(buf, "SIG%d", sigNo);
      return buf;
   }
}

/* Hit ourselves with a signal using the default handler */
void VG_(kill_self)(Int sigNo)
{
   vki_sigset_t	        mask, origmask;
   struct vki_sigaction sa, origsa;   

   sa.ksa_handler = VKI_SIG_DFL;
   sa.sa_flags = 0;
   sa.sa_restorer = 0;
   VG_(sigemptyset)(&sa.sa_mask);
      
   VG_(sigaction)(sigNo, &sa, &origsa);

   VG_(sigemptyset)(&mask);
   VG_(sigaddset)(&mask, sigNo);
   VG_(sigprocmask)(VKI_SIG_UNBLOCK, &mask, &origmask);

   VG_(tkill)(VG_(getpid)(), sigNo);

   VG_(sigaction)(sigNo, &origsa, NULL);
   VG_(sigprocmask)(VKI_SIG_SETMASK, &origmask, NULL);
}

// Core dumping is disabled until someone can work out how to abstract out
// the arch-specific and word-size-specific parts neatly.
//
// Note that the code below is not 64-bit clean!
//
#if 0
/*
  Dump core
   
  Generate a standard ELF core file corresponding to the client state
  at the time of a crash.
 */
#include <elf.h>
#ifndef NT_PRXFPREG
#define NT_PRXFPREG     0x46e62b7f      /* copied from gdb5.1/include/elf/common.h */
#endif /* NT_PRXFPREG */

/* If true, then this Segment may be mentioned in the core */
static Bool may_dump(const Segment *seg)
{
   return (seg->flags & (SF_DEVICE|SF_VALGRIND)) == 0 && VG_(is_client_addr)(seg->addr);
}

/* If true, then this Segment's contents will be in the core */
static Bool should_dump(const Segment *seg)
{
   return may_dump(seg); // && (seg->prot & VKI_PROT_WRITE);
}

static void fill_ehdr(Elf32_Ehdr *ehdr, Int num_phdrs)
{
   VG_(memset)(ehdr, 0, sizeof(*ehdr));

   VG_(memcpy)(ehdr->e_ident, ELFMAG, SELFMAG);
   ehdr->e_ident[EI_CLASS]   = VG_ELF_CLASS;
   ehdr->e_ident[EI_DATA]    = VG_ELF_ENDIANNESS;
   ehdr->e_ident[EI_VERSION] = EV_CURRENT;

   ehdr->e_type = ET_CORE;
   ehdr->e_machine = VG_ELF_MACHINE;
   ehdr->e_version = EV_CURRENT;
   ehdr->e_entry = 0;
   ehdr->e_phoff = sizeof(Elf32_Ehdr);
   ehdr->e_shoff = 0;
   ehdr->e_flags = 0;
   ehdr->e_ehsize = sizeof(Elf32_Ehdr);
   ehdr->e_phentsize = sizeof(Elf32_Phdr);
   ehdr->e_phnum = num_phdrs;
   ehdr->e_shentsize = 0;
   ehdr->e_shnum = 0;
   ehdr->e_shstrndx = 0;

}

static void fill_phdr(Elf32_Phdr *phdr, const Segment *seg, UInt off, Bool write)
{
   write = write && should_dump(seg);

   VG_(memset)(phdr, 0, sizeof(*phdr));

   phdr->p_type = PT_LOAD;
   phdr->p_offset = off;
   phdr->p_vaddr = seg->addr;
   phdr->p_paddr = 0;
   phdr->p_filesz = write ? seg->len : 0;
   phdr->p_memsz = seg->len;
   phdr->p_flags = 0;

   if (seg->prot & VKI_PROT_READ)
      phdr->p_flags |= PF_R;
   if (seg->prot & VKI_PROT_WRITE)
      phdr->p_flags |= PF_W;
   if (seg->prot & VKI_PROT_EXEC)
      phdr->p_flags |= PF_X;

   phdr->p_align = VKI_PAGE_SIZE;
}

struct note {
   struct note *next;
   Elf32_Nhdr note;
   Char name[0];
};

static UInt note_size(const struct note *n)
{
   return sizeof(Elf32_Nhdr) + ROUNDUP(VG_(strlen)(n->name)+1, 4) + ROUNDUP(n->note.n_descsz, 4);
}

static void add_note(struct note **list, const Char *name, UInt type, const void *data, UInt datasz)
{
   Int namelen = VG_(strlen)(name)+1;
   Int notelen = sizeof(struct note) + 
      ROUNDUP(namelen, 4) + 
      ROUNDUP(datasz, 4);
   struct note *n = VG_(arena_malloc)(VG_AR_CORE, notelen);

   VG_(memset)(n, 0, notelen);

   n->next = *list;
   *list = n;

   n->note.n_type = type;
   n->note.n_namesz = namelen;
   n->note.n_descsz = datasz;

   VG_(memcpy)(n->name, name, namelen);
   VG_(memcpy)(n->name+ROUNDUP(namelen,4), data, datasz);
}

static void write_note(Int fd, const struct note *n)
{
   VG_(write)(fd, &n->note, note_size(n));
}

static void fill_prpsinfo(const ThreadState *tst, struct vki_elf_prpsinfo *prpsinfo)
{
   Char *name;

   VG_(memset)(prpsinfo, 0, sizeof(*prpsinfo));

   switch(tst->status) {
   case VgTs_Runnable:
   case VgTs_Yielding:
      prpsinfo->pr_sname = 'R';
      break;

   case VgTs_WaitSys:
      prpsinfo->pr_sname = 'S';
      break;

   case VgTs_Zombie:
      prpsinfo->pr_sname = 'Z';
      break;

   case VgTs_Empty:
   case VgTs_Init:
      prpsinfo->pr_sname = '?';
      break;
   }

   prpsinfo->pr_uid = 0;
   prpsinfo->pr_gid = 0;
   
   name = VG_(resolve_filename)(VG_(clexecfd));

   if (name != NULL) {
      Char *n = name+VG_(strlen)(name)-1;

      while(n > name && *n != '/')
	 n--;
      if (n != name)
	 n++;

      VG_(strncpy)(prpsinfo->pr_fname, n, sizeof(prpsinfo->pr_fname));
   }
}

static void fill_prstatus(const ThreadState *tst, 
			  struct vki_elf_prstatus *prs, 
			  const vki_siginfo_t *si)
{
   struct vki_user_regs_struct *regs;

   VG_(memset)(prs, 0, sizeof(*prs));

   prs->pr_info.si_signo = si->si_signo;
   prs->pr_info.si_code = si->si_code;
   prs->pr_info.si_errno = 0;

   prs->pr_cursig = si->si_signo;

   prs->pr_pid = tst->os_state.lwpid;
   prs->pr_ppid = 0;
   prs->pr_pgrp = VG_(getpgrp)();
   prs->pr_sid = VG_(getpgrp)();
   
   regs = (struct vki_user_regs_struct *)prs->pr_reg;

   vg_assert(sizeof(*regs) == sizeof(prs->pr_reg));

   VGA_(fill_elfregs_from_tst)(regs, &tst->arch);
}

static void fill_fpu(const ThreadState *tst, vki_elf_fpregset_t *fpu)
{
   VGA_(fill_elffpregs_from_tst)(fpu, &tst->arch);
}

static void fill_xfpu(const ThreadState *tst, vki_elf_fpxregset_t *xfpu)
{
   VGA_(fill_elffpxregs_from_tst)(xfpu, &tst->arch);
}

static void make_coredump(ThreadId tid, const vki_siginfo_t *si, UInt max_size)
{
   Char buf[1000];
   Char *basename = "vgcore";
   Char *coreext = "";
   Int seq = 0;
   Int core_fd;
   Segment *seg;
   Elf32_Ehdr ehdr;
   Elf32_Phdr *phdrs;
   Int num_phdrs;
   Int i, idx;
   UInt off;
   struct note *notelist, *note;
   UInt notesz;
   struct vki_elf_prpsinfo prpsinfo;
   struct vki_elf_prstatus prstatus;

   if (VG_(clo_log_name) != NULL) {
      coreext = ".core";
      basename = VG_(clo_log_name);
   }

   for(;;) {
      if (seq == 0)
	 VG_(sprintf)(buf, "%s%s.pid%d",
		      basename, coreext, VG_(getpid)());
      else
	 VG_(sprintf)(buf, "%s%s.pid%d.%d",
		      basename, coreext, VG_(getpid)(), seq);
      seq++;

      core_fd = VG_(open)(buf, 			   
			  VKI_O_CREAT|VKI_O_WRONLY|VKI_O_EXCL|VKI_O_TRUNC, 
			  VKI_S_IRUSR|VKI_S_IWUSR);
      if (core_fd >= 0)
	 break;

      if (core_fd != -VKI_EEXIST)
	 return;		/* can't create file */
   }

   /* First, count how many memory segments to dump */
   num_phdrs = 1;		/* start with notes */
   for(seg = VG_(first_segment)();
       seg != NULL;
       seg = VG_(next_segment)(seg)) {
      if (!may_dump(seg))
	 continue;

      num_phdrs++;
   }

   fill_ehdr(&ehdr, num_phdrs);

   notelist = NULL;

   /* Second, work out their layout */
   phdrs = VG_(arena_malloc)(VG_AR_CORE, sizeof(*phdrs) * num_phdrs);

   for(i = 1; i < VG_N_THREADS; i++) {
      vki_elf_fpregset_t  fpu;
      vki_elf_fpxregset_t xfpu;

      if (VG_(threads)[i].status == VgTs_Empty)
	 continue;

      fill_xfpu(&VG_(threads)[i], &xfpu);
      add_note(&notelist, "LINUX", NT_PRXFPREG, &xfpu, sizeof(xfpu));

      fill_fpu(&VG_(threads)[i], &fpu);
      add_note(&notelist, "CORE", NT_FPREGSET, &fpu, sizeof(fpu));

      fill_prstatus(&VG_(threads)[i], &prstatus, si);
      add_note(&notelist, "CORE", NT_PRSTATUS, &prstatus, sizeof(prstatus));
   }

   fill_prpsinfo(&VG_(threads)[tid], &prpsinfo);
   add_note(&notelist, "CORE", NT_PRPSINFO, &prpsinfo, sizeof(prpsinfo));

   for(note = notelist, notesz = 0; note != NULL; note = note->next)
      notesz += note_size(note);

   off = sizeof(ehdr) + sizeof(*phdrs) * num_phdrs;

   phdrs[0].p_type = PT_NOTE;
   phdrs[0].p_offset = off;
   phdrs[0].p_vaddr = 0;
   phdrs[0].p_paddr = 0;
   phdrs[0].p_filesz = notesz;
   phdrs[0].p_memsz = 0;
   phdrs[0].p_flags = 0;
   phdrs[0].p_align = 0;

   off += notesz;

   off = PGROUNDUP(off);

   for(seg = VG_(first_segment)(), idx = 1;
       seg != NULL;
       seg = VG_(next_segment)(seg)) {
      if (!may_dump(seg))
	 continue;

      fill_phdr(&phdrs[idx], seg, off, (seg->len + off) < max_size);
      
      off += phdrs[idx].p_filesz;

      idx++;
   }

   /* write everything out */
   VG_(write)(core_fd, &ehdr, sizeof(ehdr));
   VG_(write)(core_fd, phdrs, sizeof(*phdrs) * num_phdrs);

   for(note = notelist; note != NULL; note = note->next)
      write_note(core_fd, note);
   
   VG_(lseek)(core_fd, phdrs[1].p_offset, VKI_SEEK_SET);

   for(seg = VG_(first_segment)(), idx = 1;
       seg != NULL;
       seg = VG_(next_segment)(seg)) {
      if (!should_dump(seg))
	 continue;

      if (phdrs[idx].p_filesz > 0) {
	 Int ret;

	 vg_assert(VG_(lseek)(core_fd, phdrs[idx].p_offset, VKI_SEEK_SET) == phdrs[idx].p_offset);
	 vg_assert(seg->len >= phdrs[idx].p_filesz);

	 ret = VG_(write)(core_fd, (void *)seg->addr, phdrs[idx].p_filesz);
      }
      idx++;
   }

   VG_(close)(core_fd);
}
#endif

/* 
   Perform the default action of a signal.  If the signal is fatal, it
   marks all threads as needing to exit, but it doesn't actually kill
   the process or thread.

   If we're not being quiet, then print out some more detail about
   fatal signals (esp. core dumping signals).
 */
static void vg_default_action(const vki_siginfo_t *info, ThreadId tid)
{
   Int  sigNo     = info->si_signo;
   Bool terminate = False;	/* kills process         */
   Bool core      = False;	/* kills process w/ core */
   struct vki_rlimit corelim;
   Bool could_core;

   vg_assert(VG_(is_running_thread)(tid));
   
   switch(sigNo) {
   case VKI_SIGQUIT:	/* core */
   case VKI_SIGILL:	/* core */
   case VKI_SIGABRT:	/* core */
   case VKI_SIGFPE:	/* core */
   case VKI_SIGSEGV:	/* core */
   case VKI_SIGBUS:	/* core */
   case VKI_SIGTRAP:	/* core */
   case VKI_SIGXCPU:	/* core */
   case VKI_SIGXFSZ:	/* core */
      terminate = True;
      core = True;
      break;

   case VKI_SIGHUP:	/* term */
   case VKI_SIGINT:	/* term */
   case VKI_SIGKILL:	/* term - we won't see this */
   case VKI_SIGPIPE:	/* term */
   case VKI_SIGALRM:	/* term */
   case VKI_SIGTERM:	/* term */
   case VKI_SIGUSR1:	/* term */
   case VKI_SIGUSR2:	/* term */
   case VKI_SIGIO:	/* term */
   case VKI_SIGPWR:	/* term */
   case VKI_SIGSYS:	/* term */
   case VKI_SIGPROF:	/* term */
   case VKI_SIGVTALRM:	/* term */
   case VKI_SIGRTMIN ... VKI_SIGRTMAX: /* term */
      terminate = True;
      break;
   }

   vg_assert(!core || (core && terminate));

   if (VG_(clo_trace_signals))
      VG_(message)(Vg_DebugMsg, "delivering %d (code %d) to default handler; action: %s%s",
		   sigNo, info->si_code, terminate ? "terminate" : "ignore", core ? "+core" : "");

   if (!terminate)
      return;			/* nothing to do */

   could_core = core;

   if (core) {
      /* If they set the core-size limit to zero, don't generate a
	 core file */
	 
      VG_(getrlimit)(VKI_RLIMIT_CORE, &corelim);

      if (corelim.rlim_cur == 0)
	 core = False;
   }

   if (VG_(clo_verbosity) > 1 || (could_core && info->si_code > VKI_SI_USER)) {
      VG_(message)(Vg_UserMsg, "");
      VG_(message)(Vg_UserMsg, "Process terminating with default action of signal %d (%s)%s", 
		   sigNo, signame(sigNo), core ? ": dumping core" : "");

      /* Be helpful - decode some more details about this fault */
      if (info->si_code > VKI_SI_USER) {
	 const Char *event = NULL;
	 Bool haveaddr = True;

	 switch(sigNo) {
	 case VKI_SIGSEGV:
	    switch(info->si_code) {
	    case 1: event = "Access not within mapped region"; break;
	    case 2: event = "Bad permissions for mapped region"; break;
	    case 128:
	       /* General Protection Fault: The CPU/kernel
		  isn't telling us anything useful, but this
		  is commonly the result of exceeding a
		  segment limit, such as the one imposed by
		  --pointercheck=yes. */
	       if (VG_(clo_pointercheck))
		  event = "GPF (Pointer out of bounds?)"; 
	       else
		  event = "General Protection Fault"; 
	       haveaddr = False;
	       break;
	    }
	    break;

	 case VKI_SIGILL:
	    switch(info->si_code) {
	    case 1: event = "Illegal opcode"; break;
	    case 2: event = "Illegal operand"; break;
	    case 3: event = "Illegal addressing mode"; break;
	    case 4: event = "Illegal trap"; break;
	    case 5: event = "Privileged opcode"; break;
	    case 6: event = "Privileged register"; break;
	    case 7: event = "Coprocessor error"; break;
	    case 8: event = "Internal stack error"; break;
	    }
	    break;

	 case VKI_SIGFPE:
	    switch (info->si_code) {
	    case 1: event = "Integer divide by zero"; break;
	    case 2: event = "Integer overflow"; break;
	    case 3: event = "FP divide by zero"; break;
	    case 4: event = "FP overflow"; break;
	    case 5: event = "FP underflow"; break;
	    case 6: event = "FP inexact"; break;
	    case 7: event = "FP invalid operation"; break;
	    case 8: event = "FP subscript out of range"; break;
	    }
	    break;

	 case VKI_SIGBUS:
	    switch (info->si_code) {
	    case 1: event = "Invalid address alignment"; break;
	    case 2: event = "Non-existent physical address"; break;
	    case 3: event = "Hardware error"; break;
	    }
	    break;
	 }

	 if (event != NULL) {
	    if (haveaddr)
	       VG_(message)(Vg_UserMsg, " %s at address %p", 
			    event, info->_sifields._sigfault._addr);
	    else
	       VG_(message)(Vg_UserMsg, " %s", event);
	 }
      }

      if (tid != VG_INVALID_THREADID) {
	 ExeContext *ec = VG_(get_ExeContext)(tid);
	 VG_(pp_ExeContext)(ec);
      }
   }

   if (VG_(is_action_requested)( "Attach to debugger", & VG_(clo_db_attach) )) {
      VG_(start_debugger)( tid );
   }

      // See comment above about this temporary disabling of core dumps.
      #if 0
   if (core) {
      const static struct vki_rlimit zero = { 0, 0 };

      make_coredump(tid, info, corelim.rlim_cur);

      /* Make sure we don't get a confusing kernel-generated
	 coredump when we finally exit */
      VG_(setrlimit)(VKI_RLIMIT_CORE, &zero);
   }
      #endif

   /* stash fatal signal in main thread */
   VG_(threads)[VG_(master_tid)].os_state.fatalsig = sigNo;

   /* everyone dies */
   VG_(nuke_all_threads_except)(tid, VgSrc_FatalSig);
   VG_(threads)[tid].exitreason = VgSrc_FatalSig;
   VG_(threads)[tid].os_state.fatalsig = sigNo;
}

static void synth_fault_common(ThreadId tid, Addr addr, Int si_code)
{
   vki_siginfo_t info;

   vg_assert(VG_(threads)[tid].status == VgTs_Runnable);

   info.si_signo = VKI_SIGSEGV;
   info.si_code = si_code;
   info._sifields._sigfault._addr = (void*)addr;

   /* If they're trying to block the signal, force it to be delivered */
   if (VG_(sigismember)(&VG_(threads)[tid].sig_mask, VKI_SIGSEGV))
      VG_(set_default_handler)(VKI_SIGSEGV);

   VG_(deliver_signal)(tid, &info);
}

// Synthesize a fault where the address is OK, but the page
// permissions are bad.
void VG_(synth_fault_perms)(ThreadId tid, Addr addr)
{
   synth_fault_common(tid, addr, 2);
}

// Synthesize a fault where the address there's nothing mapped at the address.
void VG_(synth_fault_mapping)(ThreadId tid, Addr addr)
{
   synth_fault_common(tid, addr, 1);
}

// Synthesize a misc memory fault.
void VG_(synth_fault)(ThreadId tid)
{
   synth_fault_common(tid, 0, 0x80);
}

// Synthesise a SIGILL.
void VG_(synth_sigill)(ThreadId tid, Addr addr)
{
   vki_siginfo_t info;

   vg_assert(VG_(threads)[tid].status == VgTs_Runnable);

   info.si_signo = VKI_SIGILL;
   info.si_code = 1; /* jrs: no idea what this should be */
   info._sifields._sigfault._addr = (void*)addr;

   VG_(resume_scheduler)(tid);
   VG_(deliver_signal)(tid, &info);
}

/* 
   This does the business of delivering a signal to a thread.  It may
   be called from either a real signal handler, or from normal code to
   cause the thread to enter the signal handler.

   This updates the thread state, but it does not set it to be
   Runnable.
*/
void VG_(deliver_signal) ( ThreadId tid, 
			   const vki_siginfo_t *info )

{
   Int			sigNo = info->si_signo;
   SCSS_Per_Signal	*handler = &vg_scss.scss_per_sig[sigNo];
   void			*handler_fn;
   ThreadState		*tst = VG_(get_ThreadState)(tid);

   if (VG_(clo_trace_signals))
      VG_(message)(Vg_DebugMsg,"delivering signal %d (%s):%d to thread %d", 
		   sigNo, signame(sigNo), info->si_code, tid );

   if (sigNo == VKI_SIGVGKILL) {
      /* If this is a SIGVGKILL, we're expecting it to interrupt any
	 blocked syscall.  It doesn't matter whether the VCPU state is
	 set to restart or not, because we don't expect it will
	 execute any more client instructions. */
      vg_assert(VG_(is_exiting)(tid));
      return;
   }

   /* If the client specifies SIG_IGN, treat it as SIG_DFL.

      If VG_(deliver_signal)() is being called on a thread, we want
      the signal to get through no matter what; if they're ignoring
      it, then we do this override (this is so we can send it SIGSEGV,
      etc). */
   handler_fn = handler->scss_handler;
   if (handler_fn == VKI_SIG_IGN) 
      handler_fn = VKI_SIG_DFL;

   vg_assert(handler_fn != VKI_SIG_IGN);

   if (handler_fn == VKI_SIG_DFL) {
      vg_default_action(info, tid);
   } else {
      /* Create a signal delivery frame, and set the client's %ESP and
	 %EIP so that when execution continues, we will enter the
	 signal handler with the frame on top of the client's stack,
	 as it expects.

	 Signal delivery can fail if the client stack is too small or
	 missing, and we can't push the frame.  If that happens,
	 push_signal_frame will cause the whole process to exit when
	 we next hit the scheduler.
      */
      vg_assert(VG_(is_valid_tid)(tid));

      vg_push_signal_frame ( tid, info );

      if (handler->scss_flags & VKI_SA_ONESHOT) {
	 /* Do the ONESHOT thing. */
	 handler->scss_handler = VKI_SIG_DFL;

	 handle_SCSS_change( False /* lazy update */ );
      }

      /* At this point:
	 tst->sig_mask is the current signal mask
	 tst->tmp_sig_mask is the same as sig_mask, unless we're in sigsuspend
	 handler->scss_mask is the mask set by the handler

	 Handler gets a mask of tmp_sig_mask|handler_mask|signo
       */
      tst->sig_mask = tst->tmp_sig_mask;
      if (!(handler->scss_flags & VKI_SA_NOMASK)) {
	 VG_(sigaddset_from_set)(&tst->sig_mask, &handler->scss_mask);
	 VG_(sigaddset)(&tst->sig_mask, sigNo);

	 tst->tmp_sig_mask = tst->sig_mask;
      }
   }

   /* Thread state is ready to go - just add Runnable */
}

/* Make a signal pending for a thread, for later delivery.
   VG_(poll_signals) will arrange for it to be delivered at the right
   time. 

   tid==0 means add it to the process-wide queue, and not sent it to a
   specific thread.
*/
void queue_signal(ThreadId tid, const vki_siginfo_t *si)
{
   ThreadState *tst;
   SigQueue *sq;
   vki_sigset_t savedmask;

   tst = VG_(get_ThreadState)(tid);

   /* Protect the signal queue against async deliveries */
   VG_(block_all_host_signals)(&savedmask);

   if (tst->sig_queue == NULL) {
      tst->sig_queue = VG_(arena_malloc)(VG_AR_CORE, sizeof(*tst->sig_queue));
      VG_(memset)(tst->sig_queue, 0, sizeof(*tst->sig_queue));
   }
   sq = tst->sig_queue;

   if (VG_(clo_trace_signals))
      VG_(message)(Vg_DebugMsg, "Queueing signal %d (idx %d) to thread %d",
		   si->si_signo, sq->next, tid);

   /* Add signal to the queue.  If the queue gets overrun, then old
      queued signals may get lost. 

      XXX We should also keep a sigset of pending signals, so that at
      least a non-siginfo signal gets deliviered.
   */
   if (sq->sigs[sq->next].si_signo != 0)
      VG_(message)(Vg_UserMsg, "Signal %d being dropped from thread %d's queue",
		   sq->sigs[sq->next].si_signo, tid);

   sq->sigs[sq->next] = *si;
   sq->next = (sq->next+1) % N_QUEUED_SIGNALS;

   VG_(restore_all_host_signals)(&savedmask);
}

/*
   Returns the next queued signal for thread tid which is in "set".
   tid==0 means process-wide signal.  Set si_signo to 0 when the
   signal has been delivered.

   Must be called with all signals blocked, to protect against async
   deliveries.
*/
static vki_siginfo_t *next_queued(ThreadId tid, const vki_sigset_t *set)
{
   ThreadState *tst = VG_(get_ThreadState)(tid);
   SigQueue *sq;
   Int idx;
   vki_siginfo_t *ret = NULL;

   sq = tst->sig_queue;
   if (sq == NULL)
      goto out;
   
   idx = sq->next;
   do {
      if (0)
	 VG_(printf)("idx=%d si_signo=%d inset=%d\n", idx,
		     sq->sigs[idx].si_signo, VG_(sigismember)(set, sq->sigs[idx].si_signo));

      if (sq->sigs[idx].si_signo != 0 && VG_(sigismember)(set, sq->sigs[idx].si_signo)) {
	 if (VG_(clo_trace_signals))
	    VG_(message)(Vg_DebugMsg, "Returning queued signal %d (idx %d) for thread %d",
			 sq->sigs[idx].si_signo, idx, tid);
	 ret = &sq->sigs[idx];
	 goto out;
      }

      idx = (idx + 1) % N_QUEUED_SIGNALS;
   } while(idx != sq->next);
  out:   
   return ret;   
}

/* 
   Receive an async signal from the kernel.

   This should only happen when the thread is blocked in a syscall,
   since that's the only time this set of signals is unblocked.
*/
static 
void vg_async_signalhandler ( Int sigNo, vki_siginfo_t *info, struct vki_ucontext *uc )
{
   ThreadId tid = VG_(get_lwp_tid)(VG_(gettid)());
   ThreadState *tst = VG_(get_ThreadState)(tid);

   vg_assert(tst->status == VgTs_WaitSys);

   /* The thread isn't currently running, make it so before going on */
   VG_(set_running)(tid);

   if (VG_(clo_trace_signals))
      VG_(message)(Vg_DebugMsg, "Async handler got signal %d for tid %d info %d",
		   sigNo, tid, info->si_code);

   /* Update thread state properly */
   VGA_(interrupted_syscall)(tid, uc, 
			     !!(vg_scss.scss_per_sig[sigNo].scss_flags & VKI_SA_RESTART));

   /* Set up the thread's state to deliver a signal */
   if (!VG_(is_sig_ign)(info->si_signo))
      VG_(deliver_signal)(tid, info);

   /* longjmp back to the thread's main loop to start executing the
      handler. */
   VG_(resume_scheduler)(tid);

   VG_(core_panic)("vg_async_signalhandler: got unexpected signal while outside of scheduler");
}

/* Extend the stack to cover addr.  maxsize is the limit the stack can grow to.

   Returns True on success, False on failure.

   Succeeds without doing anything if addr is already within a segment.

   Failure could be caused by:
   - addr not below a growable segment
   - new stack size would exceed maxsize
   - mmap failed for some other reason
 */
Bool VG_(extend_stack)(Addr addr, UInt maxsize)
{
   Segment *seg;
   Addr base;
   UInt newsize;

   /* Find the next Segment above addr */
   seg = VG_(find_segment)(addr);
   if (seg)
      return True;

   /* now we know addr is definitely unmapped */
   seg = VG_(find_segment_above_unmapped)(addr);

   /* If there isn't one, or it isn't growable, fail */
   if (seg == NULL || 
       !(seg->flags & SF_GROWDOWN) ||
       VG_(seg_contains)(seg, addr, sizeof(void *)))
      return False;
       
   vg_assert(seg->addr > addr);

   /* Create the mapping */
   base = PGROUNDDN(addr);
   newsize = seg->addr - base;

   if (seg->len + newsize >= maxsize)
      return False;

   if (VG_(mmap)((Char *)base, newsize,
		 seg->prot,
		 VKI_MAP_PRIVATE | VKI_MAP_FIXED | VKI_MAP_ANONYMOUS | VKI_MAP_CLIENT,
		 seg->flags,
		 -1, 0) == (void *)-1)
      return False;

   if (0)
      VG_(printf)("extended stack: %p %d\n",
		  base, newsize);

   if (VG_(clo_sanity_level) > 2)
      VG_(sanity_check_general)(False);

   return True;
}

static void (*fault_catcher)(Int sig, Addr addr);

void VG_(set_fault_catcher)(void (*catcher)(Int, Addr))
{
   if (catcher != NULL && fault_catcher != NULL)
      VG_(core_panic)("Fault catcher is already registered");

   fault_catcher = catcher;
}


/* 
   Receive a sync signal from the host. 
*/
static
void vg_sync_signalhandler ( Int sigNo, vki_siginfo_t *info, struct vki_ucontext *uc )
{
   ThreadId tid = VG_(get_lwp_tid)(VG_(gettid)());

   vg_assert(info != NULL);
   vg_assert(info->si_signo == sigNo);
   vg_assert(sigNo == VKI_SIGSEGV ||
	     sigNo == VKI_SIGBUS  ||
	     sigNo == VKI_SIGFPE  ||
	     sigNo == VKI_SIGILL  ||
	     sigNo == VKI_SIGTRAP);

   if (info->si_code <= VKI_SI_USER) {
      /* If some user-process sent us one of these signals (ie,
	 they're not the result of a faulting instruction), then treat
	 it as an async signal.  This is tricky because we could get
	 this almost anywhere:
	  - while generated client code
	    Action: queue signal and return
	  - while running Valgrind code
	    Action: queue signal and return
	  - while blocked in a syscall
	    Action: make thread runnable, queue signal, resume scheduler
      */
      if (VG_(threads)[tid].status == VgTs_WaitSys) {
	 /* Since this signal interrupted a syscall, it means the
	    client's signal mask was applied, so we can't get here
	    unless the client wants this signal right now.  This means
	    we can simply use the async_signalhandler. */
	 vg_async_signalhandler(sigNo, info, uc);
	 VG_(core_panic)("vg_async_signalhandler returned!?\n");
      }

      if (info->_sifields._kill._pid == 0) {
	 /* There's a per-user limit of pending siginfo signals.  If
	    you exceed this, by having more than that number of
	    pending signals with siginfo, then new signals are
	    delivered without siginfo.  This condition can be caused
	    by any unrelated program you're running at the same time
	    as Valgrind, if it has a large number of pending siginfo
	    signals which it isn't taking delivery of.

	    Since we depend on siginfo to work out why we were sent a
	    signal and what we should do about it, we really can't
	    continue unless we get it. */
	 VG_(message)(Vg_UserMsg, "Signal %d (%s) appears to have lost its siginfo; I can't go on.",
		      sigNo, signame(sigNo));
	 VG_(message)(Vg_UserMsg, "  This may be because one of your programs has consumed your");
	 VG_(message)(Vg_UserMsg, "  ration of siginfo structures.");

	 /* It's a fatal signal, so we force the default handler. */
	 VG_(set_default_handler)(sigNo);
	 VG_(deliver_signal)(tid, info);
	 VG_(resume_scheduler)(tid);
	 VG_(exit)(99);		/* If we can't resume, then just exit */
      }

      if (VG_(clo_trace_signals))
	 VG_(message)(Vg_DebugMsg, "Routing user-sent sync signal %d via queue",
		      sigNo);

      /* Since every thread has these signals unblocked, we can't rely
	 on the kernel to route them properly, so we need to queue
	 them manually. */
      if (info->si_code == VKI_SI_TKILL)
	 queue_signal(tid, info); /* directed to us specifically */
      else
	 queue_signal(0, info);	/* shared pending */

      return;
   } 

   if (VG_(clo_trace_signals)) {
      VG_(message)(Vg_DebugMsg, "signal %d arrived ... si_code=%d, EIP=%p, eip=%p",
                   sigNo, info->si_code, 
		   INSTR_PTR(VG_(threads)[tid].arch), 
		   UCONTEXT_INSTR_PTR(uc) );
   }
   vg_assert(sigNo >= 1 && sigNo <= VG_(max_signal));

   /* Special fault-handling case. We can now get signals which can
      act upon and immediately restart the faulting instruction.
    */
   if (info->si_signo == VKI_SIGSEGV) {
      Addr fault = (Addr)info->_sifields._sigfault._addr;
      Addr esp   =  STACK_PTR(VG_(threads)[tid].arch);
      Segment* seg;

      seg = VG_(find_segment)(fault);
      if (seg == NULL)
         seg = VG_(find_segment_above_unmapped)(fault);

      if (VG_(clo_trace_signals)) {
	 if (seg == NULL)
	    VG_(message)(Vg_DebugMsg,
			 "SIGSEGV: si_code=%d faultaddr=%p tid=%d ESP=%p seg=NULL shad=%p-%p",
			 info->si_code, fault, tid, esp,
			 VG_(shadow_base), VG_(shadow_end));
	 else
	    VG_(message)(Vg_DebugMsg,
			 "SIGSEGV: si_code=%d faultaddr=%p tid=%d ESP=%p seg=%p-%p fl=%x shad=%p-%p",
			 info->si_code, fault, tid, esp, seg->addr, seg->addr+seg->len, seg->flags,
			 VG_(shadow_base), VG_(shadow_end));
      }
      if (info->si_code == 1 /* SEGV_MAPERR */
	  && fault >= (esp - ARCH_STACK_REDZONE_SIZE)) {
	 /* If the fault address is above esp but below the current known
	    stack segment base, and it was a fault because there was
	    nothing mapped there (as opposed to a permissions fault),
	    then extend the stack segment. 
	 */
         Addr base = PGROUNDDN(esp - ARCH_STACK_REDZONE_SIZE);
	 if (VG_(extend_stack)(base, VG_(threads)[tid].stack_size)) {
	    if (VG_(clo_trace_signals))
	       VG_(message)(Vg_DebugMsg, 
			    "       -> extended stack base to %p", PGROUNDDN(fault));
	    return;             // extension succeeded, restart instruction
	 } else
	    VG_(message)(Vg_UserMsg, "Stack overflow in thread %d: can't grow stack to %p", 
			 tid, fault);

	 /* Fall into normal signal handling for all other cases */
      } else if (info->si_code == 2 && /* SEGV_ACCERR */
		 VG_(needs).shadow_memory &&
		 VG_(is_shadow_addr)(fault)) {
	 /* If there's a fault within the shadow memory range, and it
	    is a permissions fault, then it means that the client is
	    using some memory which had not previously been used.
	    This catches those faults, makes the memory accessible,
	    and calls the tool to initialize that page.
	 */
	 static Int recursion = 0;

	 if (recursion++ == 0) {
	    VG_(init_shadow_range)(PGROUNDDN(fault), VKI_PAGE_SIZE, True);
	    recursion--;
	    return;
	 } else {
	    /* otherwise fall into normal SEGV handling */	    
	    recursion--;
	 }
      }
   }

   /* OK, this is a signal we really have to deal with.  If it came
      from the client's code, then we can jump back into the scheduler
      and have it delivered.  Otherwise it's a Valgrind bug. */
   {   
      Addr context_ip;
      Char buf[1024];
      ThreadState *tst = VG_(get_ThreadState)(VG_(get_lwp_tid)(VG_(gettid)()));

      if (VG_(sigismember)(&tst->sig_mask, sigNo)) {
	 /* signal is blocked, but they're not allowed to block faults */
	 VG_(set_default_handler)(sigNo);
      }

      if (!VG_(my_fault)) {
	 /* Can't continue; must longjmp back to the scheduler and thus
	    enter the sighandler immediately. */
	 VG_(deliver_signal)(tid, info);
	 VG_(resume_scheduler)(tid);
      }

      /* Check to see if someone is interested in faults. */
      if (fault_catcher) {
	 (*fault_catcher)(sigNo, (Addr)info->_sifields._sigfault._addr);

	 /* If the catcher returns, then it didn't handle the fault,
	    so carry on panicing. */
      }

      /* If resume_scheduler returns or its our fault, it means we
	 don't have longjmp set up, implying that we weren't running
	 client code, and therefore it was actually generated by
	 Valgrind internally.
       */
      VG_(message)(Vg_DebugMsg, 
		   "INTERNAL ERROR: Valgrind received a signal %d (%s) - exiting",
		   sigNo, signame(sigNo));

      buf[0] = 0;
      context_ip = UCONTEXT_INSTR_PTR(uc);
      if (1 && !VG_(get_fnname)(context_ip, buf+2, sizeof(buf)-5)) {
	 Int len;

	 buf[0] = ' ';
	 buf[1] = '(';
	 len = VG_(strlen)(buf);
	 buf[len] = ')';
	 buf[len+1] = '\0';
      }

      VG_(message)(Vg_DebugMsg, 
		   "si_code=%x Fault EIP: %p%s; Faulting address: %p",
		   info->si_code, context_ip, buf, info->_sifields._sigfault._addr);
      VG_(message)(Vg_DebugMsg, 
		   "  esp=%p\n", uc->uc_mcontext.esp);

      if (0)
	 VG_(kill_self)(sigNo);		/* generate a core dump */

      tst = VG_(get_ThreadState)(VG_(get_lwp_tid)(VG_(gettid)()));
      VG_(core_panic_at)("Killed by fatal signal",
                         VG_(get_ExeContext2)(UCONTEXT_INSTR_PTR(uc),
                                              UCONTEXT_FRAME_PTR(uc),
                                              UCONTEXT_STACK_PTR(uc),
                                              (Addr)(tst->os_state.stack + tst->os_state.stacksize)));
   }
}


/* 
   Kill this thread.  Makes it leave any syscall it might be currently
   blocked in, and return to the scheduler.  This doesn't mark the thread
   as exiting; that's the caller's job.
 */
static void sigvgkill_handler(int signo, vki_siginfo_t *si, struct vki_ucontext *uc)
{
   ThreadId tid = VG_(get_lwp_tid)(VG_(gettid)());

   if (VG_(clo_trace_signals))
      VG_(message)(Vg_DebugMsg, "sigvgkill for lwp %d tid %d", VG_(gettid)(), tid);

   vg_assert(signo == VKI_SIGVGKILL);
   vg_assert(si->si_signo == signo);
   vg_assert(VG_(threads)[tid].status == VgTs_WaitSys);

   VG_(set_running)(tid);
   VG_(post_syscall)(tid);

   VG_(resume_scheduler)(tid);

   VG_(core_panic)("sigvgkill_handler couldn't return to the scheduler\n");
}

static __attribute((unused))
void pp_vg_ksigaction ( struct vki_sigaction* sa )
{
   Int i;
   VG_(printf)("vg_ksigaction: handler %p, flags 0x%x, restorer %p\n", 
               sa->ksa_handler, (UInt)sa->sa_flags, sa->sa_restorer);
   VG_(printf)("vg_ksigaction: { ");
   for (i = 1; i <= VG_(max_signal); i++)
      if (VG_(sigismember(&(sa->sa_mask),i)))
         VG_(printf)("%d ", i);
   VG_(printf)("}\n");
}

/* 
   Force signal handler to default
 */
void VG_(set_default_handler)(Int signo)
{
   struct vki_sigaction sa;   

   sa.ksa_handler = VKI_SIG_DFL;
   sa.sa_flags = 0;
   sa.sa_restorer = 0;
   VG_(sigemptyset)(&sa.sa_mask);
      
   VG_(do_sys_sigaction)(signo, &sa, NULL);
}

/* 
   Poll for pending signals, and set the next one up for delivery.
 */
void VG_(poll_signals)(ThreadId tid)
{
   static const struct vki_timespec zero = { 0, 0 };
   vki_siginfo_t si, *sip;
   vki_sigset_t pollset;
   ThreadState *tst = VG_(get_ThreadState)(tid);
   Int i;
   vki_sigset_t saved_mask;

   /* look for all the signals this thread isn't blocking */
   for(i = 0; i < _VKI_NSIG_WORDS; i++)
      pollset.sig[i] = ~tst->sig_mask.sig[i];

   VG_(sigdelset)(&pollset, VKI_SIGVGCHLD); /* already dealt with */

   //VG_(printf)("tid %d pollset=%08x%08x\n", tid, pollset.sig[1], pollset.sig[0]);

   VG_(block_all_host_signals)(&saved_mask); // protect signal queue

   /* First look for any queued pending signals */
   sip = next_queued(tid, &pollset); /* this thread */

   if (sip == NULL)
      sip = next_queued(0, &pollset); /* process-wide */

   /* If there was nothing queued, ask the kernel for a pending signal */
   if (sip == NULL && VG_(sigtimedwait)(&pollset, &si, &zero) > 0) {
      if (VG_(clo_trace_signals))
	 VG_(message)(Vg_DebugMsg, "poll_signals: got signal %d for thread %d", si.si_signo, tid);
      sip = &si;
   }

   if (sip != NULL) {
      /* OK, something to do; deliver it */
      if (VG_(clo_trace_signals))
	 VG_(message)(Vg_DebugMsg, "Polling found signal %d for tid %d", 
		      sip->si_signo, tid);
      if (!VG_(is_sig_ign)(sip->si_signo))
	 VG_(deliver_signal)(tid, sip);
      else if (VG_(clo_trace_signals))
	 VG_(message)(Vg_DebugMsg, "   signal %d ignored", sip->si_signo);
	 
      sip->si_signo = 0;	/* remove from signal queue, if that's
				   where it came from */
   }

   VG_(restore_all_host_signals)(&saved_mask);
}

/* Set the standard set of blocked signals, used wheneever we're not
   running a client syscall. */
void VG_(block_signals)(ThreadId tid)
{
   vki_sigset_t mask;

   VG_(sigfillset)(&mask);

   /* Don't block these because they're synchronous */
   VG_(sigdelset)(&mask, VKI_SIGSEGV);
   VG_(sigdelset)(&mask, VKI_SIGBUS);
   VG_(sigdelset)(&mask, VKI_SIGFPE);
   VG_(sigdelset)(&mask, VKI_SIGILL);
   VG_(sigdelset)(&mask, VKI_SIGTRAP);

   /* Can't block these anyway */
   VG_(sigdelset)(&mask, VKI_SIGSTOP);
   VG_(sigdelset)(&mask, VKI_SIGKILL);

   /* Master doesn't block this */
   if (tid == VG_(master_tid))
      VG_(sigdelset)(&mask, VKI_SIGVGCHLD);

   VG_(sigprocmask)(VKI_SIG_SETMASK, &mask, NULL);
}

/* At startup, copy the process' real signal state to the SCSS.
   Whilst doing this, block all real signals.  Then calculate SKSS and
   set the kernel to that.  Also initialise DCSS. 
*/
void VG_(sigstartup_actions) ( void )
{
   Int i, ret;
   vki_sigset_t saved_procmask;
   struct vki_sigaction sa;

   /* VG_(printf)("SIGSTARTUP\n"); */
   /* Block all signals.  saved_procmask remembers the previous mask,
      which the first thread inherits.
   */
   VG_(block_all_host_signals)( &saved_procmask );

   /* Copy per-signal settings to SCSS. */
   for (i = 1; i <= _VKI_NSIG; i++) {
      /* Get the old host action */
      ret = VG_(sigaction)(i, NULL, &sa);

      if (ret != 0)
	 break;

      /* Try setting it back to see if this signal is really
	 available */
      if (i >= VKI_SIGRTMIN) {
	 struct vki_sigaction tsa;

	 tsa.ksa_handler = (void *)vg_sync_signalhandler;
	 tsa.sa_flags = VKI_SA_SIGINFO;
	 tsa.sa_restorer = 0;
	 VG_(sigfillset)(&tsa.sa_mask);

	 /* try setting it to some arbitrary handler */
	 if (VG_(sigaction)(i, &tsa, NULL) != 0) {
	    /* failed - not really usable */
	    break;
	 }

	 ret = VG_(sigaction)(i, &sa, NULL);
	 vg_assert(ret == 0);
      }

      VG_(max_signal) = i;

      if (VG_(clo_trace_signals) && VG_(clo_verbosity) > 2)
         VG_(printf)("snaffling handler 0x%x for signal %d\n", 
                     (Addr)(sa.ksa_handler), i );

      vg_scss.scss_per_sig[i].scss_handler  = sa.ksa_handler;
      vg_scss.scss_per_sig[i].scss_flags    = sa.sa_flags;
      vg_scss.scss_per_sig[i].scss_mask     = sa.sa_mask;
      vg_scss.scss_per_sig[i].scss_restorer = sa.sa_restorer;
   }

   if (VG_(clo_trace_signals))
      VG_(message)(Vg_DebugMsg, "Max kernel-supported signal is %d", VG_(max_signal));

   /* Our private internal signals are treated as ignored */
   vg_scss.scss_per_sig[VKI_SIGVGCHLD].scss_handler = VKI_SIG_IGN;
   vg_scss.scss_per_sig[VKI_SIGVGCHLD].scss_flags   = VKI_SA_SIGINFO;
   VG_(sigfillset)(&vg_scss.scss_per_sig[VKI_SIGVGCHLD].scss_mask);

   vg_scss.scss_per_sig[VKI_SIGVGKILL].scss_handler = VKI_SIG_IGN;
   vg_scss.scss_per_sig[VKI_SIGVGKILL].scss_flags   = VKI_SA_SIGINFO;
   VG_(sigfillset)(&vg_scss.scss_per_sig[VKI_SIGVGKILL].scss_mask);

   /* Copy the process' signal mask into the root thread. */
   vg_assert(VG_(threads)[VG_(master_tid)].status == VgTs_Init);
   VG_(threads)[VG_(master_tid)].sig_mask = saved_procmask;
   VG_(threads)[VG_(master_tid)].tmp_sig_mask = saved_procmask;

   /* Calculate SKSS and apply it.  This also sets the initial kernel
      mask we need to run with. */
   handle_SCSS_change( True /* forced update */ );

   /* Leave with all signals still blocked; the thread scheduler loop
      will set the appropriate mask at the appropriate time. */
}

/*--------------------------------------------------------------------*/
/*--- end                                             vg_signals.c ---*/
/*--------------------------------------------------------------------*/
