
/*--------------------------------------------------------------------*/
/*--- Implementation of POSIX signals.                             ---*/
/*---                                                 vg_signals.c ---*/
/*--------------------------------------------------------------------*/

/*
   This file is part of Valgrind, an extensible x86 protected-mode
   emulator for monitoring program execution on x86-Unixes.

   Copyright (C) 2000-2003 Julian Seward 
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


#include "vg_include.h"
#include "vg_unsafe.h"

/* Sidestep the normal check which disallows using valgrind.h
   directly. */
#define __VALGRIND_SOMESKIN_H
#include "valgrind.h"  /* for VALGRIND_MAGIC_SEQUENCE */

/* Define to give more sanity checking for signals. */
#define DEBUG_SIGNALS


/* KNOWN BUGS 24 May 02:

   - A signal is not masked in its own handler.  Neither are the
     signals in the signal's blocking mask.

   - There is only one pending set for the entire process, whereas
     POSIX seems to require each thread have its own pending set.
     This means that a signal can only be pending for one thread at
     a time.

   - The following causes an infinite loop: start Hugs, Feb 2001 
     version, and do Control-C at the prompt.  There is an infinite
     series of sigints delivered (to the client); but also seemingly
     to valgrind, which is very strange.  I don't know why.

   Probably a lot more bugs which I haven't discovered yet.
*/


/* ---------------------------------------------------------------------
   Forwards decls.
   ------------------------------------------------------------------ */

static void vg_oursignalhandler ( Int sigNo );


/* ---------------------------------------------------------------------
   HIGH LEVEL STUFF TO DO WITH SIGNALS: POLICY (MOSTLY)
   ------------------------------------------------------------------ */

/* ---------------------------------------------------------------------
   Signal state for this process.
   ------------------------------------------------------------------ */


/* Base-ment of these arrays[VKI_KNSIG].

   Valid signal numbers are 1 .. VKI_KNSIG inclusive.
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
      vki_ksigset_t scss_mask;
      void* scss_restorer; /* god knows; we ignore it. */
   }
   SCSS_Per_Signal;

typedef 
   struct {
      /* per-signal info */
      SCSS_Per_Signal scss_per_sig[1+VKI_KNSIG];

      /* Signal delivery stack, if any. */
      vki_kstack_t altstack;

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
     SA_NOCLDSTOP -- passed to kernel
     SA_ONESHOT or SA_RESETHAND -- required; abort if not set
     SA_RESTART -- we observe this but set our handlers always to restart
     SA_NOMASK or SA_NODEFER -- required to not be set; abort if set
     SA_ONSTACK -- currently not supported; abort if set.
*/


typedef 
   struct {
      void* skss_handler;  /* VKI_SIG_DFL or VKI_SIG_IGN 
                              or ptr to our handler */
      UInt skss_flags;
      /* There is no skss_mask, since we know that we will always ask
         for all signals to be blocked in our one-and-only
         sighandler. */
      /* Also there is no skss_restorer. */
   }
   SKSS_Per_Signal;

typedef 
   struct {
      SKSS_Per_Signal skss_per_sig[1+VKI_KNSIG];
      vki_ksigset_t skss_sigmask; /* process' blocked signal mask */   
   } 
   SKSS;

static SKSS vg_skss;


/* -----------------------------------------------------
   Dynamic client signal state (DCSS).  This holds transient
   information about state of client signals.
   -------------------------------------------------- */

typedef 
   struct {
      /* True iff a signal has been received but not yet passed to
         client. */
      Bool dcss_sigpending[1+VKI_KNSIG];
      /* If sigpending[] is True, has meaning: 
         VG_INVALID_THREADID -- to be passed to any suitable thread 
         other -- to be passed only to the specified thread. */
      ThreadId dcss_destthread[1+VKI_KNSIG];
   } 
   DCSS;

static DCSS vg_dcss;


/* ---------------------------------------------------------------------
   Compute the SKSS required by the current SCSS.
   ------------------------------------------------------------------ */

static 
void pp_SKSS ( void )
{
   Int sig;
   VG_(printf)("\n\nSKSS:\n");
   for (sig = 1; sig <= VKI_KNSIG; sig++) {
      VG_(printf)("sig %d:  handler 0x%x,  flags 0x%x\n", sig,
                  vg_skss.skss_per_sig[sig].skss_handler,
                  vg_skss.skss_per_sig[sig].skss_flags );

   }
   VG_(printf)("Global sigmask (63 .. 0) = 0x%x 0x%x\n",
	       vg_skss.skss_sigmask.ws[1],
	       vg_skss.skss_sigmask.ws[0] );
}

static __inline__
Bool is_WaitSIGd_by_any_thread ( Int sig )
{
   ThreadId tid;
   for (tid = 1; tid < VG_N_THREADS; tid++) {
      if (VG_(threads)[tid].status != VgTs_WaitSIG) 
         continue;
      if (VG_(ksigismember)( &VG_(threads)[tid].sigs_waited_for, sig ))
         return True;
   }
   return False;
}

static __inline__
Bool is_blocked_by_all_threads ( Int sig )
{
   ThreadId tid;
   for (tid = 1; tid < VG_N_THREADS; tid++) {
      if (VG_(threads)[tid].status == VgTs_Empty) 
         continue;
      if (! VG_(ksigismember)( &VG_(threads)[tid].sig_mask, sig ))
         return False;
   }
   return True;
}


/* This is the core, clever bit.  Computation is as follows:

   For each signal
      handler = if client has a handler, then our handler
                else if is WaitSIG'd by any thread, then our handler
                else if client is DFL, then DFL
                else (client must be IGN) IGN

      blocked = if is blocked by all threads and not WaitSIG'd by
                   any thread
                then BLOCKED 
                else UNBLOCKED
*/
static
void calculate_SKSS_from_SCSS ( SKSS* dst )
{
   Int   sig;
   void* skss_handler;
   void* scss_handler;
   Bool  iz_WaitSIGd_by_any_thread;
   Bool  iz_blocked_by_all_threads;
   Bool  skss_blocked;
   UInt  scss_flags;
   UInt  skss_flags;

   VG_(ksigemptyset)( &dst->skss_sigmask );

   for (sig = 1; sig <= VKI_KNSIG; sig++) {

      /* Calculate kernel handler and blockedness for sig, as per rules
         in above comment. */

      iz_WaitSIGd_by_any_thread = is_WaitSIGd_by_any_thread(sig);
      iz_blocked_by_all_threads = is_blocked_by_all_threads(sig);
  
      scss_handler = vg_scss.scss_per_sig[sig].scss_handler;
      scss_flags   = vg_scss.scss_per_sig[sig].scss_flags;

      /* Restorer */
      /* 
      Doesn't seem like we can spin this one.
      if (vg_scss.scss_per_sig[sig].scss_restorer != NULL)
         VG_(unimplemented)
            ("sigactions with non-NULL .sa_restorer field");
      */

      /* Handler */

      if (scss_handler != VKI_SIG_DFL && scss_handler != VKI_SIG_IGN) {
         skss_handler = &vg_oursignalhandler;
      } else
      if (iz_WaitSIGd_by_any_thread) {
         skss_handler = &vg_oursignalhandler;
      } else
      if (scss_handler == VKI_SIG_DFL) {
         skss_handler = VKI_SIG_DFL;
      }
      else {
         vg_assert(scss_handler == VKI_SIG_IGN);
         skss_handler = VKI_SIG_IGN;
      }

      /* Blockfulness */

      skss_blocked
         = iz_blocked_by_all_threads && !iz_WaitSIGd_by_any_thread;

      /* Flags */

      skss_flags = 0;
      /* SA_NOCLDSTOP: pass to kernel */
      if (scss_flags & VKI_SA_NOCLDSTOP)
         skss_flags |= VKI_SA_NOCLDSTOP;
      /* SA_ONESHOT: ignore client setting */
      /*
      if (!(scss_flags & VKI_SA_ONESHOT))
         VG_(unimplemented)
            ("sigactions without SA_ONESHOT");
      vg_assert(scss_flags & VKI_SA_ONESHOT);
      skss_flags |= VKI_SA_ONESHOT;
      */
      /* SA_RESTART: ignore client setting and set for us */
      skss_flags |= VKI_SA_RESTART;
      /* SA_NOMASK: not allowed */
      /*
      .. well, ignore it. 
      if (scss_flags & VKI_SA_NOMASK)
         VG_(unimplemented)
            ("sigactions with SA_NOMASK");
      vg_assert(!(scss_flags & VKI_SA_NOMASK));
      */
      /* SA_ONSTACK: client setting is irrelevant here */
      /*
      if (scss_flags & VKI_SA_ONSTACK)
         VG_(unimplemented)
            ("signals on an alternative stack (SA_ONSTACK)");
      vg_assert(!(scss_flags & VKI_SA_ONSTACK));
      */
      /* ... but WE ask for on-stack ourselves ... */
      skss_flags |= VKI_SA_ONSTACK;

      /* Create SKSS entry for this signal. */

      if (skss_blocked
          && sig != VKI_SIGKILL && sig != VKI_SIGSTOP)
         VG_(ksigaddset)( &dst->skss_sigmask, sig );

      if (sig != VKI_SIGKILL && sig != VKI_SIGSTOP)
         dst->skss_per_sig[sig].skss_handler = skss_handler;
      else
         dst->skss_per_sig[sig].skss_handler = VKI_SIG_DFL;

      dst->skss_per_sig[sig].skss_flags   = skss_flags;
   }

   /* Sanity checks. */
   vg_assert(dst->skss_per_sig[VKI_SIGKILL].skss_handler 
             == VKI_SIG_DFL);
   vg_assert(dst->skss_per_sig[VKI_SIGSTOP].skss_handler 
             == VKI_SIG_DFL);
   vg_assert(!VG_(ksigismember)( &dst->skss_sigmask, VKI_SIGKILL ));
   vg_assert(!VG_(ksigismember)( &dst->skss_sigmask, VKI_SIGSTOP ));

   if (0)
      pp_SKSS();
}


/* ---------------------------------------------------------------------
   After a possible SCSS change, update SKSS and the kernel itself.
   ------------------------------------------------------------------ */

/* IMPORTANT NOTE: to avoid race conditions, we must always enter here
   with ALL KERNEL SIGNALS BLOCKED ! 
*/
void VG_(handle_SCSS_change) ( Bool force_update )
{
   Int            res, sig;
   SKSS           skss_old;
   vki_ksigaction ksa, ksa_old;

#  ifdef DEBUG_SIGNALS
   vki_ksigset_t  test_sigmask;
   res = VG_(ksigprocmask)( VKI_SIG_SETMASK /*irrelevant*/, 
                            NULL, &test_sigmask );
   vg_assert(res == 0);
   /* The kernel never says that SIGKILL or SIGSTOP are masked. It is
      correct! So we fake it here for the purposes only of
      assertion. */
   VG_(ksigaddset)( &test_sigmask, VKI_SIGKILL );
   VG_(ksigaddset)( &test_sigmask, VKI_SIGSTOP );
   vg_assert(VG_(kisfullsigset)( &test_sigmask ));
#  endif

   /* Remember old SKSS and calculate new one. */
   skss_old = vg_skss;
   calculate_SKSS_from_SCSS ( &vg_skss );

   /* Compare the new SKSS entries vs the old ones, and update kernel
      where they differ. */
   for (sig = 1; sig <= VKI_KNSIG; sig++) {

      /* Trying to do anything with SIGKILL is pointless; just ignore
         it. */
      if (sig == VKI_SIGKILL || sig == VKI_SIGSTOP)
         continue;

      /* Aside: take the opportunity to clean up DCSS: forget about any
         pending signals directed at dead threads. */
      if (vg_dcss.dcss_sigpending[sig] 
          && vg_dcss.dcss_destthread[sig] != VG_INVALID_THREADID) {
         ThreadId tid = vg_dcss.dcss_destthread[sig];
         vg_assert(VG_(is_valid_or_empty_tid)(tid));
         if (VG_(threads)[tid].status == VgTs_Empty) {
            vg_dcss.dcss_sigpending[sig] = False;
            vg_dcss.dcss_destthread[sig] = VG_INVALID_THREADID;
            if (VG_(clo_trace_signals)) 
               VG_(message)(Vg_DebugMsg, 
                   "discarding pending signal %d due to thread %d exiting",
                   sig, tid );
         }
      }

      /* End of the Aside.  Now the Main Business. */

      if (!force_update) {
         if ((skss_old.skss_per_sig[sig].skss_handler
              == vg_skss.skss_per_sig[sig].skss_handler)
             && (skss_old.skss_per_sig[sig].skss_flags
                 == vg_skss.skss_per_sig[sig].skss_flags))
            /* no difference */
            continue;
      }

      ksa.ksa_handler = vg_skss.skss_per_sig[sig].skss_handler;
      ksa.ksa_flags   = vg_skss.skss_per_sig[sig].skss_flags;
      vg_assert(ksa.ksa_flags & VKI_SA_ONSTACK);
      VG_(ksigfillset)( &ksa.ksa_mask );
      VG_(ksigdelset)( &ksa.ksa_mask, VKI_SIGKILL );
      VG_(ksigdelset)( &ksa.ksa_mask, VKI_SIGSTOP );
      ksa.ksa_restorer = NULL;

      if (VG_(clo_trace_signals)) 
         VG_(message)(Vg_DebugMsg, 
            "setting ksig %d to: hdlr 0x%x, flags 0x%x, "
            "mask(63..0) 0x%x 0x%x",
            sig, ksa.ksa_handler,
            ksa.ksa_flags,
            ksa.ksa_mask.ws[1], 
            ksa.ksa_mask.ws[0] 
         );

      res = VG_(ksigaction)( sig, &ksa, &ksa_old );
      vg_assert(res == 0);

      /* Since we got the old sigaction more or less for free, might
         as well extract the maximum sanity-check value from it. */
      if (!force_update) {
         vg_assert(ksa_old.ksa_handler 
                   == skss_old.skss_per_sig[sig].skss_handler);
         vg_assert(ksa_old.ksa_flags 
                   == skss_old.skss_per_sig[sig].skss_flags);
         vg_assert(ksa_old.ksa_restorer 
                   == NULL);
         VG_(ksigaddset)( &ksa_old.ksa_mask, VKI_SIGKILL );
         VG_(ksigaddset)( &ksa_old.ksa_mask, VKI_SIGSTOP );
         vg_assert(VG_(kisfullsigset)( &ksa_old.ksa_mask ));
      }
   }

   /* Just set the new sigmask, even if it's no different from the
      old, since we have to do this anyway, to unblock the host
      signals. */
   if (VG_(clo_trace_signals)) 
      VG_(message)(Vg_DebugMsg, 
         "setting kmask(63..0) to 0x%x 0x%x",
         vg_skss.skss_sigmask.ws[1], 
         vg_skss.skss_sigmask.ws[0] 
      );

   VG_(restore_all_host_signals)( &vg_skss.skss_sigmask );
}


/* ---------------------------------------------------------------------
   Update/query SCSS in accordance with client requests.
   ------------------------------------------------------------------ */

/* Logic for this alt-stack stuff copied directly from do_sigaltstack
   in kernel/signal.[ch] */

/* True if we are on the alternate signal stack.  */
static Int on_sig_stack ( Addr m_esp )
{
   return (m_esp - (Addr)vg_scss.altstack.ss_sp 
           < vg_scss.altstack.ss_size);
}

static Int sas_ss_flags ( Addr m_esp )
{
   return (vg_scss.altstack.ss_size == 0 
              ? VKI_SS_DISABLE
              : on_sig_stack(m_esp) ? VKI_SS_ONSTACK : 0);
}


void VG_(do__NR_sigaltstack) ( ThreadId tid )
{
   vki_kstack_t* ss;
   vki_kstack_t* oss;
   Addr          m_esp;

   vg_assert(VG_(is_valid_tid)(tid));
   ss    = (vki_kstack_t*)(VG_(threads)[tid].m_ebx);
   oss   = (vki_kstack_t*)(VG_(threads)[tid].m_ecx);
   m_esp = VG_(threads)[tid].m_esp;

   if (VG_(clo_trace_signals))
      VG_(message)(Vg_DebugExtraMsg, 
         "__NR_sigaltstack: tid %d, "
         "ss 0x%x, oss 0x%x (current %%esp %p)",
         tid, (UInt)ss, (UInt)oss, (UInt)m_esp );

   if (oss != NULL) {
      oss->ss_sp    = vg_scss.altstack.ss_sp;
      oss->ss_size  = vg_scss.altstack.ss_size;
      oss->ss_flags = sas_ss_flags(m_esp);
   }

   if (ss != NULL) {
      if (on_sig_stack(VG_(threads)[tid].m_esp)) {
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
         vg_scss.altstack.ss_size = 0;
         vg_scss.altstack.ss_sp = NULL;
      } else {
         if (ss->ss_size < VKI_MINSIGSTKSZ) {
            SET_SYSCALL_RETVAL(tid, -VKI_ENOMEM);
            return;
         }
      }
      vg_scss.altstack.ss_sp   = ss->ss_sp;
      vg_scss.altstack.ss_size = ss->ss_size;
   }
   SET_SYSCALL_RETVAL(tid, 0);
}


void VG_(do__NR_sigaction) ( ThreadId tid )
{
   Int              signo;
   vki_ksigaction*  new_act;
   vki_ksigaction*  old_act;
   vki_ksigset_t    irrelevant_sigmask;

   vg_assert(VG_(is_valid_tid)(tid));
   signo     = VG_(threads)[tid].m_ebx; /* int sigNo */
   new_act   = (vki_ksigaction*)(VG_(threads)[tid].m_ecx);
   old_act   = (vki_ksigaction*)(VG_(threads)[tid].m_edx);

   if (VG_(clo_trace_signals))
      VG_(message)(Vg_DebugExtraMsg, 
         "__NR_sigaction: tid %d, sigNo %d, "
         "new 0x%x, old 0x%x, new flags 0x%x",
         tid, signo, (UInt)new_act, (UInt)old_act,
         (UInt)(new_act ? new_act->ksa_flags : 0) );

   /* Rule out various error conditions.  The aim is to ensure that if
      when the call is passed to the kernel it will definitely
      succeed. */

   /* Reject out-of-range signal numbers. */
   if (signo < 1 || signo > VKI_KNSIG) goto bad_signo;

   /* Reject attempts to set a handler (or set ignore) for SIGKILL. */
   if ( (signo == VKI_SIGKILL || signo == VKI_SIGSTOP)
       && new_act
       && new_act->ksa_handler != VKI_SIG_DFL)
      goto bad_sigkill_or_sigstop;

   /* If the client supplied non-NULL old_act, copy the relevant SCSS
      entry into it. */
   if (old_act) {
      old_act->ksa_handler  = vg_scss.scss_per_sig[signo].scss_handler;
      old_act->ksa_flags    = vg_scss.scss_per_sig[signo].scss_flags;
      old_act->ksa_mask     = vg_scss.scss_per_sig[signo].scss_mask;
      old_act->ksa_restorer = vg_scss.scss_per_sig[signo].scss_restorer;
   }

   /* And now copy new SCSS entry from new_act. */
   if (new_act) {
      vg_scss.scss_per_sig[signo].scss_handler  = new_act->ksa_handler;
      vg_scss.scss_per_sig[signo].scss_flags    = new_act->ksa_flags;
      vg_scss.scss_per_sig[signo].scss_mask     = new_act->ksa_mask;
      vg_scss.scss_per_sig[signo].scss_restorer = new_act->ksa_restorer;
   }

   /* All happy bunnies ... */
   if (new_act) {
      VG_(block_all_host_signals)( &irrelevant_sigmask );
      VG_(handle_SCSS_change)( False /* lazy update */ );
   }
   SET_SYSCALL_RETVAL(tid, 0);
   return;

  bad_signo:
   if (VG_(needs).core_errors)
      VG_(message)(Vg_UserMsg,
                   "Warning: bad signal number %d in __NR_sigaction.", 
                   signo);
   SET_SYSCALL_RETVAL(tid, -VKI_EINVAL);
   return;

  bad_sigkill_or_sigstop:
   if (VG_(needs).core_errors)
      VG_(message)(Vg_UserMsg,
         "Warning: attempt to set %s handler in __NR_sigaction.", 
         signo == VKI_SIGKILL ? "SIGKILL" : "SIGSTOP" );

   SET_SYSCALL_RETVAL(tid, -VKI_EINVAL);
   return;
}


static
void do_sigprocmask_bitops ( Int vki_how, 
			     vki_ksigset_t* orig_set,
			     vki_ksigset_t* modifier )
{
   switch (vki_how) {
      case VKI_SIG_BLOCK: 
         VG_(ksigaddset_from_set)( orig_set, modifier );
         break;
      case VKI_SIG_UNBLOCK:
         VG_(ksigdelset_from_set)( orig_set, modifier );
         break;
      case VKI_SIG_SETMASK:
         *orig_set = *modifier;
         break;
      default:
         VG_(core_panic)("do_sigprocmask_bitops");
	 break;
   }
}

/* Handle blocking mask set/get uniformly for threads and process as a
   whole.  If tid==VG_INVALID_THREADID, this is really
   __NR_sigprocmask, in which case we set the masks for all threads to
   the "set" and return in "oldset" that from the root thread (1).
   Otherwise, tid will denote a valid thread, in which case we just
   set/get its mask.

   Note that the thread signal masks are an implicit part of SCSS,
   which is why this routine is allowed to mess with them.  
*/
static
void do_setmask ( ThreadId tid,
                  Int how,
                  vki_ksigset_t* newset,
		  vki_ksigset_t* oldset )
{
   vki_ksigset_t irrelevant_sigmask;

   if (VG_(clo_trace_signals))
      VG_(message)(Vg_DebugExtraMsg, 
         "do_setmask: tid = %d (%d means ALL), how = %d (%s), set = %p", 
         tid,
         VG_INVALID_THREADID,
         how,
         how==VKI_SIG_BLOCK ? "SIG_BLOCK" : (
            how==VKI_SIG_UNBLOCK ? "SIG_UNBLOCK" : (
            how==VKI_SIG_SETMASK ? "SIG_SETMASK" : "???")),
         newset
      );

   if (tid == VG_INVALID_THREADID) {
      /* Behave as if __NR_sigprocmask. */
      if (oldset) {
         /* A bit fragile.  Should do better here really. */
         vg_assert(VG_(threads)[1].status != VgTs_Empty);
         *oldset = VG_(threads)[1].sig_mask;
      }
      if (newset) {
        ThreadId tidd;
        for (tidd = 1; tidd < VG_N_THREADS; tidd++) {
            if (VG_(threads)[tidd].status == VgTs_Empty) 
               continue;
            do_sigprocmask_bitops ( 
               how, &VG_(threads)[tidd].sig_mask, newset );
         }
      }
   } else {
      /* Just do this thread. */
      vg_assert(VG_(is_valid_tid)(tid));
      if (oldset)
         *oldset = VG_(threads)[tid].sig_mask;
      if (newset)
         do_sigprocmask_bitops ( 
            how, &VG_(threads)[tid].sig_mask, newset );
   }

   if (newset) {
      VG_(block_all_host_signals)( &irrelevant_sigmask );
      VG_(handle_SCSS_change)( False /* lazy update */ );
   }
}


void VG_(do__NR_sigprocmask) ( ThreadId tid,
                               Int how, 
                               vki_ksigset_t* set,
                               vki_ksigset_t* oldset )
{
   if (how == VKI_SIG_BLOCK || how == VKI_SIG_UNBLOCK 
                            || how == VKI_SIG_SETMASK) {
      vg_assert(VG_(is_valid_tid)(tid));
      do_setmask ( VG_INVALID_THREADID, how, set, oldset );
      /* Syscall returns 0 (success) to its thread. */
      SET_SYSCALL_RETVAL(tid, 0);
   } else {
      VG_(message)(Vg_DebugMsg, 
                  "sigprocmask: unknown `how' field %d", how);
      SET_SYSCALL_RETVAL(tid, -VKI_EINVAL);
   }
}


void VG_(do_pthread_sigmask_SCSS_upd) ( ThreadId tid,
                                        Int how, 
                                        vki_ksigset_t* set,
                                        vki_ksigset_t* oldset )
{
   /* Assume that how has been validated by caller. */
   vg_assert(how == VKI_SIG_BLOCK || how == VKI_SIG_UNBLOCK 
                                  || how == VKI_SIG_SETMASK);
   vg_assert(VG_(is_valid_tid)(tid));
   do_setmask ( tid, how, set, oldset );
   /* The request return code is set in do_pthread_sigmask */
}


void VG_(send_signal_to_thread) ( ThreadId thread, Int sig )
{
   Int res;
   vg_assert(VG_(is_valid_tid)(thread));
   vg_assert(sig >= 1 && sig <= VKI_KNSIG);
   
   switch ((UInt)(vg_scss.scss_per_sig[sig].scss_handler)) {

      case ((UInt)VKI_SIG_IGN):
         if (VG_(clo_trace_signals)) 
            VG_(message)(Vg_DebugMsg, 
               "send_signal %d to_thread %d: IGN, ignored", sig, thread );
         break;

      case ((UInt)VKI_SIG_DFL):
         /* This is the tricky case.  Since we don't handle default
            actions, the simple thing is to send someone round to the
            front door and signal there.  Then the kernel will do
            whatever it does with the default action. */
         res = VG_(kkill)( VG_(getpid)(), sig );
         vg_assert(res == 0);
         break;

      default:
         if (!vg_dcss.dcss_sigpending[sig]) {
            vg_dcss.dcss_sigpending[sig] = True;
            vg_dcss.dcss_destthread[sig] = thread;
            if (VG_(clo_trace_signals)) 
               VG_(message)(Vg_DebugMsg, 
                  "send_signal %d to_thread %d: now pending", sig, thread );
         } else {
            if (vg_dcss.dcss_destthread[sig] == thread) {
               if (VG_(clo_trace_signals)) 
                  VG_(message)(Vg_DebugMsg, 
                     "send_signal %d to_thread %d: already pending ... "
                     "discarded", sig, thread );
            } else {
               if (VG_(clo_trace_signals)) 
                  VG_(message)(Vg_DebugMsg, 
                     "send_signal %d to_thread %d: was pending for %d, "
                     "now pending for %d",
                     sig, thread, vg_dcss.dcss_destthread[sig], thread );
               vg_dcss.dcss_destthread[sig] = thread;
            }
         }
   }    
}


/* Store in set the signals which could be delivered to this thread
   right now (since they are pending) but cannot be, because the
   thread has masked them out. */
void VG_(do_sigpending) ( ThreadId tid, vki_ksigset_t* set )
{
   Int           sig, res;
   Bool          maybe_pend;
   vki_ksigset_t process_pending;

   /* Get the set of signals which are pending for the process as a
      whole. */
   res = VG_(ksigpending)( &process_pending );
   vg_assert(res == 0);

   VG_(ksigemptyset)(set);
   for (sig = 1; sig <= VKI_KNSIG; sig++) {

      /* Figure out if the signal could be pending for this thread.
         There are two cases. */
      maybe_pend = False;

      /* Case 1: perhaps the signal is pending for the process as a
         whole -- that is, is blocked even valgrind's signal
         handler. */
      if (VG_(ksigismember)( &process_pending, sig ))
         maybe_pend = True;

      /* Case 2: the signal has been collected by our handler and is
         now awaiting disposition inside valgrind. */
      if (/* is it pending at all? */
          vg_dcss.dcss_sigpending[sig]
          && 
	  /* check it is not specifically directed to some other thread */
          (vg_dcss.dcss_destthread[sig] == VG_INVALID_THREADID
           || vg_dcss.dcss_destthread[sig] == tid)
         )
         maybe_pend = True;

      if (!maybe_pend)
         continue; /* this signal just ain't pending! */

      /* Check other necessary conditions now ... */

      if (VG_(ksigismember)( &VG_(threads)[tid].sigs_waited_for, sig ))
         continue; /* tid is sigwaiting for sig, so will never be
                      offered to a handler */
      if (! VG_(ksigismember)( &VG_(threads)[tid].sig_mask, sig ))
         continue; /* not blocked in this thread */

      /* Ok, sig could be delivered to this thread if only it wasn't
         masked out.  So we add it to set. */
      VG_(ksigaddset)( set, sig );
   }
}


/* ---------------------------------------------------------------------
   LOW LEVEL STUFF TO DO WITH SIGNALS: IMPLEMENTATION
   ------------------------------------------------------------------ */

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
void VG_(restore_all_host_signals) ( /* IN */ vki_ksigset_t* saved_mask )
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
      /* These 4 are parameters to the signal handler.  The order of
         them is important, since this whole struct is pushed onto the
         client's stack at delivery time.  The first 4 words -- which
         will be at the top of the stack -- constitute 4 arg words to
         the handler. */

      /* Sig handler's (bogus) return address */
      Addr retaddr;
      /* The arg to the sig handler.  We need to inspect this after
         the handler returns, but it's unreasonable to assume that the
         handler won't change it.  So we keep a second copy of it in
         sigNo_private. */
      Int  sigNo;
      /* ptr to siginfo_t; NULL for now. */
      Addr psigInfo;
      /* ptr to ucontext; NULL for now. */
      Addr puContext;

      /* The rest are private fields which the handler is unaware of. */

      /* Sanity check word. */
      UInt magicPI;
      /* Safely-saved version of sigNo, as described above. */
      Int  sigNo_private;
      /* Saved processor state. */
      UInt m_sse[VG_SIZE_OF_SSESTATE_W];

      UInt m_eax;
      UInt m_ecx;
      UInt m_edx;
      UInt m_ebx;
      UInt m_ebp;
      UInt m_esp;
      UInt m_esi;
      UInt m_edi;
      UInt m_eflags;
      Addr m_eip;

      UInt sh_eax;
      UInt sh_ebx;
      UInt sh_ecx;
      UInt sh_edx;
      UInt sh_esi;
      UInt sh_edi;
      UInt sh_ebp;
      UInt sh_esp;
      UInt sh_eflags;

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
   Addr         esp, esp_top_of_frame;
   VgSigFrame*  frame;
   ThreadState* tst;

   vg_assert(sigNo >= 1 && sigNo <= VKI_KNSIG);
   vg_assert(VG_(is_valid_tid)(tid));
   tst = & VG_(threads)[tid];

   if (/* this signal asked to run on an alt stack */
       (vg_scss.scss_per_sig[sigNo].scss_flags & VKI_SA_ONSTACK)
       && /* there is a defined and enabled alt stack, which we're not
             already using.  Logic from get_sigframe in
             arch/i386/kernel/signal.c. */
          sas_ss_flags(tst->m_esp) == 0
      ) {
      esp_top_of_frame 
         = (Addr)(vg_scss.altstack.ss_sp) + vg_scss.altstack.ss_size;
      if (VG_(clo_trace_signals))
         VG_(message)(Vg_DebugMsg,
            "delivering signal %d to thread %d: on ALT STACK", 
            sigNo, tid );

      /* Signal delivery to skins */
      VG_TRACK( pre_deliver_signal, tid, sigNo, /*alt_stack*/False );
      
   } else {
      esp_top_of_frame = tst->m_esp;

      /* Signal delivery to skins */
      VG_TRACK( pre_deliver_signal, tid, sigNo, /*alt_stack*/True );
   }

   esp = esp_top_of_frame;
   esp -= sizeof(VgSigFrame);
   frame = (VgSigFrame*)esp;

   /* For tracking memory events, indicate the entire frame has been
    * allocated, but pretend that only the first four words are written */
   VG_TRACK( new_mem_stack_signal, (Addr)frame, sizeof(VgSigFrame) );

   /* Assert that the frame is placed correctly. */
   vg_assert( (sizeof(VgSigFrame) & 0x3) == 0 );
   vg_assert( ((Char*)(&frame->magicE)) + sizeof(UInt) 
              == ((Char*)(esp_top_of_frame)) );

   /* retaddr, sigNo, psigInfo, puContext fields are to be written */
   VG_TRACK( pre_mem_write, Vg_CoreSignal, tst, "signal handler frame", 
                            (Addr)esp, 16 );
   frame->retaddr    = (UInt)(&VG_(signalreturn_bogusRA));
   frame->sigNo      = sigNo;
   frame->sigNo_private = sigNo;
   frame->psigInfo   = (Addr)NULL;
   frame->puContext  = (Addr)NULL;
   frame->magicPI    = 0x31415927;

   for (i = 0; i < VG_SIZE_OF_SSESTATE_W; i++)
      frame->m_sse[i] = tst->m_sse[i];

   frame->m_eax      = tst->m_eax;
   frame->m_ecx      = tst->m_ecx;
   frame->m_edx      = tst->m_edx;
   frame->m_ebx      = tst->m_ebx;
   frame->m_ebp      = tst->m_ebp;
   frame->m_esp      = tst->m_esp;
   frame->m_esi      = tst->m_esi;
   frame->m_edi      = tst->m_edi;
   frame->m_eflags   = tst->m_eflags;
   frame->m_eip      = tst->m_eip;

   if (VG_(needs).shadow_regs) {
      frame->sh_eax     = tst->sh_eax;
      frame->sh_ecx     = tst->sh_ecx;
      frame->sh_edx     = tst->sh_edx;
      frame->sh_ebx     = tst->sh_ebx;
      frame->sh_ebp     = tst->sh_ebp;
      frame->sh_esp     = tst->sh_esp;
      frame->sh_esi     = tst->sh_esi;
      frame->sh_edi     = tst->sh_edi;
      frame->sh_eflags  = tst->sh_eflags;
   }

   frame->status     = tst->status;

   frame->magicE     = 0x27182818;

   /* Ensure 'tid' and 'tst' correspond */
   vg_assert(& VG_(threads)[tid] == tst);
   /* Set the thread so it will next run the handler. */
   /* tst->m_esp  = esp; */
   SET_SIGNAL_ESP(tid, esp);

   tst->m_eip  = (Addr)vg_scss.scss_per_sig[sigNo].scss_handler;
   /* This thread needs to be marked runnable, but we leave that the
      caller to do. */

   /* retaddr, sigNo, psigInfo, puContext fields have been written -- 
      at 0(%ESP) .. 12(%ESP) */
   VG_TRACK( post_mem_write, (Addr)esp, 16 );

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

   vg_assert(VG_(is_valid_tid)(tid));
   tst = & VG_(threads)[tid];

   /* Correctly reestablish the frame base address. */
   esp   = tst->m_esp;
   frame = (VgSigFrame*)
              (esp -4 /* because the handler's RET pops the RA */
                  +20 /* because signalreturn_bogusRA pushes 5 words */);

   vg_assert(frame->magicPI == 0x31415927);
   vg_assert(frame->magicE  == 0x27182818);
   if (VG_(clo_trace_signals))
      VG_(message)(Vg_DebugMsg, 
         "vg_pop_signal_frame (thread %d): valid magic", tid);

   /* Mark the frame structure as nonaccessible. */
   VG_TRACK( die_mem_stack_signal, (Addr)frame, sizeof(VgSigFrame) );

   /* restore machine state */
   for (i = 0; i < VG_SIZE_OF_SSESTATE_W; i++)
      tst->m_sse[i] = frame->m_sse[i];

   tst->m_eax     = frame->m_eax;
   tst->m_ecx     = frame->m_ecx;
   tst->m_edx     = frame->m_edx;
   tst->m_ebx     = frame->m_ebx;
   tst->m_ebp     = frame->m_ebp; 
   tst->m_esp     = frame->m_esp;
   tst->m_esi     = frame->m_esi;
   tst->m_edi     = frame->m_edi;
   tst->m_eflags  = frame->m_eflags;
   tst->m_eip     = frame->m_eip;

   if (VG_(needs).shadow_regs) {
      tst->sh_eax     = frame->sh_eax;
      tst->sh_ecx     = frame->sh_ecx;
      tst->sh_edx     = frame->sh_edx;
      tst->sh_ebx     = frame->sh_ebx;
      tst->sh_ebp     = frame->sh_ebp; 
      tst->sh_esp     = frame->sh_esp;
      tst->sh_esi     = frame->sh_esi;
      tst->sh_edi     = frame->sh_edi;
      tst->sh_eflags  = frame->sh_eflags;
   }
   
   /* don't use the copy exposed to the handler; it might have changed
      it. */
   sigNo          = frame->sigNo_private; 

   /* And restore the thread's status to what it was before the signal
      was delivered. */
   tst->status    = frame->status;

   /* Notify skins */
   VG_TRACK( post_deliver_signal, tid, sigNo );

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

   vg_assert(sigNo >= 1 && sigNo <= VKI_KNSIG);

   /* Unlock and return. */
   VG_(restore_all_host_signals)( &saved_procmask );

   /* Scheduler now can resume this thread, or perhaps some other.
      Tell the scheduler whether or not any syscall interrupted by
      this signal should be restarted, if possible, or no. */
   return 
      (vg_scss.scss_per_sig[sigNo].scss_flags & VKI_SA_RESTART)
         ? True 
         : False;
}


/* Deliver all pending signals, by building stack frames for their
   handlers.  Return True if any signals were delivered. */
Bool VG_(deliver_signals) ( void )
{
   vki_ksigset_t  saved_procmask;
   Int            sigNo;
   Bool           found, scss_changed;
   ThreadState*   tst;
   ThreadId       tid;

   /* A cheap check.  We don't need to have exclusive access to the
      pending array, because in the worst case, vg_oursignalhandler
      will add signals, causing us to return, thinking there are no
      signals to deliver, when in fact there are some.  A subsequent
      call here will handle the signal(s) we missed.  */
   found = False;
   for (sigNo = 1; sigNo <= VKI_KNSIG; sigNo++)
      if (vg_dcss.dcss_sigpending[sigNo])
         found = True;

   if (!found) return False;

   /* Now we have to do it properly.  Get exclusive access by
      blocking all the host's signals.  That means vg_oursignalhandler
      can't run whilst we are messing with stuff.
   */
   scss_changed = False;
   VG_(block_all_host_signals)( &saved_procmask );

   /* Look for signals to deliver ... */
   for (sigNo = 1; sigNo <= VKI_KNSIG; sigNo++) {

      if (!vg_dcss.dcss_sigpending[sigNo])
         continue;

      /* sigNo is pending.  Try to find a suitable thread to deliver
         it to. */
      /* First off, are any threads in sigwait() for the signal? 
         If so just give to one of them and have done. */
      for (tid = 1; tid < VG_N_THREADS; tid++) {
         tst = & VG_(threads)[tid];
         /* Is tid waiting for a signal?  If not, ignore. */
         if (tst->status != VgTs_WaitSIG)
            continue;
         /* Is the signal directed at a specific thread other than
            this one?  If yes, ignore. */
         if (vg_dcss.dcss_destthread[sigNo] != VG_INVALID_THREADID
             && vg_dcss.dcss_destthread[sigNo] != tid)
            continue;
         /* Is tid waiting for the signal?  If not, ignore. */
         if (VG_(ksigismember)(&(tst->sigs_waited_for), sigNo))
            break;
      }
      if (tid < VG_N_THREADS) {
         UInt* sigwait_args;
         tst = & VG_(threads)[tid];
         if (VG_(clo_trace_signals) || VG_(clo_trace_sched))
            VG_(message)(Vg_DebugMsg,
               "releasing thread %d from sigwait() due to signal %d",
               tid, sigNo );
         sigwait_args = (UInt*)(tst->m_eax);
         if (NULL != (UInt*)(sigwait_args[2])) {
            *(Int*)(sigwait_args[2]) = sigNo;
            VG_TRACK( post_mem_write, (Addr)sigwait_args[2], sizeof(UInt));
         }
	 SET_SIGNAL_EDX(tid, 0);
         tst->status = VgTs_Runnable;
         VG_(ksigemptyset)(&tst->sigs_waited_for);
         scss_changed = True;
         vg_dcss.dcss_sigpending[sigNo] = False;
         vg_dcss.dcss_destthread[sigNo] = VG_INVALID_THREADID; 
                                          /*paranoia*/
         continue; /* for (sigNo = 1; ...) loop */
      }

      /* Well, nobody appears to be sigwaiting for it.  So we really
         are delivering the signal in the usual way.  And that the
         client really has a handler for this thread! */
      vg_assert(vg_dcss.dcss_sigpending[sigNo]);

      /* A recent addition, so as to stop seriously wierd progs dying
         at the following assertion (which this renders redundant,
         btw). */
      if (vg_scss.scss_per_sig[sigNo].scss_handler == VKI_SIG_IGN
          || vg_scss.scss_per_sig[sigNo].scss_handler == VKI_SIG_DFL) {
         /* Strange; perhaps the handler disappeared before we could
            deliver the signal. */
         VG_(message)(Vg_DebugMsg,
            "discarding signal %d for thread %d because handler missing",
            sigNo, tid );
         vg_dcss.dcss_sigpending[sigNo] = False;
         vg_dcss.dcss_destthread[sigNo] = VG_INVALID_THREADID;
         continue; /* for (sigNo = 1; ...) loop */
      }

      vg_assert(vg_scss.scss_per_sig[sigNo].scss_handler != VKI_SIG_IGN
                && vg_scss.scss_per_sig[sigNo].scss_handler != VKI_SIG_DFL);

      tid = vg_dcss.dcss_destthread[sigNo];
      vg_assert(tid == VG_INVALID_THREADID 
                || VG_(is_valid_tid)(tid));

      if (tid != VG_INVALID_THREADID) {
         /* directed to a specific thread; ensure it actually still
            exists ... */
         tst = & VG_(threads)[tid];
         if (tst->status == VgTs_Empty) {
            /* dead, for whatever reason; ignore this signal */
            if (VG_(clo_trace_signals))
               VG_(message)(Vg_DebugMsg,
                  "discarding signal %d for nonexistent thread %d",
                  sigNo, tid );
            vg_dcss.dcss_sigpending[sigNo] = False;
            vg_dcss.dcss_destthread[sigNo] = VG_INVALID_THREADID;
            continue; /* for (sigNo = 1; ...) loop */
	 } else if (VG_(ksigismember)(&(tst->sig_mask), sigNo)) {
            /* signal blocked in specific thread, so we can't
               deliver it just now */
            continue; /* for (sigNo = 1; ...) loop */
         }
      } else {
         /* not directed to a specific thread, so search for a
            suitable candidate */
         for (tid = 1; tid < VG_N_THREADS; tid++) {
            tst = & VG_(threads)[tid];
            if (tst->status != VgTs_Empty
                && !VG_(ksigismember)(&(tst->sig_mask), sigNo))
               break;
         }
         if (tid == VG_N_THREADS) 
            /* All threads have this signal blocked, so we can't
               deliver it just now */
            continue; /* for (sigNo = 1; ...) loop */
      }

      /* Ok, we can deliver signal sigNo to thread tid. */

      if (VG_(clo_trace_signals))
         VG_(message)(Vg_DebugMsg,"delivering signal %d to thread %d", 
                                  sigNo, tid );

      /* Create a signal delivery frame, and set the client's %ESP and
         %EIP so that when execution continues, we will enter the
         signal handler with the frame on top of the client's stack,
         as it expects. */
      vg_assert(VG_(is_valid_tid)(tid));
      vg_push_signal_frame ( tid, sigNo );
      VG_(threads)[tid].status = VgTs_Runnable;
      
      /* Signify that the signal has been delivered. */
      vg_dcss.dcss_sigpending[sigNo] = False;
      vg_dcss.dcss_destthread[sigNo] = VG_INVALID_THREADID;

      if (vg_scss.scss_per_sig[sigNo].scss_flags & VKI_SA_ONESHOT) {
         /* Do the ONESHOT thing. */
         vg_scss.scss_per_sig[sigNo].scss_handler = VKI_SIG_DFL;
         scss_changed = True;
      }
   }

   /* Unlock and return. */
   if (scss_changed) {
      /* handle_SCSS_change computes a new kernel blocking mask and
         applies that. */
      VG_(handle_SCSS_change)( False /* lazy update */ );
   } else {
      /* No SCSS change, so just restore the existing blocking
         mask. */
      VG_(restore_all_host_signals)( &saved_procmask );
   }

   return True;
}


/* Receive a signal from the host, and either discard it or park it in
   the queue of pending signals.  All other signals will be blocked
   when this handler runs.  Runs with all host signals blocked, so as
   to have mutual exclusion when adding stuff to the queue. */

static 
void vg_oursignalhandler ( Int sigNo )
{
   static UInt   segv_warns = 0;
   ThreadId      tid;
   Int           dummy_local;
   Bool          sane;
   vki_ksigset_t saved_procmask;

   /*
   if (sigNo == VKI_SIGUSR1) {
      VG_(printf)("YOWZA!  SIGUSR1\n\n");
      VG_(clo_trace_pthread_level) = 2;
      VG_(clo_trace_sched) = True;
      VG_(clo_trace_syscalls) = True;
      VG_(clo_trace_signals) = True;
      return;
   }
   */

   if (VG_(clo_trace_signals)) {
      VG_(start_msg)(Vg_DebugMsg);
      VG_(add_to_msg)("signal %d arrived ... ", sigNo );
   }
   vg_assert(sigNo >= 1 && sigNo <= VKI_KNSIG);

   /* Sanity check.  Ensure we're really running on the signal stack
      we asked for. */
   if (!(
         ((Char*)(&(VG_(sigstack)[0])) <= (Char*)(&dummy_local))
         &&
         ((Char*)(&dummy_local) < (Char*)(&(VG_(sigstack)[VG_SIGSTACK_SIZE_W])))
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
   vg_assert((Char*)(&dummy_local) < (Char*)(&(VG_(sigstack)[VG_SIGSTACK_SIZE_W])));

   VG_(block_all_host_signals)( &saved_procmask );

   /* This is a sanity check.  Either a signal has arrived because the
      client set a handler for it, or because some thread sigwaited on
      it.  Establish that at least one of these is the case. */
   sane = False;
   if (vg_scss.scss_per_sig[sigNo].scss_handler != VKI_SIG_DFL
       && vg_scss.scss_per_sig[sigNo].scss_handler != VKI_SIG_IGN) {
      sane = True;
   } else {
      for (tid = 1; tid < VG_N_THREADS; tid++) {
         if (VG_(threads)[tid].status != VgTs_WaitSIG) 
            continue;
         if (VG_(ksigismember)(&VG_(threads)[tid].sigs_waited_for, sigNo))
            sane = True;
      }
   }
   if (!sane) {
      if (VG_(clo_trace_signals)) {
         VG_(add_to_msg)("unexpected!");
         VG_(end_msg)();
      }
      /* Note: we panic with all signals blocked here.  Don't think
         that matters. */
      VG_(core_panic)("vg_oursignalhandler: unexpected signal");
   }
   /* End of the sanity check. */

   /* Decide what to do with it. */
   if (vg_dcss.dcss_sigpending[sigNo]) {
      /* pending; ignore it. */
      if (VG_(clo_trace_signals)) {
         VG_(add_to_msg)("already pending; discarded" );
         VG_(end_msg)();
      }
   } else {
      /* Ok, we'd better deliver it to the client. */
      /* Queue it up for delivery at some point in the future. */
      vg_dcss.dcss_sigpending[sigNo] = True;
      vg_dcss.dcss_destthread[sigNo] = VG_INVALID_THREADID;
      if (VG_(clo_trace_signals)) {
         VG_(add_to_msg)("queued" );
         VG_(end_msg)();
      }
   }

   /* We've finished messing with the queue, so re-enable host
      signals. */
   VG_(restore_all_host_signals)( &saved_procmask );

   if ( (sigNo == VKI_SIGSEGV || sigNo == VKI_SIGBUS 
         || sigNo == VKI_SIGFPE || sigNo == VKI_SIGILL)
        &&
        VG_(scheduler_jmpbuf_valid)
      ) {
      /* Can't continue; must longjmp back to the scheduler and thus
         enter the sighandler immediately. */
      VG_(longjmpd_on_signal) = sigNo;
      __builtin_longjmp(VG_(scheduler_jmpbuf),1);
   }

   if (sigNo == VKI_SIGSEGV && !VG_(scheduler_jmpbuf_valid)) {
      if (++segv_warns <= 3) {
	VG_(message)(Vg_UserMsg, 
           "Warning: SIGSEGV not in user code; either from syscall kill()" );
	VG_(message)(Vg_UserMsg, 
           "   or possible Valgrind bug.  "
           "This message is only shown 3 times." );
      }
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
   for (i = 1; i <= VKI_KNSIG; i++)
      if (VG_(ksigismember(&(sa->ksa_mask),i)))
         VG_(printf)("%d ", i);
   VG_(printf)("}\n");
}


/* At startup, copy the process' real signal state to the SCSS.
   Whilst doing this, block all real signals.  Then calculate SKSS and
   set the kernel to that.  Also initialise DCSS. 
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

   /* Copy per-signal settings to SCSS. */
   for (i = 1; i <= VKI_KNSIG; i++) {

      /* Get the old host action */
      ret = VG_(ksigaction)(i, NULL, &sa);
      vg_assert(ret == 0);

      if (VG_(clo_trace_signals))
         VG_(printf)("snaffling handler 0x%x for signal %d\n", 
                     (Addr)(sa.ksa_handler), i );

      vg_scss.scss_per_sig[i].scss_handler  = sa.ksa_handler;
      vg_scss.scss_per_sig[i].scss_flags    = sa.ksa_flags;
      vg_scss.scss_per_sig[i].scss_mask     = sa.ksa_mask;
      vg_scss.scss_per_sig[i].scss_restorer = sa.ksa_restorer;
   }

   /* Copy the alt stack, if any. */
   ret = VG_(ksigaltstack)(NULL, &vg_scss.altstack);
   vg_assert(ret == 0);

   /* Copy the process' signal mask into the root thread. */
   vg_assert(VG_(threads)[1].status == VgTs_Runnable);
   VG_(threads)[1].sig_mask = saved_procmask;

   /* Initialise DCSS. */
   for (i = 1; i <= VKI_KNSIG; i++) {
      vg_dcss.dcss_sigpending[i] = False;
      vg_dcss.dcss_destthread[i] = VG_INVALID_THREADID;
   }

   /* Register an alternative stack for our own signal handler to run on. */
   altstack_info.ss_sp = &(VG_(sigstack)[0]);
   altstack_info.ss_size = VG_SIGSTACK_SIZE_W * sizeof(UInt);
   altstack_info.ss_flags = 0;
   ret = VG_(ksigaltstack)(&altstack_info, NULL);
   if (ret != 0) {
      VG_(core_panic)(
         "vg_sigstartup_actions: couldn't install alternative sigstack");
   }
   if (VG_(clo_trace_signals)) {
      VG_(message)(Vg_DebugExtraMsg, 
         "vg_sigstartup_actions: sigstack installed ok");
   }

   /* DEBUGGING HACK */
   /* VG_(ksignal)(VKI_SIGUSR1, &VG_(oursignalhandler)); */

   /* Calculate SKSS and apply it.  This also sets the initial kernel
      mask we need to run with. */
   VG_(handle_SCSS_change)( True /* forced update */ );
}


/* Copy the process' sim signal state to the real state,
   for when we transfer from the simulated to real CPU.
   PROBLEM: what if we're running a signal handler when we
   get here?  Hmm.
   I guess we wind up in vg_signalreturn_bogusRA, *or* the
   handler has done/will do a longjmp, in which case we're ok.
*/
void VG_(sigshutdown_actions) ( void )
{
   Int i, ret;

   vki_ksigset_t  saved_procmask;
   vki_ksigaction sa;

   VG_(block_all_host_signals)( &saved_procmask );

   /* Copy per-signal settings from SCSS. */
   for (i = 1; i <= VKI_KNSIG; i++) {

      sa.ksa_handler  = vg_scss.scss_per_sig[i].scss_handler;
      sa.ksa_flags    = vg_scss.scss_per_sig[i].scss_flags;
      sa.ksa_mask     = vg_scss.scss_per_sig[i].scss_mask;
      sa.ksa_restorer = vg_scss.scss_per_sig[i].scss_restorer;

      if (VG_(clo_trace_signals))
         VG_(printf)("restoring handler 0x%x for signal %d\n", 
                     (Addr)(sa.ksa_handler), i );

      /* Set the old host action */
      ret = VG_(ksigaction)(i, &sa, NULL);
      if (i != VKI_SIGKILL && i != VKI_SIGSTOP) 
         vg_assert(ret == 0);
   }

   /* Restore the sig alt stack. */
   ret = VG_(ksigaltstack)(&vg_scss.altstack, NULL);
   vg_assert(ret == 0);

   /* A bit of a kludge -- set the sigmask to that of the root
      thread. */
   vg_assert(VG_(threads)[1].status != VgTs_Empty);
   VG_(restore_all_host_signals)( &VG_(threads)[1].sig_mask );
}


/*--------------------------------------------------------------------*/
/*--- end                                             vg_signals.c ---*/
/*--------------------------------------------------------------------*/
