
/*--------------------------------------------------------------------*/
/*--- Create/destroy signal delivery frames.                       ---*/
/*---                                       sigframe-s390x-linux.c ---*/
/*--------------------------------------------------------------------*/

/*
   This file is part of Valgrind, a dynamic binary instrumentation
   framework.

   Copyright IBM Corp. 2010-2017

   This program is free software; you can redistribute it and/or
   modify it under the terms of the GNU General Public License as
   published by the Free Software Foundation; either version 2 of the
   License, or (at your option) any later version.

   This program is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, see <http://www.gnu.org/licenses/>.

   The GNU General Public License is contained in the file COPYING.
*/

/* Contributed by Christian Borntraeger */

#include "pub_core_basics.h"
#include "pub_core_vki.h"
#include "pub_core_vkiscnums.h"
#include "pub_core_threadstate.h"
#include "pub_core_aspacemgr.h"
#include "pub_core_libcbase.h"
#include "pub_core_libcassert.h"
#include "pub_core_libcprint.h"
#include "pub_core_machine.h"
#include "pub_core_options.h"
#include "pub_core_sigframe.h"
#include "pub_core_signals.h"
#include "pub_core_tooliface.h"
#include "pub_core_trampoline.h"
#include "priv_sigframe.h"

#if defined(VGA_s390x)

/* This module creates and removes signal frames for signal deliveries
   on s390x-linux.

   Note, this file contains kernel-specific knowledge in the form of
   'struct sigframe' and 'struct rt_sigframe'.

   Either a 'struct sigframe' or a 'struct rtsigframe' is pushed
   onto the client's stack.  This contains a subsidiary
   vki_ucontext.  That holds the vcpu's state across the signal,
   so that the sighandler can mess with the vcpu state if it
   really wants.
*/

#define SET_SIGNAL_GPR(zztst, zzn, zzval)                    \
   do { zztst->arch.vex.guest_r##zzn = (unsigned long)(zzval);              \
      VG_TRACK( post_reg_write, Vg_CoreSignal, zztst->tid,     \
                offsetof(VexGuestS390XState,guest_r##zzn), \
                sizeof(UWord) );                             \
   } while (0)

/*------------------------------------------------------------*/
/*--- Signal frame layouts                                 ---*/
/*------------------------------------------------------------*/

// A structure in which to save the application's registers
// during the execution of signal handlers.

// Linux has 2 signal frame structures: one for normal signal
// deliveries, and one for SA_SIGINFO deliveries (also known as RT
// signals).
//
// In theory, so long as we get the arguments to the handler function
// right, it doesn't matter what the exact layout of the rest of the
// frame is.  Unfortunately, things like gcc's exception unwinding
// make assumptions about the locations of various parts of the frame,
// so we need to duplicate it exactly.

/* Valgrind-specific parts of the signal frame */
struct vg_sigframe
{
   /* Sanity check word. */
   UInt magicPI;

   UInt handlerflags;	/* flags for signal handler */


   /* Safely-saved version of sigNo, as described above. */
   Int  sigNo_private;

   /* XXX This is wrong.  Surely we should store the shadow values
      into the shadow memory behind the actual values? */
   VexGuestS390XState vex_shadow1;
   VexGuestS390XState vex_shadow2;

   /* HACK ALERT */
   VexGuestS390XState vex;
   /* end HACK ALERT */

   /* saved signal mask to be restored when handler returns */
   vki_sigset_t	mask;

   /* Sanity check word.  Is the highest-addressed word; do not
      move!*/
   UInt magicE;
};

#define S390_SYSCALL_SIZE 2

struct sigframe
{
   UChar callee_used_stack[__VKI_SIGNAL_FRAMESIZE];
   struct vki_sigcontext sc;
   _vki_sigregs sregs;
   Int sigNo;
   UChar retcode[S390_SYSCALL_SIZE];

   struct vg_sigframe vg;
};

struct rt_sigframe
{
   UChar callee_used_stack[__VKI_SIGNAL_FRAMESIZE];
   UChar retcode[S390_SYSCALL_SIZE];
   struct vki_siginfo info;
   struct vki_ucontext uc;

   struct vg_sigframe vg;
};

/*------------------------------------------------------------*/
/*--- Creating signal frames                               ---*/
/*------------------------------------------------------------*/

/* Saves all user-controlled register into a _vki_sigregs structure */
static void save_sigregs(ThreadState *tst, _vki_sigregs *sigregs)
{
   sigregs->regs.gprs[0]  = tst->arch.vex.guest_r0;
   sigregs->regs.gprs[1]  = tst->arch.vex.guest_r1;
   sigregs->regs.gprs[2]  = tst->arch.vex.guest_r2;
   sigregs->regs.gprs[3]  = tst->arch.vex.guest_r3;
   sigregs->regs.gprs[4]  = tst->arch.vex.guest_r4;
   sigregs->regs.gprs[5]  = tst->arch.vex.guest_r5;
   sigregs->regs.gprs[6]  = tst->arch.vex.guest_r6;
   sigregs->regs.gprs[7]  = tst->arch.vex.guest_r7;
   sigregs->regs.gprs[8]  = tst->arch.vex.guest_r8;
   sigregs->regs.gprs[9]  = tst->arch.vex.guest_r9;
   sigregs->regs.gprs[10] = tst->arch.vex.guest_r10;
   sigregs->regs.gprs[11] = tst->arch.vex.guest_r11;
   sigregs->regs.gprs[12] = tst->arch.vex.guest_r12;
   sigregs->regs.gprs[13] = tst->arch.vex.guest_r13;
   sigregs->regs.gprs[14] = tst->arch.vex.guest_r14;
   sigregs->regs.gprs[15] = tst->arch.vex.guest_r15;

   sigregs->regs.acrs[0]  = tst->arch.vex.guest_a0;
   sigregs->regs.acrs[1]  = tst->arch.vex.guest_a1;
   sigregs->regs.acrs[2]  = tst->arch.vex.guest_a2;
   sigregs->regs.acrs[3]  = tst->arch.vex.guest_a3;
   sigregs->regs.acrs[4]  = tst->arch.vex.guest_a4;
   sigregs->regs.acrs[5]  = tst->arch.vex.guest_a5;
   sigregs->regs.acrs[6]  = tst->arch.vex.guest_a6;
   sigregs->regs.acrs[7]  = tst->arch.vex.guest_a7;
   sigregs->regs.acrs[8]  = tst->arch.vex.guest_a8;
   sigregs->regs.acrs[9]  = tst->arch.vex.guest_a9;
   sigregs->regs.acrs[10] = tst->arch.vex.guest_a10;
   sigregs->regs.acrs[11] = tst->arch.vex.guest_a11;
   sigregs->regs.acrs[12] = tst->arch.vex.guest_a12;
   sigregs->regs.acrs[13] = tst->arch.vex.guest_a13;
   sigregs->regs.acrs[14] = tst->arch.vex.guest_a14;
   sigregs->regs.acrs[15] = tst->arch.vex.guest_a15;

   sigregs->fpregs.fprs[0] = *((const Double *) &tst->arch.vex.guest_v0.w64[0]);
   sigregs->fpregs.fprs[1] = *((const Double *) &tst->arch.vex.guest_v1.w64[0]);
   sigregs->fpregs.fprs[2] = *((const Double *) &tst->arch.vex.guest_v2.w64[0]);
   sigregs->fpregs.fprs[3] = *((const Double *) &tst->arch.vex.guest_v3.w64[0]);
   sigregs->fpregs.fprs[4] = *((const Double *) &tst->arch.vex.guest_v4.w64[0]);
   sigregs->fpregs.fprs[5] = *((const Double *) &tst->arch.vex.guest_v5.w64[0]);
   sigregs->fpregs.fprs[6] = *((const Double *) &tst->arch.vex.guest_v6.w64[0]);
   sigregs->fpregs.fprs[7] = *((const Double *) &tst->arch.vex.guest_v7.w64[0]);
   sigregs->fpregs.fprs[8] = *((const Double *) &tst->arch.vex.guest_v8.w64[0]);
   sigregs->fpregs.fprs[9] = *((const Double *) &tst->arch.vex.guest_v9.w64[0]);
   sigregs->fpregs.fprs[10] = *((const Double *) &tst->arch.vex.guest_v10.w64[0]);
   sigregs->fpregs.fprs[11] = *((const Double *) &tst->arch.vex.guest_v11.w64[0]);
   sigregs->fpregs.fprs[12] = *((const Double *) &tst->arch.vex.guest_v12.w64[0]);
   sigregs->fpregs.fprs[13] = *((const Double *) &tst->arch.vex.guest_v13.w64[0]);
   sigregs->fpregs.fprs[14] = *((const Double *) &tst->arch.vex.guest_v14.w64[0]);
   sigregs->fpregs.fprs[15] = *((const Double *) &tst->arch.vex.guest_v15.w64[0]);
   sigregs->fpregs.fpc      = tst->arch.vex.guest_fpc;

   sigregs->regs.psw.addr = tst->arch.vex.guest_IA;
   /* save a sane dummy mask */
   sigregs->regs.psw.mask = 0x0705000180000000UL;
}

static void restore_sigregs(ThreadState *tst, _vki_sigregs *sigregs)
{
   tst->arch.vex.guest_r0  = sigregs->regs.gprs[0];
   tst->arch.vex.guest_r1  = sigregs->regs.gprs[1];
   tst->arch.vex.guest_r2  = sigregs->regs.gprs[2];
   tst->arch.vex.guest_r3  = sigregs->regs.gprs[3];
   tst->arch.vex.guest_r4  = sigregs->regs.gprs[4];
   tst->arch.vex.guest_r5  = sigregs->regs.gprs[5];
   tst->arch.vex.guest_r6  = sigregs->regs.gprs[6];
   tst->arch.vex.guest_r7  = sigregs->regs.gprs[7];
   tst->arch.vex.guest_r8  = sigregs->regs.gprs[8];
   tst->arch.vex.guest_r9  = sigregs->regs.gprs[9];
   tst->arch.vex.guest_r10 = sigregs->regs.gprs[10];
   tst->arch.vex.guest_r11 = sigregs->regs.gprs[11];
   tst->arch.vex.guest_r12 = sigregs->regs.gprs[12];
   tst->arch.vex.guest_r13 = sigregs->regs.gprs[13];
   tst->arch.vex.guest_r14 = sigregs->regs.gprs[14];
   tst->arch.vex.guest_r15 = sigregs->regs.gprs[15];

   tst->arch.vex.guest_a0  = sigregs->regs.acrs[0];
   tst->arch.vex.guest_a1  = sigregs->regs.acrs[1];
   tst->arch.vex.guest_a2  = sigregs->regs.acrs[2];
   tst->arch.vex.guest_a3  = sigregs->regs.acrs[3];
   tst->arch.vex.guest_a4  = sigregs->regs.acrs[4];
   tst->arch.vex.guest_a5  = sigregs->regs.acrs[5];
   tst->arch.vex.guest_a6  = sigregs->regs.acrs[6];
   tst->arch.vex.guest_a7  = sigregs->regs.acrs[7];
   tst->arch.vex.guest_a8  = sigregs->regs.acrs[8];
   tst->arch.vex.guest_a9  = sigregs->regs.acrs[9];
   tst->arch.vex.guest_a10 = sigregs->regs.acrs[10];
   tst->arch.vex.guest_a11 = sigregs->regs.acrs[11];
   tst->arch.vex.guest_a12 = sigregs->regs.acrs[12];
   tst->arch.vex.guest_a13 = sigregs->regs.acrs[13];
   tst->arch.vex.guest_a14 = sigregs->regs.acrs[14];
   tst->arch.vex.guest_a15 = sigregs->regs.acrs[15];

   *((Double *) &tst->arch.vex.guest_v0.w64[0])  = sigregs->fpregs.fprs[0];
   *((Double *) &tst->arch.vex.guest_v1.w64[0])  = sigregs->fpregs.fprs[1];
   *((Double *) &tst->arch.vex.guest_v2.w64[0])  = sigregs->fpregs.fprs[2];
   *((Double *) &tst->arch.vex.guest_v3.w64[0])  = sigregs->fpregs.fprs[3];
   *((Double *) &tst->arch.vex.guest_v4.w64[0])  = sigregs->fpregs.fprs[4];
   *((Double *) &tst->arch.vex.guest_v5.w64[0])  = sigregs->fpregs.fprs[5];
   *((Double *) &tst->arch.vex.guest_v6.w64[0])  = sigregs->fpregs.fprs[6];
   *((Double *) &tst->arch.vex.guest_v7.w64[0])  = sigregs->fpregs.fprs[7];
   *((Double *) &tst->arch.vex.guest_v8.w64[0])  = sigregs->fpregs.fprs[8];
   *((Double *) &tst->arch.vex.guest_v9.w64[0])  = sigregs->fpregs.fprs[9];
   *((Double *) &tst->arch.vex.guest_v10.w64[0]) = sigregs->fpregs.fprs[10];
   *((Double *) &tst->arch.vex.guest_v11.w64[0]) = sigregs->fpregs.fprs[11];
   *((Double *) &tst->arch.vex.guest_v12.w64[0]) = sigregs->fpregs.fprs[12];
   *((Double *) &tst->arch.vex.guest_v13.w64[0]) = sigregs->fpregs.fprs[13];
   *((Double *) &tst->arch.vex.guest_v14.w64[0]) = sigregs->fpregs.fprs[14];
   *((Double *) &tst->arch.vex.guest_v15.w64[0]) = sigregs->fpregs.fprs[15];
   tst->arch.vex.guest_fpc = sigregs->fpregs.fpc;

   tst->arch.vex.guest_IA = sigregs->regs.psw.addr;
}


/* Build the Valgrind-specific part of a signal frame. */

static void build_vg_sigframe(struct vg_sigframe *frame,
			      ThreadState *tst,
			      UInt flags,
			      Int sigNo)
{
   frame->sigNo_private = sigNo;
   frame->magicPI       = 0x31415927;
   frame->vex_shadow1   = tst->arch.vex_shadow1;
   frame->vex_shadow2   = tst->arch.vex_shadow2;
   /* HACK ALERT */
   frame->vex           = tst->arch.vex;
   /* end HACK ALERT */
   frame->mask          = tst->sig_mask;
   frame->handlerflags  = flags;
   frame->magicE        = 0x27182818;
}


static Addr build_sigframe(ThreadState *tst,
			   Addr sp_top_of_frame,
			   const vki_siginfo_t *siginfo,
			   const struct vki_ucontext *siguc,
			   UInt flags,
			   const vki_sigset_t *mask,
			   void *restorer)
{
   struct sigframe *frame;
   Addr sp = sp_top_of_frame;

   vg_assert((flags & VKI_SA_SIGINFO) == 0);
   vg_assert((sizeof(*frame) & 7) == 0);
   vg_assert((sp & 7) == 0);

   sp -= sizeof(*frame);
   frame = (struct sigframe *)sp;

   if (! ML_(sf_maybe_extend_stack)(tst, sp, sizeof(*frame), flags))
      return sp_top_of_frame;

   /* retcode, sigNo, sc, sregs fields are to be written */
   VG_TRACK( pre_mem_write, Vg_CoreSignal, tst->tid, "signal handler frame",
	     sp, offsetof(struct sigframe, vg) );

   save_sigregs(tst, &frame->sregs);

   frame->sigNo = siginfo->si_signo;
   frame->sc.sregs = &frame->sregs;
   VG_(memcpy)(frame->sc.oldmask, mask->sig, sizeof(frame->sc.oldmask));

   if (flags & VKI_SA_RESTORER) {
      SET_SIGNAL_GPR(tst, 14, restorer);
   } else {
      frame->retcode[0] = 0x0a;
      frame->retcode[1] = __NR_sigreturn;
      /* This normally should be &frame->recode. but since there
         might be problems with non-exec stack and we must discard
         the translation for the on-stack sigreturn we just use the
         trampoline like x86,ppc. We still fill in the retcode, lets
         just hope that nobody actually jumps here */
      SET_SIGNAL_GPR(tst, 14, (Addr)&VG_(s390x_linux_SUBST_FOR_sigreturn));
   }

   SET_SIGNAL_GPR(tst, 2, siginfo->si_signo);
   SET_SIGNAL_GPR(tst, 3, &frame->sc);
   /* fixs390: we dont fill in trapno and prot_addr in r4 and r5*/

   /* Set up backchain. */
   *((Addr *) sp) = sp_top_of_frame;

   VG_TRACK( post_mem_write, Vg_CoreSignal, tst->tid,
             sp, offsetof(struct sigframe, vg) );

   build_vg_sigframe(&frame->vg, tst, flags, siginfo->si_signo);

   return sp;
}

static Addr build_rt_sigframe(ThreadState *tst,
			      Addr sp_top_of_frame,
			      const vki_siginfo_t *siginfo,
			      const struct vki_ucontext *siguc,
			      UInt flags,
			      const vki_sigset_t *mask,
			      void *restorer)
{
   struct rt_sigframe *frame;
   Addr sp = sp_top_of_frame;
   Int sigNo = siginfo->si_signo;

   vg_assert((flags & VKI_SA_SIGINFO) != 0);
   vg_assert((sizeof(*frame) & 7) == 0);
   vg_assert((sp & 7) == 0);

   sp -= sizeof(*frame);
   frame = (struct rt_sigframe *)sp;

   if (! ML_(sf_maybe_extend_stack)(tst, sp, sizeof(*frame), flags))
      return sp_top_of_frame;

   /* retcode, sigNo, sc, sregs fields are to be written */
   VG_TRACK( pre_mem_write, Vg_CoreSignal, tst->tid, "signal handler frame",
	     sp, offsetof(struct rt_sigframe, vg) );

   save_sigregs(tst, &frame->uc.uc_mcontext);

   if (flags & VKI_SA_RESTORER) {
      frame->retcode[0] = 0;
      frame->retcode[1] = 0;
      SET_SIGNAL_GPR(tst, 14, restorer);
   } else {
      frame->retcode[0] = 0x0a;
      frame->retcode[1] = __NR_rt_sigreturn;
      /* This normally should be &frame->recode. but since there
         might be problems with non-exec stack and we must discard
         the translation for the on-stack sigreturn we just use the
         trampoline like x86,ppc. We still fill in the retcode, lets
         just hope that nobody actually jumps here */
      SET_SIGNAL_GPR(tst, 14, (Addr)&VG_(s390x_linux_SUBST_FOR_rt_sigreturn));
   }

   VG_(memcpy)(&frame->info, siginfo, sizeof(vki_siginfo_t));
   frame->uc.uc_flags = 0;
   frame->uc.uc_link = 0;
   frame->uc.uc_sigmask = *mask;
   frame->uc.uc_stack = tst->altstack;

   SET_SIGNAL_GPR(tst, 2, siginfo->si_signo);
   SET_SIGNAL_GPR(tst, 3, &frame->info);
   SET_SIGNAL_GPR(tst, 4, &frame->uc);

   /* Set up backchain. */
   *((Addr *) sp) = sp_top_of_frame;

   VG_TRACK( post_mem_write, Vg_CoreSignal, tst->tid,
             sp, offsetof(struct rt_sigframe, vg) );

   build_vg_sigframe(&frame->vg, tst, flags, sigNo);
   return sp;
}

/* EXPORTED */
void VG_(sigframe_create)( ThreadId tid,
			   Bool on_altstack,
			   Addr sp_top_of_frame,
			   const vki_siginfo_t *siginfo,
			   const struct vki_ucontext *siguc,
			   void *handler,
			   UInt flags,
			   const vki_sigset_t *mask,
			   void *restorer )
{
   Addr sp;
   ThreadState* tst = VG_(get_ThreadState)(tid);

   if (flags & VKI_SA_SIGINFO)
      sp = build_rt_sigframe(tst, sp_top_of_frame, siginfo, siguc,
			     flags, mask, restorer);
   else
      sp = build_sigframe(tst, sp_top_of_frame, siginfo, siguc,
			  flags, mask, restorer);

   /* Set the thread so it will next run the handler. */
   VG_(set_SP)(tid, sp);
   VG_TRACK( post_reg_write, Vg_CoreSignal, tid, VG_O_STACK_PTR, sizeof(Addr));

   tst->arch.vex.guest_IA = (Addr) handler;
   /* We might have interrupted a repeating instruction that uses the guest
      counter. Since our VEX requires that a new instruction will see a
      guest counter == 0, we have to set it here. The old value will be
      restored by restore_vg_sigframe. */
   tst->arch.vex.guest_counter = 0;
   /* This thread needs to be marked runnable, but we leave that the
      caller to do. */
}


/*------------------------------------------------------------*/
/*--- Destroying signal frames                             ---*/
/*------------------------------------------------------------*/

/* Return False and don't do anything, just set the client to take a
   segfault, if it looks like the frame is corrupted. */
static
Bool restore_vg_sigframe ( ThreadState *tst,
                           struct vg_sigframe *frame, Int *sigNo )
{
   if (frame->magicPI != 0x31415927 ||
       frame->magicE  != 0x27182818) {
      VG_(message)(Vg_UserMsg, "Thread %u return signal frame "
			       "corrupted.  Killing process.\n",
		   tst->tid);
      VG_(set_default_handler)(VKI_SIGSEGV);
      VG_(synth_fault)(tst->tid);
      *sigNo = VKI_SIGSEGV;
      return False;
   }
   tst->sig_mask         = frame->mask;
   tst->tmp_sig_mask     = frame->mask;
   tst->arch.vex_shadow1 = frame->vex_shadow1;
   tst->arch.vex_shadow2 = frame->vex_shadow2;
   /* HACK ALERT */
   tst->arch.vex         = frame->vex;
   /* end HACK ALERT */
   *sigNo                = frame->sigNo_private;
   return True;
}

static
SizeT restore_sigframe ( ThreadState *tst,
                         struct sigframe *frame, Int *sigNo )
{
   if (restore_vg_sigframe(tst, &frame->vg, sigNo))
      restore_sigregs(tst, frame->sc.sregs);

   return sizeof(*frame);
}

static
SizeT restore_rt_sigframe ( ThreadState *tst,
                            struct rt_sigframe *frame, Int *sigNo )
{
   if (restore_vg_sigframe(tst, &frame->vg, sigNo)) {
      restore_sigregs(tst, &frame->uc.uc_mcontext);
   }
   return sizeof(*frame);
}


/* EXPORTED */
void VG_(sigframe_destroy)( ThreadId tid, Bool isRT )
{
   Addr          sp;
   ThreadState*  tst;
   SizeT         size;
   Int           sigNo;

   tst = VG_(get_ThreadState)(tid);

   /* Correctly reestablish the frame base address. */
   sp   = tst->arch.vex.guest_SP;

   if (!isRT)
      size = restore_sigframe(tst, (struct sigframe *)sp, &sigNo);
   else
      size = restore_rt_sigframe(tst, (struct rt_sigframe *)sp, &sigNo);

   /* same as for creation: we must announce the full memory (including
      alignment), otherwise massif might fail on longjmp */
   VG_TRACK( die_mem_stack_signal, sp - VG_STACK_REDZONE_SZB,
             size + VG_STACK_REDZONE_SZB );

   if (VG_(clo_trace_signals))
      VG_(message)(
         Vg_DebugMsg,
         "VG_(sigframe_destroy) (thread %u): isRT=%d valid magic; IP=%#llx\n",
         tid, isRT, tst->arch.vex.guest_IA);

   /* tell the tools */
   VG_TRACK( post_deliver_signal, tid, sigNo );
}

#endif /* VGA_s390x */

/*--------------------------------------------------------------------*/
/*--- end                                   sigframe-s390x-linux.c ---*/
/*--------------------------------------------------------------------*/
