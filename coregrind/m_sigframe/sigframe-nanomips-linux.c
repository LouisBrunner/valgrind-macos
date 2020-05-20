
/*--------------------------------------------------------------------*/
/*--- Create/destroy signal delivery frames.                       ---*/
/*---                                sigframe-nanomips-linux.c     ---*/
/*--------------------------------------------------------------------*/

/*
   This file is part of Valgrind, a dynamic binary instrumentation
   framework.

   Copyright (C) 2017-2018 RT-RK

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

#if defined(VGP_nanomips_linux)

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

struct vg_sig_private {
   UInt magicPI;
   UInt sigNo_private;
   VexGuestMIPS32State vex_shadow1;
   VexGuestMIPS32State vex_shadow2;
};

struct rt_sigframe {
   UInt rs_ass[4];    /* argument save space */
   UInt rs_pad[2];    /* Was: signal trampoline */
   vki_siginfo_t rs_info;
   struct vki_ucontext rs_uc;
   struct vg_sig_private priv;
};

static
void setup_sigcontext2 ( ThreadState* tst, struct vki_sigcontext **sc1,
                         const vki_siginfo_t *si) {
   struct vki_sigcontext *sc = *sc1;
   VG_TRACK( pre_mem_write, Vg_CoreSignal, tst->tid, "signal frame mcontext",
             (Addr)sc, sizeof(unsigned long long) * 34 );
   sc->sc_regs[1] = tst->arch.vex.guest_r1;
   sc->sc_regs[2] = tst->arch.vex.guest_r2;
   sc->sc_regs[3] = tst->arch.vex.guest_r3;
   sc->sc_regs[4] = tst->arch.vex.guest_r4;
   sc->sc_regs[5] = tst->arch.vex.guest_r5;
   sc->sc_regs[6] = tst->arch.vex.guest_r6;
   sc->sc_regs[7] = tst->arch.vex.guest_r7;
   sc->sc_regs[8] = tst->arch.vex.guest_r8;
   sc->sc_regs[9] = tst->arch.vex.guest_r9;
   sc->sc_regs[10] = tst->arch.vex.guest_r10;
   sc->sc_regs[11] = tst->arch.vex.guest_r11;
   sc->sc_regs[12] = tst->arch.vex.guest_r12;
   sc->sc_regs[13] = tst->arch.vex.guest_r13;
   sc->sc_regs[14] = tst->arch.vex.guest_r14;
   sc->sc_regs[15] = tst->arch.vex.guest_r15;
   sc->sc_regs[16] = tst->arch.vex.guest_r16;
   sc->sc_regs[17] = tst->arch.vex.guest_r17;
   sc->sc_regs[18] = tst->arch.vex.guest_r18;
   sc->sc_regs[19] = tst->arch.vex.guest_r19;
   sc->sc_regs[20] = tst->arch.vex.guest_r20;
   sc->sc_regs[21] = tst->arch.vex.guest_r21;
   sc->sc_regs[22] = tst->arch.vex.guest_r22;
   sc->sc_regs[23] = tst->arch.vex.guest_r23;
   sc->sc_regs[24] = tst->arch.vex.guest_r24;
   sc->sc_regs[25] = tst->arch.vex.guest_r25;
   sc->sc_regs[26] = tst->arch.vex.guest_r26;
   sc->sc_regs[27] = tst->arch.vex.guest_r27;
   sc->sc_regs[28] = tst->arch.vex.guest_r28;
   sc->sc_regs[29] = tst->arch.vex.guest_r29;
   sc->sc_regs[30] = tst->arch.vex.guest_r30;
   sc->sc_regs[31] = tst->arch.vex.guest_r31;
   sc->sc_pc = tst->arch.vex.guest_PC;
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
                           void *restorer ) {
   Addr sp;
   ThreadState* tst = VG_(get_ThreadState)(tid);
   Int sigNo = siginfo->si_signo;
   struct vg_sig_private *priv;
   tst = VG_(get_ThreadState)(tid);
   sp = sp_top_of_frame - sizeof(struct rt_sigframe);
   sp &= ~0xF;

   if (! ML_(sf_maybe_extend_stack)(tst, sp, sp_top_of_frame - sp, flags))
      return;

   vg_assert(VG_IS_16_ALIGNED(sp));
   struct rt_sigframe *frame = (struct rt_sigframe *) sp;
   struct vki_ucontext *ucp = &frame->rs_uc;

   if (VG_(clo_trace_signals))
      VG_(printf)("rt_sigframe\n");

   /* Create siginfo.  */
   VG_TRACK( pre_mem_write, Vg_CoreSignal, tid, "signal frame siginfo",
             (Addr)&frame->rs_info, sizeof(frame->rs_info) );
   VG_(memcpy)(&frame->rs_info, siginfo, sizeof(*siginfo));
   VG_TRACK( post_mem_write, Vg_CoreSignal, tid,
             (Addr)&frame->rs_info, sizeof(frame->rs_info) );
   /* Create the ucontext.  */
   VG_TRACK( pre_mem_write, Vg_CoreSignal, tid, "signal frame ucontext",
             (Addr)ucp, offsetof(struct vki_ucontext, uc_mcontext) );
   ucp->uc_flags = 0;
   ucp->uc_link = 0;
   ucp->uc_stack = tst->altstack;
   VG_TRACK( post_mem_write, Vg_CoreSignal, tid, (Addr)ucp,
             offsetof(struct vki_ucontext, uc_mcontext) );
   struct vki_sigcontext *scp = &(frame->rs_uc.uc_mcontext);
   setup_sigcontext2(tst, &(scp), siginfo);
   ucp->uc_sigmask = tst->sig_mask;
   priv = &frame->priv;
   /*
    * Arguments to signal handler:
    *
    *   a0 = signal number
    *   a1 = pointer to siginfo_t
    *   a2 = pointer to ucontext
    *
    * $25 and c0_epc point to the signal handler, $29 points to
    * the struct rt_sigframe.
    */
   tst->arch.vex.guest_r4 = siginfo->si_signo;
   tst->arch.vex.guest_r5 = (Addr) &frame->rs_info;
   tst->arch.vex.guest_r6 = (Addr) &frame->rs_uc;
   tst->arch.vex.guest_r29 = (Addr) frame;
   tst->arch.vex.guest_r25 = (Addr) handler;

   if (flags & VKI_SA_RESTORER) {
      tst->arch.vex.guest_r31 = (Addr) restorer;
   } else {
      tst->arch.vex.guest_r31 = (Addr)&VG_(nanomips_linux_SUBST_FOR_rt_sigreturn);
   }

   priv->magicPI       = 0x31415927;
   priv->sigNo_private = sigNo;
   priv->vex_shadow1   = tst->arch.vex_shadow1;
   priv->vex_shadow2   = tst->arch.vex_shadow2;
   /* Set the thread so it will next run the handler. */
   /* tst->m_sp  = sp;  also notify the tool we've updated SP */
   VG_TRACK( post_reg_write, Vg_CoreSignal, tid, VG_O_STACK_PTR, sizeof(Addr));

   if (VG_(clo_trace_signals))
      VG_(printf)("handler = %p\n", handler);

   tst->arch.vex.guest_PC = (Addr) handler;
   /* This thread needs to be marked runnable, but we leave that the
      caller to do. */
}

void VG_(sigframe_destroy)( ThreadId tid, Bool isRT ) {
   ThreadState *tst;
   struct rt_sigframe *frame;
   struct vg_sig_private *priv1;
   struct vki_ucontext *ucp;
   struct vki_sigcontext *mc;

   vg_assert(isRT);
   vg_assert(VG_(is_valid_tid)(tid));
   tst = VG_(get_ThreadState)(tid);
   frame = (struct rt_sigframe *)(Addr)tst->arch.vex.guest_r29;
   priv1 = &frame->priv;
   vg_assert(priv1->magicPI == 0x31415927);
   ucp = &frame->rs_uc;
   tst->sig_mask = ucp->uc_sigmask;
   tst->tmp_sig_mask = ucp->uc_sigmask;
   mc = &ucp->uc_mcontext;
   tst->arch.vex.guest_r1 = mc->sc_regs[1];
   tst->arch.vex.guest_r2 = mc->sc_regs[2];
   tst->arch.vex.guest_r3 = mc->sc_regs[3];
   tst->arch.vex.guest_r4 = mc->sc_regs[4];
   tst->arch.vex.guest_r5 = mc->sc_regs[5];
   tst->arch.vex.guest_r6 = mc->sc_regs[6];
   tst->arch.vex.guest_r7 = mc->sc_regs[7];
   tst->arch.vex.guest_r8 = mc->sc_regs[8];
   tst->arch.vex.guest_r9 = mc->sc_regs[9];
   tst->arch.vex.guest_r10 = mc->sc_regs[10];
   tst->arch.vex.guest_r11 = mc->sc_regs[11];
   tst->arch.vex.guest_r12 = mc->sc_regs[12];
   tst->arch.vex.guest_r13 = mc->sc_regs[13];
   tst->arch.vex.guest_r14 = mc->sc_regs[14];
   tst->arch.vex.guest_r15 = mc->sc_regs[15];
   tst->arch.vex.guest_r16 = mc->sc_regs[16];
   tst->arch.vex.guest_r17 = mc->sc_regs[17];
   tst->arch.vex.guest_r18 = mc->sc_regs[18];
   tst->arch.vex.guest_r19 = mc->sc_regs[19];
   tst->arch.vex.guest_r20 = mc->sc_regs[20];
   tst->arch.vex.guest_r21 = mc->sc_regs[21];
   tst->arch.vex.guest_r22 = mc->sc_regs[22];
   tst->arch.vex.guest_r23 = mc->sc_regs[23];
   tst->arch.vex.guest_r24 = mc->sc_regs[24];
   tst->arch.vex.guest_r25 = mc->sc_regs[25];
   tst->arch.vex.guest_r26 = mc->sc_regs[26];
   tst->arch.vex.guest_r27 = mc->sc_regs[27];
   tst->arch.vex.guest_r28 = mc->sc_regs[28];
   tst->arch.vex.guest_r30 = mc->sc_regs[30];
   tst->arch.vex.guest_PC = mc->sc_pc;
   tst->arch.vex.guest_r31 = mc->sc_regs[31];
   tst->arch.vex.guest_r29 = mc->sc_regs[29];
   tst->arch.vex_shadow1 = priv1->vex_shadow1;
   tst->arch.vex_shadow2 = priv1->vex_shadow2;
   VG_TRACK(die_mem_stack_signal, (Addr)frame, sizeof(*frame));

   if (VG_(clo_trace_signals))
      VG_(message)( Vg_DebugMsg,
                    "VG_(signal_return) (thread %u): isRT=%d valid magic; EIP=%#x\n",
                    tid, isRT, tst->arch.vex.guest_PC);

   VG_TRACK(post_deliver_signal, tid, priv1->sigNo_private);
}

#endif

/*--------------------------------------------------------------------*/
/*--- end                                sigframe-nanomips-linux.c ---*/
/*--------------------------------------------------------------------*/
