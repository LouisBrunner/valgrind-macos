
/*--------------------------------------------------------------------*/
/*--- Create/destroy signal delivery frames.                       ---*/
/*---                                  sigframe-tilegx-linux.c     ---*/
/*--------------------------------------------------------------------*/

/*
  This file is part of Valgrind, a dynamic binary instrumentation
  framework.

  Copyright (C) 2010-2015 Tilera Corp.

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

/* Contributed by Zhi-Gang Liu <zliu at tilera dot com> */

#if defined(VGP_tilegx_linux)

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

struct vg_sig_private
{
  UInt magicPI;
  UInt sigNo_private;
  VexGuestTILEGXState vex_shadow1;
  VexGuestTILEGXState vex_shadow2;
};

#ifndef C_ABI_SAVE_AREA_SIZE
#define C_ABI_SAVE_AREA_SIZE  16
#endif
struct rt_sigframe {
  unsigned char save_area[C_ABI_SAVE_AREA_SIZE]; /* caller save area */
  vki_siginfo_t rs_info;
  struct vki_ucontext rs_uc;
  struct vg_sig_private priv;
};


static
void setup_sigcontext2 ( ThreadState* tst, struct vki_sigcontext **sc1,
                         const vki_siginfo_t *si )
{

  struct vki_sigcontext *sc = *sc1;

  VG_TRACK( pre_mem_write, Vg_CoreSignal, tst->tid, "signal frame mcontext",
            (Addr)sc, sizeof(unsigned long long)*34 );
  sc->gregs[0] = tst->arch.vex.guest_r0;
  sc->gregs[1] = tst->arch.vex.guest_r1;
  sc->gregs[2] = tst->arch.vex.guest_r2;
  sc->gregs[3] = tst->arch.vex.guest_r3;
  sc->gregs[4] = tst->arch.vex.guest_r4;
  sc->gregs[5] = tst->arch.vex.guest_r5;
  sc->gregs[6] = tst->arch.vex.guest_r6;
  sc->gregs[7] = tst->arch.vex.guest_r7;
  sc->gregs[8] = tst->arch.vex.guest_r8;
  sc->gregs[9] = tst->arch.vex.guest_r9;
  sc->gregs[10] = tst->arch.vex.guest_r10;
  sc->gregs[11] = tst->arch.vex.guest_r11;
  sc->gregs[12] = tst->arch.vex.guest_r12;
  sc->gregs[13] = tst->arch.vex.guest_r13;
  sc->gregs[14] = tst->arch.vex.guest_r14;
  sc->gregs[15] = tst->arch.vex.guest_r15;
  sc->gregs[16] = tst->arch.vex.guest_r16;
  sc->gregs[17] = tst->arch.vex.guest_r17;
  sc->gregs[18] = tst->arch.vex.guest_r18;
  sc->gregs[19] = tst->arch.vex.guest_r19;
  sc->gregs[20] = tst->arch.vex.guest_r20;
  sc->gregs[21] = tst->arch.vex.guest_r21;
  sc->gregs[22] = tst->arch.vex.guest_r22;
  sc->gregs[23] = tst->arch.vex.guest_r23;
  sc->gregs[24] = tst->arch.vex.guest_r24;
  sc->gregs[25] = tst->arch.vex.guest_r25;
  sc->gregs[26] = tst->arch.vex.guest_r26;
  sc->gregs[27] = tst->arch.vex.guest_r27;
  sc->gregs[28] = tst->arch.vex.guest_r28;
  sc->gregs[29] = tst->arch.vex.guest_r29;
  sc->gregs[30] = tst->arch.vex.guest_r30;
  sc->gregs[31] = tst->arch.vex.guest_r31;
  sc->gregs[32] = tst->arch.vex.guest_r32;
  sc->gregs[33] = tst->arch.vex.guest_r33;
  sc->gregs[34] = tst->arch.vex.guest_r34;
  sc->gregs[35] = tst->arch.vex.guest_r35;
  sc->gregs[36] = tst->arch.vex.guest_r36;
  sc->gregs[37] = tst->arch.vex.guest_r37;
  sc->gregs[38] = tst->arch.vex.guest_r38;
  sc->gregs[39] = tst->arch.vex.guest_r39;
  sc->gregs[40] = tst->arch.vex.guest_r40;
  sc->gregs[41] = tst->arch.vex.guest_r41;
  sc->gregs[42] = tst->arch.vex.guest_r42;
  sc->gregs[43] = tst->arch.vex.guest_r43;
  sc->gregs[44] = tst->arch.vex.guest_r44;
  sc->gregs[45] = tst->arch.vex.guest_r45;
  sc->gregs[46] = tst->arch.vex.guest_r46;
  sc->gregs[47] = tst->arch.vex.guest_r47;
  sc->gregs[48] = tst->arch.vex.guest_r48;
  sc->gregs[49] = tst->arch.vex.guest_r49;
  sc->gregs[50] = tst->arch.vex.guest_r50;
  sc->gregs[51] = tst->arch.vex.guest_r51;
  sc->gregs[52] = tst->arch.vex.guest_r52;
  sc->tp        = tst->arch.vex.guest_r53;
  sc->sp        = tst->arch.vex.guest_r54;
  sc->lr        = tst->arch.vex.guest_r55;
  sc->pc        = tst->arch.vex.guest_pc;
}

/* EXPORTED */
void VG_(sigframe_create)( ThreadId tid,
                           Addr sp_top_of_frame,
                           const vki_siginfo_t *siginfo,
                           const struct vki_ucontext *siguc,
                           void *handler,
                           UInt flags,
                           const vki_sigset_t *mask,
                           void *restorer )
{
  Addr sp;
  ThreadState* tst;
  Addr faultaddr;
  Int sigNo = siginfo->si_signo;
  struct vg_sig_private *priv;

  /* Stack must be 8-byte aligned */
  sp_top_of_frame &= ~0x7ULL;

  sp = sp_top_of_frame - sizeof(struct rt_sigframe);

  tst = VG_(get_ThreadState)(tid);
  if (! ML_(sf_maybe_extend_stack)(tst, sp, sizeof(struct rt_sigframe), flags))
    return;

  vg_assert(VG_IS_8_ALIGNED(sp));

  /* SIGILL defines addr to be the faulting address */

  faultaddr = (Addr)siginfo->_sifields._sigfault._addr;
  if (sigNo == VKI_SIGILL && siginfo->si_code > 0)
    faultaddr = tst->arch.vex.guest_pc;


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
   *   r0 = signal number
   *   r1 = 0 (should be cause)
   *   r2 = pointer to ucontext
   *
   * r54 points to the struct rt_sigframe.
   */

  tst->arch.vex.guest_r0 = siginfo->si_signo;
  tst->arch.vex.guest_r1 = (Addr) &frame->rs_info;
  tst->arch.vex.guest_r2 = (Addr) &frame->rs_uc;
  tst->arch.vex.guest_r54 = (Addr) frame;

  if (flags & VKI_SA_RESTORER)
  {
    tst->arch.vex.guest_r55 = (Addr) restorer;
  }
  else
  {
    tst->arch.vex.guest_r55 = (Addr)&VG_(tilegx_linux_SUBST_FOR_rt_sigreturn);
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
  tst->arch.vex.guest_pc = (Addr) handler;
  /* This thread needs to be marked runnable, but we leave that the
     caller to do. */
  if (0)
    VG_(printf)("pushed signal frame; sp now = %lx, "
                "next %pc = %lx, status=%d\n",
                (Addr)frame, tst->arch.vex.guest_pc, (Int)tst->status);
}

/* EXPORTED */
void VG_(sigframe_destroy)( ThreadId tid, Bool isRT )
{
  ThreadState *tst;
  struct vg_sig_private *priv1;
  Addr sp;
  UInt frame_size;
  struct vki_sigcontext *mc;
  Int sigNo;
  Bool has_siginfo = isRT;

  vg_assert(VG_(is_valid_tid)(tid));
  tst = VG_(get_ThreadState)(tid);
  sp   = tst->arch.vex.guest_r54 + 8;
  if (has_siginfo)
  {
    struct rt_sigframe *frame = (struct rt_sigframe *)sp;
    struct vki_ucontext *ucp = &frame->rs_uc;

    if (0)
      VG_(printf)("destroy signal frame; sp = %lx, "
                  " %pc = %lx, status=%d\n",
                  (Addr)frame, tst->arch.vex.guest_pc, (Int)tst->status);

    frame_size = sizeof(*frame);
    mc = &ucp->uc_mcontext;
    priv1 = &frame->priv;
    vg_assert(priv1->magicPI == 0x31415927);
    sigNo = priv1->sigNo_private;
  }
  else
  {
    vg_assert(0);
  }

  //restore regs
  tst->arch.vex.guest_r0  = mc->gregs[0];
  tst->arch.vex.guest_r1  = mc->gregs[1];
  tst->arch.vex.guest_r2  = mc->gregs[2];
  tst->arch.vex.guest_r3  = mc->gregs[3];
  tst->arch.vex.guest_r4  = mc->gregs[4];
  tst->arch.vex.guest_r5  = mc->gregs[5];
  tst->arch.vex.guest_r6  = mc->gregs[6];
  tst->arch.vex.guest_r7  = mc->gregs[7];
  tst->arch.vex.guest_r8  = mc->gregs[8];
  tst->arch.vex.guest_r9  = mc->gregs[9];
  tst->arch.vex.guest_r10 = mc->gregs[10];
  tst->arch.vex.guest_r11 = mc->gregs[11];
  tst->arch.vex.guest_r12 = mc->gregs[12];
  tst->arch.vex.guest_r13 = mc->gregs[13];
  tst->arch.vex.guest_r14 = mc->gregs[14];
  tst->arch.vex.guest_r15 = mc->gregs[15];
  tst->arch.vex.guest_r16 = mc->gregs[16];
  tst->arch.vex.guest_r17 = mc->gregs[17];
  tst->arch.vex.guest_r18 = mc->gregs[18];
  tst->arch.vex.guest_r19 = mc->gregs[19];
  tst->arch.vex.guest_r20 = mc->gregs[20];
  tst->arch.vex.guest_r21 = mc->gregs[21];
  tst->arch.vex.guest_r22 = mc->gregs[22];
  tst->arch.vex.guest_r23 = mc->gregs[23];
  tst->arch.vex.guest_r24 = mc->gregs[24];
  tst->arch.vex.guest_r25 = mc->gregs[25];
  tst->arch.vex.guest_r26 = mc->gregs[26];
  tst->arch.vex.guest_r27 = mc->gregs[27];
  tst->arch.vex.guest_r28 = mc->gregs[28];
  tst->arch.vex.guest_r29 = mc->gregs[29];
  tst->arch.vex.guest_r30 = mc->gregs[30];
  tst->arch.vex.guest_r31 = mc->gregs[31];
  tst->arch.vex.guest_r32 = mc->gregs[32];
  tst->arch.vex.guest_r33 = mc->gregs[33];
  tst->arch.vex.guest_r34 = mc->gregs[34];
  tst->arch.vex.guest_r35 = mc->gregs[35];
  tst->arch.vex.guest_r36 = mc->gregs[36];
  tst->arch.vex.guest_r37 = mc->gregs[37];
  tst->arch.vex.guest_r38 = mc->gregs[38];
  tst->arch.vex.guest_r39 = mc->gregs[39];
  tst->arch.vex.guest_r40 = mc->gregs[40];
  tst->arch.vex.guest_r41 = mc->gregs[41];
  tst->arch.vex.guest_r42 = mc->gregs[42];
  tst->arch.vex.guest_r43 = mc->gregs[43];
  tst->arch.vex.guest_r44 = mc->gregs[44];
  tst->arch.vex.guest_r45 = mc->gregs[45];
  tst->arch.vex.guest_r46 = mc->gregs[46];
  tst->arch.vex.guest_r47 = mc->gregs[47];
  tst->arch.vex.guest_r48 = mc->gregs[48];
  tst->arch.vex.guest_r49 = mc->gregs[49];
  tst->arch.vex.guest_r50 = mc->gregs[50];
  tst->arch.vex.guest_r51 = mc->gregs[51];
  tst->arch.vex.guest_r52 = mc->gregs[52];
  tst->arch.vex.guest_r53 = mc->tp;
  tst->arch.vex.guest_r54 = mc->sp;
  tst->arch.vex.guest_r55 = mc->lr;
  tst->arch.vex.guest_pc  = mc->pc;

  VG_TRACK(die_mem_stack_signal, sp, frame_size);
  if (VG_(clo_trace_signals))
    VG_(message)( Vg_DebugMsg,
                  "VG_(signal_return) (thread %u): isRT=%d valid magic; EIP=%#x\n",
                  tid, isRT, tst->arch.vex.guest_pc);
  /* tell the tools */
  VG_TRACK( post_deliver_signal, tid, sigNo );
}

#endif // defined(VGP_tilegx_linux)

/*--------------------------------------------------------------------*/
/*--- end                                  sigframe-tilegx-linux.c ---*/
/*--------------------------------------------------------------------*/
