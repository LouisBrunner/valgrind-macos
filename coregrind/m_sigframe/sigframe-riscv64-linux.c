
/*--------------------------------------------------------------------*/
/*--- Create/destroy signal delivery frames.                       ---*/
/*---                                     sigframe-riscv64-linux.c ---*/
/*--------------------------------------------------------------------*/

/*
   This file is part of Valgrind, a dynamic binary instrumentation
   framework.

   Copyright (C) 2020-2023 Petr Pavlu
      petr.pavlu@dagobah.cz

   This program is free software; you can redistribute it and/or
   modify it under the terms of the GNU General Public License as
   published by the Free Software Foundation; either version 3 of the
   License, or (at your option) any later version.

   This program is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, see <http://www.gnu.org/licenses/>.

   The GNU General Public License is contained in the file COPYING.
*/

#if defined(VGP_riscv64_linux)

#include "libvex_guest_offsets.h"
#include "priv_sigframe.h"
#include "pub_core_aspacemgr.h"
#include "pub_core_basics.h"
#include "pub_core_libcassert.h"
#include "pub_core_libcbase.h"
#include "pub_core_libcprint.h"
#include "pub_core_machine.h"
#include "pub_core_options.h"
#include "pub_core_sigframe.h"
#include "pub_core_signals.h"
#include "pub_core_threadstate.h"
#include "pub_core_tooliface.h"
#include "pub_core_trampoline.h"
#include "pub_core_vki.h"

/*------------------------------------------------------------*/
/*--- Signal frame layout                                  ---*/
/*------------------------------------------------------------*/

/* Valgrind-specific parts of the signal frame. */
struct vg_sigframe {
   /* Sanity check word. */
   UInt magicPI;

   /* Safely-saved version of sigNo. */
   Int sigNo_private;

   /* Sanity check word. */
   UInt magicE;
};

/* Complete signal frame. */
struct rt_sigframe {
   struct vki_siginfo  info;
   struct vki_ucontext uc;
   struct vg_sigframe  vg;
};

/*------------------------------------------------------------*/
/*--- Creating a signal frame                              ---*/
/*------------------------------------------------------------*/

static void synth_ucontext(ThreadState*         tst,
                           const vki_siginfo_t* si,
                           const vki_sigset_t*  set,
                           struct vki_ucontext* uc)
{
   VG_(memset)(uc, 0, sizeof(*uc));

   /* Prepare common data. */
   uc->uc_flags = 0;
   VG_TRACK(post_mem_write, Vg_CoreSignal, tst->tid, (Addr)&uc->uc_flags,
            sizeof(uc->uc_flags));
   uc->uc_link = 0;
   VG_TRACK(post_mem_write, Vg_CoreSignal, tst->tid, (Addr)&uc->uc_link,
            sizeof(uc->uc_link));
   uc->uc_sigmask = *set;
   VG_TRACK(post_mem_write, Vg_CoreSignal, tst->tid, (Addr)&uc->uc_sigmask,
            sizeof(uc->uc_sigmask));
   uc->uc_stack = tst->altstack;
   VG_TRACK(post_mem_write, Vg_CoreSignal, tst->tid, (Addr)&uc->uc_stack,
            sizeof(uc->uc_stack));

   struct vki_sigcontext* sc = &uc->uc_mcontext;

   /* Save integer registers. */
#define IREG_TO_CTX(ureg, vreg)                                                \
   sc->sc_regs.ureg = tst->arch.vex.guest_##vreg;                              \
   VG_TRACK(copy_reg_to_mem, Vg_CoreSignal, tst->tid, OFFSET_riscv64_##vreg,   \
            (Addr)&sc->sc_regs.ureg, sizeof(UWord));
   IREG_TO_CTX(pc, pc);
   IREG_TO_CTX(ra, x1);
   IREG_TO_CTX(sp, x2);
   IREG_TO_CTX(gp, x3);
   IREG_TO_CTX(tp, x4);
   IREG_TO_CTX(t0, x5);
   IREG_TO_CTX(t1, x6);
   IREG_TO_CTX(t2, x7);
   IREG_TO_CTX(s0, x8);
   IREG_TO_CTX(s1, x9);
   IREG_TO_CTX(a0, x10);
   IREG_TO_CTX(a1, x11);
   IREG_TO_CTX(a2, x12);
   IREG_TO_CTX(a3, x13);
   IREG_TO_CTX(a4, x14);
   IREG_TO_CTX(a5, x15);
   IREG_TO_CTX(a6, x16);
   IREG_TO_CTX(a7, x17);
   IREG_TO_CTX(s2, x18);
   IREG_TO_CTX(s3, x19);
   IREG_TO_CTX(s4, x20);
   IREG_TO_CTX(s5, x21);
   IREG_TO_CTX(s6, x22);
   IREG_TO_CTX(s7, x23);
   IREG_TO_CTX(s8, x24);
   IREG_TO_CTX(s9, x25);
   IREG_TO_CTX(s10, x26);
   IREG_TO_CTX(s11, x27);
   IREG_TO_CTX(t3, x28);
   IREG_TO_CTX(t4, x29);
   IREG_TO_CTX(t5, x30);
   IREG_TO_CTX(t6, x31);
#undef IREG_TO_CTX

   /* Save floating point registers. */
#define FREG_TO_CTX(ureg, vreg, type)                                          \
   sc->sc_fpregs.d.ureg = tst->arch.vex.guest_##vreg;                          \
   VG_TRACK(copy_reg_to_mem, Vg_CoreSignal, tst->tid, OFFSET_riscv64_##vreg,   \
            (Addr)&sc->sc_fpregs.d.ureg, sizeof(type));
   FREG_TO_CTX(f[0], f0, UWord);
   FREG_TO_CTX(f[1], f1, UWord);
   FREG_TO_CTX(f[2], f2, UWord);
   FREG_TO_CTX(f[3], f3, UWord);
   FREG_TO_CTX(f[4], f4, UWord);
   FREG_TO_CTX(f[5], f5, UWord);
   FREG_TO_CTX(f[6], f6, UWord);
   FREG_TO_CTX(f[7], f7, UWord);
   FREG_TO_CTX(f[8], f8, UWord);
   FREG_TO_CTX(f[9], f9, UWord);
   FREG_TO_CTX(f[10], f10, UWord);
   FREG_TO_CTX(f[11], f11, UWord);
   FREG_TO_CTX(f[12], f12, UWord);
   FREG_TO_CTX(f[13], f13, UWord);
   FREG_TO_CTX(f[14], f14, UWord);
   FREG_TO_CTX(f[15], f15, UWord);
   FREG_TO_CTX(f[16], f16, UWord);
   FREG_TO_CTX(f[17], f17, UWord);
   FREG_TO_CTX(f[18], f18, UWord);
   FREG_TO_CTX(f[19], f19, UWord);
   FREG_TO_CTX(f[20], f20, UWord);
   FREG_TO_CTX(f[21], f21, UWord);
   FREG_TO_CTX(f[22], f22, UWord);
   FREG_TO_CTX(f[23], f23, UWord);
   FREG_TO_CTX(f[24], f24, UWord);
   FREG_TO_CTX(f[25], f25, UWord);
   FREG_TO_CTX(f[26], f26, UWord);
   FREG_TO_CTX(f[27], f27, UWord);
   FREG_TO_CTX(f[28], f28, UWord);
   FREG_TO_CTX(f[29], f29, UWord);
   FREG_TO_CTX(f[30], f30, UWord);
   FREG_TO_CTX(f[31], f31, UWord);
   FREG_TO_CTX(fcsr, fcsr, UInt);
#undef FREG_TO_CTX
}

/* Build the Valgrind-specific part of a signal frame. */
static void build_vg_sigframe(struct vg_sigframe* frame, Int sigNo)
{
   frame->magicPI       = 0x31415927;
   frame->sigNo_private = sigNo;
   frame->magicE        = 0x27182818;
}

static Addr build_rt_sigframe(ThreadState*         tst,
                              Addr                 sp_top_of_frame,
                              const vki_siginfo_t* siginfo,
                              UInt                 flags,
                              const vki_sigset_t*  mask)
{
   SizeT size = sizeof(struct rt_sigframe);
   Addr  sp   = VG_ROUNDDN(sp_top_of_frame - size, 16);

   if (!ML_(sf_maybe_extend_stack)(tst, sp, size, flags))
      return sp_top_of_frame;

   /* Tell the tools that the sigframe is to be written. */
   VG_TRACK(pre_mem_write, Vg_CoreSignal, tst->tid, "signal handler frame", sp,
            sizeof(struct rt_sigframe));

   struct rt_sigframe* frame = (struct rt_sigframe*)sp;

   /* Fill in the siginfo. */
   frame->info = *siginfo;

   /* SIGILL defines addr to be the faulting address. */
   Int sigNo = siginfo->si_signo;
   if (sigNo == VKI_SIGILL && siginfo->si_code > 0)
      frame->info._sifields._sigfault._addr = (void*)VG_(get_IP)(tst->tid);

   VG_TRACK(post_mem_write, Vg_CoreSignal, tst->tid, (Addr)&frame->info,
            sizeof(frame->info));

   /* Fill in the ucontext. */
   synth_ucontext(tst, siginfo, mask, &frame->uc);

   /* Fill in the Valgrind-specific part. */
   build_vg_sigframe(&frame->vg, sigNo);

   return sp;
}

void VG_(sigframe_create)(ThreadId                   tid,
                          Bool                       on_altstack,
                          Addr                       rsp_top_of_frame,
                          const vki_siginfo_t*       siginfo,
                          const struct vki_ucontext* siguc,
                          void*                      handler,
                          UInt                       flags,
                          const vki_sigset_t*        mask,
                          void*                      restorer)
{
   /* The restorer functionality (SA_RESTORER) is not used on riscv64-linux. */
   vg_assert(restorer == NULL);

   ThreadState* tst = VG_(get_ThreadState)(tid);

   /* Build the signal frame on the stack. */
   Addr sp = build_rt_sigframe(tst, rsp_top_of_frame, siginfo, flags, mask);
   struct rt_sigframe* frame = (struct rt_sigframe*)sp;

   /* Configure guest registers for the signal delivery. */
   VG_(set_SP)(tid, sp);
   VG_TRACK(post_reg_write, Vg_CoreSignal, tid, VG_O_STACK_PTR, sizeof(UWord));

   tst->arch.vex.guest_x10 = siginfo->si_signo;
   VG_TRACK(post_reg_write, Vg_CoreSignal, tst->tid, OFFSET_riscv64_x10,
            sizeof(UWord));
   tst->arch.vex.guest_x11 = (Addr)&frame->info;
   VG_TRACK(post_reg_write, Vg_CoreSignal, tst->tid, OFFSET_riscv64_x11,
            sizeof(UWord));
   tst->arch.vex.guest_x12 = (Addr)&frame->uc;
   VG_TRACK(post_reg_write, Vg_CoreSignal, tst->tid, OFFSET_riscv64_x12,
            sizeof(UWord));

   tst->arch.vex.guest_x1 = (Addr)&VG_(riscv64_linux_SUBST_FOR_rt_sigreturn);
   VG_TRACK(post_reg_write, Vg_CoreSignal, tst->tid, OFFSET_riscv64_x1,
            sizeof(UWord));

   /* Set up the program counter. Note that it is not necessary to inform the
      tools about this write because pc is always defined. */
   VG_(set_IP)(tid, (Addr)handler);

   if (VG_(clo_trace_signals))
      VG_(message)(Vg_DebugMsg,
                   "sigframe_create (thread %u): next pc=%#lx, next sp=%#lx\n",
                   tid, (Addr)handler, sp);
}

/*------------------------------------------------------------*/
/*--- Destroying a signal frame                            ---*/
/*------------------------------------------------------------*/

/* Restore the Valgrind-specific part of a signal frame. The returned value
   indicates whether the frame is valid. If not then nothing is restored and the
   client is set to take a segfault. */
static Bool
restore_vg_sigframe(ThreadState* tst, struct vg_sigframe* frame, Int* sigNo)
{
   if (frame->magicPI != 0x31415927 || frame->magicE != 0x27182818) {
      VG_(message)(
         Vg_UserMsg,
         "Thread %u return signal frame corrupted. Killing process.\n",
         tst->tid);
      VG_(set_default_handler)(VKI_SIGSEGV);
      VG_(synth_fault)(tst->tid);
      *sigNo = VKI_SIGSEGV;
      return False;
   }
   *sigNo = frame->sigNo_private;
   return True;
}

static void restore_ucontext(ThreadState* tst, struct vki_ucontext* uc)
{
   /* Restore common data. */
   VG_TRACK(pre_mem_read, Vg_CoreSignal, tst->tid, "signal frame mask",
            (Addr)&uc->uc_sigmask, sizeof(uc->uc_sigmask));
   tst->sig_mask     = uc->uc_sigmask;
   tst->tmp_sig_mask = tst->sig_mask;

   struct vki_sigcontext* sc = &uc->uc_mcontext;

   /* Restore integer registers. */
#define IREG_FROM_CTX(ureg, vreg)                                              \
   tst->arch.vex.guest_##vreg = sc->sc_regs.ureg;                              \
   VG_TRACK(copy_mem_to_reg, Vg_CoreSignal, tst->tid, (Addr)&sc->sc_regs.ureg, \
            OFFSET_riscv64_##vreg, sizeof(UWord));
   IREG_FROM_CTX(pc, pc);
   IREG_FROM_CTX(ra, x1);
   IREG_FROM_CTX(sp, x2);
   IREG_FROM_CTX(gp, x3);
   IREG_FROM_CTX(tp, x4);
   IREG_FROM_CTX(t0, x5);
   IREG_FROM_CTX(t1, x6);
   IREG_FROM_CTX(t2, x7);
   IREG_FROM_CTX(s0, x8);
   IREG_FROM_CTX(s1, x9);
   IREG_FROM_CTX(a0, x10);
   IREG_FROM_CTX(a1, x11);
   IREG_FROM_CTX(a2, x12);
   IREG_FROM_CTX(a3, x13);
   IREG_FROM_CTX(a4, x14);
   IREG_FROM_CTX(a5, x15);
   IREG_FROM_CTX(a6, x16);
   IREG_FROM_CTX(a7, x17);
   IREG_FROM_CTX(s2, x18);
   IREG_FROM_CTX(s3, x19);
   IREG_FROM_CTX(s4, x20);
   IREG_FROM_CTX(s5, x21);
   IREG_FROM_CTX(s6, x22);
   IREG_FROM_CTX(s7, x23);
   IREG_FROM_CTX(s8, x24);
   IREG_FROM_CTX(s9, x25);
   IREG_FROM_CTX(s10, x26);
   IREG_FROM_CTX(s11, x27);
   IREG_FROM_CTX(t3, x28);
   IREG_FROM_CTX(t4, x29);
   IREG_FROM_CTX(t5, x30);
   IREG_FROM_CTX(t6, x31);
#undef IREG_FROM_CTX

   /* Restore floating point registers. */
#define FREG_FROM_CTX(ureg, vreg, type)                                        \
   tst->arch.vex.guest_##vreg = sc->sc_fpregs.d.ureg;                          \
   VG_TRACK(copy_mem_to_reg, Vg_CoreSignal, tst->tid,                          \
            (Addr)&sc->sc_fpregs.d.ureg, OFFSET_riscv64_##vreg, sizeof(type));
   FREG_FROM_CTX(f[0], f0, UWord);
   FREG_FROM_CTX(f[1], f1, UWord);
   FREG_FROM_CTX(f[2], f2, UWord);
   FREG_FROM_CTX(f[3], f3, UWord);
   FREG_FROM_CTX(f[4], f4, UWord);
   FREG_FROM_CTX(f[5], f5, UWord);
   FREG_FROM_CTX(f[6], f6, UWord);
   FREG_FROM_CTX(f[7], f7, UWord);
   FREG_FROM_CTX(f[8], f8, UWord);
   FREG_FROM_CTX(f[9], f9, UWord);
   FREG_FROM_CTX(f[10], f10, UWord);
   FREG_FROM_CTX(f[11], f11, UWord);
   FREG_FROM_CTX(f[12], f12, UWord);
   FREG_FROM_CTX(f[13], f13, UWord);
   FREG_FROM_CTX(f[14], f14, UWord);
   FREG_FROM_CTX(f[15], f15, UWord);
   FREG_FROM_CTX(f[16], f16, UWord);
   FREG_FROM_CTX(f[17], f17, UWord);
   FREG_FROM_CTX(f[18], f18, UWord);
   FREG_FROM_CTX(f[19], f19, UWord);
   FREG_FROM_CTX(f[20], f20, UWord);
   FREG_FROM_CTX(f[21], f21, UWord);
   FREG_FROM_CTX(f[22], f22, UWord);
   FREG_FROM_CTX(f[23], f23, UWord);
   FREG_FROM_CTX(f[24], f24, UWord);
   FREG_FROM_CTX(f[25], f25, UWord);
   FREG_FROM_CTX(f[26], f26, UWord);
   FREG_FROM_CTX(f[27], f27, UWord);
   FREG_FROM_CTX(f[28], f28, UWord);
   FREG_FROM_CTX(f[29], f29, UWord);
   FREG_FROM_CTX(f[30], f30, UWord);
   FREG_FROM_CTX(f[31], f31, UWord);
   FREG_FROM_CTX(fcsr, fcsr, UInt);
#undef FREG_FROM_CTX
}

static void
restore_rt_sigframe(ThreadState* tst, struct rt_sigframe* frame, Int* sigNo)
{
   if (restore_vg_sigframe(tst, &frame->vg, sigNo))
      restore_ucontext(tst, &frame->uc);
}

void VG_(sigframe_destroy)(ThreadId tid, Bool isRT)
{
   /* Non-rt sigreturn does not exist on riscv64-linux. */
   vg_assert(isRT);

   ThreadState* tst = VG_(get_ThreadState)(tid);

   /* Correctly reestablish the frame base address. */
   Addr sp = VG_(get_SP)(tid);

   /* Restore a state from the signal frame. */
   Int sigNo;
   restore_rt_sigframe(tst, (struct rt_sigframe*)sp, &sigNo);

   VG_TRACK(die_mem_stack_signal, sp - VG_STACK_REDZONE_SZB,
            sizeof(struct rt_sigframe) + VG_STACK_REDZONE_SZB);

   /* Returning from a signal handler. */
   if (VG_(clo_trace_signals))
      VG_(message)(Vg_DebugMsg, "sigframe_return (thread %u): pc=%#lx\n", tid,
                   VG_(get_IP)(tid));

   /* Tell the tools. */
   VG_TRACK(post_deliver_signal, tid, sigNo);
}

#endif // defined(VGP_riscv64_linux)

/*--------------------------------------------------------------------*/
/*--- end                                 sigframe-riscv64-linux.c ---*/
/*--------------------------------------------------------------------*/
