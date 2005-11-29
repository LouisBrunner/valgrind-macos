
/*--------------------------------------------------------------------*/
/*--- Create/destroy signal delivery frames.                       ---*/
/*---                                       sigframe-ppc64-linux.c ---*/
/*--------------------------------------------------------------------*/

/*
   This file is part of Valgrind, a dynamic binary instrumentation
   framework.

   Copyright (C) 2000-2005 Nicholas Nethercote
      njn@valgrind.org
   Copyright (C) 2004-2005 Paul Mackerras
      paulus@samba.org

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

#include "pub_core_basics.h"
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
#include "pub_core_transtab.h"      // VG_(discard_translations)
#include "vki_unistd-ppc64-linux.h" // __NR_rt_sigreturn


/* This module creates and removes signal frames for signal deliveries
   on ppc64-linux.

   Note, this file contains kernel-specific knowledge in the form of
   'struct sigframe' and 'struct rt_sigframe'.  How does that relate
   to the vki kernel interface stuff?

   Either a 'struct sigframe' or a 'struct rtsigframe' is pushed 
   onto the client's stack.  This contains a subsidiary
   vki_ucontext.  That holds the vcpu's state across the signal, 
   so that the sighandler can mess with the vcpu state if it
   really wants.

   FIXME: sigcontexting is basically broken for the moment.  When
   delivering a signal, the integer registers and %eflags are
   correctly written into the sigcontext, however the FP and SSE state
   is not.  When returning from a signal, only the integer registers
   are restored from the sigcontext; the rest of the CPU state is
   restored to what it was before the signal.

   This will be fixed.
*/


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

/* Structure containing bits of information that we want to save
   on signal delivery. */
//zz struct vg_sig_private {
//zz    UInt magicPI;
//zz    UInt sigNo_private;
//zz    VexGuestPPC32State shadow;
//zz };
//zz 
//zz /* Structure put on stack for signal handlers with SA_SIGINFO clear. */
//zz struct nonrt_sigframe {
//zz    UInt gap1[16];
//zz    struct vki_sigcontext sigcontext;
//zz    struct vki_mcontext mcontext;
//zz    struct vg_sig_private priv;
//zz    unsigned char abigap[224];
//zz };
//zz 
//zz /* Structure put on stack for signal handlers with SA_SIGINFO set. */
//zz struct rt_sigframe {
//zz    UInt gap1[20];
//zz    vki_siginfo_t siginfo;
//zz    struct vki_ucontext ucontext;
//zz    struct vg_sig_private priv;
//zz    unsigned char abigap[224];
//zz };

#define SET_SIGNAL_LR(zztst, zzval)                          \
   do { tst->arch.vex.guest_LR = (zzval);                    \
      VG_TRACK( post_reg_write, Vg_CoreSignal, tst->tid,     \
                offsetof(VexGuestPPC32State,guest_LR),       \
                sizeof(UWord) );                             \
   } while (0)

#define SET_SIGNAL_GPR(zztst, zzn, zzval)                    \
   do { tst->arch.vex.guest_GPR##zzn = (zzval);              \
      VG_TRACK( post_reg_write, Vg_CoreSignal, tst->tid,     \
                offsetof(VexGuestPPC32State,guest_GPR##zzn), \
                sizeof(UWord) );                             \
   } while (0)


static 
void stack_mcontext ( struct vki_mcontext *mc, 
                      ThreadState* tst, 
                      Int ret,
                      UInt fault_addr )
{
//   VG_TRACK( pre_mem_write, Vg_CoreSignal, tst->tid, "signal frame mcontext",
//             (Addr)mc, sizeof(struct vki_pt_regs) );
//
//#  define DO(gpr)  mc->mc_gregs[VKI_PT_R0+gpr] = tst->arch.vex.guest_GPR##gpr
//   DO(0);  DO(1);  DO(2);  DO(3);  DO(4);  DO(5);  DO(6);  DO(7);
//   DO(8);  DO(9);  DO(10); DO(11); DO(12); DO(13); DO(14); DO(15);
//   DO(16); DO(17); DO(18); DO(19); DO(20); DO(21); DO(22); DO(23);
//   DO(24); DO(25); DO(26); DO(27); DO(28); DO(29); DO(30); DO(31);
//#  undef DO
//
//   mc->mc_gregs[VKI_PT_NIP]     = tst->arch.vex.guest_CIA;
//   mc->mc_gregs[VKI_PT_MSR]     = 0xf032;   /* pretty arbitrary */
//   mc->mc_gregs[VKI_PT_ORIG_R3] = tst->arch.vex.guest_GPR3;
//   mc->mc_gregs[VKI_PT_CTR]     = tst->arch.vex.guest_CTR;
//   mc->mc_gregs[VKI_PT_LNK]     = tst->arch.vex.guest_LR;
//   mc->mc_gregs[VKI_PT_XER]     = LibVEX_GuestPPC32_get_XER(&tst->arch.vex);
//   mc->mc_gregs[VKI_PT_CCR]     = LibVEX_GuestPPC32_get_CR(&tst->arch.vex);
//   mc->mc_gregs[VKI_PT_MQ]      = 0;
//   mc->mc_gregs[VKI_PT_TRAP]    = 0;
//   mc->mc_gregs[VKI_PT_DAR]     = fault_addr;
//   mc->mc_gregs[VKI_PT_DSISR]   = 0;
//   mc->mc_gregs[VKI_PT_RESULT]  = 0;
//   VG_TRACK( post_mem_write, Vg_CoreSignal, tst->tid, 
//             (Addr)mc, sizeof(struct vki_pt_regs) );
//
//   /* XXX should do FP and vector regs */
//
//   /* set up signal return trampoline */
//   VG_TRACK(pre_mem_write, Vg_CoreSignal, tst->tid, "signal frame mcontext",
//            (Addr)&mc->mc_pad, sizeof(mc->mc_pad));
//   mc->mc_pad[0] = 0x38000000U + ret;   /* li 0,ret */
//   mc->mc_pad[1] = 0x44000002U;         /* sc */
//   VG_TRACK( post_mem_write,  Vg_CoreSignal, tst->tid, 
//             (Addr)&mc->mc_pad, sizeof(mc->mc_pad) );
//   /* invalidate any translation of this area */
//   VG_(discard_translations)( (Addr64)(Addr)&mc->mc_pad, 
//                              sizeof(mc->mc_pad), "stack_mcontext" );   
//
//   /* set the signal handler to return to the trampoline */
//   SET_SIGNAL_LR(tst, (Addr) &mc->mc_pad[0]);
}


/* Extend the stack segment downwards if needed so as to ensure the
   new signal frames are mapped to something.  Return a Bool
   indicating whether or not the operation was successful.
*/
static Bool extend ( ThreadState *tst, Addr addr, SizeT size )
{
   I_die_here;
//   ThreadId tid = tst->tid;
//   NSegment *stackseg = NULL;
//
//   if (VG_(extend_stack)(addr, tst->client_stack_szB)) {
//      stackseg = VG_(am_find_nsegment)(addr);
//      if (0 && stackseg)
//	 VG_(printf)("frame=%p seg=%p-%p\n",
//		     addr, stackseg->start, stackseg->end);
//   }
//
//   if (stackseg == NULL || !stackseg->hasR || !stackseg->hasW) {
//      VG_(message)(
//         Vg_UserMsg,
//         "Can't extend stack to %p during signal delivery for thread %d:",
//         addr, tid);
//      if (stackseg == NULL)
//         VG_(message)(Vg_UserMsg, "  no stack segment");
//      else
//         VG_(message)(Vg_UserMsg, "  too small or bad protection modes");
//
//      /* set SIGSEGV to default handler */
//      VG_(set_default_handler)(VKI_SIGSEGV);
//      VG_(synth_fault_mapping)(tid, addr);
//
//      /* The whole process should be about to die, since the default
//	 action of SIGSEGV to kill the whole process. */
//      return False;
//   }
//
//   /* For tracking memory events, indicate the entire frame has been
//      allocated. */
//   VG_TRACK( new_mem_stack_signal, addr - VG_STACK_REDZONE_SZB,
//             size + VG_STACK_REDZONE_SZB );
//
//   return True;
}


/* EXPORTED */
void VG_(sigframe_create)( ThreadId tid, 
                           Addr sp_top_of_frame,
                           const vki_siginfo_t *siginfo,
                           void *handler, 
                           UInt flags,
                           const vki_sigset_t *mask,
		           void *restorer )
{
   I_die_here;
//   struct vg_sig_private *priv;
//   Addr sp;
//   ThreadState *tst;
//   Int sigNo = siginfo->si_signo;
//   Addr faultaddr;
//
//   /* Stack must be 16-byte aligned */
//   sp_top_of_frame &= ~0xf;
//
//   if (flags & VKI_SA_SIGINFO) {
//      sp = sp_top_of_frame - sizeof(struct rt_sigframe);
//   } else {
//      sp = sp_top_of_frame - sizeof(struct nonrt_sigframe);
//   }
//
//   tst = VG_(get_ThreadState)(tid);
//
//   if (!extend(tst, sp, sp_top_of_frame - sp))
//      return;
//
//   vg_assert(VG_IS_16_ALIGNED(sp));
//
//   /* Set up the stack chain pointer */
//   VG_TRACK( pre_mem_write, Vg_CoreSignal, tid, "signal handler frame",
//             sp, sizeof(UWord) );
//   *(Addr *)sp = tst->arch.vex.guest_GPR1;
//   VG_TRACK( post_mem_write, Vg_CoreSignal, tid, 
//             sp, sizeof(UWord) );
//
//   faultaddr = (Addr)siginfo->_sifields._sigfault._addr;
//   if (sigNo == VKI_SIGILL && siginfo->si_code > 0)
//      faultaddr = tst->arch.vex.guest_CIA;
//
//   if (flags & VKI_SA_SIGINFO) {
//      struct rt_sigframe *frame = (struct rt_sigframe *) sp;
//      struct vki_ucontext *ucp = &frame->ucontext;
//
//      VG_TRACK( pre_mem_write, Vg_CoreSignal, tid, "signal frame siginfo",
//                (Addr)&frame->siginfo, sizeof(frame->siginfo) );
//      VG_(memcpy)(&frame->siginfo, siginfo, sizeof(*siginfo));
//      VG_TRACK( post_mem_write, Vg_CoreSignal, tid, 
//                (Addr)&frame->siginfo, sizeof(frame->siginfo) );
//
//      VG_TRACK( pre_mem_write, Vg_CoreSignal, tid, "signal frame ucontext",
//                (Addr)ucp, offsetof(struct vki_ucontext, uc_pad) );
//      ucp->uc_flags = 0;
//      ucp->uc_link = 0;
//      ucp->uc_stack = tst->altstack;
//      VG_TRACK( post_mem_write, Vg_CoreSignal, tid, (Addr)ucp,
//                offsetof(struct vki_ucontext, uc_pad) );
//
//      VG_TRACK( pre_mem_write, Vg_CoreSignal, tid, "signal frame ucontext",
//                (Addr)&ucp->uc_regs,
//                sizeof(ucp->uc_regs) + sizeof(ucp->uc_sigmask) );
//      ucp->uc_regs = &ucp->uc_mcontext;
//      ucp->uc_sigmask = tst->sig_mask;
//      VG_TRACK( post_mem_write, Vg_CoreSignal, tid, 
//                (Addr)&ucp->uc_regs,
//                sizeof(ucp->uc_regs) + sizeof(ucp->uc_sigmask) );
//
//      stack_mcontext(&ucp->uc_mcontext, tst, __NR_rt_sigreturn, faultaddr);
//      priv = &frame->priv;
//
//      SET_SIGNAL_GPR(tid, 4, (Addr) &frame->siginfo);
//      SET_SIGNAL_GPR(tid, 5, (Addr) ucp);
//      /* the kernel sets this, though it doesn't seem to be in the ABI */
//      SET_SIGNAL_GPR(tid, 6, (Addr) &frame->siginfo);
//
//   } else {
//      /* non-RT signal delivery */
//      struct nonrt_sigframe *frame = (struct nonrt_sigframe *) sp;
//      struct vki_sigcontext *scp = &frame->sigcontext;
//
//      VG_TRACK( pre_mem_write, Vg_CoreSignal, tid, "signal frame sigcontext",
//                (Addr)&scp->_unused[3], sizeof(*scp) - 3 * sizeof(UInt) );
//      scp->signal = sigNo;
//      scp->handler = (Addr) handler;
//      scp->oldmask = tst->sig_mask.sig[0];
//      scp->_unused[3] = tst->sig_mask.sig[1];
//      VG_TRACK( post_mem_write, Vg_CoreSignal, tid,
//                (Addr)&scp->_unused[3], sizeof(*scp) - 3 * sizeof(UInt) );
//
//      stack_mcontext(&frame->mcontext, tst, __NR_sigreturn, faultaddr);
//      priv = &frame->priv;
//
//      SET_SIGNAL_GPR(tid, 4, (Addr) scp);
//   }
//
//   priv->magicPI       = 0x31415927;
//   priv->sigNo_private = sigNo;
//   priv->shadow        = tst->arch.vex_shadow;
//
//   SET_SIGNAL_GPR(tid, 1, sp);
//   SET_SIGNAL_GPR(tid, 3, sigNo);
//   tst->arch.vex.guest_CIA = (Addr) handler;
//
//   if (0)
//      VG_(printf)("pushed signal frame; %R1 now = %p, "
//                  "next %%CIA = %p, status=%d\n", 
//		  sp, tst->arch.vex.guest_CIA, tst->status);
}


/*------------------------------------------------------------*/
/*--- Destroying signal frames                             ---*/
/*------------------------------------------------------------*/

/* EXPORTED */
void VG_(sigframe_destroy)( ThreadId tid, Bool isRT )
{
   I_die_here;
//   ThreadState *tst;
//   struct vg_sig_private *priv;
//   Addr sp;
//   UInt frame_size;
//   struct vki_mcontext *mc;
//   Int sigNo;
//   Bool has_siginfo = isRT;
//
//   vg_assert(VG_(is_valid_tid)(tid));
//   tst = VG_(get_ThreadState)(tid);
//
//   /* Check that the stack frame looks valid */
//   sp = tst->arch.vex.guest_GPR1;
//   vg_assert(VG_IS_16_ALIGNED(sp));
//   /* JRS 17 Nov 05: This code used to check that *sp -- which should
//      have been set by the stwu at the start of the handler -- points
//      to just above the frame (ie, the previous frame).  However, that
//      isn't valid when delivering signals on alt stacks.  So I removed
//      it.  The frame is still sanity-checked using the priv->magicPI
//      field. */
//
//   if (has_siginfo) {
//      struct rt_sigframe *frame = (struct rt_sigframe *)sp;
//      frame_size = sizeof(*frame);
//      mc = &frame->ucontext.uc_mcontext;
//      priv = &frame->priv;
//      vg_assert(priv->magicPI == 0x31415927);
//      tst->sig_mask = frame->ucontext.uc_sigmask;
//   } else {
//      struct nonrt_sigframe *frame = (struct nonrt_sigframe *)sp;
//      frame_size = sizeof(*frame);
//      mc = &frame->mcontext;
//      priv = &frame->priv;
//      vg_assert(priv->magicPI == 0x31415927);
//      tst->sig_mask.sig[0] = frame->sigcontext.oldmask;
//      tst->sig_mask.sig[1] = frame->sigcontext._unused[3];
//   }
//   tst->tmp_sig_mask = tst->sig_mask;
//
//   sigNo = priv->sigNo_private;
//
//#  define DO(gpr)  tst->arch.vex.guest_GPR##gpr = mc->mc_gregs[VKI_PT_R0+gpr]
//   DO(0);  DO(1);  DO(2);  DO(3);  DO(4);  DO(5);  DO(6);  DO(7);
//   DO(8);  DO(9);  DO(10); DO(11); DO(12); DO(13); DO(14); DO(15);
//   DO(16); DO(17); DO(18); DO(19); DO(20); DO(21); DO(22); DO(23);
//   DO(24); DO(25); DO(26); DO(27); DO(28); DO(29); DO(30); DO(31);
//#  undef DO
//
//   tst->arch.vex.guest_CIA = mc->mc_gregs[VKI_PT_NIP];
//
//   // Umm ... ? (jrs 2005 July 8)
//   // tst->arch.m_orig_gpr3 = mc->mc_gregs[VKI_PT_ORIG_R3];
//
//   LibVEX_GuestPPC32_put_CR( mc->mc_gregs[VKI_PT_CCR], &tst->arch.vex );
//
//   tst->arch.vex.guest_LR  = mc->mc_gregs[VKI_PT_LNK];
//   tst->arch.vex.guest_CTR = mc->mc_gregs[VKI_PT_CTR];
//   LibVEX_GuestPPC32_put_XER( mc->mc_gregs[VKI_PT_XER], &tst->arch.vex );
//
//   tst->arch.vex_shadow = priv->shadow;
//
//   VG_TRACK(die_mem_stack_signal, sp, frame_size);
//
//   if (VG_(clo_trace_signals))
//      VG_(message)(Vg_DebugMsg,
//                   "vg_pop_signal_frame (thread %d): isRT=%d valid magic; EIP=%p",
//                   tid, has_siginfo, tst->arch.vex.guest_CIA);
//
//   /* tell the tools */
//   VG_TRACK( post_deliver_signal, tid, sigNo );
}

/*--------------------------------------------------------------------*/
/*--- end                                   sigframe-ppc64-linux.c ---*/
/*--------------------------------------------------------------------*/
