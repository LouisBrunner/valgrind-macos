
/*--------------------------------------------------------------------*/
/*--- Create/destroy signal delivery frames.                       ---*/
/*---                                       sigframe-ppc64-linux.c ---*/
/*--------------------------------------------------------------------*/

/*
   This file is part of Valgrind, a dynamic binary instrumentation
   framework.

   Copyright (C) 2000-2011 Nicholas Nethercote
      njn@valgrind.org
   Copyright (C) 2004-2011 Paul Mackerras
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

#if defined(VGP_ppc64_linux)

#include "pub_core_basics.h"
#include "pub_core_vki.h"
#include "pub_core_vkiscnums.h"
#include "pub_core_libcsetjmp.h"    // to keep _threadstate.h happy
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

// On ppc64-linux, rt_sigframe is used for all signals.

// In theory, so long as we get the arguments to the handler function
// right, it doesn't matter what the exact layout of the rest of the
// frame is.  Unfortunately, things like gcc's exception unwinding
// make assumptions about the locations of various parts of the frame,
// so we need to duplicate it exactly.

/* Many of these byzantine details derived from
   linux-2.6.13/arch/ppc64/kernel/signal.c */

#define TRAMP_SIZE 6 /* who knows why - it only needs to be 2. */

/* Structure containing bits of information that we want to save
   on signal delivery. */
struct vg_sig_private {
   UInt  magicPI;
   UInt  sigNo_private;
   ULong _unused; /* makes the struct size be zero % 16 */
   VexGuestPPC64State vex_shadow1;
   VexGuestPPC64State vex_shadow2;
};

/* Structure put on stack for all signal handlers. */
struct rt_sigframe {
   struct vki_ucontext   uc;
   ULong                 _unused[2];
   UInt                  tramp[TRAMP_SIZE];
   struct vki_siginfo*   pinfo;
   void*                 puc;
   vki_siginfo_t         info;
   struct vg_sig_private priv;
   UChar                 abigap[288];
};

#define SET_SIGNAL_LR(zztst, zzval)                          \
   do { tst->arch.vex.guest_LR = (zzval);                    \
      VG_TRACK( post_reg_write, Vg_CoreSignal, tst->tid,     \
                offsetof(VexGuestPPC64State,guest_LR),       \
                sizeof(UWord) );                             \
   } while (0)

#define SET_SIGNAL_GPR(zztst, zzn, zzval)                    \
   do { tst->arch.vex.guest_GPR##zzn = (zzval);              \
      VG_TRACK( post_reg_write, Vg_CoreSignal, tst->tid,     \
                offsetof(VexGuestPPC64State,guest_GPR##zzn), \
                sizeof(UWord) );                             \
   } while (0)


/* Extend the stack segment downwards if needed so as to ensure the
   new signal frames are mapped to something.  Return a Bool
   indicating whether or not the operation was successful.
*/
static Bool extend ( ThreadState *tst, Addr addr, SizeT size )
{
   ThreadId        tid = tst->tid;
   NSegment const* stackseg = NULL;

   if (VG_(extend_stack)(addr, tst->client_stack_szB)) {
      stackseg = VG_(am_find_nsegment)(addr);
      if (0 && stackseg)
	 VG_(printf)("frame=%#lx seg=%#lx-%#lx\n",
		     addr, stackseg->start, stackseg->end);
   }

   if (stackseg == NULL || !stackseg->hasR || !stackseg->hasW) {
      VG_(message)(
         Vg_UserMsg,
         "Can't extend stack to %#lx during signal delivery for thread %d:\n",
         addr, tid);
      if (stackseg == NULL)
         VG_(message)(Vg_UserMsg, "  no stack segment\n");
      else
         VG_(message)(Vg_UserMsg, "  too small or bad protection modes\n");

      /* set SIGSEGV to default handler */
      VG_(set_default_handler)(VKI_SIGSEGV);
      VG_(synth_fault_mapping)(tid, addr);

      /* The whole process should be about to die, since the default
	 action of SIGSEGV to kill the whole process. */
      return False;
   }

   /* For tracking memory events, indicate the entire frame has been
      allocated. */
   VG_TRACK( new_mem_stack_signal, addr - VG_STACK_REDZONE_SZB,
             size + VG_STACK_REDZONE_SZB, tid );

   return True;
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
   struct vg_sig_private* priv;
   Addr sp;
   ThreadState* tst;
   Int sigNo = siginfo->si_signo;
   /* Addr faultaddr; */ /* UNUSED */
   struct rt_sigframe* frame;

   /* Stack must be 16-byte aligned */
   vg_assert(VG_IS_16_ALIGNED(sizeof(struct vg_sig_private)));
   vg_assert(VG_IS_16_ALIGNED(sizeof(struct rt_sigframe)));

   sp_top_of_frame &= ~0xf;
   sp = sp_top_of_frame - sizeof(struct rt_sigframe);

   tst = VG_(get_ThreadState)(tid);
   if (!extend(tst, sp, sp_top_of_frame - sp))
      return;

   vg_assert(VG_IS_16_ALIGNED(sp));

   frame = (struct rt_sigframe *) sp;

   /* clear it (conservatively) */
   VG_(memset)(frame, 0, sizeof(*frame));

   /////////
   frame->pinfo = &frame->info;
   frame->puc = &frame->uc;

   frame->uc.uc_flags = 0;
   frame->uc.uc_link = 0;
   /////////

   /* Set up the stack chain pointer */
   VG_TRACK( pre_mem_write, Vg_CoreSignal, tid, "signal handler frame",
             sp, sizeof(UWord) );
   *(Addr *)sp = tst->arch.vex.guest_GPR1;
   VG_TRACK( post_mem_write, Vg_CoreSignal, tid, 
             sp, sizeof(UWord) );

   /* UNUSED:
   faultaddr = (Addr)siginfo->_sifields._sigfault._addr;
   if (sigNo == VKI_SIGILL && siginfo->si_code > 0)
      faultaddr = tst->arch.vex.guest_CIA;
   */

   VG_(memcpy)(&frame->info, siginfo, sizeof(*siginfo));
   VG_TRACK( post_mem_write, Vg_CoreSignal, tid,
             (Addr)&frame->info, sizeof(frame->info) );

   frame->uc.uc_flags = 0;
   frame->uc.uc_link  = 0;
   frame->uc.uc_stack = tst->altstack;
   frame->uc.uc_sigmask = tst->sig_mask;
   VG_TRACK( post_mem_write, Vg_CoreSignal, tid,
             (Addr)(&frame->uc), sizeof(frame->uc) );

#  define DO(gpr)  frame->uc.uc_mcontext.gp_regs[VKI_PT_R0+gpr] \
                      = tst->arch.vex.guest_GPR##gpr 
   DO(0);  DO(1);  DO(2);  DO(3);  DO(4);  DO(5);  DO(6);  DO(7);
   DO(8);  DO(9);  DO(10); DO(11); DO(12); DO(13); DO(14); DO(15);
   DO(16); DO(17); DO(18); DO(19); DO(20); DO(21); DO(22); DO(23);
   DO(24); DO(25); DO(26); DO(27); DO(28); DO(29); DO(30); DO(31);
#  undef DO

   frame->uc.uc_mcontext.gp_regs[VKI_PT_NIP]     = tst->arch.vex.guest_CIA;
   frame->uc.uc_mcontext.gp_regs[VKI_PT_MSR]     = 0xf032;   /* pretty arbitrary */
   frame->uc.uc_mcontext.gp_regs[VKI_PT_ORIG_R3] = tst->arch.vex.guest_GPR3;
   frame->uc.uc_mcontext.gp_regs[VKI_PT_CTR]     = tst->arch.vex.guest_CTR;
   frame->uc.uc_mcontext.gp_regs[VKI_PT_LNK]     = tst->arch.vex.guest_LR;
   frame->uc.uc_mcontext.gp_regs[VKI_PT_XER]     = LibVEX_GuestPPC64_get_XER(
                                                      &tst->arch.vex);
   frame->uc.uc_mcontext.gp_regs[VKI_PT_CCR]     = LibVEX_GuestPPC64_get_CR(
                                                      &tst->arch.vex);
   //mc->mc_gregs[VKI_PT_MQ]      = 0;
   //mc->mc_gregs[VKI_PT_TRAP]    = 0;
   //mc->mc_gregs[VKI_PT_DAR]     = fault_addr;
   //mc->mc_gregs[VKI_PT_DSISR]   = 0;
   //mc->mc_gregs[VKI_PT_RESULT]  = 0;

   /* XXX should do FP and vector regs */

   /* set up signal return trampoline */
   /* NB.  5 Sept 07.  mc->mc_pad[0..1] used to contain a the code to
      which the signal handler returns, and it just did sys_sigreturn
      or sys_rt_sigreturn.  But this doesn't work if the stack is
      non-executable, and it isn't consistent with the x86-linux and
      amd64-linux scheme for removing the stack frame.  So instead be
      consistent and use a stub in m_trampoline.  Then it doesn't
      matter whether or not the (guest) stack is executable.  This
      fixes #149519 and #145837. */
   frame->tramp[0] = 0; /* invalid */
   frame->tramp[1] = 0; /* invalid */
   VG_TRACK(post_mem_write, Vg_CoreSignal, tst->tid,
            (Addr)&frame->tramp, sizeof(frame->tramp));

   /* invalidate any translation of this area */
   VG_(discard_translations)( (Addr64)&frame->tramp[0], 
                              sizeof(frame->tramp), "stack_mcontext" );   

   /* set the signal handler to return to the trampoline */
   SET_SIGNAL_LR(tst, (Addr)&VG_(ppc64_linux_SUBST_FOR_rt_sigreturn));

   /* Stack pointer for the handler .. (note, back chain set
      earlier) */
   SET_SIGNAL_GPR(tid, 1, sp);

   /* Args for the handler .. */
   SET_SIGNAL_GPR(tid, 3, sigNo);
   SET_SIGNAL_GPR(tid, 4, (Addr) &frame->info);
   SET_SIGNAL_GPR(tid, 5, (Addr) &frame->uc);
   /* the kernel sets this, though it doesn't seem to be in the ABI */
   SET_SIGNAL_GPR(tid, 6, (Addr) &frame->info);

   /* Handler is in fact a standard ppc64-linux function descriptor, 
      so extract the function entry point and also the toc ptr to use. */
   SET_SIGNAL_GPR(tid, 2, (Addr) ((ULong*)handler)[1]);
   tst->arch.vex.guest_CIA = (Addr) ((ULong*)handler)[0];

   priv = &frame->priv;
   priv->magicPI       = 0x31415927;
   priv->sigNo_private = sigNo;
   priv->vex_shadow1   = tst->arch.vex_shadow1;
   priv->vex_shadow2   = tst->arch.vex_shadow2;

   if (0)
      VG_(printf)("pushed signal frame; %%R1 now = %#lx, "
                  "next %%CIA = %#llx, status=%d\n",
		  sp, tst->arch.vex.guest_CIA, tst->status);
}


/*------------------------------------------------------------*/
/*--- Destroying signal frames                             ---*/
/*------------------------------------------------------------*/

/* EXPORTED */
void VG_(sigframe_destroy)( ThreadId tid, Bool isRT )
{
   ThreadState *tst;
   struct vg_sig_private *priv;
   Addr sp;
   UInt frame_size;
   struct rt_sigframe *frame;
   Int sigNo;
   Bool has_siginfo = isRT;

   vg_assert(VG_(is_valid_tid)(tid));
   tst = VG_(get_ThreadState)(tid);

   /* Check that the stack frame looks valid */
   sp = tst->arch.vex.guest_GPR1;
   vg_assert(VG_IS_16_ALIGNED(sp));
   /* JRS 17 Nov 05: This code used to check that *sp -- which should
      have been set by the stwu at the start of the handler -- points
      to just above the frame (ie, the previous frame).  However, that
      isn't valid when delivering signals on alt stacks.  So I removed
      it.  The frame is still sanity-checked using the priv->magicPI
      field. */

   frame = (struct rt_sigframe *)sp;
   frame_size = sizeof(*frame);
   priv = &frame->priv;
   vg_assert(priv->magicPI == 0x31415927);
   tst->sig_mask = frame->uc.uc_sigmask;
   tst->tmp_sig_mask = tst->sig_mask;

   sigNo = priv->sigNo_private;

#  define DO(gpr)  tst->arch.vex.guest_GPR##gpr \
                      = frame->uc.uc_mcontext.gp_regs[VKI_PT_R0+gpr]
   DO(0);  DO(1);  DO(2);  DO(3);  DO(4);  DO(5);  DO(6);  DO(7);
   DO(8);  DO(9);  DO(10); DO(11); DO(12); DO(13); DO(14); DO(15);
   DO(16); DO(17); DO(18); DO(19); DO(20); DO(21); DO(22); DO(23);
   DO(24); DO(25); DO(26); DO(27); DO(28); DO(29); DO(30); DO(31);
#  undef DO

   tst->arch.vex.guest_CIA = frame->uc.uc_mcontext.gp_regs[VKI_PT_NIP];

   LibVEX_GuestPPC64_put_CR( frame->uc.uc_mcontext.gp_regs[VKI_PT_CCR], 
                             &tst->arch.vex );

   tst->arch.vex.guest_LR  = frame->uc.uc_mcontext.gp_regs[VKI_PT_LNK];
   tst->arch.vex.guest_CTR = frame->uc.uc_mcontext.gp_regs[VKI_PT_CTR];
   LibVEX_GuestPPC64_put_XER( frame->uc.uc_mcontext.gp_regs[VKI_PT_XER], 
                              &tst->arch.vex );

   tst->arch.vex_shadow1 = priv->vex_shadow1;
   tst->arch.vex_shadow2 = priv->vex_shadow2;

   VG_TRACK(die_mem_stack_signal, sp, frame_size);

   if (VG_(clo_trace_signals))
      VG_(message)(Vg_DebugMsg,
                   "vg_pop_signal_frame (thread %d): isRT=%d "
                   "valid magic; EIP=%#llx\n",
                   tid, has_siginfo, tst->arch.vex.guest_CIA);

   /* tell the tools */
   VG_TRACK( post_deliver_signal, tid, sigNo );
}

#endif // defined(VGP_ppc64_linux)

/*--------------------------------------------------------------------*/
/*--- end                                                          ---*/
/*--------------------------------------------------------------------*/
