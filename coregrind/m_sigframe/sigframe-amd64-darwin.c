
/*--------------------------------------------------------------------*/
/*--- Create/destroy signal delivery frames.                       ---*/
/*---                                      sigframe-amd64-darwin.c ---*/
/*--------------------------------------------------------------------*/

/*
   This file is part of Valgrind, a dynamic binary instrumentation
   framework.

   Copyright (C) 2006-2017 OpenWorks Ltd
      info@open-works.co.uk

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

#if defined(VGP_amd64_darwin)

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
#include "pub_core_signals.h"
#include "pub_core_tooliface.h"
#include "pub_core_trampoline.h"
#include "pub_core_sigframe.h"      /* self */
#include "priv_sigframe.h"


/* Originally copied from ppc32-aix5 code.
   Produce a frame with layout entirely of our own choosing.

   This module creates and removes signal frames for signal deliveries
   on amd64-darwin.  The machine state is saved in a ucontext and retrieved
   from it later, so the handler can modify it and return.

   Frame should have a 16-aligned size, just in case that turns out to
   be important for Darwin.  (be conservative)
*/
struct hacky_sigframe {
   /* first word looks like a call to a 3-arg amd64-ELF function */
   ULong               returnAddr;
   UChar               lower_guardzone[512];  // put nothing here
   VexGuestAMD64State  vex;
   VexGuestAMD64State  vex_shadow1;
   VexGuestAMD64State  vex_shadow2;
   vki_siginfo_t       fake_siginfo;
   struct vki_ucontext fake_ucontext;
   UInt                magicPI;
   UInt                sigNo_private;
   vki_sigset_t        mask; // saved sigmask; restore when hdlr returns
   UChar               upper_guardzone[512]; // put nothing here
   // and don't zero it, since that might overwrite the client's
   // stack redzone, at least on archs which have one
};

/* Create a plausible-looking sigcontext from the thread's
   Vex guest state.  NOTE: does not fill in the FP or SSE
   bits of sigcontext at the moment.
 */
static void synthesize_ucontext(ThreadState *tst,
				struct vki_ucontext *uc,
				const struct vki_ucontext *siguc)
{
   VG_(memset)(uc, 0, sizeof(*uc));

   if (siguc) uc->uc_sigmask = siguc->uc_sigmask;
   uc->uc_stack = tst->altstack;
   uc->uc_mcontext = &uc->__mcontext_data;

#  define SC2(reg,REG)  uc->__mcontext_data.__ss.reg = tst->arch.vex.guest_##REG
   SC2(__r8,R8);
   SC2(__r9,R9);
   SC2(__r10,R10);
   SC2(__r11,R11);
   SC2(__r12,R12);
   SC2(__r13,R13);
   SC2(__r14,R14);
   SC2(__r15,R15);
   SC2(__rdi,RDI);
   SC2(__rsi,RSI);
   SC2(__rbp,RBP);
   SC2(__rbx,RBX);
   SC2(__rdx,RDX);
   SC2(__rax,RAX);
   SC2(__rcx,RCX);
   SC2(__rsp,RSP);
   SC2(__rip,RIP);
   uc->__mcontext_data.__ss.__rflags = LibVEX_GuestAMD64_get_rflags(&tst->arch.vex);

   if (siguc)
      uc->__mcontext_data.__es = siguc->__mcontext_data.__es;
#  undef SC2
}

static void restore_from_ucontext(ThreadState *tst,
				  const struct vki_ucontext *uc)
{
#  define SC2(REG,reg)  tst->arch.vex.guest_##REG = uc->__mcontext_data.__ss.reg
   SC2(R8,__r8);
   SC2(R9,__r9);
   SC2(R10,__r10);
   SC2(R11,__r11);
   SC2(R12,__r12);
   SC2(R13,__r13);
   SC2(R14,__r14);
   SC2(R15,__r15);
   SC2(RDI,__rdi);
   SC2(RSI,__rsi);
   SC2(RBP,__rbp);
   SC2(RBX,__rbx);
   SC2(RDX,__rdx);
   SC2(RAX,__rax);
   SC2(RCX,__rcx);
   SC2(RSP,__rsp);
   SC2(RIP,__rip);
   /* There doesn't seem to be an easy way to restore rflags */
#  undef SC2
}

/* Create a signal frame for thread 'tid'.  Make a 3-arg frame
   regardless of whether the client originally requested a 1-arg
   version (no SA_SIGINFO) or a 3-arg one (SA_SIGINFO) since in the
   former case, the amd64 calling conventions will simply cause the
   extra 2 args to be ignored (inside the handler).  (We hope!) */
void VG_(sigframe_create) ( ThreadId tid,
                            Bool on_altstack,
                            Addr sp_top_of_frame,
                            const vki_siginfo_t *siginfo,
                            const struct vki_ucontext *siguc,
                            void *handler,
                            UInt flags,
                            const vki_sigset_t *mask,
                            void *restorer )
{
   ThreadState* tst;
   Addr rsp;
   struct hacky_sigframe* frame;
   Int sigNo = siginfo->si_signo;

   vg_assert(VG_IS_16_ALIGNED(sizeof(struct hacky_sigframe)));

   sp_top_of_frame &= ~0xfUL;
   rsp = sp_top_of_frame - sizeof(struct hacky_sigframe);
   rsp -= 8; /* ELF ABI says that rsp+8 must be 16 aligned on
                entry to a function. */

   tst = VG_(get_ThreadState)(tid);
   if (! ML_(sf_maybe_extend_stack)(tst, rsp, sp_top_of_frame - rsp, flags))
      return;

   vg_assert(VG_IS_16_ALIGNED(rsp+8));

   frame = (struct hacky_sigframe *) rsp;

   /* clear it (very conservatively) */
   VG_(memset)(&frame->lower_guardzone, 0, sizeof frame->lower_guardzone);
   VG_(memset)(&frame->vex,      0, sizeof(VexGuestAMD64State));
   VG_(memset)(&frame->vex_shadow1, 0, sizeof(VexGuestAMD64State));
   VG_(memset)(&frame->vex_shadow2, 0, sizeof(VexGuestAMD64State));
   VG_(memset)(&frame->fake_siginfo,  0, sizeof(frame->fake_siginfo));
   VG_(memset)(&frame->fake_ucontext, 0, sizeof(frame->fake_ucontext));

   /* save stuff in frame */
   frame->vex           = tst->arch.vex;
   frame->vex_shadow1   = tst->arch.vex_shadow1;
   frame->vex_shadow2   = tst->arch.vex_shadow2;
   frame->sigNo_private = sigNo;
   frame->mask          = tst->sig_mask;
   frame->magicPI       = 0x31415927;

   /* Fill in the siginfo and ucontext.  */
   synthesize_ucontext(tst, &frame->fake_ucontext, siguc);
   frame->fake_siginfo = *siginfo;

   /* Set up stack pointer */
   vg_assert(rsp == (Addr)&frame->returnAddr);
   VG_(set_SP)(tid, rsp);
   VG_TRACK( post_reg_write, Vg_CoreSignal, tid, VG_O_STACK_PTR, sizeof(ULong));

   /* Set up program counter */
   VG_(set_IP)(tid, (ULong)handler);
   VG_TRACK( post_reg_write, Vg_CoreSignal, tid, VG_O_INSTR_PTR, sizeof(ULong));

   /* Set up RA and args for the frame */
   VG_TRACK( pre_mem_write, Vg_CoreSignal, tid, "signal handler frame",
             (Addr)frame, 1*sizeof(ULong) );
   frame->returnAddr  = (ULong)&VG_(amd64_darwin_SUBST_FOR_sigreturn);

   /* XXX should tell the tool that these regs got written */
   tst->arch.vex.guest_RDI = (ULong) sigNo;
   tst->arch.vex.guest_RSI = (Addr)  &frame->fake_siginfo;
   tst->arch.vex.guest_RDX = (Addr)  &frame->fake_ucontext;

   VG_TRACK( post_mem_write, Vg_CoreSignal, tid,
             (Addr)frame, 1*sizeof(ULong) );
   VG_TRACK( post_mem_write, Vg_CoreSignal, tid,
             (Addr)&frame->fake_siginfo, sizeof(frame->fake_siginfo));
   VG_TRACK( post_mem_write, Vg_CoreSignal, tid,
             (Addr)&frame->fake_ucontext, sizeof(frame->fake_ucontext));

   if (VG_(clo_trace_signals))
      VG_(message)(Vg_DebugMsg,
                   "sigframe_create (thread %u): "
                   "next RIP=%#lx, next RSP=%#lx\n",
                   tid, (Addr)handler, (Addr)frame );
}


/* Remove a signal frame from thread 'tid's stack, and restore the CPU
   state from it.  Note, isRT is irrelevant here. */
void VG_(sigframe_destroy)( ThreadId tid, Bool isRT )
{
   ThreadState *tst;
   Addr rsp;
   Int sigNo;
   struct hacky_sigframe* frame;
 
   vg_assert(VG_(is_valid_tid)(tid));
   tst = VG_(get_ThreadState)(tid);

   /* Check that the stack frame looks valid */
   rsp = VG_(get_SP)(tid);

   /* why -8 ? because the signal handler's return will have popped
      the return address off the stack; and the return address is the
      lowest-addressed element of hacky_sigframe. */
   frame = (struct hacky_sigframe*)(rsp - 8);
   vg_assert(frame->magicPI == 0x31415927);

   /* This +8 is because of the -8 referred to in the ELF ABI comment
      in VG_(sigframe_create) just above. */
   vg_assert(VG_IS_16_ALIGNED((Addr)frame + 8));

   /* restore the entire guest state, and shadows, from the frame. */
   tst->arch.vex            = frame->vex;
   tst->arch.vex_shadow1    = frame->vex_shadow1;
   tst->arch.vex_shadow2    = frame->vex_shadow2;
   restore_from_ucontext(tst, &frame->fake_ucontext);

   tst->sig_mask            = frame->mask;
   tst->tmp_sig_mask        = frame->mask;
   sigNo                    = frame->sigNo_private;

   if (VG_(clo_trace_signals))
      VG_(message)(Vg_DebugMsg,
                   "sigframe_destroy (thread %d): "
                   "valid magic; next RIP=%#llx\n",
                   tid, tst->arch.vex.guest_RIP);

   VG_TRACK( die_mem_stack_signal, 
             (Addr)frame - VG_STACK_REDZONE_SZB, 
             sizeof(struct hacky_sigframe) );

   /* tell the tools */
   VG_TRACK( post_deliver_signal, tid, sigNo );
}

#endif // defined(VGP_amd64_darwin)

/*--------------------------------------------------------------------*/
/*--- end                                  sigframe-amd64-darwin.c ---*/
/*--------------------------------------------------------------------*/
