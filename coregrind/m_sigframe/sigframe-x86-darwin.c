
/*--------------------------------------------------------------------*/
/*--- Create/destroy signal delivery frames.                       ---*/
/*---                                        sigframe-x86-darwin.c ---*/
/*--------------------------------------------------------------------*/

/*
   This file is part of Valgrind, a dynamic binary instrumentation
   framework.

   Copyright (C) 2006-2017 OpenWorks Ltd
      info@open-works.co.uk

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

#if defined(VGP_x86_darwin)

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
   on x86-darwin.  The machine state is saved in a ucontext and retrieved
   from it later, so the handler can modify it and return.

   Frame should have a 16-aligned size, just in case that turns out to
   be important for Darwin.  (be conservative)
*/
struct hacky_sigframe {
   /* first four words look like a call to a 3-arg x86 function */
   UInt             returnAddr;
   UInt             a1_signo;
   UInt             a2_siginfo;
   UInt             a3_ucontext;
   UChar            lower_guardzone[512];  // put nothing here
   VexGuestX86State vex;
   VexGuestX86State vex_shadow1;
   VexGuestX86State vex_shadow2;
   vki_siginfo_t    fake_siginfo;
   struct vki_ucontext fake_ucontext;
   UInt             magicPI;
   UInt             sigNo_private;
   vki_sigset_t     mask; // saved sigmask; restore when hdlr returns
   UInt             __pad[3];
   UChar            upper_guardzone[512]; // put nothing here
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
   SC2(__edi,EDI);
   SC2(__esi,ESI);
   SC2(__ebp,EBP);
   SC2(__ebx,EBX);
   SC2(__edx,EDX);
   SC2(__eax,EAX);
   SC2(__ecx,ECX);
   SC2(__esp,ESP);
   SC2(__eip,EIP);
   uc->__mcontext_data.__ss.__eflags = LibVEX_GuestX86_get_eflags(&tst->arch.vex);

   if (siguc)
      uc->__mcontext_data.__es = siguc->__mcontext_data.__es;
#  undef SC2
}

static void restore_from_ucontext(ThreadState *tst,
				  const struct vki_ucontext *uc)
{
#  define SC2(REG,reg)  tst->arch.vex.guest_##REG = uc->__mcontext_data.__ss.reg
   SC2(EDI,__edi);
   SC2(ESI,__esi);
   SC2(EBP,__ebp);
   SC2(EBX,__ebx);
   SC2(EDX,__edx);
   SC2(EAX,__eax);
   SC2(ECX,__ecx);
   SC2(ESP,__esp);
   SC2(EIP,__eip);
   /* There doesn't seem to be an easy way to restore eflags */
#  undef SC2
}

/* Create a signal frame for thread 'tid'.  Make a 3-arg frame
   regardless of whether the client originally requested a 1-arg
   version (no SA_SIGINFO) or a 3-arg one (SA_SIGINFO) since in the
   former case, the x86 calling conventions will simply cause the
   extra 2 args to be ignored (inside the handler). */
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
   Addr esp;
   struct hacky_sigframe* frame;
   Int sigNo = siginfo->si_signo;

   vg_assert(VG_IS_16_ALIGNED(sizeof(struct hacky_sigframe)));

   sp_top_of_frame &= ~0xf;
   esp = sp_top_of_frame - sizeof(struct hacky_sigframe);
   esp -= 4; /* ELF ABI says that esp+4 must be 16 aligned on
                entry to a function. */

   tst = VG_(get_ThreadState)(tid);
   if (! ML_(sf_maybe_extend_stack)(tst, esp, sp_top_of_frame - esp, flags))
      return;

   vg_assert(VG_IS_16_ALIGNED(esp+4));

   frame = (struct hacky_sigframe *) esp;

   /* clear it (very conservatively) */
   VG_(memset)(&frame->lower_guardzone, 0, sizeof frame->lower_guardzone);
   VG_(memset)(&frame->vex,      0, sizeof(VexGuestX86State));
   VG_(memset)(&frame->vex_shadow1, 0, sizeof(VexGuestX86State));
   VG_(memset)(&frame->vex_shadow2, 0, sizeof(VexGuestX86State));
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
   vg_assert(esp == (Addr)&frame->returnAddr);
   VG_(set_SP)(tid, esp);
   VG_TRACK( post_reg_write, Vg_CoreSignal, tid, VG_O_STACK_PTR, sizeof(UInt));

   /* Set up program counter */
   VG_(set_IP)(tid, (UInt)handler);
   VG_TRACK( post_reg_write, Vg_CoreSignal, tid, VG_O_INSTR_PTR, sizeof(UInt));

   /* Set up RA and args for the frame */
   VG_TRACK( pre_mem_write, Vg_CoreSignal, tid, "signal handler frame",
             (Addr)frame, 4*sizeof(UInt) );
   frame->returnAddr  = (UInt)&VG_(x86_darwin_SUBST_FOR_sigreturn);

   frame->a1_signo    =         sigNo;
   frame->a2_siginfo  = (UInt)  &frame->fake_siginfo;
   frame->a3_ucontext = (UInt)  &frame->fake_ucontext;

   VG_TRACK( post_mem_write, Vg_CoreSignal, tid,
             (Addr)frame, 4*sizeof(UInt) );
   VG_TRACK( post_mem_write, Vg_CoreSignal, tid,
             (Addr)&frame->fake_siginfo, sizeof(frame->fake_siginfo));
   VG_TRACK( post_mem_write, Vg_CoreSignal, tid,
             (Addr)&frame->fake_ucontext, sizeof(frame->fake_ucontext));

   if (VG_(clo_trace_signals))
      VG_(message)(Vg_DebugMsg,
                   "sigframe_create (thread %u): "
                   "next EIP=%#lx, next ESP=%#lx\n",
                   tid, (Addr)handler, (Addr)frame );
}


/* Remove a signal frame from thread 'tid's stack, and restore the CPU
   state from it.  Note, isRT is irrelevant here. */
void VG_(sigframe_destroy)( ThreadId tid, Bool isRT )
{
   ThreadState *tst;
   Addr esp;
   Int sigNo;
   struct hacky_sigframe* frame;
 
   vg_assert(VG_(is_valid_tid)(tid));
   tst = VG_(get_ThreadState)(tid);

   /* Check that the stack frame looks valid */
   esp = VG_(get_SP)(tid);

   /* why -4 ? because the signal handler's return will have popped
      the return address off the stack; and the return address is the
      lowest-addressed element of hacky_sigframe. */
   frame = (struct hacky_sigframe*)(esp - 4);
   vg_assert(frame->magicPI == 0x31415927);

   /* This +4 is because of the -4 referred to in the ELF ABI comment
      in VG_(sigframe_create) just above. */
   vg_assert(VG_IS_16_ALIGNED((Addr)frame + 4));

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
                   "sigframe_destroy (thread %u): "
                   "valid magic; next EIP=%#x\n",
                   tid, tst->arch.vex.guest_EIP);

   VG_TRACK( die_mem_stack_signal, 
             (Addr)frame - VG_STACK_REDZONE_SZB, 
             sizeof(struct hacky_sigframe) );

   /* tell the tools */
   VG_TRACK( post_deliver_signal, tid, sigNo );
}

#endif // defined(VGP_x86_darwin)

/*--------------------------------------------------------------------*/
/*--- end                                                          ---*/
/*--------------------------------------------------------------------*/
