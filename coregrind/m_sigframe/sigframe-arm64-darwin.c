
/*--------------------------------------------------------------------*/
/*--- Create/destroy signal delivery frames.                       ---*/
/*---                                       sigframe-arm64-darwin.c ---*/
/*--------------------------------------------------------------------*/

/*
   This file is part of Valgrind, a dynamic binary instrumentation
   framework.

   Copyright (C) 2013-2017 OpenWorks
      info@open-works.net

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

#if defined(VGP_arm64_darwin)

#include "pub_core_basics.h"
#include "pub_core_vki.h"
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


/* Originally copied from amd64-darwin and arm64-linux code.
   Produce a frame with layout entirely of our own choosing.

   This module creates and removes signal frames for signal deliveries
   on amd64-darwin.  The machine state is saved in a ucontext and retrieved
   from it later, so the handler can modify it and return.

   Frame should have a 16-aligned size, just in case that turns out to
   be important for Darwin.  (be conservative)
*/

struct hacky_sigframe {
   vki_siginfo_t info;
   struct vki_ucontext uc;

   unsigned long retcode[2];

   UInt magicPI;
   UInt sigNo_private;
   vki_sigset_t mask;
   VexGuestARM64State vex;
   VexGuestARM64State vex_shadow1;
   VexGuestARM64State vex_shadow2;
};


static void synthesize_ucontext( ThreadState *tst,
                                 struct vki_ucontext *uc,
                                 const struct vki_ucontext *siguc )
{
   VG_(memset)(uc, 0, sizeof(*uc));

   uc->uc_link = 0;
   if (siguc) {
     uc->uc_sigmask = siguc->uc_sigmask;
   }
   uc->uc_stack = tst->altstack;
   uc->uc_mcontext = &uc->__mcontext_data;

#  define TO_CTX(reg)  uc->__mcontext_data.__ss.__x[reg] = tst->arch.vex.guest_X##reg
   TO_CTX(0);   TO_CTX(1);   TO_CTX(2);   TO_CTX(3);
   TO_CTX(4);   TO_CTX(5);   TO_CTX(6);   TO_CTX(7);
   TO_CTX(8);   TO_CTX(9);   TO_CTX(10);  TO_CTX(11);
   TO_CTX(12);  TO_CTX(13);  TO_CTX(14);  TO_CTX(15);
   TO_CTX(16);  TO_CTX(17);  TO_CTX(18);  TO_CTX(19);
   TO_CTX(20);  TO_CTX(21);  TO_CTX(22);  TO_CTX(23);
   TO_CTX(24);  TO_CTX(25);  TO_CTX(26);  TO_CTX(27);
   TO_CTX(28);
#  undef TO_CTX
   uc->__mcontext_data.__ss.__fp = tst->arch.vex.guest_X29;
   uc->__mcontext_data.__ss.__lr = tst->arch.vex.guest_X30;
   uc->__mcontext_data.__ss.__sp = tst->arch.vex.guest_XSP;
   uc->__mcontext_data.__ss.__pc = tst->arch.vex.guest_PC;
   uc->__mcontext_data.__ss.__cpsr = 0; /* slack .. could do better */

   if (siguc) {
    uc->__mcontext_data.__es = siguc->__mcontext_data.__es;
   }
}

static void restore_from_ucontext(ThreadState *tst,
				                          const struct vki_ucontext *uc)
{
#     define FROM_CTX(reg)  tst->arch.vex.guest_X##reg = uc->__mcontext_data.__ss.__x[reg]
      FROM_CTX(0);   FROM_CTX(1);   FROM_CTX(2);   FROM_CTX(3);
      FROM_CTX(4);   FROM_CTX(5);   FROM_CTX(6);   FROM_CTX(7);
      FROM_CTX(8);   FROM_CTX(9);   FROM_CTX(10);  FROM_CTX(11);
      FROM_CTX(12);  FROM_CTX(13);  FROM_CTX(14);  FROM_CTX(15);
      FROM_CTX(16);  FROM_CTX(17);  FROM_CTX(18);  FROM_CTX(19);
      FROM_CTX(20);  FROM_CTX(21);  FROM_CTX(22);  FROM_CTX(23);
      FROM_CTX(24);  FROM_CTX(25);  FROM_CTX(26);  FROM_CTX(27);
      FROM_CTX(28);
#     undef FROM_CTX
  tst->arch.vex.guest_XSP = uc->__mcontext_data.__ss.__sp; // should we use SET_SP here?
  tst->arch.vex.guest_PC  = uc->__mcontext_data.__ss.__pc;
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
   ThreadState *tst;
   Addr sp    = sp_top_of_frame;
   struct hacky_sigframe *frame;
   Int  sigNo = siginfo->si_signo;
   UInt size  = sizeof(struct hacky_sigframe);

   vg_assert(VG_IS_16_ALIGNED(size));

   sp -= size;
   sp = VG_ROUNDDN(sp, 16);

   tst = VG_(get_ThreadState)(tid);
   if (! ML_(sf_maybe_extend_stack)(tst, sp, size, flags))
      return; // Give up.  No idea if this is correct

  frame = (struct hacky_sigframe *) sp;

   /* save stuff in frame */
   // FIXME: track writes?
   frame->magicPI = 0x31415927;
   frame->sigNo_private = siginfo->si_signo;
   frame->mask = tst->sig_mask;
   frame->vex = tst->arch.vex;
   frame->vex_shadow1 = tst->arch.vex_shadow1;
   frame->vex_shadow2 = tst->arch.vex_shadow2;

   /* Fill in the siginfo and ucontext.  */
   VG_TRACK( pre_mem_write, Vg_CoreSignal, tst->tid, "signal handler frame",
             (Addr)frame, offsetof(struct hacky_sigframe, uc));

   synthesize_ucontext(tst, &frame->uc, siguc);

   VG_TRACK( post_mem_write, Vg_CoreSignal, tst->tid,
             (Addr)frame, offsetof(struct hacky_sigframe, uc));

   /* Track our writes to siginfo */
   VG_TRACK( pre_mem_write, Vg_CoreSignal, tst->tid,  /* VVVVV */
             "signal handler siginfo", (Addr)frame,
             offsetof(struct hacky_sigframe, info));

   frame->info = *siginfo;

   VG_TRACK( post_mem_write, Vg_CoreSignal, tst->tid, /* ^^^^^ */
         (Addr)frame, offsetof(struct hacky_sigframe, info));

   /* Set up stack pointer */
   // FIXME: not sure about this whole offset thing in the amd64 version...
   // vg_assert(rsp == (Addr)&frame->returnAddr);
   VG_(set_SP)(tid, sp);
   VG_TRACK( post_reg_write, Vg_CoreSignal, tid, VG_O_STACK_PTR, sizeof(ULong));

   /* Set up program counter */
   VG_(set_IP)(tid, (ULong)handler);
   VG_TRACK( post_reg_write, Vg_CoreSignal, tid, VG_O_INSTR_PTR, sizeof(ULong));

   VG_TRACK( pre_mem_write, Vg_CoreSignal, tid, "signal handler frame",
             (Addr)frame, 1*sizeof(ULong) );

   if (flags & VKI_SA_RESTORER)
       tst->arch.vex.guest_X30 = (Addr)restorer;
   else
       tst->arch.vex.guest_X30 = (Addr)&VG_(arm64_darwin_SUBST_FOR_sigreturn);

   tst->arch.vex.guest_X0 = sigNo;
   tst->arch.vex.guest_X1 = (Addr)&frame->info;
   tst->arch.vex.guest_X2 = (Addr)&frame->uc;

   VG_TRACK( post_mem_write, Vg_CoreSignal, tid,
             (Addr)frame, 1*sizeof(ULong) );
   VG_TRACK( post_mem_write, Vg_CoreSignal, tid,
             (Addr)&frame->info, sizeof(frame->info));
   VG_TRACK( post_mem_write, Vg_CoreSignal, tid,
             (Addr)&frame->uc, sizeof(frame->uc));

   if (VG_(clo_trace_signals))
      VG_(message)(Vg_DebugMsg,
                   "sigframe_create (thread %u): "
                   "next RIP=%#lx, next RSP=%#lx\n",
                   tid, (Addr)handler, (Addr)frame );
}


/*------------------------------------------------------------*/
/*--- Destroying signal frames                             ---*/
/*------------------------------------------------------------*/

/* Remove a signal frame from thread 'tid's stack, and restore the CPU
   state from it.  Note, isRT is irrelevant here. */
void VG_(sigframe_destroy)( ThreadId tid, Bool isRT )
{
   ThreadState* tst;
   Addr sp;
   Int sigNo;
   struct hacky_sigframe* frame;

   vg_assert(VG_(is_valid_tid)(tid));
   tst = VG_(get_ThreadState)(tid);

   /* Check that the stack frame looks valid */
   sp = VG_(get_SP)(tid);

   frame = (struct hacky_sigframe *)sp;
   vg_assert(frame->magicPI == 0x31415927);

   vg_assert(VG_IS_16_ALIGNED((Addr)frame));

   /* restore the entire guest state, and shadows, from the frame. */
   tst->arch.vex         = frame->vex;
   tst->arch.vex_shadow1 = frame->vex_shadow1;
   tst->arch.vex_shadow2 = frame->vex_shadow2;
   restore_from_ucontext(tst, &frame->uc);

   tst->sig_mask            = frame->mask;
   tst->tmp_sig_mask        = frame->mask;
   sigNo                    = frame->sigNo_private;

   if (VG_(clo_trace_signals))
      VG_(message)(Vg_DebugMsg,
                   "sigframe_destroy (thread %d): "
                   "valid magic; next PC=%#llx\n",
                   tid, tst->arch.vex.guest_PC);

   VG_TRACK( die_mem_stack_signal,
             (Addr)frame - VG_STACK_REDZONE_SZB,
             sizeof(*frame) );

   /* tell the tools */
   VG_TRACK( post_deliver_signal, tid, sigNo );
}

#endif // defined(VGP_arm64_darwin)

/*--------------------------------------------------------------------*/
/*--- end                                   sigframe-arm64-linux.c ---*/
/*--------------------------------------------------------------------*/
