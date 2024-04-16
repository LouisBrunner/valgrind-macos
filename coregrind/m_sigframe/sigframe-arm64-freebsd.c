
/*--------------------------------------------------------------------*/
/*--- Create/destroy signal delivery frames.                       ---*/
/*---                                     sigframe-arm64-freebsd.c ---*/
/*--------------------------------------------------------------------*/

/*
   This file is part of Valgrind, a dynamic binary instrumentation
   framework.

   Copyright (C) 2024
      pjfloyd@wanadoo.fr

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

#if defined(VGP_arm64_freebsd)

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

const UInt MAGIC_PI = 0x31415927U;
const UInt MAGIC_E  = 0x27182818U;

struct vg_sigframe {
   /* Sanity check word. */
   UInt magicPI;

   UInt handlerflags; /* flags for signal handler */

   /* Safely-saved version of sigNo, as described above. */
   Int sigNo_private;

   /* XXX This is wrong.  Surely we should store the shadow values
      into the shadow memory behind the actual values? */
   VexGuestARM64State vex_shadow1;
   VexGuestARM64State vex_shadow2;

   /* HACK ALERT */
   VexGuestARM64State vex;
   /* end HACK ALERT */

   /* saved signal mask to be restored when handler returns */
   vki_sigset_t mask;

   /* Sanity check word.  Is the highest-addressed word; do not
      move!*/
   UInt magicE;
};

struct sigframe {
   struct vki_ucontext uContext;
   vki_siginfo_t       sigInfo;
   // amd64 has fpstate, for arm64 it is in the ucontext
   struct vg_sigframe vg;
};

/*------------------------------------------------------------*/
/*--- Creating signal frames                               ---*/
/*------------------------------------------------------------*/

/* Create a plausible-looking sigcontext from the thread's
   Vex guest state.
*/
static void synth_ucontext(ThreadId             tid,
                           const vki_siginfo_t* si,
                           UWord                trapno,
                           UWord                err,
                           const vki_sigset_t*  set,
                           struct vki_ucontext* uc)
{

   ThreadState*         tst = VG_(get_ThreadState)(tid);
   struct vki_mcontext* sc  = &uc->uc_mcontext;

   VG_(memset)(uc, 0, sizeof(*uc));

   uc->uc_flags   = 0;
   uc->uc_link    = 0;
   uc->uc_sigmask = *set;
   uc->uc_stack   = tst->altstack;

#define TO_CTX(reg) sc->mc_gpregs.gp_x[reg] = tst->arch.vex.guest_X##reg
   TO_CTX(0);
   TO_CTX(1);
   TO_CTX(2);
   TO_CTX(3);
   TO_CTX(4);
   TO_CTX(5);
   TO_CTX(6);
   TO_CTX(7);
   TO_CTX(8);
   TO_CTX(9);
   TO_CTX(10);
   TO_CTX(11);
   TO_CTX(12);
   TO_CTX(13);
   TO_CTX(14);
   TO_CTX(15);
   TO_CTX(16);
   TO_CTX(17);
   TO_CTX(18);
   TO_CTX(19);
   TO_CTX(20);
   TO_CTX(21);
   TO_CTX(22);
   TO_CTX(23);
   TO_CTX(24);
   TO_CTX(25);
   TO_CTX(26);
   TO_CTX(27);
   TO_CTX(28);
   TO_CTX(29);
#undef TO_CTX
   sc->mc_gpregs.gp_lr   = tst->arch.vex.guest_X30;
   sc->mc_gpregs.gp_sp   = tst->arch.vex.guest_XSP;
   sc->mc_gpregs.gp_elr  = tst->arch.vex.guest_PC;
   sc->mc_gpregs.gp_spsr = LibVEX_GuestARM64_get_nzcv(&tst->arch.vex);
}

/* Extend the stack segment downwards if needed so as to ensure the
   new signal frames are mapped to something.  Return a Bool
   indicating whether or not the operation was successful.
*/
static Bool extend(ThreadState* tst, Addr addr, SizeT size)
{
   ThreadId        tid      = tst->tid;
   NSegment const* stackseg = NULL;

   if (VG_(extend_stack)(tid, addr)) {
      stackseg = VG_(am_find_nsegment)(addr);
      if (0 && stackseg) {
         VG_(printf)("frame=%#lx seg=%#lx-%#lx\n", addr, stackseg->start,
                     stackseg->end);
      }
   }

   if (stackseg == NULL || !stackseg->hasR || !stackseg->hasW) {
      VG_(message)(
         Vg_UserMsg,
         "Can't extend stack to %#lx during signal delivery for thread %u:\n",
         addr, tid);
      if (stackseg == NULL) {
         VG_(message)(Vg_UserMsg, "  no stack segment\n");
      } else {
         VG_(message)(Vg_UserMsg, "  too small or bad protection modes\n");
      }

      /* set SIGSEGV to default handler */
      VG_(set_default_handler)(VKI_SIGSEGV);
      VG_(synth_fault_mapping)(tid, addr);

      /* The whole process should be about to die, since the default
      action of SIGSEGV to kill the whole process. */
      return False;
   }

   /* For tracking memory events, indicate the entire frame has been
      allocated. */
   VG_TRACK(new_mem_stack_signal, addr - VG_STACK_REDZONE_SZB,
            size + VG_STACK_REDZONE_SZB, tid);

   return True;
}

/* Build the Valgrind-specific part of a signal frame. */

static void build_vg_sigframe(struct vg_sigframe* frame,
                              ThreadState*        tst,
                              const vki_sigset_t* mask,
                              UInt                flags,
                              Int                 sigNo)
{
   frame->sigNo_private = sigNo;
   frame->magicPI       = MAGIC_PI;
   frame->vex_shadow1   = tst->arch.vex_shadow1;
   frame->vex_shadow2   = tst->arch.vex_shadow2;
   /* HACK ALERT */
   frame->vex = tst->arch.vex;
   /* end HACK ALERT */
   frame->mask         = tst->sig_mask;
   frame->handlerflags = flags;
   frame->magicE       = MAGIC_E;
}

static Addr build_sigframe(ThreadState*               tst,
                           Addr                       sp_top_of_frame,
                           const vki_siginfo_t*       siginfo,
                           const struct vki_ucontext* siguc,
                           void*                      handler,
                           UInt                       flags,
                           const vki_sigset_t*        mask,
                           void*                      restorer)
{
   struct sigframe* frame;
   Addr             sp    = sp_top_of_frame;
   Int              sigNo = siginfo->si_signo;
   UWord            trapno;
   UWord            err;

   sp -= sizeof(*frame);
   sp    = VG_ROUNDDN(sp, 16) - 8;
   frame = (struct sigframe*)sp;

   if (!extend(tst, sp, sizeof(*frame))) {
      return sp_top_of_frame;
   }

   /* retaddr, siginfo, uContext fields are to be written */
   VG_TRACK(pre_mem_write, Vg_CoreSignal, tst->tid, "signal handler frame", sp,
            offsetof(struct sigframe, vg));

   // on amd64 these are in the ucontext
   trapno = 0;
   err    = 0;

   VG_(memcpy)(&frame->sigInfo, siginfo, sizeof(vki_siginfo_t));

   if (sigNo == VKI_SIGILL && siginfo->si_code > 0) {
      frame->sigInfo.si_addr = (void*)tst->arch.vex.guest_PC;
   }

   synth_ucontext(tst->tid, siginfo, trapno, err, mask, &frame->uContext);

   VG_TRACK(post_mem_write, Vg_CoreSignal, tst->tid, sp,
            offsetof(struct sigframe, vg));

   build_vg_sigframe(&frame->vg, tst, mask, flags, sigNo);

   return sp;
}

/* EXPORTED */
void VG_(sigframe_create)(ThreadId                   tid,
                          Bool                       on_altstack,
                          Addr                       sp_top_of_frame,
                          const vki_siginfo_t*       siginfo,
                          const struct vki_ucontext* siguc,
                          void*                      handler,
                          UInt                       flags,
                          const vki_sigset_t*        mask,
                          void*                      restorer)
{
   Addr             sp;
   struct sigframe* frame;
   ThreadState*     tst = VG_(get_ThreadState)(tid);

   sp    = build_sigframe(tst, sp_top_of_frame, siginfo, siguc, handler, flags,
                          mask, restorer);
   frame = (struct sigframe*)sp;

   VG_(set_SP)(tid, sp);
   VG_TRACK(post_reg_write, Vg_CoreSignal, tid, VG_O_STACK_PTR, sizeof(Addr));

   tst->arch.vex.guest_PC = (Addr)handler;
   tst->arch.vex.guest_X0 = (ULong)siginfo->si_signo;
   tst->arch.vex.guest_X1 = (Addr)&frame->sigInfo;
   tst->arch.vex.guest_X2 = (Addr)&frame->uContext;

   tst->arch.vex.guest_X30 = (Addr)&VG_(arm64_freebsd_SUBST_FOR_sigreturn);

   /* And tell the tool that these registers have been written. */
   VG_TRACK(post_reg_write, Vg_CoreSignal, tst->tid,
            offsetof(VexGuestARM64State, guest_PC), sizeof(UWord));
   VG_TRACK(post_reg_write, Vg_CoreSignal, tst->tid,
            offsetof(VexGuestARM64State, guest_X0), sizeof(UWord));
   VG_TRACK(post_reg_write, Vg_CoreSignal, tst->tid,
            offsetof(VexGuestARM64State, guest_X1), sizeof(UWord));
   VG_TRACK(post_reg_write, Vg_CoreSignal, tst->tid,
            offsetof(VexGuestARM64State, guest_X2), sizeof(UWord));
   VG_TRACK(post_reg_write, Vg_CoreSignal, tst->tid,
            offsetof(VexGuestARM64State, guest_X30), sizeof(UWord));
}

/*------------------------------------------------------------*/
/*--- Destroying signal frames                             ---*/
/*------------------------------------------------------------*/

/* Return False and don't do anything, just set the client to take a
   segfault, if it looks like the frame is corrupted. */
static Bool
restore_vg_sigframe(ThreadState* tst, struct vg_sigframe* frame, Int* sigNo)
{
   if (frame->magicPI != MAGIC_PI || frame->magicE != MAGIC_E) {
      VG_(message)(Vg_UserMsg,
                   "Thread %u return signal frame "
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
   tst->arch.vex = frame->vex;
   /* end HACK ALERT */
   *sigNo = frame->sigNo_private;
   return True;
}

static void restore_sigcontext(ThreadState* tst, struct vki_mcontext* sc)
{
#define FROM_CTX(reg) tst->arch.vex.guest_X##reg = sc->mc_gpregs.gp_x[reg]
   FROM_CTX(0);
   FROM_CTX(1);
   FROM_CTX(2);
   FROM_CTX(3);
   FROM_CTX(4);
   FROM_CTX(5);
   FROM_CTX(6);
   FROM_CTX(7);
   FROM_CTX(8);
   FROM_CTX(9);
   FROM_CTX(10);
   FROM_CTX(11);
   FROM_CTX(12);
   FROM_CTX(13);
   FROM_CTX(14);
   FROM_CTX(15);
   FROM_CTX(16);
   FROM_CTX(17);
   FROM_CTX(18);
   FROM_CTX(19);
   FROM_CTX(20);
   FROM_CTX(21);
   FROM_CTX(22);
   FROM_CTX(23);
   FROM_CTX(24);
   FROM_CTX(25);
   FROM_CTX(26);
   FROM_CTX(27);
   FROM_CTX(28);
   FROM_CTX(29);
#undef FROM_CTX
   tst->arch.vex.guest_X30 = sc->mc_gpregs.gp_lr;
   tst->arch.vex.guest_PC  = sc->mc_gpregs.gp_elr;
}

static SizeT
restore_sigframe(ThreadState* tst, struct sigframe* frame, Int* sigNo)
{
   if (restore_vg_sigframe(tst, &frame->vg, sigNo)) {
      restore_sigcontext(tst, &frame->uContext.uc_mcontext);
   }

   return sizeof(*frame);
}

void VG_(sigframe_destroy)(ThreadId tid)
{
   vg_assert(VG_(is_valid_tid)(tid));

   Addr         sp;
   ThreadState* tst;
   SizeT        size;
   Int          sigNo;

   tst = VG_(get_ThreadState)(tid);

   /* Correctly reestablish the frame base address. */
   sp = tst->arch.vex.guest_XSP;

   size = restore_sigframe(tst, (struct sigframe*)sp, &sigNo);

   VG_TRACK(die_mem_stack_signal, sp - VG_STACK_REDZONE_SZB,
            size + VG_STACK_REDZONE_SZB);

   if (VG_(clo_trace_signals)) {
      VG_(message)(
         Vg_DebugMsg,
         "VG_(sigframe_destroy) (thread %u): valid magic; RIP=%#llx\n", tid,
         tst->arch.vex.guest_PC);
   }

   /* tell the tools */
   VG_TRACK(post_deliver_signal, tid, sigNo);
}

#endif // defined(VGP_arm64_freebsd)

/*--------------------------------------------------------------------*/
/*--- end                                 sigframe-arm64-freebsd.c ---*/
/*--------------------------------------------------------------------*/
