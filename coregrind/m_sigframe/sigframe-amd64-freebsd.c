
/*--------------------------------------------------------------------*/
/*--- Create/destroy signal delivery frames.                       ---*/
/*---                                     sigframe-amd64-freebsd.c ---*/
/*--------------------------------------------------------------------*/

/*
   This file is part of Valgrind, a dynamic binary instrumentation
   framework.

   Copyright (C) 2000-2009 Nicholas Nethercote
      njn@valgrind.org
   Copyright (C) 2018-2021 Paul Floyd
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

#if defined(VGP_amd64_freebsd)

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
#include "pub_core_sigframe.h"   /* self */

/* This module creates and removes signal frames for signal deliveries
   on amd64-freebsd.
*/

const UInt MAGIC_PI = 0x31415927U;
const UInt MAGIC_E = 0x27182818U;

/*------------------------------------------------------------*/
/*--- Signal frame layouts                                 ---*/
/*------------------------------------------------------------*/

// A structure in which to save the application's registers
// during the execution of signal handlers.

// In theory, so long as we get the arguments to the handler function
// right, it doesn't matter what the exact layout of the rest of the
// frame is.  Unfortunately, things like gcc's exception unwinding
// make assumptions about the locations of various parts of the frame,
// so we need to duplicate it exactly.

/* Valgrind-specific parts of the signal frame */
struct vg_sigframe {
   /* Sanity check word. */
   UInt magicPI;

   UInt handlerflags;   /* flags for signal handler */


   /* Safely-saved version of sigNo, as described above. */
   Int  sigNo_private;

   /* XXX This is wrong.  Surely we should store the shadow values
      into the shadow memory behind the actual values? */
   VexGuestAMD64State vex_shadow1;
   VexGuestAMD64State vex_shadow2;

   /* HACK ALERT */
   VexGuestAMD64State vex;
   /* end HACK ALERT */

   /* saved signal mask to be restored when handler returns */
   vki_sigset_t   mask;

   /* Sanity check word.  Is the highest-addressed word; do not
      move!*/
   UInt magicE;
};

struct sigframe {
   /* Sig handler's return address */
   Addr retaddr;

   Addr phandler;      /* "action" or "handler" */

   /* pointed to by puContext */
   struct vki_ucontext uContext;

   vki_siginfo_t sigInfo;

   struct _vki_fpstate fpstate;

   struct vg_sigframe vg;
};

/*------------------------------------------------------------*/
/*--- Creating signal frames                               ---*/
/*------------------------------------------------------------*/

/* Create a plausible-looking sigcontext from the thread's
   Vex guest state.
*/
static
void synth_ucontext(ThreadId tid, const vki_siginfo_t *si,
                    UWord trapno, UWord err, const vki_sigset_t *set,
                    struct vki_ucontext *ucp, struct _vki_fpstate *fpstate)
{
   ThreadState *tst = VG_(get_ThreadState)(tid);
   struct vki_mcontext *sc = &ucp->uc_mcontext;

   VG_(memset)(ucp, 0, sizeof(*ucp));

   ucp->uc_flags = 0;
   ucp->uc_link = 0;
   ucp->uc_sigmask = *set;
   ucp->uc_stack = tst->altstack;
   VG_(memcpy)(&sc->fpstate, fpstate, sizeof(*fpstate));

#  define SC2(reg,REG)  sc->reg = tst->arch.vex.guest_##REG
   SC2(r8,R8);
   SC2(r9,R9);
   SC2(r10,R10);
   SC2(r11,R11);
   SC2(r12,R12);
   SC2(r13,R13);
   SC2(r14,R14);
   SC2(r15,R15);
   SC2(rdi,RDI);
   SC2(rsi,RSI);
   SC2(rbp,RBP);
   SC2(rbx,RBX);
   SC2(rdx,RDX);
   SC2(rax,RAX);
   SC2(rcx,RCX);
   SC2(rsp,RSP);
   /*
      SC2(cs,CS);
      SC2(gs,SS);
      XXX
   */
   SC2(rip,RIP);
   sc->addr = (vki_register_t)si->si_addr;
   sc->err = (vki_register_t)err;
   sc->fpformat = VKI_FPFMT_NODEV;
   sc->ownedfp = VKI_FPOWNED_NONE;
   sc->len = sizeof(*sc);
   sc->rflags = (vki_register_t)LibVEX_GuestAMD64_get_rflags(&tst->arch.vex);
   sc->trapno = trapno;
#  undef SC2
}


/* Extend the stack segment downwards if needed so as to ensure the
   new signal frames are mapped to something.  Return a Bool
   indicating whether or not the operation was successful.
*/
static Bool extend ( ThreadState *tst, Addr addr, SizeT size )
{
   ThreadId        tid = tst->tid;
   NSegment const* stackseg = NULL;

   if (VG_(extend_stack)(tid, addr)) {
      stackseg = VG_(am_find_nsegment)(addr);
      if (0 && stackseg) {
         VG_(printf)("frame=%#lx seg=%#lx-%#lx\n",
                     addr, stackseg->start, stackseg->end);
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
   VG_TRACK( new_mem_stack_signal, addr - VG_STACK_REDZONE_SZB,
             size + VG_STACK_REDZONE_SZB, tid );

   return True;
}


/* Build the Valgrind-specific part of a signal frame. */

static void build_vg_sigframe(struct vg_sigframe *frame,
                              ThreadState *tst,
                              const vki_sigset_t *mask,
                              UInt flags,
                              Int sigNo)
{
   frame->sigNo_private = sigNo;
   frame->magicPI       = MAGIC_PI;
   frame->vex_shadow1   = tst->arch.vex_shadow1;
   frame->vex_shadow2   = tst->arch.vex_shadow2;
   /* HACK ALERT */
   frame->vex           = tst->arch.vex;
   /* end HACK ALERT */
   frame->mask          = tst->sig_mask;
   frame->handlerflags  = flags;
   frame->magicE        = MAGIC_E;
}

static Addr build_sigframe(ThreadState *tst,
                           Addr rsp_top_of_frame,
                           const vki_siginfo_t *siginfo,
                           const struct vki_ucontext *siguc,
                           void *handler, UInt flags,
                           const vki_sigset_t *mask,
                           void *restorer)
{
   struct sigframe *frame;
   Addr rsp = rsp_top_of_frame;
   Int  sigNo = siginfo->si_signo;
   UWord trapno;
   UWord err;

   rsp -= sizeof(*frame);
   rsp = VG_ROUNDDN(rsp, 16) - 8;
   frame = (struct sigframe *)rsp;

   if (!extend(tst, rsp, sizeof(*frame))) {
      return rsp_top_of_frame;
   }

   /* retaddr, siginfo, uContext fields are to be written */
   VG_TRACK( pre_mem_write, Vg_CoreSignal, tst->tid, "signal handler frame",
             rsp, offsetof(struct sigframe, vg) );

   frame->retaddr = (Addr)&VG_(amd64_freebsd_SUBST_FOR_sigreturn);

   if (siguc) {
      trapno = siguc->uc_mcontext.trapno;
      err = siguc->uc_mcontext.err;
   } else {
      trapno = 0;
      err = 0;
   }

   VG_(memcpy)(&frame->sigInfo, siginfo, sizeof(vki_siginfo_t));

   if (sigNo == VKI_SIGILL && siginfo->si_code > 0) {
      frame->sigInfo.si_addr = (void*)tst->arch.vex.guest_RIP;
   }

   synth_ucontext(tst->tid, siginfo, trapno, err, mask,
                  &frame->uContext, &frame->fpstate);

   VG_TRACK( post_mem_write,  Vg_CoreSignal, tst->tid,
             rsp, offsetof(struct sigframe, vg) );

   build_vg_sigframe(&frame->vg, tst, mask, flags, sigNo);

   return rsp;
}


void VG_(sigframe_create)( ThreadId tid,
                           Bool on_altstack,
                           Addr rsp_top_of_frame,
                           const vki_siginfo_t *siginfo,
                           const struct vki_ucontext *siguc,
                           void *handler,
                           UInt flags,
                           const vki_sigset_t *mask,
                           void *restorer )
{
   Addr rsp;
   struct sigframe *frame;
   ThreadState* tst = VG_(get_ThreadState)(tid);

   rsp = build_sigframe(tst, rsp_top_of_frame, siginfo, siguc, handler,
                        flags, mask, restorer);
   frame = (struct sigframe *)rsp;

   /* Set the thread so it will next run the handler. */
   /* tst->m_rsp  = rsp;  also notify the tool we've updated RSP */
   VG_(set_SP)(tid, rsp);
   VG_TRACK( post_reg_write, Vg_CoreSignal, tid, VG_O_STACK_PTR, sizeof(Addr));

   //VG_(printf)("handler = %p\n", handler);
   tst->arch.vex.guest_RIP = (Addr) handler;
   tst->arch.vex.guest_RDI = (ULong) siginfo->si_signo;
   tst->arch.vex.guest_RSI = (Addr) &frame->sigInfo;
   tst->arch.vex.guest_RDX = (Addr) &frame->uContext;
   /* And tell the tool that these registers have been written. */
   VG_TRACK( post_reg_write, Vg_CoreSignal, tst->tid,
             offsetof(VexGuestAMD64State,guest_RIP), sizeof(UWord) );
   VG_TRACK( post_reg_write, Vg_CoreSignal, tst->tid,
             offsetof(VexGuestAMD64State,guest_RDI), sizeof(UWord) );
   VG_TRACK( post_reg_write, Vg_CoreSignal, tst->tid,
             offsetof(VexGuestAMD64State,guest_RSI), sizeof(UWord) );
   VG_TRACK( post_reg_write, Vg_CoreSignal, tst->tid,
             offsetof(VexGuestAMD64State,guest_RDX), sizeof(UWord) );

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
   if (frame->magicPI != MAGIC_PI ||
         frame->magicE  != MAGIC_E) {
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
void restore_sigcontext( ThreadState *tst,
                         struct vki_mcontext *sc,
                         struct _vki_fpstate *fpstate )
{
   tst->arch.vex.guest_RAX     = sc->rax;
   tst->arch.vex.guest_RCX     = sc->rcx;
   tst->arch.vex.guest_RDX     = sc->rdx;
   tst->arch.vex.guest_RBX     = sc->rbx;
   tst->arch.vex.guest_RBP     = sc->rbp;
   tst->arch.vex.guest_RSP     = sc->rsp;
   tst->arch.vex.guest_RSI     = sc->rsi;
   tst->arch.vex.guest_RDI     = sc->rdi;
   tst->arch.vex.guest_R8      = sc->r8;
   tst->arch.vex.guest_R9      = sc->r9;
   tst->arch.vex.guest_R10     = sc->r10;
   tst->arch.vex.guest_R11     = sc->r11;
   tst->arch.vex.guest_R12     = sc->r12;
   tst->arch.vex.guest_R13     = sc->r13;
   tst->arch.vex.guest_R14     = sc->r14;
   tst->arch.vex.guest_R15     = sc->r15;
   /*
      XXX:
      tst->arch.vex.guest_rflags  = sc->rflags;
   */
   tst->arch.vex.guest_RIP     = sc->rip;
   /*
      XXX
      tst->arch.vex.guest_CS      = sc->cs;
      tst->arch.vex.guest_SS      = sc->ss;
   */
   VG_(memcpy)(fpstate, &sc->fpstate, sizeof(*fpstate));
}

static
SizeT restore_sigframe ( ThreadState *tst,
                         struct sigframe *frame, Int *sigNo )
{
   if (restore_vg_sigframe(tst, &frame->vg, sigNo)) {
      restore_sigcontext(tst, &frame->uContext.uc_mcontext, &frame->fpstate);
   }

   return sizeof(*frame);
}

void VG_(sigframe_destroy)( ThreadId tid )
{
   Addr          rsp;
   ThreadState*  tst;
   SizeT  size;
   Int       sigNo;

   tst = VG_(get_ThreadState)(tid);

   /* Correctly reestablish the frame base address. */
   rsp   = tst->arch.vex.guest_RSP;

   size = restore_sigframe(tst, (struct sigframe *)rsp, &sigNo);

   VG_TRACK( die_mem_stack_signal, rsp - VG_STACK_REDZONE_SZB,
             size + VG_STACK_REDZONE_SZB );

   if (VG_(clo_trace_signals)) {
      VG_(message)(
         Vg_DebugMsg,
         "VG_(signal_return) (thread %u): valid magic; RIP=%#llx\n",
         tid, tst->arch.vex.guest_RIP);
   }

   /* tell the tools */
   VG_TRACK( post_deliver_signal, tid, sigNo );
}

#endif // defined(VGP_amd64_freebsd)

/*--------------------------------------------------------------------*/
/*--- end                                                          ---*/
/*--------------------------------------------------------------------*/
