
/*--------------------------------------------------------------------*/
/*--- Create/destroy signal delivery frames.                       ---*/
/*---                                       sigframe-x86-freebsd.c ---*/
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

#if defined(VGP_x86_freebsd)

#include "pub_core_basics.h"
#include "pub_core_vki.h"
#include "pub_core_libcsetjmp.h"    // to keep _threadstate.h happy
#include "pub_core_threadstate.h"
#include "pub_core_aspacemgr.h" /* find_segment */
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
   on x86-freebsd.
*/


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
   VexGuestX86State vex_shadow1;
   VexGuestX86State vex_shadow2;

   /* HACK ALERT */
   VexGuestX86State vex;
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

   /*
    * The following 7 members are roughly the same as
    * 'struct sigframe' in x86/sigframe.h
    */
   Int  sigNo;
   Addr psigInfo;      /* code or pointer to sigContext */
   Addr puContext;     /* points to uContext */
   Addr addr;          /* "secret" 4th argument */
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
                    struct vki_ucontext *uc, struct _vki_fpstate *fpstate)
{
   ThreadState *tst = VG_(get_ThreadState)(tid);
   struct vki_mcontext *sc = &uc->uc_mcontext;

   VG_(memset)(uc, 0, sizeof(*uc));

   uc->uc_flags = 0;
   uc->uc_link = 0;
   uc->uc_sigmask = *set;
   uc->uc_stack = tst->altstack;
   VG_(memcpy)(&sc->fpstate, fpstate, sizeof(*fpstate));

#  define SC2(reg,REG)  sc->reg = tst->arch.vex.guest_##REG
   SC2(gs,GS);
   SC2(fs,FS);
   SC2(es,ES);
   SC2(ds,DS);

   SC2(edi,EDI);
   SC2(esi,ESI);
   SC2(ebp,EBP);
   SC2(esp,ESP);
   SC2(ebx,EBX);
   SC2(edx,EDX);
   SC2(ecx,ECX);
   SC2(eax,EAX);

   SC2(eip,EIP);
   SC2(cs,CS);
   sc->eflags = LibVEX_GuestX86_get_eflags(&tst->arch.vex);
   SC2(ss,SS);
   sc->trapno = trapno;
   sc->err = err;
//   sc->addr = (UWord)si->si_addr;
   sc->fpformat = VKI_FPFMT_NODEV;
   sc->len = sizeof(*sc);
   sc->ownedfp = VKI_FPOWNED_NONE;
#  undef SC2

//   sc->cr2 = (UInt)si->_sifields._sigfault._addr;
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
      if (0 && stackseg)
         VG_(printf)("frame=%#lx seg=%#lx-%#lx\n",
                     addr, stackseg->start, stackseg->end);
   }

   if (stackseg == NULL || !stackseg->hasR || !stackseg->hasW) {
      VG_(message)(
         Vg_UserMsg,
         "Can't extend stack to %#lx during signal delivery for thread %u:\n",
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


/* Build the Valgrind-specific part of a signal frame. */

static void build_vg_sigframe(struct vg_sigframe *frame,
                              ThreadState *tst,
                              const vki_sigset_t *mask,
                              UInt flags,
                              Int sigNo)
{
   frame->sigNo_private = sigNo;
   frame->magicPI       = 0x31415927;
   frame->vex_shadow1   = tst->arch.vex_shadow1;
   frame->vex_shadow2   = tst->arch.vex_shadow2;
   /* HACK ALERT */
   frame->vex           = tst->arch.vex;
   /* end HACK ALERT */
   frame->mask          = tst->sig_mask;
   frame->handlerflags  = flags;
   frame->magicE        = 0x27182818;
}

/*
 * According to the comments in lib/libc/i386/gen/signalcontext.c
 * the stack sould look like this [where n = 4 = sizeof(int)]
 *
 * 2n+sizeof(struct sigframe) ucp
 * 2n          struct sigframe
 * 1n          &func
 * 0n          &_ctx_start
 *
 * Note that the 'struct sigframe' above is the one defined in FreeBSD.
 * 5 word members + ucontext + siginfo
 *
 * 'struct sigframe' below is the one defined in this file:
 *
 * Addr
 * FreeBSD struct sigfame
 * fpstate
 * vg_sigframe
 *
 */
static Addr build_sigframe(ThreadState *tst,
                           Addr esp_top_of_frame,
                           const vki_siginfo_t *siginfo,
                           const struct vki_ucontext *siguc,
                           void *handler, UInt flags,
                           const vki_sigset_t *mask,
                           void *restorer)
{
   struct sigframe *frame;
   Addr esp = esp_top_of_frame;
   Int  sigNo = siginfo->si_signo;
   UWord trapno;
   UWord err;

#if defined(__clang__)
   esp -= 4;
   esp = VG_ROUNDDN(esp, 16);
   esp -= sizeof(*frame) + 4;
#else
   esp -= sizeof(*frame);
   esp = VG_ROUNDDN(esp, 16);
#endif

   frame = (struct sigframe *)esp;

   if (!extend(tst, esp, sizeof(*frame)))
      return esp_top_of_frame;

   /* retaddr, siginfo, uContext fields are to be written */
   VG_TRACK( pre_mem_write, Vg_CoreSignal, tst->tid, "signal handler frame",
             esp, offsetof(struct sigframe, vg) );

   frame->sigNo = sigNo;
   frame->retaddr = (Addr)&VG_(x86_freebsd_SUBST_FOR_sigreturn);
   if ((flags & VKI_SA_SIGINFO) == 0)
      frame->psigInfo = (Addr)siginfo->si_code;
   else
      frame->psigInfo = (Addr)&frame->sigInfo;
   VG_(memcpy)(&frame->sigInfo, siginfo, sizeof(vki_siginfo_t));

   if (siguc != NULL) {
      trapno = siguc->uc_mcontext.trapno;
      err = siguc->uc_mcontext.err;
   } else {
      trapno = 0;
      err = 0;
   }

   frame->puContext =  (Addr)&frame->uContext;

   synth_ucontext(tst->tid, siginfo, trapno, err, mask,
                  &frame->uContext, &frame->fpstate);

   if (sigNo == VKI_SIGILL && siginfo->si_code > 0)
      frame->sigInfo.si_addr = (void*)tst->arch.vex.guest_EIP;

   VG_TRACK( post_mem_write,  Vg_CoreSignal, tst->tid,
             esp, offsetof(struct sigframe, vg) );

   build_vg_sigframe(&frame->vg, tst, mask, flags, sigNo);

   return esp;
}

/* EXPORTED */
void VG_(sigframe_create)( ThreadId tid,
                           Bool on_altstack,
                           Addr esp_top_of_frame,
                           const vki_siginfo_t *siginfo,
                           const struct vki_ucontext *siguc,
                           void *handler,
                           UInt flags,
                           const vki_sigset_t *mask,
                           void *restorer )
{
   Addr     esp;
   struct sigframe *frame;
   ThreadState* tst = VG_(get_ThreadState)(tid);

   esp = build_sigframe(tst, esp_top_of_frame, siginfo, siguc, handler,
                        flags, mask, restorer);
   frame = (struct sigframe *)esp;

   /* Set the thread so it will next run the handler. */
   /* tst->m_esp  = esp;  also notify the tool we've updated ESP */
   VG_(set_SP)(tid, esp);
   VG_TRACK( post_reg_write, Vg_CoreSignal, tid, VG_O_STACK_PTR, sizeof(Addr));

   tst->arch.vex.guest_EIP = (Addr) handler;
   tst->arch.vex.guest_EDI = (ULong) siginfo->si_signo;
   tst->arch.vex.guest_ESI = (Addr) &frame->sigInfo;
   tst->arch.vex.guest_EDX = (Addr) &frame->uContext;
   /* This thread needs to be marked runnable, but we leave that the
      caller to do. */

   if (0)
      VG_(printf)("pushed signal frame; %%ESP now = %#lx, "
                  "next %%EIP = %#x, status=%u\n",
                  esp, tst->arch.vex.guest_EIP, tst->status);
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
   if (frame->magicPI != 0x31415927 ||
         frame->magicE  != 0x27182818) {
      VG_(message)(Vg_UserMsg, "Thread %u return signal frame "
                   "corrupted.  Killing process.", tst->tid);
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
   tst->arch.vex.guest_EAX     = sc->eax;
   tst->arch.vex.guest_ECX     = sc->ecx;
   tst->arch.vex.guest_EDX     = sc->edx;
   tst->arch.vex.guest_EBX     = sc->ebx;
   tst->arch.vex.guest_EBP     = sc->ebp;
   tst->arch.vex.guest_ESP     = sc->esp;
   tst->arch.vex.guest_ESI     = sc->esi;
   tst->arch.vex.guest_EDI     = sc->edi;
//::    tst->arch.vex.guest_eflags  = sc->eflags;
   tst->arch.vex.guest_EIP     = sc->eip;
   tst->arch.vex.guest_CS      = sc->cs;
   tst->arch.vex.guest_SS      = sc->ss;
   tst->arch.vex.guest_DS      = sc->ds;
   tst->arch.vex.guest_ES      = sc->es;
   tst->arch.vex.guest_FS      = sc->fs;
   tst->arch.vex.guest_GS      = sc->gs;
   VG_(memcpy)(fpstate, &sc->fpstate, sizeof(*fpstate));
}


static
SizeT restore_sigframe ( ThreadState *tst,
                         struct sigframe *frame, Int *sigNo )
{
   if (restore_vg_sigframe(tst, &frame->vg, sigNo))
      restore_sigcontext(tst, &frame->uContext.uc_mcontext, &frame->fpstate);

   return sizeof(*frame);
}

/* EXPORTED */
void VG_(sigframe_destroy)( ThreadId tid )
{
   Addr          esp;
   ThreadState*  tst;
   SizeT  size;
   Int       sigNo;

   tst = VG_(get_ThreadState)(tid);

   /* Correctly reestablish the frame base address. */
   esp   = tst->arch.vex.guest_ESP;
   esp  += 8; /* Clean up stack from argument/ret passed to sigreturn(2) */

   size = restore_sigframe(tst, (struct sigframe *)esp, &sigNo);

   VG_TRACK( die_mem_stack_signal, esp - VG_STACK_REDZONE_SZB,
             size + VG_STACK_REDZONE_SZB );

   if (VG_(clo_trace_signals))
      VG_(message)(
         Vg_DebugMsg,
         "VG_(signal_return) (thread %u): EIP=%#x\n",
         tid, tst->arch.vex.guest_EIP);

   /* tell the tools */
   VG_TRACK( post_deliver_signal, tid, sigNo );
}

#endif // defined(VGP_x86_freebsd)

/*--------------------------------------------------------------------*/
/*--- end                                                          ---*/
/*--------------------------------------------------------------------*/
