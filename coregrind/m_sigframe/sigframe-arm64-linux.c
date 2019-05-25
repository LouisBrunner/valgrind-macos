
/*--------------------------------------------------------------------*/
/*--- Create/destroy signal delivery frames.                       ---*/
/*---                                       sigframe-arm64-linux.c ---*/
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

#if defined(VGP_arm64_linux)

#include "pub_core_basics.h"
#include "pub_core_vki.h"
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


/* This uses the hack of dumping the vex guest state along with both
   shadows in the frame, and restoring it afterwards from there,
   rather than pulling it out of the ucontext.  Then, integer
   registers, the SP and the PC are restored from the ucontext.  That
   means that signal handlers which modify floating point registers or
   in general any register state apart from X0-X30, XSP and PC in the
   ucontext and then return, expecting their modifications to take
   effect, will have those modifications ignored. */

/* This also always does the 'has siginfo' behaviour whether or
   not it is requested. */

struct vg_sig_private {
   UInt magicPI;
   UInt sigNo_private;
   VexGuestARM64State vex;
   VexGuestARM64State vex_shadow1;
   VexGuestARM64State vex_shadow2;
};

struct sigframe {
   struct vki_ucontext uc;
   unsigned long retcode[2];
   struct vg_sig_private vp;
};

struct rt_sigframe {
   vki_siginfo_t info;
   struct sigframe sig;
};


static void synth_ucontext( ThreadId tid, const vki_siginfo_t *si,
                            UWord trapno, UWord err, const vki_sigset_t *set, 
                            struct vki_ucontext *uc) 
{

   ThreadState *tst = VG_(get_ThreadState)(tid);
   struct vki_sigcontext *sc = &uc->uc_mcontext;

   VG_(memset)(uc, 0, sizeof(*uc));

   uc->uc_flags = 0;
   uc->uc_link = 0;
   uc->uc_sigmask = *set;
   uc->uc_stack = tst->altstack;

#  define TO_CTX(reg)  sc->regs[reg] = tst->arch.vex.guest_X##reg
   TO_CTX(0);   TO_CTX(1);   TO_CTX(2);   TO_CTX(3);
   TO_CTX(4);   TO_CTX(5);   TO_CTX(6);   TO_CTX(7);
   TO_CTX(8);   TO_CTX(9);   TO_CTX(10);  TO_CTX(11);
   TO_CTX(12);  TO_CTX(13);  TO_CTX(14);  TO_CTX(15);
   TO_CTX(16);  TO_CTX(17);  TO_CTX(18);  TO_CTX(19);
   TO_CTX(20);  TO_CTX(21);  TO_CTX(22);  TO_CTX(23);
   TO_CTX(24);  TO_CTX(25);  TO_CTX(26);  TO_CTX(27);
   TO_CTX(28);  TO_CTX(29);  TO_CTX(30);
#  undef TO_CTX
   sc->sp = tst->arch.vex.guest_XSP;
   sc->pc = tst->arch.vex.guest_PC;
   sc->pstate = 0; /* slack .. could do better */

   //sc->trap_no = trapno;
   //sc->error_code = err;
   sc->fault_address = (ULong)si->_sifields._sigfault._addr;
}


static void build_sigframe(ThreadState *tst,
                           struct sigframe *frame,
                           const vki_siginfo_t *siginfo,
                           const struct vki_ucontext *siguc,
                           void *handler, UInt flags,
                           const vki_sigset_t *mask,
                           void *restorer)
{
   UWord trapno;
   UWord err;
   Int   sigNo = siginfo->si_signo;
   struct vg_sig_private *priv = &frame->vp;

   VG_TRACK( pre_mem_write, Vg_CoreSignal, tst->tid, "signal handler frame",
             (Addr)frame, offsetof(struct sigframe, vp));

   if (siguc) {
      trapno = 0; //siguc->uc_mcontext.trap_no;
      err = 0; //siguc->uc_mcontext.error_code;
   } else {
      trapno = 0;
      err = 0;
   }

   synth_ucontext(tst->tid, siginfo, trapno, err, mask, &frame->uc);

   VG_TRACK( post_mem_write, Vg_CoreSignal, tst->tid,
             (Addr)frame, offsetof(struct sigframe, vp));

   priv->magicPI = 0x31415927;
   priv->sigNo_private = sigNo;
   priv->vex         = tst->arch.vex;
   priv->vex_shadow1 = tst->arch.vex_shadow1;
   priv->vex_shadow2 = tst->arch.vex_shadow2;
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
   Int  sigNo = siginfo->si_signo;
   UInt size;

   tst = VG_(get_ThreadState)(tid);

   size = sizeof(struct rt_sigframe);

   sp -= size;
   sp = VG_ROUNDDN(sp, 16);

   if (! ML_(sf_maybe_extend_stack)(tst, sp, size, flags))
      return; // Give up.  No idea if this is correct

   struct rt_sigframe *rsf = (struct rt_sigframe *)sp;
      
   /* Track our writes to siginfo */
   VG_TRACK( pre_mem_write, Vg_CoreSignal, tst->tid,  /* VVVVV */
             "signal handler siginfo", (Addr)rsf, 
             offsetof(struct rt_sigframe, sig));

   VG_(memcpy)(&rsf->info, siginfo, sizeof(vki_siginfo_t));

   if (sigNo == VKI_SIGILL && siginfo->si_code > 0) {
      rsf->info._sifields._sigfault._addr
        = (Addr*)(tst)->arch.vex.guest_PC;
   }
   VG_TRACK( post_mem_write, Vg_CoreSignal, tst->tid, /* ^^^^^ */
         (Addr)rsf, offsetof(struct rt_sigframe, sig));

   build_sigframe(tst, &rsf->sig, siginfo, siguc,
                       handler, flags, mask, restorer);
   tst->arch.vex.guest_X1 = (Addr)&rsf->info;
   tst->arch.vex.guest_X2 = (Addr)&rsf->sig.uc;

   VG_(set_SP)(tid, sp);
   VG_TRACK( post_reg_write, Vg_CoreSignal, tid, VG_O_STACK_PTR,
             sizeof(Addr));
   tst->arch.vex.guest_X0 = sigNo; 

   if (flags & VKI_SA_RESTORER)
       tst->arch.vex.guest_X30 = (Addr)restorer; 
   else
       tst->arch.vex.guest_X30
          = (Addr)&VG_(arm64_linux_SUBST_FOR_rt_sigreturn);

   tst->arch.vex.guest_PC = (Addr)handler;
}


/*------------------------------------------------------------*/
/*--- Destroying signal frames                             ---*/
/*------------------------------------------------------------*/

/* EXPORTED */
void VG_(sigframe_destroy)( ThreadId tid, Bool isRT )
{
   vg_assert(VG_(is_valid_tid)(tid));

   Bool         has_siginfo = isRT;
   ThreadState* tst         = VG_(get_ThreadState)(tid);
   Addr         sp          = tst->arch.vex.guest_XSP;

   struct rt_sigframe*    frame = (struct rt_sigframe *)sp;
   struct vg_sig_private* priv  = &frame->sig.vp;
   vg_assert(priv->magicPI == 0x31415927);

   tst->sig_mask     = frame->sig.uc.uc_sigmask;
   tst->tmp_sig_mask = tst->sig_mask;

   Int  sigNo      = priv->sigNo_private;
   UInt frame_size = sizeof(*frame);

   /* Restore the entire machine state from our private copy.  This
      isn't really right, but we'll now move on to pick up at least
      some changes that the signal handler may have made to the
      sigcontext. */
   tst->arch.vex         = priv->vex;
   tst->arch.vex_shadow1 = priv->vex_shadow1;
   tst->arch.vex_shadow2 = priv->vex_shadow2;

   if (has_siginfo) {
      /* Pick up at least some state changes from the ucontext, just
         in case the handler changed it.  The shadow values will be
         wrong, but hey.  This restores the integer registers, the
         program counter and stack pointer.  FP/Vector regs, and any
         condition code, FP status/control bits, etc, are not
         restored. */
      struct vki_sigcontext *sc =&frame->sig.uc.uc_mcontext;
#     define FROM_CTX(reg)  tst->arch.vex.guest_X##reg = sc->regs[reg]
      FROM_CTX(0);   FROM_CTX(1);   FROM_CTX(2);   FROM_CTX(3);
      FROM_CTX(4);   FROM_CTX(5);   FROM_CTX(6);   FROM_CTX(7);
      FROM_CTX(8);   FROM_CTX(9);   FROM_CTX(10);  FROM_CTX(11);
      FROM_CTX(12);  FROM_CTX(13);  FROM_CTX(14);  FROM_CTX(15);
      FROM_CTX(16);  FROM_CTX(17);  FROM_CTX(18);  FROM_CTX(19);
      FROM_CTX(20);  FROM_CTX(21);  FROM_CTX(22);  FROM_CTX(23);
      FROM_CTX(24);  FROM_CTX(25);  FROM_CTX(26);  FROM_CTX(27);
      FROM_CTX(28);  FROM_CTX(29);  FROM_CTX(30);
#     undef FROM_CTX
      tst->arch.vex.guest_XSP = sc->sp; // should we use SET_SP here?
      tst->arch.vex.guest_PC  = sc->pc;
   }

   VG_TRACK( die_mem_stack_signal, sp - VG_STACK_REDZONE_SZB,
             frame_size + VG_STACK_REDZONE_SZB );
             
   if (VG_(clo_trace_signals))
      VG_(message)(Vg_DebugMsg,
                   "vg_pop_signal_frame (thread %u): "
                   "isRT=%d valid magic; PC=%#llx\n",
                   tid, has_siginfo, tst->arch.vex.guest_PC);

   /* tell the tools */
   VG_TRACK( post_deliver_signal, tid, sigNo );
}

#endif // defined(VGP_arm_linux)

/*--------------------------------------------------------------------*/
/*--- end                                   sigframe-arm64-linux.c ---*/
/*--------------------------------------------------------------------*/
