
/*--------------------------------------------------------------------*/
/*--- Create/destroy signal delivery frames.                       ---*/
/*---                                         sigframe-arm-linux.c ---*/
/*--------------------------------------------------------------------*/

/*
   This file is part of Valgrind, a dynamic binary instrumentation
   framework.

   Copyright (C) 2000-2017 Nicholas Nethercote
      njn@valgrind.org
   Copyright (C) 2004-2017 Paul Mackerras
      paulus@samba.org
   Copyright (C) 2008-2017 Evan Geller
      gaze@bea.ms

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

#if defined(VGP_arm_linux)

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
   in general any register state apart from R0 to R15 in the ucontext
   and then return, expecting their modifications to take effect, will
   have those modifications ignored. */

struct vg_sig_private {
   UInt magicPI;
   UInt sigNo_private;
   VexGuestARMState vex;
   VexGuestARMState vex_shadow1;
   VexGuestARMState vex_shadow2;
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
                    struct vki_ucontext *uc){

   ThreadState *tst = VG_(get_ThreadState)(tid);
   struct vki_sigcontext *sc = &uc->uc_mcontext;

   VG_(memset)(uc, 0, sizeof(*uc));

   uc->uc_flags = 0;
   uc->uc_link = 0;
   uc->uc_sigmask = *set;
   uc->uc_stack = tst->altstack;

#  define TO_CTX(reg,REG)  sc->arm_##reg = tst->arch.vex.guest_##REG
   TO_CTX(r0,R0);   TO_CTX(r1,R1);   TO_CTX(r2,R2);   TO_CTX(r3,R3);
   TO_CTX(r4,R4);   TO_CTX(r5,R5);   TO_CTX(r6,R6);   TO_CTX(r7,R7);
   TO_CTX(r8,R8);   TO_CTX(r9,R9);   TO_CTX(r10,R10); TO_CTX(fp,R11);
   TO_CTX(ip,R12);  TO_CTX(sp,R13);  TO_CTX(lr,R14);  TO_CTX(pc,R15T);
#  undef TO_CTX

   sc->trap_no = trapno;
   sc->error_code = err;
   sc->fault_address = (UInt)si->_sifields._sigfault._addr;
}


static void build_sigframe(ThreadState *tst,
            struct sigframe *frame,
            const vki_siginfo_t *siginfo,
            const struct vki_ucontext *siguc,
            void *handler, UInt flags,
            const vki_sigset_t *mask,
            void *restorer){

   UWord trapno;
   UWord err;
   Int  sigNo = siginfo->si_signo;
   struct vg_sig_private *priv = &frame->vp;

   VG_TRACK( pre_mem_write, Vg_CoreSignal, tst->tid, "signal handler frame",
         (Addr)frame, offsetof(struct sigframe, vp));

   if(siguc) {
      trapno = siguc->uc_mcontext.trap_no;
      err = siguc->uc_mcontext.error_code;
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
   Addr sp = sp_top_of_frame;
   ThreadState *tst;
   Int sigNo = siginfo->si_signo;
   UInt size;

   tst = VG_(get_ThreadState)(tid);

   size = flags & VKI_SA_SIGINFO ? sizeof(struct rt_sigframe) :
      sizeof(struct sigframe);

   sp -= size;
   sp = VG_ROUNDDN(sp, 16);

   if (! ML_(sf_maybe_extend_stack)(tst, sp, size, flags))
      I_die_here; // XXX Incorrect behavior


   if (flags & VKI_SA_SIGINFO){
      struct rt_sigframe *rsf = (struct rt_sigframe *)sp;
      
      /* Track our writes to siginfo */
      VG_TRACK( pre_mem_write, Vg_CoreSignal, tst->tid,  /* VVVVV */
            "signal handler siginfo", (Addr)rsf, 
            offsetof(struct rt_sigframe, sig));

      VG_(memcpy)(&rsf->info, siginfo, sizeof(vki_siginfo_t));

      if(sigNo == VKI_SIGILL && siginfo->si_code > 0) {
         rsf->info._sifields._sigfault._addr
            = (Addr *) (tst)->arch.vex.guest_R12; /* IP */
      }
      VG_TRACK( post_mem_write, Vg_CoreSignal, tst->tid, /* ^^^^^ */
            (Addr)rsf, offsetof(struct rt_sigframe, sig));

      build_sigframe(tst, &rsf->sig, siginfo, siguc,
                             handler, flags, mask, restorer);
      tst->arch.vex.guest_R1 = (Addr)&rsf->info;
      tst->arch.vex.guest_R2 = (Addr)&rsf->sig.uc;
   }
   else {
      build_sigframe(tst, (struct sigframe *)sp, siginfo, siguc,
                             handler, flags, mask, restorer);
   }

   VG_(set_SP)(tid, sp);
   VG_TRACK( post_reg_write, Vg_CoreSignal, tid, VG_O_STACK_PTR,
         sizeof(Addr));
   tst->arch.vex.guest_R0  = sigNo; 

   if (flags & VKI_SA_RESTORER)
       tst->arch.vex.guest_R14 = (Addr)restorer; 
   else
       tst->arch.vex.guest_R14 
          = (flags & VKI_SA_SIGINFO)
            ? (Addr)&VG_(arm_linux_SUBST_FOR_rt_sigreturn)
            : (Addr)&VG_(arm_linux_SUBST_FOR_sigreturn);

   tst->arch.vex.guest_R15T = (Addr) handler; /* R15 == PC */

   /* Regardless of what the state of ITSTATE was, it makes no sense
      to enter the handler with the first 1-4 instructions possibly
      predicated as "don't execute".  So set ITSTATE to [AL,AL,AL,AL]
      before entering the handler. */
   tst->arch.vex.guest_ITSTATE = 0;

   if (VG_(clo_trace_signals))
      VG_(message)(Vg_DebugMsg,
                   "VG_(sigframe_create): continuing in handler with PC=%#lx\n",
                   (Addr)handler);
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
   Int sigNo;
   Bool has_siginfo = isRT;

   struct rt_sigframe *rt_frame = NULL;

   vg_assert(VG_(is_valid_tid)(tid));
   tst = VG_(get_ThreadState)(tid);
   sp = tst->arch.vex.guest_R13;

   if (has_siginfo) {
      struct rt_sigframe *frame = (struct rt_sigframe *)sp;
      frame_size = sizeof(*frame);
      priv = &frame->sig.vp;
      vg_assert(priv->magicPI == 0x31415927);
      tst->sig_mask = frame->sig.uc.uc_sigmask;
      rt_frame = frame;
   } else {
      struct sigframe *frame = (struct sigframe *)sp;
      frame_size = sizeof(*frame);
      priv = &frame->vp;
      vg_assert(priv->magicPI == 0x31415927);
      tst->sig_mask = frame->uc.uc_sigmask;
   }
   tst->tmp_sig_mask = tst->sig_mask;

   sigNo = priv->sigNo_private;

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
      vg_assert(rt_frame != NULL);
      struct vki_sigcontext *sc = &rt_frame->sig.uc.uc_mcontext;
      Bool handler_changed_pc = sc->arm_pc != tst->arch.vex.guest_R15T;

#     define FROM_CTX(reg,REG)  tst->arch.vex.guest_##REG = sc->arm_##reg;
      FROM_CTX(r0,R0);  FROM_CTX(r1,R1);  FROM_CTX(r2,R2);   FROM_CTX(r3,R3);
      FROM_CTX(r4,R4);  FROM_CTX(r5,R5);  FROM_CTX(r6,R6);   FROM_CTX(r7,R7);
      FROM_CTX(r8,R8);  FROM_CTX(r9,R9);  FROM_CTX(r10,R10); FROM_CTX(fp,R11);
      FROM_CTX(ip,R12); FROM_CTX(sp,R13); FROM_CTX(lr,R14);  FROM_CTX(pc,R15T);
#     undef FROM_CTX

      /* A nasty ITSTATE hack -- apparently necessary as there doesn't
         seem to be anywhere in the mcontext in which the ITSTATE is
         stored.  If the handler appears to have changed the PC, set
         ITSTATE to [AL,AL,AL,AL] on the basis that it would be nuts
         to start executing code with an ITSTATE value that pertained
         to some other code address.  Otherwise ITSTATE is restored
         directly from the VEX state that we shoved on the stack when
         creating the signal frame. */
      if (handler_changed_pc) {
         tst->arch.vex.guest_ITSTATE = 0;
      }
   }

   VG_TRACK( die_mem_stack_signal, sp - VG_STACK_REDZONE_SZB,
             frame_size + VG_STACK_REDZONE_SZB );
             
   if (VG_(clo_trace_signals))
      VG_(message)(Vg_DebugMsg,
                   "vg_pop_signal_frame (thread %u): "
                   "isRT=%d valid magic; PC=%#x\n",
                   tid, has_siginfo, tst->arch.vex.guest_R15T);

   /* tell the tools */
   VG_TRACK( post_deliver_signal, tid, sigNo );
}

#endif // defined(VGP_arm_linux)

/*--------------------------------------------------------------------*/
/*--- end                                     sigframe-arm-linux.c ---*/
/*--------------------------------------------------------------------*/
