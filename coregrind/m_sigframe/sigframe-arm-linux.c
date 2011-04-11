
/*--------------------------------------------------------------------*/
/*--- Create/destroy signal delivery frames.                       ---*/
/*---                                         sigframe-arm-linux.c ---*/
/*--------------------------------------------------------------------*/

/*
   This file is part of Valgrind, a dynamic binary instrumentation
   framework.

   Copyright (C) 2000-2010 Nicholas Nethercote
      njn@valgrind.org
   Copyright (C) 2004-2010 Paul Mackerras
      paulus@samba.org
   Copyright (C) 2008-2010 Evan Geller
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
   along with this program; if not, write to the Free Software
   Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
   02111-1307, USA.

   The GNU General Public License is contained in the file COPYING.
*/

#if defined(VGP_arm_linux)

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


struct vg_sig_private {
   UInt magicPI;
   UInt sigNo_private;
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
         "Can't extend stack to %#lx during signal delivery for thread %d:",
         addr, tid);
      if (stackseg == NULL)
         VG_(message)(Vg_UserMsg, "  no stack segment");
      else
         VG_(message)(Vg_UserMsg, "  too small or bad protection modes");

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

#  define SC2(reg,REG)  sc->arm_##reg = tst->arch.vex.guest_##REG
   SC2(r0,R0);
   SC2(r1,R1);
   SC2(r2,R2);
   SC2(r3,R3);
   SC2(r4,R4);
   SC2(r5,R5);
   SC2(r6,R6);
   SC2(r7,R7);
   SC2(r8,R8);
   SC2(r9,R9);
   SC2(r10,R10);
   SC2(fp,R11);
   SC2(ip,R12);
   SC2(sp,R13);
   SC2(lr,R14);
   SC2(pc,R15T);
#  undef SC2

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
   priv->vex_shadow1 = tst->arch.vex_shadow1;
   priv->vex_shadow2 = tst->arch.vex_shadow2;

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
//   struct vg_sig_private *priv;
   Addr sp = sp_top_of_frame;
   ThreadState *tst;
   Int sigNo = siginfo->si_signo;
//   Addr faultaddr;
   UInt size;

   tst = VG_(get_ThreadState)(tid);

   size = flags & VKI_SA_SIGINFO ? sizeof(struct rt_sigframe) :
      sizeof(struct sigframe);

   sp -= size;
   sp = VG_ROUNDDN(sp, 16);

   if(!extend(tst, sp, size))
      I_die_here; // XXX Incorrect behavior


   if (flags & VKI_SA_SIGINFO){
      struct rt_sigframe *rsf = (struct rt_sigframe *)sp;
      
      /* Track our writes to siginfo */
      VG_TRACK( pre_mem_write, Vg_CoreSignal, tst->tid,  /* VVVVV */
            "signal handler siginfo", (Addr)rsf, 
            offsetof(struct rt_sigframe, sig));

      VG_(memcpy)(&rsf->info, siginfo, sizeof(vki_siginfo_t));

      if(sigNo == VKI_SIGILL && siginfo->si_code > 0) {
         rsf->info._sifields._sigfault._addr = (Addr *) (tst)->arch.vex.guest_R12; /* IP */
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
       tst->arch.vex.guest_R14 = (Addr) restorer; 

   tst->arch.vex.guest_R15T = (Addr) handler; /* R15 == PC */
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
   struct vki_sigcontext *mc;
   Int sigNo;
   Bool has_siginfo = isRT;

   vg_assert(VG_(is_valid_tid)(tid));
   tst = VG_(get_ThreadState)(tid);
   sp = tst->arch.vex.guest_R13;

   if (has_siginfo) {
      struct rt_sigframe *frame = (struct rt_sigframe *)sp;
      frame_size = sizeof(*frame);
      mc = &frame->sig.uc.uc_mcontext;
      priv = &frame->sig.vp;
      vg_assert(priv->magicPI == 0x31415927);
      tst->sig_mask = frame->sig.uc.uc_sigmask;
   } else {
      struct sigframe *frame = (struct sigframe *)sp;
      frame_size = sizeof(*frame);
      mc = &frame->uc.uc_mcontext;
      priv = &frame->vp;
      vg_assert(priv->magicPI == 0x31415927);
      tst->sig_mask = frame->uc.uc_sigmask;
      /*tst->sig_mask.sig[0] = frame->uc.uc_mcontext.oldmask;
      tst->sig_mask.sig[1] = frame->uc.uc_mcontext._unused[3];
      VG_(printf)("Setting signmask to %08x%08x\n",tst->sig_mask[0],tst->sig_mask[1]);
*/
   }
   tst->tmp_sig_mask = tst->sig_mask;

   sigNo = priv->sigNo_private;

    //XXX: restore regs
#  define REST(reg,REG)  tst->arch.vex.guest_##REG = mc->arm_##reg;
   REST(r0,R0);
   REST(r1,R1);
   REST(r2,R2);
   REST(r3,R3);
   REST(r4,R4);
   REST(r5,R5);
   REST(r6,R6);
   REST(r7,R7);
   REST(r8,R8);
   REST(r9,R9);
   REST(r10,R10);
   REST(fp,R11);
   REST(ip,R12);
   REST(sp,R13);
   REST(lr,R14);
   REST(pc,R15T);
#  undef REST

   tst->arch.vex_shadow1 = priv->vex_shadow1;
   tst->arch.vex_shadow2 = priv->vex_shadow2;

   VG_TRACK( die_mem_stack_signal, sp - VG_STACK_REDZONE_SZB,
             frame_size + VG_STACK_REDZONE_SZB );
             
   if (VG_(clo_trace_signals))
      VG_(message)(Vg_DebugMsg,
                   "vg_pop_signal_frame (thread %d): "
                   "isRT=%d valid magic; PC=%#x",
                   tid, has_siginfo, tst->arch.vex.guest_R15T);

   /* tell the tools */
   VG_TRACK( post_deliver_signal, tid, sigNo );
}

#endif // defined(VGP_arm_linux)

/*--------------------------------------------------------------------*/
/*--- end                                     sigframe-arm-linux.c ---*/
/*--------------------------------------------------------------------*/
