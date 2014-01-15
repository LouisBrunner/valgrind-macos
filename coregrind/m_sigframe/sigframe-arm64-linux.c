
/*--------------------------------------------------------------------*/
/*--- Create/destroy signal delivery frames.                       ---*/
/*---                                       sigframe-arm64-linux.c ---*/
/*--------------------------------------------------------------------*/

/*
   This file is part of Valgrind, a dynamic binary instrumentation
   framework.

   Copyright (C) 2013-2013 OpenWorks
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
   along with this program; if not, write to the Free Software
   Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
   02111-1307, USA.

   The GNU General Public License is contained in the file COPYING.
*/

#if defined(VGP_arm64_linux)

#include "pub_core_basics.h"
#include "pub_core_vki.h"
//ZZ #include "pub_core_vkiscnums.h"
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
//ZZ #include "pub_core_transtab.h"      // VG_(discard_translations)


/* This uses the hack of dumping the vex guest state along with both
   shadows in the frame, and restoring it afterwards from there,
   rather than pulling it out of the ucontext.  That means that signal
   handlers which modify the ucontext and then return, expecting their
   modifications to take effect, will have those modifications
   ignored.  This could be fixed properly with an hour or so more
   effort. */

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
                            struct vki_ucontext *uc) 
{

   ThreadState *tst = VG_(get_ThreadState)(tid);
   struct vki_sigcontext *sc = &uc->uc_mcontext;

   VG_(memset)(uc, 0, sizeof(*uc));

   uc->uc_flags = 0;
   uc->uc_link = 0;
   uc->uc_sigmask = *set;
   uc->uc_stack = tst->altstack;

#  define SC2(reg)  sc->regs[reg] = tst->arch.vex.guest_X##reg
   SC2(0);   SC2(1);   SC2(2);   SC2(3);
   SC2(4);   SC2(5);   SC2(6);   SC2(7);
   SC2(8);   SC2(9);   SC2(10);  SC2(11);
   SC2(12);  SC2(13);  SC2(14);  SC2(15);
   SC2(16);  SC2(17);  SC2(18);  SC2(19);
   SC2(20);  SC2(21);  SC2(22);  SC2(23);
   SC2(24);  SC2(25);  SC2(26);  SC2(27);
   SC2(28);  SC2(29);  SC2(30);
#  undef SC2
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

   if (!extend(tst, sp, size))
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
   ThreadState *tst;
   struct vg_sig_private *priv;
   Addr sp;
   UInt frame_size;
//ZZ    struct vki_sigcontext *mc;
   Int sigNo;
   Bool has_siginfo = isRT;

   vg_assert(VG_(is_valid_tid)(tid));
   tst = VG_(get_ThreadState)(tid);
   sp = tst->arch.vex.guest_XSP;

//ZZ    if (has_siginfo) {
      struct rt_sigframe *frame = (struct rt_sigframe *)sp;
      frame_size = sizeof(*frame);
      //mc = &frame->sig.uc.uc_mcontext;
      priv = &frame->sig.vp;
      vg_assert(priv->magicPI == 0x31415927);
      tst->sig_mask = frame->sig.uc.uc_sigmask;
//ZZ    } else {
//ZZ       struct sigframe *frame = (struct sigframe *)sp;
//ZZ       frame_size = sizeof(*frame);
//ZZ       mc = &frame->uc.uc_mcontext;
//ZZ       priv = &frame->vp;
//ZZ       vg_assert(priv->magicPI == 0x31415927);
//ZZ       tst->sig_mask = frame->uc.uc_sigmask;
//ZZ       //VG_(printf)("Setting signmask to %08x%08x\n",
//ZZ       //            tst->sig_mask[0],tst->sig_mask[1]);
//ZZ    }
   tst->tmp_sig_mask = tst->sig_mask;

   sigNo = priv->sigNo_private;

//ZZ     //XXX: restore regs
//ZZ #  define REST(reg,REG)  tst->arch.vex.guest_##REG = mc->arm_##reg;
//ZZ    REST(r0,R0);
//ZZ    REST(r1,R1);
//ZZ    REST(r2,R2);
//ZZ    REST(r3,R3);
//ZZ    REST(r4,R4);
//ZZ    REST(r5,R5);
//ZZ    REST(r6,R6);
//ZZ    REST(r7,R7);
//ZZ    REST(r8,R8);
//ZZ    REST(r9,R9);
//ZZ    REST(r10,R10);
//ZZ    REST(fp,R11);
//ZZ    REST(ip,R12);
//ZZ    REST(sp,R13);
//ZZ    REST(lr,R14);
//ZZ    REST(pc,R15T);
//ZZ #  undef REST

   /* Uh, the next line makes all the REST() above pointless. */
   tst->arch.vex         = priv->vex;

   tst->arch.vex_shadow1 = priv->vex_shadow1;
   tst->arch.vex_shadow2 = priv->vex_shadow2;

   VG_TRACK( die_mem_stack_signal, sp - VG_STACK_REDZONE_SZB,
             frame_size + VG_STACK_REDZONE_SZB );
             
   if (VG_(clo_trace_signals))
      VG_(message)(Vg_DebugMsg,
                   "vg_pop_signal_frame (thread %d): "
                   "isRT=%d valid magic; PC=%#llx\n",
                   tid, has_siginfo, tst->arch.vex.guest_PC);

   /* tell the tools */
   VG_TRACK( post_deliver_signal, tid, sigNo );
}

#endif // defined(VGP_arm_linux)

/*--------------------------------------------------------------------*/
/*--- end                                   sigframe-arm64-linux.c ---*/
/*--------------------------------------------------------------------*/
