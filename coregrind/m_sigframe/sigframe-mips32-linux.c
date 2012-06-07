
/*--------------------------------------------------------------------*/
/*--- Create/destroy signal delivery frames.                       ---*/
/*---                                  sigframe-mips32-linux.c     ---*/
/*--------------------------------------------------------------------*/

/*
   This file is part of Valgrind, a dynamic binary instrumentation
   framework.

   Copyright (C) 2010-2012 RT-RK
      mips-valgrind@rt-rk.com

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

#if defined(VGP_mips32_linux)

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

struct vg_sig_private
{
  UInt magicPI;
  UInt sigNo_private;
  VexGuestMIPS32State vex_shadow1;
  VexGuestMIPS32State vex_shadow2;
};

struct sigframe 
{
  UInt sf_ass[4];       /* argument save space for o32 */
  UInt sf_pad[2];       /* Was: signal trampoline */
  struct vki_sigcontext sf_sc;
  vki_sigset_t sf_mask;
  struct vg_sig_private priv;
};

struct rt_sigframe
{
  UInt rs_ass[4];		/* argument save space for o32 */
  UInt rs_pad[2];		/* Was: signal trampoline */
  vki_siginfo_t rs_info;
  struct vki_ucontext rs_uc;
  struct vg_sig_private priv;
};

/* Extend the stack segment downwards if needed so as to ensure the
   new signal frames are mapped to something.  Return a Bool
   indicating whether or not the operation was successful.
*/
static Bool extend ( ThreadState *tst, Addr addr, SizeT size )
{
  ThreadId        tid = tst->tid;
  NSegment const* stackseg = NULL;

  if (VG_(extend_stack)(addr, tst->client_stack_szB))
    {
      stackseg = VG_(am_find_nsegment)(addr);
   }

   if (stackseg == NULL || !stackseg->hasR || !stackseg->hasW)
     {
       VG_(message)(Vg_UserMsg,
         "Can't extend stack to %#lx during signal delivery for thread %d:\n",
         addr, tid );
       if (stackseg == NULL)
         VG_(message)( Vg_UserMsg, "  no stack segment\n" );
       else
         VG_(message)( Vg_UserMsg, "  too small or bad protection modes\n" );

       /* set SIGSEGV to default handler */
       VG_(set_default_handler)( VKI_SIGSEGV );
       VG_(synth_fault_mapping)( tid, addr );

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

static 
void setup_sigcontext2 ( ThreadState* tst, struct vki_sigcontext **sc1, const vki_siginfo_t *si)
{

  struct vki_sigcontext *sc = *sc1;

  VG_TRACK( pre_mem_write, Vg_CoreSignal, tst->tid, "signal frame mcontext",
           (Addr)sc, sizeof(unsigned long long)*34 );       
  sc->sc_regs[1] = tst->arch.vex.guest_r1;
  sc->sc_regs[2] = tst->arch.vex.guest_r2;
  sc->sc_regs[3] = tst->arch.vex.guest_r3;
  sc->sc_regs[4] = tst->arch.vex.guest_r4;
  sc->sc_regs[5] = tst->arch.vex.guest_r5;
  sc->sc_regs[6] = tst->arch.vex.guest_r6;
  sc->sc_regs[7] = tst->arch.vex.guest_r7;
  sc->sc_regs[8] = tst->arch.vex.guest_r8;
  sc->sc_regs[9] = tst->arch.vex.guest_r9;
  sc->sc_regs[10] = tst->arch.vex.guest_r10;
  sc->sc_regs[11] = tst->arch.vex.guest_r11;
  sc->sc_regs[12] = tst->arch.vex.guest_r12;
  sc->sc_regs[13] = tst->arch.vex.guest_r13;
  sc->sc_regs[14] = tst->arch.vex.guest_r14;
  sc->sc_regs[15] = tst->arch.vex.guest_r15;
  sc->sc_regs[16] = tst->arch.vex.guest_r16;
  sc->sc_regs[17] = tst->arch.vex.guest_r17;
  sc->sc_regs[18] = tst->arch.vex.guest_r18;
  sc->sc_regs[19] = tst->arch.vex.guest_r19;
  sc->sc_regs[20] = tst->arch.vex.guest_r20;
  sc->sc_regs[21] = tst->arch.vex.guest_r21;
  sc->sc_regs[22] = tst->arch.vex.guest_r22;
  sc->sc_regs[23] = tst->arch.vex.guest_r23;
  sc->sc_regs[24] = tst->arch.vex.guest_r24;
  sc->sc_regs[25] = tst->arch.vex.guest_r25;
  sc->sc_regs[26] = tst->arch.vex.guest_r26;
  sc->sc_regs[27] = tst->arch.vex.guest_r27;
  sc->sc_regs[28] = tst->arch.vex.guest_r28;
  sc->sc_regs[29] = tst->arch.vex.guest_r29;
  sc->sc_regs[30] = tst->arch.vex.guest_r30;
  sc->sc_regs[31] = tst->arch.vex.guest_r31;
  sc->sc_pc = tst->arch.vex.guest_PC;
  sc->sc_mdhi = tst->arch.vex.guest_HI;
  sc->sc_mdlo = tst->arch.vex.guest_LO;
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
  Addr sp;
  ThreadState* tst = VG_(get_ThreadState)(tid);
  Addr faultaddr;
  Int sigNo = siginfo->si_signo;
  struct vg_sig_private *priv;

  /* Stack must be 8-byte aligned */
  sp_top_of_frame &= ~0xf;

  if (flags & VKI_SA_SIGINFO)
    {
      sp = sp_top_of_frame - sizeof(struct rt_sigframe);
    }
  else
    {
      sp = sp_top_of_frame - sizeof(struct sigframe);
    }

  tst = VG_(get_ThreadState)(tid);
  if (!extend(tst, sp, sp_top_of_frame - sp))
    return;

  vg_assert(VG_IS_8_ALIGNED(sp));

  /* SIGILL defines addr to be the faulting address */

  faultaddr = (Addr)siginfo->_sifields._sigfault._addr;
  if (sigNo == VKI_SIGILL && siginfo->si_code > 0)
    faultaddr = tst->arch.vex.guest_PC;
      
  if (flags & VKI_SA_SIGINFO)
    {
      struct rt_sigframe *frame = (struct rt_sigframe *) sp;
      struct vki_ucontext *ucp = &frame->rs_uc;
      if (VG_(clo_trace_signals))
        VG_(printf)("rt_sigframe\n");
      /* Create siginfo.  */
      VG_TRACK( pre_mem_write, Vg_CoreSignal, tid, "signal frame siginfo",
               (Addr)&frame->rs_info, sizeof(frame->rs_info) );

      VG_(memcpy)(&frame->rs_info, siginfo, sizeof(*siginfo));

      VG_TRACK( post_mem_write, Vg_CoreSignal, tid, 
               (Addr)&frame->rs_info, sizeof(frame->rs_info) );

      /* Create the ucontext.  */
      VG_TRACK( pre_mem_write, Vg_CoreSignal, tid, "signal frame ucontext",
               (Addr)ucp, offsetof(struct vki_ucontext, uc_mcontext) );

      ucp->uc_flags = 0;  
      ucp->uc_link = 0;
      ucp->uc_stack = tst->altstack;

      VG_TRACK( post_mem_write, Vg_CoreSignal, tid, (Addr)ucp,
               offsetof(struct vki_ucontext, uc_mcontext) );

      struct vki_sigcontext *scp = &(frame->rs_uc.uc_mcontext);
      setup_sigcontext2(tst, &(scp), siginfo);

      ucp->uc_sigmask = tst->sig_mask;

      priv = &frame->priv;

      /*
       * Arguments to signal handler:
       *
       *   a0 = signal number
       *   a1 = 0 (should be cause)
       *   a2 = pointer to ucontext
       *
       * $25 and c0_epc point to the signal handler, $29 points to
       * the struct rt_sigframe.
       */

      tst->arch.vex.guest_r4 = siginfo->si_signo;
      tst->arch.vex.guest_r5 = (Addr) &frame->rs_info;
      tst->arch.vex.guest_r6 = (Addr) &frame->rs_uc;
      tst->arch.vex.guest_r29 = (Addr) frame;
      tst->arch.vex.guest_r25 = (Addr) handler;

      if (flags & VKI_SA_RESTORER)
        {
          tst->arch.vex.guest_r31 = (Addr) restorer;  
        }
      else
        {
          tst->arch.vex.guest_r31 = (Addr)&VG_(mips32_linux_SUBST_FOR_rt_sigreturn);
        }

    }
  else
    {
      if (VG_(clo_trace_signals))
        VG_(printf)("sigframe\n");
      struct sigframe *frame = (struct sigframe *) sp;
      struct vki_sigcontext *scp = &(frame->sf_sc);
      setup_sigcontext2(tst, &(scp), siginfo);
      frame->sf_mask = tst->sig_mask;
      priv = &frame->priv;
      /*
       * Arguments to signal handler:
       *
       *   a0 = signal number
       *   a1 = 0 (should be cause)
       *   a2 = pointer to struct sigcontext
       *
       * $25 and c0_epc point to the signal handler, $29 points to the
       * struct sigframe.
       */
      tst->arch.vex.guest_r4 = siginfo->si_signo;
      tst->arch.vex.guest_r5 = 0;
      tst->arch.vex.guest_r6 = (Addr) &frame->sf_sc;
      tst->arch.vex.guest_r29 = (Addr) frame;
      tst->arch.vex.guest_r25 = (Addr) handler;

      if (flags & VKI_SA_RESTORER)
        {
          tst->arch.vex.guest_r31 = (Addr) restorer;  
        }
      else
        {
          tst->arch.vex.guest_r31 = (Addr)&VG_(mips32_linux_SUBST_FOR_sigreturn);
        }
    }

  priv->magicPI       = 0x31415927;
  priv->sigNo_private = sigNo;
  priv->vex_shadow1   = tst->arch.vex_shadow1;
  priv->vex_shadow2   = tst->arch.vex_shadow2;
  /* Set the thread so it will next run the handler. */
  /* tst->m_sp  = sp;  also notify the tool we've updated SP */
  VG_TRACK( post_reg_write, Vg_CoreSignal, tid, VG_O_STACK_PTR, sizeof(Addr));
  if (VG_(clo_trace_signals))
    VG_(printf)("handler = %p\n", handler);
  tst->arch.vex.guest_PC = (Addr) handler;
  /* This thread needs to be marked runnable, but we leave that the
     caller to do. */
}

/* EXPORTED */
void VG_(sigframe_destroy)( ThreadId tid, Bool isRT )
{
  ThreadState *tst;
  struct vg_sig_private *priv1;
  Addr sp;
  UInt frame_size;
  struct vki_sigcontext *mc;
  Int sigNo;
  Bool has_siginfo = isRT;

  vg_assert(VG_(is_valid_tid)(tid));
  tst = VG_(get_ThreadState)(tid);
  sp   = tst->arch.vex.guest_r29;
  if (has_siginfo)
    {
      struct rt_sigframe *frame = (struct rt_sigframe *)sp;
      struct vki_ucontext *ucp = &frame->rs_uc;
      frame_size = sizeof(*frame);
      mc = &ucp->uc_mcontext;
      priv1 = &frame->priv;
      vg_assert(priv1->magicPI == 0x31415927);
      sigNo = priv1->sigNo_private;
    }
  else 
    {
      struct sigframe *frame = (struct sigframe *)sp;
      frame_size = sizeof(*frame);
      mc = &(frame->sf_sc);
      priv1 = &frame->priv;
      vg_assert(priv1->magicPI == 0x31415927);
      tst->sig_mask = frame->sf_mask;
      tst->tmp_sig_mask = tst->sig_mask;
      sigNo = priv1->sigNo_private;
    }
  //restore regs
  tst->arch.vex.guest_r1 = mc->sc_regs[1];
  tst->arch.vex.guest_r2 = mc->sc_regs[2];
  tst->arch.vex.guest_r3 = mc->sc_regs[3];
  tst->arch.vex.guest_r4 = mc->sc_regs[4];
  tst->arch.vex.guest_r5 = mc->sc_regs[5];
  tst->arch.vex.guest_r6 = mc->sc_regs[6];
  tst->arch.vex.guest_r7 = mc->sc_regs[7];
  tst->arch.vex.guest_r8 = mc->sc_regs[8];
  tst->arch.vex.guest_r9 = mc->sc_regs[9];    
  tst->arch.vex.guest_r10 = mc->sc_regs[10];
  tst->arch.vex.guest_r11 = mc->sc_regs[11];
  tst->arch.vex.guest_r12 = mc->sc_regs[12];
  tst->arch.vex.guest_r13= mc->sc_regs[13];
  tst->arch.vex.guest_r14 = mc->sc_regs[14];
  tst->arch.vex.guest_r15 = mc->sc_regs[15];
  tst->arch.vex.guest_r16 = mc->sc_regs[16];
  tst->arch.vex.guest_r17 = mc->sc_regs[17];   
  tst->arch.vex.guest_r18 = mc->sc_regs[18];
  tst->arch.vex.guest_r19 = mc->sc_regs[19];
  tst->arch.vex.guest_r20 = mc->sc_regs[20];
  tst->arch.vex.guest_r21 = mc->sc_regs[21];
  tst->arch.vex.guest_r22 = mc->sc_regs[22];
  tst->arch.vex.guest_r23 = mc->sc_regs[23];
  tst->arch.vex.guest_r24 = mc->sc_regs[24];
  tst->arch.vex.guest_r25 = mc->sc_regs[25];
  tst->arch.vex.guest_r26 = mc->sc_regs[26];
  tst->arch.vex.guest_r27 = mc->sc_regs[27];
  tst->arch.vex.guest_r28 = mc->sc_regs[28];
  tst->arch.vex.guest_r30 = mc->sc_regs[30];
  tst->arch.vex.guest_PC = mc->sc_pc;
  tst->arch.vex.guest_r31 = mc->sc_regs[31];
  tst->arch.vex.guest_r29 = mc->sc_regs[29];

  tst->arch.vex.guest_HI = mc->sc_mdhi;
  tst->arch.vex.guest_LO = mc->sc_mdlo;
  tst->arch.vex_shadow1 = priv1->vex_shadow1;
  tst->arch.vex_shadow2 = priv1->vex_shadow2; 

  VG_TRACK(die_mem_stack_signal, sp, frame_size);
  if (VG_(clo_trace_signals))
    VG_(message)( Vg_DebugMsg, 
         "VG_(signal_return) (thread %d): isRT=%d valid magic; EIP=%#x\n",
         tid, isRT, tst->arch.vex.guest_PC);
  /* tell the tools */
  VG_TRACK( post_deliver_signal, tid, sigNo );
}

#endif // defined(VGP_mips32_linux)

/*--------------------------------------------------------------------*/
/*--- end                                  sigframe-mips32-linux.c ---*/
/*--------------------------------------------------------------------*/
