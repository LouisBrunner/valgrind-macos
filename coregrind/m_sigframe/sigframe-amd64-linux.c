
/*--------------------------------------------------------------------*/
/*--- Create/destroy signal delivery frames.                       ---*/
/*---                                       sigframe-amd64-linux.c ---*/
/*--------------------------------------------------------------------*/

/*
   This file is part of Valgrind, a dynamic binary instrumentation
   framework.

   Copyright (C) 2000-2017 Nicholas Nethercote
      njn@valgrind.org

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

#if defined(VGP_amd64_linux)

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
#include "priv_sigframe.h"

/* This module creates and removes signal frames for signal deliveries
   on amd64-linux.

   Note, this file contains kernel-specific knowledge in the form of
   'struct rt_sigframe'.  How does that relate to the vki kernel
   interface stuff?

   A 'struct rtsigframe' is pushed onto the client's stack.  This
   contains a subsidiary vki_ucontext.  That holds the vcpu's state
   across the signal, so that the sighandler can mess with the vcpu
   state if it really wants.

   FIXME: sigcontexting is basically broken for the moment.  When
   delivering a signal, the integer registers and %rflags are
   correctly written into the sigcontext, however the FP and SSE state
   is not.  When returning from a signal, only the integer registers
   are restored from the sigcontext; the rest of the CPU state is
   restored to what it was before the signal.

   This will be fixed.
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
struct vg_sigframe
{
   /* Sanity check word. */
   UInt magicPI;

   UInt handlerflags;	/* flags for signal handler */


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
   vki_sigset_t	mask;

   /* Sanity check word.  Is the highest-addressed word; do not
      move!*/
   UInt magicE;
};

struct rt_sigframe
{
   /* Sig handler's return address */
   Addr retaddr;

   /* ucontext */
   struct vki_ucontext uContext;

   /* siginfo */
   vki_siginfo_t sigInfo;
   struct _vki_fpstate fpstate;

   struct vg_sigframe vg;
};


//:: /*------------------------------------------------------------*/
//:: /*--- Signal operations                                    ---*/
//:: /*------------------------------------------------------------*/
//:: 
//:: /* 
//::    Great gobs of FP state conversion taken wholesale from
//::    linux/arch/i386/kernel/i387.c
//::  */
//:: 
//:: /*
//::  * FXSR floating point environment conversions.
//::  */
//:: #define X86_FXSR_MAGIC		0x0000
//:: 
//:: /*
//::  * FPU tag word conversions.
//::  */
//:: 
//:: static inline unsigned short twd_i387_to_fxsr( unsigned short twd )
//:: {
//::    unsigned int tmp; /* to avoid 16 bit prefixes in the code */
//::  
//::    /* Transform each pair of bits into 01 (valid) or 00 (empty) */
//::    tmp = ~twd;
//::    tmp = (tmp | (tmp>>1)) & 0x5555; /* 0V0V0V0V0V0V0V0V */
//::    /* and move the valid bits to the lower byte. */
//::    tmp = (tmp | (tmp >> 1)) & 0x3333; /* 00VV00VV00VV00VV */
//::    tmp = (tmp | (tmp >> 2)) & 0x0f0f; /* 0000VVVV0000VVVV */
//::    tmp = (tmp | (tmp >> 4)) & 0x00ff; /* 00000000VVVVVVVV */
//::    return tmp;
//:: }
//:: 
//:: static unsigned long twd_fxsr_to_i387( const struct i387_fxsave_struct *fxsave )
//:: {
//::    struct _vki_fpxreg *st = NULL;
//::    unsigned long twd = (unsigned long) fxsave->twd;
//::    unsigned long tag;
//::    unsigned long ret = 0xffff0000u;
//::    int i;
//:: 
//:: #define FPREG_ADDR(f, n)	((char *)&(f)->st_space + (n) * 16);
//:: 
//::    for ( i = 0 ; i < 8 ; i++ ) {
//::       if ( twd & 0x1 ) {
//:: 	 st = (struct _vki_fpxreg *) FPREG_ADDR( fxsave, i );
//:: 
//:: 	 switch ( st->exponent & 0x7fff ) {
//:: 	 case 0x7fff:
//:: 	    tag = 2;		/* Special */
//:: 	    break;
//:: 	 case 0x0000:
//:: 	    if ( !st->significand[0] &&
//:: 		 !st->significand[1] &&
//:: 		 !st->significand[2] &&
//:: 		 !st->significand[3] ) {
//:: 	       tag = 1;	/* Zero */
//:: 	    } else {
//:: 	       tag = 2;	/* Special */
//:: 	    }
//:: 	    break;
//:: 	 default:
//:: 	    if ( st->significand[3] & 0x8000 ) {
//:: 	       tag = 0;	/* Valid */
//:: 	    } else {
//:: 	       tag = 2;	/* Special */
//:: 	    }
//:: 	    break;
//:: 	 }
//::       } else {
//:: 	 tag = 3;			/* Empty */
//::       }
//::       ret |= (tag << (2 * i));
//::       twd = twd >> 1;
//::    }
//::    return ret;
//:: }
//:: 
//:: static void convert_fxsr_to_user( struct _vki_fpstate *buf,
//:: 				  const struct i387_fxsave_struct *fxsave )
//:: {
//::    unsigned long env[7];
//::    struct _vki_fpreg *to;
//::    struct _vki_fpxreg *from;
//::    int i;
//:: 
//::    env[0] = (unsigned long)fxsave->cwd | 0xffff0000ul;
//::    env[1] = (unsigned long)fxsave->swd | 0xffff0000ul;
//::    env[2] = twd_fxsr_to_i387(fxsave);
//::    env[3] = fxsave->fip;
//::    env[4] = fxsave->fcs | ((unsigned long)fxsave->fop << 16);
//::    env[5] = fxsave->foo;
//::    env[6] = fxsave->fos;
//::    
//::    VG_(memcpy)(buf, env, 7 * sizeof(unsigned long));
//:: 
//::    to = &buf->_st[0];
//::    from = (struct _vki_fpxreg *) &fxsave->st_space[0];
//::    for ( i = 0 ; i < 8 ; i++, to++, from++ ) {
//::       unsigned long __user *t = (unsigned long __user *)to;
//::       unsigned long *f = (unsigned long *)from;
//:: 
//::       t[0] = f[0];
//::       t[1] = f[1];
//::       to->exponent = from->exponent;
//::    }
//:: }
//:: 
//:: static void convert_fxsr_from_user( struct i387_fxsave_struct *fxsave,
//:: 				    const struct _vki_fpstate *buf )
//:: {
//::    unsigned long env[7];
//::    struct _vki_fpxreg *to;
//::    const struct _vki_fpreg *from;
//::    int i;
//:: 	
//::    VG_(memcpy)(env, buf, 7 * sizeof(long));
//:: 
//::    fxsave->cwd = (unsigned short)(env[0] & 0xffff);
//::    fxsave->swd = (unsigned short)(env[1] & 0xffff);
//::    fxsave->twd = twd_i387_to_fxsr((unsigned short)(env[2] & 0xffff));
//::    fxsave->fip = env[3];
//::    fxsave->fop = (unsigned short)((env[4] & 0xffff0000ul) >> 16);
//::    fxsave->fcs = (env[4] & 0xffff);
//::    fxsave->foo = env[5];
//::    fxsave->fos = env[6];
//:: 
//::    to = (struct _vki_fpxreg *) &fxsave->st_space[0];
//::    from = &buf->_st[0];
//::    for ( i = 0 ; i < 8 ; i++, to++, from++ ) {
//::       unsigned long *t = (unsigned long *)to;
//::       unsigned long __user *f = (unsigned long __user *)from;
//:: 
//::       t[0] = f[0];
//::       t[1] = f[1];
//::       to->exponent = from->exponent;
//::    }
//:: }
//:: 
//:: static inline void save_i387_fsave( arch_thread_t *regs, struct _vki_fpstate *buf )
//:: {
//::    struct i387_fsave_struct *fs = &regs->m_sse.fsave;
//:: 
//::    fs->status = fs->swd;
//::    VG_(memcpy)(buf, fs, sizeof(*fs));
//:: }
//:: 
//:: static void save_i387_fxsave( arch_thread_t *regs, struct _vki_fpstate *buf )
//:: {
//::    const struct i387_fxsave_struct *fx = &regs->m_sse.fxsave;
//::    convert_fxsr_to_user( buf, fx );
//:: 
//::    buf->status = fx->swd;
//::    buf->magic = X86_FXSR_MAGIC;
//::    VG_(memcpy)(buf->_fxsr_env, fx, sizeof(struct i387_fxsave_struct));
//:: }
//:: 
//:: static void save_i387( arch_thread_t *regs, struct _vki_fpstate *buf )
//:: {
//::    if ( VG_(have_ssestate) )
//::       save_i387_fxsave( regs, buf );
//::    else
//::       save_i387_fsave( regs, buf );
//:: }
//:: 
//:: static inline void restore_i387_fsave( arch_thread_t *regs, const struct _vki_fpstate __user *buf )
//:: {
//::    VG_(memcpy)( &regs->m_sse.fsave, buf, sizeof(struct i387_fsave_struct) );
//:: }
//:: 
//:: static void restore_i387_fxsave( arch_thread_t *regs, const struct _vki_fpstate __user *buf )
//:: {
//::    VG_(memcpy)(&regs->m_sse.fxsave, &buf->_fxsr_env[0], 
//:: 	       sizeof(struct i387_fxsave_struct) );
//::    /* mxcsr reserved bits must be masked to zero for security reasons */
//::    regs->m_sse.fxsave.mxcsr &= 0xffbf;
//::    convert_fxsr_from_user( &regs->m_sse.fxsave, buf );
//:: }
//:: 
//:: static void restore_i387( arch_thread_t *regs, const struct _vki_fpstate __user *buf )
//:: {
//::    if ( VG_(have_ssestate) ) {
//::       restore_i387_fxsave( regs, buf );
//::    } else {
//::       restore_i387_fsave( regs, buf );
//::    }
//:: }


/*------------------------------------------------------------*/
/*--- Creating signal frames                               ---*/
/*------------------------------------------------------------*/

/* Create a plausible-looking sigcontext from the thread's
   Vex guest state.  NOTE: does not fill in the FP or SSE
   bits of sigcontext at the moment.
*/
static 
void synth_ucontext(ThreadId tid, const vki_siginfo_t *si,
                    UWord trapno, UWord err, const vki_sigset_t *set, 
                    struct vki_ucontext *uc, struct _vki_fpstate *fpstate)
{
   ThreadState *tst = VG_(get_ThreadState)(tid);
   struct vki_sigcontext *sc = &uc->uc_mcontext;

   VG_(memset)(uc, 0, sizeof(*uc));

   uc->uc_flags = 0;
   uc->uc_link = 0;
   uc->uc_sigmask = *set;
   uc->uc_stack = tst->altstack;
   sc->fpstate = fpstate;

   // FIXME: save_i387(&tst->arch, fpstate);

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

   SC2(rip,RIP);
   sc->eflags = LibVEX_GuestAMD64_get_rflags(&tst->arch.vex);
   // FIXME: SC2(cs,CS);
   // FIXME: SC2(gs,GS);
   // FIXME: SC2(fs,FS);
   sc->trapno = trapno;
   sc->err = err;
#  undef SC2

   sc->cr2 = (UWord)si->_sifields._sigfault._addr;
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


static Addr build_rt_sigframe(ThreadState *tst,
			      Addr rsp_top_of_frame,
			      const vki_siginfo_t *siginfo,
                              const struct vki_ucontext *siguc,
			      void *handler, UInt flags,
			      const vki_sigset_t *mask,
			      void *restorer)
{
   struct rt_sigframe *frame;
   Addr rsp = rsp_top_of_frame;
   Int	sigNo = siginfo->si_signo;
   UWord trapno;
   UWord err;

   rsp -= sizeof(*frame);
   rsp = VG_ROUNDDN(rsp, 16) - 8;
   frame = (struct rt_sigframe *)rsp;

   if (! ML_(sf_maybe_extend_stack)(tst, rsp, sizeof(*frame), flags))
      return rsp_top_of_frame;

   /* retaddr, siginfo, uContext fields are to be written */
   VG_TRACK( pre_mem_write, Vg_CoreSignal, tst->tid, "rt signal handler frame", 
	     rsp, offsetof(struct rt_sigframe, vg) );

   if (flags & VKI_SA_RESTORER)
      frame->retaddr = (Addr)restorer;
   else
      frame->retaddr = (Addr)&VG_(amd64_linux_SUBST_FOR_rt_sigreturn);

   if (siguc) {
      trapno = siguc->uc_mcontext.trapno;
      err = siguc->uc_mcontext.err;
   } else {
      trapno = 0;
      err = 0;
   }

   VG_(memcpy)(&frame->sigInfo, siginfo, sizeof(vki_siginfo_t));

   /* SIGILL defines addr to be the faulting address */
   if (sigNo == VKI_SIGILL && siginfo->si_code > 0)
      frame->sigInfo._sifields._sigfault._addr 
         = (void*)tst->arch.vex.guest_RIP;

   synth_ucontext(tst->tid, siginfo, trapno, err, mask,
                  &frame->uContext, &frame->fpstate);

   VG_TRACK( post_mem_write,  Vg_CoreSignal, tst->tid, 
             rsp, offsetof(struct rt_sigframe, vg) );

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
   struct rt_sigframe *frame;
   ThreadState* tst = VG_(get_ThreadState)(tid);

   rsp = build_rt_sigframe(tst, rsp_top_of_frame, siginfo, siguc,
                                handler, flags, mask, restorer);
   frame = (struct rt_sigframe *)rsp;

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

   if (0)
      VG_(printf)("pushed signal frame; %%RSP now = %#lx, "
                  "next %%RIP = %#llx, status=%d\n",
		  rsp, tst->arch.vex.guest_RIP, (Int)tst->status);
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
                         struct vki_sigcontext *sc, 
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
//::    tst->arch.vex.guest_rflags  = sc->rflags;
   tst->arch.vex.guest_RIP     = sc->rip;

//::    tst->arch.vex.guest_CS      = sc->cs; 
//::    tst->arch.vex.guest_FS      = sc->fs;
//::    tst->arch.vex.guest_GS      = sc->gs;

//::    restore_i387(&tst->arch, fpstate);
}


static 
SizeT restore_rt_sigframe ( ThreadState *tst, 
                            struct rt_sigframe *frame, Int *sigNo )
{
   if (restore_vg_sigframe(tst, &frame->vg, sigNo))
      restore_sigcontext(tst, &frame->uContext.uc_mcontext, &frame->fpstate);

   return sizeof(*frame);
}


void VG_(sigframe_destroy)( ThreadId tid, Bool isRT )
{
   Addr          rsp;
   ThreadState*  tst;
   SizeT	 size;
   Int		 sigNo;

   vg_assert(isRT);

   tst = VG_(get_ThreadState)(tid);

   /* Correctly reestablish the frame base address. */
   rsp   = tst->arch.vex.guest_RSP;

   size = restore_rt_sigframe(tst, (struct rt_sigframe *)rsp, &sigNo);

   VG_TRACK( die_mem_stack_signal, rsp - VG_STACK_REDZONE_SZB,
             size + VG_STACK_REDZONE_SZB );

   if (VG_(clo_trace_signals))
      VG_(message)(
         Vg_DebugMsg,
         "VG_(sigframe_destroy) (thread %u): isRT=%d valid magic; RIP=%#llx\n",
         tid, isRT, tst->arch.vex.guest_RIP);

   /* tell the tools */
   VG_TRACK( post_deliver_signal, tid, sigNo );
}

#endif // defined(VGP_amd64_linux)

/*--------------------------------------------------------------------*/
/*--- end                                                          ---*/
/*--------------------------------------------------------------------*/
