
/*--------------------------------------------------------------------*/
/*--- Create/destroy signal delivery frames.                       ---*/
/*---                                       sigframe-ppc32-linux.c ---*/
/*--------------------------------------------------------------------*/

/*
   This file is part of Valgrind, a dynamic binary instrumentation
   framework.

   Copyright (C) 2000-2017 Nicholas Nethercote
      njn@valgrind.org
   Copyright (C) 2004-2017 Paul Mackerras
      paulus@samba.org

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

#if defined(VGP_ppc32_linux)

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
#include "pub_core_transtab.h"      // VG_(discard_translations)
#include "priv_sigframe.h"

/* This module creates and removes signal frames for signal deliveries
   on ppc32-linux.

   Note, this file contains kernel-specific knowledge in the form of
   'struct sigframe' and 'struct rt_sigframe'.  How does that relate
   to the vki kernel interface stuff?

   Either a 'struct sigframe' or a 'struct rtsigframe' is pushed 
   onto the client's stack.  This contains a subsidiary
   vki_ucontext.  That holds the vcpu's state across the signal, 
   so that the sighandler can mess with the vcpu state if it
   really wants.

   FIXME: sigcontexting is basically broken for the moment.  When
   delivering a signal, the integer registers and %eflags are
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

// Linux has 2 signal frame structures: one for normal signal
// deliveries, and one for SA_SIGINFO deliveries (also known as RT
// signals).
//
// In theory, so long as we get the arguments to the handler function
// right, it doesn't matter what the exact layout of the rest of the
// frame is.  Unfortunately, things like gcc's exception unwinding
// make assumptions about the locations of various parts of the frame,
// so we need to duplicate it exactly.

/* Structure containing bits of information that we want to save
   on signal delivery. */
struct vg_sig_private {
   UInt magicPI;
   UInt sigNo_private;
   VexGuestPPC32State vex_shadow1;
   VexGuestPPC32State vex_shadow2;
};

/* Structure put on stack for signal handlers with SA_SIGINFO clear. */
struct nonrt_sigframe {
   UInt gap1[16];
   struct vki_sigcontext sigcontext;
   struct vki_mcontext mcontext;
   struct vg_sig_private priv;
   unsigned char abigap[224];    // unused
};

/* Structure put on stack for signal handlers with SA_SIGINFO set. */
struct rt_sigframe {
   UInt gap1[20];
   vki_siginfo_t siginfo;
   struct vki_ucontext ucontext;
   struct vg_sig_private priv;
   unsigned char abigap[224];    // unused
};

#define SET_SIGNAL_LR(zztst, zzval)                          \
   do { tst->arch.vex.guest_LR = (zzval);                    \
      VG_TRACK( post_reg_write, Vg_CoreSignal, tst->tid,     \
                offsetof(VexGuestPPC32State,guest_LR),       \
                sizeof(UWord) );                             \
   } while (0)

#define SET_SIGNAL_GPR(zztst, zzn, zzval)                    \
   do { tst->arch.vex.guest_GPR##zzn = (zzval);              \
      VG_TRACK( post_reg_write, Vg_CoreSignal, tst->tid,     \
                offsetof(VexGuestPPC32State,guest_GPR##zzn), \
                sizeof(UWord) );                             \
   } while (0)


static 
void stack_mcontext ( struct vki_mcontext *mc, 
                      ThreadState* tst, 
                      Bool use_rt_sigreturn,
                      UInt fault_addr )
{
   VG_TRACK( pre_mem_write, Vg_CoreSignal, tst->tid, "signal frame mcontext",
             (Addr)mc, sizeof(struct vki_pt_regs) );

#  define DO(gpr)  mc->mc_gregs[VKI_PT_R0+gpr] = tst->arch.vex.guest_GPR##gpr
   DO(0);  DO(1);  DO(2);  DO(3);  DO(4);  DO(5);  DO(6);  DO(7);
   DO(8);  DO(9);  DO(10); DO(11); DO(12); DO(13); DO(14); DO(15);
   DO(16); DO(17); DO(18); DO(19); DO(20); DO(21); DO(22); DO(23);
   DO(24); DO(25); DO(26); DO(27); DO(28); DO(29); DO(30); DO(31);
#  undef DO

   mc->mc_gregs[VKI_PT_NIP]     = tst->arch.vex.guest_CIA;
   mc->mc_gregs[VKI_PT_MSR]     = 0xf032;   /* pretty arbitrary */
   mc->mc_gregs[VKI_PT_ORIG_R3] = tst->arch.vex.guest_GPR3;
   mc->mc_gregs[VKI_PT_CTR]     = tst->arch.vex.guest_CTR;
   mc->mc_gregs[VKI_PT_LNK]     = tst->arch.vex.guest_LR;
   mc->mc_gregs[VKI_PT_XER]     = LibVEX_GuestPPC32_get_XER(&tst->arch.vex);
   mc->mc_gregs[VKI_PT_CCR]     = LibVEX_GuestPPC32_get_CR(&tst->arch.vex);
   mc->mc_gregs[VKI_PT_MQ]      = 0;
   mc->mc_gregs[VKI_PT_TRAP]    = 0;
   mc->mc_gregs[VKI_PT_DAR]     = fault_addr;
   mc->mc_gregs[VKI_PT_DSISR]   = 0;
   mc->mc_gregs[VKI_PT_RESULT]  = 0;
   VG_TRACK( post_mem_write, Vg_CoreSignal, tst->tid, 
             (Addr)mc, sizeof(struct vki_pt_regs) );

   /* XXX should do FP and vector regs */

   /* set up signal return trampoline */
   /* NB.  5 Sept 07.  mc->mc_pad[0..1] used to contain a the code to
      which the signal handler returns, and it just did sys_sigreturn
      or sys_rt_sigreturn.  But this doesn't work if the stack is
      non-executable, and it isn't consistent with the x86-linux and
      amd64-linux scheme for removing the stack frame.  So instead be
      consistent and use a stub in m_trampoline.  Then it doesn't
      matter whether or not the (guest) stack is executable.  This
      fixes #149519 and #145837. */
   VG_TRACK(pre_mem_write, Vg_CoreSignal, tst->tid, "signal frame mcontext",
            (Addr)&mc->mc_pad, sizeof(mc->mc_pad));
   mc->mc_pad[0] = 0; /* invalid */
   mc->mc_pad[1] = 0; /* invalid */
   VG_TRACK( post_mem_write,  Vg_CoreSignal, tst->tid, 
             (Addr)&mc->mc_pad, sizeof(mc->mc_pad) );
   /* invalidate any translation of this area */
   VG_(discard_translations)( (Addr)&mc->mc_pad, 
                              sizeof(mc->mc_pad), "stack_mcontext" );   

   /* set the signal handler to return to the trampoline */
   SET_SIGNAL_LR(tst, (Addr)(use_rt_sigreturn 
                               ? (Addr)&VG_(ppc32_linux_SUBST_FOR_rt_sigreturn)
                               : (Addr)&VG_(ppc32_linux_SUBST_FOR_sigreturn)
                      ));
}

//:: /* Valgrind-specific parts of the signal frame */
//:: struct vg_sigframe
//:: {
//::    /* Sanity check word. */
//::    UInt magicPI;
//:: 
//::    UInt handlerflags;	/* flags for signal handler */
//:: 
//:: 
//::    /* Safely-saved version of sigNo, as described above. */
//::    Int  sigNo_private;
//:: 
//::    /* XXX This is wrong.  Surely we should store the shadow values
//::       into the shadow memory behind the actual values? */
//::    VexGuestPPC32State vex_shadow;
//:: 
//::    /* HACK ALERT */
//::    VexGuestPPC32State vex;
//::    /* end HACK ALERT */
//:: 
//::    /* saved signal mask to be restored when handler returns */
//::    vki_sigset_t	mask;
//:: 
//::    /* Sanity check word.  Is the highest-addressed word; do not
//::       move!*/
//::    UInt magicE;
//:: };
//:: 
//:: struct sigframe
//:: {
//::    /* Sig handler's return address */
//::    Addr retaddr;
//::    Int  sigNo;
//:: 
//::    struct vki_sigcontext sigContext;
//:: //..    struct _vki_fpstate fpstate;
//:: 
//::    struct vg_sigframe vg;
//:: };
//:: 
//:: struct rt_sigframe
//:: {
//::    /* Sig handler's return address */
//::    Addr retaddr;
//::    Int  sigNo;
//:: 
//::    /* ptr to siginfo_t. */
//::    Addr psigInfo;
//:: 
//::    /* ptr to ucontext */
//::    Addr puContext;
//::    /* pointed to by psigInfo */
//::    vki_siginfo_t sigInfo;
//:: 
//::    /* pointed to by puContext */
//::    struct vki_ucontext uContext;
//:: //..    struct _vki_fpstate fpstate;
//:: 
//::    struct vg_sigframe vg;
//:: };


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

//.. /* Create a plausible-looking sigcontext from the thread's
//..    Vex guest state.  NOTE: does not fill in the FP or SSE
//..    bits of sigcontext at the moment.
//.. */
//.. static 
//.. void synth_ucontext(ThreadId tid, const vki_siginfo_t *si, 
//..                     const vki_sigset_t *set, struct vki_ucontext *uc)
//.. {
//..    ThreadState *tst = VG_(get_ThreadState)(tid);
//..    struct vki_sigcontext *sc = &uc->uc_mcontext;
//.. 
//..    VG_(memset)(uc, 0, sizeof(*uc));
//.. 
//..    uc->uc_flags = 0;
//..    uc->uc_link = 0;
//..    uc->uc_sigmask = *set;
//..    uc->uc_stack = tst->altstack;
//..    sc->fpstate = fpstate;
//.. 
//..    // FIXME: save_i387(&tst->arch, fpstate);
//.. 
//.. #  define SC2(reg,REG)  sc->reg = tst->arch.vex.guest_##REG
//..    SC2(gs,GS);
//..    SC2(fs,FS);
//..    SC2(es,ES);
//..    SC2(ds,DS);
//.. 
//..    SC2(edi,EDI);
//..    SC2(esi,ESI);
//..    SC2(ebp,EBP);
//..    SC2(esp,ESP);
//..    SC2(ebx,EBX);
//..    SC2(edx,EDX);
//..    SC2(ecx,ECX);
//..    SC2(eax,EAX);
//.. 
//..    SC2(eip,EIP);
//..    SC2(cs,CS);
//..    sc->eflags = LibVEX_GuestX86_get_eflags(&tst->arch.vex);
//..    SC2(ss,SS);
//..    /* XXX esp_at_signal */
//..    /* XXX trapno */
//..    /* XXX err */
//.. #  undef SC2
//.. 
//..    sc->cr2 = (UInt)si->_sifields._sigfault._addr;
//.. }
/*
//.. #define SET_SIGNAL_ESP(zztid, zzval) \
//..    SET_THREAD_REG(zztid, zzval, STACK_PTR, post_reg_write, \
//..                   Vg_CoreSignal, zztid, VG_O_STACK_PTR, sizeof(Addr))
*/


//.. /* Build the Valgrind-specific part of a signal frame. */
//.. 
//.. static void build_vg_sigframe(struct vg_sigframe *frame,
//.. 			      ThreadState *tst,
//.. 			      const vki_sigset_t *mask,
//.. 			      UInt flags,
//.. 			      Int sigNo)
//.. {
//..    frame->sigNo_private = sigNo;
//..    frame->magicPI       = 0x31415927;
//..    frame->vex_shadow    = tst->arch.vex_shadow;
//..    /* HACK ALERT */
//..    frame->vex           = tst->arch.vex;
//..    /* end HACK ALERT */
//..    frame->mask          = tst->sig_mask;
//..    frame->handlerflags  = flags;
//..    frame->magicE        = 0x27182818;
//.. }


//.. static Addr build_sigframe(ThreadState *tst,
//.. 			   Addr esp_top_of_frame,
//.. 			   const vki_siginfo_t *siginfo,
//.. 			   void *handler, UInt flags,
//.. 			   const vki_sigset_t *mask,
//.. 			   void *restorer)
//.. {
//..    struct sigframe *frame;
//..    Addr esp = esp_top_of_frame;
//..    Int	sigNo = siginfo->si_signo;
//..    struct vki_ucontext uc;
//.. 
//..    vg_assert((flags & VKI_SA_SIGINFO) == 0);
//.. 
//..    esp -= sizeof(*frame);
//..    esp = ROUNDDN(esp, 16);
//..    frame = (struct sigframe *)esp;
//.. 
//..    if (!extend(tst, esp, sizeof(*frame)))
//..       return esp_top_of_frame;
//.. 
//..    /* retaddr, sigNo, siguContext fields are to be written */
//..    VG_TRACK( pre_mem_write, Vg_CoreSignal, tst->tid, "signal handler frame", 
//.. 	     esp, offsetof(struct sigframe, vg) );
//.. 
//..    frame->sigNo = sigNo;
//.. 
//..    if (flags & VKI_SA_RESTORER)
//..       frame->retaddr = (Addr)restorer;
//..    else
//..       frame->retaddr
//..          = VG_(client_trampoline_code)+VG_(tramp_sigreturn_offset);
//.. 
//..    synth_ucontext(tst->tid, siginfo, mask, &uc, &frame->fpstate);
//.. 
//..    VG_(memcpy)(&frame->sigContext, &uc.uc_mcontext, 
//.. 	       sizeof(struct vki_sigcontext));
//..    frame->sigContext.oldmask = mask->sig[0];
//.. 
//..    VG_TRACK( post_mem_write, Vg_CoreSignal, tst->tid, 
//..              esp, offsetof(struct sigframe, vg) );
//.. 
//..    build_vg_sigframe(&frame->vg, tst, mask, flags, sigNo);
//..    
//..    return esp;
//.. }


//.. static Addr build_rt_sigframe(ThreadState *tst,
//.. 			      Addr esp_top_of_frame,
//.. 			      const vki_siginfo_t *siginfo,
//.. 			      void *handler, UInt flags,
//.. 			      const vki_sigset_t *mask,
//.. 			      void *restorer)
//.. {
//..    struct rt_sigframe *frame;
//..    Addr esp = esp_top_of_frame;
//..    Int	sigNo = siginfo->si_signo;
//.. 
//..    vg_assert((flags & VKI_SA_SIGINFO) != 0);
//.. 
//..    esp -= sizeof(*frame);
//..    esp = ROUNDDN(esp, 16);
//..    frame = (struct rt_sigframe *)esp;
//.. 
//..    if (!extend(tst, esp, sizeof(*frame)))
//..       return esp_top_of_frame;
//.. 
//..    /* retaddr, sigNo, pSiginfo, puContext fields are to be written */
//..    VG_TRACK( pre_mem_write, Vg_CoreSignal, tst->tid, "rt signal handler frame", 
//.. 	     esp, offsetof(struct rt_sigframe, vg) );
//.. 
//..    frame->sigNo = sigNo;
//.. 
//..    if (flags & VKI_SA_RESTORER)
//..       frame->retaddr = (Addr)restorer;
//..    else
//..       frame->retaddr 
//..          = VG_(client_trampoline_code)+VG_(tramp_rt_sigreturn_offset);
//.. 
//..    frame->psigInfo = (Addr)&frame->sigInfo;
//..    frame->puContext = (Addr)&frame->uContext;
//..    VG_(memcpy)(&frame->sigInfo, siginfo, sizeof(vki_siginfo_t));
//.. 
//..    /* SIGILL defines addr to be the faulting address */
//..    if (sigNo == VKI_SIGILL && siginfo->si_code > 0)
//..       frame->sigInfo._sifields._sigfault._addr 
//..          = (void*)tst->arch.vex.guest_CIA;
//.. 
//..    synth_ucontext(tst->tid, siginfo, mask, &frame->uContext, &frame->fpstate);
//.. 
//..    VG_TRACK( post_mem_write,  Vg_CoreSignal, tst->tid, 
//..              esp, offsetof(struct rt_sigframe, vg) );
//.. 
//..    build_vg_sigframe(&frame->vg, tst, mask, flags, sigNo);
//..    
//..    return esp;
//.. }


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
   struct vg_sig_private *priv;
   Addr sp;
   ThreadState *tst;
   Int sigNo = siginfo->si_signo;
   Addr faultaddr;

   /* Stack must be 16-byte aligned */
   sp_top_of_frame &= ~0xf;

   if (flags & VKI_SA_SIGINFO) {
      sp = sp_top_of_frame - sizeof(struct rt_sigframe);
   } else {
      sp = sp_top_of_frame - sizeof(struct nonrt_sigframe);
   }

   tst = VG_(get_ThreadState)(tid);

   if (! ML_(sf_maybe_extend_stack)(tst, sp, sp_top_of_frame - sp, flags))
      return;

   vg_assert(VG_IS_16_ALIGNED(sp));

   /* Set up the stack chain pointer */
   VG_TRACK( pre_mem_write, Vg_CoreSignal, tid, "signal handler frame",
             sp, sizeof(UWord) );
   *(Addr *)sp = tst->arch.vex.guest_GPR1;
   VG_TRACK( post_mem_write, Vg_CoreSignal, tid, 
             sp, sizeof(UWord) );

   faultaddr = (Addr)siginfo->_sifields._sigfault._addr;
   if (sigNo == VKI_SIGILL && siginfo->si_code > 0)
      faultaddr = tst->arch.vex.guest_CIA;

   if (flags & VKI_SA_SIGINFO) {
      struct rt_sigframe *frame = (struct rt_sigframe *) sp;
      struct vki_ucontext *ucp = &frame->ucontext;

      VG_TRACK( pre_mem_write, Vg_CoreSignal, tid, "signal frame siginfo",
                (Addr)&frame->siginfo, sizeof(frame->siginfo) );
      VG_(memcpy)(&frame->siginfo, siginfo, sizeof(*siginfo));
      VG_TRACK( post_mem_write, Vg_CoreSignal, tid, 
                (Addr)&frame->siginfo, sizeof(frame->siginfo) );

      VG_TRACK( pre_mem_write, Vg_CoreSignal, tid, "signal frame ucontext",
                (Addr)ucp, offsetof(struct vki_ucontext, uc_pad) );
      ucp->uc_flags = 0;
      ucp->uc_link = 0;
      ucp->uc_stack = tst->altstack;
      VG_TRACK( post_mem_write, Vg_CoreSignal, tid, (Addr)ucp,
                offsetof(struct vki_ucontext, uc_pad) );

      VG_TRACK( pre_mem_write, Vg_CoreSignal, tid, "signal frame ucontext",
                (Addr)&ucp->uc_regs,
                sizeof(ucp->uc_regs) + sizeof(ucp->uc_sigmask) );
      ucp->uc_regs = &ucp->uc_mcontext;
      ucp->uc_sigmask = tst->sig_mask;
      VG_TRACK( post_mem_write, Vg_CoreSignal, tid, 
                (Addr)&ucp->uc_regs,
                sizeof(ucp->uc_regs) + sizeof(ucp->uc_sigmask) );

      stack_mcontext(&ucp->uc_mcontext, tst, True/*use_rt_sigreturn*/, faultaddr);
      priv = &frame->priv;

      SET_SIGNAL_GPR(tid, 4, (Addr) &frame->siginfo);
      SET_SIGNAL_GPR(tid, 5, (Addr) ucp);
      /* the kernel sets this, though it doesn't seem to be in the ABI */
      SET_SIGNAL_GPR(tid, 6, (Addr) &frame->siginfo);

   } else {
      /* non-RT signal delivery */
      struct nonrt_sigframe *frame = (struct nonrt_sigframe *) sp;
      struct vki_sigcontext *scp = &frame->sigcontext;

      VG_TRACK( pre_mem_write, Vg_CoreSignal, tid, "signal frame sigcontext",
                (Addr)&scp->_unused[3], sizeof(*scp) - 3 * sizeof(UInt) );
      scp->signal = sigNo;
      scp->handler = (Addr) handler;
      scp->oldmask = tst->sig_mask.sig[0];
      scp->_unused[3] = tst->sig_mask.sig[1];
      VG_TRACK( post_mem_write, Vg_CoreSignal, tid,
                (Addr)&scp->_unused[3], sizeof(*scp) - 3 * sizeof(UInt) );

      stack_mcontext(&frame->mcontext, tst, False/*!use_rt_sigreturn*/, faultaddr);
      priv = &frame->priv;

      SET_SIGNAL_GPR(tid, 4, (Addr) scp);
   }

   priv->magicPI       = 0x31415927;
   priv->sigNo_private = sigNo;
   priv->vex_shadow1   = tst->arch.vex_shadow1;
   priv->vex_shadow2   = tst->arch.vex_shadow2;

   SET_SIGNAL_GPR(tid, 1, sp);
   SET_SIGNAL_GPR(tid, 3, sigNo);
   tst->arch.vex.guest_CIA = (Addr) handler;

//..    Addr		esp;
//..    ThreadState* tst = VG_(get_ThreadState)(tid);
//.. 
//..    if (flags & VKI_SA_SIGINFO)
//..       esp = build_rt_sigframe(tst, esp_top_of_frame, siginfo, 
//..                                    handler, flags, mask, restorer);
//..    else
//..       esp = build_sigframe(tst, esp_top_of_frame, 
//..                                 siginfo, handler, flags, mask, restorer);
//.. 
//..    /* Set the thread so it will next run the handler. */
//..    /* tst->m_esp  = esp; */
//..    SET_SIGNAL_ESP(tid, esp);
//.. 
//..    //VG_(printf)("handler = %p\n", handler);
//..    tst->arch.vex.guest_CIA = (Addr) handler;
//..    /* This thread needs to be marked runnable, but we leave that the
//..       caller to do. */

   if (0)
      VG_(printf)("pushed signal frame; %%R1 now = %#lx, "
                  "next %%CIA = %#x, status=%d\n",
		  sp, tst->arch.vex.guest_CIA, (Int)tst->status);
}


/*------------------------------------------------------------*/
/*--- Destroying signal frames                             ---*/
/*------------------------------------------------------------*/

//.. /* Return False and don't do anything, just set the client to take a
//..    segfault, if it looks like the frame is corrupted. */
//.. static 
//.. Bool restore_vg_sigframe ( ThreadState *tst, 
//..                            struct vg_sigframe *frame, Int *sigNo )
//.. {
//..    if (frame->magicPI != 0x31415927 ||
//..        frame->magicE  != 0x27182818) {
//..       VG_(message)(Vg_UserMsg, "Thread %d return signal frame "
//..                                "corrupted.  Killing process.",
//.. 		   tst->tid);
//..       VG_(set_default_handler)(VKI_SIGSEGV);
//..       VG_(synth_fault)(tst->tid);
//..       *sigNo = VKI_SIGSEGV;
//..       return False;
//..    }
//..    tst->sig_mask        = frame->mask;
//..    tst->tmp_sig_mask    = frame->mask;
//..    tst->arch.vex_shadow = frame->vex_shadow;
//..    /* HACK ALERT */
//..    tst->arch.vex        = frame->vex;
//..    /* end HACK ALERT */
//..    *sigNo               = frame->sigNo_private;
//..    return True;
//.. }

//.. static 
//.. void restore_sigcontext( ThreadState *tst, 
//..                          struct vki_sigcontext *sc )
//.. //..                          struct vki_sigcontext *sc, struct _vki_fpstate *fpstate )
//.. {
//..    tst->arch.vex.guest_EAX     = sc->eax;
//..    tst->arch.vex.guest_ECX     = sc->ecx;
//..    tst->arch.vex.guest_EDX     = sc->edx;
//..    tst->arch.vex.guest_EBX     = sc->ebx;
//..    tst->arch.vex.guest_EBP     = sc->ebp; 
//..    tst->arch.vex.guest_ESP     = sc->esp;
//..    tst->arch.vex.guest_ESI     = sc->esi;
//..    tst->arch.vex.guest_EDI     = sc->edi;
//.. //::    tst->arch.vex.guest_eflags  = sc->eflags;
//.. //::    tst->arch.vex.guest_EIP     = sc->eip;
//.. 
//..    tst->arch.vex.guest_CS      = sc->cs; 
//..    tst->arch.vex.guest_SS      = sc->ss;
//..    tst->arch.vex.guest_DS      = sc->ds;
//..    tst->arch.vex.guest_ES      = sc->es;
//..    tst->arch.vex.guest_FS      = sc->fs;
//..    tst->arch.vex.guest_GS      = sc->gs;
//.. 
//.. //::    restore_i387(&tst->arch, fpstate);
//.. }


//.. static 
//.. SizeT restore_sigframe ( ThreadState *tst, 
//..                          struct sigframe *frame, Int *sigNo )
//.. {
//..    if (restore_vg_sigframe(tst, &frame->vg, sigNo))
//..       restore_sigcontext(tst, &frame->sigContext, &frame->fpstate);
//..    return sizeof(*frame);
//.. }

//.. static 
//.. SizeT restore_rt_sigframe ( ThreadState *tst, 
//..                             struct rt_sigframe *frame, Int *sigNo )
//.. {
//..    if (restore_vg_sigframe(tst, &frame->vg, sigNo))
//..       restore_sigcontext(tst, &frame->uContext.uc_mcontext, &frame->fpstate);
//..    return sizeof(*frame);
//.. }


/* EXPORTED */
void VG_(sigframe_destroy)( ThreadId tid, Bool isRT )
{
   ThreadState *tst;
   struct vg_sig_private *priv;
   Addr sp;
   UInt frame_size;
   struct vki_mcontext *mc;
   Int sigNo;
   Bool has_siginfo = isRT;

   vg_assert(VG_(is_valid_tid)(tid));
   tst = VG_(get_ThreadState)(tid);

   /* Check that the stack frame looks valid */
   sp = tst->arch.vex.guest_GPR1;
   vg_assert(VG_IS_16_ALIGNED(sp));
   /* JRS 17 Nov 05: This code used to check that *sp -- which should
      have been set by the stwu at the start of the handler -- points
      to just above the frame (ie, the previous frame).  However, that
      isn't valid when delivering signals on alt stacks.  So I removed
      it.  The frame is still sanity-checked using the priv->magicPI
      field. */

   if (has_siginfo) {
      struct rt_sigframe *frame = (struct rt_sigframe *)sp;
      frame_size = sizeof(*frame);
      mc = &frame->ucontext.uc_mcontext;
      priv = &frame->priv;
      vg_assert(priv->magicPI == 0x31415927);
      tst->sig_mask = frame->ucontext.uc_sigmask;
   } else {
      struct nonrt_sigframe *frame = (struct nonrt_sigframe *)sp;
      frame_size = sizeof(*frame);
      mc = &frame->mcontext;
      priv = &frame->priv;
      vg_assert(priv->magicPI == 0x31415927);
      tst->sig_mask.sig[0] = frame->sigcontext.oldmask;
      tst->sig_mask.sig[1] = frame->sigcontext._unused[3];
   }
   tst->tmp_sig_mask = tst->sig_mask;

   sigNo = priv->sigNo_private;

#  define DO(gpr)  tst->arch.vex.guest_GPR##gpr = mc->mc_gregs[VKI_PT_R0+gpr]
   DO(0);  DO(1);  DO(2);  DO(3);  DO(4);  DO(5);  DO(6);  DO(7);
   DO(8);  DO(9);  DO(10); DO(11); DO(12); DO(13); DO(14); DO(15);
   DO(16); DO(17); DO(18); DO(19); DO(20); DO(21); DO(22); DO(23);
   DO(24); DO(25); DO(26); DO(27); DO(28); DO(29); DO(30); DO(31);
#  undef DO

   tst->arch.vex.guest_CIA = mc->mc_gregs[VKI_PT_NIP];

   // Umm ... ? (jrs 2005 July 8)
   // tst->arch.m_orig_gpr3 = mc->mc_gregs[VKI_PT_ORIG_R3];

   LibVEX_GuestPPC32_put_CR( mc->mc_gregs[VKI_PT_CCR], &tst->arch.vex );

   tst->arch.vex.guest_LR  = mc->mc_gregs[VKI_PT_LNK];
   tst->arch.vex.guest_CTR = mc->mc_gregs[VKI_PT_CTR];
   LibVEX_GuestPPC32_put_XER( mc->mc_gregs[VKI_PT_XER], &tst->arch.vex );

   tst->arch.vex_shadow1 = priv->vex_shadow1;
   tst->arch.vex_shadow2 = priv->vex_shadow2;

   VG_TRACK(die_mem_stack_signal, sp, frame_size);

   if (VG_(clo_trace_signals))
      VG_(message)(Vg_DebugMsg,
                   "vg_pop_signal_frame (thread %u): "
                   "isRT=%d valid magic; EIP=%#x\n",
                   tid, has_siginfo, tst->arch.vex.guest_CIA);

   /* tell the tools */
   VG_TRACK( post_deliver_signal, tid, sigNo );

//..    Addr          esp;
//..    ThreadState*  tst;
//..    SizeT	 size;
//..    Int		 sigNo;
//.. 
//..    tst = VG_(get_ThreadState)(tid);
//.. 
//..    /* Correctly reestablish the frame base address. */
//..    esp   = tst->arch.vex.guest_ESP;
//.. 
//..    if (!isRT)
//..       size = restore_sigframe(tst, (struct sigframe *)esp, &sigNo);
//..    else
//..       size = restore_rt_sigframe(tst, (struct rt_sigframe *)esp, &sigNo);
//.. 
//..    VG_TRACK( die_mem_stack_signal, esp, size );
//.. 
//..    if (VG_(clo_trace_signals))
//..       VG_(message)(
//..          Vg_DebugMsg, 
//..          "VG_(signal_return) (thread %u): isRT=%d valid magic; EIP=%p", 
//..          tid, isRT, tst->arch.vex.guest_EIP);
//.. 
//..    /* tell the tools */
//..    VG_TRACK( post_deliver_signal, tid, sigNo );
}

#endif // defined(VGP_ppc32_linux)

/*--------------------------------------------------------------------*/
/*--- end                                                          ---*/
/*--------------------------------------------------------------------*/
