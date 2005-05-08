
/*--------------------------------------------------------------------*/
/*--- Create/destroy signal delivery frames.                       ---*/
/*---                                         sigframe-x86-linux.c ---*/
/*--------------------------------------------------------------------*/

/*
   This file is part of Valgrind, a dynamic binary instrumentation
   framework.

   Copyright (C) 2000-2005 Nicholas Nethercote
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
   along with this program; if not, write to the Free Software
   Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
   02111-1307, USA.

   The GNU General Public License is contained in the file COPYING.
*/

#include "core.h"
#include "pub_core_sigframe.h"
#include "pub_core_aspacemgr.h" /* find_segment */


#include "libvex_guest_x86.h"


/* This module creates and removes signal frames for signal deliveries
   on x86-linux.

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
   VexGuestX86State vex_shadow;

   /* HACK ALERT */
   VexGuestX86State vex;
   /* end HACK ALERT */

   /* saved signal mask to be restored when handler returns */
   vki_sigset_t	mask;

   /* Sanity check word.  Is the highest-addressed word; do not
      move!*/
   UInt magicE;
};

struct sigframe
{
   /* Sig handler's return address */
   Addr retaddr;
   Int  sigNo;

   struct vki_sigcontext sigContext;
   struct _vki_fpstate fpstate;

   struct vg_sigframe vg;
};

struct rt_sigframe
{
   /* Sig handler's return address */
   Addr retaddr;
   Int  sigNo;

   /* ptr to siginfo_t. */
   Addr psigInfo;

   /* ptr to ucontext */
   Addr puContext;
   /* pointed to by psigInfo */
   vki_siginfo_t sigInfo;

   /* pointed to by puContext */
   struct vki_ucontext uContext;
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
                    const vki_sigset_t *set, 
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
   /* XXX esp_at_signal */
   /* XXX trapno */
   /* XXX err */
#  undef SC2

   sc->cr2 = (UInt)si->_sifields._sigfault._addr;
}


/* Extend the stack segment downwards if needed so as to ensure the
   new signal frames are mapped to something.  Return a Bool
   indicating whether or not the operation was successful.
*/
static Bool extend ( ThreadState *tst, Addr addr, SizeT size )
{
   ThreadId tid = tst->tid;
   Segment *stackseg = NULL;

   if (VG_(extend_stack)(addr, tst->client_stack_szB)) {
      stackseg = VG_(find_segment)(addr);
      if (0 && stackseg)
	 VG_(printf)("frame=%p seg=%p-%p\n",
		     addr, stackseg->addr, stackseg->addr+stackseg->len);
   }

   if (stackseg == NULL 
       || (stackseg->prot & (VKI_PROT_READ|VKI_PROT_WRITE)) == 0) {
      VG_(message)(
         Vg_UserMsg,
         "Can't extend stack to %p during signal delivery for thread %d:",
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
   VG_TRACK( new_mem_stack_signal, addr - VGA_STACK_REDZONE_SZB,
             size + VGA_STACK_REDZONE_SZB );

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
   frame->vex_shadow    = tst->arch.vex_shadow;
   /* HACK ALERT */
   frame->vex           = tst->arch.vex;
   /* end HACK ALERT */
   frame->mask          = tst->sig_mask;
   frame->handlerflags  = flags;
   frame->magicE        = 0x27182818;
}


static Addr build_sigframe(ThreadState *tst,
			   Addr esp_top_of_frame,
			   const vki_siginfo_t *siginfo,
			   void *handler, UInt flags,
			   const vki_sigset_t *mask,
			   void *restorer)
{
   struct sigframe *frame;
   Addr esp = esp_top_of_frame;
   Int	sigNo = siginfo->si_signo;
   struct vki_ucontext uc;

   vg_assert((flags & VKI_SA_SIGINFO) == 0);

   esp -= sizeof(*frame);
   esp = ROUNDDN(esp, 16);
   frame = (struct sigframe *)esp;

   if (!extend(tst, esp, sizeof(*frame)))
      return esp_top_of_frame;

   /* retaddr, sigNo, siguContext fields are to be written */
   VG_TRACK( pre_mem_write, Vg_CoreSignal, tst->tid, "signal handler frame", 
	     esp, offsetof(struct sigframe, vg) );

   frame->sigNo = sigNo;

   if (flags & VKI_SA_RESTORER)
      frame->retaddr = (Addr)restorer;
   else
      frame->retaddr
         = VG_(client_trampoline_code)+VG_(tramp_sigreturn_offset);

   synth_ucontext(tst->tid, siginfo, mask, &uc, &frame->fpstate);

   VG_(memcpy)(&frame->sigContext, &uc.uc_mcontext, 
	       sizeof(struct vki_sigcontext));
   frame->sigContext.oldmask = mask->sig[0];

   VG_TRACK( post_mem_write, Vg_CoreSignal, tst->tid, 
             esp, offsetof(struct sigframe, vg) );

   build_vg_sigframe(&frame->vg, tst, mask, flags, sigNo);
   
   return esp;
}


static Addr build_rt_sigframe(ThreadState *tst,
			      Addr esp_top_of_frame,
			      const vki_siginfo_t *siginfo,
			      void *handler, UInt flags,
			      const vki_sigset_t *mask,
			      void *restorer)
{
   struct rt_sigframe *frame;
   Addr esp = esp_top_of_frame;
   Int	sigNo = siginfo->si_signo;

   vg_assert((flags & VKI_SA_SIGINFO) != 0);

   esp -= sizeof(*frame);
   esp = ROUNDDN(esp, 16);
   frame = (struct rt_sigframe *)esp;

   if (!extend(tst, esp, sizeof(*frame)))
      return esp_top_of_frame;

   /* retaddr, sigNo, pSiginfo, puContext fields are to be written */
   VG_TRACK( pre_mem_write, Vg_CoreSignal, tst->tid, "rt signal handler frame", 
	     esp, offsetof(struct rt_sigframe, vg) );

   frame->sigNo = sigNo;

   if (flags & VKI_SA_RESTORER)
      frame->retaddr = (Addr)restorer;
   else
      frame->retaddr 
         = VG_(client_trampoline_code)+VG_(tramp_rt_sigreturn_offset);

   frame->psigInfo = (Addr)&frame->sigInfo;
   frame->puContext = (Addr)&frame->uContext;
   VG_(memcpy)(&frame->sigInfo, siginfo, sizeof(vki_siginfo_t));

   /* SIGILL defines addr to be the faulting address */
   if (sigNo == VKI_SIGILL && siginfo->si_code > 0)
      frame->sigInfo._sifields._sigfault._addr 
         = (void*)tst->arch.vex.guest_EIP;

   synth_ucontext(tst->tid, siginfo, mask, &frame->uContext, &frame->fpstate);

   VG_TRACK( post_mem_write,  Vg_CoreSignal, tst->tid, 
             esp, offsetof(struct rt_sigframe, vg) );

   build_vg_sigframe(&frame->vg, tst, mask, flags, sigNo);
   
   return esp;
}


/* EXPORTED */
void VG_(sigframe_create)( ThreadId tid, 
                           Addr esp_top_of_frame,
                           const vki_siginfo_t *siginfo,
                           void *handler, 
                           UInt flags,
                           const vki_sigset_t *mask,
		           void *restorer )
{
   Addr		esp;
   ThreadState* tst = VG_(get_ThreadState)(tid);

   if (flags & VKI_SA_SIGINFO)
      esp = build_rt_sigframe(tst, esp_top_of_frame, siginfo, 
                                   handler, flags, mask, restorer);
   else
      esp = build_sigframe(tst, esp_top_of_frame, 
                                siginfo, handler, flags, mask, restorer);

   /* Set the thread so it will next run the handler. */
   /* tst->m_esp  = esp;  also notify the tool we've updated ESP */
   STACK_PTR(VG_(threads)[tid].arch) = esp;
   VG_TRACK( post_reg_write, Vg_CoreSignal, tid, O_STACK_PTR, sizeof(Addr));

   //VG_(printf)("handler = %p\n", handler);
   tst->arch.vex.guest_EIP = (Addr) handler;
   /* This thread needs to be marked runnable, but we leave that the
      caller to do. */

   if (0)
      VG_(printf)("pushed signal frame; %%ESP now = %p, "
                  "next %%EIP = %p, status=%d\n", 
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
      VG_(message)(Vg_UserMsg, "Thread %d return signal frame "
                               "corrupted.  Killing process.",
		   tst->tid);
      VG_(set_default_handler)(VKI_SIGSEGV);
      VG_(synth_fault)(tst->tid);
      *sigNo = VKI_SIGSEGV;
      return False;
   }
   tst->sig_mask        = frame->mask;
   tst->tmp_sig_mask    = frame->mask;
   tst->arch.vex_shadow = frame->vex_shadow;
   /* HACK ALERT */
   tst->arch.vex        = frame->vex;
   /* end HACK ALERT */
   *sigNo               = frame->sigNo_private;
   return True;
}

static 
void restore_sigcontext( ThreadState *tst, 
                         struct vki_sigcontext *sc, 
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
//::    tst->arch.vex.guest_EIP     = sc->eip;

   tst->arch.vex.guest_CS      = sc->cs; 
   tst->arch.vex.guest_SS      = sc->ss;
   tst->arch.vex.guest_DS      = sc->ds;
   tst->arch.vex.guest_ES      = sc->es;
   tst->arch.vex.guest_FS      = sc->fs;
   tst->arch.vex.guest_GS      = sc->gs;

//::    restore_i387(&tst->arch, fpstate);
}


static 
SizeT restore_sigframe ( ThreadState *tst, 
                         struct sigframe *frame, Int *sigNo )
{
   if (restore_vg_sigframe(tst, &frame->vg, sigNo))
      restore_sigcontext(tst, &frame->sigContext, &frame->fpstate);

   return sizeof(*frame);
}

static 
SizeT restore_rt_sigframe ( ThreadState *tst, 
                            struct rt_sigframe *frame, Int *sigNo )
{
   if (restore_vg_sigframe(tst, &frame->vg, sigNo))
      restore_sigcontext(tst, &frame->uContext.uc_mcontext, &frame->fpstate);

   return sizeof(*frame);
}


/* EXPORTED */
void VG_(sigframe_destroy)( ThreadId tid, Bool isRT )
{
   Addr          esp;
   ThreadState*  tst;
   SizeT	 size;
   Int		 sigNo;

   tst = VG_(get_ThreadState)(tid);

   /* Correctly reestablish the frame base address. */
   esp   = tst->arch.vex.guest_ESP;

   if (!isRT)
      size = restore_sigframe(tst, (struct sigframe *)esp, &sigNo);
   else
      size = restore_rt_sigframe(tst, (struct rt_sigframe *)esp, &sigNo);

   VG_TRACK( die_mem_stack_signal, esp - VGA_STACK_REDZONE_SZB,
             size + VGA_STACK_REDZONE_SZB );

   if (VG_(clo_trace_signals))
      VG_(message)(
         Vg_DebugMsg, 
         "VGA_(signal_return) (thread %d): isRT=%d valid magic; EIP=%p", 
         tid, isRT, tst->arch.vex.guest_EIP);

   /* tell the tools */
   VG_TRACK( post_deliver_signal, tid, sigNo );
}

//:: /*------------------------------------------------------------*/
//:: /*--- Making coredumps                                     ---*/
//:: /*------------------------------------------------------------*/
//:: 
//:: void VGA_(fill_elfregs_from_tst)(struct vki_user_regs_struct* regs, 
//::                                  const arch_thread_t* arch)
//:: {
//::    regs->eflags = arch->m_eflags;
//::    regs->esp    = arch->m_esp;
//::    regs->eip    = arch->m_eip;
//:: 
//::    regs->ebx    = arch->m_ebx;
//::    regs->ecx    = arch->m_ecx;
//::    regs->edx    = arch->m_edx;
//::    regs->esi    = arch->m_esi;
//::    regs->edi    = arch->m_edi;
//::    regs->ebp    = arch->m_ebp;
//::    regs->eax    = arch->m_eax;
//:: 
//::    regs->cs     = arch->m_cs;
//::    regs->ds     = arch->m_ds;
//::    regs->ss     = arch->m_ss;
//::    regs->es     = arch->m_es;
//::    regs->fs     = arch->m_fs;
//::    regs->gs     = arch->m_gs;
//:: }
//:: 
//:: static void fill_fpu(vki_elf_fpregset_t *fpu, const Char *from)
//:: {
//::    if (VG_(have_ssestate)) {
//::       UShort *to;
//::       Int i;
//:: 
//::       /* This is what the kernel does */
//::       VG_(memcpy)(fpu, from, 7*sizeof(long));
//::    
//::       to = (UShort *)&fpu->st_space[0];
//::       from += 18 * sizeof(UShort);
//:: 
//::       for (i = 0; i < 8; i++, to += 5, from += 8) 
//:: 	 VG_(memcpy)(to, from, 5*sizeof(UShort));
//::    } else
//::       VG_(memcpy)(fpu, from, sizeof(*fpu));
//:: }
//:: 
//:: void VGA_(fill_elffpregs_from_tst)( vki_elf_fpregset_t* fpu,
//::                                     const arch_thread_t* arch)
//:: {
//::    fill_fpu(fpu, (const Char *)&arch->m_sse);
//:: }
//:: 
//:: void VGA_(fill_elffpxregs_from_tst) ( vki_elf_fpxregset_t* xfpu,
//::                                       const arch_thread_t* arch )
//:: {
//::    VG_(memcpy)(xfpu, arch->m_sse.state, sizeof(*xfpu));
//:: }

/*--------------------------------------------------------------------*/
/*--- end                                     sigframe-x86-linux.c ---*/
/*--------------------------------------------------------------------*/
