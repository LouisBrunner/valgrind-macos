
/*--------------------------------------------------------------------*/
/*--- x86 signals, etc.                               x86/signal.c ---*/
/*--------------------------------------------------------------------*/

/*
   This file is part of Valgrind, an extensible x86 protected-mode
   emulator for monitoring program execution on x86-Unixes.

   Copyright (C) 2000-2004 Nicholas Nethercote
      njn25@cam.ac.uk

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

#include "libvex_guest_x86.h"

/*------------------------------------------------------------*/
/*--- Signal frame                                         ---*/
/*------------------------------------------------------------*/

// A structure in which to save the application's registers
// during the execution of signal handlers.

typedef
   struct {
      /* There are two different stack frame formats, depending on
	 whether the client set the SA_SIGINFO flag for the handler.
	 This structure is put onto the client's stack as part of
	 signal delivery, and therefore appears as the signal
	 handler's arguments.

	 The first two words are common for both frame formats -
	 they're the return address and the signal number. */

      /* Sig handler's (bogus) return address */
      Addr retaddr;
      /* The arg to the sig handler.  We need to inspect this after
         the handler returns, but it's unreasonable to assume that the
         handler won't change it.  So we keep a second copy of it in
         sigNo_private. */
      Int  sigNo;

      /* This is where the two frames start differing. */
      union {
	 struct {		/* set SA_SIGINFO */
	    /* ptr to siginfo_t. */
	    Addr psigInfo;

	    /* ptr to ucontext */
	    Addr puContext;
	 } sigInfo;
	 struct vki_sigcontext sigContext; /* did not set SA_SIGINFO */
      } handlerArgs;

      /* The rest are private fields which the handler is unaware of. */

      /* Sanity check word. */
      UInt magicPI;
      /* pointed to by psigInfo */
      vki_siginfo_t sigInfo;
      /* pointed to by puContext */
      struct vki_ucontext uContext;

      /* Safely-saved version of sigNo, as described above. */
      Int  sigNo_private;

      /* Saved processor state. */
      VexGuestX86State vex;
      VexGuestX86State vex_shadow;

      /* saved signal mask to be restored when handler returns */
      vki_sigset_t	mask;

      /* Scheduler-private stuff: what was the thread's status prior to
         delivering this signal? */
      ThreadStatus status;
      void* /*pthread_mutex_t* */ associated_mx;
      void* /*pthread_cond_t* */ associated_cv;

      /* Sanity check word.  Is the highest-addressed word; do not
         move!*/
      UInt magicE;
   }
   VgSigFrame;

/*------------------------------------------------------------*/
/*--- Signal operations                                    ---*/
/*------------------------------------------------------------*/

/* Make up a plausible-looking thread state from the thread's current state */
static void synth_ucontext(ThreadId tid, const vki_siginfo_t *si, 
			   const vki_sigset_t *set, struct vki_ucontext *uc)
{
   ThreadState *tst = VG_(get_ThreadState)(tid);
   struct vki_sigcontext *sc = &uc->uc_mcontext;

   VG_(memset)(uc, 0, sizeof(*uc));

   uc->uc_flags = 0;
   uc->uc_link = 0;
   uc->uc_sigmask = *set;
   uc->uc_stack = tst->altstack;

#define SC2(reg,REG)  sc->reg = tst->arch.vex.guest_##REG
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
#undef SC2

   sc->cr2 = (UInt)si->_sifields._sigfault._addr;
}

#define SET_SIGNAL_ESP(zztid, zzval) \
   SET_THREAD_REG(zztid, zzval, STACK_PTR, post_reg_write, \
                  Vg_CoreSignal, zztid, O_STACK_PTR, sizeof(Addr))

void VGA_(push_signal_frame)(ThreadId tid, Addr esp_top_of_frame,
                             const vki_siginfo_t *siginfo,
                             void *handler, UInt flags,
                             const vki_sigset_t *mask)
{
   Addr		esp;
   ThreadState* tst;
   VgSigFrame*  frame;
   Int		sigNo = siginfo->si_signo;

   esp = esp_top_of_frame;
   esp -= sizeof(VgSigFrame);
   frame = (VgSigFrame*)esp;

   tst = & VG_(threads)[tid];

   /* For tracking memory events, indicate the entire frame has been
    * allocated, but pretend that only the first four words are written */
   VG_TRACK( new_mem_stack_signal, (Addr)frame, sizeof(VgSigFrame) );

   /* Assert that the frame is placed correctly. */
   vg_assert( (sizeof(VgSigFrame) & 0x3) == 0 );
   vg_assert( ((Char*)(&frame->magicE)) + sizeof(UInt) 
              == ((Char*)(esp_top_of_frame)) );

   /* retaddr, sigNo, psigInfo, puContext fields are to be written */
   VG_TRACK( pre_mem_write, Vg_CoreSignal, tid, "signal handler frame", 
                            (Addr)frame, offsetof(VgSigFrame, handlerArgs) );
   frame->retaddr    = (UInt)VG_(client_trampoline_code)+VG_(tramp_sigreturn_offset);
   frame->sigNo      = sigNo;
   frame->sigNo_private = sigNo;
   VG_TRACK( post_mem_write, Vg_CoreSignal, tid,
             (Addr)frame, offsetof(VgSigFrame, handlerArgs) );

   if (flags & VKI_SA_SIGINFO) {
      /* if the client asked for a siginfo delivery, then build the stack that way */
      VG_TRACK( pre_mem_write, Vg_CoreSignal, tid, "signal handler frame (siginfo)", 
		(Addr)&frame->handlerArgs, sizeof(frame->handlerArgs.sigInfo) );
      frame->handlerArgs.sigInfo.psigInfo   = (Addr)&frame->sigInfo;
      frame->handlerArgs.sigInfo.puContext = (Addr)&frame->uContext;
      VG_TRACK( post_mem_write, Vg_CoreSignal, tid,
                (Addr)&frame->handlerArgs, sizeof(frame->handlerArgs.sigInfo) );

      VG_TRACK( pre_mem_write, Vg_CoreSignal, tid, "signal handler frame (siginfo)", 
		(Addr)&frame->sigInfo, sizeof(frame->sigInfo) );
      VG_(memcpy)(&frame->sigInfo, siginfo, sizeof(vki_siginfo_t));
      VG_TRACK( post_mem_write, Vg_CoreSignal, tid, 
                (Addr)&frame->sigInfo, sizeof(frame->sigInfo) );

      VG_TRACK( pre_mem_write, Vg_CoreSignal, tid, "signal handler frame (siginfo)", 
		(Addr)&frame->uContext, sizeof(frame->uContext) );
      synth_ucontext(tid, siginfo, mask, &frame->uContext);
      VG_TRACK( post_mem_write, Vg_CoreSignal, tid, 
                (Addr)&frame->uContext, sizeof(frame->uContext) );
   } else {
      struct vki_ucontext uc;

      /* otherwise just put the sigcontext there */

      synth_ucontext(tid, siginfo, mask, &uc);

      VG_TRACK( pre_mem_write, Vg_CoreSignal, tid, "signal handler frame (sigcontext)", 
		(Addr)&frame->handlerArgs, sizeof(frame->handlerArgs.sigContext) );
      VG_(memcpy)(&frame->handlerArgs.sigContext, &uc.uc_mcontext, 
		  sizeof(struct vki_sigcontext));
      VG_TRACK( post_mem_write, Vg_CoreSignal, tid,
                (Addr)&frame->handlerArgs, sizeof(frame->handlerArgs.sigContext) );
      
      frame->handlerArgs.sigContext.oldmask = tst->sig_mask.sig[0];
   }

   frame->magicPI    = 0x31415927;

   frame->vex        = tst->arch.vex;
   frame->vex_shadow = tst->arch.vex_shadow;

   frame->mask = tst->sig_mask;

   /* If the thread is currently blocked in a syscall, we want it to
      resume as runnable. */
   if (tst->status == VgTs_WaitSys)
      frame->status = VgTs_Runnable;
   else
      frame->status = tst->status;
 
   frame->associated_mx = tst->associated_mx;
   frame->associated_cv = tst->associated_cv;

   frame->magicE     = 0x27182818;

   /* Ensure 'tid' and 'tst' correspond */
   vg_assert(& VG_(threads)[tid] == tst);
   /* Set the thread so it will next run the handler. */
   /* tst->m_esp  = esp; */
   SET_SIGNAL_ESP(tid, esp);

   tst->arch.vex.guest_EIP = (Addr) handler;
   /* This thread needs to be marked runnable, but we leave that the
      caller to do. */

   if (0)
      VG_(printf)("pushed signal frame; %%ESP now = %p, next %%EBP = %p, status=%d\n", 
		  esp, tst->arch.vex.guest_EIP, tst->status);
}

Int VGA_(pop_signal_frame)(ThreadId tid)
{
   Addr          esp;
   VgSigFrame*   frame;
   ThreadState*  tst;

   vg_assert(VG_(is_valid_tid)(tid));
   tst = & VG_(threads)[tid];

   /* Correctly reestablish the frame base address. */
   esp   = tst->arch.vex.guest_ESP;
   frame = (VgSigFrame*)
              (esp -4 /* because the handler's RET pops the RA */
                  +20 /* because signalreturn_bogusRA pushes 5 words */);

   vg_assert(frame->magicPI == 0x31415927);
   vg_assert(frame->magicE  == 0x27182818);
   if (VG_(clo_trace_signals))
      VG_(message)(Vg_DebugMsg, 
         "vg_pop_signal_frame (thread %d): valid magic; EIP=%p", tid, frame->vex.guest_EIP);

   /* Mark the frame structure as nonaccessible. */
   VG_TRACK( die_mem_stack_signal, (Addr)frame, sizeof(VgSigFrame) );

   /* restore machine state */
   tst->arch.vex        = frame->vex;
   tst->arch.vex_shadow = frame->vex_shadow;

   /* And restore the thread's status to what it was before the signal
      was delivered. */
   tst->status    = frame->status;

   tst->associated_mx = frame->associated_mx;
   tst->associated_cv = frame->associated_cv;

   tst->sig_mask  = frame->mask;

   /* don't use the copy exposed to the handler; it might have changed
      it. */
   return frame->sigNo_private; 
}

/*------------------------------------------------------------*/
/*--- Making coredumps                                     ---*/
/*------------------------------------------------------------*/

// Nb: these functions do *not* represent the right way to abstract out the
// arch-specific parts of coredumps.  Some rethinking is required.
#if 0
void VGA_(fill_elfregs_from_tst)(struct vki_user_regs_struct* regs, 
                                 ThreadArchState* arch)
{
   regs->eflags = LibVEX_GuestX86_get_eflags(&arch->vex);
   regs->esp    = arch->vex.guest_ESP;
   regs->eip    = arch->vex.guest_EIP;

   regs->ebx    = arch->vex.guest_EBX;
   regs->ecx    = arch->vex.guest_ECX;
   regs->edx    = arch->vex.guest_EDX;
   regs->esi    = arch->vex.guest_ESI;
   regs->edi    = arch->vex.guest_EDI;
   regs->ebp    = arch->vex.guest_EBP;
   regs->eax    = arch->vex.guest_EAX;

   regs->cs     = arch->vex.guest_CS;
   regs->ds     = arch->vex.guest_DS;
   regs->ss     = arch->vex.guest_SS;
   regs->es     = arch->vex.guest_ES;
   regs->fs     = arch->vex.guest_FS;
   regs->gs     = arch->vex.guest_GS;
}

static void fill_fpu(vki_elf_fpregset_t *fpu, const Char *from)
{
   if (VG_(have_ssestate)) {
      UShort *to;
      Int i;

      /* This is what the kernel does */
      VG_(memcpy)(fpu, from, 7*sizeof(long));
   
      to = (UShort *)&fpu->st_space[0];
      from += 18 * sizeof(UShort);

      for (i = 0; i < 8; i++, to += 5, from += 8) 
	 VG_(memcpy)(to, from, 5*sizeof(UShort));
   } else
      VG_(memcpy)(fpu, from, sizeof(*fpu));
}

void VGA_(fill_elffpregs_from_BB)( vki_elf_fpregset_t* fpu )
{
   fill_fpu(fpu, (const Char *)&VG_(baseBlock)[VGOFF_(m_ssestate)]);
}

void VGA_(fill_elffpregs_from_tst)( vki_elf_fpregset_t* fpu,
                                    const ThreadArchState* arch)
{
   fill_fpu(fpu, (const Char *)&arch->m_sse);
}

void VGA_(fill_elffpxregs_from_BB) ( vki_elf_fpxregset_t* xfpu )
{
   VG_(memcpy)(xfpu, &VG_(baseBlock)[VGOFF_(m_ssestate)], sizeof(*xfpu));
}

void VGA_(fill_elffpxregs_from_tst) ( vki_elf_fpxregset_t* xfpu,
                                      const ThreadArchState* arch )
{
   VG_(memcpy)(xfpu, arch->m_sse, sizeof(*xfpu));
}
#endif

/*--------------------------------------------------------------------*/
/*--- end                                                          ---*/
/*--------------------------------------------------------------------*/
