
/*--------------------------------------------------------------------*/
/*--- x86 signals, etc.                                   signal.c ---*/
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
      vki_ksiginfo_t sigInfo;
      /* pointed to by puContext */
      struct vki_ucontext uContext;

      /* Safely-saved version of sigNo, as described above. */
      Int  sigNo_private;
      /* Saved processor state. */
      UInt m_sse[VG_SIZE_OF_SSESTATE_W];

      UInt m_eax;
      UInt m_ecx;
      UInt m_edx;
      UInt m_ebx;
      UInt m_ebp;
      UInt m_esp;
      UInt m_esi;
      UInt m_edi;
      UInt m_eflags;
      Addr m_eip;

      UInt sh_eax;
      UInt sh_ebx;
      UInt sh_ecx;
      UInt sh_edx;
      UInt sh_esi;
      UInt sh_edi;
      UInt sh_ebp;
      UInt sh_esp;
      UInt sh_eflags;

      /* saved signal mask to be restored when handler returns */
      vki_ksigset_t	mask;

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
static void synth_ucontext(ThreadId tid, const vki_ksiginfo_t *si, 
			   const vki_ksigset_t *set, struct vki_ucontext *uc)
{
   ThreadState *tst = VG_(get_ThreadState)(tid);
   struct vki_sigcontext *sc = &uc->uc_mcontext;

   VG_(memset)(uc, 0, sizeof(*uc));

   uc->uc_flags = 0;
   uc->uc_link = 0;
   uc->uc_sigmask = *set;
   uc->uc_stack = tst->altstack;

#define SC(reg)	sc->reg = tst->arch.m_##reg
   SC(gs);
   SC(fs);
   SC(es);
   SC(ds);

   SC(edi);
   SC(esi);
   SC(ebp);
   SC(esp);
   SC(ebx);
   SC(edx);
   SC(ecx);
   SC(eax);

   SC(eip);
   SC(cs);
   SC(eflags);
   SC(ss);
   /* XXX esp_at_signal */
   /* XXX trapno */
   /* XXX err */
#undef SC

   sc->cr2 = (UInt)si->_sifields._sigfault._addr;
}

#define SET_SIGNAL_ESP(zztid, zzval) \
   SET_THREAD_REG(zztid, zzval, ARCH_STACK_PTR, R_STACK_PTR, \
                  post_reg_write_deliver_signal)

void VGA_(push_signal_frame)(ThreadId tid, Addr esp_top_of_frame,
                             const vki_ksiginfo_t *siginfo,
                             void *handler, UInt flags,
                             const vki_ksigset_t *mask)
{
   Addr		esp;
   ThreadState* tst;
   Int          i;
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
   VG_TRACK( post_mem_write, (Addr)frame, offsetof(VgSigFrame, handlerArgs) );

   if (flags & VKI_SA_SIGINFO) {
      /* if the client asked for a siginfo delivery, then build the stack that way */
      VG_TRACK( pre_mem_write, Vg_CoreSignal, tid, "signal handler frame (siginfo)", 
		(Addr)&frame->handlerArgs, sizeof(frame->handlerArgs.sigInfo) );
      frame->handlerArgs.sigInfo.psigInfo   = (Addr)&frame->sigInfo;
      frame->handlerArgs.sigInfo.puContext = (Addr)&frame->uContext;
      VG_TRACK( post_mem_write, (Addr)&frame->handlerArgs, sizeof(frame->handlerArgs.sigInfo) );

      VG_TRACK( pre_mem_write, Vg_CoreSignal, tid, "signal handler frame (siginfo)", 
		(Addr)&frame->sigInfo, sizeof(frame->sigInfo) );
      VG_(memcpy)(&frame->sigInfo, siginfo, sizeof(vki_ksiginfo_t));
      VG_TRACK( post_mem_write, (Addr)&frame->sigInfo, sizeof(frame->sigInfo) );

      VG_TRACK( pre_mem_write, Vg_CoreSignal, tid, "signal handler frame (siginfo)", 
		(Addr)&frame->uContext, sizeof(frame->uContext) );
      synth_ucontext(tid, siginfo, mask, &frame->uContext);
      VG_TRACK( post_mem_write, (Addr)&frame->uContext, sizeof(frame->uContext) );
   } else {
      struct vki_ucontext uc;

      /* otherwise just put the sigcontext there */

      synth_ucontext(tid, siginfo, mask, &uc);

      VG_TRACK( pre_mem_write, Vg_CoreSignal, tid, "signal handler frame (sigcontext)", 
		(Addr)&frame->handlerArgs, sizeof(frame->handlerArgs.sigContext) );
      VG_(memcpy)(&frame->handlerArgs.sigContext, &uc.uc_mcontext, 
		  sizeof(struct vki_sigcontext));
      VG_TRACK( post_mem_write, (Addr)&frame->handlerArgs, 
		sizeof(frame->handlerArgs.sigContext) );
      
      frame->handlerArgs.sigContext.oldmask = tst->sig_mask.ws[0];
   }

   frame->magicPI    = 0x31415927;

   for (i = 0; i < VG_SIZE_OF_SSESTATE_W; i++)
      frame->m_sse[i] = tst->arch.m_sse[i];

   frame->m_eax      = tst->arch.m_eax;
   frame->m_ecx      = tst->arch.m_ecx;
   frame->m_edx      = tst->arch.m_edx;
   frame->m_ebx      = tst->arch.m_ebx;
   frame->m_ebp      = tst->arch.m_ebp;
   frame->m_esp      = tst->arch.m_esp;
   frame->m_esi      = tst->arch.m_esi;
   frame->m_edi      = tst->arch.m_edi;
   frame->m_eflags   = tst->arch.m_eflags;
   frame->m_eip      = tst->arch.m_eip;

   if (VG_(needs).shadow_regs) {
      frame->sh_eax     = tst->arch.sh_eax;
      frame->sh_ecx     = tst->arch.sh_ecx;
      frame->sh_edx     = tst->arch.sh_edx;
      frame->sh_ebx     = tst->arch.sh_ebx;
      frame->sh_ebp     = tst->arch.sh_ebp;
      frame->sh_esp     = tst->arch.sh_esp;
      frame->sh_esi     = tst->arch.sh_esi;
      frame->sh_edi     = tst->arch.sh_edi;
      frame->sh_eflags  = tst->arch.sh_eflags;
   }

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

   tst->arch.m_eip  = (Addr) handler;
   /* This thread needs to be marked runnable, but we leave that the
      caller to do. */

   if (0)
      VG_(printf)("pushed signal frame; %%ESP now = %p, next %%EBP = %p, status=%d\n", 
		  esp, tst->arch.m_eip, tst->status);
}

Int VGA_(pop_signal_frame)(ThreadId tid)
{
   Addr          esp;
   Int           i;
   VgSigFrame*   frame;
   ThreadState*  tst;

   vg_assert(VG_(is_valid_tid)(tid));
   tst = & VG_(threads)[tid];

   /* Correctly reestablish the frame base address. */
   esp   = tst->arch.m_esp;
   frame = (VgSigFrame*)
              (esp -4 /* because the handler's RET pops the RA */
                  +20 /* because signalreturn_bogusRA pushes 5 words */);

   vg_assert(frame->magicPI == 0x31415927);
   vg_assert(frame->magicE  == 0x27182818);
   if (VG_(clo_trace_signals))
      VG_(message)(Vg_DebugMsg, 
         "vg_pop_signal_frame (thread %d): valid magic; EIP=%p", tid, frame->m_eip);

   /* Mark the frame structure as nonaccessible. */
   VG_TRACK( die_mem_stack_signal, (Addr)frame, sizeof(VgSigFrame) );

   /* restore machine state */
   for (i = 0; i < VG_SIZE_OF_SSESTATE_W; i++)
      tst->arch.m_sse[i] = frame->m_sse[i];

   tst->arch.m_eax     = frame->m_eax;
   tst->arch.m_ecx     = frame->m_ecx;
   tst->arch.m_edx     = frame->m_edx;
   tst->arch.m_ebx     = frame->m_ebx;
   tst->arch.m_ebp     = frame->m_ebp; 
   tst->arch.m_esp     = frame->m_esp;
   tst->arch.m_esi     = frame->m_esi;
   tst->arch.m_edi     = frame->m_edi;
   tst->arch.m_eflags  = frame->m_eflags;
   tst->arch.m_eip     = frame->m_eip;

   if (VG_(needs).shadow_regs) {
      tst->arch.sh_eax     = frame->sh_eax;
      tst->arch.sh_ecx     = frame->sh_ecx;
      tst->arch.sh_edx     = frame->sh_edx;
      tst->arch.sh_ebx     = frame->sh_ebx;
      tst->arch.sh_ebp     = frame->sh_ebp; 
      tst->arch.sh_esp     = frame->sh_esp;
      tst->arch.sh_esi     = frame->sh_esi;
      tst->arch.sh_edi     = frame->sh_edi;
      tst->arch.sh_eflags  = frame->sh_eflags;
   }

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

void VGA_(fill_elfregs_from_BB)(struct user_regs_struct* regs)
{
   regs->eflags = VG_(baseBlock)[VGOFF_(m_eflags)];
   regs->esp    = VG_(baseBlock)[VGOFF_(m_esp)];
   regs->eip    = VG_(baseBlock)[VGOFF_(m_eip)];

   regs->ebx    = VG_(baseBlock)[VGOFF_(m_ebx)];
   regs->ecx    = VG_(baseBlock)[VGOFF_(m_ecx)];
   regs->edx    = VG_(baseBlock)[VGOFF_(m_edx)];
   regs->esi    = VG_(baseBlock)[VGOFF_(m_esi)];
   regs->edi    = VG_(baseBlock)[VGOFF_(m_edi)];
   regs->ebp    = VG_(baseBlock)[VGOFF_(m_ebp)];
   regs->eax    = VG_(baseBlock)[VGOFF_(m_eax)];

   regs->cs     = VG_(baseBlock)[VGOFF_(m_cs)];
   regs->ds     = VG_(baseBlock)[VGOFF_(m_ds)];
   regs->ss     = VG_(baseBlock)[VGOFF_(m_ss)];
   regs->es     = VG_(baseBlock)[VGOFF_(m_es)];
   regs->fs     = VG_(baseBlock)[VGOFF_(m_fs)];
   regs->gs     = VG_(baseBlock)[VGOFF_(m_gs)];
}


void VGA_(fill_elfregs_from_tst)(struct user_regs_struct* regs, 
                                 const arch_thread_t* arch)
{
   regs->eflags = arch->m_eflags;
   regs->esp    = arch->m_esp;
   regs->eip    = arch->m_eip;

   regs->ebx    = arch->m_ebx;
   regs->ecx    = arch->m_ecx;
   regs->edx    = arch->m_edx;
   regs->esi    = arch->m_esi;
   regs->edi    = arch->m_edi;
   regs->ebp    = arch->m_ebp;
   regs->eax    = arch->m_eax;

   regs->cs     = arch->m_cs;
   regs->ds     = arch->m_ds;
   regs->ss     = arch->m_ss;
   regs->es     = arch->m_es;
   regs->fs     = arch->m_fs;
   regs->gs     = arch->m_gs;
}

static void fill_fpu(elf_fpregset_t *fpu, const Char *from)
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

void VGA_(fill_elffpregs_from_BB)( elf_fpregset_t* fpu )
{
   fill_fpu(fpu, (const Char *)&VG_(baseBlock)[VGOFF_(m_ssestate)]);
}

void VGA_(fill_elffpregs_from_tst)( elf_fpregset_t* fpu,
                                    const arch_thread_t* arch)
{
   fill_fpu(fpu, (const Char *)&arch->m_sse);
}

void VGA_(fill_elffpxregs_from_BB) ( elf_fpxregset_t* xfpu )
{
   VG_(memcpy)(xfpu, &VG_(baseBlock)[VGOFF_(m_ssestate)], sizeof(*xfpu));
}

void VGA_(fill_elffpxregs_from_tst) ( elf_fpxregset_t* xfpu,
                                      const arch_thread_t* arch )
{
   VG_(memcpy)(xfpu, arch->m_sse, sizeof(*xfpu));
}

/*--------------------------------------------------------------------*/
/*--- end                                                          ---*/
/*--------------------------------------------------------------------*/
