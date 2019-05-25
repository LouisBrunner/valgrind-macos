
/*--------------------------------------------------------------------*/
/*--- Create/destroy signal delivery frames.                       ---*/
/*---                                           sigframe-solaris.c ---*/
/*--------------------------------------------------------------------*/

/*
   This file is part of Valgrind, a dynamic binary instrumentation
   framework.

   Copyright (C) 2011-2017 Petr Pavlu
      setup@dagobah.cz

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

#if defined(VGP_x86_solaris) || defined(VGP_amd64_solaris)

#include "pub_core_basics.h"
#include "pub_core_vki.h"
#include "pub_core_threadstate.h"
#include "pub_core_aspacemgr.h"
#include "pub_core_libcassert.h"
#include "pub_core_libcbase.h"
#include "pub_core_libcprint.h"
#include "pub_core_machine.h"
#include "pub_core_options.h"
#include "pub_core_signals.h"
#include "pub_core_tooliface.h"
#include "pub_core_sigframe.h"      /* Self */
#include "pub_core_syswrap.h"
#include "priv_sigframe.h"

/* This module creates and removes signal frames for signal deliveries
   on x86/amd64-solaris. */

/* Create a signal frame for thread 'tid'.  Make a 3-arg frame regardless of
   whether the client originally requested a 1-arg version (no SA_SIGINFO) or
   a 3-arg one (SA_SIGINFO) since in the former case, the x86/amd64 calling
   conventions will simply cause the extra 2 args to be ignored (inside the
   handler). */
void VG_(sigframe_create)(ThreadId tid, Bool on_altstack,
                          Addr sp_top_of_frame, const vki_siginfo_t *siginfo,
                          const struct vki_ucontext *siguc,
                          void *handler, UInt flags, const vki_sigset_t *mask,
                          void *restorer)
{
   ThreadState *tst = VG_(get_ThreadState)(tid);
   Addr esp;
   vki_sigframe_t *frame;
   Int signo = siginfo->si_signo;

   /* Calculate new stack pointer. */
   esp = sp_top_of_frame - sizeof(vki_sigframe_t);
   esp = VG_ROUNDDN(esp, 16) - sizeof(UWord);

   if (!ML_(sf_maybe_extend_stack)(tst, esp, sp_top_of_frame - esp, flags))
      return;

   /* Fill in the frame. */
   frame = (vki_sigframe_t*)esp;

   /* Set a bogus return address.  This return address should be never used
      because to return from a signal handler a program has to call
      setcontext() explicitly. */
   frame->return_addr = (void*)~0UL;

   /* Save current context.  (This has to be done before the thread state is
      modified in any way.) */
   VG_(save_context)(tid, &frame->ucontext, Vg_CoreSignal);

   /* Fill in the siginfo. */
   frame->siginfo = *siginfo;
   /* Set expected si_addr value.

      Manual page siginfo.h(3HEAD) describes that some signals define si_addr
      to be an address of the faulting instruction (SIGILL). Then it is needed
      to change the real CPU address to the VCPU address. Some signals define
      si_addr to be an address of the faulting memory reference (SIGSEGV,
      SIGBUS). Then the address should be passed unmodified.

      However documentation contained in the manpage does not reflect the
      reality found in the Solaris kernel - uts/<arch>/os/trap.c. Here one can
      observe that in some cases si_addr is set to address provided by the
      underlying subsystem. In some cases si_addr is set to the current
      program counter. Other signals are missing documentation altogether.
      It is almost impossible to determine what value is stored in si_addr
      based on the information provided by kernel to the signal handler.

      POSIX.1-2008 says about si_addr:
      SIGILL, SIGFPE ... Address of faulting instruction.
      SIGSEGV, SIGBUS ... Address of faulting memory reference.
      For some implementations, the value of si_addr may be inaccurate.

      See tests none/tests/faultstatus and none/tests/x86/badseg for examples.
      The code below simply follows the POSIX standard, but propagates any
      possibly incorrect values from the kernel to the user.
    */
   switch (signo) {
   case VKI_SIGSEGV:
      switch (siginfo->si_code) {
      case VKI_SEGV_ACCERR:
      case VKI_SEGV_MAPERR:
      default:
         break;
      case VKI_SEGV_MADE_UP_GPF:
         /* Translate si_code synthesized by Valgrind to SEGV_MAPPER. */
         frame->siginfo.si_code = VKI_SEGV_MAPERR;
         break;
      }
      break;
   case VKI_SIGBUS:
      break;
   case VKI_SIGFPE:
   case VKI_SIGILL:
   case VKI_SIGTRAP:
      frame->siginfo.si_addr = (void*)VG_(get_IP)(tid);
      break;
   case VKI_SIGPROF:
      frame->siginfo.si_faddr = (void*)VG_(get_IP)(tid);
      break;
   default:
      break;
   }
   VG_TRACK(post_mem_write, Vg_CoreSignal, tid, (Addr)&frame->siginfo,
            sizeof(frame->siginfo));

   /* Save the signal number in an unused slot.  Later, when a return from the
      signal is made, this value is used to inform the tool that the
      processing for the given signal has ended. */
   VKI_UC_SIGNO(&frame->ucontext) = signo | ((~(UWord)signo & 0xFFFF) << 16);
   /* Old context has to point to the saved ucontext. */
   tst->os_state.oldcontext = &frame->ucontext;
   /* Save ERR and TRAPNO if siguc is present. */
   if (siguc) {
      frame->ucontext.uc_mcontext.gregs[VKI_REG_ERR]
         = siguc->uc_mcontext.gregs[VKI_REG_ERR];
      VG_TRACK(post_mem_write, Vg_CoreSignal, tid,
               (Addr)&frame->ucontext.uc_mcontext.gregs[VKI_REG_ERR],
               sizeof(UWord));
      frame->ucontext.uc_mcontext.gregs[VKI_REG_TRAPNO]
         = siguc->uc_mcontext.gregs[VKI_REG_TRAPNO];
      VG_TRACK(post_mem_write, Vg_CoreSignal, tid,
               (Addr)&frame->ucontext.uc_mcontext.gregs[VKI_REG_TRAPNO],
               sizeof(UWord));
   }

   /* Prepare parameters for a signal handler. */
   frame->a1_signo = signo;
   /* The first parameter has to be 16-byte aligned, resembling function
      calls. */
   {
      /* Using
         vg_assert(VG_IS_16_ALIGNED(&frame->a1_signo));
         seems to get miscompiled on amd64 with GCC 4.7.2. */
      Addr signo_addr = (Addr)&frame->a1_signo;
      vg_assert(VG_IS_16_ALIGNED(signo_addr));
   }
   frame->a2_siginfo = &frame->siginfo;
   VG_TRACK(post_mem_write, Vg_CoreSignal, tid, (Addr)&frame->a1_signo,
            sizeof(frame->a1_signo) + sizeof(frame->a2_siginfo));
#if defined(VGP_x86_solaris)
   frame->a3_ucontext = &frame->ucontext;
   VG_TRACK(post_mem_write, Vg_CoreSignal, tid, (Addr)&frame->a3_ucontext,
            sizeof(frame->a3_ucontext));
#elif defined(VGP_amd64_solaris)
   tst->arch.vex.guest_RDI = signo;
   VG_TRACK(post_reg_write, Vg_CoreSignal, tid, offsetof(VexGuestAMD64State,
            guest_RDI), sizeof(ULong));
   tst->arch.vex.guest_RSI = (Addr)&frame->siginfo;
   VG_TRACK(post_reg_write, Vg_CoreSignal, tid, offsetof(VexGuestAMD64State,
            guest_RSI), sizeof(ULong));
   tst->arch.vex.guest_RDX = (Addr)&frame->ucontext;
   VG_TRACK(post_reg_write, Vg_CoreSignal, tid, offsetof(VexGuestAMD64State,
            guest_RDX), sizeof(ULong));
#endif

   /* Set up the stack pointer. */
   vg_assert(esp == (Addr)&frame->return_addr);
   VG_(set_SP)(tid, esp);
   VG_TRACK(post_reg_write, Vg_CoreSignal, tid, VG_O_STACK_PTR, sizeof(Addr));

   /* Set up the program counter. Note that we don't inform a tool about IP
      write because IP is always defined. */
   VG_(set_IP)(tid, (Addr)handler);

   /* If the signal is delivered on the alternate stack, copy it out to
      ustack.  This has to be done after setting a new IP so the SS_ONSTACK
      flag is set by VG_(do_sys_sigaltstack)(). */
   if (on_altstack && tst->os_state.ustack
       && VG_(am_is_valid_for_client)((Addr)tst->os_state.ustack,
                                      sizeof(*tst->os_state.ustack),
                                      VKI_PROT_WRITE)) {
      SysRes res;
      vki_stack_t altstack;

      /* Get information about alternate stack. */
      res = VG_(do_sys_sigaltstack)(tid, NULL, &altstack);
      vg_assert(!sr_isError(res));

      /* Copy it to ustack. */
      *tst->os_state.ustack = altstack;
      VG_TRACK(post_mem_write, Vg_CoreSignal, tid, (Addr)tst->os_state.ustack,
               sizeof(*tst->os_state.ustack));
   }

   if (VG_(clo_trace_signals))
      VG_(message)(Vg_DebugMsg,
                   "sigframe_create (thread %u): next IP=%#lx, "
                   "next SP=%#lx\n",
                   tid, (Addr)handler, (Addr)frame);
}

void VG_(sigframe_destroy)(ThreadId tid, Bool isRT)
{
   /* Not used on Solaris. */
   vg_assert(0);
}

void VG_(sigframe_return)(ThreadId tid, const vki_ucontext_t *uc)
{
   Int signo;

   /* Check if a signal number was saved in the restored context. */
   signo = VKI_UC_SIGNO_CONST(uc) & 0xFFFF;
   if (!signo || signo != ((~VKI_UC_SIGNO_CONST(uc) >> 16) & 0xFFFF))
      return;

   /* Note: The active tool should be informed here about the dead stack area.
      However, this was already done when the original context was restored (in
      VG_(restore_context)()) so it is not necessary to do it here again.

      There is a small nuance though, VG_(restore_context)() triggers the
      die_mem_stack event while in this case, it should really trigger the
      die_mem_stack_signal event.  This is not currently a problem because all
      official tools handle these two events in the same way.

      If a return from an alternate stack is made then no die_mem_stack event
      is currently triggered. */

   /* Returning from a signal handler. */
   if (VG_(clo_trace_signals))
      VG_(message)(Vg_DebugMsg,
                   "sigframe_return (thread %u): IP=%#lx\n",
                   tid, VG_(get_IP)(tid));

   /* Tell the tool. */
   VG_TRACK(post_deliver_signal, tid, signo);
}

#endif // defined(VGP_x86_solaris) || defined(VGP_amd64_solaris)

/*--------------------------------------------------------------------*/
/*--- end                                                          ---*/
/*--------------------------------------------------------------------*/
