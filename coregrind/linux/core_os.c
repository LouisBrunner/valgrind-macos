
/*--------------------------------------------------------------------*/
/*--- OS-specific stuff.                           linux/core_os.c ---*/
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
#include "pub_core_options.h"
#include "pub_core_tooliface.h"

void VGA_(os_state_clear)(ThreadState *tst)
{
   tst->os_state.lwpid = 0;
   tst->os_state.threadgroup = 0;
}

void VGA_(os_state_init)(ThreadState *tst)
{
   tst->os_state.valgrind_stack_base = 0;
   tst->os_state.valgrind_stack_szB  = 0;

   VGA_(os_state_clear)(tst);
}

static void terminate(ThreadId tid, VgSchedReturnCode src)
{
   vg_assert(tid == VG_(master_tid));
   vg_assert(VG_(count_living_threads)() == 0);

   //--------------------------------------------------------------
   // Exit, according to the scheduler's return code
   //--------------------------------------------------------------
   switch (src) {
   case VgSrc_ExitSyscall: /* the normal way out */
      VG_(exit)( VG_(threads)[VG_(master_tid)].os_state.exitcode );
      /* NOT ALIVE HERE! */
      VG_(core_panic)("entered the afterlife in main() -- ExitSyscall");
      break; /* what the hell :) */

   case VgSrc_FatalSig:
      /* We were killed by a fatal signal, so replicate the effect */
      vg_assert(VG_(threads)[VG_(master_tid)].os_state.fatalsig != 0);
      VG_(kill_self)(VG_(threads)[VG_(master_tid)].os_state.fatalsig);
      VG_(core_panic)("main(): signal was supposed to be fatal");
      break;

   default:
      VG_(core_panic)("main(): unexpected scheduler return code");
   }
}

/* Run a thread from beginning to end. Does not return. */
void VGA_(thread_wrapper)(Word /*ThreadId*/ tidW)
{
   VgSchedReturnCode ret;
   ThreadId     tid = (ThreadId)tidW;
   ThreadState* tst = VG_(get_ThreadState)(tid);

   vg_assert(tst->status == VgTs_Init);

   /* make sure we get the CPU lock before doing anything significant */
   VG_(set_running)(tid);

   if (0)
      VG_(printf)("thread tid %d started: stack = %p\n",
		  tid, &tid);

   VG_TRACK ( post_thread_create, tst->os_state.parent, tid );

   tst->os_state.lwpid = VG_(gettid)();
   tst->os_state.threadgroup = VG_(getpid)();

   /* Thread created with all signals blocked; scheduler will set the
      appropriate mask */

   ret = VG_(scheduler)(tid);

   vg_assert(VG_(is_exiting)(tid));
   
   vg_assert(tst->status == VgTs_Runnable);
   vg_assert(VG_(is_running_thread)(tid));
   
   if (tid == VG_(master_tid)) {
      VG_(shutdown_actions)(tid);
      terminate(tid, ret);
   }
}

/* wait until all other threads are dead */
static Bool alldead(void *v)
{
   /* master_tid must be alive... */
   Int c = VG_(count_living_threads)();
   //VG_(printf)("alldead: count=%d\n", c);
   return c <= 1;
}

static void sigvgchld_handler(Int sig)
{
   VG_(printf)("got a sigvgchld?\n");
}

/* 
   Wait until some predicate about threadstates is satisfied.

   This uses SIGVGCHLD as a notification that it is now worth
   re-evaluating the predicate.
 */
static void wait_for_threadstate(Bool (*pred)(void *), void *arg)
{
   vki_sigset_t set, saved;
   struct vki_sigaction sa, old_sa;

   /* 
      SIGVGCHLD is set to be ignored, and is unblocked by default.
      This means all such signals are simply discarded.

      In this loop, we actually block it, and then poll for it with
      sigtimedwait.
    */
   VG_(sigemptyset)(&set);
   VG_(sigaddset)(&set, VKI_SIGVGCHLD);

   VG_(set_sleeping)(VG_(master_tid), VgTs_Yielding);
   VG_(sigprocmask)(VKI_SIG_BLOCK, &set, &saved);

   /* It shouldn't be necessary to set a handler, since the signal is
      always blocked, but it seems to be necessary to convice the
      kernel not to just toss the signal... */
   sa.ksa_handler = sigvgchld_handler;
   sa.sa_flags = 0;
   VG_(sigfillset)(&sa.sa_mask);
   VG_(sigaction)(VKI_SIGVGCHLD, &sa, &old_sa);

   vg_assert(old_sa.ksa_handler == VKI_SIG_IGN);

   while(!(*pred)(arg)) {
      struct vki_siginfo si;
      Int ret = VG_(sigtimedwait)(&set, &si, NULL);

      if (ret > 0 && VG_(clo_trace_signals))
	 VG_(message)(Vg_DebugMsg, "Got %d (code=%d) from tid lwp %d",
		      ret, si.si_code, si._sifields._kill._pid);
   }

   VG_(sigaction)(VKI_SIGVGCHLD, &old_sa, NULL);
   VG_(sigprocmask)(VKI_SIG_SETMASK, &saved, NULL);
   VG_(set_running)(VG_(master_tid));
}

void VGA_(reap_threads)(ThreadId self)
{
   vg_assert(self == VG_(master_tid));
   wait_for_threadstate(alldead, NULL);
}

/* The we need to know the address of it so it can be
   called at program exit. */
static Addr __libc_freeres_wrapper;

void VGA_(intercept_libc_freeres_wrapper)(Addr addr)
{
   __libc_freeres_wrapper = addr;
}

/* Final clean-up before terminating the process.  
   Clean up the client by calling __libc_freeres() (if requested) */
void VGA_(final_tidyup)(ThreadId tid)
{
   vg_assert(VG_(is_running_thread)(tid));
   
   if (!VG_(needs).libc_freeres ||
       !VG_(clo_run_libc_freeres) ||
       __libc_freeres_wrapper == 0)
      return;			/* can't/won't do it */

   if (VG_(clo_verbosity) > 2  ||
       VG_(clo_trace_syscalls) ||
       VG_(clo_trace_sched))
      VG_(message)(Vg_DebugMsg, 
		   "Caught __NR_exit; running __libc_freeres()");
      
   /* point thread context to point to libc_freeres_wrapper */
   INSTR_PTR(VG_(threads)[tid].arch) = __libc_freeres_wrapper;
   // XXX should we use a special stack?

   /* Block all blockable signals by copying the real block state into
      the thread's block state*/
   VG_(sigprocmask)(VKI_SIG_BLOCK, NULL, &VG_(threads)[tid].sig_mask);
   VG_(threads)[tid].tmp_sig_mask = VG_(threads)[tid].sig_mask;

   /* and restore handlers to default */
   VG_(set_default_handler)(VKI_SIGSEGV);
   VG_(set_default_handler)(VKI_SIGBUS);
   VG_(set_default_handler)(VKI_SIGILL);
   VG_(set_default_handler)(VKI_SIGFPE);

   // We were exiting, so assert that...
   vg_assert(VG_(is_exiting)(tid));
   // ...but now we're not again
   VG_(threads)[tid].exitreason = VgSrc_None;

   // run until client thread exits - ideally with LIBC_FREERES_DONE,
   // but exit/exitgroup/signal will do
   VG_(scheduler)(tid);

   vg_assert(VG_(is_exiting)(tid));
}

// Arch-specific client requests
Bool VGA_(client_request)(ThreadId tid, UWord *args)
{
   Bool handled = True;

   vg_assert(VG_(is_running_thread)(tid));

   switch(args[0]) {
   case VG_USERREQ__LIBC_FREERES_DONE:
      /* This is equivalent to an exit() syscall, but we don't set the
	 exitcode (since it might already be set) */
      if (0 || VG_(clo_trace_syscalls) || VG_(clo_trace_sched))
	 VG_(message)(Vg_DebugMsg, 
		      "__libc_freeres() done; really quitting!");
      VG_(threads)[tid].exitreason = VgSrc_ExitSyscall;
      break;

   default:
      handled = False;
      break;
   }

   return handled;
}

/*--------------------------------------------------------------------*/
/*--- end                                                          ---*/
/*--------------------------------------------------------------------*/
