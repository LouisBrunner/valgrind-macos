
/*--------------------------------------------------------------------*/
/*--- Platform-specific syscalls stuff.     amd64-linux/syscalls.c ---*/
/*--------------------------------------------------------------------*/

/*
   This file is part of Valgrind, a dynamic binary instrumentation
   framework.

   Copyright (C) 2000-2005 Nicholas Nethercote
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
#include "ume.h"                /* for jmp_with_stack */


/* COPIED FROM /usr/include/asm-i386/prctl.h (amd64-linux) */
#define ARCH_SET_GS 0x1001
#define ARCH_SET_FS 0x1002
#define ARCH_GET_FS 0x1003
#define ARCH_GET_GS 0x1004


/* ---------------------------------------------------------------------
   Stacks, thread wrappers, clone
   Note.  Why is this stuff here?
   ------------------------------------------------------------------ */

/* These are addresses within VGA_(client_syscall).  See syscall.S for details. */
extern const Addr VGA_(blksys_setup);
extern const Addr VGA_(blksys_restart);
extern const Addr VGA_(blksys_complete);
extern const Addr VGA_(blksys_committed);
extern const Addr VGA_(blksys_finished);

// Back up to restart a system call.
void VGA_(restart_syscall)(ThreadArchState *arch)
{
   arch->vex.guest_RIP -= 2;             // sizeof(syscall)

   /* Make sure our caller is actually sane, and we're really backing
      back over a syscall.

      syscall == 0F 05 
   */
   {
      UChar *p = (UChar *)arch->vex.guest_RIP;
      
      if (p[0] != 0x0F || p[1] != 0x05)
         VG_(message)(Vg_DebugMsg,
                      "?! restarting over syscall at %p %02x %02x\n",
                      arch->vex.guest_RIP, p[0], p[1]); 

      vg_assert(p[0] == 0x0F && p[1] == 0x05);
   }
}

/* 
   Fix up the VCPU state when a syscall is interrupted by a signal.

   To do this, we determine the precise state of the syscall by
   looking at the (real) rip at the time the signal happened.  The
   syscall sequence looks like:

     1. unblock signals
     2. perform syscall
     3. save result to RAX
     4. re-block signals

   If a signal
   happens at      Then     Why?
   [1-2)           restart  nothing has happened (restart syscall)
   [2]             restart  syscall hasn't started, or kernel wants to restart
   [2-3)           save     syscall complete, but results not saved
   [3-4)           syscall complete, results saved

   Sometimes we never want to restart an interrupted syscall (because
   sigaction says not to), so we only restart if "restart" is True.

   This will also call VG_(post_syscall)() if the syscall has actually
   completed (either because it was interrupted, or because it
   actually finished).  It will not call VG_(post_syscall)() if the
   syscall is set up for restart, which means that the pre-wrapper may
   get called multiple times.
 */
/* NB: this is identical to the x86 version */
void VGA_(interrupted_syscall)(ThreadId tid, 
			       struct vki_ucontext *uc,
			       Bool restart)
{
   static const Bool debug = 0;

   ThreadState *tst = VG_(get_ThreadState)(tid);
   ThreadArchState *th_regs = &tst->arch;
   Word ip = VGP_UCONTEXT_INSTR_PTR(uc);

   if (debug)
      VG_(printf)("interrupted_syscall: ip=%p; restart=%d eax=%d\n", 
		  ip, restart, VGP_UCONTEXT_SYSCALL_NUM(uc));

   if (ip < VGA_(blksys_setup) || ip >= VGA_(blksys_finished)) {
      VG_(printf)("  not in syscall (%p - %p)\n", VGA_(blksys_setup), VGA_(blksys_finished));
      vg_assert(tst->syscallno == -1);
      return;
   }

   vg_assert(tst->syscallno != -1);

   if (ip >= VGA_(blksys_setup) && ip < VGA_(blksys_restart)) {
      /* syscall hasn't even started; go around again */
      if (debug)
	 VG_(printf)("  not started: restart\n");
      VGA_(restart_syscall)(th_regs);
   } else if (ip == VGA_(blksys_restart)) {
      /* We're either about to run the syscall, or it was interrupted
	 and the kernel restarted it.  Restart if asked, otherwise
	 EINTR it. */
      if (restart)
	 VGA_(restart_syscall)(th_regs);
      else {
	 th_regs->vex.VGP_SYSCALL_RET = -VKI_EINTR;
	 VG_(post_syscall)(tid);
      }
   } else if (ip >= VGA_(blksys_complete) && ip < VGA_(blksys_committed)) {
      /* Syscall complete, but result hasn't been written back yet.
	 The saved real CPU %rax has the result, which we need to move
	 to RAX. */
      if (debug)
	 VG_(printf)("  completed: ret=%d\n", VGP_UCONTEXT_SYSCALL_RET(uc));
      th_regs->vex.VGP_SYSCALL_RET = VGP_UCONTEXT_SYSCALL_RET(uc);
      VG_(post_syscall)(tid);
   } else if (ip >= VGA_(blksys_committed) && ip < VGA_(blksys_finished)) {
      /* Result committed, but the signal mask has not been restored;
	 we expect our caller (the signal handler) will have fixed
	 this up. */
      if (debug)
	 VG_(printf)("  all done\n");
      VG_(post_syscall)(tid);
   } else
      VG_(core_panic)("?? strange syscall interrupt state?");
   
   tst->syscallno = -1;
}

extern void VGA_(_client_syscall)(Int syscallno, 
                                  void* guest_state,
				  const vki_sigset_t *syscall_mask,
				  const vki_sigset_t *restore_mask,
				  Int nsigwords);

void VGA_(client_syscall)(Int syscallno, ThreadState *tst,
			  const vki_sigset_t *syscall_mask)
{
   vki_sigset_t saved;
   VGA_(_client_syscall)(syscallno, &tst->arch.vex, 
                         syscall_mask, &saved, _VKI_NSIG_WORDS * sizeof(UWord));
}


/* 
   Allocate a stack for this thread.

   They're allocated lazily, but never freed.
 */
#define FILL	0xdeadbeef

static ULong *allocstack(ThreadId tid)
{
   ThreadState *tst = VG_(get_ThreadState)(tid);
   ULong* rsp;
   UInt*  pUInt;

   if (tst->os_state.stack == NULL) {
      void *stk = VG_(mmap)(0, VGA_STACK_SIZE_W * sizeof(Int) + VKI_PAGE_SIZE,
			    VKI_PROT_READ|VKI_PROT_WRITE,
			    VKI_MAP_PRIVATE|VKI_MAP_ANONYMOUS,
			    SF_VALGRIND,
			    -1, 0);

      if (stk != (void *)-1) {
	 VG_(mprotect)(stk, VKI_PAGE_SIZE, VKI_PROT_NONE); /* guard page */
	 tst->os_state.stack = (UInt *)stk + VKI_PAGE_SIZE/sizeof(UInt);
	 tst->os_state.stacksize = VGA_STACK_SIZE_W;
      } else 
	 return (ULong *)-1;
   }

   for (pUInt = tst->os_state.stack; 
        pUInt < (tst->os_state.stack + tst->os_state.stacksize); 
        pUInt++)
      *pUInt = FILL;
   /* rsp is left at top of stack */
   rsp = (ULong*)pUInt;

   if (0)
      VG_(printf)("stack for tid %d at %p (%x); rsp=%p\n",
		  tid, tst->os_state.stack, *tst->os_state.stack,
		  rsp);

   return rsp;
}

/* NB: this is identical the the x86 version. */
/* Return how many bytes of this stack have not been used */
Int VGA_(stack_unused)(ThreadId tid)
{
   ThreadState *tst = VG_(get_ThreadState)(tid);
   UInt *p;

   for (p = tst->os_state.stack; 
	p && (p < (tst->os_state.stack + tst->os_state.stacksize)); 
	p++)
      if (*p != FILL)
	 break;

   if (0)
      VG_(printf)("p=%p %x tst->os_state.stack=%p\n", p, *p, tst->os_state.stack);

   return (p - tst->os_state.stack) * sizeof(*p);
}


/*
   Allocate a stack for the main thread, and call VGA_(thread_wrapper)
   on that stack.
 */
void VGA_(main_thread_wrapper)(ThreadId tid)
{
   ULong *rsp = allocstack(tid);
   vg_assert(tid == VG_(master_tid));

   call_on_new_stack_0_1( 
      (Addr)rsp,             /* stack */
      0,                     /*bogus return address*/
      VGA_(thread_wrapper),  /* fn to call */
      (Word)tid              /* arg to give it */
   );

   /*NOTREACHED*/
   vg_assert(0);
}

static Int start_thread(void *arg)
{
   ThreadState *tst = (ThreadState *)arg;
   ThreadId tid = tst->tid;

   VGA_(thread_wrapper)(tid);

   /* OK, thread is dead; this releases the run lock */
   VG_(exit_thread)(tid);

   vg_assert(tst->status == VgTs_Zombie);

   /* Poke the reaper */
   if (VG_(clo_trace_signals))
      VG_(message)(Vg_DebugMsg, "Sending SIGVGCHLD to master tid=%d lwp=%d", 
		   VG_(master_tid), VG_(threads)[VG_(master_tid)].os_state.lwpid);

   VG_(tkill)(VG_(threads)[VG_(master_tid)].os_state.lwpid, VKI_SIGVGCHLD);

   /* We have to use this sequence to terminate the thread to prevent
      a subtle race.  If VG_(exit_thread)() had left the ThreadState
      as Empty, then it could have been reallocated, reusing the stack
      while we're doing these last cleanups.  Instead,
      VG_(exit_thread) leaves it as Zombie to prevent reallocation.
      We need to make sure we don't touch the stack between marking it
      Empty and exiting.  Hence the assembler. */
   asm volatile (
      "movl	%1, %0\n"	/* set tst->status = VgTs_Empty */
      "movq	%2, %%rax\n"    /* set %rax = __NR_exit */
      "movq	%3, %%rdi\n"    /* set %rdi = tst->os_state.exitcode */
      "syscall\n"		/* exit(tst->os_state.exitcode) */
      : "=m" (tst->status)
      : "n" (VgTs_Empty), "n" (__NR_exit), "m" (tst->os_state.exitcode));

   VG_(core_panic)("Thread exit failed?\n");
}

/* 
   clone() handling

   When a client clones, we need to keep track of the new thread.  This means:
   1. allocate a ThreadId+ThreadState+stack for the the thread

   2. initialize the thread's new VCPU state

   3. create the thread using the same args as the client requested,
   but using the scheduler entrypoint for EIP, and a separate stack
   for ESP.
 */
static Int do_clone(ThreadId ptid, 
		    UInt flags, Addr rsp, 
		    Int *parent_tidptr, 
		    Int *child_tidptr, 
		    Addr tlsaddr)
{
   static const Bool debug = False;

   ThreadId ctid = VG_(alloc_ThreadState)();
   ThreadState *ptst = VG_(get_ThreadState)(ptid);
   ThreadState *ctst = VG_(get_ThreadState)(ctid);
   ULong *stack;
   Segment *seg;
   Int ret;
   vki_sigset_t blockall, savedmask;

   VG_(sigfillset)(&blockall);

   vg_assert(VG_(is_running_thread)(ptid));
   vg_assert(VG_(is_valid_tid)(ctid));

   stack = allocstack(ctid);

   /* Copy register state

      Both parent and child return to the same place, and the code
      following the clone syscall works out which is which, so we
      don't need to worry about it.

      The parent gets the child's new tid returned from clone, but the
      child gets 0.

      If the clone call specifies a NULL rsp for the new thread, then
      it actually gets a copy of the parent's rsp.
   */
   VGA_(setup_child)( &ctst->arch, &ptst->arch );

   VGP_SET_SYSCALL_RESULT(ctst->arch, 0);
   if (rsp != 0)
      ctst->arch.vex.guest_RSP = rsp;

   ctst->os_state.parent = ptid;
   ctst->os_state.clone_flags = flags;
   ctst->os_state.parent_tidptr = parent_tidptr;
   ctst->os_state.child_tidptr = child_tidptr;

   /* inherit signal mask */
   ctst->sig_mask = ptst->sig_mask;
   ctst->tmp_sig_mask = ptst->sig_mask;

   /* We don't really know where the client stack is, because its
      allocated by the client.  The best we can do is look at the
      memory mappings and try to derive some useful information.  We
      assume that esp starts near its highest possible value, and can
      only go down to the start of the mmaped segment. */
   seg = VG_(find_segment)((Addr)rsp);
   if (seg) {
      ctst->stack_base = seg->addr;
      ctst->stack_highest_word = (Addr)PGROUNDUP(rsp);
      ctst->stack_size = ctst->stack_highest_word - ctst->stack_base;

      if (debug)
	 VG_(printf)("tid %d: guessed client stack range %p-%p\n",
		     ctid, seg->addr, PGROUNDUP(rsp));
   } else {
      VG_(message)(Vg_UserMsg, "!? New thread %d starts with RSP(%p) unmapped\n",
		   ctid, rsp);
      ctst->stack_base = 0;
      ctst->stack_size = 0;
   }

   if (flags & VKI_CLONE_SETTLS) {
      if (debug)
	 VG_(printf)("clone child has SETTLS: tls at %p\n", tlsaddr);
      ctst->arch.vex.guest_FS_ZERO = tlsaddr;
   }

   flags &= ~VKI_CLONE_SETTLS;

   /* start the thread with everything blocked */
   VG_(sigprocmask)(VKI_SIG_SETMASK, &blockall, &savedmask);

   /* Create the new thread */
   ret = VG_(clone)(start_thread, stack, flags, &VG_(threads)[ctid],
		    child_tidptr, parent_tidptr, NULL);

   VG_(sigprocmask)(VKI_SIG_SETMASK, &savedmask, NULL);

   if (ret < 0) {
      /* clone failed */
      VGA_(cleanup_thread)(&ctst->arch);
      ctst->status = VgTs_Empty;
   }

   return ret;
}

/* Do a clone which is really a fork() */
static Int do_fork_clone(ThreadId tid, UInt flags, Addr rsp, Int *parent_tidptr, Int *child_tidptr)
{
   vki_sigset_t fork_saved_mask;
   vki_sigset_t mask;
   Int ret;

   if (flags & (VKI_CLONE_SETTLS | VKI_CLONE_FS | VKI_CLONE_VM | VKI_CLONE_FILES | VKI_CLONE_VFORK))
      return -VKI_EINVAL;

   /* Block all signals during fork, so that we can fix things up in
      the child without being interrupted. */
   VG_(sigfillset)(&mask);
   VG_(sigprocmask)(VKI_SIG_SETMASK, &mask, &fork_saved_mask);

   VG_(do_atfork_pre)(tid);

   /* Since this is the fork() form of clone, we don't need all that
      VG_(clone) stuff */
   ret = VG_(do_syscall5)(__NR_clone, flags, (UWord)NULL, (UWord)parent_tidptr, 
                                             (UWord)NULL, (UWord)child_tidptr);

   if (ret == 0) {
      /* child */
      VG_(do_atfork_child)(tid);

      /* restore signal mask */
      VG_(sigprocmask)(VKI_SIG_SETMASK, &fork_saved_mask, NULL);
   } else if (ret > 0) {
      /* parent */
      if (VG_(clo_trace_syscalls))
	  VG_(printf)("   clone(fork): process %d created child %d\n", VG_(getpid)(), ret);

      VG_(do_atfork_parent)(tid);

      /* restore signal mask */
      VG_(sigprocmask)(VKI_SIG_SETMASK, &fork_saved_mask, NULL);
   }

   return ret;
}

/* ---------------------------------------------------------------------
   PRE/POST wrappers for AMD64/Linux-specific syscalls
   ------------------------------------------------------------------ */

// Nb: See the comment above the generic PRE/POST wrappers in
// coregrind/vg_syscalls.c for notes about how they work.

#define PRE(name, f)     PRE_TEMPLATE(static, amd64_linux, name, f)
#define POST(name)      POST_TEMPLATE(static, amd64_linux, name)

PRE(sys_clone, Special)
{
   UInt cloneflags;

   PRINT("sys_clone ( %x, %p, %p, %p, %p )",ARG1,ARG2,ARG3,ARG4,ARG5);
   PRE_REG_READ5(int, "clone",
                 unsigned long, flags,
                 void *, child_stack,
                 int *, parent_tidptr,
                 int *, child_tidptr,
                 void *, tlsaddr);

   if (ARG1 & VKI_CLONE_PARENT_SETTID) {
      PRE_MEM_WRITE("clone(parent_tidptr)", ARG3, sizeof(Int));
      if (!VG_(is_addressable)(ARG3, sizeof(Int), VKI_PROT_WRITE)) {
         SET_RESULT( -VKI_EFAULT );
         return;
      }
   }
   if (ARG1 & (VKI_CLONE_CHILD_SETTID | VKI_CLONE_CHILD_CLEARTID)) {
      PRE_MEM_WRITE("clone(child_tidptr)", ARG4, sizeof(Int));
      if (!VG_(is_addressable)(ARG4, sizeof(Int), VKI_PROT_WRITE)) {
         SET_RESULT( -VKI_EFAULT );
         return;
      }
   }

   cloneflags = ARG1;

   if (!VG_(client_signal_OK)(ARG1 & VKI_CSIGNAL)) {
      SET_RESULT( -VKI_EINVAL );
      return;
   }

   /* Only look at the flags we really care about */
   switch(cloneflags & (VKI_CLONE_VM | VKI_CLONE_FS | VKI_CLONE_FILES | VKI_CLONE_VFORK)) {
   case VKI_CLONE_VM | VKI_CLONE_FS | VKI_CLONE_FILES:
      /* thread creation */
      SET_RESULT(do_clone(tid,
                          ARG1,         /* flags */
                          (Addr)ARG2,   /* child ESP */
                          (Int *)ARG3,  /* parent_tidptr */
                          (Int *)ARG4,  /* child_tidptr */
                          (Addr)ARG5)); /* set_tls */
      break;

   case VKI_CLONE_VFORK | VKI_CLONE_VM: /* vfork */
      /* FALLTHROUGH - assume vfork == fork */
      cloneflags &= ~(VKI_CLONE_VFORK | VKI_CLONE_VM);

   case 0: /* plain fork */
      SET_RESULT(do_fork_clone(tid,
                               cloneflags,              /* flags */
                               (Addr)ARG2,      /* child ESP */
                               (Int *)ARG3,     /* parent_tidptr */
                               (Int *)ARG4));   /* child_tidptr */
      break;

   default:
      /* should we just ENOSYS? */
      VG_(message)(Vg_UserMsg, "Unsupported clone() flags: %x", ARG1);
      VG_(unimplemented)
         ("Valgrind does not support general clone().  The only supported uses "
          "are via a threads library, fork, or vfork.");
   }

   if (!VG_(is_kerror)(RES)) {
      if (ARG1 & VKI_CLONE_PARENT_SETTID)
         POST_MEM_WRITE(ARG3, sizeof(Int));
      if (ARG1 & (VKI_CLONE_CHILD_SETTID | VKI_CLONE_CHILD_CLEARTID))
         POST_MEM_WRITE(ARG4, sizeof(Int));

      /* Thread creation was successful; let the child have the chance
         to run */
      VG_(vg_yield)();
   }
}

PRE(sys_rt_sigreturn, Special)
{
   PRINT("rt_sigreturn ( )");

   /* Adjust esp to point to start of frame; skip back up over handler
      ret addr */
   tst->arch.vex.guest_RSP -= sizeof(Addr);

   /* This is only so that the RIP is (might be) useful to report if
      something goes wrong in the sigreturn */
   VGA_(restart_syscall)(&tst->arch);

   VGA_(signal_return)(tid, True);

   /* Keep looking for signals until there are none */
   VG_(poll_signals)(tid);

   /* placate return-must-be-set assertion */
   SET_RESULT(RES);
}

PRE(sys_arch_prctl, 0)
{
   PRINT( "arch_prctl ( %d, %llx )", ARG1, ARG2 );

   // Nb: can't use "ARG2".."ARG5" here because that's our own macro...
   PRE_REG_READ2(long, "arch_prctl",
                 int, option, unsigned long, arg2);
   // XXX: totally wrong... we need to look at the 'option' arg, and do
   // PRE_MEM_READs/PRE_MEM_WRITEs as necessary...

   /* "do" the syscall ourselves; the kernel never sees it */
   vg_assert(ARG1 == ARCH_SET_FS);
   tst->arch.vex.guest_FS_ZERO = ARG2;
   SET_RESULT( 0 );
}

PRE(sys_socket, 0)
{
   PRINT("sys_socket ( %d, %d, %d )",ARG1,ARG2,ARG3);
   PRE_REG_READ3(long, "socket", int, domain, int, type, int, protocol);
}

POST(sys_socket)
{
   UWord r = VG_(generic_POST_sys_socket)(tid, RES);
   SET_RESULT(r);
}

PRE(sys_setsockopt, 0)
{
   PRINT("sys_setsockopt ( %d, %d, %d, %p, %d )",ARG1,ARG2,ARG3,ARG4,ARG5);
   PRE_REG_READ5(long, "setsockopt",
                 int, s, int, level, int, optname,
                 const void *, optval, int, optlen);
   VG_(generic_PRE_sys_setsockopt)(tid, ARG1,ARG2,ARG3,ARG4,ARG5);
}

PRE(sys_getsockopt, 0)
{
   PRINT("sys_getsockopt ( %d, %d, %d, %p, %p )",ARG1,ARG2,ARG3,ARG4,ARG5);
   PRE_REG_READ5(long, "getsockopt",
                 int, s, int, level, int, optname,
                 void *, optval, int, *optlen);
   VG_(generic_PRE_sys_getsockopt)(tid, ARG1,ARG2,ARG3,ARG4,ARG5);
}

POST(sys_getsockopt)
{
   VG_(generic_POST_sys_getsockopt)(tid, RES,ARG1,ARG2,ARG3,ARG4,ARG5);
}

PRE(sys_connect, MayBlock)
{
   PRINT("sys_connect ( %d, %p, %d )",ARG1,ARG2,ARG3);
   PRE_REG_READ3(long, "connect",
                 int, sockfd, struct sockaddr *, serv_addr, int, addrlen);
   VG_(generic_PRE_sys_connect)(tid, ARG1,ARG2,ARG3);
}

PRE(sys_accept, MayBlock)
{
   PRINT("sys_accept ( %d, %p, %d )",ARG1,ARG2,ARG3);
   PRE_REG_READ3(long, "accept",
                 int, s, struct sockaddr *, addr, int, *addrlen);
   VG_(generic_PRE_sys_accept)(tid, ARG1,ARG2,ARG3);
}

POST(sys_accept)
{
   UWord r = VG_(generic_POST_sys_accept)(tid, RES,ARG1,ARG2,ARG3);
   SET_RESULT(r);
}

PRE(sys_sendto, MayBlock)
{
   PRINT("sys_sendto ( %d, %s, %d, %u, %p, %d )",ARG1,ARG2,ARG3,ARG4,ARG5,ARG6);
   PRE_REG_READ6(long, "sendto",
                 int, s, const void *, msg, int, len, 
                 unsigned int, flags, 
                 const struct sockaddr *, to, int, tolen);
   VG_(generic_PRE_sys_sendto)(tid, ARG1,ARG2,ARG3,ARG4,ARG5,ARG6);
}

PRE(sys_recvfrom, MayBlock)
{
   PRINT("sys_recvfrom ( %d, %p, %d, %u, %p, %p )",ARG1,ARG2,ARG3,ARG4,ARG5,ARG6);
   PRE_REG_READ6(long, "recvfrom",
                 int, s, void *, buf, int, len, unsigned int, flags,
                 struct sockaddr *, from, int *, fromlen);
   VG_(generic_PRE_sys_recvfrom)(tid, ARG1,ARG2,ARG3,ARG4,ARG5,ARG6);
}
POST(sys_recvfrom)
{
   VG_(generic_POST_sys_recvfrom)(tid, RES,ARG1,ARG2,ARG3,ARG4,ARG5,ARG6);
}

PRE(sys_sendmsg, MayBlock)
{
   PRINT("sys_sendmsg ( %d, %p, %d )",ARG1,ARG2,ARG3);
   PRE_REG_READ3(long, "sendmsg",
                 int, s, const struct msghdr *, msg, int, flags);
   VG_(generic_PRE_sys_sendmsg)(tid, ARG1,ARG2);
}

PRE(sys_recvmsg, MayBlock)
{
   PRINT("sys_recvmsg ( %d, %p, %d )",ARG1,ARG2,ARG3);
   PRE_REG_READ3(long, "recvmsg", int, s, struct msghdr *, msg, int, flags);
   VG_(generic_PRE_sys_recvmsg)(tid, ARG1,ARG2);
}
POST(sys_recvmsg)
{
   VG_(generic_POST_sys_recvmsg)(tid, RES,ARG1,ARG2);
}

PRE(sys_shutdown, MayBlock)
{
   PRINT("sys_shutdown ( %d, %d )",ARG1,ARG2);
   PRE_REG_READ2(int, "shutdown", int, s, int, how);
}

PRE(sys_bind, 0)
{
   PRINT("sys_bind ( %d, %p, %d )",ARG1,ARG2,ARG3);
   PRE_REG_READ3(long, "bind",
                 int, sockfd, struct sockaddr *, my_addr, int, addrlen);
   VG_(generic_PRE_sys_bind)(tid, ARG1,ARG2,ARG3);
}

PRE(sys_listen, 0)
{
   PRINT("sys_listen ( %d, %d )",ARG1,ARG2);
   PRE_REG_READ2(long, "listen", int, s, int, backlog);
}

PRE(sys_getsockname, 0)
{
   PRINT("sys_getsockname ( %d, %p, %p )",ARG1,ARG2,ARG3);
   PRE_REG_READ3(long, "getsockname",
                 int, s, struct sockaddr *, name, int *, namelen);
   VG_(generic_PRE_sys_getsockname)(tid, ARG1,ARG2,ARG3);
}
POST(sys_getsockname)
{
   VG_(generic_POST_sys_getsockname)(tid, RES,ARG1,ARG2,ARG3);
}

PRE(sys_getpeername, 0)
{
   PRINT("sys_getpeername ( %d, %p, %p )",ARG1,ARG2,ARG3);
   PRE_REG_READ3(long, "getpeername",
                 int, s, struct sockaddr *, name, int *, namelen);
   VG_(generic_PRE_sys_getpeername)(tid, ARG1,ARG2,ARG3);
}
POST(sys_getpeername)
{
   VG_(generic_POST_sys_getpeername)(tid, RES,ARG1,ARG2,ARG3);
}

PRE(sys_socketpair, 0)
{
   PRINT("sys_socketpair ( %d, %d, %d, %p )",ARG1,ARG2,ARG3,ARG4);
   PRE_REG_READ4(long, "socketpair",
                 int, d, int, type, int, protocol, int [2], sv);
   VG_(generic_PRE_sys_socketpair)(tid, ARG1,ARG2,ARG3,ARG4);
}
POST(sys_socketpair)
{
   VG_(generic_POST_sys_socketpair)(tid, RES,ARG1,ARG2,ARG3,ARG4);
}

PRE(sys_semget, 0)
{
   PRINT("sys_semget ( %d, %d, %d )",ARG1,ARG2,ARG3);
   PRE_REG_READ3(long, "semget", key_t, key, int, nsems, int, semflg);
}

PRE(sys_semop, MayBlock)
{
   PRINT("sys_semop ( %d, %p, %u )",ARG1,ARG2,ARG3);
   PRE_REG_READ3(long, "semop",
                 int, semid, struct sembuf *, sops, unsigned, nsoops);
   VG_(generic_PRE_sys_semop)(tid, ARG1,ARG2,ARG3);
}

PRE(sys_semtimedop, MayBlock)
{
   PRINT("sys_semtimedop ( %d, %p, %u, %p )",ARG1,ARG2,ARG3,ARG4);
   PRE_REG_READ4(long, "semtimedop",
                 int, semid, struct sembuf *, sops, unsigned, nsoops,
                 struct timespec *, timeout);
   VG_(generic_PRE_sys_semtimedop)(tid, ARG1,ARG2,ARG3,ARG4);
}

PRE(sys_semctl, 0)
{
   switch (ARG3 & ~VKI_IPC_64) {
   case VKI_IPC_INFO:
   case VKI_SEM_INFO:
      PRINT("sys_semctl ( %d, %d, %d, %p )",ARG1,ARG2,ARG3,ARG4);
      PRE_REG_READ4(long, "semctl",
                    int, semid, int, semnum, int, cmd, struct seminfo *, arg);
      break;
   case VKI_IPC_STAT:
   case VKI_SEM_STAT:
   case VKI_IPC_SET:
      PRINT("sys_semctl ( %d, %d, %d, %p )",ARG1,ARG2,ARG3,ARG4);
      PRE_REG_READ4(long, "semctl",
                    int, semid, int, semnum, int, cmd, struct semid_ds *, arg);
      break;
   case VKI_GETALL:
   case VKI_SETALL:
      PRINT("sys_semctl ( %d, %d, %d, %p )",ARG1,ARG2,ARG3,ARG4);
      PRE_REG_READ4(long, "semctl",
                    int, semid, int, semnum, int, cmd, unsigned short *, arg);
      break;
   default:
      PRINT("sys_semctl ( %d, %d, %d )",ARG1,ARG2,ARG3);
      PRE_REG_READ3(long, "semctl",
                    int, semid, int, semnum, int, cmd);
      break;
   }
   VG_(generic_PRE_sys_semctl)(tid, ARG1,ARG2,ARG3,ARG4);
}

POST(sys_semctl)
{
   VG_(generic_POST_sys_semctl)(tid, RES,ARG1,ARG2,ARG3,ARG4);
}

PRE(sys_msgget, 0)
{
   PRINT("sys_msgget ( %d, %d )",ARG1,ARG2);
   PRE_REG_READ2(long, "msgget", key_t, key, int, msgflg);
}

PRE(sys_msgsnd, 0)
{
   PRINT("sys_msgsnd ( %d, %p, %d, %d )",ARG1,ARG2,ARG3,ARG4);
   PRE_REG_READ4(long, "msgsnd",
                 int, msqid, struct msgbuf *, msgp, size_t, msgsz, int, msgflg);
   VG_(generic_PRE_sys_msgsnd)(tid, ARG1,ARG2,ARG3,ARG4);
      /* if ((ARG4 & VKI_IPC_NOWAIT) == 0)
            tst->sys_flags |= MayBlock;
      */
}

PRE(sys_msgrcv, 0)
{
   PRINT("sys_msgrcv ( %d, %p, %d, %d, %d )",ARG1,ARG2,ARG3,ARG4,ARG5);
   PRE_REG_READ5(long, "msgrcv",
                 int, msqid, struct msgbuf *, msgp, size_t, msgsz,
                 long, msgytp, int, msgflg);
   VG_(generic_PRE_sys_msgrcv)(tid, ARG1,ARG2,ARG3,ARG4,ARG5);
      /* if ((ARG4 & VKI_IPC_NOWAIT) == 0)
            tst->sys_flags |= MayBlock;
      */
}

POST(sys_msgrcv)
{
   VG_(generic_POST_sys_msgrcv)(tid, RES,ARG1,ARG2,ARG3,ARG4,ARG5);
}

PRE(sys_msgctl, 0)
{
   PRINT("sys_msgctl ( %d, %d, %p )",ARG1,ARG2,ARG3);
   PRE_REG_READ3(long, "msgctl",
                 int, msqid, int, cmd, struct msqid_ds *, buf);
   VG_(generic_PRE_sys_msgctl)(tid, ARG1,ARG2,ARG3);
}

POST(sys_msgctl)
{
   VG_(generic_POST_sys_msgctl)(tid, RES,ARG1,ARG2,ARG3);
}

PRE(sys_shmget, 0)
{
   PRINT("sys_shmget ( %d, %d, %d )",ARG1,ARG2,ARG3);
   PRE_REG_READ3(long, "shmget", key_t, key, size_t, size, int, shmflg);
}

PRE(wrap_sys_shmat, 0)
{
   PRINT("wrap_sys_shmat ( %d, %p, %d )",ARG1,ARG2,ARG3);
   PRE_REG_READ3(long, "shmat",
                 int, shmid, const void *, shmaddr, int, shmflg);
   ARG2 = VG_(generic_PRE_sys_shmat)(tid, ARG1,ARG2,ARG3);
   if (ARG2 == 0)
      SET_RESULT( -VKI_EINVAL );
}

POST(wrap_sys_shmat)
{
   VG_(generic_POST_sys_shmat)(tid, RES,ARG1,ARG2,ARG3);
}

PRE(sys_shmdt, 0)
{
   PRINT("sys_shmdt ( %p )",ARG1);
   PRE_REG_READ1(long, "shmdt", const void *, shmaddr);
   if (!VG_(generic_PRE_sys_shmdt)(tid, ARG1))
      SET_RESULT( -VKI_EINVAL );
}

POST(sys_shmdt)
{
   VG_(generic_POST_sys_shmdt)(tid, RES,ARG1);
}

PRE(sys_shmctl, 0)
{
   PRINT("sys_shmctl ( %d, %d, %p )",ARG1,ARG2,ARG3);
   PRE_REG_READ3(long, "shmctl",
                 int, shmid, int, cmd, struct shmid_ds *, buf);
   VG_(generic_PRE_sys_shmctl)(tid, ARG1,ARG2,ARG3);
}

POST(sys_shmctl)
{
   VG_(generic_POST_sys_shmctl)(tid, RES,ARG1,ARG2,ARG3);
}

#undef PRE
#undef POST


/* ---------------------------------------------------------------------
   The AMD64/Linux syscall table
   ------------------------------------------------------------------ */

// Macros for adding AMD64/Linux-specific wrappers to the syscall table.
#define PLAX_(const, name)    SYS_WRAPPER_ENTRY_X_(amd64_linux, const, name) 
#define PLAXY(const, name)    SYS_WRAPPER_ENTRY_XY(amd64_linux, const, name) 

// This table maps from __NR_xxx syscall numbers (from
// linux/include/asm-x86_64/unistd.h) to the appropriate PRE/POST sys_foo()
// wrappers on AMD64 (as per sys_call_table in
// linux/arch/x86_64/kernel/entry.S).
//
// When implementing these wrappers, you need to work out if the wrapper is
// generic, Linux-only (but arch-independent), or AMD64/Linux only.

const struct SyscallTableEntry VGA_(syscall_table)[] = {
   GENXY(__NR_read,              sys_read),           // 0 
   GENX_(__NR_write,             sys_write),          // 1 
   GENXY(__NR_open,              sys_open),           // 2 
   GENXY(__NR_close,             sys_close),          // 3 
   GENXY(__NR_stat,              sys_newstat),        // 4 

   GENXY(__NR_fstat,             sys_newfstat),       // 5 
   GENXY(__NR_lstat,             sys_newlstat),       // 6 
   GENXY(__NR_poll,              sys_poll),           // 7 
   GENX_(__NR_lseek,             sys_lseek),          // 8 
   GENXY(__NR_mmap,              sys_mmap2),          // 9 

   GENXY(__NR_mprotect,          sys_mprotect),       // 10 
   GENXY(__NR_munmap,            sys_munmap),         // 11 
   GENX_(__NR_brk,               sys_brk),            // 12 
   GENXY(__NR_rt_sigaction,      sys_rt_sigaction),   // 13 
   GENXY(__NR_rt_sigprocmask,    sys_rt_sigprocmask), // 14 

   PLAX_(__NR_rt_sigreturn,      sys_rt_sigreturn),   // 15 
   GENXY(__NR_ioctl,             sys_ioctl),          // 16 
   GENXY(__NR_pread64,           sys_pread64),        // 17 
   //   (__NR_pwrite64,          sys_pwrite64),       // 18 
   GENXY(__NR_readv,             sys_readv),          // 19 

   GENX_(__NR_writev,            sys_writev),         // 20 
   GENX_(__NR_access,            sys_access),         // 21 
   GENXY(__NR_pipe,              sys_pipe),           // 22 
   GENX_(__NR_select,            sys_select),         // 23 
   //   (__NR_sched_yield,       sys_sched_yield),    // 24 

   GENX_(__NR_mremap,            sys_mremap),         // 25 
   //   (__NR_msync,             sys_msync),          // 26 
   //   (__NR_mincore,           sys_mincore),        // 27 
   GENX_(__NR_madvise,           sys_madvise),        // 28 
   PLAX_(__NR_shmget,            sys_shmget),         // 29 

   PLAXY(__NR_shmat,             wrap_sys_shmat),     // 30 
   PLAXY(__NR_shmctl,            sys_shmctl),         // 31 
   GENXY(__NR_dup,               sys_dup),            // 32 
   GENXY(__NR_dup2,              sys_dup2),           // 33 
   GENX_(__NR_pause,             sys_pause),          // 34 

   GENXY(__NR_nanosleep,         sys_nanosleep),      // 35 
   //   (__NR_getitimer,         sys_getitimer),      // 36 
   GENX_(__NR_alarm,             sys_alarm),          // 37 
   //   (__NR_setitimer,         sys_setitimer),      // 38 
   GENX_(__NR_getpid,            sys_getpid),         // 39 

   //   (__NR_sendfile,          sys_sendfile64),     // 40 
   PLAXY(__NR_socket,            sys_socket),         // 41 
   PLAX_(__NR_connect,           sys_connect),        // 42
   PLAXY(__NR_accept,            sys_accept),         // 43 
   PLAX_(__NR_sendto,            sys_sendto),         // 44 

   PLAXY(__NR_recvfrom,          sys_recvfrom),       // 45 
   PLAX_(__NR_sendmsg,           sys_sendmsg),        // 46 
   PLAXY(__NR_recvmsg,           sys_recvmsg),        // 47
   PLAX_(__NR_shutdown,          sys_shutdown),       // 48 
   PLAX_(__NR_bind,              sys_bind),           // 49 

   PLAX_(__NR_listen,            sys_listen),         // 50 
   PLAXY(__NR_getsockname,       sys_getsockname),    // 51 
   PLAXY(__NR_getpeername,       sys_getpeername),    // 52 
   PLAXY(__NR_socketpair,        sys_socketpair),     // 53 
   PLAX_(__NR_setsockopt,        sys_setsockopt),     // 54

   PLAXY(__NR_getsockopt,        sys_getsockopt),     // 55 
   PLAX_(__NR_clone,             sys_clone),          // 56 
   GENX_(__NR_fork,              sys_fork),           // 57 
   GENX_(__NR_vfork,             sys_fork),           // 58 treat as fork
   GENX_(__NR_execve,            sys_execve),         // 59 

   GENX_(__NR_exit,              sys_exit),           // 60
   GENXY(__NR_wait4,             sys_wait4),          // 61 
   GENX_(__NR_kill,              sys_kill),           // 62 
   GENXY(__NR_uname,             sys_newuname),       // 63 
   PLAX_(__NR_semget,            sys_semget),         // 64 

   PLAX_(__NR_semop,             sys_semop),          // 65 
   PLAXY(__NR_semctl,            sys_semctl),         // 66 
   PLAXY(__NR_shmdt,             sys_shmdt),          // 67 
   PLAX_(__NR_msgget,            sys_msgget),         // 68 
   PLAX_(__NR_msgsnd,            sys_msgsnd),         // 69 

   PLAXY(__NR_msgrcv,            sys_msgrcv),         // 70 
   PLAXY(__NR_msgctl,            sys_msgctl),         // 71 
   GENXY(__NR_fcntl,             sys_fcntl),          // 72 
   //   (__NR_flock,             sys_flock),          // 73 
   //   (__NR_fsync,             sys_fsync),          // 74 

   //   (__NR_fdatasync,         sys_fdatasync),      // 75 
   //   (__NR_truncate,          sys_truncate),       // 76 
   GENX_(__NR_ftruncate,         sys_ftruncate),      // 77 
   GENXY(__NR_getdents,          sys_getdents),       // 78 
   GENXY(__NR_getcwd,            sys_getcwd),         // 79 

   GENX_(__NR_chdir,             sys_chdir),          // 80 
   GENX_(__NR_fchdir,            sys_fchdir),         // 81 
   GENX_(__NR_rename,            sys_rename),         // 82 
   GENX_(__NR_mkdir,             sys_mkdir),          // 83 
   GENX_(__NR_rmdir,             sys_rmdir),          // 84 

   GENXY(__NR_creat,             sys_creat),          // 85 
   GENX_(__NR_link,              sys_link),           // 86 
   GENX_(__NR_unlink,            sys_unlink),         // 87 
   GENX_(__NR_symlink,           sys_symlink),        // 88 
   GENX_(__NR_readlink,          sys_readlink),       // 89 

   GENX_(__NR_chmod,             sys_chmod),          // 90 
   //   (__NR_fchmod,            sys_fchmod),         // 91 
   GENX_(__NR_chown,             sys_chown),          // 92 
   //   (__NR_fchown,            sys_fchown),         // 93 
   //   (__NR_lchown,            sys_lchown),         // 94 

   GENX_(__NR_umask,             sys_umask),          // 95 
   GENXY(__NR_gettimeofday,      sys_gettimeofday),   // 96 
   GENXY(__NR_getrlimit,         sys_getrlimit),      // 97 
   GENXY(__NR_getrusage,         sys_getrusage),      // 98 
   //   (__NR_sysinfo,           sys_sysinfo),        // 99 

   GENXY(__NR_times,             sys_times),          // 100 
   //   (__NR_ptrace,            sys_ptrace),         // 101 
   GENX_(__NR_getuid,            sys_getuid),         // 102 
   //   (__NR_syslog,            sys_syslog),         // 103 
   GENX_(__NR_getgid,            sys_getgid),         // 104 

   GENX_(__NR_setuid,            sys_setuid),         // 105 
   GENX_(__NR_setgid,            sys_setgid),         // 106 
   GENX_(__NR_geteuid,           sys_geteuid),        // 107 
   GENX_(__NR_getegid,           sys_getegid),        // 108 
   GENX_(__NR_setpgid,           sys_setpgid),        // 109 

   GENX_(__NR_getppid,           sys_getppid),        // 110 
   GENX_(__NR_getpgrp,           sys_getpgrp),        // 111 
   //   (__NR_setsid,            sys_setsid),         // 112 
   //   (__NR_setreuid,          sys_setreuid),       // 113 
   //   (__NR_setregid,          sys_setregid),       // 114 

   GENXY(__NR_getgroups,         sys_getgroups),      // 115 
   GENX_(__NR_setgroups,         sys_setgroups),      // 116 
   //   (__NR_setresuid,         sys_setresuid),      // 117 
   //   (__NR_getresuid,         sys_getresuid),      // 118 
   //   (__NR_setresgid,         sys_setresgid),      // 119 

   //   (__NR_getresgid,         sys_getresgid),      // 120 
   //   (__NR_getpgid,           sys_getpgid),        // 121 
   //   (__NR_setfsuid,          sys_setfsuid),       // 122 
   //   (__NR_setfsgid,          sys_setfsgid),       // 123 
   //   (__NR_getsid,            sys_getsid),         // 124 

   //   (__NR_capget,            sys_capget),         // 125 
   //   (__NR_capset,            sys_capset),         // 126 
   GENXY(__NR_rt_sigpending,     sys_rt_sigpending),  // 127 
   GENXY(__NR_rt_sigtimedwait,   sys_rt_sigtimedwait),// 128 
   GENXY(__NR_rt_sigqueueinfo,   sys_rt_sigqueueinfo),// 129 

   GENX_(__NR_rt_sigsuspend,     sys_rt_sigsuspend),  // 130 
   GENXY(__NR_sigaltstack,       sys_sigaltstack),    // 131 
   GENX_(__NR_utime,             sys_utime),          // 132 
   GENX_(__NR_mknod,             sys_mknod),          // 133 
   //   (__NR_uselib,            sys_uselib),         // 134 

   //   (__NR_personality,       sys_personality),    // 135 
   //   (__NR_ustat,             sys_ustat),          // 136 
   GENXY(__NR_statfs,            sys_statfs),         // 137 
   //   (__NR_fstatfs,           sys_fstatfs),        // 138 
   //   (__NR_sysfs,             sys_sysfs),          // 139 

   //   (__NR_getpriority,             sys_getpriority),             // 140 
   //   (__NR_setpriority,             sys_setpriority),             // 141 
   //   (__NR_sched_setparam,          sys_sched_setparam),          // 142 
   //   (__NR_sched_getparam,          sys_sched_getparam),          // 143 
   //   (__NR_sched_setscheduler,      sys_sched_setscheduler),      // 144 

   //   (__NR_sched_getscheduler,      sys_sched_getscheduler),      // 145 
   //   (__NR_sched_get_priority_max,  sys_sched_get_priority_max),  // 146 
   //   (__NR_sched_get_priority_min,  sys_sched_get_priority_min),  // 147 
   //   (__NR_sched_rr_get_interval,   sys_sched_rr_get_interval),   // 148 
   //   (__NR_mlock,                   sys_mlock),                   // 149 

   //   (__NR_munlock,           sys_munlock),        // 150 
   //   (__NR_mlockall,          sys_mlockall),       // 151 
   //   (__NR_munlockall,        sys_munlockall),     // 152 
   //   (__NR_vhangup,           sys_vhangup),        // 153 
   //   (__NR_modify_ldt,        sys_modify_ldt),     // 154 

   //   (__NR_pivot_root,        sys_pivot_root),     // 155 
   LINXY(__NR__sysctl,           sys_sysctl),         // 156 
   //   (__NR_prctl,             sys_prctl),          // 157 
   PLAX_(__NR_arch_prctl,	 sys_arch_prctl),     // 158 
   //   (__NR_adjtimex,          sys_adjtimex),       // 159 

   GENX_(__NR_setrlimit,         sys_setrlimit),      // 160 
   GENX_(__NR_chroot,            sys_chroot),         // 161 
   //   (__NR_sync,              sys_sync),           // 162 
   //   (__NR_acct,              sys_acct),           // 163 
   //   (__NR_settimeofday,      sys_settimeofday),   // 164 

   LINX_(__NR_mount,             sys_mount),          // 165
   //   (__NR_umount2,           sys_umount),         // 166 
   //   (__NR_swapon,            sys_swapon),         // 167 
   //   (__NR_swapoff,           sys_swapoff),        // 168 
   //   (__NR_reboot,            sys_reboot),         // 169 

   //   (__NR_sethostname,       sys_sethostname),    // 170 
   //   (__NR_setdomainname,     sys_setdomainname),  // 171 
   //   (__NR_iopl,              stub_iopl),          // 172 
   //   (__NR_ioperm,            sys_ioperm),         // 173 
   //   (__NR_create_module,     sys_ni_syscall),     // 174 

   //   (__NR_init_module,       sys_init_module),    // 175 
   //   (__NR_delete_module,     sys_delete_module),  // 176 
   //   (__NR_get_kernel_syms,   sys_ni_syscall),     // 177 
   //   (__NR_query_module,      sys_ni_syscall),     // 178 
   //   (__NR_quotactl,          sys_quotactl),       // 179 

   //   (__NR_nfsservctl,        sys_nfsservctl),     // 180 
   //   (__NR_getpmsg,           sys_ni_syscall),     // 181
   //   (__NR_putpmsg,           sys_ni_syscall),     // 182
   //   (__NR_afs_syscall,       sys_ni_syscall),     // 183 
   //   (__NR_tuxcall,           sys_ni_syscall),     // 184

   //   (__NR_security,          sys_ni_syscall),     // 185 
   LINX_(__NR_gettid,            sys_gettid),         // 186 
   //   (__NR_readahead,         sys_readahead),      // 187 
   //   (__NR_setxattr,          sys_setxattr),       // 188 
   //   (__NR_lsetxattr,         sys_lsetxattr),      // 189 

   //   (__NR_fsetxattr,         sys_fsetxattr),      // 190 
   GENXY(__NR_getxattr,          sys_getxattr),       // 191 
   //   (__NR_lgetxattr,         sys_lgetxattr),      // 192 
   //   (__NR_fgetxattr,         sys_fgetxattr),      // 193 
   //   (__NR_listxattr,         sys_listxattr),      // 194 

   //   (__NR_llistxattr,        sys_llistxattr),     // 195 
   //   (__NR_flistxattr,        sys_flistxattr),     // 196 
   //   (__NR_removexattr,       sys_removexattr),    // 197 
   //   (__NR_lremovexattr,      sys_lremovexattr),   // 198 
   //   (__NR_fremovexattr,      sys_fremovexattr),   // 199 

   //   (__NR_tkill,             sys_tkill),             // 200 
   GENXY(__NR_time,              sys_time), /*was sys_time64*/ // 201 
   LINXY(__NR_futex,             sys_futex),             // 202 
   //   (__NR_sched_setaffinity, sys_sched_setaffinity), // 203 
   //   (__NR_sched_getaffinity, sys_sched_getaffinity), // 204 

   //   (__NR_set_thread_area,   sys_ni_syscall),     // 205 
   //   (__NR_io_setup,          sys_io_setup),       // 206 
   //   (__NR_io_destroy,        sys_io_destroy),     // 207 
   //   (__NR_io_getevents,      sys_io_getevents),   // 208 
   //   (__NR_io_submit,         sys_io_submit),      // 209 

   //   (__NR_io_cancel,         sys_io_cancel),      // 210 
   //   (__NR_get_thread_area,   sys_ni_syscall),     // 211 
   //   (__NR_lookup_dcookie,    sys_lookup_dcookie), // 212 
   //   (__NR_epoll_create,      sys_epoll_create),   // 213 
   //   (__NR_epoll_ctl_old,     sys_ni_syscall),     // 214 

   //   (__NR_epoll_wait_old,    sys_ni_syscall),     // 215 
   //   (__NR_remap_file_pages,  sys_remap_file_pages)// 216 
   GENXY(__NR_getdents64,        sys_getdents64),     // 217 
   GENX_(__NR_set_tid_address,   sys_set_tid_address),// 218 
   //   (__NR_restart_syscall,   sys_restart_syscall),// 219 

   PLAX_(__NR_semtimedop,        sys_semtimedop),     // 220 
   //   (__NR_fadvise64,         sys_fadvise64),      // 221 
   //   (__NR_timer_create,      sys_timer_create),   // 222 
   //   (__NR_timer_settime,     sys_timer_settime),  // 223 
   //   (__NR_timer_gettime,     sys_timer_gettime),  // 224 

   //   (__NR_timer_getoverrun,  sys_timer_getoverrun)// 225 
   //   (__NR_timer_delete,      sys_timer_delete),   // 226 
   //   (__NR_clock_settime,     sys_clock_settime),  // 227 
   GENXY(__NR_clock_gettime,     sys_clock_gettime),  // 228 
   //   (__NR_clock_getres,      sys_clock_getres),   // 229 

   //   (__NR_clock_nanosleep,   sys_clock_nanosleep),// 230 
   LINX_(__NR_exit_group,        sys_exit_group),     // 231 
   //   (__NR_epoll_wait,        sys_epoll_wait),     // 232 
   //   (__NR_epoll_ctl,         sys_epoll_ctl),      // 233 
   LINXY(__NR_tgkill,            sys_tgkill),         // 234 

   //   (__NR_utimes,            sys_utimes),         // 235 
   //   (__NR_vserver,           sys_ni_syscall),     // 236 
   //   (__NR_vserver,           sys_ni_syscall),     // 236 
   //   (__NR_mbind,             sys_mbind),          // 237 
   //   (__NR_set_mempolicy,     sys_set_mempolicy),  // 238 

   //   (__NR_get_mempolicy,     sys_get_mempolicy),  // 239 
   GENXY(__NR_mq_open,           sys_mq_open),        // 240 
   GENX_(__NR_mq_unlink,         sys_mq_unlink),      // 241 
   GENX_(__NR_mq_timedsend,      sys_mq_timedsend),   // 242 
   GENX_(__NR_mq_timedreceive,   sys_mq_timedreceive),// 243 

   GENX_(__NR_mq_notify,         sys_mq_notify),      // 244 
   GENXY(__NR_mq_getsetattr,     sys_mq_getsetattr),  // 245 
   //   (__NR_kexec_load,        sys_ni_syscall),     // 246 
   //   (__NR_waitid,            sys_waitid),         // 247 
};

const UInt VGA_(syscall_table_size) = 
            sizeof(VGA_(syscall_table)) / sizeof(VGA_(syscall_table)[0]);

//void        VG_(clear_TLS_for_thread)      ( VgLdtEntry* tls )
//{
//}

/*--------------------------------------------------------------------*/
/*--- end                                   amd64-linux/syscalls.c ---*/
/*--------------------------------------------------------------------*/
