
/*--------------------------------------------------------------------*/
/*--- Proxy LWP machinery.                           vg_proxylwp.c ---*/
/*--------------------------------------------------------------------*/

/*
   This file is part of Valgrind, an extensible x86 protected-mode
   emulator for monitoring program execution on x86-Unixes.

   Copyright (C) 2000-2004 Julian Seward 
      jseward@acm.org

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


#include "vg_include.h"

/* We need our own copy of VG_(do_syscall)() to handle a special
   race-condition.  If we've got signals unblocked, and we take a
   signal in the gap either just before or after the syscall, we may
   end up not running the syscall at all, or running it more than
   once.

   The solution is to make the signal handler derive the proxy's
   precise state by looking to see which eip it is executing at
   exception time.

   Ranges:

   sys_before ... sys_restarted:
	Setting up register arguments and running state.  If
	interrupted, then the syscall should be considered to return
	ERESTARTSYS.

   sys_restarted:
	If interrupted and eip==sys_restarted, then either the syscall
	was about to start running, or it has run, was interrupted and
	the kernel wants to restart it.  eax still contains the
	syscall number.  If interrupted, then the syscall return value
	should be ERESTARTSYS.

   sys_after:
	If interrupted and eip==sys_after, the syscall either just
	finished, or it was interrupted and the kernel doesn't want to
	restart it.  Either way, eax equals the correct return value
	(either the actual return value, or EINTR).

   sys_after ... sys_done:
	System call is complete, but the state hasn't been updated,
	nor has the result been written back.  eax contains the return
	value.
*/

enum PXState
{
   PXS_BAD = -1,
   PXS_WaitReq,		/* waiting for a request */
   PXS_RunSyscall,	/* running a syscall */
   PXS_IntReply,	/* request interrupted - need to send reply */
   PXS_SysDone,		/* small window between syscall
			   complete and results written out */
   PXS_SigACK,		/* waiting for a signal ACK */
};

enum RequestType {
   PX_BAD = -1,
   PX_SetSigmask,		/* sched->proxy; proxy->sched */
   PX_RunSyscall,		/* sched->proxy; proxy->sched */
   PX_Signal,			/* proxy->sched */
   PX_SigACK,			/* sched->proxy */
   PX_Ping,			/* use for sanity-checking */
   PX_Exiting,			/* reply sent by proxy for exit sync */
};

extern void do_thread_syscall(Int sys, 
			      Int arg1, Int arg2, Int arg3, Int arg4, Int arg5, Int arg6,
			      Int *result, enum PXState *statep, enum PXState poststate);

asm(
".text\n"
"	.type do_thread_syscall,@function\n"

"do_thread_syscall:\n"
"	push	%esi\n"
"	push	%edi\n"
"	push	%ebx\n"
"	push	%ebp\n"
".sys_before:\n"
"	movl	16+ 4(%esp),%eax\n" /* syscall */
"	movl	16+ 8(%esp),%ebx\n" /* arg1 */
"	movl	16+12(%esp),%ecx\n" /* arg2 */
"	movl	16+16(%esp),%edx\n" /* arg3 */
"	movl	16+20(%esp),%esi\n" /* arg4 */
"	movl	16+24(%esp),%edi\n" /* arg5 */
"	movl	16+28(%esp),%ebp\n" /* arg6 */
".sys_restarted:\n"
"	int	$0x80\n"
".sys_after:\n"
"	movl	16+32(%esp),%ebx\n"	/* ebx = Int *res */
"	movl	%eax, (%ebx)\n"		/* write the syscall retval */

"	movl	16+36(%esp),%ebx\n"	/* ebx = enum PXState * */
"	testl	%ebx, %ebx\n"
"	jz	1f\n"

"	movl	16+40(%esp),%ecx\n"	/* write the post state (must be after retval write) */
"	movl	%ecx,(%ebx)\n"

".sys_done:\n"				/* OK, all clear from here */
"1:	popl	%ebp\n"
"	popl	%ebx\n"
"	popl	%edi\n"
"	popl	%esi\n"
"	ret\n"
"	.size do_thread_syscall,.-do_thread_syscall\n"
".previous\n"

".section .rodata\n"
"sys_before:	.long	.sys_before\n"
"sys_restarted:	.long	.sys_restarted\n"
"sys_after:	.long	.sys_after\n"
"sys_done:	.long	.sys_done\n"
".previous\n"
);
extern const Addr sys_before, sys_restarted, sys_after, sys_done;

/* Run a syscall for a particular thread, getting the arguments from
   the thread's registers, and returning the result in the thread's
   eax.

   Assumes that the only thread state which matters is the contents of
   %eax-%ebp and the return value in %eax.
 */
static void thread_syscall(Int syscallno, ThreadState *tst, 
			   enum PXState *state , enum PXState poststate)
{
   do_thread_syscall(syscallno,   /* syscall no. */
		     tst->m_ebx,  /* arg 1 */
		     tst->m_ecx,  /* arg 2 */
		     tst->m_edx,  /* arg 3 */
		     tst->m_esi,  /* arg 4 */
		     tst->m_edi,  /* arg 5 */
		     tst->m_ebp,  /* arg 6 */
		     &tst->m_eax, /* result */
		     state,	  /* state to update */
		     poststate);  /* state when syscall has finished */
}

#define VG_PROXY_MAGIC	0xef83b192
struct ProxyLWP {
   UInt			magic;		/* magic number */
   ThreadId		tid;		/* scheduler's tid */
   ThreadState		*tst;		/* thread state */
   Int			lwp;		/* kernel's ID for LWP */
   Int			exitcode;	/* ProxyLWP exit code */

   Int			topx, frommain;	/* pipe fds */
   vki_ksiginfo_t	siginfo;	/* received signal */
   Bool			terminating;	/* in the middle of exiting */

   /* State of proxy */
   enum PXState		state;

   jmp_buf		jumpbuf;
};

static void sys_wait_results(Bool block, ThreadId tid, enum RequestType reqtype, Bool restart);

struct PX_Request {
   enum RequestType	request;

   vki_ksigset_t	sigmask;	/* sigmask applied by SigACK */
};

/* All replies are multiplexed over a single pipe, so we need to disinguish them */
struct PX_Reply {
   ThreadId		tid;		/* tid this reply pertains to */
   enum RequestType	req;		/* what this relates to */

   union {
      Int		syscallno;	/* system call completed */
      vki_ksiginfo_t	siginfo;	/* signal */
   } u;
};

/* results pipe */
static Int result_send = -1, result_recv = -1;

/* reentrant printf for proxy use */
#if 0
static void px_printf(const Char *fmt, ...)
{
   Char buf[1024];
   Char *cp = buf;
   va_list vargs;

   void addbuf(Char c) { *cp++ = c; }

   cp += VG_(sprintf)(buf, "[%d, %d]: ", VG_(getpid)(), VG_(gettid)());

   va_start(vargs,fmt);
   VG_(vprintf)(addbuf, fmt, vargs);
   va_end(vargs);
   VG_(send_bytes_to_logging_sink)(buf, cp-buf);
}
#else
static void px_printf(const Char *fmt, ...)
{
}
#endif

static const Char *pxs_name(enum PXState s)
{
   switch(s) {
#define S(x)	case PXS_##x: return #x
      S(BAD);
      S(WaitReq);
      S(RunSyscall);
      S(IntReply);
      S(SysDone);
      S(SigACK);
#undef S
   default: return "???";
   }
}

static const Char *px_name(enum RequestType r)
{
   switch(r) {
#define S(x)	case PX_##x: return #x
      S(BAD);
      S(SetSigmask);
      S(RunSyscall);
      S(Signal);
      S(SigACK);
      S(Ping);
      S(Exiting);
#undef S
   default: return "???";
   }
}

#define PROXYLWP_OFFSET	(VKI_BYTES_PER_PAGE - sizeof(ProxyLWP))

/* 
   Allocate a page for the ProxyLWP and its stack.

   This uses the trick for finding the LWP's private data by knowing
   that the stack is a single page, and that the ProxyLWP structure is
   at the end of it.  Therefore, given any %esp in the stack, you can
   find the ProxyLWP structure (see LWP_TSD()).
 */
static ProxyLWP *LWP_alloc(void)
{
   UChar *p = VG_(get_memory_from_mmap)(VKI_BYTES_PER_PAGE, "alloc_LWP");
   ProxyLWP *ret;
   vg_assert(p == (UChar *)PGROUNDDN(p)); /* px must be page aligned */

   ret = (ProxyLWP *)(p + PROXYLWP_OFFSET);

   ret->magic = VG_PROXY_MAGIC;

   return ret;
}

/* Free a thread structure */
static void LWP_free(ProxyLWP *px)
{
   UChar *p = (UChar *)PGROUNDDN(px);
   
   vg_assert(px->magic == VG_PROXY_MAGIC);
   px->magic = 0;
   vg_assert((p + PROXYLWP_OFFSET) == (UChar *)px);

   VG_(munmap)(p, VKI_BYTES_PER_PAGE);
}

/* Get a particular ProxyLWP's LWP structure from its esp (relies on
   stacks being page aligned, with the ProxyLWP structure at the
   end). */
static inline ProxyLWP *LWP_TSD(void *esp)
{
   UChar *p = (UChar *)PGROUNDDN(esp);
   ProxyLWP *ret;

   ret = (ProxyLWP *)(p + PROXYLWP_OFFSET);
   vg_assert(ret->magic == VG_PROXY_MAGIC);

   return ret;
}

/* Get top of stack */
static inline void *LWP_stack(ProxyLWP *px)
{
   vg_assert(px->magic == VG_PROXY_MAGIC);

   return (void *)(((void **)px) - 1);
}

static void proxy_fork_cleanup(ThreadId tid);

/* Init the proxy mechanism */
void VG_(proxy_init)(void)
{
   Int p[2];
   Int res;

   /* this will ignore any duplicate registrations */
   VG_(atfork)(NULL, NULL, proxy_fork_cleanup);

   vg_assert(result_recv == -1);
   vg_assert(result_send == -1);

   res = VG_(pipe)(p);
   vg_assert(res == 0);

   result_recv = VG_(safe_fd)(p[0]);
   result_send = VG_(safe_fd)(p[1]);
   
   /* Make reading end non-blocking */
   VG_(fcntl)(result_recv, VKI_F_SETFL, VKI_O_NONBLOCK);
}

/* After fork, the forking thread is in a strange state of having a
   couple of pipes still linked to the parent. */
static void proxy_fork_cleanup(ThreadId tid)
{
   ThreadId t;

   VG_(close)(result_recv);
   VG_(close)(result_send);

   result_recv = result_send = -1;

   VG_(proxy_init)();

   for(t = 1; t < VG_N_THREADS; t++) {
      ThreadState *tst = VG_(get_ThreadState)(t);
      ProxyLWP *proxy = tst->proxy;

      if (tst->status == VgTs_Empty) {
	 vg_assert(proxy == NULL);
	 continue;
      }

      vg_assert(proxy != NULL);

      /* We need to do a manual teardown, since the proxy this structure
	 describes is our parent's */
      VG_(close)(proxy->topx);
      VG_(close)(proxy->frommain);
   
      LWP_free(proxy);
      tst->proxy = NULL;
   }

   /* Create a proxy for calling thread.

      Since fork() is non-blocking, the thread status should already
      be Runnable.
    */
   vg_assert(VG_(is_valid_tid)(tid));
   vg_assert(VG_(threads)[tid].proxy == NULL);
   vg_assert(VG_(threads)[tid].status == VgTs_Runnable);

   VG_(proxy_create)(tid);
   VG_(proxy_setsigmask)(tid);
}

Int VG_(proxy_resfd)(void)
{
   return result_recv;
}

void VG_(proxy_shutdown)(void)
{
   VG_(close)(result_recv);
   VG_(close)(result_send);

   result_recv = result_send = -1;
}

/* This is called from within a proxy LWP signal handler.  This
   function records the siginfo, then longjmps back into the proxy
   main state machine loop.  The presumption is that the signal
   handler is being run with all signals blocked; the longjmp is
   there to make sure they stay masked until the application thread is
   ready to run its signal handler. */
void VG_(proxy_handlesig)(const vki_ksiginfo_t *siginfo, 
			  const struct vki_sigcontext *sigcontext)
{
   UChar local;
   ProxyLWP *px = LWP_TSD(&local);
   Addr eip = sigcontext->eip;
   Int eax = sigcontext->eax;

   vg_assert(siginfo->si_signo != 0);
   if (px->siginfo.si_signo != 0) {
      px_printf("proxy_handlesig: tid %d already has %d pending, new sig %d\n",
		px->lwp, px->siginfo.si_signo, siginfo->si_signo);
   }
   vg_assert(px->siginfo.si_signo == 0);

   px->siginfo = *siginfo;

   px_printf("proxy got signal %d\n", siginfo->si_signo);

   /* First look to see if the EIP is within our interesting ranges
      near a syscall to work out what should happen. */
   if (sys_before <= eip && eip <= sys_restarted) {
      /* We are before the syscall actually ran, or it did run and
	 wants to be restarted.  Either way, set the return code to
	 indicate a restart.  This is not really any different from
	 anywhere else, except that we can make some assertions about
	 the proxy and machine state here. */
      vg_assert(px->state == PXS_RunSyscall);
      vg_assert(px->tst->m_eax == -VKI_ERESTARTSYS);
   } else if (sys_after <= eip && eip <= sys_done) {
      /* We're after the syscall.  Either it was interrupted by the
	 signal, or the syscall completed normally.  In either case
	 eax contains the correct syscall return value, and the new
	 state is effectively PXS_SysDone. */
      vg_assert(px->state == PXS_RunSyscall ||
		px->state == PXS_SysDone);
      px->state = PXS_SysDone;
      px->tst->m_eax = eax;
   }
   px_printf("  signalled in state %s\n", pxs_name(px->state));

   __builtin_longjmp(px->jumpbuf, 1);
}

static Bool send_reply(const struct PX_Reply *reply)
{
   const Int size = sizeof(struct PX_Reply);

   return VG_(write)(result_send, reply, size) == size;
}

static Bool recv_reply(struct PX_Reply *reply)
{
   const Int size = sizeof(struct PX_Reply);

   return VG_(read)(result_recv, reply, size) == size;
}

/* Proxy LWP thread.  This is run as a separate cloned() thread, so it
   MUST NOT touch any core Valgrind data structures directly: the only
   exception is while we're running a PX_RunSyscall command, we may
   look at and update the thread's register state.  It interacts with
   the rest of Valgrind by receiving messages through its pipe and
   sending results through result_send. */
static Int proxylwp(void *v)
{
   ProxyLWP *px = (ProxyLWP *)v;
   Int frommain = px->frommain;
   ThreadState *tst = px->tst;
   vki_ksigset_t allsig;
   vki_ksigset_t appsigmask;	/* signal mask the client has asked for */
   Int ret = 1000;
   static const vki_kstack_t ss = { .ss_flags = VKI_SS_DISABLE };

   /* Block everything until we're told otherwise (LWP should have
      been started with all signals blocked anyway) */
   VG_(ksigfillset)(&allsig);
   VG_(ksigdelset)(&allsig, VKI_SIGVGKILL);	/* but allow SIGVGKILL to interrupt */

   VG_(ksigprocmask)(VKI_SIG_SETMASK, &allsig, NULL);

   appsigmask = allsig;

   /* no signal stack for us */
   VG_(ksigaltstack)(&ss, NULL);

   for(;;) {
      struct PX_Reply reply, sigreply;
      struct PX_Request req;
      Int res;

      if (__builtin_setjmp(px->jumpbuf)) {
	 /* We were hit by a signal.  This is the signal-driven part
	    of the state machine. 

	    This code prepares a reply which is suitable for whatever
	    was interrupted by this signal.  If "no reply" is the
	    right response, then it sets reply.req = PX_BAD.

	    NOTE: the ST:N notation represents the correspondence
	    between states where we can be interrupted in the main
	    state machine loop, and where those states are handled
	    here.
	 */

	 if (px->siginfo.si_signo != VKI_SIGVGKILL) {
	    /* First, send the signal info */
	    sigreply.tid = px->tid;
	    sigreply.req = PX_Signal;
	    sigreply.u.siginfo = px->siginfo;

	    if (!send_reply(&sigreply)) {
	       ret = 44;		/* incomplete or failed write */
	       goto out;
	    }
	 } else {
	    /* We got VKI_SIGVGKILL, which means we just skip all the
	       below and exit.  (Don't bother dealing with any pending
	       requests, because we'll probably just get confused.) */
	    px->state = PXS_WaitReq;
	    px->siginfo.si_signo = 0;
	    ret = 0;
	    goto out;
	 }

	 px->siginfo.si_signo = 0;

	 /* Now work out what our new state is, and what to do on the way. */
	 switch(px->state) {
	 case PXS_WaitReq:
	    /* We were interrupted while waiting for a request.  See
	       if we had actually read the request, and do the
	       appropriate thing if so. */
	    reply.req = req.request;
	    reply.tid = px->tid;

	    switch(req.request) {
	    case PX_BAD:
	       /* ST:1 */
	       /* nothing read; just wait for SigACK */
	       px->state = PXS_SigACK;
	       break;

	    case PX_RunSyscall:
	       /* ST:2 */
	       /* They asked for a syscall, but we were signalled
		  before even getting started.  Claim the syscall was
		  interrupted.

		  XXX how to distunguish between restartable and
		  non-restartable syscalls?  Does it matter?
	       */
	       reply.u.syscallno = tst->syscallno;

	       tst->m_eax = -VKI_ERESTARTSYS;
	       px->state = PXS_IntReply;
	       break;

	    case PX_SetSigmask:
	       /* ST:2 */
	       /* ST:3 */
	       /* They asked for a signal mask update. Ignore it,
		  because they're going to give us a new mask when
		  they send a SigACK, and we want all signals blocked
		  in the meantime.  However, we set the state to
		  PXS_IntReply to make sure the reply from the
		  PX_SetSigmask is sent. */
	       vg_assert(reply.req == PX_SetSigmask);
	       px->state = PXS_IntReply;
	       break;

	    case PX_Ping:
	       /* ST:2 */
	       /* We read a Ping request, so we need to send a Ping
		  reply. */
	       vg_assert(reply.req == PX_Ping);
	       px->state = PXS_IntReply;
	       break;

	    case PX_Exiting:
	    case PX_Signal:
	       ret = 10;	/* completely bogus - noone should send us a signal */
	       goto out;

	    case PX_SigACK:
	       ret = 11;	/* Also bogus.  No way we should get a
				   signal while waiting for a
				   SigACK. */
	       goto out;
	    }
	    break;

	 case PXS_RunSyscall:
	    /* ST:4 */
	    /* We were actually running the syscall when interrupted.
	       reply should already be set up, including return in eax. */
	    vg_assert(reply.req == PX_RunSyscall);
	    vg_assert(reply.u.syscallno == tst->syscallno);
	    vg_assert(tst->status == VgTs_WaitSys);
	    px->state = PXS_IntReply;
	    break;

	 case PXS_SysDone:
	    /* The syscall is done; we just need to send the results
	       back. */
	    vg_assert(reply.req == PX_RunSyscall);
	    vg_assert(reply.u.syscallno == tst->syscallno);
	    px->state = PXS_IntReply;
	    break;

	 case PXS_IntReply:
	 case PXS_SigACK:
	    ret = 13;		/* Bogus.  Same as ret=11 above. */
	    goto out;

	 case PXS_BAD:
	    ret = 33;
	    goto out;
	 }

	 /* End of signal handling states.  If the scheduler LWP is
	    currently running application code, tell it to drop back
	    into the scheduler loop ASAP to handle the signal. */
	 if (VG_(clo_lowlat_signals))
	    VG_(need_resched)(px->tid);
      }

      /* state_machine: */
      px_printf("proxylwp main: state %s\n", pxs_name(px->state));

      switch(px->state) {
      case PXS_WaitReq:
      case PXS_SigACK:
	 req.request = PX_BAD;	/* init request so we know if the read() read anything */

	 if (px->state == PXS_WaitReq) {
	    /* allow signals when waiting for a normal request */
	    VG_(ksigprocmask)(VKI_SIG_SETMASK, &appsigmask, NULL);
	 }

	 /* ST:1 */

	 res = VG_(read)(frommain, &req, sizeof(req));

	 /* ST:2 */

	 /* process message with signals blocked */
	 VG_(ksigprocmask)(VKI_SIG_SETMASK, &allsig, NULL);

	 if (res == 0) {
	    ret = 0;
	    goto out;		/* EOF - we're quitting */
	 }
	 
	 if (res < 0) {
	    px_printf("read(frommain) failed %d\n", res);
	    ret = 1;		/* error */
	    goto out;
	 }
	 if (res != sizeof(req)) {
	    ret = 2;		/* error - partial read */
	    goto out;
	 }

	 px_printf("read req: %s\n", px_name(req.request));

	 reply.tid = px->tid;
	 reply.req = req.request;

	 switch(req.request) {
	 case PX_Ping:
	    /* do nothing; just send reply */
	    break;

	 case PX_SigACK:
	    /* The thread ACKed the signal, and sent the mask they
	       want while running the handler. */
	    vg_assert(px->state == PXS_SigACK);
	    appsigmask = req.sigmask;
	    VG_(ksigdelset)(&appsigmask, VKI_SIGVGKILL);  /* but allow SIGVGKILL */
	    VG_(ksigdelset)(&appsigmask, VKI_SIGVGINT);   /*       and SIGVGINT to interrupt */
	    px->state = PXS_WaitReq;
	    reply.req = PX_BAD;	/* don't reply */
	    break;
	    
	 case PX_SetSigmask:
	    appsigmask = req.sigmask;
	    VG_(ksigdelset)(&appsigmask, VKI_SIGVGKILL);  /* but allow SIGVGKILL */
	    VG_(ksigdelset)(&appsigmask, VKI_SIGVGINT);   /*       and SIGVGINT to interrupt */

	    vg_assert(px->state == PXS_WaitReq || 
		      px->state == PXS_SigACK);

	    if (px->state != PXS_SigACK) {
	       /* If we're not waiting for a PX_SigACK, set the apps mask
		  to get at least one of the pending signals, which will
		  be delivered synchronously, so that some progress is
		  made before the we tell the client the mask has been
		  set..  Then reset the mask back to all blocked. */
	       VG_(ksigprocmask)(VKI_SIG_SETMASK, &appsigmask, NULL);
	       /* ST:3 */
	       VG_(ksigprocmask)(VKI_SIG_SETMASK, &allsig, NULL);
	    } else {
	       /* Waiting for SigACK.  We want all signals blocked,
		  and when the SigACK arrives, it will give us the
		  thread's signal mask for its handler. */
	    }
	    break;

	 case PX_RunSyscall:
	    /* Run a syscall for our thread; results will be poked
	       back into tst */
	    reply.u.syscallno = tst->syscallno;

	    vg_assert(px->state == PXS_WaitReq || 
		      px->state == PXS_SigACK);
	    if (px->state == PXS_SigACK) {
	       /* If we're in the middle of signal handling, make the
		  client's syscalls fail with ERESTARTSYS until its signal
		  handler runs - there should be at most one, if it was
		  on the way to us as we got the signal.  
	       */
	       px_printf("RunSyscall in SigACK: rejecting syscall %d with ERESTARTSYS\n",
			 reply.u.syscallno);
	       tst->m_eax = -VKI_ERESTARTSYS;
	    } else {
	       Int syscallno = tst->syscallno;
	       
	       px->state = PXS_RunSyscall;
	       /* If we're interrupted before we get to the syscall
		  itself, we want the syscall restarted. */
	       tst->m_eax = -VKI_ERESTARTSYS;

	       /* set our process group ID to match parent */
	       if (VG_(getpgrp)() != VG_(main_pgrp))
		  VG_(setpgid)(0, VG_(main_pgrp));

	       VG_(ksigprocmask)(VKI_SIG_SETMASK, &appsigmask, NULL);

	       /* ST:4 */
	       
	       thread_syscall(syscallno, tst, &px->state, PXS_SysDone);

	       /* ST:5 */

	       VG_(ksigprocmask)(VKI_SIG_SETMASK, &allsig, NULL);
	       /* whew - made it here without being interrupted */
	       px->state = PXS_WaitReq;

	       if (VG_(clo_lowlat_syscalls))
		  VG_(need_resched)(px->tid);
	    }
	    break;
	    
	 case PX_BAD:
	 case PX_Signal:
	 case PX_Exiting:
	    /* we never expect to see these */
	    ret = 3;
	    goto out;
	 }
	 break;

      case PXS_IntReply:
	 /* This state only exists so that we fall out and write the
	    interrupted syscall reply before moving to SigACK */
	 px->state = PXS_SigACK;
	 break;

      case PXS_RunSyscall:
      case PXS_SysDone:
      case PXS_BAD:
      default:
	 /* Never expect to see these states here */
	 ret = 5;
	 goto out;
      }

      /* If we have something sensible to say, say it */
      if (reply.req != PX_BAD) {
	 px_printf("sending reply %s\n", px_name(reply.req));

	 if (!send_reply(&reply)) {
	    ret = 4;		/* error - didn't write full message */
	    goto out;
	 }
	 reply.req = PX_BAD;
      }
   }
      
  out:
   px_printf("proxy exiting with ret=%d\n", ret);

   {
      struct PX_Reply reply;
      reply.req = PX_Exiting;
      reply.tid = px->tid;
      px_printf("exit: sending %s\n", px_name(reply.req));
      
      send_reply(&reply);
   }

   px->frommain = -1;
   VG_(close)(frommain);

   px->exitcode = ret;
   return ret;
}

/* Send a signal to a proxy LWP */
void VG_(proxy_sendsig)(ThreadId tid, Int sig)
{
   ThreadState *tst = VG_(get_ThreadState)(tid);
   ProxyLWP *proxy = tst->proxy;
   Int lwp;

   if (proxy == NULL)
      return;

   lwp = proxy->lwp;		/* proxy->lwp may change async */

   if (lwp != 0) {
      /* SIGKILL and SIGSTOP always apply to all threads (need to
	 route for route_signals case?) */
      if (sig == VKI_SIGKILL || sig == VKI_SIGSTOP)
	 VG_(kkill)(VG_(main_pid), sig);
      else
	 VG_(ktkill)(lwp, sig);
   }

   /* If a thread is sending a signal to itself and the signal isn't
      blocked (ie, it will be delivered), wait until the signal
      message gets sent back, thus making the signal synchronous. */
   if (sig != 0 && 
       !VG_(is_sig_ign)(sig) &&
       tid == VG_(get_current_or_recent_tid)() && 
       !VG_(ksigismember)(&tst->eff_sig_mask, sig)) {
      /* If the LWP is actually blocked in a sigtimedwait, then it
	 will eat the signal rather than make it pending and deliver
	 it by the normal mechanism.  In this case, just wait for the
	 syscall to dinish. */
      if (tst->status == VgTs_WaitSys && tst->syscallno == __NR_rt_sigtimedwait)
	 sys_wait_results(True, tid, PX_RunSyscall, True);
      else
	 sys_wait_results(True, tid, PX_Signal, True);
   }
}

/* If a thread is blocked in a syscall, this function will interrupt
   the proxy LWP's syscall by hitting it with a VKI_SIGVGINT signal.
   This signal will not be reported to the client application. */
void VG_(proxy_abort_syscall)(ThreadId tid)
{
   ThreadState *tst = VG_(get_ThreadState)(tid);
   ProxyLWP *proxy = tst->proxy;
   Int lwp;

   if (tst->status != VgTs_WaitSys)
      return;

   vg_assert(proxy != NULL);

   lwp = proxy->lwp;
   
   if (lwp != 0)
      VG_(ktkill)(lwp, VKI_SIGVGINT);

   sys_wait_results(True, tid, PX_RunSyscall, False);

   vg_assert(tst->status == VgTs_Runnable);
}

static Int do_futex(void *addr, Int op, Int val, struct vki_timespec *time, void *addr2)
{
   return VG_(do_syscall)(__NR_futex, addr, op, val, time, addr2);
}

static Int have_settid = -1;	/* -1 -> unknown */

/*
  Create a proxy LWP using whatever varient of clone makes the most
   sense for the current kernel.  We use futexes for termination
   notification wherever possible.  Returns 0 on success, or a -ve
   error code on failure.
*/
static Int proxy_clone(ProxyLWP *proxy)
{
   Int ret = -1;

   if (have_settid != 0) {
      ret = VG_(clone)(proxylwp, 
		       LWP_stack(proxy),
		       VKI_CLONE_FS | VKI_CLONE_FILES | VKI_CLONE_VM |
		       VKI_CLONE_SIGHAND | VKI_CLONE_THREAD | 
		       VKI_CLONE_PARENT_SETTID |
		       VKI_CLONE_CHILD_CLEARTID | VKI_CLONE_DETACHED,
		       proxy, &proxy->lwp, &proxy->lwp);

      if ( have_settid == -1 && (ret < 0 || proxy->lwp == 0) ) {
         have_settid = 0;
	 
	 /* Assume that not having parent_settid also means that we've
	    got 2.4-style signal handling, which means we need to do
	    more work. */
         VG_(do_signal_routing) = True;

	 if (ret > 0) {
	    /* If clone actually succeeded and just ignored the
	       CLONE_PARENT_SETTID flag, then use the LWP it created
	       for us. */
	    proxy->lwp = ret;
	 }
       }
       else 
           have_settid = 1;
   }

   if (ret < 0) {
      vg_assert(have_settid == 0);
      vg_assert(proxy->lwp == 0);

      ret = VG_(clone)(proxylwp, 
		       LWP_stack(proxy),
		       VKI_CLONE_FS | VKI_CLONE_FILES | VKI_CLONE_VM |
		       VKI_CLONE_SIGHAND | VKI_CLONE_THREAD, 
		       proxy, NULL, NULL);
      proxy->lwp = ret;
   }

   return (ret < 0) ? ret : 0;
}

/* Wait on a proxy LWP.  Returns True if the LWP has exited. */
static Bool proxy_wait(ProxyLWP *proxy, Bool block, Int *status)
{
   Bool ret = False;

   if (have_settid == -1)
      return False;

   if (have_settid) {
      if (block) {
	 Int lwp = proxy->lwp;


	 if(proxy->lwp != 0)
	    do_futex(&proxy->lwp, VKI_FUTEX_WAIT, lwp, NULL, NULL);

	 if (status)
	    *status = proxy->exitcode;
	 ret = True;
      } else {
	 if (proxy->lwp == 0) {
	    *status = proxy->exitcode;
	    ret = True;
	 }
      }
   } else {
      Int flags = VKI__WCLONE;
      Int res;

      if (!block)
	 flags |= VKI_WNOHANG;
      res = VG_(waitpid)(proxy->lwp, status, flags);
      if (res == proxy->lwp) {
	 vg_assert(*status == proxy->exitcode);
	 ret = True;
      }
   }

   return ret;
}

/* Create a proxy for a new thread */
void VG_(proxy_create)(ThreadId tid)
{
   ThreadState *tst = VG_(get_ThreadState)(tid);
   ProxyLWP *proxy;
   Int p[2];
   vki_ksigset_t mask;
   Int ret;

   vg_assert(tst->proxy == NULL);
   vg_assert(tst->status == VgTs_Runnable);

   proxy = LWP_alloc();

   VG_(pipe)(p);

   proxy->tid = tid;
   proxy->tst = tst;
   proxy->exitcode = 0;
   proxy->lwp = 0;
   proxy->siginfo.si_signo = 0;
   proxy->frommain = VG_(safe_fd)(p[0]);
   proxy->topx = VG_(safe_fd)(p[1]);
   proxy->state = PXS_WaitReq;	/* start by waiting for requests */
   proxy->terminating = False;

   /* Make sure proxy LWP starts with all signals blocked (not even
      SEGV, BUS, ILL or FPE) */
   VG_(block_all_host_signals)(&mask);

   ret = proxy_clone(proxy);
   if (ret < 0) {
	   VG_(printf)("Error %d trying to create proxy LWP for tid %d\n",
		       ret, tid);
	   VG_(core_panic)("Can't start proxy LWPs");
   }

   VG_(restore_all_host_signals)(&mask);

   tst->proxy = proxy;
}

/* Clean up proxy after thread dies */
void VG_(proxy_delete)(ThreadId tid, Bool force)
{
   ThreadState *tst = VG_(get_ThreadState)(tid);
   ProxyLWP *proxy = tst->proxy;
   Bool res;
   Int status;
   Int lwp;

   if (proxy == NULL)
      return;			/* nothing to do */

   lwp = proxy->lwp;

#if 0
   MAYBE_PRINTF("VG_(proxy_delete)(tid=%d (lwp=%d), force=%s; tst->status=%d\n",
		tid, lwp, force ? "true" : "false", tst->status);
#endif
   vg_assert(proxy->tid == tid);
   if (proxy->terminating)
      return;		/* already going away */

   proxy->terminating = True;

   VG_(close)(proxy->topx);
   proxy->topx = -1;

   /* proxy thread will close proxy->frommain itself */

   if (force && lwp != 0) {
      /* wouldn't need to force it if it were already dead */
      vg_assert(tst->status != VgTs_Empty);
      //VG_(printf)("kill %d with SIGVGKILL\n", lwp);
      VG_(ktkill)(lwp, VKI_SIGVGKILL);
   } else
      vg_assert(tst->status == VgTs_Empty); /* just killed */

   status = -1;
   res = False;

   /* We need to wait for the PX_Exiting message before doing the
      proxy_wait, because if we don't read the results pipe, the proxy
      may be blocked writing to it, causing a deadlock with us as we
      wait for it to exit. */
   sys_wait_results(True, tid, PX_Exiting, True);
   res = proxy_wait(proxy, True, &status);

   if ((!res || status != 0) && VG_(clo_verbosity) > 1)
      VG_(printf)("proxy %d for tid %d exited status %d, res %d\n",
		  lwp, tid, status, res);

   LWP_free(proxy);
   tst->proxy = NULL;
}

/* Read back the results of any completed syscalls.

   At this point, there should be only one pending syscall per thread.
   Those threads should be in VgTs_WaitSys state.  Each syscall return
   may have multiple signals associated with it, so we read those and
   set up some pending signals in our signal simulation.  When we
   finally get the message saying the syscall is complete, we mark the
   thread as runnable and return.

   If block is set to True, then this call will block until anything
   happens (ie, some progress was made).

   If reqtype != PX_BAD, then this will block until some reply for
   that request type appears (assuming you're expecting that kind of
   reply, otherwise it will block forever).  If tid != 0, then it will
   wait for a reply for that particular tid.
 */
static void sys_wait_results(Bool block, ThreadId tid, enum RequestType reqtype, Bool restart)
{
   Bool found_reply = (reqtype == PX_BAD);
   struct PX_Reply res;
   
   vg_assert(VG_(gettid)() == VG_(main_pid));

   do {
      if (reqtype != PX_BAD || block) {
	 /* wait for activity on recv_res */
	 struct vki_pollfd pollfd;
	 Int ret;

	 /* result_recv could be -1 if we're asking for results before any
	    syscalls are issued - which is OK - but we can't block on
	    it. */
	 vg_assert(result_recv != -1);

	 pollfd.fd = result_recv;
	 pollfd.events = VKI_POLLIN;

	 do {
	    ret = VG_(poll)(&pollfd, 1, -1);
	 } while(ret == -VKI_EINTR);
 
	 if (ret <= 0) {
	    VG_(printf)("sys_wait_results: poll failed fd=%d errno=%d\n",
			pollfd.fd, ret);
	    return;
	 }
      }

      while(recv_reply(&res)) {
	 ThreadState *tst;

	 if (reqtype != PX_BAD &&
	     res.req == reqtype &&
	     (tid == 0 || tid == res.tid))
	    found_reply = True;

	 tst = VG_(get_ThreadState)(res.tid);

	 switch(res.req) {
	 case PX_SetSigmask:
	    /* Don't need to do anything */
	    if (VG_(clo_trace_signals) || VG_(clo_trace_syscalls))
	       VG_(message)(Vg_DebugMsg, "sys_wait_results: got PX_SetSigmask for TID %d",
			    res.tid);
	    break;

	 case PX_RunSyscall:
	    if (VG_(clo_trace_syscalls))
	       VG_(message)(Vg_DebugMsg, "sys_wait_results: got PX_RunSyscall for TID %d: syscall %d result %d",
			    res.tid, tst->syscallno, tst->m_eax);

	    if (tst->status != VgTs_WaitSys)
	       VG_(printf)("tid %d in status %d\n",
			   tst->tid, tst->status);
	 
	    vg_assert(res.u.syscallno == tst->syscallno);
	    vg_assert(tst->status == VgTs_WaitSys);

	    VG_(post_syscall)(res.tid, restart);
	    break;

	 case PX_Signal:
	    if (VG_(clo_trace_signals) || VG_(clo_trace_syscalls))
	       VG_(message)(Vg_DebugMsg, "sys_wait_results: got PX_Signal for TID %d, signal %d",
			    res.tid, res.u.siginfo.si_signo);

	    vg_assert(res.u.siginfo.si_signo != 0);
	    if (VG_(threads)[res.tid].proxy && 
		!VG_(threads)[res.tid].proxy->terminating)
	       VG_(deliver_signal)(res.tid, &res.u.siginfo, True);
	    break;

	 case PX_Ping:
	    /* Got a ping response. Great. */
	    break;

	 case PX_Exiting:
	    /* They're exiting.  Hooray! */
	    break;

	 case PX_BAD:
	 case PX_SigACK:
	 default:
	    VG_(core_panic)("sys_wait_results: got PX_BAD/PX_SigACK!\n");
	 }
      }
   } while(!found_reply);
}

/* External version */
void VG_(proxy_results)(void)
{
   sys_wait_results(False, 0, PX_BAD, True);
}

void VG_(proxy_wait_sys)(ThreadId tid, Bool restart)
{
   ThreadState *tst = VG_(get_ThreadState)(tid);

   vg_assert(tst->status == VgTs_WaitSys);

   sys_wait_results(True, tid, PX_RunSyscall, restart);
}

/* Tell proxy about it's thread's updated signal mask */
void VG_(proxy_setsigmask)(ThreadId tid)
{
   ThreadState *tst = VG_(get_ThreadState)(tid);
   ProxyLWP *proxy = tst->proxy;
   Int res;
   struct PX_Request req;

   vg_assert(proxy != NULL);
   vg_assert(proxy->tid == tid);

   req.request = PX_SetSigmask;
   req.sigmask = tst->sig_mask;

   tst->eff_sig_mask = tst->sig_mask;

   /* clear the results pipe before we try to write to a proxy to
      prevent a deadlock */
   VG_(proxy_results)();
   res = VG_(write)(proxy->topx, &req, sizeof(req));
   vg_assert(res == sizeof(req));

   /* wait for proxy to ack mask update; mask changes don't really
      have to be synchronous, but they do have to be fully ordered
      with respect to each other (ie, if thread A then thread B
      updates their signal masks, A's update must be done before B's
      is).  */
   sys_wait_results(True, tid, PX_SetSigmask, True);
}

void VG_(proxy_sigack)(ThreadId tid, const vki_ksigset_t *mask)
{
   ThreadState *tst = VG_(get_ThreadState)(tid);
   ProxyLWP *proxy = tst->proxy;
   Int res;
   struct PX_Request req;

   vg_assert(proxy != NULL);
   vg_assert(proxy->tid == tid);
   
   if (proxy_wait(proxy, False, NULL))
      return;

   req.request = PX_SigACK;
   req.sigmask = *mask;

   tst->eff_sig_mask = *mask;

#if 0
   /* Clear the results pipe before we try to write to a proxy to
      prevent a deadlock.

      XXX this breaks things.  This is called as a result of a
      PX_Signal message, and is called from within sys_wait_results.
      If that sys_wait_results was blocking of a particular message,
      it will never wake up if we eat those messages by calling
      sys_wait_results ourselves from here.  Maybe make
      sys_wait_results non-recursive?
   */
   VG_(proxy_results)();
#endif

   res = VG_(write)(proxy->topx, &req, sizeof(req));
   vg_assert(res == sizeof(req));
}

/* Wait for a signal to be delivered to any thread */
void VG_(proxy_waitsig)(void)
{
   if (VG_(do_signal_routing))
      VG_(route_signals)();
   else
      sys_wait_results(True, VG_INVALID_THREADID /* any */, PX_Signal, True);
}

/* Issue a syscall to the thread's ProxyLWP */
Int VG_(sys_issue)(int tid)
{
   ThreadState *tst = VG_(get_ThreadState)(tid);
   ProxyLWP *proxy = tst->proxy;
   Int res;
   struct PX_Request req;

   vg_assert(proxy != NULL);
   vg_assert(proxy->tid == tid);
   vg_assert(tst->status == VgTs_WaitSys);

   /* Clear the results pipe before we try to write to a proxy to
      prevent a deadlock (the proxyLWP may be trying to write a result
      back to the scheduler LWP, and therefore not be reading its
      input pipe, which would then block the write below).

      XXX I think this can't happen - the pipe has 4k of buffering,
      and can therefore fit many messages, but we can only have one
      outstanding - the write below will not block forever.  Fetching
      results here can cause all kinds of confusion, because we
      definitely don't want the complexity of trying to deliver a
      signal right now.
   */
   if (0)
      VG_(proxy_results)();

   req.request = PX_RunSyscall;

   tst->syscallno = tst->m_eax;
   tst->m_eax = -VKI_ERESTARTSYS;

   res = VG_(write)(proxy->topx, &req, sizeof(req));

   if (res != sizeof(req)) {
      VG_(message)(Vg_DebugMsg, "sys_issue: write to tid %d failed %d (not %d)\n",
		   tid, res, sizeof(req));
   }
   return 0;
}

/* Relatively expensive sanity tests for the syscall machinery */
void VG_(proxy_sanity)(void)
{
   Int tid;
   Bool sane = True;
   static const struct PX_Request req = { .request = PX_Ping };

   for(tid = 0; tid < VG_N_THREADS; tid++) {
      ThreadState *tst = &VG_(threads)[tid];
      ProxyLWP *px;
      Int status = 0;
      Int ret;

      if (tst->status == VgTs_Empty)
	 continue;

      if (tst->proxy == NULL) {
	 VG_(message)(Vg_DebugMsg, "TID %d: NULL proxy");
	 sane = False;
	 continue;
      }

      px = tst->proxy;

      if (px->tid != tid) {
	 VG_(message)(Vg_DebugMsg, 
		      "TID %d: proxy LWP %d doesn't have right tid (%d)\n",
		      tid, px->lwp, px->tid);
	 sane = False;
      }

      if (proxy_wait(px, False, &status)) {
	 VG_(message)(Vg_DebugMsg,
		      "TID %d: proxy LWP %d exited with status %d\n",
		      tid, px->lwp, status);
	 sane = False;
	 continue;
      }

      /* No point checking if proxy is busy in a syscall, but all
	 other times it should respond promptly. */
      if (tst->status != VgTs_WaitSys) {
	 ret = VG_(write)(px->topx, &req, sizeof(req));
	 if (ret != sizeof(req)) {
	    VG_(message)(Vg_DebugMsg,
			 "TID %d: failed to write PX_Ping to lwp %d: %d\n",
			 tid, px->lwp, ret);
	    sane = False;
	 }
	 sys_wait_results(True, tid, PX_Ping, True);
	 /* Can't make an assertion here, fortunately; this will
	    either come back or it won't. */
      }
   }

   vg_assert(sane);
}

/* Get the PID/TID of the ProxyLWP. */
__attribute__((unused))
static Int proxy_id(ThreadId tid)
{
   ThreadState *tst = VG_(get_ThreadState)(tid);
   return tst->proxy->lwp;
}

/*--------------------------------------------------------------------*/
/*--- Proxy LWP machinery.                           vg_proxylwp.c ---*/
/*--------------------------------------------------------------------*/
