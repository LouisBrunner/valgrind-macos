
/*--------------------------------------------------------------------*/
/*--- Platform-specific syscalls stuff.      syswrap-amd64-linux.c ---*/
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
#include "pub_core_debuglog.h"
#include "pub_core_aspacemgr.h"
#include "pub_core_options.h"
#include "pub_core_libcbase.h"
#include "pub_core_libcassert.h"
#include "pub_core_libcmman.h"
#include "pub_core_libcprint.h"
#include "pub_core_libcproc.h"
#include "pub_core_libcsignal.h"
#include "pub_core_sigframe.h"
#include "pub_core_signals.h"
#include "pub_core_syscall.h"
#include "pub_core_syswrap.h"
#include "pub_core_tooliface.h"

#include "priv_types_n_macros.h"
#include "priv_syswrap-generic.h"   /* for decls of generic wrappers */
#include "priv_syswrap-linux.h"     /* for decls of linux-ish wrappers */
#include "priv_syswrap-main.h"

#include "vki_unistd.h"              /* for the __NR_* constants */


/* ---------------------------------------------------------------------
   Stacks, thread wrappers
   Note.  Why is this stuff here?
   ------------------------------------------------------------------ */

/* 
   Allocate a stack for this thread.

   They're allocated lazily, but never freed.
 */
#define FILL	0xdeadbeefcabafeed

// Valgrind's stack size, in words.
#define STACK_SIZE_W      16384

static UWord* allocstack(ThreadId tid)
{
   ThreadState *tst = VG_(get_ThreadState)(tid);
   UWord* rsp;

   if (tst->os_state.valgrind_stack_base == 0) {
      void *stk = VG_(mmap)(0, STACK_SIZE_W * sizeof(UWord) + VKI_PAGE_SIZE,
			    VKI_PROT_READ|VKI_PROT_WRITE,
			    VKI_MAP_PRIVATE|VKI_MAP_ANONYMOUS,
			    SF_VALGRIND,
			    -1, 0);

      if (stk != (void *)-1) {
	 VG_(mprotect)(stk, VKI_PAGE_SIZE, VKI_PROT_NONE); /* guard page */
	 tst->os_state.valgrind_stack_base = ((Addr)stk) + VKI_PAGE_SIZE;
	 tst->os_state.valgrind_stack_szB  = STACK_SIZE_W * sizeof(UWord);
      } else 
	 return (UWord*)-1;
   }

   for (rsp = (UWord*) tst->os_state.valgrind_stack_base; 
        rsp < (UWord*)(tst->os_state.valgrind_stack_base +
                       tst->os_state.valgrind_stack_szB); 
        rsp++)
      *rsp = FILL;
   /* rsp is left at top of stack */

   if (0)
      VG_(printf)("stack for tid %d at %p (%llx); rsp=%p\n",
		  tid, tst->os_state.valgrind_stack_base,
                  *(UWord*)(tst->os_state.valgrind_stack_base), rsp);

   return rsp;
}

/* NB: this is identical the the x86 version. */
/* Return how many bytes of this stack have not been used */
SSizeT VGA_(stack_unused)(ThreadId tid)
{
   ThreadState *tst = VG_(get_ThreadState)(tid);
   UWord* p;

   for (p = (UWord*)tst->os_state.valgrind_stack_base; 
	p && (p < (UWord*)(tst->os_state.valgrind_stack_base +
                           tst->os_state.valgrind_stack_szB)); 
	p++)
      if (*p != FILL)
	 break;

   if (0)
      VG_(printf)("p=%p %llx tst->os_state.valgrind_stack_base=%p\n",
                  p, *p, tst->os_state.valgrind_stack_base);

   return ((Addr)p) - tst->os_state.valgrind_stack_base;
}


/* Run a thread all the way to the end, then do appropriate exit actions
   (this is the last-one-out-turn-off-the-lights bit). 
*/
static void run_a_thread_NORETURN ( Word tidW )
{
   ThreadId tid = (ThreadId)tidW;

   VG_(debugLog)(1, "syswrap-amd64-linux", 
                    "run_a_thread_NORETURN(tid=%lld): "
                       "VG_(thread_wrapper) called\n",
                       (ULong)tidW);

   /* Run the thread all the way through. */
   VgSchedReturnCode src = VG_(thread_wrapper)(tid);  

   VG_(debugLog)(1, "syswrap-amd64-linux", 
                    "run_a_thread_NORETURN(tid=%lld): "
                       "VG_(thread_wrapper) done\n",
                       (ULong)tidW);

   Int c = VG_(count_living_threads)();
   vg_assert(c >= 1); /* stay sane */

   if (c == 1) {

      VG_(debugLog)(1, "syswrap-amd64-linux", 
                       "run_a_thread_NORETURN(tid=%lld): "
                          "last one standing\n",
                          (ULong)tidW);

      /* We are the last one standing.  Keep hold of the lock and
         carry on to show final tool results, then exit the entire system. */
      VG_(shutdown_actions_NORETURN)(tid, src);

   } else {

      VG_(debugLog)(1, "syswrap-amd64-linux", 
                       "run_a_thread_NORETURN(tid=%lld): "
                          "not last one standing\n",
                          (ULong)tidW);

      /* OK, thread is dead, but others still exist.  Just exit. */
      ThreadState *tst = VG_(get_ThreadState)(tid);

      /* This releases the run lock */
      VG_(exit_thread)(tid);
      vg_assert(tst->status == VgTs_Zombie);

      /* We have to use this sequence to terminate the thread to
         prevent a subtle race.  If VG_(exit_thread)() had left the
         ThreadState as Empty, then it could have been reallocated,
         reusing the stack while we're doing these last cleanups.
         Instead, VG_(exit_thread) leaves it as Zombie to prevent
         reallocation.  We need to make sure we don't touch the stack
         between marking it Empty and exiting.  Hence the
         assembler. */
      asm volatile (
         "movl	%1, %0\n"	/* set tst->status = VgTs_Empty */
         "movq	%2, %%rax\n"    /* set %rax = __NR_exit */
         "movq	%3, %%rdi\n"    /* set %rdi = tst->os_state.exitcode */
         "syscall\n"		/* exit(tst->os_state.exitcode) */
         : "=m" (tst->status)
         : "n" (VgTs_Empty), "n" (__NR_exit), "m" (tst->os_state.exitcode));

      VG_(core_panic)("Thread exit failed?\n");
   }

   /*NOTREACHED*/
   vg_assert(0);
}


/* Call f(arg1), but first switch stacks, using 'stack' as the new
   stack, and use 'retaddr' as f's return-to address.  Also, clear all
   the integer registers before entering f.  */
__attribute__((noreturn))
void call_on_new_stack_0_1 ( Addr stack,
			     Addr retaddr,
			     void (*f)(Word),
                             Word arg1 );
// %rdi == stack
// %rsi == retaddr
// %rdx == f
// %rcx == arg1
asm(
"call_on_new_stack_0_1:\n"
"   movq   %rdi, %rsp\n"   // set stack
"   pushq  %rsi\n"         // retaddr to stack
"   pushq  %rdx\n"         // f to stack
"   pushq  %rcx\n"         // arg1 to stack
"   movq $0, %rax\n"       // zero all GP regs
"   movq $0, %rbx\n" 
"   movq $0, %rcx\n"
"   movq $0, %rdx\n"
"   movq $0, %rsi\n"
"   movq $0, %rdi\n"
"   movq $0, %rbp\n"
"   movq $0, %r8\n"
"   movq $0, %r9\n"
"   movq $0, %r10\n"
"   movq $0, %r11\n"
"   movq $0, %r12\n"
"   movq $0, %r13\n"
"   movq $0, %r14\n"
"   movq $0, %r15\n"
"   popq   %rdi\n"         // arg1 to correct arg reg
"   ret\n"                 // jump to f
"   ud2\n"                 // should never get here
);

/*
   Allocate a stack for the main thread, and run it all the way to the
   end.  
*/
void VGP_(main_thread_wrapper_NORETURN)(ThreadId tid)
{
   VG_(debugLog)(1, "syswrap-amd64-linux", 
                    "entering VGP_(main_thread_wrapper_NORETURN)\n");

   UWord* rsp = allocstack(tid);

   /* shouldn't be any other threads around yet */
   vg_assert( VG_(count_living_threads)() == 1 );

   call_on_new_stack_0_1( 
      (Addr)rsp,              /* stack */
      0,                      /*bogus return address*/
      run_a_thread_NORETURN,  /* fn to call */
      (Word)tid               /* arg to give it */
   );

   /*NOTREACHED*/
   vg_assert(0);
}


static Long start_thread_NORETURN ( void* arg )
{
   ThreadState* tst = (ThreadState*)arg;
   ThreadId     tid = tst->tid;

   run_a_thread_NORETURN ( (Word)tid );
   /*NOTREACHED*/
   vg_assert(0);
}


/* ---------------------------------------------------------------------
   clone() handling
   ------------------------------------------------------------------ */

/*
        Perform a clone system call.  clone is strange because it has
        fork()-like return-twice semantics, so it needs special
        handling here.

	Upon entry, we have:

	    int (*fn)(void*)	in %rdi
	    void*  child_stack	in %rsi
	    int    flags	in %rdx
	    void*  arg		in %rcx
	    pid_t* child_tid	in %r8
	    pid_t* parent_tid	in %r9
	    void*  tls_ptr      at 8(%rsp)

	System call requires:

	    int    $__NR_clone  in %rax
	    int    flags	in %rdi
	    void*  child_stack	in %rsi
	    pid_t* parent_tid	in %rdx
	    pid_t* child_tid	in %r10
	    void*  tls_ptr      in %r8

	Returns a Long encoded in the linux-amd64 way, not a SysRes.
 */
#define STRINGIFZ(__str) #__str
#define STRINGIFY(__str)  STRINGIFZ(__str)
#define __NR_CLONE        STRINGIFY(__NR_clone)
#define __NR_EXIT         STRINGIFY(__NR_exit)

extern
Long do_syscall_clone_amd64_linux ( Long (*fn)(void *), 
                                    void* stack, 
                                    Long  flags, 
                                    void* arg,
                                    Long* child_tid, 
                                    Long* parent_tid, 
                                    vki_modify_ldt_t * );
asm(
"\n"
"do_syscall_clone_amd64_linux:\n"
        // set up child stack, temporarily preserving fn and arg
"       subq    $16, %rsi\n"            // make space on stack
"       movq    %rcx, 8(%rsi)\n"        // save arg
"       movq    %rdi, 0(%rsi)\n"        // save fn 
        
        // setup syscall
"       movq    $"__NR_CLONE", %rax\n"  // syscall number
"       movq    %rdx,     %rdi\n"       // syscall arg1: flags
        // %rsi already setup           // syscall arg2: child_stack
"       movq    %r9,      %rdx\n"       // syscall arg3: parent_tid
"       movq    %r8,      %r10\n"       // syscall arg4: child_tid
"       movq    8(%rsp),  %r8\n"        // syscall arg5: tls_ptr

"       syscall\n"                      // clone()

"       testq   %rax, %rax\n"           // child if retval == 0
"       jnz     1f\n"

        // CHILD - call thread function
"       pop     %rax\n"                 // pop fn
"       pop     %rdi\n"                 // pop fn arg1: arg
"       call    *%rax\n"                // call fn

        // exit with result
"       movq    %rax, %rdi\n"           // arg1: return value from fn
"       movq    $"__NR_EXIT", %rax\n"

"       syscall\n"

        // Exit returned?!
"       ud2\n"

"1:\n"  // PARENT or ERROR
"       ret\n"
);

#undef __NR_CLONE
#undef __NR_EXIT
#undef STRINGIFY
#undef STRINGIFZ


// forward declaration
static void setup_child ( ThreadArchState*, ThreadArchState* );

/* 
   When a client clones, we need to keep track of the new thread.  This means:
   1. allocate a ThreadId+ThreadState+stack for the the thread

   2. initialize the thread's new VCPU state

   3. create the thread using the same args as the client requested,
   but using the scheduler entrypoint for EIP, and a separate stack
   for ESP.
 */
static SysRes do_clone ( ThreadId ptid, 
                         ULong flags, Addr rsp, 
                         Long* parent_tidptr, 
                         Long* child_tidptr, 
                         Addr tlsaddr )
{
   static const Bool debug = False;

   ThreadId     ctid = VG_(alloc_ThreadState)();
   ThreadState* ptst = VG_(get_ThreadState)(ptid);
   ThreadState* ctst = VG_(get_ThreadState)(ctid);
   UWord*       stack;
   Segment*     seg;
   SysRes       res;
   Long         rax;
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
   setup_child( &ctst->arch, &ptst->arch );

   /* Make sys_clone appear to have returned Success(0) in the
      child. */
   ctst->arch.vex.guest_RAX = 0;

   if (rsp != 0)
      ctst->arch.vex.guest_RSP = rsp;

   ctst->os_state.parent = ptid;

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
      ctst->client_stack_highest_word = (Addr)VG_PGROUNDUP(rsp);
      ctst->client_stack_szB  = ctst->client_stack_highest_word - seg->addr;

      if (debug)
	 VG_(printf)("tid %d: guessed client stack range %p-%p\n",
		     ctid, seg->addr, VG_PGROUNDUP(rsp));
   } else {
      VG_(message)(Vg_UserMsg, "!? New thread %d starts with RSP(%p) unmapped\n",
		   ctid, rsp);
      ctst->client_stack_szB  = 0;
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
   rax = do_syscall_clone_amd64_linux(
            start_thread_NORETURN, stack, flags, &VG_(threads)[ctid],
            child_tidptr, parent_tidptr, NULL
         );
   res = VG_(mk_SysRes)( rax );

   VG_(sigprocmask)(VKI_SIG_SETMASK, &savedmask, NULL);

   if (res.isError) {
      /* clone failed */
      VGP_(cleanup_thread)(&ctst->arch);
      ctst->status = VgTs_Empty;
   }

   return res;
}


/* Do a clone which is really a fork() */
static SysRes do_fork_clone ( ThreadId tid, 
                              ULong flags, Addr rsp, 
                              Long* parent_tidptr, 
                              Long* child_tidptr )
{
   vki_sigset_t fork_saved_mask;
   vki_sigset_t mask;
   SysRes       res;

   if (flags & (VKI_CLONE_SETTLS | VKI_CLONE_FS | VKI_CLONE_VM 
                | VKI_CLONE_FILES | VKI_CLONE_VFORK))
      return VG_(mk_SysRes_Error)( VKI_EINVAL );

   /* Block all signals during fork, so that we can fix things up in
      the child without being interrupted. */
   VG_(sigfillset)(&mask);
   VG_(sigprocmask)(VKI_SIG_SETMASK, &mask, &fork_saved_mask);

   VG_(do_atfork_pre)(tid);

   /* Since this is the fork() form of clone, we don't need all that
      VG_(clone) stuff */
   res = VG_(do_syscall5)( __NR_clone, flags, 
                           (UWord)NULL, (UWord)parent_tidptr, 
                           (UWord)NULL, (UWord)child_tidptr );

   if (!res.isError && res.val == 0) {
      /* child */
      VG_(do_atfork_child)(tid);

      /* restore signal mask */
      VG_(sigprocmask)(VKI_SIG_SETMASK, &fork_saved_mask, NULL);
   } 
   else 
   if (!res.isError && res.val > 0) {
      /* parent */
      if (VG_(clo_trace_syscalls))
	  VG_(printf)("   clone(fork): process %d created child %d\n", 
                      VG_(getpid)(), res.val);

      VG_(do_atfork_parent)(tid);

      /* restore signal mask */
      VG_(sigprocmask)(VKI_SIG_SETMASK, &fork_saved_mask, NULL);
   }

   return res;
}

/* ---------------------------------------------------------------------
   More thread stuff
   ------------------------------------------------------------------ */

void VGP_(cleanup_thread) ( ThreadArchState *arch )
{  
}  

void setup_child ( /*OUT*/ ThreadArchState *child, 
                   /*IN*/  ThreadArchState *parent )
{  
   /* We inherit our parent's guest state. */
   child->vex = parent->vex;
   child->vex_shadow = parent->vex_shadow;
}  


/* ---------------------------------------------------------------------
   PRE/POST wrappers for AMD64/Linux-specific syscalls
   ------------------------------------------------------------------ */

#define PRE(name)       DEFN_PRE_TEMPLATE(amd64_linux, name)
#define POST(name)      DEFN_POST_TEMPLATE(amd64_linux, name)

/* Add prototypes for the wrappers declared here, so that gcc doesn't
   harass us for not having prototypes.  Really this is a kludge --
   the right thing to do is to make these wrappers 'static' since they
   aren't visible outside this file, but that requires even more macro
   magic. */
DECL_TEMPLATE(amd64_linux, sys_clone);
DECL_TEMPLATE(amd64_linux, sys_rt_sigreturn);
DECL_TEMPLATE(amd64_linux, sys_socket);
DECL_TEMPLATE(amd64_linux, sys_setsockopt);
DECL_TEMPLATE(amd64_linux, sys_getsockopt);
DECL_TEMPLATE(amd64_linux, sys_connect);
DECL_TEMPLATE(amd64_linux, sys_accept);
DECL_TEMPLATE(amd64_linux, sys_sendto);
DECL_TEMPLATE(amd64_linux, sys_recvfrom);
DECL_TEMPLATE(amd64_linux, sys_sendmsg);
DECL_TEMPLATE(amd64_linux, sys_recvmsg);
DECL_TEMPLATE(amd64_linux, sys_shutdown);
DECL_TEMPLATE(amd64_linux, sys_bind);
DECL_TEMPLATE(amd64_linux, sys_listen);
DECL_TEMPLATE(amd64_linux, sys_getsockname);
DECL_TEMPLATE(amd64_linux, sys_getpeername);
DECL_TEMPLATE(amd64_linux, sys_socketpair);
DECL_TEMPLATE(amd64_linux, sys_semget);
DECL_TEMPLATE(amd64_linux, sys_semop);
DECL_TEMPLATE(amd64_linux, sys_semtimedop);
DECL_TEMPLATE(amd64_linux, sys_semctl);
DECL_TEMPLATE(amd64_linux, sys_msgget);
DECL_TEMPLATE(amd64_linux, sys_msgrcv);
DECL_TEMPLATE(amd64_linux, sys_msgsnd);
DECL_TEMPLATE(amd64_linux, sys_msgctl);
DECL_TEMPLATE(amd64_linux, sys_shmget);
DECL_TEMPLATE(amd64_linux, wrap_sys_shmat);
DECL_TEMPLATE(amd64_linux, sys_shmdt);
DECL_TEMPLATE(amd64_linux, sys_shmdt);
DECL_TEMPLATE(amd64_linux, sys_shmctl);
DECL_TEMPLATE(amd64_linux, sys_arch_prctl);


PRE(sys_clone)
{
   ULong cloneflags;

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
         SET_STATUS_Failure( VKI_EFAULT );
         return;
      }
   }
   if (ARG1 & (VKI_CLONE_CHILD_SETTID | VKI_CLONE_CHILD_CLEARTID)) {
      PRE_MEM_WRITE("clone(child_tidptr)", ARG4, sizeof(Int));
      if (!VG_(is_addressable)(ARG4, sizeof(Int), VKI_PROT_WRITE)) {
         SET_STATUS_Failure( VKI_EFAULT );
         return;
      }
   }

   cloneflags = ARG1;

   if (!VG_(client_signal_OK)(ARG1 & VKI_CSIGNAL)) {
      SET_STATUS_Failure( VKI_EINVAL );
      return;
   }

   /* Only look at the flags we really care about */
   switch (cloneflags & (VKI_CLONE_VM | VKI_CLONE_FS 
                         | VKI_CLONE_FILES | VKI_CLONE_VFORK)) {
   case VKI_CLONE_VM | VKI_CLONE_FS | VKI_CLONE_FILES:
      /* thread creation */
      SET_STATUS_from_SysRes(
         do_clone(tid,
                  ARG1,          /* flags */
                  (Addr)ARG2,    /* child ESP */
                  (Long *)ARG3,  /* parent_tidptr */
                  (Long *)ARG4,  /* child_tidptr */
                  (Addr)ARG5));  /* set_tls */
      break;

   case VKI_CLONE_VFORK | VKI_CLONE_VM: /* vfork */
      /* FALLTHROUGH - assume vfork == fork */
      cloneflags &= ~(VKI_CLONE_VFORK | VKI_CLONE_VM);

   case 0: /* plain fork */
      SET_STATUS_from_SysRes(
         do_fork_clone(tid,
                       cloneflags,      /* flags */
                       (Addr)ARG2,      /* child ESP */
                       (Long *)ARG3,    /* parent_tidptr */
                       (Long *)ARG4));  /* child_tidptr */
      break;

   default:
      /* should we just ENOSYS? */
      VG_(message)(Vg_UserMsg, "Unsupported clone() flags: 0x%x", ARG1);
      VG_(message)(Vg_UserMsg, "");
      VG_(message)(Vg_UserMsg, "The only supported clone() uses are:");
      VG_(message)(Vg_UserMsg, " - via a threads library (LinuxThreads or NPTL)");
      VG_(message)(Vg_UserMsg, " - via the implementation of fork or vfork");
      VG_(unimplemented)
         ("Valgrind does not support general clone().");
   }

   if (SUCCESS) {
      if (ARG1 & VKI_CLONE_PARENT_SETTID)
         POST_MEM_WRITE(ARG3, sizeof(Int));
      if (ARG1 & (VKI_CLONE_CHILD_SETTID | VKI_CLONE_CHILD_CLEARTID))
         POST_MEM_WRITE(ARG4, sizeof(Int));

      /* Thread creation was successful; let the child have the chance
         to run */
      *flags |= SfYieldAfter;
   }
}

PRE(sys_rt_sigreturn)
{
   ThreadState* tst;
   PRINT("rt_sigreturn ( )");

   vg_assert(VG_(is_valid_tid)(tid));
   vg_assert(tid >= 1 && tid < VG_N_THREADS);
   vg_assert(VG_(is_running_thread)(tid));

   /* Adjust esp to point to start of frame; skip back up over handler
      ret addr */
   tst = VG_(get_ThreadState)(tid);
   tst->arch.vex.guest_RSP -= sizeof(Addr);

   /* This is only so that the RIP is (might be) useful to report if
      something goes wrong in the sigreturn */
   VG_(fixup_guest_state_to_restart_syscall)(&tst->arch);

   VG_(sigframe_destroy)(tid, True);

   /* For unclear reasons, it appears we need the syscall to return
      without changing %RAX.  Since %RAX is the return value, and can
      denote either success or failure, we must set up so that the
      driver logic copies it back unchanged.  Also, note %RAX is of
      the guest registers written by VG_(sigframe_destroy). */
   SET_STATUS_from_SysRes( VG_(mk_SysRes)( tst->arch.vex.guest_RAX ) );

   /* Check to see if some any signals arose as a result of this. */
   *flags |= SfPollAfter;
}

PRE(sys_arch_prctl)
{
   ThreadState* tst;
   PRINT( "arch_prctl ( %d, %llx )", ARG1, ARG2 );

   vg_assert(VG_(is_valid_tid)(tid));
   vg_assert(tid >= 1 && tid < VG_N_THREADS);
   vg_assert(VG_(is_running_thread)(tid));

   // Nb: can't use "ARG2".."ARG5" here because that's our own macro...
   PRE_REG_READ2(long, "arch_prctl",
                 int, option, unsigned long, arg2);
   // XXX: totally wrong... we need to look at the 'option' arg, and do
   // PRE_MEM_READs/PRE_MEM_WRITEs as necessary...

   /* "do" the syscall ourselves; the kernel never sees it */
   vg_assert(ARG1 == VKI_ARCH_SET_FS);
   tst = VG_(get_ThreadState)(tid);
   tst->arch.vex.guest_FS_ZERO = ARG2;

   /* Note; the Status writeback to guest state that happens after
      this wrapper returns does not change guest_FS_ZERO; hence that
      direct assignment to the guest state is safe here. */
   SET_STATUS_Success( 0 );
}

PRE(sys_socket)
{
   PRINT("sys_socket ( %d, %d, %d )",ARG1,ARG2,ARG3);
   PRE_REG_READ3(long, "socket", int, domain, int, type, int, protocol);
}
POST(sys_socket)
{
   SysRes r;
   vg_assert(SUCCESS);
   r = VG_(generic_POST_sys_socket)(tid, VG_(mk_SysRes_Success)(RES));
   SET_STATUS_from_SysRes(r);
}

PRE(sys_setsockopt)
{
   PRINT("sys_setsockopt ( %d, %d, %d, %p, %d )",ARG1,ARG2,ARG3,ARG4,ARG5);
   PRE_REG_READ5(long, "setsockopt",
                 int, s, int, level, int, optname,
                 const void *, optval, int, optlen);
   VG_(generic_PRE_sys_setsockopt)(tid, ARG1,ARG2,ARG3,ARG4,ARG5);
}

PRE(sys_getsockopt)
{
   PRINT("sys_getsockopt ( %d, %d, %d, %p, %p )",ARG1,ARG2,ARG3,ARG4,ARG5);
   PRE_REG_READ5(long, "getsockopt",
                 int, s, int, level, int, optname,
                 void *, optval, int, *optlen);
   VG_(generic_PRE_sys_getsockopt)(tid, ARG1,ARG2,ARG3,ARG4,ARG5);
}
POST(sys_getsockopt)
{
   vg_assert(SUCCESS);
   VG_(generic_POST_sys_getsockopt)(tid, VG_(mk_SysRes_Success)(RES),
                                         ARG1,ARG2,ARG3,ARG4,ARG5);
}

PRE(sys_connect)
{
   *flags |= SfMayBlock;
   PRINT("sys_connect ( %d, %p, %d )",ARG1,ARG2,ARG3);
   PRE_REG_READ3(long, "connect",
                 int, sockfd, struct sockaddr *, serv_addr, int, addrlen);
   VG_(generic_PRE_sys_connect)(tid, ARG1,ARG2,ARG3);
}

PRE(sys_accept)
{
   *flags |= SfMayBlock;
   PRINT("sys_accept ( %d, %p, %d )",ARG1,ARG2,ARG3);
   PRE_REG_READ3(long, "accept",
                 int, s, struct sockaddr *, addr, int, *addrlen);
   VG_(generic_PRE_sys_accept)(tid, ARG1,ARG2,ARG3);
}
POST(sys_accept)
{
   SysRes r;
   vg_assert(SUCCESS);
   r = VG_(generic_POST_sys_accept)(tid, VG_(mk_SysRes_Success)(RES),
                                         ARG1,ARG2,ARG3);
   SET_STATUS_from_SysRes(r);
}

PRE(sys_sendto)
{
   *flags |= SfMayBlock;
   PRINT("sys_sendto ( %d, %s, %d, %u, %p, %d )",ARG1,ARG2,ARG3,ARG4,ARG5,ARG6);
   PRE_REG_READ6(long, "sendto",
                 int, s, const void *, msg, int, len, 
                 unsigned int, flags, 
                 const struct sockaddr *, to, int, tolen);
   VG_(generic_PRE_sys_sendto)(tid, ARG1,ARG2,ARG3,ARG4,ARG5,ARG6);
}

PRE(sys_recvfrom)
{
   *flags |= SfMayBlock;
   PRINT("sys_recvfrom ( %d, %p, %d, %u, %p, %p )",ARG1,ARG2,ARG3,ARG4,ARG5,ARG6);
   PRE_REG_READ6(long, "recvfrom",
                 int, s, void *, buf, int, len, unsigned int, flags,
                 struct sockaddr *, from, int *, fromlen);
   VG_(generic_PRE_sys_recvfrom)(tid, ARG1,ARG2,ARG3,ARG4,ARG5,ARG6);
}
POST(sys_recvfrom)
{
   vg_assert(SUCCESS);
   VG_(generic_POST_sys_recvfrom)(tid, VG_(mk_SysRes_Success)(RES),
                                       ARG1,ARG2,ARG3,ARG4,ARG5,ARG6);
}

PRE(sys_sendmsg)
{
   *flags |= SfMayBlock;
   PRINT("sys_sendmsg ( %d, %p, %d )",ARG1,ARG2,ARG3);
   PRE_REG_READ3(long, "sendmsg",
                 int, s, const struct msghdr *, msg, int, flags);
   VG_(generic_PRE_sys_sendmsg)(tid, ARG1,ARG2);
}

PRE(sys_recvmsg)
{
   *flags |= SfMayBlock;
   PRINT("sys_recvmsg ( %d, %p, %d )",ARG1,ARG2,ARG3);
   PRE_REG_READ3(long, "recvmsg", int, s, struct msghdr *, msg, int, flags);
   VG_(generic_PRE_sys_recvmsg)(tid, ARG1,ARG2);
}
POST(sys_recvmsg)
{
   VG_(generic_POST_sys_recvmsg)(tid, ARG1,ARG2);
}

PRE(sys_shutdown)
{
   *flags |= SfMayBlock;
   PRINT("sys_shutdown ( %d, %d )",ARG1,ARG2);
   PRE_REG_READ2(int, "shutdown", int, s, int, how);
}

PRE(sys_bind)
{
   PRINT("sys_bind ( %d, %p, %d )",ARG1,ARG2,ARG3);
   PRE_REG_READ3(long, "bind",
                 int, sockfd, struct sockaddr *, my_addr, int, addrlen);
   VG_(generic_PRE_sys_bind)(tid, ARG1,ARG2,ARG3);
}

PRE(sys_listen)
{
   PRINT("sys_listen ( %d, %d )",ARG1,ARG2);
   PRE_REG_READ2(long, "listen", int, s, int, backlog);
}

PRE(sys_getsockname)
{
   PRINT("sys_getsockname ( %d, %p, %p )",ARG1,ARG2,ARG3);
   PRE_REG_READ3(long, "getsockname",
                 int, s, struct sockaddr *, name, int *, namelen);
   VG_(generic_PRE_sys_getsockname)(tid, ARG1,ARG2,ARG3);
}
POST(sys_getsockname)
{
   vg_assert(SUCCESS);
   VG_(generic_POST_sys_getsockname)(tid, VG_(mk_SysRes_Success)(RES),
                                          ARG1,ARG2,ARG3);
}

PRE(sys_getpeername)
{
   PRINT("sys_getpeername ( %d, %p, %p )",ARG1,ARG2,ARG3);
   PRE_REG_READ3(long, "getpeername",
                 int, s, struct sockaddr *, name, int *, namelen);
   VG_(generic_PRE_sys_getpeername)(tid, ARG1,ARG2,ARG3);
}
POST(sys_getpeername)
{
   vg_assert(SUCCESS);
   VG_(generic_POST_sys_getpeername)(tid, VG_(mk_SysRes_Success)(RES),
                                          ARG1,ARG2,ARG3);
}

PRE(sys_socketpair)
{
   PRINT("sys_socketpair ( %d, %d, %d, %p )",ARG1,ARG2,ARG3,ARG4);
   PRE_REG_READ4(long, "socketpair",
                 int, d, int, type, int, protocol, int [2], sv);
   VG_(generic_PRE_sys_socketpair)(tid, ARG1,ARG2,ARG3,ARG4);
}
POST(sys_socketpair)
{
   vg_assert(SUCCESS);
   VG_(generic_POST_sys_socketpair)(tid, VG_(mk_SysRes_Success)(RES),
                                         ARG1,ARG2,ARG3,ARG4);
}

PRE(sys_semget)
{
   PRINT("sys_semget ( %d, %d, %d )",ARG1,ARG2,ARG3);
   PRE_REG_READ3(long, "semget", vki_key_t, key, int, nsems, int, semflg);
}

PRE(sys_semop)
{
   *flags |= SfMayBlock;
   PRINT("sys_semop ( %d, %p, %u )",ARG1,ARG2,ARG3);
   PRE_REG_READ3(long, "semop",
                 int, semid, struct sembuf *, sops, unsigned, nsoops);
   VG_(generic_PRE_sys_semop)(tid, ARG1,ARG2,ARG3);
}

PRE(sys_semtimedop)
{
   *flags |= SfMayBlock;
   PRINT("sys_semtimedop ( %d, %p, %u, %p )",ARG1,ARG2,ARG3,ARG4);
   PRE_REG_READ4(long, "semtimedop",
                 int, semid, struct sembuf *, sops, unsigned, nsoops,
                 struct timespec *, timeout);
   VG_(generic_PRE_sys_semtimedop)(tid, ARG1,ARG2,ARG3,ARG4);
}

PRE(sys_semctl)
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

PRE(sys_msgget)
{
   PRINT("sys_msgget ( %d, %d )",ARG1,ARG2);
   PRE_REG_READ2(long, "msgget", vki_key_t, key, int, msgflg);
}

PRE(sys_msgsnd)
{
   PRINT("sys_msgsnd ( %d, %p, %d, %d )",ARG1,ARG2,ARG3,ARG4);
   PRE_REG_READ4(long, "msgsnd",
                 int, msqid, struct msgbuf *, msgp, vki_size_t, msgsz, int, msgflg);
   VG_(generic_PRE_sys_msgsnd)(tid, ARG1,ARG2,ARG3,ARG4);
   if ((ARG4 & VKI_IPC_NOWAIT) == 0)
      *flags |= SfMayBlock;
}

PRE(sys_msgrcv)
{
   PRINT("sys_msgrcv ( %d, %p, %d, %d, %d )",ARG1,ARG2,ARG3,ARG4,ARG5);
   PRE_REG_READ5(long, "msgrcv",
                 int, msqid, struct msgbuf *, msgp, vki_size_t, msgsz,
                 long, msgytp, int, msgflg);
   VG_(generic_PRE_sys_msgrcv)(tid, ARG1,ARG2,ARG3,ARG4,ARG5);
   if ((ARG4 & VKI_IPC_NOWAIT) == 0)
      *flags |= SfMayBlock;
}
POST(sys_msgrcv)
{
   VG_(generic_POST_sys_msgrcv)(tid, RES,ARG1,ARG2,ARG3,ARG4,ARG5);
}

PRE(sys_msgctl)
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

PRE(sys_shmget)
{
   PRINT("sys_shmget ( %d, %d, %d )",ARG1,ARG2,ARG3);
   PRE_REG_READ3(long, "shmget", vki_key_t, key, vki_size_t, size, int, shmflg);
}

PRE(wrap_sys_shmat)
{
   UWord arg2tmp;
   PRINT("wrap_sys_shmat ( %d, %p, %d )",ARG1,ARG2,ARG3);
   PRE_REG_READ3(long, "shmat",
                 int, shmid, const void *, shmaddr, int, shmflg);
   arg2tmp = VG_(generic_PRE_sys_shmat)(tid, ARG1,ARG2,ARG3);
   if (arg2tmp == 0)
      SET_STATUS_Failure( VKI_EINVAL );
   else
      ARG2 = arg2tmp;
}
POST(wrap_sys_shmat)
{
   VG_(generic_POST_sys_shmat)(tid, RES,ARG1,ARG2,ARG3);
}

PRE(sys_shmdt)
{
   PRINT("sys_shmdt ( %p )",ARG1);
   PRE_REG_READ1(long, "shmdt", const void *, shmaddr);
   if (!VG_(generic_PRE_sys_shmdt)(tid, ARG1))
      SET_STATUS_Failure( VKI_EINVAL );
}
POST(sys_shmdt)
{
   VG_(generic_POST_sys_shmdt)(tid, RES,ARG1);
}

PRE(sys_shmctl)
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

/* Add an amd64-linux specific wrapper to a syscall table. */
#define PLAX_(const, name)    WRAPPER_ENTRY_X_(amd64_linux, const, name) 
#define PLAXY(const, name)    WRAPPER_ENTRY_XY(amd64_linux, const, name) 

// This table maps from __NR_xxx syscall numbers (from
// linux/include/asm-x86_64/unistd.h) to the appropriate PRE/POST sys_foo()
// wrappers on AMD64 (as per sys_call_table in
// linux/arch/x86_64/kernel/entry.S).
//
// When implementing these wrappers, you need to work out if the wrapper is
// generic, Linux-only (but arch-independent), or AMD64/Linux only.

const SyscallTableEntry VGP_(syscall_table)[] = {
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
   GENXY(__NR_getitimer,         sys_getitimer),      // 36 
   GENX_(__NR_alarm,             sys_alarm),          // 37 
   GENXY(__NR_setitimer,         sys_setitimer),      // 38 
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
   GENX_(__NR_fsync,             sys_fsync),          // 74 

   GENX_(__NR_fdatasync,         sys_fdatasync),      // 75 
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
   GENX_(__NR_fchmod,            sys_fchmod),         // 91 
//zz    GENX_(__NR_chown,             sys_chown),          // 92 
//zz    GENX_(__NR_fchown,            sys_fchown),         // 93 
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

//zz    GENX_(__NR_setuid,            sys_setuid),         // 105 
//zz    GENX_(__NR_setgid,            sys_setgid),         // 106 
   GENX_(__NR_geteuid,           sys_geteuid),        // 107 
   GENX_(__NR_getegid,           sys_getegid),        // 108 
   GENX_(__NR_setpgid,           sys_setpgid),        // 109 

   GENX_(__NR_getppid,           sys_getppid),        // 110 
   GENX_(__NR_getpgrp,           sys_getpgrp),        // 111 
   GENX_(__NR_setsid,            sys_setsid),         // 112 
   //   (__NR_setreuid,          sys_setreuid),       // 113 
   //   (__NR_setregid,          sys_setregid),       // 114 

   GENXY(__NR_getgroups,         sys_getgroups),      // 115 
//zz    GENX_(__NR_setgroups,         sys_setgroups),      // 116 
//zz    LINX_(__NR_setresuid,         sys_setresuid),      // 117 
   LINXY(__NR_getresuid,         sys_getresuid),      // 118 
//zz    LINX_(__NR_setresgid,         sys_setresgid),      // 119 

   LINXY(__NR_getresgid,         sys_getresgid),      // 120 
   GENX_(__NR_getpgid,           sys_getpgid),        // 121 
   //   (__NR_setfsuid,          sys_setfsuid),       // 122 
   //   (__NR_setfsgid,          sys_setfsgid),       // 123 
   //   (__NR_getsid,            sys_getsid),         // 124 

   //   (__NR_capget,            sys_capget),         // 125 
   //   (__NR_capset,            sys_capset),         // 126 
//zz    GENXY(__NR_rt_sigpending,     sys_rt_sigpending),  // 127 
   GENXY(__NR_rt_sigtimedwait,   sys_rt_sigtimedwait),// 128 
//zz    GENXY(__NR_rt_sigqueueinfo,   sys_rt_sigqueueinfo),// 129 

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
//zz    GENXY(__NR_sched_setparam,          sys_sched_setparam),          // 142 
   GENXY(__NR_sched_getparam,          sys_sched_getparam),          // 143 
   GENX_(__NR_sched_setscheduler,      sys_sched_setscheduler),      // 144 

   GENX_(__NR_sched_getscheduler,      sys_sched_getscheduler),      // 145 
   GENX_(__NR_sched_get_priority_max,  sys_sched_get_priority_max),  // 146 
   GENX_(__NR_sched_get_priority_min,  sys_sched_get_priority_min),  // 147 
   //   (__NR_sched_rr_get_interval,   sys_sched_rr_get_interval),   // 148 
   GENX_(__NR_mlock,                   sys_mlock),                   // 149 

   GENX_(__NR_munlock,           sys_munlock),        // 150 
   GENX_(__NR_mlockall,          sys_mlockall),       // 151 
   GENX_(__NR_munlockall,        sys_munlockall),     // 152 
   //   (__NR_vhangup,           sys_vhangup),        // 153 
   //   (__NR_modify_ldt,        sys_modify_ldt),     // 154 

   //   (__NR_pivot_root,        sys_pivot_root),     // 155 
   LINXY(__NR__sysctl,           sys_sysctl),         // 156 
   //   (__NR_prctl,             sys_prctl),          // 157 
   PLAX_(__NR_arch_prctl,	 sys_arch_prctl),     // 158 
   //   (__NR_adjtimex,          sys_adjtimex),       // 159 

   GENX_(__NR_setrlimit,         sys_setrlimit),      // 160 
   GENX_(__NR_chroot,            sys_chroot),         // 161 
   GENX_(__NR_sync,              sys_sync),           // 162 
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
//zz    LINX_(__NR_io_setup,          sys_io_setup),       // 206 
//zz    LINX_(__NR_io_destroy,        sys_io_destroy),     // 207 
//zz    LINXY(__NR_io_getevents,      sys_io_getevents),   // 208 
//zz    LINX_(__NR_io_submit,         sys_io_submit),      // 209 

//zz    LINXY(__NR_io_cancel,         sys_io_cancel),      // 210 
   //   (__NR_get_thread_area,   sys_ni_syscall),     // 211 
   //   (__NR_lookup_dcookie,    sys_lookup_dcookie), // 212 
//zz    LINXY(__NR_epoll_create,      sys_epoll_create),   // 213 
   //   (__NR_epoll_ctl_old,     sys_ni_syscall),     // 214 

   //   (__NR_epoll_wait_old,    sys_ni_syscall),     // 215 
   //   (__NR_remap_file_pages,  sys_remap_file_pages)// 216 
   GENXY(__NR_getdents64,        sys_getdents64),     // 217 
   LINX_(__NR_set_tid_address,   sys_set_tid_address),// 218 
   //   (__NR_restart_syscall,   sys_restart_syscall),// 219 

   PLAX_(__NR_semtimedop,        sys_semtimedop),     // 220 
   LINX_(__NR_fadvise64,         sys_fadvise64),      // 221 
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
//zz    LINXY(__NR_epoll_wait,        sys_epoll_wait),     // 232 
//zz    LINX_(__NR_epoll_ctl,         sys_epoll_ctl),      // 233 
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

const UInt VGP_(syscall_table_size) = 
            sizeof(VGP_(syscall_table)) / sizeof(VGP_(syscall_table)[0]);

/*--------------------------------------------------------------------*/
/*--- end                                                          ---*/
/*--------------------------------------------------------------------*/
