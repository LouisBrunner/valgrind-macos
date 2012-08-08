
/*--------------------------------------------------------------------*/
/*--- Platform-specific syscalls stuff.      syswrap-amd64-linux.c ---*/
/*--------------------------------------------------------------------*/

/*
   This file is part of Valgrind, a dynamic binary instrumentation
   framework.

   Copyright (C) 2000-2012 Nicholas Nethercote
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

#if defined(VGP_amd64_linux)

#include "pub_core_basics.h"
#include "pub_core_vki.h"
#include "pub_core_vkiscnums.h"
#include "pub_core_libcsetjmp.h"    // to keep _threadstate.h happy
#include "pub_core_threadstate.h"
#include "pub_core_aspacemgr.h"
#include "pub_core_debuglog.h"
#include "pub_core_options.h"
#include "pub_core_libcbase.h"
#include "pub_core_libcassert.h"
#include "pub_core_libcprint.h"
#include "pub_core_libcproc.h"
#include "pub_core_libcsignal.h"
#include "pub_core_scheduler.h"
#include "pub_core_sigframe.h"
#include "pub_core_signals.h"
#include "pub_core_syscall.h"
#include "pub_core_syswrap.h"
#include "pub_core_tooliface.h"
#include "pub_core_stacks.h"        // VG_(register_stack)

#include "priv_types_n_macros.h"
#include "priv_syswrap-generic.h"   /* for decls of generic wrappers */
#include "priv_syswrap-linux.h"     /* for decls of linux-ish wrappers */
#include "priv_syswrap-linux-variants.h" /* decls of linux variant wrappers */
#include "priv_syswrap-main.h"


/* ---------------------------------------------------------------------
   clone() handling
   ------------------------------------------------------------------ */

/* Call f(arg1), but first switch stacks, using 'stack' as the new
   stack, and use 'retaddr' as f's return-to address.  Also, clear all
   the integer registers before entering f.  */
__attribute__((noreturn))
void ML_(call_on_new_stack_0_1) ( Addr stack,
			          Addr retaddr,
			          void (*f)(Word),
                                  Word arg1 );
// %rdi == stack
// %rsi == retaddr
// %rdx == f
// %rcx == arg1
asm(
".text\n"
".globl vgModuleLocal_call_on_new_stack_0_1\n"
"vgModuleLocal_call_on_new_stack_0_1:\n"
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
".previous\n"
);

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
#define __NR_CLONE        VG_STRINGIFY(__NR_clone)
#define __NR_EXIT         VG_STRINGIFY(__NR_exit)

extern
Long do_syscall_clone_amd64_linux ( Word (*fn)(void *), 
                                    void* stack, 
                                    Long  flags, 
                                    void* arg,
                                    Long* child_tid, 
                                    Long* parent_tid, 
                                    vki_modify_ldt_t * );
asm(
".text\n"
".globl do_syscall_clone_amd64_linux\n"
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
".previous\n"
);

#undef __NR_CLONE
#undef __NR_EXIT


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
   NSegment const* seg;
   SysRes       res;
   Long         rax;
   vki_sigset_t blockall, savedmask;

   VG_(sigfillset)(&blockall);

   vg_assert(VG_(is_running_thread)(ptid));
   vg_assert(VG_(is_valid_tid)(ctid));

   stack = (UWord*)ML_(allocstack)(ctid);
   if (stack == NULL) {
      res = VG_(mk_SysRes_Error)( VKI_ENOMEM );
      goto out;
   }

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

   /* Start the child with its threadgroup being the same as the
      parent's.  This is so that any exit_group calls that happen
      after the child is created but before it sets its
      os_state.threadgroup field for real (in thread_wrapper in
      syswrap-linux.c), really kill the new thread.  a.k.a this avoids
      a race condition in which the thread is unkillable (via
      exit_group) because its threadgroup is not set.  The race window
      is probably only a few hundred or a few thousand cycles long.
      See #226116. */
   ctst->os_state.threadgroup = ptst->os_state.threadgroup;

   /* We don't really know where the client stack is, because its
      allocated by the client.  The best we can do is look at the
      memory mappings and try to derive some useful information.  We
      assume that esp starts near its highest possible value, and can
      only go down to the start of the mmaped segment. */
   seg = VG_(am_find_nsegment)((Addr)rsp);
   if (seg && seg->kind != SkResvn) {
      ctst->client_stack_highest_word = (Addr)VG_PGROUNDUP(rsp);
      ctst->client_stack_szB = ctst->client_stack_highest_word - seg->start;

      VG_(register_stack)(seg->start, ctst->client_stack_highest_word);

      if (debug)
	 VG_(printf)("tid %d: guessed client stack range %#lx-%#lx\n",
		     ctid, seg->start, VG_PGROUNDUP(rsp));
   } else {
      VG_(message)(Vg_UserMsg,
                   "!? New thread %d starts with RSP(%#lx) unmapped\n",
		   ctid, rsp);
      ctst->client_stack_szB  = 0;
   }

   /* Assume the clone will succeed, and tell any tool that wants to
      know that this thread has come into existence.  If the clone
      fails, we'll send out a ll_exit notification for it at the out:
      label below, to clean up. */
   vg_assert(VG_(owns_BigLock_LL)(ptid));
   VG_TRACK ( pre_thread_ll_create, ptid, ctid );

   if (flags & VKI_CLONE_SETTLS) {
      if (debug)
	 VG_(printf)("clone child has SETTLS: tls at %#lx\n", tlsaddr);
      ctst->arch.vex.guest_FS_ZERO = tlsaddr;
   }

   flags &= ~VKI_CLONE_SETTLS;

   /* start the thread with everything blocked */
   VG_(sigprocmask)(VKI_SIG_SETMASK, &blockall, &savedmask);

   /* Create the new thread */
   rax = do_syscall_clone_amd64_linux(
            ML_(start_thread_NORETURN), stack, flags, &VG_(threads)[ctid],
            child_tidptr, parent_tidptr, NULL
         );
   res = VG_(mk_SysRes_amd64_linux)( rax );

   VG_(sigprocmask)(VKI_SIG_SETMASK, &savedmask, NULL);

  out:
   if (sr_isError(res)) {
      /* clone failed */
      VG_(cleanup_thread)(&ctst->arch);
      ctst->status = VgTs_Empty;
      /* oops.  Better tell the tool the thread exited in a hurry :-) */
      VG_TRACK( pre_thread_ll_exit, ctid );
   }

   return res;
}


/* ---------------------------------------------------------------------
   More thread stuff
   ------------------------------------------------------------------ */

void VG_(cleanup_thread) ( ThreadArchState *arch )
{  
}  

void setup_child ( /*OUT*/ ThreadArchState *child, 
                   /*IN*/  ThreadArchState *parent )
{  
   /* We inherit our parent's guest state. */
   child->vex = parent->vex;
   child->vex_shadow1 = parent->vex_shadow1;
   child->vex_shadow2 = parent->vex_shadow2;
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
DECL_TEMPLATE(amd64_linux, sys_accept4);
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
DECL_TEMPLATE(amd64_linux, sys_ptrace);
DECL_TEMPLATE(amd64_linux, sys_fadvise64);
DECL_TEMPLATE(amd64_linux, sys_mmap);
DECL_TEMPLATE(amd64_linux, sys_syscall184);


PRE(sys_clone)
{
   ULong cloneflags;

   PRINT("sys_clone ( %lx, %#lx, %#lx, %#lx, %#lx )",ARG1,ARG2,ARG3,ARG4,ARG5);
   PRE_REG_READ2(int, "clone",
                 unsigned long, flags,
                 void *, child_stack);

   if (ARG1 & VKI_CLONE_PARENT_SETTID) {
      if (VG_(tdict).track_pre_reg_read) {
         PRA3("clone", int *, parent_tidptr);
      }
      PRE_MEM_WRITE("clone(parent_tidptr)", ARG3, sizeof(Int));
      if (!VG_(am_is_valid_for_client)(ARG3, sizeof(Int), VKI_PROT_WRITE)) {
         SET_STATUS_Failure( VKI_EFAULT );
         return;
      }
   }
   if (ARG1 & VKI_CLONE_SETTLS) {
      if (VG_(tdict).track_pre_reg_read) {
         PRA4("clone", vki_modify_ldt_t *, tlsinfo);
      }
      PRE_MEM_READ("clone(tlsinfo)", ARG4, sizeof(vki_modify_ldt_t));
      if (!VG_(am_is_valid_for_client)(ARG4, sizeof(vki_modify_ldt_t), 
                                             VKI_PROT_READ)) {
         SET_STATUS_Failure( VKI_EFAULT );
         return;
      }
   }
   if (ARG1 & (VKI_CLONE_CHILD_SETTID | VKI_CLONE_CHILD_CLEARTID)) {
      if (VG_(tdict).track_pre_reg_read) {
         PRA5("clone", int *, child_tidptr);
      }
      PRE_MEM_WRITE("clone(child_tidptr)", ARG4, sizeof(Int));
      if (!VG_(am_is_valid_for_client)(ARG4, sizeof(Int), VKI_PROT_WRITE)) {
         SET_STATUS_Failure( VKI_EFAULT );
         return;
      }
   }

   cloneflags = ARG1;

   if (!ML_(client_signal_OK)(ARG1 & VKI_CSIGNAL)) {
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
         ML_(do_fork_clone)(tid,
                       cloneflags,      /* flags */
                       (Int *)ARG3,     /* parent_tidptr */
                       (Int *)ARG4));   /* child_tidptr */
      break;

   default:
      /* should we just ENOSYS? */
      VG_(message)(Vg_UserMsg,
                   "Unsupported clone() flags: 0x%lx\n", ARG1);
      VG_(message)(Vg_UserMsg,
                   "\n");
      VG_(message)(Vg_UserMsg,
                   "The only supported clone() uses are:\n");
      VG_(message)(Vg_UserMsg,
                   " - via a threads library (LinuxThreads or NPTL)\n");
      VG_(message)(Vg_UserMsg,
                   " - via the implementation of fork or vfork\n");
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
   /* This isn't really a syscall at all - it's a misuse of the
      syscall mechanism by m_sigframe.  VG_(sigframe_create) sets the
      return address of the signal frames it creates to be a short
      piece of code which does this "syscall".  The only purpose of
      the syscall is to call VG_(sigframe_destroy), which restores the
      thread's registers from the frame and then removes it.
      Consequently we must ask the syswrap driver logic not to write
      back the syscall "result" as that would overwrite the
      just-restored register state. */

   ThreadState* tst;
   PRINT("sys_rt_sigreturn ( )");

   vg_assert(VG_(is_valid_tid)(tid));
   vg_assert(tid >= 1 && tid < VG_N_THREADS);
   vg_assert(VG_(is_running_thread)(tid));

   /* Adjust RSP to point to start of frame; skip back up over handler
      ret addr */
   tst = VG_(get_ThreadState)(tid);
   tst->arch.vex.guest_RSP -= sizeof(Addr);

   /* This is only so that the RIP is (might be) useful to report if
      something goes wrong in the sigreturn.  JRS 20070318: no idea
      what this is for */
   ML_(fixup_guest_state_to_restart_syscall)(&tst->arch);

   /* Restore register state from frame and remove it, as 
      described above */
   VG_(sigframe_destroy)(tid, True);

   /* Tell the driver not to update the guest state with the "result",
      and set a bogus result to keep it happy. */
   *flags |= SfNoWriteResult;
   SET_STATUS_Success(0);

   /* Check to see if any signals arose as a result of this. */
   *flags |= SfPollAfter;
}

PRE(sys_arch_prctl)
{
   ThreadState* tst;
   PRINT( "arch_prctl ( %ld, %lx )", ARG1, ARG2 );

   vg_assert(VG_(is_valid_tid)(tid));
   vg_assert(tid >= 1 && tid < VG_N_THREADS);
   vg_assert(VG_(is_running_thread)(tid));

   // Nb: can't use "ARG2".."ARG5" here because that's our own macro...
   PRE_REG_READ2(long, "arch_prctl",
                 int, option, unsigned long, arg2);
   // XXX: totally wrong... we need to look at the 'option' arg, and do
   // PRE_MEM_READs/PRE_MEM_WRITEs as necessary...

   /* "do" the syscall ourselves; the kernel never sees it */
   if (ARG1 == VKI_ARCH_SET_FS) {
      tst = VG_(get_ThreadState)(tid);
      tst->arch.vex.guest_FS_ZERO = ARG2;
   }
   else if (ARG1 == VKI_ARCH_GET_FS) {
      PRE_MEM_WRITE("arch_prctl(addr)", ARG2, sizeof(unsigned long));
      tst = VG_(get_ThreadState)(tid);
      *(unsigned long *)ARG2 = tst->arch.vex.guest_FS_ZERO;
      POST_MEM_WRITE(ARG2, sizeof(unsigned long));
   }
   else {
      VG_(core_panic)("Unsupported arch_prtctl option");
   }

   /* Note; the Status writeback to guest state that happens after
      this wrapper returns does not change guest_FS_ZERO; hence that
      direct assignment to the guest state is safe here. */
   SET_STATUS_Success( 0 );
}

// Parts of this are amd64-specific, but the *PEEK* cases are generic.
//
// ARG3 is only used for pointers into the traced process's address
// space and for offsets into the traced process's struct
// user_regs_struct. It is never a pointer into this process's memory
// space, and we should therefore not check anything it points to.
PRE(sys_ptrace)
{
   PRINT("sys_ptrace ( %ld, %ld, %#lx, %#lx )", ARG1,ARG2,ARG3,ARG4);
   PRE_REG_READ4(int, "ptrace", 
                 long, request, long, pid, long, addr, long, data);
   switch (ARG1) {
   case VKI_PTRACE_PEEKTEXT:
   case VKI_PTRACE_PEEKDATA:
   case VKI_PTRACE_PEEKUSR:
      PRE_MEM_WRITE( "ptrace(peek)", ARG4, 
		     sizeof (long));
      break;
   case VKI_PTRACE_GETREGS:
      PRE_MEM_WRITE( "ptrace(getregs)", ARG4, 
		     sizeof (struct vki_user_regs_struct));
      break;
   case VKI_PTRACE_GETFPREGS:
      PRE_MEM_WRITE( "ptrace(getfpregs)", ARG4, 
		     sizeof (struct vki_user_i387_struct));
      break;
   case VKI_PTRACE_SETREGS:
      PRE_MEM_READ( "ptrace(setregs)", ARG4, 
		     sizeof (struct vki_user_regs_struct));
      break;
   case VKI_PTRACE_SETFPREGS:
      PRE_MEM_READ( "ptrace(setfpregs)", ARG4, 
		     sizeof (struct vki_user_i387_struct));
      break;
   case VKI_PTRACE_GETEVENTMSG:
      PRE_MEM_WRITE( "ptrace(geteventmsg)", ARG4, sizeof(unsigned long));
      break;
   case VKI_PTRACE_GETSIGINFO:
      PRE_MEM_WRITE( "ptrace(getsiginfo)", ARG4, sizeof(vki_siginfo_t));
      break;
   case VKI_PTRACE_SETSIGINFO:
      PRE_MEM_READ( "ptrace(setsiginfo)", ARG4, sizeof(vki_siginfo_t));
      break;
   default:
      break;
   }
}

POST(sys_ptrace)
{
   switch (ARG1) {
   case VKI_PTRACE_PEEKTEXT:
   case VKI_PTRACE_PEEKDATA:
   case VKI_PTRACE_PEEKUSR:
      POST_MEM_WRITE( ARG4, sizeof (long));
      break;
   case VKI_PTRACE_GETREGS:
      POST_MEM_WRITE( ARG4, sizeof (struct vki_user_regs_struct));
      break;
   case VKI_PTRACE_GETFPREGS:
      POST_MEM_WRITE( ARG4, sizeof (struct vki_user_i387_struct));
      break;
   case VKI_PTRACE_GETEVENTMSG:
      POST_MEM_WRITE( ARG4, sizeof(unsigned long));
      break;
   case VKI_PTRACE_GETSIGINFO:
      /* XXX: This is a simplification. Different parts of the
       * siginfo_t are valid depending on the type of signal.
       */
      POST_MEM_WRITE( ARG4, sizeof(vki_siginfo_t));
      break;
   default:
      break;
   }
}

PRE(sys_socket)
{
   PRINT("sys_socket ( %ld, %ld, %ld )",ARG1,ARG2,ARG3);
   PRE_REG_READ3(long, "socket", int, domain, int, type, int, protocol);
}
POST(sys_socket)
{
   SysRes r;
   vg_assert(SUCCESS);
   r = ML_(generic_POST_sys_socket)(tid, VG_(mk_SysRes_Success)(RES));
   SET_STATUS_from_SysRes(r);
}

PRE(sys_setsockopt)
{
   PRINT("sys_setsockopt ( %ld, %ld, %ld, %#lx, %ld )",ARG1,ARG2,ARG3,ARG4,ARG5);
   PRE_REG_READ5(long, "setsockopt",
                 int, s, int, level, int, optname,
                 const void *, optval, int, optlen);
   ML_(generic_PRE_sys_setsockopt)(tid, ARG1,ARG2,ARG3,ARG4,ARG5);
}

PRE(sys_getsockopt)
{
   PRINT("sys_getsockopt ( %ld, %ld, %ld, %#lx, %#lx )",ARG1,ARG2,ARG3,ARG4,ARG5);
   PRE_REG_READ5(long, "getsockopt",
                 int, s, int, level, int, optname,
                 void *, optval, int, *optlen);
   ML_(linux_PRE_sys_getsockopt)(tid, ARG1,ARG2,ARG3,ARG4,ARG5);
}
POST(sys_getsockopt)
{
   vg_assert(SUCCESS);
   ML_(linux_POST_sys_getsockopt)(tid, VG_(mk_SysRes_Success)(RES),
                                       ARG1,ARG2,ARG3,ARG4,ARG5);
}

PRE(sys_connect)
{
   *flags |= SfMayBlock;
   PRINT("sys_connect ( %ld, %#lx, %ld )",ARG1,ARG2,ARG3);
   PRE_REG_READ3(long, "connect",
                 int, sockfd, struct sockaddr *, serv_addr, int, addrlen);
   ML_(generic_PRE_sys_connect)(tid, ARG1,ARG2,ARG3);
}

PRE(sys_accept)
{
   *flags |= SfMayBlock;
   PRINT("sys_accept ( %ld, %#lx, %ld )",ARG1,ARG2,ARG3);
   PRE_REG_READ3(long, "accept",
                 int, s, struct sockaddr *, addr, int, *addrlen);
   ML_(generic_PRE_sys_accept)(tid, ARG1,ARG2,ARG3);
}
POST(sys_accept)
{
   SysRes r;
   vg_assert(SUCCESS);
   r = ML_(generic_POST_sys_accept)(tid, VG_(mk_SysRes_Success)(RES),
                                         ARG1,ARG2,ARG3);
   SET_STATUS_from_SysRes(r);
}

PRE(sys_accept4)
{
   *flags |= SfMayBlock;
   PRINT("sys_accept4 ( %ld, %#lx, %ld, %ld )",ARG1,ARG2,ARG3,ARG4);
   PRE_REG_READ4(long, "accept4",
                 int, s, struct sockaddr *, addr, int, *addrlen, int, flags);
   ML_(generic_PRE_sys_accept)(tid, ARG1,ARG2,ARG3);
}
POST(sys_accept4)
{
   SysRes r;
   vg_assert(SUCCESS);
   r = ML_(generic_POST_sys_accept)(tid, VG_(mk_SysRes_Success)(RES),
                                         ARG1,ARG2,ARG3);
   SET_STATUS_from_SysRes(r);
}

PRE(sys_sendto)
{
   *flags |= SfMayBlock;
   PRINT("sys_sendto ( %ld, %#lx, %ld, %lu, %#lx, %ld )",ARG1,ARG2,ARG3,ARG4,ARG5,ARG6);
   PRE_REG_READ6(long, "sendto",
                 int, s, const void *, msg, int, len, 
                 unsigned int, flags, 
                 const struct sockaddr *, to, int, tolen);
   ML_(generic_PRE_sys_sendto)(tid, ARG1,ARG2,ARG3,ARG4,ARG5,ARG6);
}

PRE(sys_recvfrom)
{
   *flags |= SfMayBlock;
   PRINT("sys_recvfrom ( %ld, %#lx, %ld, %lu, %#lx, %#lx )",ARG1,ARG2,ARG3,ARG4,ARG5,ARG6);
   PRE_REG_READ6(long, "recvfrom",
                 int, s, void *, buf, int, len, unsigned int, flags,
                 struct sockaddr *, from, int *, fromlen);
   ML_(generic_PRE_sys_recvfrom)(tid, ARG1,ARG2,ARG3,ARG4,ARG5,ARG6);
}
POST(sys_recvfrom)
{
   vg_assert(SUCCESS);
   ML_(generic_POST_sys_recvfrom)(tid, VG_(mk_SysRes_Success)(RES),
                                       ARG1,ARG2,ARG3,ARG4,ARG5,ARG6);
}

PRE(sys_sendmsg)
{
   *flags |= SfMayBlock;
   PRINT("sys_sendmsg ( %ld, %#lx, %ld )",ARG1,ARG2,ARG3);
   PRE_REG_READ3(long, "sendmsg",
                 int, s, const struct msghdr *, msg, int, flags);
   ML_(generic_PRE_sys_sendmsg)(tid, "msg", (struct vki_msghdr *)ARG2);
}

PRE(sys_recvmsg)
{
   *flags |= SfMayBlock;
   PRINT("sys_recvmsg ( %ld, %#lx, %ld )",ARG1,ARG2,ARG3);
   PRE_REG_READ3(long, "recvmsg", int, s, struct msghdr *, msg, int, flags);
   ML_(generic_PRE_sys_recvmsg)(tid, "msg", (struct vki_msghdr *)ARG2);
}
POST(sys_recvmsg)
{
   ML_(generic_POST_sys_recvmsg)(tid, "msg", (struct vki_msghdr *)ARG2, RES);
}

PRE(sys_shutdown)
{
   *flags |= SfMayBlock;
   PRINT("sys_shutdown ( %ld, %ld )",ARG1,ARG2);
   PRE_REG_READ2(int, "shutdown", int, s, int, how);
}

PRE(sys_bind)
{
   PRINT("sys_bind ( %ld, %#lx, %ld )",ARG1,ARG2,ARG3);
   PRE_REG_READ3(long, "bind",
                 int, sockfd, struct sockaddr *, my_addr, int, addrlen);
   ML_(generic_PRE_sys_bind)(tid, ARG1,ARG2,ARG3);
}

PRE(sys_listen)
{
   PRINT("sys_listen ( %ld, %ld )",ARG1,ARG2);
   PRE_REG_READ2(long, "listen", int, s, int, backlog);
}

PRE(sys_getsockname)
{
   PRINT("sys_getsockname ( %ld, %#lx, %#lx )",ARG1,ARG2,ARG3);
   PRE_REG_READ3(long, "getsockname",
                 int, s, struct sockaddr *, name, int *, namelen);
   ML_(generic_PRE_sys_getsockname)(tid, ARG1,ARG2,ARG3);
}
POST(sys_getsockname)
{
   vg_assert(SUCCESS);
   ML_(generic_POST_sys_getsockname)(tid, VG_(mk_SysRes_Success)(RES),
                                          ARG1,ARG2,ARG3);
}

PRE(sys_getpeername)
{
   PRINT("sys_getpeername ( %ld, %#lx, %#lx )",ARG1,ARG2,ARG3);
   PRE_REG_READ3(long, "getpeername",
                 int, s, struct sockaddr *, name, int *, namelen);
   ML_(generic_PRE_sys_getpeername)(tid, ARG1,ARG2,ARG3);
}
POST(sys_getpeername)
{
   vg_assert(SUCCESS);
   ML_(generic_POST_sys_getpeername)(tid, VG_(mk_SysRes_Success)(RES),
                                          ARG1,ARG2,ARG3);
}

PRE(sys_socketpair)
{
   PRINT("sys_socketpair ( %ld, %ld, %ld, %#lx )",ARG1,ARG2,ARG3,ARG4);
   PRE_REG_READ4(long, "socketpair",
                 int, d, int, type, int, protocol, int*, sv);
   ML_(generic_PRE_sys_socketpair)(tid, ARG1,ARG2,ARG3,ARG4);
}
POST(sys_socketpair)
{
   vg_assert(SUCCESS);
   ML_(generic_POST_sys_socketpair)(tid, VG_(mk_SysRes_Success)(RES),
                                         ARG1,ARG2,ARG3,ARG4);
}

PRE(sys_semget)
{
   PRINT("sys_semget ( %ld, %ld, %ld )",ARG1,ARG2,ARG3);
   PRE_REG_READ3(long, "semget", vki_key_t, key, int, nsems, int, semflg);
}

PRE(sys_semop)
{
   *flags |= SfMayBlock;
   PRINT("sys_semop ( %ld, %#lx, %lu )",ARG1,ARG2,ARG3);
   PRE_REG_READ3(long, "semop",
                 int, semid, struct sembuf *, sops, unsigned, nsoops);
   ML_(generic_PRE_sys_semop)(tid, ARG1,ARG2,ARG3);
}

PRE(sys_semtimedop)
{
   *flags |= SfMayBlock;
   PRINT("sys_semtimedop ( %ld, %#lx, %lu, %#lx )",ARG1,ARG2,ARG3,ARG4);
   PRE_REG_READ4(long, "semtimedop",
                 int, semid, struct sembuf *, sops, unsigned, nsoops,
                 struct timespec *, timeout);
   ML_(generic_PRE_sys_semtimedop)(tid, ARG1,ARG2,ARG3,ARG4);
}

PRE(sys_semctl)
{
   switch (ARG3 & ~VKI_IPC_64) {
   case VKI_IPC_INFO:
   case VKI_SEM_INFO:
      PRINT("sys_semctl ( %ld, %ld, %ld, %#lx )",ARG1,ARG2,ARG3,ARG4);
      PRE_REG_READ4(long, "semctl",
                    int, semid, int, semnum, int, cmd, struct seminfo *, arg);
      break;
   case VKI_IPC_STAT:
   case VKI_SEM_STAT:
   case VKI_IPC_SET:
      PRINT("sys_semctl ( %ld, %ld, %ld, %#lx )",ARG1,ARG2,ARG3,ARG4);
      PRE_REG_READ4(long, "semctl",
                    int, semid, int, semnum, int, cmd, struct semid_ds *, arg);
      break;
   case VKI_GETALL:
   case VKI_SETALL:
      PRINT("sys_semctl ( %ld, %ld, %ld, %#lx )",ARG1,ARG2,ARG3,ARG4);
      PRE_REG_READ4(long, "semctl",
                    int, semid, int, semnum, int, cmd, unsigned short *, arg);
      break;
   default:
      PRINT("sys_semctl ( %ld, %ld, %ld )",ARG1,ARG2,ARG3);
      PRE_REG_READ3(long, "semctl",
                    int, semid, int, semnum, int, cmd);
      break;
   }
   ML_(generic_PRE_sys_semctl)(tid, ARG1,ARG2,ARG3|VKI_IPC_64,ARG4);
}
POST(sys_semctl)
{
   ML_(generic_POST_sys_semctl)(tid, RES,ARG1,ARG2,ARG3|VKI_IPC_64,ARG4);
}

PRE(sys_msgget)
{
   PRINT("sys_msgget ( %ld, %ld )",ARG1,ARG2);
   PRE_REG_READ2(long, "msgget", vki_key_t, key, int, msgflg);
}

PRE(sys_msgsnd)
{
   PRINT("sys_msgsnd ( %ld, %#lx, %ld, %ld )",ARG1,ARG2,ARG3,ARG4);
   PRE_REG_READ4(long, "msgsnd",
                 int, msqid, struct msgbuf *, msgp, vki_size_t, msgsz, int, msgflg);
   ML_(linux_PRE_sys_msgsnd)(tid, ARG1,ARG2,ARG3,ARG4);
   if ((ARG4 & VKI_IPC_NOWAIT) == 0)
      *flags |= SfMayBlock;
}

PRE(sys_msgrcv)
{
   PRINT("sys_msgrcv ( %ld, %#lx, %ld, %ld, %ld )",ARG1,ARG2,ARG3,ARG4,ARG5);
   PRE_REG_READ5(long, "msgrcv",
                 int, msqid, struct msgbuf *, msgp, vki_size_t, msgsz,
                 long, msgytp, int, msgflg);
   ML_(linux_PRE_sys_msgrcv)(tid, ARG1,ARG2,ARG3,ARG4,ARG5);
   if ((ARG4 & VKI_IPC_NOWAIT) == 0)
      *flags |= SfMayBlock;
}
POST(sys_msgrcv)
{
   ML_(linux_POST_sys_msgrcv)(tid, RES,ARG1,ARG2,ARG3,ARG4,ARG5);
}

PRE(sys_msgctl)
{
   PRINT("sys_msgctl ( %ld, %ld, %#lx )",ARG1,ARG2,ARG3);
   PRE_REG_READ3(long, "msgctl",
                 int, msqid, int, cmd, struct msqid_ds *, buf);
   ML_(linux_PRE_sys_msgctl)(tid, ARG1,ARG2,ARG3);
}
POST(sys_msgctl)
{
   ML_(linux_POST_sys_msgctl)(tid, RES,ARG1,ARG2,ARG3);
}

PRE(sys_shmget)
{
   PRINT("sys_shmget ( %ld, %ld, %ld )",ARG1,ARG2,ARG3);
   PRE_REG_READ3(long, "shmget", vki_key_t, key, vki_size_t, size, int, shmflg);
}

PRE(wrap_sys_shmat)
{
   UWord arg2tmp;
   PRINT("wrap_sys_shmat ( %ld, %#lx, %ld )",ARG1,ARG2,ARG3);
   PRE_REG_READ3(long, "shmat",
                 int, shmid, const void *, shmaddr, int, shmflg);
   arg2tmp = ML_(generic_PRE_sys_shmat)(tid, ARG1,ARG2,ARG3);
   if (arg2tmp == 0)
      SET_STATUS_Failure( VKI_EINVAL );
   else
      ARG2 = arg2tmp;  // used in POST
}
POST(wrap_sys_shmat)
{
   ML_(generic_POST_sys_shmat)(tid, RES,ARG1,ARG2,ARG3);
}

PRE(sys_shmdt)
{
   PRINT("sys_shmdt ( %#lx )",ARG1);
   PRE_REG_READ1(long, "shmdt", const void *, shmaddr);
   if (!ML_(generic_PRE_sys_shmdt)(tid, ARG1))
      SET_STATUS_Failure( VKI_EINVAL );
}
POST(sys_shmdt)
{
   ML_(generic_POST_sys_shmdt)(tid, RES,ARG1);
}

PRE(sys_shmctl)
{
   PRINT("sys_shmctl ( %ld, %ld, %#lx )",ARG1,ARG2,ARG3);
   PRE_REG_READ3(long, "shmctl",
                 int, shmid, int, cmd, struct shmid_ds *, buf);
   ML_(generic_PRE_sys_shmctl)(tid, ARG1,ARG2|VKI_IPC_64,ARG3);
}
POST(sys_shmctl)
{
   ML_(generic_POST_sys_shmctl)(tid, RES,ARG1,ARG2|VKI_IPC_64,ARG3);
}

PRE(sys_fadvise64)
{
   PRINT("sys_fadvise64 ( %ld, %ld, %lu, %ld )", ARG1,ARG2,ARG3,ARG4);
   PRE_REG_READ4(long, "fadvise64",
                 int, fd, vki_loff_t, offset, vki_size_t, len, int, advice);
}

PRE(sys_mmap)
{
   SysRes r;

   PRINT("sys_mmap ( %#lx, %llu, %ld, %ld, %d, %ld )",
         ARG1, (ULong)ARG2, ARG3, ARG4, (Int)ARG5, ARG6 );
   PRE_REG_READ6(long, "mmap",
                 unsigned long, start, unsigned long, length,
                 unsigned long, prot,  unsigned long, flags,
                 unsigned long, fd,    unsigned long, offset);

   r = ML_(generic_PRE_sys_mmap)( tid, ARG1, ARG2, ARG3, ARG4, ARG5, ARG6 );
   SET_STATUS_from_SysRes(r);
}


/* ---------------------------------------------------------------
   PRE/POST wrappers for AMD64/Linux-variant specific syscalls
   ------------------------------------------------------------ */

PRE(sys_syscall184)
{
   Int err;

   /* 184 is used by sys_bproc.  If we're not on a declared bproc
      variant, fail in the usual way, since it is otherwise unused. */

   if (!VG_(strstr)(VG_(clo_kernel_variant), "bproc")) {
      PRINT("non-existent syscall! (syscall 184)");
      PRE_REG_READ0(long, "ni_syscall(184)");
      SET_STATUS_Failure( VKI_ENOSYS );
      return;
   }

   err = ML_(linux_variant_PRE_sys_bproc)( ARG1, ARG2, ARG3, 
                                           ARG4, ARG5, ARG6 );
   if (err) {
      SET_STATUS_Failure( err );
      return;
   }
   /* Let it go through. */
   *flags |= SfMayBlock; /* who knows?  play safe. */
}

POST(sys_syscall184)
{
   ML_(linux_variant_POST_sys_bproc)( ARG1, ARG2, ARG3, 
                                      ARG4, ARG5, ARG6 );
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

static SyscallTableEntry syscall_table[] = {
   GENXY(__NR_read,              sys_read),           // 0 
   GENX_(__NR_write,             sys_write),          // 1 
   GENXY(__NR_open,              sys_open),           // 2 
   GENXY(__NR_close,             sys_close),          // 3 
   GENXY(__NR_stat,              sys_newstat),        // 4 

   GENXY(__NR_fstat,             sys_newfstat),       // 5 
   GENXY(__NR_lstat,             sys_newlstat),       // 6 
   GENXY(__NR_poll,              sys_poll),           // 7 
   LINX_(__NR_lseek,             sys_lseek),          // 8 
   PLAX_(__NR_mmap,              sys_mmap),           // 9 

   GENXY(__NR_mprotect,          sys_mprotect),       // 10 
   GENXY(__NR_munmap,            sys_munmap),         // 11 
   GENX_(__NR_brk,               sys_brk),            // 12 
   LINXY(__NR_rt_sigaction,      sys_rt_sigaction),   // 13 
   LINXY(__NR_rt_sigprocmask,    sys_rt_sigprocmask), // 14 

   PLAX_(__NR_rt_sigreturn,      sys_rt_sigreturn),   // 15 
   LINXY(__NR_ioctl,             sys_ioctl),          // 16 
   GENXY(__NR_pread64,           sys_pread64),        // 17 
   GENX_(__NR_pwrite64,          sys_pwrite64),       // 18 
   GENXY(__NR_readv,             sys_readv),          // 19 

   GENX_(__NR_writev,            sys_writev),         // 20 
   GENX_(__NR_access,            sys_access),         // 21 
   LINXY(__NR_pipe,              sys_pipe),           // 22 
   GENX_(__NR_select,            sys_select),         // 23 
   LINX_(__NR_sched_yield,       sys_sched_yield),    // 24 

   GENX_(__NR_mremap,            sys_mremap),         // 25 
   GENX_(__NR_msync,             sys_msync),          // 26 
   GENXY(__NR_mincore,           sys_mincore),        // 27 
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

   LINXY(__NR_sendfile,          sys_sendfile),       // 40 
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
   LINXY(__NR_fcntl,             sys_fcntl),          // 72 
   GENX_(__NR_flock,             sys_flock),          // 73 
   GENX_(__NR_fsync,             sys_fsync),          // 74 

   GENX_(__NR_fdatasync,         sys_fdatasync),      // 75 
   GENX_(__NR_truncate,          sys_truncate),       // 76 
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
   GENX_(__NR_chown,             sys_chown),          // 92 
   GENX_(__NR_fchown,            sys_fchown),         // 93 
   GENX_(__NR_lchown,            sys_lchown),         // 94 

   GENX_(__NR_umask,             sys_umask),          // 95 
   GENXY(__NR_gettimeofday,      sys_gettimeofday),   // 96 
   GENXY(__NR_getrlimit,         sys_getrlimit),      // 97 
   GENXY(__NR_getrusage,         sys_getrusage),      // 98 
   LINXY(__NR_sysinfo,           sys_sysinfo),        // 99 

   GENXY(__NR_times,             sys_times),          // 100 
   PLAXY(__NR_ptrace,            sys_ptrace),         // 101 
   GENX_(__NR_getuid,            sys_getuid),         // 102 
   LINXY(__NR_syslog,            sys_syslog),         // 103 
   GENX_(__NR_getgid,            sys_getgid),         // 104 

   GENX_(__NR_setuid,            sys_setuid),         // 105 
   GENX_(__NR_setgid,            sys_setgid),         // 106 
   GENX_(__NR_geteuid,           sys_geteuid),        // 107 
   GENX_(__NR_getegid,           sys_getegid),        // 108 
   GENX_(__NR_setpgid,           sys_setpgid),        // 109 

   GENX_(__NR_getppid,           sys_getppid),        // 110 
   GENX_(__NR_getpgrp,           sys_getpgrp),        // 111 
   GENX_(__NR_setsid,            sys_setsid),         // 112 
   GENX_(__NR_setreuid,          sys_setreuid),       // 113 
   GENX_(__NR_setregid,          sys_setregid),       // 114 

   GENXY(__NR_getgroups,         sys_getgroups),      // 115 
   GENX_(__NR_setgroups,         sys_setgroups),      // 116 
   LINX_(__NR_setresuid,         sys_setresuid),      // 117 
   LINXY(__NR_getresuid,         sys_getresuid),      // 118 
   LINX_(__NR_setresgid,         sys_setresgid),      // 119 

   LINXY(__NR_getresgid,         sys_getresgid),      // 120 
   GENX_(__NR_getpgid,           sys_getpgid),        // 121 
   LINX_(__NR_setfsuid,          sys_setfsuid),       // 122 
   LINX_(__NR_setfsgid,          sys_setfsgid),       // 123 
   GENX_(__NR_getsid,            sys_getsid),         // 124 

   LINXY(__NR_capget,            sys_capget),         // 125 
   LINX_(__NR_capset,            sys_capset),         // 126 
   LINXY(__NR_rt_sigpending,     sys_rt_sigpending),  // 127 
   LINXY(__NR_rt_sigtimedwait,   sys_rt_sigtimedwait),// 128 
   LINXY(__NR_rt_sigqueueinfo,   sys_rt_sigqueueinfo),// 129 

   LINX_(__NR_rt_sigsuspend,     sys_rt_sigsuspend),  // 130 
   GENXY(__NR_sigaltstack,       sys_sigaltstack),    // 131 
   LINX_(__NR_utime,             sys_utime),          // 132 
   GENX_(__NR_mknod,             sys_mknod),          // 133 
   //   (__NR_uselib,            sys_uselib),         // 134 

   LINX_(__NR_personality,       sys_personality),    // 135 
   //   (__NR_ustat,             sys_ustat),          // 136 
   GENXY(__NR_statfs,            sys_statfs),         // 137 
   GENXY(__NR_fstatfs,           sys_fstatfs),        // 138 
   //   (__NR_sysfs,             sys_sysfs),          // 139 

   GENX_(__NR_getpriority,             sys_getpriority),             // 140 
   GENX_(__NR_setpriority,             sys_setpriority),             // 141 
   LINXY(__NR_sched_setparam,          sys_sched_setparam),          // 142 
   LINXY(__NR_sched_getparam,          sys_sched_getparam),          // 143 
   LINX_(__NR_sched_setscheduler,      sys_sched_setscheduler),      // 144 

   LINX_(__NR_sched_getscheduler,      sys_sched_getscheduler),      // 145 
   LINX_(__NR_sched_get_priority_max,  sys_sched_get_priority_max),  // 146 
   LINX_(__NR_sched_get_priority_min,  sys_sched_get_priority_min),  // 147 
   LINXY(__NR_sched_rr_get_interval,   sys_sched_rr_get_interval),   // 148 
   GENX_(__NR_mlock,                   sys_mlock),                   // 149 

   GENX_(__NR_munlock,           sys_munlock),        // 150 
   GENX_(__NR_mlockall,          sys_mlockall),       // 151 
   LINX_(__NR_munlockall,        sys_munlockall),     // 152 
   LINX_(__NR_vhangup,           sys_vhangup),        // 153 
   //   (__NR_modify_ldt,        sys_modify_ldt),     // 154 

   //   (__NR_pivot_root,        sys_pivot_root),     // 155 
   LINXY(__NR__sysctl,           sys_sysctl),         // 156 
   LINXY(__NR_prctl,             sys_prctl),          // 157 
   PLAX_(__NR_arch_prctl,	 sys_arch_prctl),     // 158 
   LINXY(__NR_adjtimex,          sys_adjtimex),       // 159 

   GENX_(__NR_setrlimit,         sys_setrlimit),      // 160 
   GENX_(__NR_chroot,            sys_chroot),         // 161 
   GENX_(__NR_sync,              sys_sync),           // 162 
   //   (__NR_acct,              sys_acct),           // 163 
   GENX_(__NR_settimeofday,      sys_settimeofday),   // 164 

   LINX_(__NR_mount,             sys_mount),          // 165
   LINX_(__NR_umount2,           sys_umount),         // 166 
   //   (__NR_swapon,            sys_swapon),         // 167 
   //   (__NR_swapoff,           sys_swapoff),        // 168 
   //   (__NR_reboot,            sys_reboot),         // 169 

   //   (__NR_sethostname,       sys_sethostname),    // 170 
   //   (__NR_setdomainname,     sys_setdomainname),  // 171 
   GENX_(__NR_iopl,              sys_iopl),           // 172 
   LINX_(__NR_ioperm,            sys_ioperm),         // 173 
   GENX_(__NR_create_module,     sys_ni_syscall),     // 174 

   LINX_(__NR_init_module,       sys_init_module),    // 175 
   LINX_(__NR_delete_module,     sys_delete_module),  // 176 
   //   (__NR_get_kernel_syms,   sys_ni_syscall),     // 177 
   //   (__NR_query_module,      sys_ni_syscall),     // 178 
   LINX_(__NR_quotactl,          sys_quotactl),       // 179 

   //   (__NR_nfsservctl,        sys_nfsservctl),     // 180 
   //   (__NR_getpmsg,           sys_ni_syscall),     // 181
   //   (__NR_putpmsg,           sys_ni_syscall),     // 182
   //   (__NR_afs_syscall,       sys_ni_syscall),     // 183 
   PLAXY(184,                    sys_syscall184),     // 184 // sys_bproc?

   //   (__NR_security,          sys_ni_syscall),     // 185 
   LINX_(__NR_gettid,            sys_gettid),         // 186 
   LINX_(__NR_readahead,         sys_readahead),      // 187 
   LINX_(__NR_setxattr,          sys_setxattr),       // 188 
   LINX_(__NR_lsetxattr,         sys_lsetxattr),      // 189 

   LINX_(__NR_fsetxattr,         sys_fsetxattr),      // 190 
   LINXY(__NR_getxattr,          sys_getxattr),       // 191 
   LINXY(__NR_lgetxattr,         sys_lgetxattr),      // 192 
   LINXY(__NR_fgetxattr,         sys_fgetxattr),      // 193 
   LINXY(__NR_listxattr,         sys_listxattr),      // 194 

   LINXY(__NR_llistxattr,        sys_llistxattr),     // 195 
   LINXY(__NR_flistxattr,        sys_flistxattr),     // 196 
   LINX_(__NR_removexattr,       sys_removexattr),    // 197 
   LINX_(__NR_lremovexattr,      sys_lremovexattr),   // 198 
   LINX_(__NR_fremovexattr,      sys_fremovexattr),   // 199 

   LINXY(__NR_tkill,             sys_tkill),             // 200 
   GENXY(__NR_time,              sys_time), /*was sys_time64*/ // 201 
   LINXY(__NR_futex,             sys_futex),             // 202 
   LINX_(__NR_sched_setaffinity, sys_sched_setaffinity), // 203 
   LINXY(__NR_sched_getaffinity, sys_sched_getaffinity), // 204 

   //   (__NR_set_thread_area,   sys_ni_syscall),     // 205 
   LINXY(__NR_io_setup,          sys_io_setup),       // 206 
   LINX_(__NR_io_destroy,        sys_io_destroy),     // 207 
   LINXY(__NR_io_getevents,      sys_io_getevents),   // 208 
   LINX_(__NR_io_submit,         sys_io_submit),      // 209 

   LINXY(__NR_io_cancel,         sys_io_cancel),      // 210 
   //   (__NR_get_thread_area,   sys_ni_syscall),     // 211 
   LINXY(__NR_lookup_dcookie,    sys_lookup_dcookie), // 212 
   LINXY(__NR_epoll_create,      sys_epoll_create),   // 213 
   //   (__NR_epoll_ctl_old,     sys_ni_syscall),     // 214 

   //   (__NR_epoll_wait_old,    sys_ni_syscall),     // 215 
   //   (__NR_remap_file_pages,  sys_remap_file_pages)// 216 
   GENXY(__NR_getdents64,        sys_getdents64),     // 217 
   LINX_(__NR_set_tid_address,   sys_set_tid_address),// 218 
   //   (__NR_restart_syscall,   sys_restart_syscall),// 219 

   PLAX_(__NR_semtimedop,        sys_semtimedop),     // 220 
   PLAX_(__NR_fadvise64,         sys_fadvise64),      // 221 
   LINXY(__NR_timer_create,      sys_timer_create),   // 222 
   LINXY(__NR_timer_settime,     sys_timer_settime),  // 223 
   LINXY(__NR_timer_gettime,     sys_timer_gettime),  // 224 

   LINX_(__NR_timer_getoverrun,  sys_timer_getoverrun), // 225 
   LINX_(__NR_timer_delete,      sys_timer_delete),   // 226 
   LINX_(__NR_clock_settime,     sys_clock_settime),  // 227 
   LINXY(__NR_clock_gettime,     sys_clock_gettime),  // 228 
   LINXY(__NR_clock_getres,      sys_clock_getres),   // 229 

   LINXY(__NR_clock_nanosleep,   sys_clock_nanosleep),// 230 
   LINX_(__NR_exit_group,        sys_exit_group),     // 231 
   LINXY(__NR_epoll_wait,        sys_epoll_wait),     // 232 
   LINX_(__NR_epoll_ctl,         sys_epoll_ctl),      // 233 
   LINXY(__NR_tgkill,            sys_tgkill),         // 234 

   GENX_(__NR_utimes,            sys_utimes),         // 235 
   //   (__NR_vserver,           sys_ni_syscall),     // 236 
   LINX_(__NR_mbind,             sys_mbind),          // 237 
   LINX_(__NR_set_mempolicy,     sys_set_mempolicy),  // 238 
   LINXY(__NR_get_mempolicy,     sys_get_mempolicy),  // 239

   LINXY(__NR_mq_open,           sys_mq_open),        // 240 
   LINX_(__NR_mq_unlink,         sys_mq_unlink),      // 241 
   LINX_(__NR_mq_timedsend,      sys_mq_timedsend),   // 242 
   LINXY(__NR_mq_timedreceive,   sys_mq_timedreceive),// 243 
   LINX_(__NR_mq_notify,         sys_mq_notify),      // 244

   LINXY(__NR_mq_getsetattr,     sys_mq_getsetattr),  // 245 
   //   (__NR_kexec_load,        sys_ni_syscall),     // 246 
   LINXY(__NR_waitid,            sys_waitid),         // 247 
   LINX_(__NR_add_key,           sys_add_key),        // 248
   LINX_(__NR_request_key,       sys_request_key),    // 249

   LINXY(__NR_keyctl,            sys_keyctl),         // 250
   LINX_(__NR_ioprio_set,        sys_ioprio_set),     // 251
   LINX_(__NR_ioprio_get,        sys_ioprio_get),     // 252
   LINX_(__NR_inotify_init,	 sys_inotify_init),   // 253
   LINX_(__NR_inotify_add_watch, sys_inotify_add_watch), // 254

   LINX_(__NR_inotify_rm_watch,	 sys_inotify_rm_watch), // 255
//   LINX_(__NR_migrate_pages,	 sys_migrate_pages),    // 256
   LINXY(__NR_openat,		 sys_openat),           // 257
   LINX_(__NR_mkdirat,		 sys_mkdirat),          // 258
   LINX_(__NR_mknodat,		 sys_mknodat),          // 259

   LINX_(__NR_fchownat,		 sys_fchownat),         // 260
   LINX_(__NR_futimesat,	 sys_futimesat),        // 261
   LINXY(__NR_newfstatat,	 sys_newfstatat),       // 262
   LINX_(__NR_unlinkat,		 sys_unlinkat),         // 263
   LINX_(__NR_renameat,		 sys_renameat),         // 264

   LINX_(__NR_linkat,		 sys_linkat),           // 265
   LINX_(__NR_symlinkat,	 sys_symlinkat),        // 266
   LINX_(__NR_readlinkat,	 sys_readlinkat),       // 267
   LINX_(__NR_fchmodat,		 sys_fchmodat),         // 268
   LINX_(__NR_faccessat,	 sys_faccessat),        // 269

   LINX_(__NR_pselect6,		 sys_pselect6),         // 270
   LINXY(__NR_ppoll,		 sys_ppoll),            // 271
//   LINX_(__NR_unshare,		 sys_unshare),          // 272
   LINX_(__NR_set_robust_list,	 sys_set_robust_list),  // 273
   LINXY(__NR_get_robust_list,	 sys_get_robust_list),  // 274

   LINX_(__NR_splice,            sys_splice),           // 275
   LINX_(__NR_tee,               sys_tee),              // 276
   LINX_(__NR_sync_file_range,   sys_sync_file_range),  // 277
   LINXY(__NR_vmsplice,          sys_vmsplice),         // 278
   LINXY(__NR_move_pages,        sys_move_pages),       // 279

   LINX_(__NR_utimensat,         sys_utimensat),        // 280
   LINXY(__NR_epoll_pwait,       sys_epoll_pwait),      // 281
   LINXY(__NR_signalfd,          sys_signalfd),         // 282
   LINXY(__NR_timerfd_create,    sys_timerfd_create),   // 283
   LINX_(__NR_eventfd,           sys_eventfd),          // 284

   LINX_(__NR_fallocate,         sys_fallocate),        // 285
   LINXY(__NR_timerfd_settime,   sys_timerfd_settime),  // 286
   LINXY(__NR_timerfd_gettime,   sys_timerfd_gettime),  // 287
   PLAXY(__NR_accept4,           sys_accept4),          // 288
   LINXY(__NR_signalfd4,         sys_signalfd4),        // 289

   LINX_(__NR_eventfd2,          sys_eventfd2),         // 290
   LINXY(__NR_epoll_create1,     sys_epoll_create1),    // 291
   LINXY(__NR_dup3,              sys_dup3),             // 292
   LINXY(__NR_pipe2,             sys_pipe2),            // 293
   LINXY(__NR_inotify_init1,     sys_inotify_init1),    // 294

   LINXY(__NR_preadv,            sys_preadv),           // 295
   LINX_(__NR_pwritev,           sys_pwritev),          // 296
   LINXY(__NR_rt_tgsigqueueinfo, sys_rt_tgsigqueueinfo),// 297
   LINXY(__NR_perf_event_open,   sys_perf_event_open),  // 298
   LINXY(__NR_recvmmsg,          sys_recvmmsg),         // 299

//   LINX_(__NR_fanotify_init,     sys_ni_syscall),       // 300
//   LINX_(__NR_fanotify_mark,     sys_ni_syscall),       // 301
   LINXY(__NR_prlimit64,         sys_prlimit64),        // 302
//   LINX_(__NR_name_to_handle_at, sys_ni_syscall),       // 303
//   LINX_(__NR_open_by_handle_at, sys_ni_syscall),       // 304

//   LINX_(__NR_clock_adjtime,     sys_ni_syscall),       // 305
//   LINX_(__NR_syncfs,            sys_ni_syscall),       // 306
   LINXY(__NR_sendmmsg,          sys_sendmmsg),         // 307
//   LINX_(__NR_setns,             sys_ni_syscall),       // 308
   LINXY(__NR_getcpu,            sys_getcpu),           // 309

   LINXY(__NR_process_vm_readv,  sys_process_vm_readv), // 310
   LINX_(__NR_process_vm_writev, sys_process_vm_writev) // 311
};

SyscallTableEntry* ML_(get_linux_syscall_entry) ( UInt sysno )
{
   const UInt syscall_table_size
      = sizeof(syscall_table) / sizeof(syscall_table[0]);

   /* Is it in the contiguous initial section of the table? */
   if (sysno < syscall_table_size) {
      SyscallTableEntry* sys = &syscall_table[sysno];
      if (sys->before == NULL)
         return NULL; /* no entry */
      else
         return sys;
   }

   /* Can't find a wrapper */
   return NULL;
}

#endif // defined(VGP_amd64_linux)

/*--------------------------------------------------------------------*/
/*--- end                                                          ---*/
/*--------------------------------------------------------------------*/
