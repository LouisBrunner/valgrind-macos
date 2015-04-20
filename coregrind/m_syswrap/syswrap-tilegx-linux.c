
/*--------------------------------------------------------------------*/
/*--- Platform-specific syscalls stuff.    syswrap-tilegx-linux.c ----*/
/*--------------------------------------------------------------------*/

/*
  This file is part of Valgrind, a dynamic binary instrumentation
  framework.

  Copyright (C) 2010-2013 Tilera Corp.

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

/* Contributed by Zhi-Gang Liu */

#if defined(VGP_tilegx_linux)
#include "pub_core_basics.h"
#include "pub_core_vki.h"
#include "pub_core_vkiscnums.h"
#include "pub_core_threadstate.h"
#include "pub_core_aspacemgr.h"
#include "pub_core_debuglog.h"
#include "pub_core_libcbase.h"
#include "pub_core_libcassert.h"
#include "pub_core_libcprint.h"
#include "pub_core_libcproc.h"
#include "pub_core_libcsignal.h"
#include "pub_core_options.h"
#include "pub_core_scheduler.h"
#include "pub_core_sigframe.h"     // For VG_(sigframe_destroy)()
#include "pub_core_signals.h"
#include "pub_core_syscall.h"
#include "pub_core_syswrap.h"
#include "pub_core_tooliface.h"
#include "pub_core_stacks.h"        // VG_(register_stack)
#include "pub_core_transtab.h"      // VG_(discard_translations)
#include "priv_types_n_macros.h"
#include "priv_syswrap-generic.h"   /* for decls of generic wrappers */
#include "priv_syswrap-linux.h"     /* for decls of linux wrappers */
#include "priv_syswrap-main.h"

#include "pub_core_debuginfo.h"     // VG_(di_notify_*)
#include "pub_core_xarray.h"
#include "pub_core_clientstate.h"   // VG_(brk_base), VG_(brk_limit)
#include "pub_core_errormgr.h"
#include "pub_core_libcfile.h"
#include "pub_core_machine.h"       // VG_(get_SP)
#include "pub_core_mallocfree.h"
#include "pub_core_stacktrace.h"    // For VG_(get_and_pp_StackTrace)()
#include "pub_core_ume.h"

#include "config.h"

/* ---------------------------------------------------------------------
   clone() handling
   ------------------------------------------------------------------ */
/* Call f(arg1), but first switch stacks, using 'stack' as the new
   stack, and use 'retaddr' as f's return-to address.  Also, clear all
   the integer registers before entering f.*/

__attribute__ ((noreturn))
void ML_(call_on_new_stack_0_1) (Addr stack, Addr retaddr,
                                 void (*f) (Word), Word arg1);
                                //    r0 = stack
                                //    r1 = retaddr
                                //    r2 = f
                                //    r3 = arg1
     asm (
       ".text\n"
       ".globl vgModuleLocal_call_on_new_stack_0_1\n"
       "vgModuleLocal_call_on_new_stack_0_1:\n"
       "  {\n"
       "   move sp, r0\n\t"
       "   move r51, r2\n\t"
       "  }\n"
       "  {\n"
       "   move r0, r3\n\t"
       "   move r1, zero\n\t"
       "  }\n"
       "  {\n"
       "   move r2, zero\n\t"
       "   move r3, zero\n\t"
       "  }\n"
       "  {\n"
       "   move r4, zero\n\t"
       "   move r5, zero\n\t"
       "  }\n"
       "  {\n"
       "   move r6, zero\n\t"
       "   move r7, zero\n\t"
       "  }\n"
       "  {\n"
       "   move r8, zero\n\t"
       "   move r9, zero\n\t"
       "  }\n"
       "  {\n"
       "   move r10, zero\n\t"
       "   move r11, zero\n\t"
       "  }\n"
       "  {\n"
       "   move r12, zero\n\t"
       "   move r13, zero\n\t"
       "  }\n"
       "  {\n"
       "   move r14, zero\n\t"
       "   move r15, zero\n\t"
       "  }\n"
       "  {\n"
       "   move r16, zero\n\t"
       "   move r17, zero\n\t"
       "  }\n"
       "  {\n"
       "   move r18, zero\n\t"
       "   move r19, zero\n\t"
       "  }\n"
       "  {\n"
       "   move r20, zero\n\t"
       "   move r21, zero\n\t"
       "  }\n"
       "  {\n"
       "   move r22, zero\n\t"
       "   move r23, zero\n\t"
       "  }\n"
       "  {\n"
       "   move r24, zero\n\t"
       "   move r25, zero\n\t"
       "  }\n"
       "  {\n"
       "   move r26, zero\n\t"
       "   move r27, zero\n\t"
       "  }\n"
       "  {\n"
       "   move r28, zero\n\t"
       "   move r29, zero\n\t"
       "  }\n"
       "  {\n"
       "   move r30, zero\n\t"
       "   move r31, zero\n\t"
       "  }\n"
       "  {\n"
       "   move r32, zero\n\t"
       "   move r33, zero\n\t"
       "  }\n"
       "  {\n"
       "   move r34, zero\n\t"
       "   move r35, zero\n\t"
       "  }\n"
       "  {\n"
       "   move r36, zero\n\t"
       "   move r37, zero\n\t"
       "  }\n"
       "  {\n"
       "   move r38, zero\n\t"
       "   move r39, zero\n\t"
       "  }\n"
       "  {\n"
       "   move r40, zero\n\t"
       "   move r41, zero\n\t"
       "  }\n"
       "  {\n"
       "   move r42, zero\n\t"
       "   move r43, zero\n\t"
       "  }\n"
       "  {\n"
       "   move r44, zero\n\t"
       "   move r45, zero\n\t"
       "  }\n"
       "  {\n"
       "   move r46, zero\n\t"
       "   move r47, zero\n\t"
       "  }\n"
       "  {\n"
       "   move r48, zero\n\t"
       "   move r49, zero\n\t"
       "  }\n"
       "  {\n"
       "   move r50, zero\n\t"
       "   jr      r51\n\t"
       "  }\n"
       "   ill \n"    // should never get here
          );
/*
  Perform a clone system call.  clone is strange because it has
  fork()-like return-twice semantics, so it needs special
  handling here.
  Upon entry, we have:
  int (fn)(void*)     in  r0
  void* child_stack   in  r1
  int flags           in  r2
  void* arg           in  r3
  pid_t* child_tid    in  r4
  pid_t* parent_tid   in  r5
  void* tls_ptr       in  r6

  System call requires:
  int    $__NR_clone  in r10
  int    flags        in r0
  void*  child_stack  in r1
  pid_t* parent_tid   in r2
  void*  tls_ptr      in $r3
  pid_t* child_tid    in sr4

  int clone(int (*fn)(void *arg), void *child_stack, int flags, void *arg,
  void *parent_tidptr, void *tls, void *child_tidptr)

  Returns an Int encoded in the linux-tilegx way, not a SysRes.
*/
#define __NR_CLONE        VG_STRINGIFY(__NR_clone)
#define __NR_EXIT         VG_STRINGIFY(__NR_exit)

Long do_syscall_clone_tilegx_linux ( Word (*fn) (void *),  //r0
                                     void *stack,          //r1
                                     Long flags,           //r2
                                     void *arg,            //r3
                                     Long * child_tid,     //r4
                                     Long * parent_tid,    //r5
                                     Long   tls );         //r6
    /*
      stack
      high -> 4  r29
      3
      2
      1  r10
      low  -> 0  lr    <- sp
    */
     asm (
       ".text\n"
       "   .globl   do_syscall_clone_tilegx_linux\n"
       "   do_syscall_clone_tilegx_linux:\n"
       "   beqz  r0, .Linvalid\n"
       "   beqz  r1, .Linvalid\n"
       "   {\n"
       "    st    sp, r29; "       // save r29 at top
       "    addli sp, sp, -32\n"   // open new stack space
       "   }\n"

       "    move  r29, sp; "       // r29 <- sp
       "    st    r29, lr\n"       // save lr at 0(sp)

       "    addi  r29, r29, 8\n"
       "   {\n"
       "    st    r29, r10\n"      // save r10 at 8(sp)
       /*  setup child stack */
       "    addi  r1, r1, -32\n"   // new stack frame for child
       "   }\n"
       /*  save fn */
       "   { st  r1, r0; addi r1, r1, 8 }\n"
       /*  save args */
       "   { st  r1, r3; addi r1, r1, 8 }\n"
       /*  save flags */
       "   { st  r1, r2; addi r1, r1, -16 }\n"

       /*  Child stack layout

           flags
           args
           r1->  fn
       */
       "   {\n"
       /*   prepare args for clone. */
       "    move r0,  r2\n"   // arg0 = flags
       /*   arg1=r1 child stack */
       "    move r2,  r5\n"   // arg2 = parent tid
       "   }\n"
       "   {\n"
       "    move r3,  r4\n"   // arg3 = child tid
       "    move r4,  r6\n"   // arg4 = tls
       "   }\n"
       "   moveli r10, " __NR_CLONE "\n"
       "   swint1\n"

       "   beqz  r0, .Lchild\n"
       "   move r29, sp\n"
       "   ld   lr, r29\n"        // Restore lr
       "   addi r29, r29, 8\n"
       "   {\n"
       "    ld   r10,  r29\n"      // resotre r10
       "    addi sp, sp, 32\n"
       "   }\n"
       "   ld   r29, sp\n"
       "   jrp  lr\n"

       ".Lchild:"
       "   move r2, sp\n"
       "   {\n"
       "    ld   r3, r2\n"
       "    addi r2, r2, 8\n"
       "   }\n"
       "   ld   r0, r2\n"
       "   jalr r3\n"
       "   moveli r10, " __NR_EXIT "\n"
       "   swint1\n"

       ".Linvalid:"
       "  { movei r1, 22; jrp lr }\n"
          );

#undef __NR_CLONE
#undef __NR_EXIT

// forward declarations
static void setup_child ( ThreadArchState *, ThreadArchState * );
static SysRes sys_set_tls ( ThreadId tid, Addr tlsptr );
 /*
   When a client clones, we need to keep track of the new thread.  This means:
   1. allocate a ThreadId+ThreadState+stack for the the thread
   2. initialize the thread's new VCPU state
   3. create the thread using the same args as the client requested,
   but using the scheduler entrypoint for IP, and a separate stack
   for SP.
 */
static SysRes do_clone ( ThreadId ptid,
                         Long flags, Addr sp,
                         Long * parent_tidptr,
                         Long * child_tidptr,
                         Addr child_tls )
{
  const Bool debug = False;
  ThreadId ctid = VG_ (alloc_ThreadState) ();
  ThreadState * ptst = VG_ (get_ThreadState) (ptid);
  ThreadState * ctst = VG_ (get_ThreadState) (ctid);
  Long ret = 0;
  Long * stack;
  SysRes res;
  vki_sigset_t blockall, savedmask;

  VG_ (sigfillset) (&blockall);
  vg_assert (VG_ (is_running_thread) (ptid));
  vg_assert (VG_ (is_valid_tid) (ctid));
  stack = (Long *) ML_ (allocstack) (ctid);
  if (stack == NULL) {
    res = VG_ (mk_SysRes_Error) (VKI_ENOMEM);
    goto out;
  }
  setup_child (&ctst->arch, &ptst->arch);

  /* On TILEGX we need to set r0 and r3 to zero */
  ctst->arch.vex.guest_r0 = 0;
  ctst->arch.vex.guest_r3 = 0;
  if (sp != 0)
    ctst->arch.vex.guest_r54 = sp;

  ctst->os_state.parent = ptid;
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
  ML_(guess_and_register_stack) (sp, ctst);

  VG_TRACK (pre_thread_ll_create, ptid, ctid);
  if (flags & VKI_CLONE_SETTLS) {
    if (debug)
      VG_(printf)("clone child has SETTLS: tls at %#lx\n", child_tls);
    ctst->arch.vex.guest_r53 = child_tls;
    res = sys_set_tls(ctid, child_tls);
    if (sr_isError(res))
      goto out;
  }

  flags &= ~VKI_CLONE_SETTLS;
  VG_ (sigprocmask) (VKI_SIG_SETMASK, &blockall, &savedmask);
  /* Create the new thread */
  ret = do_syscall_clone_tilegx_linux (ML_ (start_thread_NORETURN),
                                       stack, flags, &VG_ (threads)[ctid],
                                       child_tidptr, parent_tidptr,
                                       (Long)NULL /*child_tls*/);

  /* High half word64 is syscall return value. */
  if (debug)
    VG_(printf)("ret: 0x%lx\n", ret);

  res = VG_(mk_SysRes_tilegx_linux) (/*val */ ret);

  VG_ (sigprocmask) (VKI_SIG_SETMASK, &savedmask, NULL);

 out:
  if (sr_isError (res)) {
    VG_(cleanup_thread) (&ctst->arch);
    ctst->status = VgTs_Empty;
    VG_TRACK (pre_thread_ll_exit, ctid);
  }
  ptst->arch.vex.guest_r0 = 0;

  return res;
}

extern Addr do_brk ( Addr newbrk );

extern
SysRes do_mremap( Addr old_addr, SizeT old_len,
                  Addr new_addr, SizeT new_len,
                  UWord flags, ThreadId tid );

extern Bool linux_kernel_2_6_22(void);

/* ---------------------------------------------------------------------
   More thread stuff
   ------------------------------------------------------------------ */

// TILEGX doesn't have any architecture specific thread stuff that
// needs to be cleaned up.
void
VG_ (cleanup_thread) ( ThreadArchState * arch ) { }

void
setup_child ( /*OUT*/ ThreadArchState * child,
              /*IN*/ ThreadArchState * parent )
{
  /* We inherit our parent's guest state. */
  child->vex = parent->vex;
  child->vex_shadow1 = parent->vex_shadow1;
  child->vex_shadow2 = parent->vex_shadow2;
}

SysRes sys_set_tls ( ThreadId tid, Addr tlsptr )
{
  VG_(threads)[tid].arch.vex.guest_r53 = tlsptr;
  return VG_(mk_SysRes_Success)( 0 );
}


/* ---------------------------------------------------------------------
   PRE/POST wrappers for tilegx/Linux-specific syscalls
   ------------------------------------------------------------------ */
#define PRE(name)       DEFN_PRE_TEMPLATE(tilegx_linux, name)
#define POST(name)      DEFN_POST_TEMPLATE(tilegx_linux, name)

/* Add prototypes for the wrappers declared here, so that gcc doesn't
   harass us for not having prototypes.  Really this is a kludge --
   the right thing to do is to make these wrappers 'static' since they
   aren't visible outside this file, but that requires even more macro
   magic. */

DECL_TEMPLATE (tilegx_linux, sys_clone);
DECL_TEMPLATE (tilegx_linux, sys_rt_sigreturn);
DECL_TEMPLATE (tilegx_linux, sys_socket);
DECL_TEMPLATE (tilegx_linux, sys_setsockopt);
DECL_TEMPLATE (tilegx_linux, sys_getsockopt);
DECL_TEMPLATE (tilegx_linux, sys_connect);
DECL_TEMPLATE (tilegx_linux, sys_accept);
DECL_TEMPLATE (tilegx_linux, sys_accept4);
DECL_TEMPLATE (tilegx_linux, sys_sendto);
DECL_TEMPLATE (tilegx_linux, sys_recvfrom);
DECL_TEMPLATE (tilegx_linux, sys_sendmsg);
DECL_TEMPLATE (tilegx_linux, sys_recvmsg);
DECL_TEMPLATE (tilegx_linux, sys_shutdown);
DECL_TEMPLATE (tilegx_linux, sys_bind);
DECL_TEMPLATE (tilegx_linux, sys_listen);
DECL_TEMPLATE (tilegx_linux, sys_getsockname);
DECL_TEMPLATE (tilegx_linux, sys_getpeername);
DECL_TEMPLATE (tilegx_linux, sys_socketpair);
DECL_TEMPLATE (tilegx_linux, sys_semget);
DECL_TEMPLATE (tilegx_linux, sys_semop);
DECL_TEMPLATE (tilegx_linux, sys_semtimedop);
DECL_TEMPLATE (tilegx_linux, sys_semctl);
DECL_TEMPLATE (tilegx_linux, sys_msgget);
DECL_TEMPLATE (tilegx_linux, sys_msgrcv);
DECL_TEMPLATE (tilegx_linux, sys_msgsnd);
DECL_TEMPLATE (tilegx_linux, sys_msgctl);
DECL_TEMPLATE (tilegx_linux, sys_shmget);
DECL_TEMPLATE (tilegx_linux, wrap_sys_shmat);
DECL_TEMPLATE (tilegx_linux, sys_shmdt);
DECL_TEMPLATE (tilegx_linux, sys_shmdt);
DECL_TEMPLATE (tilegx_linux, sys_shmctl);
DECL_TEMPLATE (tilegx_linux, sys_arch_prctl);
DECL_TEMPLATE (tilegx_linux, sys_ptrace);
DECL_TEMPLATE (tilegx_linux, sys_fadvise64);
DECL_TEMPLATE (tilegx_linux, sys_mmap);
DECL_TEMPLATE (tilegx_linux, sys_syscall184);
DECL_TEMPLATE (tilegx_linux, sys_cacheflush);
DECL_TEMPLATE (tilegx_linux, sys_set_dataplane);

PRE(sys_clone)
{
  ULong cloneflags;

  PRINT("sys_clone ( %lx, %#lx, %#lx, %#lx, %#lx )",ARG1,ARG2,ARG3,ARG4,ARG5);
  PRE_REG_READ5(int, "clone",
                unsigned long, flags,
                void *, child_stack,
                int *, parent_tidptr,
                int *, child_tidptr,
                void *, tlsaddr);

  if (ARG1 & VKI_CLONE_PARENT_SETTID) {
    PRE_MEM_WRITE("clone(parent_tidptr)", ARG3, sizeof(Int));
    if (!VG_(am_is_valid_for_client)(ARG3, sizeof(Int), VKI_PROT_WRITE)) {
      SET_STATUS_Failure( VKI_EFAULT );
      return;
    }
  }
  if (ARG1 & (VKI_CLONE_CHILD_SETTID | VKI_CLONE_CHILD_CLEARTID)) {
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
  tst->arch.vex.guest_r54 -= sizeof(Addr);

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
  PRINT( "arch_prctl ( %ld, %lx )", ARG1, ARG2 );

  vg_assert(VG_(is_valid_tid)(tid));
  vg_assert(tid >= 1 && tid < VG_N_THREADS);
  vg_assert(VG_(is_running_thread)(tid));

  I_die_here;
}

// Parts of this are tilegx-specific, but the *PEEK* cases are generic.
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
#if 0 // FIXME
  case VKI_PTRACE_GETFPREGS:
    PRE_MEM_WRITE( "ptrace(getfpregs)", ARG4,
                   sizeof (struct vki_user_i387_struct));
    break;
#endif
  case VKI_PTRACE_SETREGS:
    PRE_MEM_READ( "ptrace(setregs)", ARG4,
                  sizeof (struct vki_user_regs_struct));
    break;
#if 0 // FIXME
  case VKI_PTRACE_SETFPREGS:
    PRE_MEM_READ( "ptrace(setfpregs)", ARG4,
                  sizeof (struct vki_user_i387_struct));
    break;
#endif
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
#if 0 // FIXME
  case VKI_PTRACE_GETFPREGS:
    POST_MEM_WRITE( ARG4, sizeof (struct vki_user_i387_struct));
    break;
#endif
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
  PRINT("sys_sendto ( %ld, %#lx, %ld, %lu, %#lx, %ld )",ARG1,ARG2,ARG3,
        ARG4,ARG5,ARG6);
  PRE_REG_READ6(long, "sendto",
                int, s, const void *, msg, int, len,
                unsigned int, flags,
                const struct sockaddr *, to, int, tolen);
  ML_(generic_PRE_sys_sendto)(tid, ARG1,ARG2,ARG3,ARG4,ARG5,ARG6);
}

PRE(sys_recvfrom)
{
  *flags |= SfMayBlock;
  PRINT("sys_recvfrom ( %ld, %#lx, %ld, %lu, %#lx, %#lx )",ARG1,ARG2,ARG3,
        ARG4,ARG5,ARG6);
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
  ML_(generic_PRE_sys_sendmsg)(tid, "msg", ARG2);
}

PRE(sys_recvmsg)
{
  *flags |= SfMayBlock;
  PRINT("sys_recvmsg ( %ld, %#lx, %ld )",ARG1,ARG2,ARG3);
  PRE_REG_READ3(long, "recvmsg", int, s, struct msghdr *, msg, int, flags);
  ML_(generic_PRE_sys_recvmsg)(tid, "msg", (struct vki_msghdr *) ARG2);
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
                int, msqid, struct msgbuf *, msgp, vki_size_t, msgsz,
                int, msgflg);
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
   PRE/POST wrappers for TILEGX/Linux-variant specific syscalls
   ------------------------------------------------------------ */
PRE(sys_cacheflush)
{
   PRINT("cacheflush (%lx, %lx, %lx)", ARG1, ARG2, ARG3);
   PRE_REG_READ3(long, "cacheflush", unsigned long, addr,
                 int, nbytes, int, cache);
   VG_ (discard_translations) ((Addr)ARG1, (ULong) ARG2,
                               "PRE(sys_cacheflush)");
   SET_STATUS_Success(0);
}

PRE(sys_set_dataplane)
{
  *flags |= SfMayBlock;
  PRINT("sys_set_dataplane ( %ld )", ARG1);
  PRE_REG_READ1(long, "set_dataplane", unsigned long, flag);
}

#undef PRE
#undef POST


/* ---------------------------------------------------------------------
   The TILEGX/Linux syscall table
   ------------------------------------------------------------------ */

/* Add an tilegx-linux specific wrapper to a syscall table. */
#define PLAX_(const, name)    WRAPPER_ENTRY_X_(tilegx_linux, const, name)
#define PLAXY(const, name)    WRAPPER_ENTRY_XY(tilegx_linux, const, name)

// This table maps from __NR_xxx syscall numbers (from
// linux/include/asm/unistd.h) to the appropriate PRE/POST sys_foo()
//
// When implementing these wrappers, you need to work out if the wrapper is
// generic, Linux-only (but arch-independent), or TILEGX/Linux only.

static SyscallTableEntry syscall_table[] = {

  LINXY(__NR_io_setup,          sys_io_setup),             // 0
  LINX_(__NR_io_destroy,        sys_io_destroy),           // 1
  LINX_(__NR_io_submit,         sys_io_submit),            // 2
  LINXY(__NR_io_cancel,         sys_io_cancel),            // 3
  LINXY(__NR_io_getevents,      sys_io_getevents),         // 4
  LINX_(__NR_setxattr,          sys_setxattr),             // 5
  LINX_(__NR_lsetxattr,         sys_lsetxattr),            // 6
  LINX_(__NR_fsetxattr,         sys_fsetxattr),            // 7
  LINXY(__NR_getxattr,          sys_getxattr),             // 8
  LINXY(__NR_lgetxattr,         sys_lgetxattr),            // 9
  LINXY(__NR_fgetxattr,         sys_fgetxattr),            // 10
  LINXY(__NR_listxattr,         sys_listxattr),            // 11
  LINXY(__NR_llistxattr,        sys_llistxattr),           // 12
  LINXY(__NR_flistxattr,        sys_flistxattr),           // 13
  LINX_(__NR_removexattr,       sys_removexattr),          // 14
  LINX_(__NR_lremovexattr,      sys_lremovexattr),         // 15
  LINX_(__NR_fremovexattr,      sys_fremovexattr),         // 16
  GENXY(__NR_getcwd,            sys_getcwd),               // 17
  LINXY(__NR_lookup_dcookie,    sys_lookup_dcookie),       // 18
  LINX_(__NR_eventfd2,          sys_eventfd2),             // 19
  LINXY(__NR_epoll_create1,     sys_epoll_create1),        // 20
  LINX_(__NR_epoll_ctl,         sys_epoll_ctl),            // 21
  LINXY(__NR_epoll_pwait,       sys_epoll_pwait),          // 22
  GENXY(__NR_dup,               sys_dup),                  // 23
  GENXY(__NR_dup2,              sys_dup2),                 // 23
  LINXY(__NR_dup3,              sys_dup3),                 // 24
  LINXY(__NR_fcntl,             sys_fcntl),                // 25
  LINXY(__NR_inotify_init1,     sys_inotify_init1),        // 26
  LINX_(__NR_inotify_add_watch, sys_inotify_add_watch),    // 27
  LINX_(__NR_inotify_rm_watch,  sys_inotify_rm_watch),     // 28
  LINXY(__NR_ioctl,             sys_ioctl),                // 29
  LINX_(__NR_ioprio_set,        sys_ioprio_set),           // 30
  LINX_(__NR_ioprio_get,        sys_ioprio_get),           // 31
  GENX_(__NR_flock,             sys_flock),                // 32
  LINX_(__NR_mknodat,           sys_mknodat),              // 33
  LINX_(__NR_mkdirat,           sys_mkdirat),              // 34
  LINX_(__NR_unlinkat,          sys_unlinkat),             // 35
  LINX_(__NR_symlinkat,         sys_symlinkat),            // 36
  LINX_(__NR_linkat,            sys_linkat),               // 37
  LINX_(__NR_renameat,          sys_renameat),             // 38
  LINX_(__NR_umount2,           sys_umount),               // 39
  LINX_(__NR_mount,             sys_mount),                // 40

  GENXY(__NR_statfs,            sys_statfs),               // 43
  GENXY(__NR_fstatfs,           sys_fstatfs),              // 44
  GENX_(__NR_truncate,          sys_truncate),             // 45
  GENX_(__NR_ftruncate,         sys_ftruncate),            // 46
  LINX_(__NR_fallocate,         sys_fallocate),            // 47
  LINX_(__NR_faccessat,         sys_faccessat),            // 48
  GENX_(__NR_chdir,             sys_chdir),                // 49
  GENX_(__NR_fchdir,            sys_fchdir),               // 50
  GENX_(__NR_chroot,            sys_chroot),               // 51
  GENX_(__NR_fchmod,            sys_fchmod),               // 52
  LINX_(__NR_fchmodat,          sys_fchmodat),             // 53
  LINX_(__NR_fchownat,          sys_fchownat),             // 54
  GENX_(__NR_fchown,            sys_fchown),               // 55
  LINXY(__NR_openat,            sys_openat),               // 56
  GENXY(__NR_close,             sys_close),                // 57
  LINX_(__NR_vhangup,           sys_vhangup),              // 58
  LINXY(__NR_pipe2,             sys_pipe2),                // 59
  LINX_(__NR_quotactl,          sys_quotactl),             // 60
  GENXY(__NR_getdents64,        sys_getdents64),           // 61
  LINX_(__NR_lseek,             sys_lseek),                // 62
  GENXY(__NR_read,              sys_read),                 // 63
  GENX_(__NR_write,             sys_write),                // 64
  GENXY(__NR_readv,             sys_readv),                // 65
  GENX_(__NR_writev,            sys_writev),               // 66
  GENXY(__NR_pread64,           sys_pread64),              // 67
  GENX_(__NR_pwrite64,          sys_pwrite64),             // 68
  LINXY(__NR_preadv,            sys_preadv),               // 69
  LINX_(__NR_pwritev,           sys_pwritev),              // 70
  LINXY(__NR_sendfile,          sys_sendfile),             // 71
  LINX_(__NR_pselect6,          sys_pselect6),             // 72
  LINXY(__NR_ppoll,             sys_ppoll),                // 73
  LINXY(__NR_signalfd4,         sys_signalfd4),            // 74
  LINX_(__NR_splice,            sys_splice),               // 75
  LINX_(__NR_readlinkat,        sys_readlinkat),           // 78
  LINXY(__NR3264_fstatat,       sys_newfstatat),           // 79
  GENXY(__NR_fstat,             sys_newfstat),             // 80
  GENX_(__NR_sync,              sys_sync),                 // 81
  GENX_(__NR_fsync,             sys_fsync),                // 82
  GENX_(__NR_fdatasync,         sys_fdatasync),            // 83
  LINX_(__NR_sync_file_range,   sys_sync_file_range),      // 84
  LINXY(__NR_timerfd_create,    sys_timerfd_create),       // 85
  LINXY(__NR_timerfd_settime,   sys_timerfd_settime),      // 86
  LINXY(__NR_timerfd_gettime,   sys_timerfd_gettime),      // 87
  LINX_(__NR_utimensat,         sys_utimensat),            // 88

  LINXY(__NR_capget,            sys_capget),               // 90
  LINX_(__NR_capset,            sys_capset),               // 91
  LINX_(__NR_personality,       sys_personality),          // 92
  GENX_(__NR_exit,              sys_exit),                 // 93
  LINX_(__NR_exit_group,        sys_exit_group),           // 94
  LINXY(__NR_waitid,            sys_waitid),               // 95
  LINX_(__NR_set_tid_address,   sys_set_tid_address),      // 96
  LINXY(__NR_futex,             sys_futex),                // 98
  LINX_(__NR_set_robust_list,   sys_set_robust_list),      // 99
  LINXY(__NR_get_robust_list,   sys_get_robust_list),      // 100
  GENXY(__NR_nanosleep,         sys_nanosleep),            // 101
  GENXY(__NR_getitimer,         sys_getitimer),            // 102
  GENXY(__NR_setitimer,         sys_setitimer),            // 103
  LINX_(__NR_init_module,       sys_init_module),          // 105
  LINX_(__NR_delete_module,     sys_delete_module),        // 106
  LINXY(__NR_timer_create,      sys_timer_create),         // 107
  LINXY(__NR_timer_gettime,     sys_timer_gettime),        // 108
  LINX_(__NR_timer_getoverrun,  sys_timer_getoverrun),     // 109
  LINXY(__NR_timer_settime,     sys_timer_settime),        // 110
  LINX_(__NR_timer_delete,      sys_timer_delete),         // 111
  LINX_(__NR_clock_settime,     sys_clock_settime),        // 112
  LINXY(__NR_clock_gettime,     sys_clock_gettime),        // 113
  LINXY(__NR_clock_getres,      sys_clock_getres),         // 114
  LINXY(__NR_clock_nanosleep,   sys_clock_nanosleep),      // 115
  LINXY(__NR_syslog,            sys_syslog),               // 116
  PLAXY(__NR_ptrace,            sys_ptrace),               // 117
  LINXY(__NR_sched_setparam,          sys_sched_setparam), // 118
  LINX_(__NR_sched_setscheduler,      sys_sched_setscheduler),     // 119
  LINX_(__NR_sched_getscheduler,      sys_sched_getscheduler),     // 120
  LINXY(__NR_sched_getparam,          sys_sched_getparam), // 121
  LINX_(__NR_sched_setaffinity, sys_sched_setaffinity),    // 122
  LINXY(__NR_sched_getaffinity, sys_sched_getaffinity),    // 123
  LINX_(__NR_sched_yield,       sys_sched_yield),          // 124
  LINX_(__NR_sched_get_priority_max,  sys_sched_get_priority_max), // 125
  LINX_(__NR_sched_get_priority_min,  sys_sched_get_priority_min), // 126
  LINXY(__NR_sched_rr_get_interval,   sys_sched_rr_get_interval),  // 127

  GENX_(__NR_kill,              sys_kill),                 // 129
  LINXY(__NR_tkill,             sys_tkill),                // 130
  LINXY(__NR_tgkill,            sys_tgkill),               // 131
  GENXY(__NR_sigaltstack,       sys_sigaltstack),          // 132
  LINX_(__NR_rt_sigsuspend,     sys_rt_sigsuspend),        // 133
  LINXY(__NR_rt_sigaction,      sys_rt_sigaction),         // 134
  LINXY(__NR_rt_sigprocmask,    sys_rt_sigprocmask),       // 135
  LINXY(__NR_rt_sigpending,     sys_rt_sigpending),        // 136
  LINXY(__NR_rt_sigtimedwait,   sys_rt_sigtimedwait),      // 137
  LINXY(__NR_rt_sigqueueinfo,   sys_rt_sigqueueinfo),      // 138
  PLAX_(__NR_rt_sigreturn,      sys_rt_sigreturn),         // 139
  GENX_(__NR_setpriority,             sys_setpriority),    // 140
  GENX_(__NR_getpriority,             sys_getpriority),    // 141

  GENX_(__NR_setregid,          sys_setregid),             // 143
  GENX_(__NR_setgid,            sys_setgid),               // 144
  GENX_(__NR_setreuid,          sys_setreuid),             // 145
  GENX_(__NR_setuid,            sys_setuid),               // 146
  LINX_(__NR_setresuid,         sys_setresuid),            // 147
  LINXY(__NR_getresuid,         sys_getresuid),            // 148
  LINX_(__NR_setresgid,         sys_setresgid),            // 149
  LINXY(__NR_getresgid,         sys_getresgid),            // 150
  LINX_(__NR_setfsuid,          sys_setfsuid),             // 151
  LINX_(__NR_setfsgid,          sys_setfsgid),             // 152
  GENXY(__NR_times,             sys_times),                // 153
  GENX_(__NR_setpgid,           sys_setpgid),              // 154
  GENX_(__NR_getpgid,           sys_getpgid),              // 155
  GENX_(__NR_getsid,            sys_getsid),               // 156
  GENX_(__NR_setsid,            sys_setsid),               // 157
  GENXY(__NR_getgroups,         sys_getgroups),            // 158
  GENX_(__NR_setgroups,         sys_setgroups),            // 159
  GENXY(__NR_uname,             sys_newuname),             // 160
  GENXY(__NR_getrlimit,         sys_getrlimit),            // 163
  GENX_(__NR_setrlimit,         sys_setrlimit),            // 164
  GENXY(__NR_getrusage,         sys_getrusage),            // 165
  GENX_(__NR_umask,             sys_umask),                // 166
  LINXY(__NR_prctl,             sys_prctl),                // 167

  GENXY(__NR_gettimeofday,      sys_gettimeofday),         // 169
  GENX_(__NR_settimeofday,      sys_settimeofday),         // 170
  LINXY(__NR_adjtimex,          sys_adjtimex),             // 171
  GENX_(__NR_getpid,            sys_getpid),               // 172
  GENX_(__NR_getppid,           sys_getppid),              // 173
  GENX_(__NR_getuid,            sys_getuid),               // 174
  GENX_(__NR_geteuid,           sys_geteuid),              // 175
  GENX_(__NR_getgid,            sys_getgid),               // 176
  GENX_(__NR_getegid,           sys_getegid),              // 177
  LINX_(__NR_gettid,            sys_gettid),               // 178
  LINXY(__NR_sysinfo,           sys_sysinfo),              // 179
  LINXY(__NR_mq_open,           sys_mq_open),              // 180
  LINX_(__NR_mq_unlink,         sys_mq_unlink),            // 181
  LINX_(__NR_mq_timedsend,      sys_mq_timedsend),         // 182
  LINXY(__NR_mq_timedreceive,   sys_mq_timedreceive),      // 183
  LINX_(__NR_mq_notify,         sys_mq_notify),            // 184
  LINXY(__NR_mq_getsetattr,     sys_mq_getsetattr),        // 185
  PLAX_(__NR_msgget,            sys_msgget),               // 186
  PLAXY(__NR_msgctl,            sys_msgctl),               // 187
  PLAXY(__NR_msgrcv,            sys_msgrcv),               // 188
  PLAX_(__NR_msgsnd,            sys_msgsnd),               // 189
  PLAX_(__NR_semget,            sys_semget),               // 190
  PLAXY(__NR_semctl,            sys_semctl),               // 191
  PLAX_(__NR_semtimedop,        sys_semtimedop),           // 192
  PLAX_(__NR_semop,             sys_semop),                // 193
  PLAX_(__NR_shmget,            sys_shmget),               // 194
  PLAXY(__NR_shmat,             wrap_sys_shmat),           // 196
  PLAXY(__NR_shmctl,            sys_shmctl),               // 195
  PLAXY(__NR_shmdt,             sys_shmdt),                // 197
  PLAXY(__NR_socket,            sys_socket),               // 198
  PLAXY(__NR_socketpair,        sys_socketpair),           // 199
  PLAX_(__NR_bind,              sys_bind),                 // 200
  PLAX_(__NR_listen,            sys_listen),               // 201
  PLAXY(__NR_accept,            sys_accept),               // 202
  PLAX_(__NR_connect,           sys_connect),              // 203
  PLAXY(__NR_getsockname,       sys_getsockname),          // 204
  PLAXY(__NR_getpeername,       sys_getpeername),          // 205
  PLAX_(__NR_sendto,            sys_sendto),               // 206
  PLAXY(__NR_recvfrom,          sys_recvfrom),             // 207
  PLAX_(__NR_setsockopt,        sys_setsockopt),           // 208
  PLAXY(__NR_getsockopt,        sys_getsockopt),           // 209
  PLAX_(__NR_shutdown,          sys_shutdown),             // 210
  PLAX_(__NR_sendmsg,           sys_sendmsg),              // 211
  PLAXY(__NR_recvmsg,           sys_recvmsg),              // 212
  LINX_(__NR_readahead,         sys_readahead),            // 213
  GENX_(__NR_brk,               sys_brk),                  // 214
  GENXY(__NR_munmap,            sys_munmap),               // 215
  GENX_(__NR_mremap,            sys_mremap),               // 216
  LINX_(__NR_add_key,           sys_add_key),              // 217
  LINX_(__NR_request_key,       sys_request_key),          // 218
  LINXY(__NR_keyctl,            sys_keyctl),               // 219
  PLAX_(__NR_clone,             sys_clone),                // 220
  GENX_(__NR_execve,            sys_execve),               // 221
  PLAX_(__NR_mmap,              sys_mmap),                 // 222
  GENXY(__NR_mprotect,          sys_mprotect),             // 226
  GENX_(__NR_msync,             sys_msync),                // 227
  GENX_(__NR_mlock,                   sys_mlock),          // 228
  GENX_(__NR_munlock,           sys_munlock),              // 229
  GENX_(__NR_mlockall,          sys_mlockall),             // 230
  LINX_(__NR_munlockall,        sys_munlockall),           // 231
  GENX_(__NR_mincore,           sys_mincore),              // 232
  GENX_(__NR_madvise,           sys_madvise),              // 233

  LINX_(__NR_mbind,             sys_mbind),                // 235
  LINXY(__NR_get_mempolicy,     sys_get_mempolicy),        // 236
  LINX_(__NR_set_mempolicy,     sys_set_mempolicy),        // 237

  LINXY(__NR_rt_tgsigqueueinfo, sys_rt_tgsigqueueinfo),    // 240

  PLAXY(__NR_accept4,           sys_accept4),              // 242

  PLAX_(__NR_cacheflush,        sys_cacheflush),           // 245
  PLAX_(__NR_set_dataplane,     sys_set_dataplane),        // 246

  GENXY(__NR_wait4,             sys_wait4),                // 260
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
  //vex_printf("sysno: %d\n", sysno);

  /* Can't find a wrapper */
  return NULL;
}

#endif // defined(VGP_tilegx_linux)

/*--------------------------------------------------------------------*/
/*--- end                                   syswrap-tilegx-linux.c ---*/
/*--------------------------------------------------------------------*/
