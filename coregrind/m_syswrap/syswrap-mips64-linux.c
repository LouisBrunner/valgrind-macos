
/*--------------------------------------------------------------------*/
/*--- Platform-specific syscalls stuff.    syswrap-mips64-linux.c ----*/
/*--------------------------------------------------------------------*/

/*
   This file is part of Valgrind, a dynamic binary instrumentation
   framework.

   Copyright (C) 2010-2013 RT-RK
      mips-valgrind@rt-rk.com

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

#if defined(VGP_mips64_linux)
#include "pub_core_basics.h"
#include "pub_core_vki.h"
#include "pub_core_vkiscnums.h"
#include "pub_core_libcsetjmp.h"   /* to keep _threadstate.h happy */
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
#include "pub_core_sigframe.h"     /* For VG_(sigframe_destroy)() */
#include "pub_core_signals.h"
#include "pub_core_syscall.h"
#include "pub_core_syswrap.h"
#include "pub_core_tooliface.h"
#include "pub_core_stacks.h"       /* VG_(register_stack) */
#include "pub_core_transtab.h"     /* VG_(discard_translations) */
#include "priv_types_n_macros.h"
#include "priv_syswrap-generic.h"  /* for decls of generic wrappers */
#include "priv_syswrap-linux.h"    /* for decls of linux-ish wrappers */
#include "priv_syswrap-main.h"

#include "pub_core_debuginfo.h"    /* VG_(di_notify_*) */
#include "pub_core_xarray.h"
#include "pub_core_clientstate.h"  /* VG_(brk_base), VG_(brk_limit) */
#include "pub_core_errormgr.h"
#include "pub_tool_gdbserver.h"    /* VG_(gdbserver) */
#include "pub_core_libcfile.h"
#include "pub_core_machine.h"      /* VG_(get_SP) */
#include "pub_core_mallocfree.h"
#include "pub_core_stacktrace.h"   /* For VG_(get_and_pp_StackTrace)() */
#include "pub_core_ume.h"

#include "config.h"

#include <errno.h>

/* ---------------------------------------------------------------------
                             clone() handling
   ------------------------------------------------------------------ */

/* Call f(arg1), but first switch stacks, using 'stack' as the new stack, and
   use 'retaddr' as f's return-to address. Also, clear all the integer registers
   before entering f. */
__attribute__ ((noreturn))
void ML_(call_on_new_stack_0_1) ( Addr stack,             /* $4 - $a0 */
                                  Addr retaddr,           /* $5 - $a1 */
                                  void (*f_desc) (Word),  /* $6 - $a2 */
                                  Word arg1 );            /* $7 - $a3 */
asm (
".text\n"
".globl vgModuleLocal_call_on_new_stack_0_1\n"
"vgModuleLocal_call_on_new_stack_0_1:\n"
"   move $29, $4\n"  /* set stack */
"   move $4,  $7\n"  /* arg1 to $4 */
"   move $25, $6\n"
"   move $31, $5\n"  /* retaddr to $ra */
"   jr $25\n"        /* jump to f */
"   break 0x7\n"     /* should never get here */
".previous\n"
);

/* Perform a clone system call.  clone is strange because it has fork()-like
   return-twice semantics, so it needs special handling here.

   Upon entry, we have:

      word (fn)(void*)    in a0 = 4
      void* child_stack   in a1 = 5
      word flags          in a2 = 6
      void* arg           in a3 = 7
      pid_t* parent_tid   in a4 = 8
      void* tls           in a5 = 9
      pid_t* child_tid    in a6 = 10  

   System call requires:

      int    $__NR_clone  in v0 
      int    flags        in a0 = 4 
      void*  child_stack  in a1 = 5 
      pid_t* parent_tid   in a2 = 6
      void*  tls_ptr      in a3 = 7 
      pid_t* child_tid    in a4 = 8 */

#define __NR_CLONE        __NR_clone
#define __NR_EXIT         __NR_exit

ULong do_syscall_clone_mips64_linux ( Word (*fn) (void *),  /* a0 - 4 */
                                      void* stack,          /* a1 - 5 */
                                      Int   flags,          /* a2 - 6 */
                                      void* arg,            /* a3 - 7 */
                                      Int*  parent_tid,     /* a4 - 8 */
                                      void* /* Int tls */,  /* a5 - 9 */
                                      Int*  child_tid );    /* a6 - 10 */

asm(
".text\n" 
".set noreorder\n"
".set nomacro\n"
".globl do_syscall_clone_mips64_linux\n"
"do_syscall_clone_mips64_linux:\n"
"   daddiu $29, $29, -32\n"
"   sd $31, 0($29)\n"
"   sd $30, 8($29)\n"
"   sd $28, 16($29)\n"

"   daddiu  $5, $5, -32\n"
"   sd $4, 0($5)\n"   /* fn */
"   sd $7, 8($5)\n"   /* arg */
"   sd $6, 16($5)\n"  /* flags */

/* 1. arg for syscalls */
"   move $4, $6\n"   /* flags */
"   move $6, $8\n"   /* parent */
"   move $7, $a5\n"  /* tls */
"   move $8, $a6\n"  /* child */

/* 2. do a syscall to clone */
"   li  $2, 5055\n"  /* syscall num for clone */
"   syscall\n"

/* 3. See if we are a child, call fn and after that exit */
"   bnez $7, p_or_error\n"
"   nop\n"

"   bnez $2, p_or_error\n"
"   nop\n"

"   ld $25,0($29)\n"
"   jalr $25\n"
"   ld $4,8($29)\n"

"   move $4, $2\n\t"  /* retval from fn is in $v0 */
"   li $2, 5058\n\t"  /* NR_exit */
"   syscall\n\t"
"   nop\n\t"
/* 4. If we are parent or error, just return to caller */
"   p_or_error:\n"
"   ld $31, 0($29)\n"
"   ld $30, 8($29)\n"
"   ld $28, 16($29)\n"
"   jr $31\n"
"   daddi $29,$29, 32\n"
".previous\n"
);

#undef __NR_CLONE
#undef __NR_EXIT

/* forward declarations */
static void setup_child ( ThreadArchState *, ThreadArchState *);
static SysRes sys_set_tls ( ThreadId tid, Addr tlsptr);

/* When a client clones, we need to keep track of the new thread. This means:
   1. allocate a ThreadId+ThreadState+stack for the the thread

   2. initialize the thread's new VCPU state

   3. create the thread using the same args as the client requested, but using
      the scheduler entrypoint for IP, and a separate stack for SP. */
static SysRes do_clone ( ThreadId ptid,
                         UInt flags, Addr sp,
                         Int* parent_tidptr,
                         Int* child_tidptr,
                         Addr child_tls )
{
   const Bool debug = False;
   ThreadId ctid = VG_ (alloc_ThreadState) ();
   ThreadState * ptst = VG_ (get_ThreadState) (ptid);
   ThreadState * ctst = VG_ (get_ThreadState) (ctid);
   UInt ret = 0;
   UWord * stack;
   NSegment const *seg;
   SysRes res;
   vki_sigset_t blockall, savedmask;

   VG_(sigfillset)(&blockall);
   vg_assert(VG_(is_running_thread)(ptid));
   vg_assert(VG_(is_valid_tid)(ctid));
   stack = (UWord *)ML_(allocstack)(ctid);
   if (stack == NULL) {
      res = VG_(mk_SysRes_Error)(VKI_ENOMEM);
      goto out;
   }
   setup_child(&ctst->arch, &ptst->arch);

   /* on MIPS we need to set V0 and A3 to zero */
   ctst->arch.vex.guest_r2 = 0;
   ctst->arch.vex.guest_r7 = 0;
   if (sp != 0)
      ctst->arch.vex.guest_r29 = sp;

   ctst->os_state.parent = ptid;
   ctst->sig_mask = ptst->sig_mask;
   ctst->tmp_sig_mask = ptst->sig_mask;

   ctst->os_state.threadgroup = ptst->os_state.threadgroup;
   seg = VG_(am_find_nsegment)((Addr)sp);

   if (seg && seg->kind != SkResvn) {
      ctst->client_stack_highest_word = sp;
      ctst->client_stack_szB = ctst->client_stack_highest_word - seg->start;
      VG_(register_stack)(seg->start, ctst->client_stack_highest_word);
      if (debug)
        VG_(printf)("tid %d: guessed client stack range %#lx-%#lx\n",
                    ctid, seg->start, sp /* VG_PGROUNDUP (sp) */ );
   } else {
      VG_(message)(Vg_UserMsg,
                    "!? New thread %d starts with sp+%#lx) unmapped\n",
                    ctid, sp);
      ctst->client_stack_szB = 0;
   }

   VG_TRACK(pre_thread_ll_create, ptid, ctid);
   if (flags & VKI_CLONE_SETTLS) {
       if (debug)
         VG_(printf)("clone child has SETTLS: tls at %#lx\n", child_tls);
       res = sys_set_tls(ctid, child_tls);
       if (sr_isError(res))
          goto out;
       ctst->arch.vex.guest_r27 = child_tls;
   }

   flags &= ~VKI_CLONE_SETTLS;
   VG_ (sigprocmask) (VKI_SIG_SETMASK, &blockall, &savedmask);
   /* Create the new thread */
   ret = do_syscall_clone_mips64_linux(ML_(start_thread_NORETURN),
                                       stack, flags, &VG_(threads)[ctid],
                                       parent_tidptr, NULL /*child_tls*/,
                                       child_tidptr);
   if (debug)
     VG_(printf)("ret: 0x%x\n", ret);

   res = VG_(mk_SysRes_mips64_linux)( /* val */ ret, 0, /* errflag */ 0);

   VG_(sigprocmask)(VKI_SIG_SETMASK, &savedmask, NULL);

   out:
   if (sr_isError (res)) {
      VG_ (cleanup_thread) (&ctst->arch);
      ctst->status = VgTs_Empty;
      VG_TRACK (pre_thread_ll_exit, ctid);
   }
   ptst->arch.vex.guest_r2 = 0;

   return res;
}

/* ---------------------------------------------------------------------
                          More thread stuff
   ------------------------------------------------------------------ */
void VG_(cleanup_thread) ( ThreadArchState * arch ) { };

void setup_child ( /* OUT */ ThreadArchState * child,
                   /* IN  */ ThreadArchState * parent )
{
   /* We inherit our parent's guest state. */
   child->vex = parent->vex;
   child->vex_shadow1 = parent->vex_shadow1;
   child->vex_shadow2 = parent->vex_shadow2;
}

SysRes sys_set_tls ( ThreadId tid, Addr tlsptr )
{
   VG_(threads)[tid].arch.vex.guest_ULR = tlsptr;
   return VG_(mk_SysRes_Success)( 0 );
}

/* ---------------------------------------------------------------------
           PRE/POST wrappers for mips/Linux-specific syscalls
   ------------------------------------------------------------------ */

#define PRE(name)       DEFN_PRE_TEMPLATE(mips_linux, name)
#define POST(name)      DEFN_POST_TEMPLATE(mips_linux, name)

/* Add prototypes for the wrappers declared here, so that gcc doesn't harass us
   for not having prototypes. Really this is a kludge -- the right thing to do
   is to make these wrappers 'static' since they aren't visible outside this
   file, but that requires even more macro magic. */

DECL_TEMPLATE (mips_linux, sys_set_thread_area);
DECL_TEMPLATE (mips_linux, sys_clone);
DECL_TEMPLATE (mips_linux, sys_socket);
DECL_TEMPLATE (mips_linux, sys_setsockopt);
DECL_TEMPLATE (mips_linux, sys_getsockopt);
DECL_TEMPLATE (mips_linux, sys_connect);
DECL_TEMPLATE (mips_linux, sys_accept);
DECL_TEMPLATE (mips_linux, sys_tee);
DECL_TEMPLATE (mips_linux, sys_splice);
DECL_TEMPLATE (mips_linux, sys_vmsplice);
DECL_TEMPLATE (mips_linux, sys_ustat);
DECL_TEMPLATE (mips_linux, sys_sysfs);
DECL_TEMPLATE (mips_linux, sys_swapon);
DECL_TEMPLATE (mips_linux, sys_swapoff);
DECL_TEMPLATE (mips_linux, sys_setdomainname);
DECL_TEMPLATE (mips_linux, sys_sethostname);
DECL_TEMPLATE (mips_linux, sys_accept4);
DECL_TEMPLATE (mips_linux, sys_sendto);
DECL_TEMPLATE (mips_linux, sys_recvfrom);
DECL_TEMPLATE (mips_linux, sys_sendmsg);
DECL_TEMPLATE (mips_linux, sys_recvmsg);
DECL_TEMPLATE (mips_linux, sys_shutdown);
DECL_TEMPLATE (mips_linux, sys_bind);
DECL_TEMPLATE (mips_linux, sys_reboot);
DECL_TEMPLATE (mips_linux, sys_listen);
DECL_TEMPLATE (mips_linux, sys_getsockname);
DECL_TEMPLATE (mips_linux, sys_getpeername);
DECL_TEMPLATE (mips_linux, sys_socketpair);
DECL_TEMPLATE (mips_linux, sys_semget);
DECL_TEMPLATE (mips_linux, sys_semop);
DECL_TEMPLATE (mips_linux, sys_semtimedop);
DECL_TEMPLATE (mips_linux, sys_cacheflush);
DECL_TEMPLATE (mips_linux, sys_semctl);
DECL_TEMPLATE (mips_linux, sys_sched_rr_get_interval);
DECL_TEMPLATE (mips_linux, sys_msgget);
DECL_TEMPLATE (mips_linux, sys_msgrcv);
DECL_TEMPLATE (mips_linux, sys_unshare);
DECL_TEMPLATE (mips_linux, sys_msgsnd);
DECL_TEMPLATE (mips_linux, sys_msgctl);
DECL_TEMPLATE (mips_linux, sys_shmget);
DECL_TEMPLATE (mips_linux, wrap_sys_shmat);
DECL_TEMPLATE (mips_linux, sys_shmdt);
DECL_TEMPLATE (mips_linux, sys_shmdt);
DECL_TEMPLATE (mips_linux, sys_shmctl);
DECL_TEMPLATE (mips_linux, sys_arch_prctl);
DECL_TEMPLATE (mips_linux, sys_ptrace);
DECL_TEMPLATE (mips_linux, sys_mmap);
DECL_TEMPLATE (mips_linux, sys_rt_sigreturn);
DECL_TEMPLATE (mips_linux, sys_pipe);

PRE(sys_socketpair)
{
   PRINT("sys_socketpair ( %ld, %ld, %ld, %#lx )", ARG1, ARG2, ARG3, ARG4);
   PRE_REG_READ4(long, "socketpair", int, d, int, type, int, protocol,
                 int *, sv);
   ML_(generic_PRE_sys_socketpair)(tid, ARG1, ARG2, ARG3, ARG4);
}

POST(sys_socketpair)
{
   vg_assert(SUCCESS);
   ML_(generic_POST_sys_socketpair)(tid, VG_(mk_SysRes_Success)(RES),
                                    ARG1, ARG2, ARG3, ARG4);
}

PRE(wrap_sys_shmat)
{
   UWord arg2tmp;
   PRINT("wrap_sys_shmat ( %ld, %#lx, %ld )", ARG1, ARG2, ARG3);
   PRE_REG_READ3(long, "shmat", int, shmid, const void *, shmaddr, int, shmflg);
   arg2tmp = ML_(generic_PRE_sys_shmat)(tid, ARG1, ARG2, ARG3);
   if (arg2tmp == 0)
      SET_STATUS_Failure(VKI_EINVAL);
   else
      ARG2 = arg2tmp;
}

POST(wrap_sys_shmat)
{
   ML_(generic_POST_sys_shmat)(tid, RES, ARG1, ARG2, ARG3);
}

PRE(sys_shmdt)
{
   PRINT("sys_shmdt ( %#lx )", ARG1);
   PRE_REG_READ1(long, "shmdt", const void *, shmaddr);
   if (!ML_(generic_PRE_sys_shmdt)(tid, ARG1))
      SET_STATUS_Failure(VKI_EINVAL);
}

POST(sys_shmdt)
{
   ML_(generic_POST_sys_shmdt)(tid, RES, ARG1);
}

PRE(sys_shmctl)
{
   PRINT("sys_shmctl ( %ld, %ld, %#lx )", ARG1, ARG2, ARG3);
   PRE_REG_READ3(long, "shmctl", int, shmid, int, cmd, struct shmid_ds *, buf);
   ML_(generic_PRE_sys_shmctl)(tid, ARG1, ARG2, ARG3);
}

POST(sys_shmctl)
{
   ML_(generic_POST_sys_shmctl)(tid, RES, ARG1, ARG2, ARG3);
}

PRE(sys_shmget)
{
   PRINT("sys_shmget ( %ld, %ld, %ld )", ARG1, ARG2, ARG3);
   PRE_REG_READ3(long, "shmget", vki_key_t, key, vki_size_t, size, int, shmflg);
}

PRE(sys_tee)
{
   PRINT("sys_tee ( %ld, %ld, %ld, %ld )", ARG1, ARG2, ARG3, ARG4);
   PRE_REG_READ4(long, "sys_tee", int, fdin, int, fdout, vki_size_t, len,
                 int, flags);
}

PRE(sys_splice)
{
   PRINT("sys_splice ( %ld, %ld, %ld, %ld, %ld, %ld )", ARG1, ARG2, ARG3,
                                                        ARG4, ARG5, ARG6);

   PRE_REG_READ6(long, "sys_splice", int, fdin, vki_loff_t, sizein, int,
                 fdout, vki_loff_t, sizeout, vki_size_t, len, int, flags);
}

PRE(sys_vmsplice)
{
   PRINT("sys_vmsplice ( %ld, %ld, %ld, %ld )", ARG1, ARG2, ARG3, ARG4);
   PRE_REG_READ4(long, "sys_vmsplice", int, fdin, struct vki_iovec *, v,
                 vki_size_t, len, int, flags);
}

PRE(sys_unshare)
{
   PRINT("sys_unshare ( %ld )", ARG1);
   PRE_REG_READ1(long, "sys_unshare", int, flags);
}

PRE(sys_msgctl)
{
   PRINT("sys_msgctl ( %ld, %ld, %#lx )", ARG1, ARG2, ARG3);
   PRE_REG_READ3(long, "msgctl", int, msqid, int, cmd, struct msqid_ds *, buf);
   ML_(linux_PRE_sys_msgctl) (tid, ARG1, ARG2, ARG3);
}

POST(sys_msgctl)
{
   ML_(linux_POST_sys_msgctl)(tid, RES, ARG1, ARG2, ARG3);
}

PRE(sys_msgget)
{
   PRINT("sys_msgget ( %ld, %ld )", ARG1, ARG2);
   PRE_REG_READ2(long, "msgget", vki_key_t, key, int, msgflg);
}

PRE(sys_sched_rr_get_interval)
{
   PRINT("sys_sched_rr_get_interval ( %ld, %#lx)", ARG1, ARG2);
   PRE_REG_READ2(long, "sched_rr_get_interval", int, flags,
                 struct timespec *, timer);
   *flags |= SfMayBlock;
}

PRE(sys_ustat)
{
   PRINT("sys_ustat ( %ld, %#lx)", ARG1, ARG2);
   PRE_REG_READ2(long, "ustat", int, flags, const void *, path);
}

PRE(sys_swapon)
{
   PRINT("sys_swapon ( %#lx, %ld )", ARG1, ARG2);
   PRE_REG_READ2(long, "swapon", const void *, path, int, flags);
}

PRE(sys_swapoff)
{
   PRINT("sys_swapoff ( %#lx )", ARG1);
   PRE_REG_READ1(long, "swapoff", const void *, path);
}

PRE(sys_sysfs)
{
   PRINT("sys_sysfs ( %ld, %ld, %#lx )", ARG1, ARG2, ARG3);
   PRE_REG_READ3(long, "sysfs", int, flags, int, desc, const void *, path);
}

PRE(sys_msgsnd)
{
   PRINT("sys_msgsnd ( %ld, %#lx, %ld, %ld )", ARG1, ARG2, ARG3, ARG4);
   PRE_REG_READ4(long, "msgsnd", int, msqid, struct msgbuf *, msgp,
                 vki_size_t, msgsz, int, msgflg);
   ML_(linux_PRE_sys_msgsnd)(tid, ARG1, ARG2, ARG3, ARG4);
   if ((ARG4 & VKI_IPC_NOWAIT) == 0)
      *flags |= SfMayBlock;
}

PRE(sys_msgrcv)
{
   PRINT("sys_msgrcv ( %ld, %#lx, %ld, %ld, %ld )", ARG1, ARG2, ARG3, ARG4,
                                                    ARG5);
   PRE_REG_READ5(long, "msgrcv", int, msqid, struct msgbuf *, msgp,
                 vki_size_t, msgsz, long, msgytp, int, msgflg);
   ML_(linux_PRE_sys_msgrcv) (tid, ARG1, ARG2, ARG3, ARG4, ARG5);
   if ((ARG4 & VKI_IPC_NOWAIT) == 0)
      *flags |= SfMayBlock;
}

POST(sys_msgrcv)
{
   ML_(linux_POST_sys_msgrcv)(tid, RES, ARG1, ARG2, ARG3, ARG4, ARG5);
}

PRE(sys_semctl)
{
   switch (ARG3 & ~VKI_IPC_64) {
      case VKI_IPC_INFO:
      case VKI_SEM_INFO:
        PRINT("sys_semctl ( %ld, %ld, %ld, %#lx )", ARG1, ARG2, ARG3, ARG4);
        PRE_REG_READ4(long, "semctl", int, semid, int, semnum, int, cmd,
                      struct seminfo *, arg);
      break;
      case VKI_IPC_STAT:
      case VKI_SEM_STAT:
      case VKI_IPC_SET:
         PRINT("sys_semctl ( %ld, %ld, %ld, %#lx )", ARG1, ARG2, ARG3, ARG4);
         PRE_REG_READ4(long, "semctl", int, semid, int, semnum, int, cmd,
                       struct semid_ds *, arg);
         break;
      case VKI_GETALL:
      case VKI_SETALL:
         PRINT("sys_semctl ( %ld, %ld, %ld, %#lx )", ARG1, ARG2, ARG3, ARG4);
         PRE_REG_READ4(long, "semctl", int, semid, int, semnum, int, cmd,
                       unsigned short *, arg);
         break;
      default:
         PRINT("sys_semctl ( %ld, %ld, %ld )", ARG1, ARG2, ARG3);
         PRE_REG_READ3(long, "semctl", int, semid, int, semnum, int, cmd);
         break;
   }
   ML_(generic_PRE_sys_semctl)(tid, ARG1, ARG2, ARG3, ARG4);
}

POST(sys_semctl)
{
   ML_(generic_POST_sys_semctl)(tid, RES, ARG1, ARG2, ARG3, ARG4);
}

PRE(sys_semget)
{
   PRINT("sys_semget ( %ld, %ld, %ld )", ARG1, ARG2, ARG3);
   PRE_REG_READ3(long, "semget", vki_key_t, key, int, nsems, int, semflg);
}

PRE(sys_semop)
{
   *flags |= SfMayBlock;
   PRINT("sys_semop ( %ld, %#lx, %lu )", ARG1, ARG2, ARG3);
   PRE_REG_READ3(long, "semop", int, semid, struct sembuf *, sops,
                 unsigned, nsoops);
   ML_(generic_PRE_sys_semop)(tid, ARG1, ARG2, ARG3);
}

PRE(sys_semtimedop)
{
   *flags |= SfMayBlock;
   PRINT("sys_semtimedop ( %ld, %#lx, %lu, %#lx )", ARG1, ARG2, ARG3, ARG4);
   PRE_REG_READ4(long, "semtimedop", int, semid, struct sembuf *, sops,
                 unsigned, nsoops, struct timespec *, timeout);
   ML_(generic_PRE_sys_semtimedop)(tid, ARG1, ARG2, ARG3, ARG4);
}

/* Very much MIPS specific */
PRE(sys_cacheflush)
{
   PRINT("cacheflush (%lx, %#lx, %#lx)", ARG1, ARG2, ARG3);
   PRE_REG_READ3(long, "cacheflush", void *, addrlow, void *, addrhigh,
                 int, flags);
   VG_(discard_translations)((Addr64) ARG1, ((ULong) ARG2) - ((ULong) ARG1) +
                             1ULL /*paranoia */ , "PRE(sys_cacheflush)");
   SET_STATUS_Success(0);
}

PRE(sys_getpeername)
{
   PRINT("sys_getpeername ( %ld, %#lx, %#lx )", ARG1, ARG2, ARG3);
   PRE_REG_READ3(long, "getpeername", int, s, struct sockaddr *, name,
                 int *, namelen);
   ML_(generic_PRE_sys_getpeername) (tid, ARG1, ARG2, ARG3);
}

POST(sys_getpeername)
{
   vg_assert (SUCCESS);
   ML_(generic_POST_sys_getpeername)(tid, VG_ (mk_SysRes_Success) (RES),
                                     ARG1, ARG2, ARG3);
}

PRE(sys_getsockname)
{
   PRINT("sys_getsockname ( %ld, %#lx, %#lx )", ARG1, ARG2, ARG3);
   PRE_REG_READ3(long, "getsockname", int, s, struct sockaddr *, name,
                 int *, namelen);
   ML_(generic_PRE_sys_getsockname)(tid, ARG1, ARG2, ARG3);
}

POST(sys_getsockname)
{
   vg_assert(SUCCESS);
   ML_(generic_POST_sys_getsockname)(tid, VG_ (mk_SysRes_Success) (RES),
                                     ARG1, ARG2, ARG3);
}

PRE(sys_shutdown)
{
   *flags |= SfMayBlock;
   PRINT("sys_shutdown ( %ld, %ld )", ARG1, ARG2);
   PRE_REG_READ2(int, "shutdown", int, s, int, how);
}

PRE(sys_reboot)
{
   PRINT("sys_reboot ( %ld )", ARG1);
   PRE_REG_READ1(int, "reboot", int, flags);
   *flags |= SfMayBlock;
}

PRE(sys_bind)
{
   PRINT("sys_bind ( %ld, %#lx, %ld )", ARG1, ARG2, ARG3);
   PRE_REG_READ3(long, "bind", int, sockfd, struct sockaddr *, my_addr,
                 int, addrlen);
   ML_(generic_PRE_sys_bind)(tid, ARG1, ARG2, ARG3);
}

PRE(sys_setdomainname)
{
   PRINT ("sys_setdomainname ( %#lx, %ld )", ARG1, ARG2);
   PRE_REG_READ2 (long, "setdomainname", const void *, name, int, len);
}

PRE(sys_sethostname)
{
   PRINT ("sys_sethostname ( %ld, %ld )", ARG1, ARG2);
   PRE_REG_READ2 (long, "sethostname", const void *, name, int, len);
}

PRE(sys_listen)
{
   PRINT ("sys_listen ( %ld, %ld )", ARG1, ARG2);
   PRE_REG_READ2 (long, "listen", int, s, int, backlog);
}

PRE(sys_sendmsg)
{
   *flags |= SfMayBlock;
   PRINT("sys_sendmsg ( %ld, %#lx, %ld )",ARG1, ARG2, ARG3);
   PRE_REG_READ3(long, "sendmsg", int, s, const struct msghdr *, msg,
                 int, flags);
   ML_(generic_PRE_sys_sendmsg)(tid, "msg", (struct vki_msghdr *)ARG2);
}

PRE(sys_recvmsg)
{
   *flags |= SfMayBlock;
   PRINT("sys_recvmsg ( %ld, %#lx, %ld )",ARG1, ARG2, ARG3);
   PRE_REG_READ3(long, "recvmsg", int, s, struct msghdr *, msg, int, flags);
   ML_(generic_PRE_sys_recvmsg)(tid, "msg", (struct vki_msghdr *)ARG2);
}

POST(sys_recvmsg)
{
   ML_(generic_POST_sys_recvmsg)(tid, "msg", (struct vki_msghdr *)ARG2, RES);
}

PRE(sys_sendto)
{
   *flags |= SfMayBlock;
   PRINT ("sys_sendto ( %ld, %#lx, %ld, %lu, %#lx, %ld )", ARG1, ARG2, ARG3,
                                                           ARG4, ARG5, ARG6);
   PRE_REG_READ6 (long, "sendto", int, s, const void *, msg, int, len,
                  unsigned int, flags, const struct sockaddr *, to, int, tolen);
   ML_(generic_PRE_sys_sendto) (tid, ARG1, ARG2, ARG3, ARG4, ARG5, ARG6);
}

PRE(sys_recvfrom)
{
   *flags |= SfMayBlock;
   PRINT("sys_recvfrom ( %ld, %#lx, %ld, %lu, %#lx, %#lx )", ARG1, ARG2,
                                                 ARG3, ARG4, ARG5, ARG6);
   PRE_REG_READ6(long, "recvfrom", int, s, void *, buf, int, len,
                 unsigned int, flags, struct sockaddr *, from, int *, fromlen);
   ML_(generic_PRE_sys_recvfrom)(tid, ARG1, ARG2, ARG3, ARG4, ARG5, ARG6);
}

POST(sys_recvfrom)
{
   vg_assert(SUCCESS);
   ML_(generic_POST_sys_recvfrom)(tid, VG_(mk_SysRes_Success)(RES),
                                  ARG1, ARG2, ARG3, ARG4, ARG5, ARG6);
}

PRE (sys_connect)
{
   *flags |= SfMayBlock;
   PRINT("sys_connect ( %ld, %#lx, %ld )", ARG1, ARG2, ARG3);
   PRE_REG_READ3(long, "connect", int, sockfd, struct sockaddr *, serv_addr,
                 int, addrlen);
   ML_(generic_PRE_sys_connect)(tid, ARG1, ARG2, ARG3);
}

PRE(sys_accept)
{
   *flags |= SfMayBlock;
   PRINT("sys_accept ( %ld, %#lx, %ld )", ARG1, ARG2, ARG3);
   PRE_REG_READ3(long, "accept",  int, s, struct sockaddr *, addr,
                 int, *addrlen);
   ML_(generic_PRE_sys_accept)(tid, ARG1, ARG2, ARG3);
}

PRE(sys_accept4)
{
   *flags |= SfMayBlock;
}

POST(sys_accept)
{
   SysRes r;
   vg_assert(SUCCESS);
   r = ML_(generic_POST_sys_accept)(tid, VG_ (mk_SysRes_Success) (RES),
                                    ARG1, ARG2, ARG3);
   SET_STATUS_from_SysRes(r);
}

PRE(sys_setsockopt)
{
   PRINT("sys_setsockopt ( %ld, %ld, %ld, %#lx, %ld )", ARG1, ARG2, ARG3,
                                                        ARG4, ARG5);
   PRE_REG_READ5(long, "setsockopt", int, s, int, level, int, optname,
                 const void *, optval, int, optlen);
   ML_(generic_PRE_sys_setsockopt)(tid, ARG1, ARG2, ARG3, ARG4, ARG5);
}

PRE(sys_getsockopt)
{
   PRINT("sys_getsockopt ( %ld, %ld, %ld, %#lx, %#lx )", ARG1, ARG2, ARG3,
                                                         ARG4, ARG5);
   PRE_REG_READ5(long, "getsockopt", int, s, int, level, int, optname,
                 void *, optval, int, *optlen);
   ML_(linux_PRE_sys_getsockopt)(tid, ARG1, ARG2, ARG3, ARG4, ARG5);
}

POST(sys_getsockopt)
{
   vg_assert(SUCCESS);
   ML_(linux_POST_sys_getsockopt)(tid, VG_(mk_SysRes_Success) (RES),
                                  ARG1, ARG2, ARG3, ARG4, ARG5);
}

PRE(sys_socket)
{
   PRINT("sys_socket ( %ld, %ld, %ld )", ARG1, ARG2, ARG3);
   PRE_REG_READ3(long, "socket", int, domain, int, type, int, protocol);
}

POST(sys_socket)
{
   SysRes r;
   vg_assert(SUCCESS);
   r = ML_(generic_POST_sys_socket)(tid, VG_(mk_SysRes_Success)(RES));
   SET_STATUS_from_SysRes(r);
}

PRE(sys_ptrace)
{
   PRINT("sys_ptrace ( %ld, %ld, %#lx, %#lx )", ARG1, ARG2, ARG3, ARG4);
   PRE_REG_READ4(int, "ptrace", long, request, long, pid, long, addr,
                 long, data);
   switch (ARG1) {
      case VKI_PTRACE_PEEKTEXT:
      case VKI_PTRACE_PEEKDATA:
      case VKI_PTRACE_PEEKUSR:
         PRE_MEM_WRITE("ptrace(peek)", ARG4, sizeof(long));
         break;
      case VKI_PTRACE_GETEVENTMSG:
         PRE_MEM_WRITE("ptrace(geteventmsg)", ARG4, sizeof(unsigned long));
         break;
      case VKI_PTRACE_GETSIGINFO:
         PRE_MEM_WRITE("ptrace(getsiginfo)", ARG4, sizeof(vki_siginfo_t));
         break;
      case VKI_PTRACE_SETSIGINFO:
         PRE_MEM_READ("ptrace(setsiginfo)", ARG4, sizeof(vki_siginfo_t));
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
         POST_MEM_WRITE (ARG4, sizeof(long));
         break;
      case VKI_PTRACE_GETEVENTMSG:
         POST_MEM_WRITE (ARG4, sizeof(unsigned long));
      break;
      case VKI_PTRACE_GETSIGINFO:
         POST_MEM_WRITE (ARG4, sizeof(vki_siginfo_t));
         break;
      default:
      break;
   }
}

PRE (sys_mmap)
{
   SysRes r;
   PRINT("sys_mmap ( %#lx, %llu, %lu, %lu, %lu, %ld )", ARG1, (ULong)ARG2,
                                                        ARG3, ARG4, ARG5, ARG6);
   PRE_REG_READ6(long, "mmap", unsigned long, start, vki_size_t, length,
                 int, prot, int, flags, int, fd, unsigned long, offset);
   r = ML_(generic_PRE_sys_mmap)(tid, ARG1, ARG2, ARG3, ARG4, ARG5,
                                 (Off64T) ARG6);
   SET_STATUS_from_SysRes(r);
}

PRE(sys_clone)
{
   Bool badarg = False;
   UInt cloneflags;
   PRINT("sys_clone ( %lx, %#lx, %#lx, %#lx, %#lx )", ARG1, ARG2, ARG3,
                                                      ARG4, ARG5);
   PRE_REG_READ2(int, "clone", unsigned long, flags, void *, child_stack);
   if (ARG1 & VKI_CLONE_PARENT_SETTID) {
      if (VG_(tdict).track_pre_reg_read) {
         PRA3("clone", int *, parent_tidptr);
      }
      PRE_MEM_WRITE("clone(parent_tidptr)", ARG3, sizeof(Int));
      if (!VG_(am_is_valid_for_client)(ARG3, sizeof(Int), VKI_PROT_WRITE)) {
         badarg = True;
      }
   }
   if (ARG1 & (VKI_CLONE_CHILD_SETTID | VKI_CLONE_CHILD_CLEARTID)) {
      if (VG_(tdict).track_pre_reg_read) {
         PRA5("clone", int *, child_tidptr);
      }
      PRE_MEM_WRITE("clone(child_tidptr)", ARG5, sizeof (Int));
      if (!VG_(am_is_valid_for_client)(ARG5, sizeof (Int), VKI_PROT_WRITE))
         badarg = True;
   }
   if (badarg) {
      SET_STATUS_Failure(VKI_EFAULT);
      return;
   }
   cloneflags = ARG1;
   if (!ML_(client_signal_OK)(ARG1 & VKI_CSIGNAL)) {
      SET_STATUS_Failure(VKI_EINVAL);
      return;
   }
   /* Only look at the flags we really care about */
   switch (cloneflags & (VKI_CLONE_VM | VKI_CLONE_FS
           |VKI_CLONE_FILES | VKI_CLONE_VFORK)) {
      case VKI_CLONE_VM | VKI_CLONE_FS | VKI_CLONE_FILES:
         /* thread creation */
         SET_STATUS_from_SysRes(do_clone(tid,
                                         ARG1,          /* flags */
                                         (Addr)ARG2,    /* child SP */
                                         (Int *)ARG3,   /* parent_tidptr */
                                         (Int *)ARG5,   /* child_tidptr */
                                         (Addr)ARG4));  /* child_tls */
         break;

      case VKI_CLONE_VFORK | VKI_CLONE_VM:  /* vfork */
         /* FALLTHROUGH - assume vfork == fork */
         cloneflags &= ~(VKI_CLONE_VFORK | VKI_CLONE_VM);
      case 0:  /* plain fork */
         SET_STATUS_from_SysRes(ML_(do_fork_clone)(tid,
                                cloneflags,     /* flags */
                                (Int *)ARG3,    /* parent_tidptr */
                                (Int *)ARG5));  /* child_tidptr */
         break;

      default:
         /* should we just ENOSYS? */
         VG_(message)(Vg_UserMsg, "Unsupported clone() flags: 0x%lx\n", ARG1);
         VG_(message)(Vg_UserMsg, "\n");
         VG_(message)(Vg_UserMsg, "The only supported clone() uses are:\n");
         VG_(message)(Vg_UserMsg,
                       " - via a threads library (LinuxThreads or NPTL)\n");
         VG_(message)(Vg_UserMsg,
                       " - via the implementation of fork or vfork\n");
         VG_(unimplemented)("Valgrind does not support general clone().");
   }
   if (SUCCESS) {
      if (ARG1 & VKI_CLONE_PARENT_SETTID)
         POST_MEM_WRITE(ARG3, sizeof(Int));
      if (ARG1 & (VKI_CLONE_CHILD_SETTID | VKI_CLONE_CHILD_CLEARTID))
         POST_MEM_WRITE(ARG5, sizeof(Int));
      /* Thread creation was successful; let the child have the chance to run */
      *flags |= SfYieldAfter;
   }
}

PRE(sys_rt_sigreturn)
{
   /* See comments on PRE(sys_rt_sigreturn) in syswrap-s390x-linux.c for
      an explanation of what follows. */
   ThreadState* tst;
   PRINT("sys_rt_sigreturn ( )");

   vg_assert(VG_(is_valid_tid)(tid));
   vg_assert(tid >= 1 && tid < VG_N_THREADS);
   vg_assert(VG_(is_running_thread)(tid));

   tst = VG_(get_ThreadState)(tid);

   /* This is only so that the IA is (might be) useful to report if
      something goes wrong in the sigreturn */
   ML_(fixup_guest_state_to_restart_syscall)(&tst->arch);

   /* Restore register state from frame and remove it */
   VG_(sigframe_destroy)(tid, True);

   /* Tell the driver not to update the guest state with the "result",
      and set a bogus result to keep it happy. */
   *flags |= SfNoWriteResult;
   SET_STATUS_Success(0);

   /* Check to see if any signals arose as a result of this. */
   *flags |= SfPollAfter;
}

PRE(sys_set_thread_area)
{
   PRINT("set_thread_area (%lx)", ARG1);
   PRE_REG_READ1(long, "set_thread_area", unsigned long, addr);
   SET_STATUS_from_SysRes(sys_set_tls(tid, ARG1));
}

PRE(sys_pipe)
{
   PRINT("sys_pipe ( %#lx )", ARG1);
   PRE_REG_READ1(int, "pipe", int *, filedes);
   PRE_MEM_WRITE( "pipe(filedes)", ARG1, 2*sizeof(int) );
}

POST(sys_pipe)
{
   Int p0, p1;
   vg_assert(SUCCESS);
   p0 = RES;
   p1 = sr_ResEx(status->sres);

   if (!ML_(fd_allowed)(p0, "pipe", tid, True) ||
       !ML_(fd_allowed)(p1, "pipe", tid, True)) {
      VG_(close)(p0);
      VG_(close)(p1);
      SET_STATUS_Failure( VKI_EMFILE );
   } else {
      if (VG_(clo_track_fds)) {
         ML_(record_fd_open_nameless)(tid, p0);
         ML_(record_fd_open_nameless)(tid, p1);
      }
   }
}

#undef PRE
#undef POST

/* ---------------------------------------------------------------------
   The mips64/Linux syscall table
   ------------------------------------------------------------------ */

/* Add an mips64-linux specific wrapper to a syscall table. */
#define PLAX_(sysno, name)    WRAPPER_ENTRY_X_(mips_linux, sysno, name)
#define PLAXY(sysno, name)    WRAPPER_ENTRY_XY(mips_linux, sysno, name)

static SyscallTableEntry syscall_main_table[] = {
   GENXY (__NR_read, sys_read),  /* 5000 */
   GENX_ (__NR_write, sys_write),
   GENXY (__NR_open, sys_open),
   GENXY (__NR_close, sys_close),
   GENXY (__NR_stat, sys_newstat),
   GENXY (__NR_fstat, sys_newfstat),
   GENXY (__NR_lstat, sys_newlstat),
   GENXY (__NR_poll, sys_poll),
   LINX_ (__NR_lseek, sys_lseek),
   PLAX_ (__NR_mmap, sys_mmap),
   GENXY (__NR_mprotect, sys_mprotect),
   GENXY (__NR_munmap, sys_munmap),
   GENX_ (__NR_brk, sys_brk),
   LINXY (__NR_rt_sigaction, sys_rt_sigaction),
   LINXY (__NR_rt_sigprocmask, sys_rt_sigprocmask),
   LINXY (__NR_ioctl, sys_ioctl),
   LINXY (__NR_eventfd2, sys_eventfd2),
   LINXY (__NR_signalfd4, sys_signalfd4),
   GENXY (__NR_pread64, sys_pread64),
   GENX_ (__NR_pwrite64, sys_pwrite64),
   GENXY (__NR_readv, sys_readv),
   GENX_ (__NR_writev, sys_writev),
   GENX_ (__NR_access, sys_access),
   PLAXY (__NR_pipe, sys_pipe),
   LINXY (__NR_pipe2, sys_pipe2),
   GENX_ (__NR__newselect,sys_select),
   LINX_ (__NR_sched_yield, sys_sched_yield),
   GENX_ (__NR_mremap, sys_mremap),
   GENX_ (__NR_msync, sys_msync),
   GENXY (__NR_mincore, sys_mincore),
   GENX_ (__NR_madvise, sys_madvise),
   PLAX_ (__NR_shmget, sys_shmget),
   /* PLAXY(__NR_shmat,sys_shmat), */
   PLAXY (__NR_shmctl, sys_shmctl),
   GENXY (__NR_dup, sys_dup),
   GENXY (__NR_dup2, sys_dup2),
   /* GENXY(__NR_dup3,sys_dup3), */
   GENX_ (__NR_pause, sys_pause),
   GENXY (__NR_nanosleep, sys_nanosleep),
   GENXY (__NR_getitimer, sys_getitimer),
   GENXY (__NR_setitimer, sys_setitimer),
   GENX_ (__NR_alarm, sys_alarm),
   GENX_ (__NR_getpid, sys_getpid),
   /* LINX_(__NR_fallocate,sys_fallocate), */
   LINXY (__NR_sendfile, sys_sendfile),
   PLAXY (__NR_socket, sys_socket),
   PLAX_ (__NR_connect, sys_connect),
   PLAXY (__NR_accept, sys_accept),
   /* PLAX_(__NR_accept4,sys_accept4), */
   PLAX_ (__NR_sendto, sys_sendto),
   PLAXY (__NR_recvfrom, sys_recvfrom),
   PLAX_ (__NR_sendmsg, sys_sendmsg),
   PLAXY (__NR_recvmsg, sys_recvmsg),
   PLAX_ (__NR_shutdown, sys_shutdown),
   PLAX_ (__NR_bind, sys_bind),
   PLAX_ (__NR_listen, sys_listen),
   PLAXY (__NR_getsockname, sys_getsockname),
   PLAXY (__NR_getpeername, sys_getpeername),
   PLAXY (__NR_socketpair, sys_socketpair),
   PLAX_ (__NR_setsockopt, sys_setsockopt),
   PLAXY (__NR_getsockopt, sys_getsockopt),
   PLAX_ (__NR_clone, sys_clone),
   GENX_ (__NR_fork, sys_fork),
   GENX_ (__NR_execve, sys_execve),
   GENX_ (__NR_exit, sys_exit),
   GENXY (__NR_wait4, sys_wait4),
   GENX_ (__NR_kill, sys_kill),
   GENXY (__NR_uname, sys_newuname),
   PLAX_ (__NR_semget, sys_semget),
   PLAX_ (__NR_semop, sys_semop),
   PLAXY (__NR_semctl, sys_semctl),
   PLAXY (__NR_shmdt, sys_shmdt),
   PLAX_ (__NR_msgget, sys_msgget),
   PLAX_ (__NR_msgsnd, sys_msgsnd),
   PLAXY (__NR_msgrcv, sys_msgrcv),
   PLAXY (__NR_msgctl, sys_msgctl),
   LINXY (__NR_fcntl, sys_fcntl),
   GENX_ (__NR_flock, sys_flock),
   GENX_ (__NR_fsync, sys_fsync),
   GENX_ (__NR_fdatasync, sys_fdatasync),
   GENX_ (__NR_truncate, sys_truncate),
   GENX_ (__NR_ftruncate, sys_ftruncate),
   GENXY (__NR_getdents, sys_getdents),
   GENXY (__NR_getcwd, sys_getcwd),
   GENX_ (__NR_chdir, sys_chdir),
   GENX_ (__NR_fchdir, sys_fchdir),
   GENX_ (__NR_rename, sys_rename),
   GENX_ (__NR_mkdir, sys_mkdir),
   GENX_ (__NR_rmdir, sys_rmdir),
   GENXY (__NR_creat, sys_creat),
   GENX_ (__NR_link, sys_link),
   GENX_ (__NR_unlink, sys_unlink),
   GENX_ (__NR_symlink, sys_symlink),
   GENX_ (__NR_readlink, sys_readlink),
   GENX_ (__NR_chmod, sys_chmod),
   GENX_ (__NR_fchmod, sys_fchmod),
   GENX_ (__NR_chown, sys_chown),
   GENX_ (__NR_fchown, sys_fchown),
   GENX_ (__NR_lchown, sys_lchown),
   GENX_ (__NR_umask, sys_umask),
   GENXY (__NR_gettimeofday, sys_gettimeofday),
   GENXY (__NR_getrlimit, sys_getrlimit),
   GENXY (__NR_getrusage, sys_getrusage),
   LINXY (__NR_sysinfo, sys_sysinfo),
   GENXY (__NR_times, sys_times),
   PLAXY (__NR_ptrace, sys_ptrace),
   GENX_ (__NR_getuid, sys_getuid),
   LINXY (__NR_syslog, sys_syslog),
   GENX_ (__NR_getgid, sys_getgid),
   GENX_ (__NR_setuid, sys_setuid),
   GENX_ (__NR_setgid, sys_setgid),
   GENX_ (__NR_geteuid, sys_geteuid),
   GENX_ (__NR_getegid, sys_getegid),
   GENX_ (__NR_setpgid, sys_setpgid),
   GENX_ (__NR_getppid, sys_getppid),
   GENX_ (__NR_getpgrp, sys_getpgrp),
   GENX_ (__NR_setsid, sys_setsid),
   GENX_ (__NR_setreuid, sys_setreuid),
   GENX_ (__NR_setregid, sys_setregid),
   GENXY (__NR_getgroups, sys_getgroups),
   GENX_ (__NR_setgroups, sys_setgroups),
   LINX_ (__NR_setresuid, sys_setresuid),
   LINXY (__NR_getresuid, sys_getresuid),
   LINX_ (__NR_setresgid, sys_setresgid),
   LINXY (__NR_getresgid, sys_getresgid),
   GENX_ (__NR_getpgid, sys_getpgid),
   LINX_ (__NR_setfsuid, sys_setfsuid),
   LINX_ (__NR_setfsgid, sys_setfsgid),
   GENX_ (__NR_getsid, sys_getsid),
   LINXY (__NR_capget, sys_capget),
   LINX_ (__NR_capset, sys_capset),
   LINXY (__NR_rt_sigpending, sys_rt_sigpending),
   LINXY (__NR_rt_sigtimedwait, sys_rt_sigtimedwait),
   LINXY (__NR_rt_sigqueueinfo, sys_rt_sigqueueinfo),
   LINX_ (__NR_rt_sigsuspend, sys_rt_sigsuspend),
   GENXY (__NR_sigaltstack, sys_sigaltstack),
   LINX_ (__NR_utime, sys_utime),
   GENX_ (__NR_mknod, sys_mknod),
   LINX_ (__NR_personality, sys_personality),
   PLAX_ (__NR_ustat, sys_ustat),
   GENXY (__NR_statfs, sys_statfs),
   GENXY (__NR_fstatfs, sys_fstatfs),
   PLAX_ (__NR_sysfs, sys_sysfs),
   GENX_ (__NR_getpriority, sys_getpriority),
   GENX_ (__NR_setpriority, sys_setpriority),
   LINXY (__NR_sched_setparam, sys_sched_setparam),
   LINXY (__NR_sched_getparam, sys_sched_getparam),
   LINX_ (__NR_sched_setscheduler, sys_sched_setscheduler),
   LINX_ (__NR_sched_getscheduler, sys_sched_getscheduler),
   LINX_ (__NR_sched_get_priority_max, sys_sched_get_priority_max),
   LINX_ (__NR_sched_get_priority_min, sys_sched_get_priority_min),
   PLAX_ (__NR_sched_rr_get_interval, sys_sched_rr_get_interval),
   GENX_ (__NR_mlock, sys_mlock),
   GENX_ (__NR_munlock, sys_munlock),
   GENX_ (__NR_mlockall, sys_mlockall),
   LINX_ (__NR_munlockall, sys_munlockall),
   LINX_ (__NR_vhangup, sys_vhangup),
   /* GENX_(__NR_pivot_root,sys_pivot_root), */
   LINXY (__NR__sysctl, sys_sysctl),
   LINXY (__NR_prctl, sys_prctl),
   LINXY (__NR_adjtimex, sys_adjtimex),
   GENX_ (__NR_setrlimit, sys_setrlimit),
   GENX_ (__NR_chroot, sys_chroot),
   GENX_ (__NR_sync, sys_sync),
   GENX_ (__NR_acct, sys_acct),
   GENX_ (__NR_settimeofday, sys_settimeofday),
   LINX_ (__NR_mount, sys_mount),
   LINX_ (__NR_umount2, sys_umount),
   PLAX_ (__NR_swapon, sys_swapon),
   PLAX_ (__NR_swapoff, sys_swapoff),
   PLAX_ (__NR_reboot, sys_reboot),
   PLAX_ (__NR_sethostname, sys_sethostname),
   PLAX_ (__NR_setdomainname, sys_setdomainname),
   GENX_ (__NR_create_module, sys_ni_syscall),
   LINX_ (__NR_init_module, sys_init_module),
   LINX_ (__NR_delete_module, sys_delete_module),
   GENX_ (__NR_get_kernel_syms, sys_ni_syscall),
   GENX_ (__NR_query_module, sys_ni_syscall),
   LINX_ (__NR_quotactl, sys_quotactl),
   /* GENX_(__NR_nfsservctl,sys_nfsservctl), */
   GENXY (__NR_getpmsg, sys_getpmsg),
   GENX_ (__NR_putpmsg, sys_putpmsg),
   GENX_ (__NR_afs_syscall, sys_ni_syscall),
   /* GENX_(__NR_reserved177,sys_reserved177), */
   LINX_ (__NR_gettid, sys_gettid),
   /* GENX_(__NR_readahead,sys_readahead), */
   LINX_ (__NR_setxattr, sys_setxattr),
   LINX_ (__NR_lsetxattr, sys_lsetxattr),
   LINX_ (__NR_fsetxattr, sys_fsetxattr),
   LINXY (__NR_getxattr, sys_getxattr),
   LINXY (__NR_lgetxattr, sys_lgetxattr),
   LINXY (__NR_fgetxattr, sys_fgetxattr),
   LINXY (__NR_listxattr, sys_listxattr),
   LINXY (__NR_llistxattr, sys_llistxattr),
   LINXY (__NR_flistxattr, sys_flistxattr),
   LINX_ (__NR_removexattr, sys_removexattr),
   LINX_ (__NR_lremovexattr, sys_lremovexattr),
   LINX_ (__NR_fremovexattr, sys_fremovexattr),
   LINXY (__NR_tkill, sys_tkill),
   /* GENX_(__NR_reserved193,sys_reserved193), */
   LINXY (__NR_futex, sys_futex),
   LINX_ (__NR_sched_setaffinity, sys_sched_setaffinity),
   LINXY (__NR_sched_getaffinity, sys_sched_getaffinity),
   PLAX_ (__NR_cacheflush, sys_cacheflush),
   LINXY (__NR_io_setup, sys_io_setup),
   LINX_ (__NR_io_destroy, sys_io_destroy),
   LINXY (__NR_io_getevents, sys_io_getevents),
   LINX_ (__NR_io_submit, sys_io_submit),
   LINXY (__NR_io_cancel, sys_io_cancel),
   LINX_ (__NR_exit_group, sys_exit_group),
   /* LINXY (__NR_lookup_dcookie, sys_lookup_dcookie), */
   LINXY (__NR_epoll_create, sys_epoll_create),
   /* LINXY(__NR_epoll_create1,sys_epoll_create1), */
   LINX_ (__NR_epoll_ctl, sys_epoll_ctl),
   LINXY (__NR_epoll_wait, sys_epoll_wait),
   PLAX_(__NR_rt_sigreturn,sys_rt_sigreturn),
   /* LINXY(__NR_fcntl64,sys_fcntl64), */
   LINX_ (__NR_set_tid_address, sys_set_tid_address),
   PLAX_ (__NR_semtimedop, sys_semtimedop),
   LINX_ (__NR_fadvise64, sys_fadvise64),
   LINXY (__NR_timer_create, sys_timer_create),
   LINXY (__NR_timer_settime, sys_timer_settime),
   LINXY (__NR_timer_gettime, sys_timer_gettime),
   LINX_ (__NR_timer_getoverrun, sys_timer_getoverrun),
   LINX_ (__NR_timer_delete, sys_timer_delete),
   LINX_ (__NR_clock_settime, sys_clock_settime),
   LINXY (__NR_clock_gettime, sys_clock_gettime),
   LINXY (__NR_clock_getres, sys_clock_getres),
   LINXY (__NR_clock_nanosleep, sys_clock_nanosleep),
   LINX_ (__NR_tgkill, sys_tgkill),
   GENX_ (__NR_utimes, sys_utimes),
   LINX_ (__NR_mbind, sys_mbind),
   LINXY (__NR_get_mempolicy, sys_get_mempolicy),
   LINX_ (__NR_set_mempolicy, sys_set_mempolicy),
   LINXY (__NR_mq_open, sys_mq_open),
   LINX_ (__NR_mq_unlink, sys_mq_unlink),
   LINX_ (__NR_mq_timedsend, sys_mq_timedsend),
   LINXY (__NR_mq_timedreceive, sys_mq_timedreceive),
   LINX_ (__NR_mq_notify, sys_mq_notify),
   LINXY (__NR_mq_getsetattr, sys_mq_getsetattr),
   GENX_ (__NR_vserver, sys_ni_syscall),
   LINXY (__NR_waitid, sys_waitid),
   LINX_ (__NR_add_key, sys_add_key),
   LINX_ (__NR_request_key, sys_request_key),
   LINXY (__NR_keyctl, sys_keyctl),
   PLAX_ (__NR_set_thread_area, sys_set_thread_area),
   LINX_ (__NR_inotify_init, sys_inotify_init),
   LINX_ (__NR_inotify_add_watch, sys_inotify_add_watch),
   LINX_ (__NR_inotify_rm_watch, sys_inotify_rm_watch),
   LINXY (__NR_openat, sys_openat),
   LINX_ (__NR_mkdirat, sys_mkdirat),
   LINX_ (__NR_mknodat, sys_mknodat),
   LINX_ (__NR_fchownat, sys_fchownat),
   LINX_ (__NR_futimesat, sys_futimesat),
   LINX_ (__NR_unlinkat, sys_unlinkat),
   LINX_ (__NR_renameat, sys_renameat),
   LINX_ (__NR_linkat, sys_linkat),
   LINX_ (__NR_symlinkat, sys_symlinkat),
   LINX_ (__NR_readlinkat, sys_readlinkat),
   LINX_ (__NR_fchmodat, sys_fchmodat),
   LINX_ (__NR_faccessat, sys_faccessat),
   LINX_ (__NR_pselect6, sys_pselect6),
   LINXY (__NR_ppoll, sys_ppoll),
   PLAX_ (__NR_unshare, sys_unshare),
   PLAX_ (__NR_splice, sys_splice),
   LINX_ (__NR_sync_file_range, sys_sync_file_range),
   PLAX_ (__NR_tee, sys_tee),
   PLAX_ (__NR_vmsplice, sys_vmsplice),
   LINX_ (__NR_set_robust_list, sys_set_robust_list),
   LINXY (__NR_get_robust_list, sys_get_robust_list),
   LINXY (__NR_epoll_pwait, sys_epoll_pwait),
   LINX_ (__NR_ioprio_set, sys_ioprio_set),
   LINX_ (__NR_ioprio_get, sys_ioprio_get),
   LINX_ (__NR_utimensat, sys_utimensat),
   LINXY (__NR_signalfd, sys_signalfd),
   LINXY (__NR_eventfd, sys_eventfd),
   LINX_ (__NR_fallocate, sys_fallocate),
   LINXY (__NR_timerfd_create, sys_timerfd_create),
   LINXY (__NR_timerfd_gettime, sys_timerfd_gettime),
   LINXY (__NR_timerfd_settime, sys_timerfd_settime)
};

SyscallTableEntry * ML_(get_linux_syscall_entry) ( UInt sysno )
{
   const UInt syscall_main_table_size
      = sizeof(syscall_main_table) / sizeof(syscall_main_table[0]);

   if (sysno < syscall_main_table_size) {
      SyscallTableEntry * sys = &syscall_main_table[sysno];
      if (sys->before == NULL)
         return NULL;  /* no entry */
      else
         return sys;
   }
   /* Can't find a wrapper */
   return NULL;
}

#endif  /* defined(VGP_mips64_linux) */

/*--------------------------------------------------------------------*/ 
/*--- end                                   syswrap-mips64-linux.c ---*/ 
/*--------------------------------------------------------------------*/ 
