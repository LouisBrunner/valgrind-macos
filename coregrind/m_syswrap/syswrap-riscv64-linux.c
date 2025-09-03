
/*--------------------------------------------------------------------*/
/*--- Platform-specific syscalls stuff.  syswrap-riscv64-linux.c -----*/
/*--------------------------------------------------------------------*/

/*
   This file is part of Valgrind, a dynamic binary instrumentation
   framework.

   Copyright (C) 2020-2023 Petr Pavlu
      petr.pavlu@dagobah.cz

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

#if defined(VGP_riscv64_linux)

#include "pub_core_basics.h"
#include "pub_core_libcassert.h"
#include "pub_core_libcprint.h"
#include "pub_core_options.h"
#include "pub_core_sigframe.h"
#include "pub_core_stacktrace.h"
#include "pub_core_syscall.h"
#include "pub_core_syswrap.h"
#include "pub_core_threadstate.h"
#include "pub_core_tooliface.h"
#include "pub_core_transtab.h"
#include "pub_core_vki.h"
#include "pub_core_vkiscnums.h"

#include "priv_syswrap-generic.h"
#include "priv_syswrap-linux.h"
#include "priv_types_n_macros.h"

/* ---------------------------------------------------------------------
   clone() handling
   ------------------------------------------------------------------ */

/* Call f(arg1), but first switch stacks, using 'stack' as the new stack, and
   use 'retaddr' as f's return-to address. Also, clear all the integer registers
   before entering f.*/
__attribute__((noreturn)) void ML_(call_on_new_stack_0_1)(Addr stack,
                                                          Addr retaddr,
                                                          void (*f)(Word),
                                                          Word arg1);
/* a0 = stack
   a1 = retaddr
   a2 = f
   a3 = arg1 */
asm(".text\n"
    ".globl vgModuleLocal_call_on_new_stack_0_1\n"
    "vgModuleLocal_call_on_new_stack_0_1:\n"
    "mv sp, a0\n" /* Set the stack pointer. */
    "mv ra, a1\n" /* Set the return address. */
    "mv a0, a3\n" /* Set the first argument. */
    "li t0, 0\n"  /* Clear our GPRs. */
    "li t1, 0\n"
    "li t2, 0\n"
    "li s0, 0\n"
    "li s1, 0\n"
    /* Don't zero out a0, already set to the first argument. */
    "li a1, 0\n"
    /* Don't zero out a2, holds the target function f(). */
    "li a3, 0\n"
    "li a4, 0\n"
    "li a5, 0\n"
    "li a6, 0\n"
    "li a7, 0\n"
    "li s2, 0\n"
    "li s3, 0\n"
    "li s4, 0\n"
    "li s5, 0\n"
    "li s6, 0\n"
    "li s7, 0\n"
    "li s8, 0\n"
    "li s9, 0\n"
    "li s10, 0\n"
    "li s11, 0\n"
    "li t3, 0\n"
    "li t4, 0\n"
    "li t5, 0\n"
    "li t6, 0\n"
    "jr a2\n"
    ".previous\n");

/* Perform a clone system call. Clone is strange because it has fork()-like
   return-twice semantics, so it needs special handling here.

   Upon entry, we have:

      Word (*fn)(void*)   in a0
      void*  child_stack  in a1
      int    flags        in a2
      void*  arg          in a3
      pid_t* child_tid    in a4
      pid_t* parent_tid   in a5
      void*  tls_ptr      in a6

   System call requires:

      int    $__NR_clone  in a7
      int    flags        in a0
      void*  child_stack  in a1
      pid_t* parent_tid   in a2
      void*  tls_ptr      in a3
      pid_t* child_tid    in a4

   Returns a Long encoded in the linux-riscv64 way, not a SysRes.
*/
#define __NR_CLONE VG_STRINGIFY(__NR_clone)
#define __NR_EXIT  VG_STRINGIFY(__NR_exit)

/* See priv_syswrap-linux.h for arg profile. */
asm(".text\n"
    ".globl do_syscall_clone_riscv64_linux\n"
    "do_syscall_clone_riscv64_linux:\n"
    /* Set up the child stack, temporarily preserving fn and arg. */
    "addi a1, a1, -16\n" /* Make space on the stack. */
    "sd a3, 8(a1)\n"     /* Save arg. */
    "sd a0, 0(a1)\n"     /* Save fn. */

    /* Setup the syscall. */
    "li a7, " __NR_CLONE "\n" /* Load the syscall number. */
    "mv a0, a2\n"             /* Load syscall arg1: flags. */
    "mv a1, a1\n"             /* Load syscall arg2: child_stack. */
    "mv a2, a5\n"             /* Load syscall arg3: parent_tid. */
    "mv a3, a6\n"             /* Load syscall arg4: tls_ptr. */
    "mv a4, a4\n"             /* Load syscall arg5: child_tid. */

    "ecall\n" /* clone() */

    "bnez a0, 1f\n" /* Child if retval == 0. */

    /* CHILD - call the thread function. */
    "ld a1, 0(sp)\n" /* Pop fn. */
    "ld a0, 8(sp)\n" /* Pop fn arg1: arg. */
    "addi sp, sp, 16\n"
    "jalr a1\n" /* Call fn. */

    /* Exit with result. */
    "mv a0, a0\n" /* arg1: return value from fn. */
    "li a7, " __NR_EXIT "\n"

    "ecall\n"

    /* Exit returned?! */
    "unimp\n"

    "1:\n" /* PARENT or ERROR. a0 holds return value from the clone syscall. */
    "ret\n"
    ".previous\n");

#undef __NR_CLONE
#undef __NR_EXIT

/* ---------------------------------------------------------------------
   More thread stuff
   ------------------------------------------------------------------ */

/* riscv64 doesn't have any architecture specific thread stuff that needs to be
   cleaned up. */
void VG_(cleanup_thread)(ThreadArchState* arch) {}

/* ---------------------------------------------------------------------
   PRE/POST wrappers for riscv64/Linux-specific syscalls
   ------------------------------------------------------------------ */

#define PRE(name)  DEFN_PRE_TEMPLATE(riscv64_linux, name)
#define POST(name) DEFN_POST_TEMPLATE(riscv64_linux, name)

/* ARG3 is only used for pointers into the traced process's address space and
   for offsets into the traced process's struct user_regs_struct. It is never
   a pointer into this process's memory space, and we should therefore not check
   anything it points to. */
static PRE(sys_ptrace)
{
   PRINT("sys_ptrace ( %ld, %ld, %#lx, %#lx )", SARG1, SARG2, ARG3, ARG4);
   PRE_REG_READ4(int, "ptrace", long, request, long, pid, long, addr, long,
                 data);
   switch (ARG1) {
   case VKI_PTRACE_TRACEME:
      break;
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
   case VKI_PTRACE_GETREGSET:
      ML_(linux_PRE_getregset)(tid, ARG3, ARG4);
      break;
   case VKI_PTRACE_SETREGSET:
      ML_(linux_PRE_setregset)(tid, ARG3, ARG4);
      break;
   default:
      VG_(umsg)("WARNING: unhandled ptrace request %ld.\n", SARG1);
      if (VG_(clo_verbosity) > 1)
         VG_(get_and_pp_StackTrace)(tid, VG_(clo_backtrace_size));
      break;
   }
}

static POST(sys_ptrace)
{
   switch (ARG1) {
   case VKI_PTRACE_TRACEME:
      ML_(linux_POST_traceme)(tid);
      break;
   case VKI_PTRACE_PEEKTEXT:
   case VKI_PTRACE_PEEKDATA:
   case VKI_PTRACE_PEEKUSR:
      POST_MEM_WRITE(ARG4, sizeof(long));
      break;
   case VKI_PTRACE_GETEVENTMSG:
      POST_MEM_WRITE(ARG4, sizeof(unsigned long));
      break;
   case VKI_PTRACE_GETSIGINFO:
      /* XXX: This is a simplification. Different parts of the siginfo_t are
         valid depending on the type of signal. */
      POST_MEM_WRITE(ARG4, sizeof(vki_siginfo_t));
      break;
   case VKI_PTRACE_GETREGSET:
      ML_(linux_POST_getregset)(tid, ARG3, ARG4);
      break;
   default:
      break;
   }
}

static PRE(sys_rt_sigreturn)
{
   /* See comments on PRE(sys_rt_sigreturn) in syswrap-amd64-linux.c for
      an explanation of what follows. */

   PRINT("rt_sigreturn ( )");

   vg_assert(VG_(is_valid_tid)(tid));
   vg_assert(tid >= 1 && tid < VG_N_THREADS);
   vg_assert(VG_(is_running_thread)(tid));

   /* Restore register state from frame and remove it. */
   VG_(sigframe_destroy)(tid, True);

   /* Tell the driver not to update the guest state with the "result", and set
      a bogus result to keep it happy. */
   *flags |= SfNoWriteResult;
   SET_STATUS_Success(0);

   /* Check to see if any signals arose as a result of this. */
   *flags |= SfPollAfter;
}

static PRE(sys_mmap)
{
   PRINT("sys_mmap ( %#lx, %lu, %lu, %#lx, %lu, %lu )", ARG1, ARG2, ARG3, ARG4,
         ARG5, ARG6);
   PRE_REG_READ6(long, "mmap", unsigned long, start, unsigned long, length,
                 unsigned long, prot, unsigned long, flags, unsigned long, fd,
                 unsigned long, offset);

   SysRes r =
      ML_(generic_PRE_sys_mmap)(tid, ARG1, ARG2, ARG3, ARG4, ARG5, ARG6);
   SET_STATUS_from_SysRes(r);
}

static PRE(sys_riscv_flush_icache)
{
   PRINT("sys_riscv_flush_icache ( %#lx, %lx, %#lx )", ARG1, ARG2, ARG3);
   PRE_REG_READ3(long, "riscv_flush_icache", unsigned long, start,
                 unsigned long, end, unsigned long, flags);

   VG_(discard_translations)((Addr)ARG1, (ULong)ARG2 - (ULong)ARG1,
                             "PRE(sys_riscv_flush_icache)");
   SET_STATUS_Success(0);
}

#undef PRE
#undef POST

/* ---------------------------------------------------------------------
   The riscv64/Linux syscall table
   ------------------------------------------------------------------ */

/* Add a riscv64-linux specific wrapper to a syscall table. */
#define PLAX_(sysno, name) WRAPPER_ENTRY_X_(riscv64_linux, sysno, name)
#define PLAXY(sysno, name) WRAPPER_ENTRY_XY(riscv64_linux, sysno, name)

/* This table maps from __NR_xxx syscall numbers to the appropriate PRE/POST
   sys_foo() wrappers on riscv64. */
static SyscallTableEntry syscall_main_table[] = {
   LINXY(__NR_io_setup, sys_io_setup),                             /* 0 */
   LINX_(__NR_io_destroy, sys_io_destroy),                         /* 1 */
   LINX_(__NR_io_submit, sys_io_submit),                           /* 2 */
   LINXY(__NR_io_cancel, sys_io_cancel),                           /* 3 */
   LINXY(__NR_io_getevents, sys_io_getevents),                     /* 4 */
   LINX_(__NR_setxattr, sys_setxattr),                             /* 5 */
   LINX_(__NR_lsetxattr, sys_lsetxattr),                           /* 6 */
   LINX_(__NR_fsetxattr, sys_fsetxattr),                           /* 7 */
   LINXY(__NR_getxattr, sys_getxattr),                             /* 8 */
   LINXY(__NR_lgetxattr, sys_lgetxattr),                           /* 9 */
   LINXY(__NR_fgetxattr, sys_fgetxattr),                           /* 10 */
   LINXY(__NR_listxattr, sys_listxattr),                           /* 11 */
   LINXY(__NR_llistxattr, sys_llistxattr),                         /* 12 */
   LINXY(__NR_flistxattr, sys_flistxattr),                         /* 13 */
   LINX_(__NR_removexattr, sys_removexattr),                       /* 14 */
   LINX_(__NR_lremovexattr, sys_lremovexattr),                     /* 15 */
   LINX_(__NR_fremovexattr, sys_fremovexattr),                     /* 16 */
   GENXY(__NR_getcwd, sys_getcwd),                                 /* 17 */
   LINXY(__NR_lookup_dcookie, sys_lookup_dcookie),                 /* 18 */
   LINXY(__NR_eventfd2, sys_eventfd2),                             /* 19 */
   LINXY(__NR_epoll_create1, sys_epoll_create1),                   /* 20 */
   LINX_(__NR_epoll_ctl, sys_epoll_ctl),                           /* 21 */
   LINXY(__NR_epoll_pwait, sys_epoll_pwait),                       /* 22 */
   GENXY(__NR_dup, sys_dup),                                       /* 23 */
   LINXY(__NR_dup3, sys_dup3),                                     /* 24 */
   LINXY(__NR_fcntl, sys_fcntl),                                   /* 25 */
   LINXY(__NR_inotify_init1, sys_inotify_init1),                   /* 26 */
   LINX_(__NR_inotify_add_watch, sys_inotify_add_watch),           /* 27 */
   LINX_(__NR_inotify_rm_watch, sys_inotify_rm_watch),             /* 28 */
   LINXY(__NR_ioctl, sys_ioctl),                                   /* 29 */
   LINX_(__NR_ioprio_set, sys_ioprio_set),                         /* 30 */
   LINX_(__NR_ioprio_get, sys_ioprio_get),                         /* 31 */
   GENX_(__NR_flock, sys_flock),                                   /* 32 */
   LINX_(__NR_mknodat, sys_mknodat),                               /* 33 */
   LINX_(__NR_mkdirat, sys_mkdirat),                               /* 34 */
   LINX_(__NR_unlinkat, sys_unlinkat),                             /* 35 */
   LINX_(__NR_symlinkat, sys_symlinkat),                           /* 36 */
   LINX_(__NR_linkat, sys_linkat),                                 /* 37 */
   LINX_(__NR_umount2, sys_umount),                                /* 39 */
   LINX_(__NR_mount, sys_mount),                                   /* 40 */
   LINX_(__NR_pivot_root, sys_pivot_root),                         /* 41 */
   GENXY(__NR_statfs, sys_statfs),                                 /* 43 */
   GENXY(__NR_fstatfs, sys_fstatfs),                               /* 44 */
   GENX_(__NR_truncate, sys_truncate),                             /* 45 */
   GENX_(__NR_ftruncate, sys_ftruncate),                           /* 46 */
   LINX_(__NR_fallocate, sys_fallocate),                           /* 47 */
   LINX_(__NR_faccessat, sys_faccessat),                           /* 48 */
   GENX_(__NR_chdir, sys_chdir),                                   /* 49 */
   GENX_(__NR_fchdir, sys_fchdir),                                 /* 50 */
   GENX_(__NR_chroot, sys_chroot),                                 /* 51 */
   GENX_(__NR_fchmod, sys_fchmod),                                 /* 52 */
   LINX_(__NR_fchmodat, sys_fchmodat),                             /* 53 */
   LINX_(__NR_fchownat, sys_fchownat),                             /* 54 */
   GENX_(__NR_fchown, sys_fchown),                                 /* 55 */
   LINXY(__NR_openat, sys_openat),                                 /* 56 */
   GENX_(__NR_close, sys_close),                                   /* 57 */
   LINX_(__NR_vhangup, sys_vhangup),                               /* 58 */
   LINXY(__NR_pipe2, sys_pipe2),                                   /* 59 */
   LINX_(__NR_quotactl, sys_quotactl),                             /* 60 */
   GENXY(__NR_getdents64, sys_getdents64),                         /* 61 */
   LINX_(__NR_lseek, sys_lseek),                                   /* 62 */
   GENXY(__NR_read, sys_read),                                     /* 63 */
   GENX_(__NR_write, sys_write),                                   /* 64 */
   GENXY(__NR_readv, sys_readv),                                   /* 65 */
   GENX_(__NR_writev, sys_writev),                                 /* 66 */
   GENXY(__NR_pread64, sys_pread64),                               /* 67 */
   GENX_(__NR_pwrite64, sys_pwrite64),                             /* 68 */
   LINXY(__NR_preadv, sys_preadv),                                 /* 69 */
   LINX_(__NR_pwritev, sys_pwritev),                               /* 70 */
   LINXY(__NR_sendfile, sys_sendfile),                             /* 71 */
   LINXY(__NR_pselect6, sys_pselect6),                             /* 72 */
   LINXY(__NR_ppoll, sys_ppoll),                                   /* 73 */
   LINXY(__NR_signalfd4, sys_signalfd4),                           /* 74 */
   LINXY(__NR_vmsplice, sys_vmsplice),                             /* 75 */
   LINX_(__NR_splice, sys_splice),                                 /* 76 */
   LINX_(__NR_tee, sys_tee),                                       /* 77 */
   LINXY(__NR_readlinkat, sys_readlinkat),                         /* 78 */
   LINXY(__NR_newfstatat, sys_newfstatat),                         /* 79 */
   GENXY(__NR_fstat, sys_newfstat),                                /* 80 */
   GENX_(__NR_sync, sys_sync),                                     /* 81 */
   GENX_(__NR_fsync, sys_fsync),                                   /* 82 */
   GENX_(__NR_fdatasync, sys_fdatasync),                           /* 83 */
   LINX_(__NR_sync_file_range, sys_sync_file_range),               /* 84 */
   LINXY(__NR_timerfd_create, sys_timerfd_create),                 /* 85 */
   LINXY(__NR_timerfd_settime, sys_timerfd_settime),               /* 86 */
   LINXY(__NR_timerfd_gettime, sys_timerfd_gettime),               /* 87 */
   LINX_(__NR_utimensat, sys_utimensat),                           /* 88 */
   GENX_(__NR_acct, sys_acct),                                     /* 89 */
   LINXY(__NR_capget, sys_capget),                                 /* 90 */
   LINX_(__NR_capset, sys_capset),                                 /* 91 */
   LINX_(__NR_personality, sys_personality),                       /* 92 */
   GENX_(__NR_exit, sys_exit),                                     /* 93 */
   LINX_(__NR_exit_group, sys_exit_group),                         /* 94 */
   LINXY(__NR_waitid, sys_waitid),                                 /* 95 */
   LINX_(__NR_set_tid_address, sys_set_tid_address),               /* 96 */
   LINX_(__NR_unshare, sys_unshare),                               /* 97 */
   LINXY(__NR_futex, sys_futex),                                   /* 98 */
   LINX_(__NR_set_robust_list, sys_set_robust_list),               /* 99 */
   LINXY(__NR_get_robust_list, sys_get_robust_list),               /* 100 */
   GENXY(__NR_nanosleep, sys_nanosleep),                           /* 101 */
   GENXY(__NR_getitimer, sys_getitimer),                           /* 102 */
   GENXY(__NR_setitimer, sys_setitimer),                           /* 103 */
   GENX_(__NR_kexec_load, sys_ni_syscall),                         /* 104 */
   LINX_(__NR_init_module, sys_init_module),                       /* 105 */
   LINX_(__NR_delete_module, sys_delete_module),                   /* 106 */
   LINXY(__NR_timer_create, sys_timer_create),                     /* 107 */
   LINXY(__NR_timer_gettime, sys_timer_gettime),                   /* 108 */
   LINX_(__NR_timer_getoverrun, sys_timer_getoverrun),             /* 109 */
   LINXY(__NR_timer_settime, sys_timer_settime),                   /* 110 */
   LINX_(__NR_timer_delete, sys_timer_delete),                     /* 111 */
   LINX_(__NR_clock_settime, sys_clock_settime),                   /* 112 */
   LINXY(__NR_clock_gettime, sys_clock_gettime),                   /* 113 */
   LINXY(__NR_clock_getres, sys_clock_getres),                     /* 114 */
   LINXY(__NR_clock_nanosleep, sys_clock_nanosleep),               /* 115 */
   LINXY(__NR_syslog, sys_syslog),                                 /* 116 */
   PLAXY(__NR_ptrace, sys_ptrace),                                 /* 117 */
   LINXY(__NR_sched_setparam, sys_sched_setparam),                 /* 118 */
   LINX_(__NR_sched_setscheduler, sys_sched_setscheduler),         /* 119 */
   LINX_(__NR_sched_getscheduler, sys_sched_getscheduler),         /* 120 */
   LINXY(__NR_sched_getparam, sys_sched_getparam),                 /* 121 */
   LINX_(__NR_sched_setaffinity, sys_sched_setaffinity),           /* 122 */
   LINXY(__NR_sched_getaffinity, sys_sched_getaffinity),           /* 123 */
   LINX_(__NR_sched_yield, sys_sched_yield),                       /* 124 */
   LINX_(__NR_sched_get_priority_max, sys_sched_get_priority_max), /* 125 */
   LINX_(__NR_sched_get_priority_min, sys_sched_get_priority_min), /* 126 */
   LINXY(__NR_sched_rr_get_interval, sys_sched_rr_get_interval),   /* 127 */
   GENX_(__NR_kill, sys_kill),                                     /* 129 */
   LINXY(__NR_tkill, sys_tkill),                                   /* 130 */
   LINX_(__NR_tgkill, sys_tgkill),                                 /* 131 */
   GENXY(__NR_sigaltstack, sys_sigaltstack),                       /* 132 */
   LINX_(__NR_rt_sigsuspend, sys_rt_sigsuspend),                   /* 133 */
   LINXY(__NR_rt_sigaction, sys_rt_sigaction),                     /* 134 */
   LINXY(__NR_rt_sigprocmask, sys_rt_sigprocmask),                 /* 135 */
   LINXY(__NR_rt_sigpending, sys_rt_sigpending),                   /* 136 */
   LINXY(__NR_rt_sigtimedwait, sys_rt_sigtimedwait),               /* 137 */
   LINXY(__NR_rt_sigqueueinfo, sys_rt_sigqueueinfo),               /* 138 */
   PLAX_(__NR_rt_sigreturn, sys_rt_sigreturn),                     /* 139 */
   GENX_(__NR_setpriority, sys_setpriority),                       /* 140 */
   GENX_(__NR_getpriority, sys_getpriority),                       /* 141 */
   GENX_(__NR_setregid, sys_setregid),                             /* 143 */
   GENX_(__NR_setgid, sys_setgid),                                 /* 144 */
   GENX_(__NR_setreuid, sys_setreuid),                             /* 145 */
   GENX_(__NR_setuid, sys_setuid),                                 /* 146 */
   LINX_(__NR_setresuid, sys_setresuid),                           /* 147 */
   LINXY(__NR_getresuid, sys_getresuid),                           /* 148 */
   LINX_(__NR_setresgid, sys_setresgid),                           /* 149 */
   LINXY(__NR_getresgid, sys_getresgid),                           /* 150 */
   LINX_(__NR_setfsuid, sys_setfsuid),                             /* 151 */
   LINX_(__NR_setfsgid, sys_setfsgid),                             /* 152 */
   GENXY(__NR_times, sys_times),                                   /* 153 */
   GENX_(__NR_setpgid, sys_setpgid),                               /* 154 */
   GENX_(__NR_getpgid, sys_getpgid),                               /* 155 */
   GENX_(__NR_getsid, sys_getsid),                                 /* 156 */
   GENX_(__NR_setsid, sys_setsid),                                 /* 157 */
   GENXY(__NR_getgroups, sys_getgroups),                           /* 158 */
   GENX_(__NR_setgroups, sys_setgroups),                           /* 159 */
   GENXY(__NR_uname, sys_newuname),                                /* 160 */
   GENX_(__NR_sethostname, sys_sethostname),                       /* 161 */
   GENXY(__NR_getrlimit, sys_getrlimit),                           /* 163 */
   GENX_(__NR_setrlimit, sys_setrlimit),                           /* 164 */
   GENXY(__NR_getrusage, sys_getrusage),                           /* 165 */
   GENX_(__NR_umask, sys_umask),                                   /* 166 */
   LINXY(__NR_prctl, sys_prctl),                                   /* 167 */
   LINXY(__NR_getcpu, sys_getcpu),                                 /* 168 */
   GENXY(__NR_gettimeofday, sys_gettimeofday),                     /* 169 */
   GENX_(__NR_settimeofday, sys_settimeofday),                     /* 170 */
   LINXY(__NR_adjtimex, sys_adjtimex),                             /* 171 */
   GENX_(__NR_getpid, sys_getpid),                                 /* 172 */
   GENX_(__NR_getppid, sys_getppid),                               /* 173 */
   GENX_(__NR_getuid, sys_getuid),                                 /* 174 */
   GENX_(__NR_geteuid, sys_geteuid),                               /* 175 */
   GENX_(__NR_getgid, sys_getgid),                                 /* 176 */
   GENX_(__NR_getegid, sys_getegid),                               /* 177 */
   LINX_(__NR_gettid, sys_gettid),                                 /* 178 */
   LINXY(__NR_sysinfo, sys_sysinfo),                               /* 179 */
   LINXY(__NR_mq_open, sys_mq_open),                               /* 180 */
   LINX_(__NR_mq_unlink, sys_mq_unlink),                           /* 181 */
   LINX_(__NR_mq_timedsend, sys_mq_timedsend),                     /* 182 */
   LINXY(__NR_mq_timedreceive, sys_mq_timedreceive),               /* 183 */
   LINX_(__NR_mq_notify, sys_mq_notify),                           /* 184 */
   LINXY(__NR_mq_getsetattr, sys_mq_getsetattr),                   /* 185 */
   LINX_(__NR_msgget, sys_msgget),                                 /* 186 */
   LINXY(__NR_msgctl, sys_msgctl),                                 /* 187 */
   LINXY(__NR_msgrcv, sys_msgrcv),                                 /* 188 */
   LINX_(__NR_msgsnd, sys_msgsnd),                                 /* 189 */
   LINX_(__NR_semget, sys_semget),                                 /* 190 */
   LINXY(__NR_semctl, sys_semctl),                                 /* 191 */
   LINX_(__NR_semtimedop, sys_semtimedop),                         /* 192 */
   LINX_(__NR_semop, sys_semop),                                   /* 193 */
   LINX_(__NR_shmget, sys_shmget),                                 /* 194 */
   LINXY(__NR_shmctl, sys_shmctl),                                 /* 195 */
   LINXY(__NR_shmat, sys_shmat),                                   /* 196 */
   LINXY(__NR_shmdt, sys_shmdt),                                   /* 197 */
   LINXY(__NR_socket, sys_socket),                                 /* 198 */
   LINXY(__NR_socketpair, sys_socketpair),                         /* 199 */
   LINX_(__NR_bind, sys_bind),                                     /* 200 */
   LINX_(__NR_listen, sys_listen),                                 /* 201 */
   LINXY(__NR_accept, sys_accept),                                 /* 202 */
   LINX_(__NR_connect, sys_connect),                               /* 203 */
   LINXY(__NR_getsockname, sys_getsockname),                       /* 204 */
   LINXY(__NR_getpeername, sys_getpeername),                       /* 205 */
   LINX_(__NR_sendto, sys_sendto),                                 /* 206 */
   LINXY(__NR_recvfrom, sys_recvfrom),                             /* 207 */
   LINX_(__NR_setsockopt, sys_setsockopt),                         /* 208 */
   LINXY(__NR_getsockopt, sys_getsockopt),                         /* 209 */
   LINX_(__NR_shutdown, sys_shutdown),                             /* 210 */
   LINX_(__NR_sendmsg, sys_sendmsg),                               /* 211 */
   LINXY(__NR_recvmsg, sys_recvmsg),                               /* 212 */
   LINX_(__NR_readahead, sys_readahead),                           /* 213 */
   GENX_(__NR_brk, sys_brk),                                       /* 214 */
   GENXY(__NR_munmap, sys_munmap),                                 /* 215 */
   GENX_(__NR_mremap, sys_mremap),                                 /* 216 */
   LINX_(__NR_add_key, sys_add_key),                               /* 217 */
   LINX_(__NR_request_key, sys_request_key),                       /* 218 */
   LINXY(__NR_keyctl, sys_keyctl),                                 /* 219 */
   LINX_(__NR_clone, sys_clone),                                   /* 220 */
   GENX_(__NR_execve, sys_execve),                                 /* 221 */
   PLAX_(__NR_mmap, sys_mmap),                                     /* 222 */
   GENX_(__NR_fadvise64, sys_ni_syscall),                          /* 223 */
   GENXY(__NR_mprotect, sys_mprotect),                             /* 226 */
   GENX_(__NR_msync, sys_msync),                                   /* 227 */
   GENX_(__NR_mlock, sys_mlock),                                   /* 228 */
   GENX_(__NR_munlock, sys_munlock),                               /* 229 */
   GENX_(__NR_mlockall, sys_mlockall),                             /* 230 */
   LINX_(__NR_munlockall, sys_munlockall),                         /* 231 */
   GENXY(__NR_mincore, sys_mincore),                               /* 232 */
   GENX_(__NR_madvise, sys_madvise),                               /* 233 */
   LINX_(__NR_remap_file_pages, sys_remap_file_pages),             /* 234 */
   LINX_(__NR_mbind, sys_mbind),                                   /* 235 */
   LINXY(__NR_get_mempolicy, sys_get_mempolicy),                   /* 236 */
   LINX_(__NR_set_mempolicy, sys_set_mempolicy),                   /* 237 */
   LINXY(__NR_move_pages, sys_move_pages),                         /* 239 */
   LINXY(__NR_rt_tgsigqueueinfo, sys_rt_tgsigqueueinfo),           /* 240 */
   LINXY(__NR_perf_event_open, sys_perf_event_open),               /* 241 */
   LINXY(__NR_accept4, sys_accept4),                               /* 242 */
   LINXY(__NR_recvmmsg, sys_recvmmsg),                             /* 243 */
   GENX_(__NR_riscv_hwprobe, sys_ni_syscall),                      /* 258 */
   PLAX_(__NR_riscv_flush_icache, sys_riscv_flush_icache),         /* 259 */
   GENXY(__NR_wait4, sys_wait4),                                   /* 260 */
   LINXY(__NR_prlimit64, sys_prlimit64),                           /* 261 */
   LINXY(__NR_fanotify_init, sys_fanotify_init),                   /* 262 */
   LINX_(__NR_fanotify_mark, sys_fanotify_mark),                   /* 263 */
   LINXY(__NR_name_to_handle_at, sys_name_to_handle_at),           /* 264 */
   LINXY(__NR_open_by_handle_at, sys_open_by_handle_at),           /* 265 */
   LINXY(__NR_clock_adjtime, sys_clock_adjtime),                   /* 266 */
   LINX_(__NR_syncfs, sys_syncfs),                                 /* 267 */
   LINX_(__NR_setns, sys_setns),                                   /* 268 */
   LINXY(__NR_sendmmsg, sys_sendmmsg),                             /* 269 */
   LINXY(__NR_process_vm_readv, sys_process_vm_readv),             /* 270 */
   LINX_(__NR_process_vm_writev, sys_process_vm_writev),           /* 271 */
   LINX_(__NR_kcmp, sys_kcmp),                                     /* 272 */
   LINX_(__NR_finit_module, sys_finit_module),                     /* 273 */
   LINX_(__NR_sched_setattr, sys_sched_setattr),                   /* 274 */
   LINXY(__NR_sched_getattr, sys_sched_getattr),                   /* 275 */
   LINX_(__NR_renameat2, sys_renameat2),                           /* 276 */
   LINXY(__NR_getrandom, sys_getrandom),                           /* 278 */
   LINXY(__NR_memfd_create, sys_memfd_create),                     /* 279 */
   LINXY(__NR_bpf, sys_bpf),                                       /* 280 */
   LINX_(__NR_execveat, sys_execveat),                             /* 281 */
   LINXY(__NR_userfaultfd, sys_userfaultfd),                       /* 282 */
   LINX_(__NR_membarrier, sys_membarrier),                         /* 283 */
   GENX_(__NR_mlock2, sys_mlock2),                                 /* 284 */
   LINX_(__NR_copy_file_range, sys_copy_file_range),               /* 285 */
   LINXY(__NR_preadv2, sys_preadv2),                               /* 286 */
   LINX_(__NR_pwritev2, sys_pwritev2),                             /* 287 */
   LINXY(__NR_statx, sys_statx),                                   /* 291 */
   LINX_(__NR_io_pgetevents, sys_io_pgetevents),                   /* 292 */
   GENX_(__NR_rseq, sys_ni_syscall),                               /* 293 */
   LINXY(__NR_io_uring_setup, sys_io_uring_setup),                 /* 425 */
   LINXY(__NR_io_uring_enter, sys_io_uring_enter),                 /* 426 */
   LINXY(__NR_io_uring_register, sys_io_uring_register),           /* 427 */
   LINXY(__NR_open_tree, sys_open_tree),                           /* 428 */
   LINX_(__NR_move_mount, sys_move_mount),                         /* 429 */
   LINXY(__NR_fsopen, sys_fsopen),                                 /* 430 */
   LINX_(__NR_fsconfig, sys_fsconfig),                             /* 431 */
   LINXY(__NR_fsmount, sys_fsmount),                               /* 432 */
   LINXY(__NR_fspick, sys_fspick),                                 /* 433 */
   LINXY(__NR_pidfd_open, sys_pidfd_open),                         /* 434 */
   GENX_(__NR_clone3, sys_ni_syscall),                             /* 435 */
   LINXY(__NR_close_range, sys_close_range),                       /* 436 */
   LINXY(__NR_openat2, sys_openat2),                               /* 437 */
   LINXY(__NR_pidfd_getfd, sys_pidfd_getfd),                       /* 438 */
   LINX_(__NR_faccessat2, sys_faccessat2),                         /* 439 */
   LINXY(__NR_epoll_pwait2, sys_epoll_pwait2),                     /* 441 */
   LINXY(__NR_landlock_create_ruleset, sys_landlock_create_ruleset), /* 444 */
   LINX_(__NR_landlock_add_rule, sys_landlock_add_rule),           /* 445 */
   LINX_(__NR_landlock_restrict_self, sys_landlock_restrict_self), /* 446 */
   LINXY(__NR_memfd_secret, sys_memfd_secret),                     /* 447 */
   LINXY(__NR_cachestat, sys_cachestat),                           /* 451 */
   LINX_(__NR_fchmodat2, sys_fchmodat2),                           /* 452 */
   LINXY(__NR_statmount, sys_statmount),                           /* 457 */
   LINXY(__NR_listmount, sys_listmount),                           /* 458 */
   LINX_(__NR_mseal, sys_mseal),                                   /* 462 */
};

SyscallTableEntry* ML_(get_linux_syscall_entry)(UInt sysno)
{
   const UInt syscall_main_table_size =
      sizeof(syscall_main_table) / sizeof(syscall_main_table[0]);

   /* Is it in the contiguous initial section of the table? */
   if (sysno < syscall_main_table_size) {
      SyscallTableEntry* sys = &syscall_main_table[sysno];
      if (sys->before == NULL)
         return NULL; /* no entry */
      else
         return sys;
   }

   /* Can't find a wrapper. */
   return NULL;
}

#endif // defined(VGP_riscv64_linux)

/*--------------------------------------------------------------------*/
/*--- end                                  syswrap-riscv64-linux.c ---*/
/*--------------------------------------------------------------------*/
