
/*--------------------------------------------------------------------*/
/*--- Platform-specific syscalls stuff.     amd64-linux/syscalls.c ---*/
/*--------------------------------------------------------------------*/

/*
   This file is part of Valgrind, a dynamic binary instrumentation
   framework.

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

// See the comment accompanying the declaration of VGA_(thread_syscall)() in
// coregrind/core.h for an explanation of what this does, and why.
asm(
".text\n"
"	.type vgArch_do_thread_syscall,@function\n"

".globl  vgArch_do_thread_syscall\n"
"vgArch_do_thread_syscall:\n"
"	pushq	%r15\n"
"	pushq	%r14\n"
"	pushq	%r13\n"
"	pushq	%r12\n"
"	pushq	%rbx\n"
"	pushq	%rbp\n"
".vgArch_sys_before:\n"

/* Params:
        rdi = UWord sys
        rsi = UWord arg1
        rdx = UWord arg2
        rcx = UWord arg3
        r8  = UWord arg4
        r9  = UWord arg5
   Stack now looks like this (remaining args pushed R->L):
        Int    poststate    80
        Int*   statep       72
        HWord* result       64
        UWord  arg6         56
        ReturnAddress       48
        r15                 40
        r14                 32
        r13                 24
        r12                 16
        rbx                 8
        rbp                 0+rsp
*/

/* Convert function calling convention --> syscall calling convention */
"	movq	%rdi, %rax\n"         /* syscall */
"	movq	%rsi, %rdi\n"         /* arg1 */
"	movq	%rdx, %rsi\n"         /* arg2 */
"	movq	%rcx, %rdx\n"         /* arg3 */
"	movq	%r8,  %r10\n"         /* arg4 */
"	movq	%r9,  %r8\n"          /* arg5 */
"	movq    56(%rsp), %r9\n"      /* arg6 */ /* last arg from stack */
".vgArch_sys_restarted:\n"
"	syscall\n"
".vgArch_sys_after:\n"
"	movq	64(%rsp),%rbx\n"	/* rbx = HWord* result */
"	movq	%rax, (%rbx)\n"		/* write the syscall retval */

"	movl	72(%esp),%ebx\n"	/* rbx = Int* stateP */
"	testl	%ebx, %ebx\n"
"	jz	1f\n"

"	movl	80(%rsp),%ecx\n"	/* write the post state (must be after retval write) */
"	movl	%ecx,(%rbx)\n"

".vgArch_sys_done:\n"			/* OK, all clear from here */
"1:	popq	%rbp\n"
"	popq	%rbx\n"
"	popq	%r12\n"
"	popq	%r13\n"
"	popq	%r14\n"
"	popq	%r15\n"
"	ret\n"
"	.size vgArch_do_thread_syscall,.-vgArch_do_thread_syscall\n"
".previous\n"

".section .rodata\n"
"       .globl  vgArch_sys_before\n"
"vgArch_sys_before:	.long	.vgArch_sys_before\n"
"       .globl  vgArch_sys_restarted\n"
"vgArch_sys_restarted:	.long	.vgArch_sys_restarted\n"
"       .globl  vgArch_sys_after\n"
"vgArch_sys_after:	.long	.vgArch_sys_after\n"
"       .globl  vgArch_sys_done\n"
"vgArch_sys_done:	.long	.vgArch_sys_done\n"
".previous\n"
);



// Back up to restart a system call.
void VGA_(restart_syscall)(ThreadArchState *arch)
{
   I_die_here;
#if 0
   arch->vex.guest_EIP -= 2;             // sizeof(int $0x80)

   /* Make sure our caller is actually sane, and we're really backing
      back over a syscall.

      int $0x80 == CD 80 
   */
   {
      UChar *p = (UChar *)arch->vex.guest_EIP;
      
      if (p[0] != 0xcd || p[1] != 0x80)
         VG_(message)(Vg_DebugMsg,
                      "?! restarting over syscall at %p %02x %02x\n",
                      arch->vex.guest_EIP, p[0], p[1]); 

      vg_assert(p[0] == 0xcd && p[1] == 0x80);
   }
#endif
}

/* ---------------------------------------------------------------------
   PRE/POST wrappers for AMD64/Linux-specific syscalls
   ------------------------------------------------------------------ */

// Nb: See the comment above the generic PRE/POST wrappers in
// coregrind/vg_syscalls.c for notes about how they work.

#define PRE(name, f)     PRE_TEMPLATE(static, amd64_linux, name, f)
#define POST(name)      POST_TEMPLATE(static, amd64_linux, name)

PRE(sys_arch_prctl, 0)
{
   // Nb: this wrapper might actually need a corresponding POST wrapper, and
   // also a non-0 flag in the line above.  In other words, don't believe
   // anything about the current stub implementation.
   I_die_here;
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
   //   (__NR_write,             sys_write),          // 1 
   GENX_(__NR_open,              sys_open),           // 2 
   GENXY(__NR_close,             sys_close),          // 3 
   GENXY(__NR_stat,              sys_newstat),        // 4 

   GENXY(__NR_fstat,             sys_newfstat),       // 5 
   //   (__NR_lstat,             sys_newlstat),       // 6 
   //   (__NR_poll,              sys_poll),           // 7 
   GENX_(__NR_lseek,             sys_lseek),          // 8 
   GENXY(__NR_mmap,              sys_mmap2),          // 9 

   GENXY(__NR_mprotect,          sys_mprotect),       // 10 
   //   (__NR_munmap,            sys_munmap),         // 11 
   GENX_(__NR_brk,               sys_brk),            // 12 
   //   (__NR_rt_sigaction,      sys_rt_sigaction),   // 13 
   //   (__NR_rt_sigprocmask,    sys_rt_sigprocmask), // 14 

   //   (__NR_rt_sigreturn,      stub_rt_sigreturn),  // 15 
   //   (__NR_ioctl,             sys_ioctl),          // 16 
   //   (__NR_pread64,           sys_pread64),        // 17 
   //   (__NR_pwrite64,          sys_pwrite64),       // 18 
   //   (__NR_readv,             sys_readv),          // 19 

   GENX_(__NR_writev,            sys_writev),         // 20 
   GENX_(__NR_access,            sys_access),         // 21 
   //   (__NR_pipe,              sys_pipe),           // 22 
   //   (__NR_select,            sys_select),         // 23 
   //   (__NR_sched_yield,       sys_sched_yield),    // 24 

   //   (__NR_mremap,            sys_mremap),         // 25 
   //   (__NR_msync,             sys_msync),          // 26 
   //   (__NR_mincore,           sys_mincore),        // 27 
   GENX_(__NR_madvise,           sys_madvise),        // 28 
   //   (__NR_shmget,            sys_shmget),         // 29 

   //   (__NR_shmat,             wrap_sys_shmat),     // 30 
   //   (__NR_shmctl,            sys_shmctl),         // 31 
   //   (__NR_dup,               sys_dup),            // 32 
   //   (__NR_dup2,              sys_dup2),           // 33 
   //   (__NR_pause,             sys_pause),          // 34 

   //   (__NR_nanosleep,         sys_nanosleep),      // 35 
   //   (__NR_getitimer,         sys_getitimer),      // 36 
   //   (__NR_alarm,             sys_alarm),          // 37 
   //   (__NR_setitimer,         sys_setitimer),      // 38 
   //   (__NR_getpid,            sys_getpid),         // 39 

   //   (__NR_sendfile,          sys_sendfile64),     // 40 
   //   (__NR_socket,            sys_socket),         // 41 
   //   (__NR_connect,           sys_connect),        // 42 
   //   (__NR_accept,            sys_accept),         // 43 
   //   (__NR_sendto,            sys_sendto),         // 44 

   //   (__NR_recvfrom,          sys_recvfrom),       // 45 
   //   (__NR_sendmsg,           sys_sendmsg),        // 46 
   //   (__NR_recvmsg,           sys_recvmsg),        // 47 
   //   (__NR_shutdown,          sys_shutdown),       // 48 
   //   (__NR_bind,              sys_bind),           // 49 

   //   (__NR_listen,            sys_listen),         // 50 
   //   (__NR_getsockname,       sys_getsockname),    // 51 
   //   (__NR_getpeername,       sys_getpeername),    // 52 
   //   (__NR_socketpair,        sys_socketpair),     // 53 
   //   (__NR_setsockopt,        sys_setsockopt),     // 54 

   //   (__NR_getsockopt,        sys_getsockopt),     // 55 
   //   (__NR_clone,             stub_clone),         // 56 
   //   (__NR_fork,              stub_fork),          // 57 
   //   (__NR_vfork,             stub_vfork),         // 58 
   //   (__NR_execve,            stub_execve),        // 59 

   GENX_(__NR_exit,              sys_exit),           // 60
   //   (__NR_wait4,             sys_wait4),          // 61 
   //   (__NR_kill,              sys_kill),           // 62 
   GENXY(__NR_uname,             sys_newuname),       // 63 
   //   (__NR_semget,            sys_semget),         // 64 

   //   (__NR_semop,             sys_semop),          // 65 
   //   (__NR_semctl,            sys_semctl),         // 66 
   //   (__NR_shmdt,             sys_shmdt),          // 67 
   //   (__NR_msgget,            sys_msgget),         // 68 
   //   (__NR_msgsnd,            sys_msgsnd),         // 69 

   //   (__NR_msgrcv,            sys_msgrcv),         // 70 
   //   (__NR_msgctl,            sys_msgctl),         // 71 
   //   (__NR_fcntl,             sys_fcntl),          // 72 
   //   (__NR_flock,             sys_flock),          // 73 
   //   (__NR_fsync,             sys_fsync),          // 74 

   //   (__NR_fdatasync,         sys_fdatasync),      // 75 
   //   (__NR_truncate,          sys_truncate),       // 76 
   //   (__NR_ftruncate,         sys_ftruncate),      // 77 
   //   (__NR_getdents,          sys_getdents),       // 78 
   //   (__NR_getcwd,            sys_getcwd),         // 79 

   //   (__NR_chdir,             sys_chdir),          // 80 
   //   (__NR_fchdir,            sys_fchdir),         // 81 
   //   (__NR_rename,            sys_rename),         // 82 
   //   (__NR_mkdir,             sys_mkdir),          // 83 
   //   (__NR_rmdir,             sys_rmdir),          // 84 

   //   (__NR_creat,             sys_creat),          // 85 
   //   (__NR_link,              sys_link),           // 86 
   //   (__NR_unlink,            sys_unlink),         // 87 
   //   (__NR_symlink,           sys_symlink),        // 88 
   //   (__NR_readlink,          sys_readlink),       // 89 

   //   (__NR_chmod,             sys_chmod),          // 90 
   //   (__NR_fchmod,            sys_fchmod),         // 91 
   //   (__NR_chown,             sys_chown),          // 92 
   //   (__NR_fchown,            sys_fchown),         // 93 
   //   (__NR_lchown,            sys_lchown),         // 94 

   //   (__NR_umask,             sys_umask),          // 95 
   //   (__NR_gettimeofday,      sys_gettimeofday),   // 96 
   //   (__NR_getrlimit,         sys_getrlimit),      // 97 
   //   (__NR_getrusage,         sys_getrusage),      // 98 
   //   (__NR_sysinfo,           sys_sysinfo),        // 99 

   //   (__NR_times,             sys_times),          // 100 
   //   (__NR_ptrace,            sys_ptrace),         // 101 
   //   (__NR_getuid,            sys_getuid),         // 102 
   //   (__NR_syslog,            sys_syslog),         // 103 
   //   (__NR_getgid,            sys_getgid),         // 104 

   //   (__NR_setuid,            sys_setuid),         // 105 
   //   (__NR_setgid,            sys_setgid),         // 106 
   //   (__NR_geteuid,           sys_geteuid),        // 107 
   //   (__NR_getegid,           sys_getegid),        // 108 
   //   (__NR_setpgid,           sys_setpgid),        // 109 

   //   (__NR_getppid,           sys_getppid),        // 110 
   //   (__NR_getpgrp,           sys_getpgrp),        // 111 
   //   (__NR_setsid,            sys_setsid),         // 112 
   //   (__NR_setreuid,          sys_setreuid),       // 113 
   //   (__NR_setregid,          sys_setregid),       // 114 

   //   (__NR_getgroups,         sys_getgroups),      // 115 
   //   (__NR_setgroups,         sys_setgroups),      // 116 
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
   //   (__NR_rt_sigpending,     sys_rt_sigpending),  // 127 
   //   (__NR_rt_sigtimedwait,   sys_rt_sigtimedwait),// 128 
   //   (__NR_rt_sigqueueinfo,   sys_rt_sigqueueinfo),// 129 

   //   (__NR_rt_sigsuspend,     stub_rt_sigsuspend), // 130 
   //   (__NR_sigaltstack,       stub_sigaltstack),   // 131 
   //   (__NR_utime,             sys_utime),          // 132 
   //   (__NR_mknod,             sys_mknod),          // 133 
   //   (__NR_uselib,            sys_uselib),         // 134 

   //   (__NR_personality,       sys_personality),    // 135 
   //   (__NR_ustat,             sys_ustat),          // 136 
   //   (__NR_statfs,            sys_statfs),         // 137 
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
   //   (__NR__sysctl,           sys_sysctl),         // 156 
   //   (__NR_prctl,             sys_prctl),          // 157 
   PLAX_(__NR_arch_prctl,	 sys_arch_prctl),     // 158 
   //   (__NR_adjtimex,          sys_adjtimex),       // 159 

   //   (__NR_setrlimit,         sys_setrlimit),      // 160 
   //   (__NR_chroot,            sys_chroot),         // 161 
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
   //   (__NR_gettid,            sys_gettid),         // 186 
   //   (__NR_readahead,         sys_readahead),      // 187 
   //   (__NR_setxattr,          sys_setxattr),       // 188 
   //   (__NR_lsetxattr,         sys_lsetxattr),      // 189 

   //   (__NR_fsetxattr,         sys_fsetxattr),      // 190 
   //   (__NR_getxattr,          sys_getxattr),       // 191 
   //   (__NR_lgetxattr,         sys_lgetxattr),      // 192 
   //   (__NR_fgetxattr,         sys_fgetxattr),      // 193 
   //   (__NR_listxattr,         sys_listxattr),      // 194 

   //   (__NR_llistxattr,        sys_llistxattr),     // 195 
   //   (__NR_flistxattr,        sys_flistxattr),     // 196 
   //   (__NR_removexattr,       sys_removexattr),    // 197 
   //   (__NR_lremovexattr,      sys_lremovexattr),   // 198 
   //   (__NR_fremovexattr,      sys_fremovexattr),   // 199 

   //   (__NR_tkill,             sys_tkill),             // 200 
   //   (__NR_time,              sys_time64),            // 201 
   //   (__NR_futex,             sys_futex),             // 202 
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
   //   (__NR_getdents64,        sys_getdents64),     // 217 
   //   (__NR_set_tid_address,   sys_set_tid_address),// 218 
   //   (__NR_restart_syscall,   sys_restart_syscall),// 219 

   //   (__NR_semtimedop,        sys_semtimedop),     // 220 
   //   (__NR_fadvise64,         sys_fadvise64),      // 221 
   //   (__NR_timer_create,      sys_timer_create),   // 222 
   //   (__NR_timer_settime,     sys_timer_settime),  // 223 
   //   (__NR_timer_gettime,     sys_timer_gettime),  // 224 

   //   (__NR_timer_getoverrun,  sys_timer_getoverrun)// 225 
   //   (__NR_timer_delete,      sys_timer_delete),   // 226 
   //   (__NR_clock_settime,     sys_clock_settime),  // 227 
   //   (__NR_clock_gettime,     sys_clock_gettime),  // 228 
   //   (__NR_clock_getres,      sys_clock_getres),   // 229 

   //   (__NR_clock_nanosleep,   sys_clock_nanosleep),// 230 
   //   (__NR_exit_group,        sys_exit_group),     // 231 
   //   (__NR_epoll_wait,        sys_epoll_wait),     // 232 
   //   (__NR_epoll_ctl,         sys_epoll_ctl),      // 233 
   //   (__NR_tgkill,            sys_tgkill),         // 234 

   //   (__NR_utimes,            sys_utimes),         // 235 
   //   (__NR_vserver,           sys_ni_syscall),     // 236 
   //   (__NR_vserver,           sys_ni_syscall),     // 236 
   //   (__NR_mbind,             sys_mbind),          // 237 
   //   (__NR_set_mempolicy,     sys_set_mempolicy),  // 238 

   //   (__NR_get_mempolicy,     sys_get_mempolicy),  // 239 
   //   (__NR_mq_open,           sys_mq_open),        // 240 
   //   (__NR_mq_unlink,         sys_mq_unlink),      // 241 
   //   (__NR_mq_timedsend,      sys_mq_timedsend),   // 242 
   //   (__NR_mq_timedreceive,   sys_mq_timedreceive),// 243 

   //   (__NR_mq_notify,         sys_mq_notify),      // 244 
   //   (__NR_mq_getsetattr,     sys_mq_getsetattr),  // 245 
   //   (__NR_kexec_load,        sys_ni_syscall),     // 246 
   //   (__NR_waitid,            sys_waitid),         // 247 
};

const UInt VGA_(syscall_table_size) = 
            sizeof(VGA_(syscall_table)) / sizeof(VGA_(syscall_table)[0]);

//void        VG_(clear_TLS_for_thread)      ( VgLdtEntry* tls )
//{
//}

/*--------------------------------------------------------------------*/
/*--- end                                                          ---*/
/*--------------------------------------------------------------------*/
