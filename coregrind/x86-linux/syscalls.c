
/*--------------------------------------------------------------------*/
/*--- x86/Linux-specific syscalls, etc.       x86-linux/syscalls.c ---*/
/*--------------------------------------------------------------------*/

/*
   This file is part of Valgrind, an extensible x86 protected-mode
   emulator for monitoring program execution on x86-Unixes.

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

#include "syscall_wrappers.h"

/* We need our own copy of VG_(do_syscall)() to handle a special
   race-condition.  If we've got signals unblocked, and we take a
   signal in the gap either just before or after the syscall, we may
   end up not running the syscall at all, or running it more than
   once.

   The solution is to make the signal handler derive the proxy's
   precise state by looking to see which eip it is executing at
   exception time.

   Ranges:

   vga_sys_before ... vga_sys_restarted:
	Setting up register arguments and running state.  If
	interrupted, then the syscall should be considered to return
	ERESTARTSYS.

   vga_sys_restarted:
	If interrupted and eip==vga_sys_restarted, then either the syscall
	was about to start running, or it has run, was interrupted and
	the kernel wants to restart it.  eax still contains the
	syscall number.  If interrupted, then the syscall return value
	should be ERESTARTSYS.

   vga_sys_after:
	If interrupted and eip==vga_sys_after, the syscall either just
	finished, or it was interrupted and the kernel doesn't want to
	restart it.  Either way, eax equals the correct return value
	(either the actual return value, or EINTR).

   vga_sys_after ... vga_sys_done:
	System call is complete, but the state hasn't been updated,
	nor has the result been written back.  eax contains the return
	value.
*/
extern void do_thread_syscall(Int sys, 
			      Int arg1, Int arg2, Int arg3, Int arg4,
                              Int arg5, Int arg6,
			      Int *result, enum PXState *statep,
                              enum PXState poststate);

asm(
".text\n"
"	.type do_thread_syscall,@function\n"

"do_thread_syscall:\n"
"	push	%esi\n"
"	push	%edi\n"
"	push	%ebx\n"
"	push	%ebp\n"
".vga_sys_before:\n"
"	movl	16+ 4(%esp),%eax\n" /* syscall */
"	movl	16+ 8(%esp),%ebx\n" /* arg1 */
"	movl	16+12(%esp),%ecx\n" /* arg2 */
"	movl	16+16(%esp),%edx\n" /* arg3 */
"	movl	16+20(%esp),%esi\n" /* arg4 */
"	movl	16+24(%esp),%edi\n" /* arg5 */
"	movl	16+28(%esp),%ebp\n" /* arg6 */
".vga_sys_restarted:\n"
"	int	$0x80\n"
".vga_sys_after:\n"
"	movl	16+32(%esp),%ebx\n"	/* ebx = Int *res */
"	movl	%eax, (%ebx)\n"		/* write the syscall retval */

"	movl	16+36(%esp),%ebx\n"	/* ebx = enum PXState * */
"	testl	%ebx, %ebx\n"
"	jz	1f\n"

"	movl	16+40(%esp),%ecx\n"	/* write the post state (must be after retval write) */
"	movl	%ecx,(%ebx)\n"

".vga_sys_done:\n"				/* OK, all clear from here */
"1:	popl	%ebp\n"
"	popl	%ebx\n"
"	popl	%edi\n"
"	popl	%esi\n"
"	ret\n"
"	.size do_thread_syscall,.-do_thread_syscall\n"
".previous\n"

".section .rodata\n"
"       .globl  vga_sys_before\n"
"vga_sys_before:	.long	.vga_sys_before\n"
"       .globl  vga_sys_restarted\n"
"vga_sys_restarted:	.long	.vga_sys_restarted\n"
"       .globl  vga_sys_after\n"
"vga_sys_after:	.long	.vga_sys_after\n"
"       .globl  vga_sys_done\n"
"vga_sys_done:	.long	.vga_sys_done\n"
".previous\n"
);

/* Run a syscall for a particular thread, getting the arguments from
   the thread's registers, and returning the result in the thread's
   eax.

   Assumes that the only thread state which matters is the contents of
   %eax-%ebp and the return value in %eax.
 */
void VGA_(thread_syscall)(Int syscallno, arch_thread_t *arch, 
                          enum PXState *state , enum PXState poststate)
{
   do_thread_syscall(syscallno,    // syscall no.
		     arch->m_ebx,  // arg 1
		     arch->m_ecx,  // arg 2
		     arch->m_edx,  // arg 3
		     arch->m_esi,  // arg 4
		     arch->m_edi,  // arg 5
		     arch->m_ebp,  // arg 6
		     &arch->m_eax, // result
		     state,	   // state to update
		     poststate);   // state when syscall has finished
}



// Back up to restart a system call.
void VGA_(restart_syscall)(arch_thread_t *arch)
{
   arch->m_eip -= 2;             // sizeof(int $0x80)

   /* Make sure our caller is actually sane, and we're really backing
      back over a syscall.

      int $0x80 == CD 80 
   */
   {
      UChar *p = (UChar *)arch->m_eip;
      
      if (p[0] != 0xcd || p[1] != 0x80)
         VG_(message)(Vg_DebugMsg,
                      "?! restarting over syscall at %p %02x %02x\n",
                      arch->m_eip, p[0], p[1]); 

      vg_assert(p[0] == 0xcd && p[1] == 0x80);
   }
}

/* ---------------------------------------------------------------------
   PRE/POST wrappers for x86/Linux-specific syscalls
   ------------------------------------------------------------------ */

// Nb: See the comment above the generic PRE/POST wrappers in
// coregrind/vg_syscalls.c for notes about how they work.

#define PRE(x,f) \
   static UInt x86_linux_##x##_flags = f; \
   static void x86_linux_##x##_before(ThreadId tid, ThreadState *tst)
#define POST(x) \
   static void x86_linux_##x##_after (ThreadId tid, ThreadState *tst)

#define SYSNO	PLATFORM_SYSCALL_NUM(tst->arch)    // in PRE(x)
#define res	PLATFORM_SYSCALL_RET(tst->arch)	   // in POST(x)
#define arg1	PLATFORM_SYSCALL_ARG1(tst->arch)
#define arg2	PLATFORM_SYSCALL_ARG2(tst->arch)
#define arg3	PLATFORM_SYSCALL_ARG3(tst->arch)
#define arg4	PLATFORM_SYSCALL_ARG4(tst->arch)
#define arg5	PLATFORM_SYSCALL_ARG5(tst->arch)
#define arg6	PLATFORM_SYSCALL_ARG6(tst->arch)

#define set_result(val) PLATFORM_SET_SYSCALL_RESULT(tst->arch, (val))

#define PRINT(format, args...)  \
   if (VG_(clo_trace_syscalls))        \
      VG_(printf)(format, ## args)

PRE(sys_modify_ldt, Special)
{
   PRINT("sys_modify_ldt ( %d, %p, %d )", arg1,arg2,arg3);
   PRE_REG_READ3(int, "modify_ldt", int, func, void *, ptr,
                 unsigned long, bytecount);
   
   if (arg1 == 0) {
      /* read the LDT into ptr */
      PRE_MEM_WRITE( "modify_ldt(ptr)", arg2, arg3 );
   }
   if (arg1 == 1 || arg1 == 0x11) {
      /* write the LDT with the entry pointed at by ptr */
      PRE_MEM_READ( "modify_ldt(ptr)", arg2, sizeof(vki_modify_ldt_t) );
   }
   /* "do" the syscall ourselves; the kernel never sees it */
   res = VG_(sys_modify_ldt)( tid, arg1, (void*)arg2, arg3 );

   if (arg1 == 0 && !VG_(is_kerror)(res) && res > 0) {
      POST_MEM_WRITE( arg2, res );
   }
}

PRE(sys_set_thread_area, Special)
{
   PRINT("sys_set_thread_area ( %p )", arg1);
   PRE_REG_READ1(int, "set_thread_area", struct user_desc *, u_info)
   PRE_MEM_READ( "set_thread_area(u_info)", arg1, sizeof(vki_modify_ldt_t) );

   /* "do" the syscall ourselves; the kernel never sees it */
   set_result( VG_(sys_set_thread_area)( tid, (void *)arg1 ) );
}

PRE(sys_get_thread_area, Special)
{
   PRINT("sys_get_thread_area ( %p )", arg1);
   PRE_REG_READ1(int, "get_thread_area", struct user_desc *, u_info)
   PRE_MEM_WRITE( "get_thread_area(u_info)", arg1, sizeof(vki_modify_ldt_t) );

   /* "do" the syscall ourselves; the kernel never sees it */
   set_result( VG_(sys_get_thread_area)( tid, (void *)arg1 ) );

   if (!VG_(is_kerror)(res)) {
      POST_MEM_WRITE( arg1, sizeof(vki_modify_ldt_t) );
   }
}

#undef PRE
#undef POST

/* ---------------------------------------------------------------------
   The x86/Linux syscall table
   ------------------------------------------------------------------ */

// XXX: temporary: I've started labelled each entry with the target of its
// __NR_foo-->sys_foo() mapping, and the __NR_foo number (for x86), and
// properties of the sys_foo() function:
//   '*' ones are Linux-generic (ie. present in
//       linux-2.6.8.1/include/linux/syscalls.h);  non-generic ones have an
//       indication of which architecture they are specific to.
//   "##" ones are Linux-generic, but within a "#ifdef CONFIG_UID16".
//   "%%" ones are Linux-generic, but within a "#if BITS_PER_LONG == 32"
//   'P' ones are POSIX-generic, according to man pages.  
//   'L' ones are Linux-specific, according to man pages.

// XXX: this does duplicate the vki_unistd.h file somewhat -- could make
// this table replace that somehow...

#define GENX_(const, name) \
   [const] = { &VGA_(gen_##name##_flags), VGA_(gen_##name##_before), NULL }
#define GENXY(const, name) \
   [const] = { &VGA_(gen_##name##_flags), VGA_(gen_##name##_before), \
                                          VGA_(gen_##name##_after) }

#define LINX_(const, name) \
   [const] = { &VGA_(linux_##name##_flags), VGA_(linux_##name##_before), NULL }
#define LINXY(const, name) \
   [const] = { &VGA_(linux_##name##_flags), VGA_(linux_##name##_before), \
                                            VGA_(linux_##name##_after) }
#define PLAX_(const, name) \
   [const] = { &x86_linux_##name##_flags, x86_linux_##name##_before, NULL }
#define PLAXY(const, name) \
   [const] = { &x86_linux_##name##_flags, x86_linux_##name##_before, \
                                          x86_linux_##name##_after }

// This table maps from __NR_xxx syscall numbers to the appropriate
// PRE/POST sys_foo() wrappers on x86.
const struct SyscallTableEntry VGA_(syscall_table)[] = {
   //   (restart_syscall)                             // 0
   GENX_(__NR_exit,              sys_exit),           // 1
   GENXY(__NR_fork,              sys_fork),           // 2
   GENXY(__NR_read,              sys_read),           // 3
   GENX_(__NR_write,             sys_write),          // 4

   GENXY(__NR_open,              sys_open),           // 5
   GENXY(__NR_close,             sys_close),          // 6
   GENXY(__NR_waitpid,           sys_waitpid),        // 7
   GENXY(__NR_creat,             sys_creat),          // 8
   GENX_(__NR_link,              sys_link),           // 9

   GENX_(__NR_unlink,            sys_unlink),         // 10
   GENX_(__NR_execve,            sys_execve),         // 11 (*??) P
   GENX_(__NR_chdir,             sys_chdir),          // 12
   GENXY(__NR_time,              sys_time),           // 13
   GENX_(__NR_mknod,             sys_mknod),          // 14

   GENX_(__NR_chmod,             sys_chmod),          // 15
   //   (__NR_lchown,            sys_lchown16),       // 16 ## P
   GENX_(__NR_break,             sys_ni_syscall),     // 17 -- unimplemented
   //   (__NR_oldstat,           sys_stat),           // 18 * L -- obsolete
   GENX_(__NR_lseek,             sys_lseek),          // 19

   GENX_(__NR_getpid,            sys_getpid),         // 20
   LINX_(__NR_mount,             sys_mount),          // 21
   LINX_(__NR_umount,            sys_oldumount),      // 22
   GENX_(__NR_setuid,            sys_setuid16),       // 23 ## P
   GENX_(__NR_getuid,            sys_getuid16),       // 24 ## P

   //   (__NR_stime,             sys_stime),        // 25 * (SVr4,SVID,X/OPEN)
   GENXY(__NR_ptrace,            sys_ptrace),         // 26 (x86?) (L?)
   GENX_(__NR_alarm,             sys_alarm),          // 27 * P
   //   (__NR_oldfstat,          sys_fstat),        // 28 * L -- obsolete
   GENX_(__NR_pause,             sys_pause),          // 29 * P

   GENX_(__NR_utime,             sys_utime),          // 30 * P
   GENX_(__NR_stty,              sys_ni_syscall),     // 31 -- unimplemented
   GENX_(__NR_gtty,              sys_ni_syscall),     // 32 -- unimplemented
   GENX_(__NR_access,            sys_access),         // 33 * P
   GENX_(__NR_nice,              sys_nice),           // 34 * (almost P)

   GENX_(__NR_ftime,             sys_ni_syscall),     // 35 -- unimplemented
   GENX_(__NR_sync,              sys_sync),           // 36 * (almost P)
   GENXY(__NR_kill,              sys_kill),           // 37
   GENX_(__NR_rename,            sys_rename),         // 38
   GENX_(__NR_mkdir,             sys_mkdir),          // 39

   GENX_(__NR_rmdir,             sys_rmdir),          // 40
   GENXY(__NR_dup,               sys_dup),            // 41
   GENXY(__NR_pipe,              sys_pipe),           // 42 (x86) P
   GENXY(__NR_times,             sys_times),          // 43
   GENX_(__NR_prof,              sys_ni_syscall),     // 44 -- unimplemented

   GENX_(__NR_brk,               sys_brk),            // 45 * non-P
   GENX_(__NR_setgid,            sys_setgid16),       // 46 ## (SVr4,SVID)
   GENX_(__NR_getgid,            sys_getgid16),       // 47 ## P
   //   (__NR_signal,            sys_signal),         // 48 * (ANSI C?)
   GENX_(__NR_geteuid,           sys_geteuid16),      // 49 ## P

   GENX_(__NR_getegid,           sys_getegid16),      // 50 ## (P16)
   GENX_(__NR_acct,              sys_acct),           // 51 * (SVR4, non-POSIX)
   LINX_(__NR_umount2,           sys_umount),         // 52
   GENX_(__NR_lock,              sys_ni_syscall),     // 53 -- unimplemented
   GENXY(__NR_ioctl,             sys_ioctl),          // 54 */x86 (varying)

   GENXY(__NR_fcntl,             sys_fcntl),          // 55
   GENX_(__NR_mpx,               sys_ni_syscall),     // 56 -- unimplemented
   GENXY(__NR_setpgid,           sys_setpgid),        // 57 * P
   GENX_(__NR_ulimit,            sys_ni_syscall),     // 58 -- unimplemented
   //   (__NR_oldolduname,       sys_olduname),       // 59 (?) L -- obsolete

   GENX_(__NR_umask,             sys_umask),          // 60
   GENX_(__NR_chroot,            sys_chroot),         // 61 * (almost P)
   //   (__NR_ustat,             sys_ustat)           // 62 * (SVr4) -- deprecated
   GENXY(__NR_dup2,              sys_dup2),           // 63
   GENX_(__NR_getppid,           sys_getppid),        // 64

   GENX_(__NR_getpgrp,           sys_getpgrp),        // 65
   GENX_(__NR_setsid,            sys_setsid),         // 66
   GENXY(__NR_sigaction,         sys_sigaction),      // 67 (x86) P
   //   (__NR_sgetmask,          sys_sgetmask),       // 68 * (ANSI C)
   //   (__NR_ssetmask,          sys_ssetmask),       // 69 * (ANSI C)

   GENX_(__NR_setreuid,          sys_setreuid16),     // 70 ## (BSD4.3)
   GENX_(__NR_setregid,          sys_setregid16),     // 71 ## (BSD4.3)
   GENX_(__NR_sigsuspend,        sys_sigsuspend),     // 72 () P
   GENXY(__NR_sigpending,        sys_sigpending),     // 73 * P
   //   (__NR_sethostname,       sys_sethostname),    // 74 * (almost P)

   GENX_(__NR_setrlimit,         sys_setrlimit),      // 75 * (SVr4,BSD4.3)
   GENXY(__NR_getrlimit,         sys_old_getrlimit),  // 76 * (SVr4,BSD4.3)
   GENXY(__NR_getrusage,         sys_getrusage),      // 77 * (SVr4,BSD4.3)
   GENXY(__NR_gettimeofday,      sys_gettimeofday),   // 78 * P
   GENX_(__NR_settimeofday,      sys_settimeofday),   // 79 * almost-P

   GENXY(__NR_getgroups,         sys_getgroups16),    // 80 ## P
   GENX_(__NR_setgroups,         sys_setgroups16),    // 81 ## almost-P
   GENX_(__NR_select,           old_select),          // 82 (x86) (4.4BSD)
   GENX_(__NR_symlink,           sys_symlink),        // 83 * P
   //   (__NR_oldlstat,          sys_lstat),          // 84 * L -- obsolete

   GENXY(__NR_readlink,          sys_readlink),       // 85 * (X/OPEN,4.4BSD)
   //   (__NR_uselib,            sys_uselib),         // 86 * L
   //   (__NR_swapon,            sys_swapon),         // 87 * L
   //   (__NR_reboot,            sys_reboot),         // 88 * L
   //   (__NR_readdir,           old_readdir),        // 89 () L -- superseded

   GENX_(__NR_mmap,             old_mmap),            // 90 (x86) (P but not...)
   GENXY(__NR_munmap,            sys_munmap),         // 91
   GENX_(__NR_truncate,          sys_truncate),       // 92
   GENX_(__NR_ftruncate,         sys_ftruncate),      // 93
   GENX_(__NR_fchmod,            sys_fchmod),         // 94

   GENX_(__NR_fchown,            sys_fchown16),       // 95 ## (SVr4,BSD4.3)
   GENX_(__NR_getpriority,       sys_getpriority),    // 96 * (SVr4,4.4BSD)
   GENX_(__NR_setpriority,       sys_setpriority),    // 97 * (SVr4,4.4BSD)
   GENX_(__NR_profil,            sys_ni_syscall),     // 98 -- unimplemented
   GENXY(__NR_statfs,            sys_statfs),         // 99 * (P-ish)

   GENXY(__NR_fstatfs,           sys_fstatfs),        // 100 * (P-ish)
   LINX_(__NR_ioperm,            sys_ioperm),         // 101 * L
   GENXY(__NR_socketcall,        sys_socketcall),     // 102 */Linux (?)
   LINXY(__NR_syslog,            sys_syslog),         // 103
   GENXY(__NR_setitimer,         sys_setitimer),      // 104 * (SVr4,4.4BSD)

   GENXY(__NR_getitimer,         sys_getitimer),      // 105 * (SVr4,4.4BSD)
   GENXY(__NR_stat,              sys_newstat),        // 106 * P
   GENXY(__NR_lstat,             sys_newlstat),       // 107 *
   GENXY(__NR_fstat,             sys_newfstat),       // 108 * P (SVr4,BSD4.3)
   //   (__NR_olduname,          sys_uname),          // 109 (?) L -- obsolete

   GENX_(__NR_iopl,              sys_iopl),           // 110 (x86/amd64) L
   LINX_(__NR_vhangup,           sys_vhangup),        // 111
   GENX_(__NR_idle,              sys_ni_syscall),     // 112 -- unimplemented
   //   (__NR_vm86old,           sys_vm86old),        // 113 x86/Linux-only
   GENXY(__NR_wait4,             sys_wait4),          // 114 * P

   //   (__NR_swapoff,           sys_swapoff),        // 115 */Linux 
   LINXY(__NR_sysinfo,           sys_sysinfo),        // 116
   GENXY(__NR_ipc,               sys_ipc),            // 117 (x86) L
   GENX_(__NR_fsync,             sys_fsync),          // 118
   //   (__NR_sigreturn,         sys_sigreturn),      // 119 () L

   GENX_(__NR_clone,             sys_clone),          // 120 (x86) L
   //   (__NR_setdomainname,     sys_setdomainname),  // 121 * (non-P?)
   GENXY(__NR_uname,             sys_newuname),       // 122 * P
   PLAX_(__NR_modify_ldt,        sys_modify_ldt),     // 123 (x86,amd64) L
   LINXY(__NR_adjtimex,          sys_adjtimex),       // 124 * L

   GENXY(__NR_mprotect,          sys_mprotect),       // 125 * P
   GENXY(__NR_sigprocmask,       sys_sigprocmask),    // 126 * P
   // Nb: create_module() was removed 2.4-->2.6
   GENX_(__NR_create_module,     sys_ni_syscall),     // 127 -- unimplemented
   GENX_(__NR_init_module,       sys_init_module),    // 128 * L?
   //   (__NR_delete_module,     sys_delete_module),  // 129 () (L?)

   // Nb: get_kernel_syms() was removed 2.4-->2.6
   GENX_(__NR_get_kernel_syms,   sys_ni_syscall),     // 130 -- unimplemented
   GENX_(__NR_quotactl,          sys_quotactl),       // 131 * (?)
   GENX_(__NR_getpgid,           sys_getpgid),        // 132
   GENX_(__NR_fchdir,            sys_fchdir),         // 133 * (almost-P)
   //   (__NR_bdflush,           sys_bdflush),        // 134 * L

   //   (__NR_sysfs,             sys_sysfs),          // 135 * (SVr4)
   LINX_(__NR_personality,       sys_personality),    // 136
   GENX_(__NR_afs_syscall,       sys_ni_syscall),     // 137 -- unimplemented
   LINX_(__NR_setfsuid,          sys_setfsuid16),     // 138 ## L
   LINX_(__NR_setfsgid,          sys_setfsgid16),     // 139 ## L

   LINXY(__NR__llseek,           sys_llseek),         // 140
   GENXY(__NR_getdents,          sys_getdents),       // 141 * (SVr4,SVID)
   GENX_(__NR__newselect,        sys_select),         // 142 * (4.4BSD...)
   GENX_(__NR_flock,             sys_flock),          // 143 * (4.4BSD...)
   GENX_(__NR_msync,             sys_msync),          // 144 * P

   GENXY(__NR_readv,             sys_readv),          // 145 * P
   GENX_(__NR_writev,            sys_writev),         // 146 * P
   GENX_(__NR_getsid,            sys_getsid),         // 147 * P
   GENX_(__NR_fdatasync,         sys_fdatasync),      // 148 * P
   LINXY(__NR__sysctl,           sys_sysctl),         // 149

   GENX_(__NR_mlock,             sys_mlock),          // 150
   GENX_(__NR_munlock,           sys_munlock),        // 151
   GENX_(__NR_mlockall,          sys_mlockall),       // 152
   GENX_(__NR_munlockall,        sys_munlockall),     // 153
   GENXY(__NR_sched_setparam,    sys_sched_setparam), // 154

   GENXY(__NR_sched_getparam,         sys_sched_getparam),        // 155
   GENX_(__NR_sched_setscheduler,     sys_sched_setscheduler),    // 156
   GENX_(__NR_sched_getscheduler,     sys_sched_getscheduler),    // 157
   GENX_(__NR_sched_yield,            sys_sched_yield),           // 158
   GENX_(__NR_sched_get_priority_max, sys_sched_get_priority_max),// 159

   GENX_(__NR_sched_get_priority_min, sys_sched_get_priority_min),// 160
   //   (__NR_sched_rr_get_interval,  sys_sched_rr_get_interval), // 161 (gen)
   GENXY(__NR_nanosleep,         sys_nanosleep),      // 162
   GENX_(__NR_mremap,            sys_mremap),         // 163
   LINX_(__NR_setresuid,         sys_setresuid16),    // 164

   LINXY(__NR_getresuid,         sys_getresuid16),    // 165
   //   (__NR_vm86,              sys_vm86),           // 166 x86/Linux-only
   GENX_(__NR_query_module,      sys_ni_syscall),     // 167 -- unimplemented
   GENXY(__NR_poll,              sys_poll),           // 168 * (XPG4-UNIX)
   //   (__NR_nfsservctl,        sys_nfsservctl),     // 169 * L

   LINX_(__NR_setresgid,         sys_setresgid16),    // 170
   LINXY(__NR_getresgid,         sys_getresgid16),    // 171
   LINX_(__NR_prctl,             sys_prctl),          // 172
   //   (__NR_rt_sigreturn,      sys_rt_sigreturn),   // 173 (x86) ()
   GENXY(__NR_rt_sigaction,      sys_rt_sigaction),   // 174 (x86) ()

   GENXY(__NR_rt_sigprocmask,    sys_rt_sigprocmask), // 175 * ?
   GENXY(__NR_rt_sigpending,     sys_rt_sigpending),  // 176 * ?
   GENXY(__NR_rt_sigtimedwait,   sys_rt_sigtimedwait),// 177 * ?
   GENXY(__NR_rt_sigqueueinfo,   sys_rt_sigqueueinfo),// 178 * ?
   GENX_(__NR_rt_sigsuspend,     sys_rt_sigsuspend),  // 179 () ()
   GENXY(__NR_pread64,           sys_pread64),        // 180 * (Unix98?)

   GENX_(__NR_pwrite64,          sys_pwrite64),       // 181 * (Unix98?)
   GENX_(__NR_chown,             sys_chown16),        // 182 * P
   GENXY(__NR_getcwd,            sys_getcwd),         // 183 * P
   GENXY(__NR_capget,            sys_capget),         // 184 * L?

   GENX_(__NR_capset,            sys_capset),         // 185 * L?
   GENXY(__NR_sigaltstack,       sys_sigaltstack),    // 186 (x86) (XPG4-UNIX)
   GENXY(__NR_sendfile,          sys_sendfile),       // 187 * L
   GENXY(__NR_getpmsg,           sys_getpmsg),        // 188 (?) (?)
   GENX_(__NR_putpmsg,           sys_putpmsg),        // 189 (?) (?)

   // Nb: we convert vfork() to fork() in VG_(pre_syscall)().
   //   (__NR_vfork,             sys_vfork),          // 190 -- Valgrind avoids
   GENXY(__NR_ugetrlimit,        sys_getrlimit),      // 191 * (?)
   GENXY(__NR_mmap2,             sys_mmap2),          // 192 (x86?) P?
   GENX_(__NR_truncate64,        sys_truncate64),     // 193 %% (P?)
   GENX_(__NR_ftruncate64,       sys_ftruncate64),    // 194 %% (P?)
   
   GENXY(__NR_stat64,            sys_stat64),         // 195 %% (?)
   GENXY(__NR_lstat64,           sys_lstat64),        // 196 %% (?)
   GENXY(__NR_fstat64,           sys_fstat64),        // 197 %% (?)
   GENX_(__NR_lchown32,          sys_lchown),         // 198 * (L?)
   GENX_(__NR_getuid32,          sys_getuid),         // 199

   GENX_(__NR_getgid32,          sys_getgid),         // 200
   GENX_(__NR_geteuid32,         sys_geteuid),        // 201
   GENX_(__NR_getegid32,         sys_getegid),        // 202
   GENX_(__NR_setreuid32,        sys_setreuid),       // 203 * (BSD4.3)
   GENX_(__NR_setregid32,        sys_setregid),       // 204 * (BSD4.3)

   GENXY(__NR_getgroups32,       sys_getgroups),      // 205 * P
   GENX_(__NR_setgroups32,       sys_setgroups),      // 206 * almost-P
   GENX_(__NR_fchown32,          sys_fchown),         // 207 * (SVr4,BSD4.3)
   LINX_(__NR_setresuid32,       sys_setresuid),      // 208
   LINXY(__NR_getresuid32,       sys_getresuid),      // 209

   LINX_(__NR_setresgid32,       sys_setresgid),      // 210
   LINXY(__NR_getresgid32,       sys_getresgid),      // 211
   GENX_(__NR_chown32,           sys_chown),          // 212 * P
   GENX_(__NR_setuid32,          sys_setuid),         // 213 *
   GENX_(__NR_setgid32,          sys_setgid),         // 214 * (SVr4,SVID)

   LINX_(__NR_setfsuid32,        sys_setfsuid),       // 215
   LINX_(__NR_setfsgid32,        sys_setfsgid),       // 216
   //   (__NR_pivot_root,        sys_pivot_root),     // 217 */Linux
   GENXY(__NR_mincore,           sys_mincore),        // 218 * non-P?
   GENX_(__NR_madvise,           sys_madvise),        // 219 * P

   GENXY(__NR_getdents64,        sys_getdents64),     // 220 * (SVr4,SVID?)
   GENXY(__NR_fcntl64,           sys_fcntl64),        // 221 * P?
   GENX_(222,                    sys_ni_syscall),     // 222 -- reserved for TUX
   GENX_(223,                    sys_ni_syscall),     // 223 -- unused
   //   (__NR_gettid,            sys_gettid),         // 224 */Linux

   //   (__NR_readahead,         sys_readahead),      // 225 */(Linux?)
   GENX_(__NR_setxattr,          sys_setxattr),       // 226 * L?
   GENX_(__NR_lsetxattr,         sys_lsetxattr),      // 227 * L?
   GENX_(__NR_fsetxattr,         sys_fsetxattr),      // 228 * L?
   GENXY(__NR_getxattr,          sys_getxattr),       // 229 * L?

   GENXY(__NR_lgetxattr,         sys_lgetxattr),      // 230 * L?
   GENXY(__NR_fgetxattr,         sys_fgetxattr),      // 231 * L?
   GENXY(__NR_listxattr,         sys_listxattr),      // 232 * L?
   GENXY(__NR_llistxattr,        sys_llistxattr),     // 233 * L?
   GENXY(__NR_flistxattr,        sys_flistxattr),     // 234 * L?

   GENX_(__NR_removexattr,       sys_removexattr),    // 235 * L?
   GENX_(__NR_lremovexattr,      sys_lremovexattr),   // 236 * L?
   GENX_(__NR_fremovexattr,      sys_fremovexattr),   // 237 * L?
   //   (__NR_tkill,             sys_tkill),          // 238 */Linux
   GENXY(__NR_sendfile64,        sys_sendfile64),     // 239 * L

   GENXY(__NR_futex,             sys_futex),             // 240 * L
   GENX_(__NR_sched_setaffinity, sys_sched_setaffinity), // 241 * L?
   GENXY(__NR_sched_getaffinity, sys_sched_getaffinity), // 242 * L?
   PLAX_(__NR_set_thread_area,   sys_set_thread_area),   // 243
   PLAX_(__NR_get_thread_area,   sys_get_thread_area),   // 244

   LINX_(__NR_io_setup,          sys_io_setup),       // 245
   LINX_(__NR_io_destroy,        sys_io_destroy),     // 246
   LINXY(__NR_io_getevents,      sys_io_getevents),   // 247
   LINX_(__NR_io_submit,         sys_io_submit),      // 248
   LINXY(__NR_io_cancel,         sys_io_cancel),      // 249

   //   (__NR_fadvise64,         sys_fadvise64),      // 250 * ()
   GENX_(251,                    sys_ni_syscall),     // 251 -- unused
   GENX_(__NR_exit_group,        sys_exit_group),     // 252 *
   GENXY(__NR_lookup_dcookie,    sys_lookup_dcookie), // 253 (*/32/64) L
   GENXY(__NR_epoll_create,      sys_epoll_create),   // 254 * L

   GENX_(__NR_epoll_ctl,         sys_epoll_ctl),         // 255 * L
   GENXY(__NR_epoll_wait,        sys_epoll_wait),        // 256 * L
   //   (__NR_remap_file_pages,  sys_remap_file_pages),  // 257 * L
   GENX_(__NR_set_tid_address,   sys_set_tid_address),   // 258 * ?
   GENXY(__NR_timer_create,      sys_timer_create),      // 259 (?) P

   GENXY(__NR_timer_settime,     sys_timer_settime),  // (timer_create+1) * P
   GENXY(__NR_timer_gettime,     sys_timer_gettime),  // (timer_create+2) * P
   GENX_(__NR_timer_getoverrun,  sys_timer_getoverrun),//(timer_create+3) * P
   GENX_(__NR_timer_delete,      sys_timer_delete),   // (timer_create+4) * P
   GENX_(__NR_clock_settime,     sys_clock_settime),  // (timer_create+5) * P

   GENXY(__NR_clock_gettime,     sys_clock_gettime),  // (timer_create+6) * P
   GENXY(__NR_clock_getres,      sys_clock_getres),   // (timer_create+7) * P
   //   (__NR_clock_nanosleep,   sys_clock_nanosleep),// (timer_create+8) * P
   GENXY(__NR_statfs64,          sys_statfs64),       // 268 * (?)
   GENXY(__NR_fstatfs64,         sys_fstatfs64),      // 269 * (?)

   //   (__NR_tgkill,            sys_tgkill),         // 270 */Linux
   GENX_(__NR_utimes,            sys_utimes),         // 271 * (4.3BSD)
   //   (__NR_fadvise64_64,      sys_fadvise64_64),   // 272 */(Linux?)
   GENX_(__NR_vserver,           sys_ni_syscall),     // 273 -- unimplemented
   //   (__NR_mbind,             sys_mbind),          // 274 ?/?

   //   (__NR_get_mempolicy,     sys_get_mempolicy),  // 275 ?/?
   //   (__NR_set_mempolicy,     sys_set_mempolicy),  // 276 ?/?
   GENXY(__NR_mq_open,           sys_mq_open),        // 277 * P?
   GENX_(__NR_mq_unlink,         sys_mq_unlink),      // (mq_open+1) * P?
   GENX_(__NR_mq_timedsend,      sys_mq_timedsend),   // (mq_open+2) * P?

   GENXY(__NR_mq_timedreceive,   sys_mq_timedreceive),// (mq_open+3) * P?
   GENX_(__NR_mq_notify,         sys_mq_notify),      // (mq_open+4) * P?
   GENXY(__NR_mq_getsetattr,     sys_mq_getsetattr),  // (mq_open+5) * P?
   GENX_(__NR_sys_kexec_load,    sys_ni_syscall),     // 283
};

const UInt VGA_(syscall_table_size) = 
            sizeof(VGA_(syscall_table)) / sizeof(VGA_(syscall_table)[0]);

#undef GENX_
#undef GENXY

/*--------------------------------------------------------------------*/
/*--- end                                                          ---*/
/*--------------------------------------------------------------------*/
