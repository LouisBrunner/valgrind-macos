
/*--------------------------------------------------------------------*/
/*--- Linux-specific syscalls stuff.          priv_syswrap-linux.h ---*/
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

#ifndef __PRIV_SYSWRAP_LINUX_H
#define __PRIV_SYSWRAP_LINUX_H

/* requires #include "priv_types_n_macros.h" */

// Clone-related functions
extern Word ML_(start_thread_NORETURN) ( void* arg );
extern Addr ML_(allocstack)            ( ThreadId tid );
extern void ML_(call_on_new_stack_0_1) ( Addr stack, Addr retaddr,
			                 void (*f)(Word), Word arg1 );
extern SysRes ML_(do_fork_clone) ( ThreadId tid, UInt flags,
                                   Int* parent_tidptr, Int* child_tidptr );


// Linux-specific (but non-arch-specific) syscalls

DECL_TEMPLATE(linux, sys_mount);
DECL_TEMPLATE(linux, sys_oldumount);
DECL_TEMPLATE(linux, sys_umount);
DECL_TEMPLATE(linux, sys_perf_event_open);
DECL_TEMPLATE(linux, sys_preadv);
DECL_TEMPLATE(linux, sys_pwritev);
DECL_TEMPLATE(linux, sys_sendmmsg);
DECL_TEMPLATE(linux, sys_recvmmsg);
DECL_TEMPLATE(linux, sys_dup3);
DECL_TEMPLATE(linux, sys_getcpu);
DECL_TEMPLATE(linux, sys_splice);
DECL_TEMPLATE(linux, sys_tee);
DECL_TEMPLATE(linux, sys_vmsplice);
DECL_TEMPLATE(linux, sys_readahead);
DECL_TEMPLATE(linux, sys_move_pages);

// POSIX, but various sub-cases differ between Linux and Darwin.
DECL_TEMPLATE(linux, sys_fcntl);
DECL_TEMPLATE(linux, sys_fcntl64);
DECL_TEMPLATE(linux, sys_ioctl);

DECL_TEMPLATE(linux, sys_setfsuid16);
DECL_TEMPLATE(linux, sys_setfsuid);
DECL_TEMPLATE(linux, sys_setfsgid16);
DECL_TEMPLATE(linux, sys_setfsgid);
DECL_TEMPLATE(linux, sys_setresuid16);  // man page says "non-standard";
DECL_TEMPLATE(linux, sys_setresuid);    // man page says "non-standard"
DECL_TEMPLATE(linux, sys_getresuid16);
DECL_TEMPLATE(linux, sys_getresuid);
DECL_TEMPLATE(linux, sys_setresgid16);  // man page says "non-standard"
DECL_TEMPLATE(linux, sys_setresgid);    // man page says "non-standard"
DECL_TEMPLATE(linux, sys_getresgid16);
DECL_TEMPLATE(linux, sys_getresgid);

DECL_TEMPLATE(linux, sys_exit_group);
DECL_TEMPLATE(linux, sys_llseek);
DECL_TEMPLATE(linux, sys_adjtimex);
DECL_TEMPLATE(linux, sys_ioperm);
DECL_TEMPLATE(linux, sys_syslog);
DECL_TEMPLATE(linux, sys_vhangup);
DECL_TEMPLATE(linux, sys_sysinfo);
DECL_TEMPLATE(linux, sys_personality);
DECL_TEMPLATE(linux, sys_sysctl);
DECL_TEMPLATE(linux, sys_prctl);
DECL_TEMPLATE(linux, sys_sendfile);
DECL_TEMPLATE(linux, sys_sendfile64);
DECL_TEMPLATE(linux, sys_futex);
DECL_TEMPLATE(linux, sys_set_robust_list);
DECL_TEMPLATE(linux, sys_get_robust_list);
DECL_TEMPLATE(linux, sys_pselect6);
DECL_TEMPLATE(linux, sys_ppoll);

DECL_TEMPLATE(linux, sys_epoll_create);
DECL_TEMPLATE(linux, sys_epoll_create1);
DECL_TEMPLATE(linux, sys_epoll_ctl);
DECL_TEMPLATE(linux, sys_epoll_wait);
DECL_TEMPLATE(linux, sys_epoll_pwait);
DECL_TEMPLATE(linux, sys_eventfd);
DECL_TEMPLATE(linux, sys_eventfd2);

DECL_TEMPLATE(linux, sys_fallocate);

DECL_TEMPLATE(linux, sys_prlimit64);

DECL_TEMPLATE(linux, sys_gettid);
DECL_TEMPLATE(linux, sys_set_tid_address);
DECL_TEMPLATE(linux, sys_tkill);
DECL_TEMPLATE(linux, sys_tgkill);

DECL_TEMPLATE(linux, sys_fadvise64);
DECL_TEMPLATE(linux, sys_fadvise64_64);

DECL_TEMPLATE(linux, sys_io_setup);
DECL_TEMPLATE(linux, sys_io_destroy);
DECL_TEMPLATE(linux, sys_io_getevents);
DECL_TEMPLATE(linux, sys_io_submit);
DECL_TEMPLATE(linux, sys_io_cancel);

DECL_TEMPLATE(linux, sys_ioprio_set);
DECL_TEMPLATE(linux, sys_ioprio_get);

DECL_TEMPLATE(linux, sys_mbind);
DECL_TEMPLATE(linux, sys_set_mempolicy);
DECL_TEMPLATE(linux, sys_get_mempolicy);

DECL_TEMPLATE(linux, sys_inotify_init);
DECL_TEMPLATE(linux, sys_inotify_init1);
DECL_TEMPLATE(linux, sys_inotify_add_watch);
DECL_TEMPLATE(linux, sys_inotify_rm_watch);

DECL_TEMPLATE(linux, sys_mq_open);
DECL_TEMPLATE(linux, sys_mq_unlink);
DECL_TEMPLATE(linux, sys_mq_timedsend);
DECL_TEMPLATE(linux, sys_mq_timedreceive);
DECL_TEMPLATE(linux, sys_mq_notify);
DECL_TEMPLATE(linux, sys_mq_getsetattr);

DECL_TEMPLATE(linux, sys_clock_settime);
DECL_TEMPLATE(linux, sys_clock_gettime);
DECL_TEMPLATE(linux, sys_clock_getres);
DECL_TEMPLATE(linux, sys_clock_nanosleep);

DECL_TEMPLATE(linux, sys_timer_create);      // Linux: varies across archs?
DECL_TEMPLATE(linux, sys_timer_settime);
DECL_TEMPLATE(linux, sys_timer_gettime);
DECL_TEMPLATE(linux, sys_timer_getoverrun);
DECL_TEMPLATE(linux, sys_timer_delete);
DECL_TEMPLATE(linux, sys_timerfd_create);
DECL_TEMPLATE(linux, sys_timerfd_gettime);
DECL_TEMPLATE(linux, sys_timerfd_settime);

DECL_TEMPLATE(linux, sys_signalfd);
DECL_TEMPLATE(linux, sys_signalfd4);

DECL_TEMPLATE(linux, sys_capget);
DECL_TEMPLATE(linux, sys_capset);

DECL_TEMPLATE(linux, sys_openat);
DECL_TEMPLATE(linux, sys_mkdirat);
DECL_TEMPLATE(linux, sys_mknodat);
DECL_TEMPLATE(linux, sys_fchownat);
DECL_TEMPLATE(linux, sys_futimesat);
DECL_TEMPLATE(linux, sys_newfstatat);
DECL_TEMPLATE(linux, sys_unlinkat);
DECL_TEMPLATE(linux, sys_renameat);
DECL_TEMPLATE(linux, sys_linkat);
DECL_TEMPLATE(linux, sys_symlinkat);
DECL_TEMPLATE(linux, sys_readlinkat);
DECL_TEMPLATE(linux, sys_fchmodat);
DECL_TEMPLATE(linux, sys_faccessat);
DECL_TEMPLATE(linux, sys_utimensat);

DECL_TEMPLATE(linux, sys_add_key);
DECL_TEMPLATE(linux, sys_request_key);
DECL_TEMPLATE(linux, sys_keyctl);

// These ones have 32-bit generic equivalents, but the 16-bit versions (they
// use 16-bit gid_t and uid_t types) seem to be Linux-specific.
DECL_TEMPLATE(linux, sys_getuid16);
DECL_TEMPLATE(linux, sys_setuid16);
DECL_TEMPLATE(linux, sys_getgid16);
DECL_TEMPLATE(linux, sys_setgid16);
DECL_TEMPLATE(linux, sys_geteuid16);
DECL_TEMPLATE(linux, sys_getegid16);
DECL_TEMPLATE(linux, sys_setreuid16);
DECL_TEMPLATE(linux, sys_setregid16);
DECL_TEMPLATE(linux, sys_getgroups16);
DECL_TEMPLATE(linux, sys_setgroups16);

// Again, these 16-bit versions are Linux-specific, the 32-bit versions are
// generic.
DECL_TEMPLATE(linux, sys_chown16);
DECL_TEMPLATE(linux, sys_fchown16);
//DECL_TEMPLATE(linux, sys_lchown16);      // not yet encountered

// Are these POSIX?  In Darwin they have an extra parameter 'position',
// so put them here.
DECL_TEMPLATE(linux, sys_setxattr);
DECL_TEMPLATE(linux, sys_lsetxattr);
DECL_TEMPLATE(linux, sys_fsetxattr);
DECL_TEMPLATE(linux, sys_getxattr);
DECL_TEMPLATE(linux, sys_lgetxattr);
DECL_TEMPLATE(linux, sys_fgetxattr);
DECL_TEMPLATE(linux, sys_listxattr);
DECL_TEMPLATE(linux, sys_llistxattr);
DECL_TEMPLATE(linux, sys_flistxattr);
DECL_TEMPLATE(linux, sys_removexattr);
DECL_TEMPLATE(linux, sys_lremovexattr);
DECL_TEMPLATE(linux, sys_fremovexattr);

// These are Posix, but not necessarily syscalls.  Darwin only supports
// sched_get_priority_{min,max} and sched_yield, but as libc functions, not
// syscalls.
DECL_TEMPLATE(linux, sys_sched_setparam);
DECL_TEMPLATE(linux, sys_sched_getparam);
DECL_TEMPLATE(linux, sys_sched_setscheduler);
DECL_TEMPLATE(linux, sys_sched_getscheduler);
DECL_TEMPLATE(linux, sys_sched_yield);
DECL_TEMPLATE(linux, sys_sched_get_priority_max);
DECL_TEMPLATE(linux, sys_sched_get_priority_min);
DECL_TEMPLATE(linux, sys_sched_rr_get_interval);
DECL_TEMPLATE(linux, sys_sched_setaffinity);
DECL_TEMPLATE(linux, sys_sched_getaffinity);

// These ones have different parameters and/or return values on Darwin.
// Also, some archs on Linux do not match the generic wrapper for sys_pipe.
DECL_TEMPLATE(linux, sys_munlockall);
DECL_TEMPLATE(linux, sys_pipe);
DECL_TEMPLATE(linux, sys_pipe2);
DECL_TEMPLATE(linux, sys_quotactl);
DECL_TEMPLATE(linux, sys_waitid);

// Posix, but in Darwin utime is a libc function that calls syscall utimes.
DECL_TEMPLATE(linux, sys_utime);

// On Darwin, off_t is 64-bits even on 32-bit platforms.
DECL_TEMPLATE(linux, sys_lseek);

// Darwin (and probably other OSes) don't have the old_sigset_t type.
DECL_TEMPLATE(linux, sys_sigpending);
DECL_TEMPLATE(linux, sys_sigprocmask);
DECL_TEMPLATE(linux, sys_sigaction);

// I think these are Linux-specific?
DECL_TEMPLATE(linux, sys_rt_sigaction);
DECL_TEMPLATE(linux, sys_rt_sigprocmask);
DECL_TEMPLATE(linux, sys_rt_sigpending);
DECL_TEMPLATE(linux, sys_rt_sigtimedwait);
DECL_TEMPLATE(linux, sys_rt_sigqueueinfo);
DECL_TEMPLATE(linux, sys_rt_tgsigqueueinfo);
DECL_TEMPLATE(linux, sys_rt_sigsuspend);

// Linux-specific?
DECL_TEMPLATE(linux, sys_sync_file_range);
DECL_TEMPLATE(linux, sys_sync_file_range2);
DECL_TEMPLATE(linux, sys_stime);  /* maybe generic?  I'm not sure */

// Linux specific (kernel modules)
DECL_TEMPLATE(linux, sys_init_module);
DECL_TEMPLATE(linux, sys_delete_module);

// Linux-specific (oprofile-related)
DECL_TEMPLATE(linux, sys_lookup_dcookie);        // (*/32/64) L

// Linux-specific (new in Linux 3.2)
DECL_TEMPLATE(linux, sys_process_vm_readv);
DECL_TEMPLATE(linux, sys_process_vm_writev);

/* ---------------------------------------------------------------------
   Wrappers for sockets and ipc-ery.  These are split into standalone
   procedures because x86-linux hides them inside multiplexors
   (sys_socketcall and sys_ipc).
   ------------------------------------------------------------------ */

#define TId ThreadId
#define UW  UWord
#define SR  SysRes

extern void ML_(linux_PRE_sys_msgsnd)      ( TId, UW, UW, UW, UW );
extern void ML_(linux_PRE_sys_msgrcv)      ( TId, UW, UW, UW, UW, UW );
extern void ML_(linux_POST_sys_msgrcv)     ( TId, UW, UW, UW, UW, UW, UW );
extern void ML_(linux_PRE_sys_msgctl)      ( TId, UW, UW, UW );
extern void ML_(linux_POST_sys_msgctl)     ( TId, UW, UW, UW, UW );
extern void ML_(linux_PRE_sys_getsockopt)  ( TId, UW, UW, UW, UW, UW );
extern void ML_(linux_POST_sys_getsockopt) ( TId, SR, UW, UW, UW, UW, UW );

#undef TId
#undef UW
#undef SR

#endif   // __PRIV_SYSWRAP_LINUX_H

/*--------------------------------------------------------------------*/
/*--- end                                                          ---*/
/*--------------------------------------------------------------------*/
