
/*--------------------------------------------------------------------*/
/*--- Linux-specific syscalls stuff.          priv_syswrap-linux.h ---*/
/*--------------------------------------------------------------------*/

/*
   This file is part of Valgrind, a dynamic binary instrumentation
   framework.

   Copyright (C) 2000-2017 Nicholas Nethercote
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
   along with this program; if not, see <http://www.gnu.org/licenses/>.

   The GNU General Public License is contained in the file COPYING.
*/

#ifndef __PRIV_SYSWRAP_LINUX_H
#define __PRIV_SYSWRAP_LINUX_H

#include "pub_core_basics.h"     // ThreadId
#include "priv_types_n_macros.h" // DECL_TEMPLATE

// Clone-related functions
extern Word ML_(start_thread_NORETURN) ( void* arg );
extern Addr ML_(allocstack)            ( ThreadId tid );
extern void ML_(call_on_new_stack_0_1) ( Addr stack, Addr retaddr,
			                 void (*f)(Word), Word arg1 );

// Linux-specific (but non-arch-specific) syscalls

DECL_TEMPLATE(linux, sys_clone)
DECL_TEMPLATE(linux, sys_mount);
DECL_TEMPLATE(linux, sys_oldumount);
DECL_TEMPLATE(linux, sys_umount);
DECL_TEMPLATE(linux, sys_perf_event_open);
DECL_TEMPLATE(linux, sys_preadv);
DECL_TEMPLATE(linux, sys_preadv2);
DECL_TEMPLATE(linux, sys_pwritev);
DECL_TEMPLATE(linux, sys_pwritev2);
DECL_TEMPLATE(linux, sys_sendmmsg);
DECL_TEMPLATE(linux, sys_recvmmsg);
DECL_TEMPLATE(linux, sys_dup3);
DECL_TEMPLATE(linux, sys_getcpu);
DECL_TEMPLATE(linux, sys_splice);
DECL_TEMPLATE(linux, sys_tee);
DECL_TEMPLATE(linux, sys_vmsplice);
DECL_TEMPLATE(linux, sys_readahead);
DECL_TEMPLATE(linux, sys_move_pages);

// clone is similar enough between linux variants to have a generic
// version, but which will call an extern defined in syswrap-<platform>-linux.c
DECL_TEMPLATE(linux, sys_clone);

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
DECL_TEMPLATE(linux, sys_pivot_root);
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
DECL_TEMPLATE(linux, sys_epoll_pwait2);
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
DECL_TEMPLATE(linux, sys_clock_adjtime);

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
DECL_TEMPLATE(linux, sys_renameat2);
DECL_TEMPLATE(linux, sys_linkat);
DECL_TEMPLATE(linux, sys_symlinkat);
DECL_TEMPLATE(linux, sys_readlinkat);
DECL_TEMPLATE(linux, sys_fchmodat);
DECL_TEMPLATE(linux, sys_faccessat);
DECL_TEMPLATE(linux, sys_faccessat2);
DECL_TEMPLATE(linux, sys_utimensat);
DECL_TEMPLATE(linux, sys_name_to_handle_at);
DECL_TEMPLATE(linux, sys_open_by_handle_at);

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
DECL_TEMPLATE(linux, sys_sched_setattr);
DECL_TEMPLATE(linux, sys_sched_getattr);
DECL_TEMPLATE(linux, sys_sched_setscheduler);
DECL_TEMPLATE(linux, sys_sched_getscheduler);
DECL_TEMPLATE(linux, sys_sched_yield);
DECL_TEMPLATE(linux, sys_sched_get_priority_max);
DECL_TEMPLATE(linux, sys_sched_get_priority_min);
DECL_TEMPLATE(linux, sys_sched_rr_get_interval);
DECL_TEMPLATE(linux, sys_sched_setaffinity);
DECL_TEMPLATE(linux, sys_sched_getaffinity);

DECL_TEMPLATE(linux, sys_unshare);
DECL_TEMPLATE(linux, sys_setns);

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
DECL_TEMPLATE(linux, sys_finit_module);
DECL_TEMPLATE(linux, sys_delete_module);

// Linux-specific (oprofile-related)
DECL_TEMPLATE(linux, sys_lookup_dcookie);        // (*/32/64) L

// Linux-specific (new in Linux 3.2)
DECL_TEMPLATE(linux, sys_process_vm_readv);
DECL_TEMPLATE(linux, sys_process_vm_writev);

// Linux-specific (new in Linux 2.6.36)
DECL_TEMPLATE(linux, sys_fanotify_init);
DECL_TEMPLATE(linux, sys_fanotify_mark);

// Linux-specific (new in Linux 3.17)
DECL_TEMPLATE(linux, sys_getrandom);
DECL_TEMPLATE(linux, sys_memfd_create);

DECL_TEMPLATE(linux, sys_syncfs);

DECL_TEMPLATE(linux, sys_membarrier);

// Linux-specific (new in Linux 3.18)
DECL_TEMPLATE(linux, sys_bpf);

// Linux-specific (new in Linux 3.19)
DECL_TEMPLATE(linux, sys_execveat);

// Linux-specific (new in Linux 4.11)
DECL_TEMPLATE(linux, sys_statx);

// Linux-specific memory protection key syscalls (since Linux 4.9)
DECL_TEMPLATE(linux, sys_pkey_alloc);
DECL_TEMPLATE(linux, sys_pkey_free);
DECL_TEMPLATE(linux, sys_pkey_mprotect);

// Linux io_uring system calls. See also commit 2b188cc1bb85 ("Add io_uring IO
// interface") # v5.1. See also commit edafccee56ff ("io_uring: add support
// for pre-mapped user IO buffers") # v5.1.
DECL_TEMPLATE(linux, sys_io_uring_setup);
DECL_TEMPLATE(linux, sys_io_uring_enter);
DECL_TEMPLATE(linux, sys_io_uring_register);

// Linux-specific (new in Linux 5.3)
DECL_TEMPLATE(linux, sys_pidfd_open);

// Linux-specific (new in Linux 5.9)
DECL_TEMPLATE(linux, sys_close_range);
DECL_TEMPLATE(linux, sys_openat2);

// Linux-specific (new in Linux 5.14)
DECL_TEMPLATE(linux, sys_memfd_secret);

// Linux-specific (since Linux 5.6)
DECL_TEMPLATE(linux, sys_pidfd_getfd);

// Since Linux 6.6
DECL_TEMPLATE(linux, sys_fchmodat2);

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
extern void ML_(linux_PRE_sys_setsockopt)  ( TId, UW, UW, UW, UW, UW );
extern void ML_(linux_PRE_sys_recvmmsg)    ( TId, UW, UW, UW, UW, UW );
extern void ML_(linux_POST_sys_recvmmsg)   ( TId, UW, UW, UW, UW, UW, UW );
extern void ML_(linux_PRE_sys_sendmmsg)    ( TId, UW, UW, UW, UW );
extern void ML_(linux_POST_sys_sendmmsg)   ( TId, UW, UW, UW, UW, UW );

// Linux-specific (but non-arch-specific) ptrace wrapper helpers
extern void ML_(linux_PRE_getregset) ( ThreadId, long, long );
extern void ML_(linux_PRE_setregset) ( ThreadId, long, long );
extern void ML_(linux_POST_traceme)  ( ThreadId );
extern void ML_(linux_POST_getregset)( ThreadId, long, long );

#undef TId
#undef UW
#undef SR

/* sys_ipc and sys_socketcall are multiplexors which implements several syscalls.
   Used e.g. by x86, ppc32, ppc64, ... */
DECL_TEMPLATE(linux, sys_ipc);
DECL_TEMPLATE(linux, sys_socketcall);

/* Depending on the platform, the below are implemented as
   direct syscalls or via the above sys_socketcall multiplexor. */

/* Direct ipc related syscalls. */
/* Semaphore */
DECL_TEMPLATE(linux, sys_semget);
DECL_TEMPLATE(linux, sys_semop);
DECL_TEMPLATE(linux, sys_semctl);
DECL_TEMPLATE(linux, sys_semtimedop);
/* Shared memory */
DECL_TEMPLATE(linux, sys_shmat);
DECL_TEMPLATE(linux, sys_shmget);
DECL_TEMPLATE(linux, sys_shmdt);
DECL_TEMPLATE(linux, sys_shmctl);
/* Message queue */
DECL_TEMPLATE(linux, sys_msgget);
DECL_TEMPLATE(linux, sys_msgrcv);
DECL_TEMPLATE(linux, sys_msgsnd);
DECL_TEMPLATE(linux, sys_msgctl);

/* Direct socket related syscalls. */
DECL_TEMPLATE(linux, sys_socket);
DECL_TEMPLATE(linux, sys_setsockopt);
DECL_TEMPLATE(linux, sys_getsockopt);
DECL_TEMPLATE(linux, sys_connect);
DECL_TEMPLATE(linux, sys_accept);
DECL_TEMPLATE(linux, sys_accept4);
DECL_TEMPLATE(linux, sys_send);
DECL_TEMPLATE(linux, sys_sendto);
DECL_TEMPLATE(linux, sys_recv);
DECL_TEMPLATE(linux, sys_recvfrom);
DECL_TEMPLATE(linux, sys_sendmsg);
DECL_TEMPLATE(linux, sys_recvmsg);
DECL_TEMPLATE(linux, sys_shutdown);
DECL_TEMPLATE(linux, sys_bind);
DECL_TEMPLATE(linux, sys_listen);
DECL_TEMPLATE(linux, sys_getsockname);
DECL_TEMPLATE(linux, sys_getpeername);
DECL_TEMPLATE(linux, sys_socketpair);
DECL_TEMPLATE(linux, sys_kcmp);
DECL_TEMPLATE(linux, sys_copy_file_range);

/* 64bit time_t syscalls for 32bit arches.  */
DECL_TEMPLATE(linux, sys_clock_gettime64)
DECL_TEMPLATE(linux, sys_clock_settime64)
// clock_adjtime64
DECL_TEMPLATE(linux, sys_clock_getres_time64)
DECL_TEMPLATE(linux, sys_clock_nanosleep_time64);
DECL_TEMPLATE(linux, sys_timer_gettime64);
DECL_TEMPLATE(linux, sys_timer_settime64);
DECL_TEMPLATE(linux, sys_timerfd_gettime64);
DECL_TEMPLATE(linux, sys_timerfd_settime64);
DECL_TEMPLATE(linux, sys_utimensat_time64);
DECL_TEMPLATE(linux, sys_pselect6_time64);
DECL_TEMPLATE(linux, sys_ppoll_time64);
// io_pgetevents_time64
DECL_TEMPLATE(linux, sys_recvmmsg_time64);
DECL_TEMPLATE(linux, sys_mq_timedsend_time64);
DECL_TEMPLATE(linux, sys_mq_timedreceive_time64);
DECL_TEMPLATE(linux, sys_semtimedop_time64);
DECL_TEMPLATE(linux, sys_rt_sigtimedwait_time64);
DECL_TEMPLATE(linux, sys_futex_time64);
DECL_TEMPLATE(linux, sys_sched_rr_get_interval_time64);

// Some arch specific functions called from syswrap-linux.c
extern Int do_syscall_clone_x86_linux ( Word (*fn)(void *), 
                                        void* stack, 
                                        Int   flags, 
                                        void* arg,
                                        Int*  child_tid, 
                                        Int*  parent_tid, 
                                        void* tls_ptr);
extern SysRes ML_(x86_sys_set_thread_area) ( ThreadId tid,
                                             vki_modify_ldt_t* info );
extern void ML_(x86_setup_LDT_GDT) ( /*OUT*/ ThreadArchState *child, 
                                     /*IN*/  ThreadArchState *parent );

extern Long do_syscall_clone_amd64_linux ( Word (*fn)(void *), 
                                           void* stack, 
                                           Long  flags, 
                                           void* arg,
                                           Int* child_tid, 
                                           Int* parent_tid, 
                                           void* tls_ptr);
extern ULong do_syscall_clone_ppc32_linux ( Word (*fn)(void *), 
                                            void* stack, 
                                            Int   flags, 
                                            void* arg,
                                            Int*  child_tid, 
                                            Int*  parent_tid, 
                                            void* tls_ptr);
extern ULong do_syscall_clone_ppc64_linux ( Word (*fn)(void *), 
                                            void* stack, 
                                            Int   flags, 
                                            void* arg,
                                            Int*  child_tid, 
                                            Int*  parent_tid, 
                                            void* tls_ptr );
extern ULong do_syscall_clone_s390x_linux ( void  *stack,
                                            ULong flags,
                                            Int   *parent_tid,
                                            Int   *child_tid,
                                            void*  tls_ptr,
                                            Word (*fn)(void *),
                                            void  *arg);
extern Long do_syscall_clone_arm64_linux ( Word (*fn)(void *), 
                                           void* stack, 
                                           Long  flags, 
                                           void* arg,
                                           Int*  child_tid,
                                           Int*  parent_tid,
                                           void* tls_ptr );
extern ULong do_syscall_clone_arm_linux   ( Word (*fn)(void *), 
                                            void* stack, 
                                            Int   flags, 
                                            void* arg,
                                            Int*  child_tid,
                                            Int*  parent_tid,
                                            void* tls_ptr );
extern ULong do_syscall_clone_mips64_linux ( Word (*fn) (void *),  /* a0 - 4 */
                                             void* stack,          /* a1 - 5 */
                                             Int   flags,          /* a2 - 6 */
                                             void* arg,            /* a3 - 7 */
                                             Int*  parent_tid,     /* a4 - 8 */
                                             void* tls_ptr,        /* a5 - 9 */
                                             Int*  child_tid );    /* a6 - 10 */
extern UInt do_syscall_clone_mips_linux ( Word (*fn) (void *), //a0     0    32
                                          void* stack,         //a1     4    36
                                          Int   flags,         //a2     8    40
                                          void* arg,           //a3     12   44
                                          Int*  child_tid,     //stack  16   48
                                          Int*  parent_tid,    //stack  20   52
                                          void* tls_ptr);      //stack  24   56
extern UInt do_syscall_clone_nanomips_linux ( Word (*fn) (void *),  /* a0 - 4 */
                                              void* stack,          /* a1 - 5 */
                                              Int   flags,          /* a2 - 6 */
                                              void* arg,            /* a3 - 7 */
                                              Int*  child_tid,      /* a4 - 8 */
                                              Int*  parent_tid,     /* a5 - 9 */
                                              void* tls_ptr);       /* a6 - 10 */
#endif   // __PRIV_SYSWRAP_LINUX_H

/*--------------------------------------------------------------------*/
/*--- end                                                          ---*/
/*--------------------------------------------------------------------*/
