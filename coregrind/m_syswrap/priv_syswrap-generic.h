
/*--------------------------------------------------------------------*/
/*--- Private syscalls header.              priv_syswrap-generic.h ---*/
/*--------------------------------------------------------------------*/

/*
   This file is part of Valgrind, a dynamic binary instrumentation
   framework.

   Copyright (C) 2000-2005 Julian Seward
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

#ifndef __PRIV_SYSWRAP_GENERIC_H
#define __PRIV_SYSWRAP_GENERIC_H

/* requires #include "priv_types_n_macros.h" */


// Return true if address range entirely contained within client
// address space.
extern
Bool VG_(valid_client_addr)(Addr start, SizeT size, ThreadId tid,
                            const Char *syscallname);

// Returns True if the signal is OK for the client to use.
extern Bool VG_(client_signal_OK)(Int sigNo);

// Return true if we're allowed to use or create this fd.
extern
Bool VG_(fd_allowed)(Int fd, const Char *syscallname, ThreadId tid, Bool soft);

extern
void VG_(record_fd_open)(ThreadId tid, Int fd, char *pathname);

// Used when killing threads -- we must not kill a thread if it's the thread
// that would do Valgrind's final cleanup and output.
extern
Bool VG_(do_sigkill)(Int pid, Int tgid);

/* So that it can be seen from syswrap-x86-linux.c. */
extern 
void VG_(mmap_segment) ( Addr a, SizeT len, UInt prot, 
                         UInt mm_flags, Int fd, ULong offset );


DECL_TEMPLATE(generic, sys_ni_syscall);            // * P -- unimplemented
DECL_TEMPLATE(generic, sys_exit);
DECL_TEMPLATE(generic, sys_fork);
DECL_TEMPLATE(generic, sys_read);
DECL_TEMPLATE(generic, sys_write);
DECL_TEMPLATE(generic, sys_open);
DECL_TEMPLATE(generic, sys_close);
DECL_TEMPLATE(generic, sys_waitpid);
DECL_TEMPLATE(generic, sys_creat);
DECL_TEMPLATE(generic, sys_link);
DECL_TEMPLATE(generic, sys_unlink);
DECL_TEMPLATE(generic, sys_execve);    // (*??) P
DECL_TEMPLATE(generic, sys_chdir);
DECL_TEMPLATE(generic, sys_time);
DECL_TEMPLATE(generic, sys_mknod);
DECL_TEMPLATE(generic, sys_chmod);
DECL_TEMPLATE(generic, sys_lseek);
DECL_TEMPLATE(generic, sys_getpid);
DECL_TEMPLATE(generic, sys_alarm);
DECL_TEMPLATE(generic, sys_pause);
DECL_TEMPLATE(generic, sys_utime);
DECL_TEMPLATE(generic, sys_access);
DECL_TEMPLATE(generic, sys_kill);
DECL_TEMPLATE(generic, sys_rename);
DECL_TEMPLATE(generic, sys_mkdir);
DECL_TEMPLATE(generic, sys_rmdir);
DECL_TEMPLATE(generic, sys_dup);
DECL_TEMPLATE(generic, sys_times);
DECL_TEMPLATE(generic, sys_fcntl);        // POSIX (but complicated)
DECL_TEMPLATE(generic, sys_setpgid);
DECL_TEMPLATE(generic, sys_umask);
DECL_TEMPLATE(generic, sys_dup2);
DECL_TEMPLATE(generic, sys_getppid);
DECL_TEMPLATE(generic, sys_getpgrp);
DECL_TEMPLATE(generic, sys_setsid);
DECL_TEMPLATE(generic, sys_munmap);
DECL_TEMPLATE(generic, sys_truncate);
DECL_TEMPLATE(generic, sys_ftruncate);
DECL_TEMPLATE(generic, sys_fchmod);
DECL_TEMPLATE(generic, sys_msync);
DECL_TEMPLATE(generic, sys_readv);
DECL_TEMPLATE(generic, sys_writev);
DECL_TEMPLATE(generic, sys_getsid);
DECL_TEMPLATE(generic, sys_fdatasync);
DECL_TEMPLATE(generic, sys_mlock);
DECL_TEMPLATE(generic, sys_munlock);
DECL_TEMPLATE(generic, sys_mlockall);
DECL_TEMPLATE(generic, sys_munlockall);
DECL_TEMPLATE(generic, sys_sched_setparam);
DECL_TEMPLATE(generic, sys_sched_getparam);
DECL_TEMPLATE(generic, sys_sched_rr_get_interval);
DECL_TEMPLATE(generic, sys_sched_setscheduler);
DECL_TEMPLATE(generic, sys_sched_getscheduler);
DECL_TEMPLATE(generic, sys_sched_yield);
DECL_TEMPLATE(generic, sys_sched_get_priority_max);
DECL_TEMPLATE(generic, sys_sched_get_priority_min);
DECL_TEMPLATE(generic, sys_nanosleep);
DECL_TEMPLATE(generic, sys_mremap);    // POSIX, but Linux arg order may be odd
DECL_TEMPLATE(generic, sys_getuid);
DECL_TEMPLATE(generic, sys_getgid);
DECL_TEMPLATE(generic, sys_geteuid);
DECL_TEMPLATE(generic, sys_getegid);
DECL_TEMPLATE(generic, sys_getpgid);
DECL_TEMPLATE(generic, sys_fsync);
DECL_TEMPLATE(generic, sys_wait4);
DECL_TEMPLATE(generic, sys_mprotect);
DECL_TEMPLATE(generic, sys_sigprocmask);
DECL_TEMPLATE(generic, sys_timer_create);    // Linux: varies across archs?
DECL_TEMPLATE(generic, sys_timer_settime);
DECL_TEMPLATE(generic, sys_timer_gettime);
DECL_TEMPLATE(generic, sys_timer_getoverrun);
DECL_TEMPLATE(generic, sys_timer_delete);
DECL_TEMPLATE(generic, sys_clock_settime);
DECL_TEMPLATE(generic, sys_clock_gettime);
DECL_TEMPLATE(generic, sys_clock_getres);
DECL_TEMPLATE(generic, sys_clock_nanosleep);
DECL_TEMPLATE(generic, sys_getcwd);
DECL_TEMPLATE(generic, sys_symlink);
DECL_TEMPLATE(generic, sys_getgroups);
DECL_TEMPLATE(generic, sys_setgroups);             // SVr4, SVID, X/OPEN, 4.3BSD
DECL_TEMPLATE(generic, sys_chown);
DECL_TEMPLATE(generic, sys_setuid);
DECL_TEMPLATE(generic, sys_gettimeofday);
DECL_TEMPLATE(generic, sys_madvise);
DECL_TEMPLATE(generic, sys_sigpending);

// These ones aren't POSIX, but are in some standard and look reasonably
// generic,  and are the same for all architectures under Linux.
DECL_TEMPLATE(generic, sys_nice);      // SVr4, SVID EXT, AT&T, X/OPEN, BSD 4.3
DECL_TEMPLATE(generic, sys_sync);      // SVr4, SVID, X/OPEN, BSD 4.3
DECL_TEMPLATE(generic, sys_brk);       // 4.3BSD
DECL_TEMPLATE(generic, sys_acct);      // SVR4, non-POSIX
DECL_TEMPLATE(generic, sys_chroot);    // SVr4, SVID, 4.4BSD, X/OPEN
DECL_TEMPLATE(generic, sys_readlink);  // X/OPEN, 4.4BSD
DECL_TEMPLATE(generic, sys_fchdir);    // SVr4, SVID, POSIX, X/OPEN, 4.4BSD
DECL_TEMPLATE(generic, sys_getdents);  // SVr4,SVID
DECL_TEMPLATE(generic, sys_select);    // 4.4BSD
DECL_TEMPLATE(generic, sys_flock);     // 4.4BSD
DECL_TEMPLATE(generic, sys_poll);      // XPG4-UNIX
DECL_TEMPLATE(generic, sys_getrusage); // SVr4, 4.3BSD
DECL_TEMPLATE(generic, sys_stime);	    // SVr4, SVID, X/OPEN
DECL_TEMPLATE(generic, sys_settimeofday); // SVr4, 4.3BSD (non-POSIX)
DECL_TEMPLATE(generic, sys_getpriority);  // SVr4, 4.4BSD
DECL_TEMPLATE(generic, sys_setpriority);  // SVr4, 4.4BSD
DECL_TEMPLATE(generic, sys_setitimer);    // SVr4, 4.4BSD
DECL_TEMPLATE(generic, sys_getitimer);    // SVr4, 4.4BSD
DECL_TEMPLATE(generic, sys_setreuid);     // 4.3BSD
DECL_TEMPLATE(generic, sys_setregid);     // 4.3BSD
DECL_TEMPLATE(generic, sys_fchown);       // SVr4,4.3BSD
DECL_TEMPLATE(generic, sys_setgid);       // SVr4,SVID
DECL_TEMPLATE(generic, sys_utimes);       // 4.3BSD

// These ones may be Linux specific... not sure.  They use 16-bit gid_t and
// uid_t types.  The similarly named (minus the "16" suffix) ones below use
// 32-bit versions of these types.
DECL_TEMPLATE(generic, sys_setuid16);              // ## P
DECL_TEMPLATE(generic, sys_getuid16);              // ## P
DECL_TEMPLATE(generic, sys_setgid16);              // ## SVr4,SVID
DECL_TEMPLATE(generic, sys_getgid16);              // ## P
DECL_TEMPLATE(generic, sys_geteuid16);             // ## P
DECL_TEMPLATE(generic, sys_getegid16);             // ## P
DECL_TEMPLATE(generic, sys_setreuid16);            // ## BSD4.3
DECL_TEMPLATE(generic, sys_setregid16);            // ## BSD4.3
DECL_TEMPLATE(generic, sys_getgroups16);           // ## P
DECL_TEMPLATE(generic, sys_setgroups16);           // ## SVr4, SVID, X/OPEN, 4.3BSD
DECL_TEMPLATE(generic, sys_fchown16);              // ## SVr4,BSD4.3
DECL_TEMPLATE(generic, sys_chown16);               // ## P

// Some archs on Linux do not match the generic wrapper for sys_pipe().
DECL_TEMPLATE(generic, sys_pipe);

// May not be generic for every architecture under Linux.
DECL_TEMPLATE(generic, sys_sigaction);             // (x86) P

// Funny names, not sure...
DECL_TEMPLATE(generic, sys_newstat);               // * P
DECL_TEMPLATE(generic, sys_newlstat);              // *
DECL_TEMPLATE(generic, sys_newfstat);              // * P (SVr4,BSD4.3)

// For the remainder, not really sure yet
DECL_TEMPLATE(generic, sys_ptrace);                // (x86?) (almost-P)
DECL_TEMPLATE(generic, sys_sigsuspend);            // POSIX, but L (proto varies across archs)
DECL_TEMPLATE(generic, sys_setrlimit);             // SVr4, 4.3BSD
DECL_TEMPLATE(generic, sys_ioctl);                 // x86? (various)
DECL_TEMPLATE(generic, sys_old_getrlimit);         // SVr4, 4.3BSD L?
DECL_TEMPLATE(generic, sys_statfs);                // * L?
DECL_TEMPLATE(generic, sys_fstatfs);               // * L?
DECL_TEMPLATE(generic, sys_iopl);                  // (x86/amd64) L
DECL_TEMPLATE(generic, sys_ipc);                   // (x86) L
DECL_TEMPLATE(generic, sys_newuname);              // * P
DECL_TEMPLATE(generic, sys_init_module);           // * L?
DECL_TEMPLATE(generic, sys_quotactl);              // * (?)
DECL_TEMPLATE(generic, sys_rt_sigaction);          // (x86) ()
DECL_TEMPLATE(generic, sys_rt_sigprocmask);        // * ?
DECL_TEMPLATE(generic, sys_rt_sigpending);         // * ?
DECL_TEMPLATE(generic, sys_rt_sigtimedwait);       // * ?
DECL_TEMPLATE(generic, sys_rt_sigqueueinfo);       // * ?
DECL_TEMPLATE(generic, sys_rt_sigsuspend);         // () ()
DECL_TEMPLATE(generic, sys_pread64);               // * (Unix98?)
DECL_TEMPLATE(generic, sys_pwrite64);              // * (Unix98?)
DECL_TEMPLATE(generic, sys_capget);                // * L?
DECL_TEMPLATE(generic, sys_capset);                // * L?
DECL_TEMPLATE(generic, sys_sigaltstack);           // (x86) (XPG4-UNIX)
DECL_TEMPLATE(generic, sys_getpmsg);               // (?) (?)
DECL_TEMPLATE(generic, sys_putpmsg);               // (?) (?)
DECL_TEMPLATE(generic, sys_getrlimit);             // * (?)
DECL_TEMPLATE(generic, sys_mmap2);                 // (x86?) P?
DECL_TEMPLATE(generic, sys_truncate64);            // %% (P?)
DECL_TEMPLATE(generic, sys_ftruncate64);           // %% (P?)
DECL_TEMPLATE(generic, sys_lchown);                // * (L?)
DECL_TEMPLATE(generic, sys_mincore);               // * L?
DECL_TEMPLATE(generic, sys_getdents64);            // * (SVr4,SVID?)
DECL_TEMPLATE(generic, sys_fcntl64);               // * P?
DECL_TEMPLATE(generic, sys_setxattr);              // * L?
DECL_TEMPLATE(generic, sys_lsetxattr);             // * L?
DECL_TEMPLATE(generic, sys_fsetxattr);             // * L?
DECL_TEMPLATE(generic, sys_getxattr);              // * L?
DECL_TEMPLATE(generic, sys_lgetxattr);             // * L?
DECL_TEMPLATE(generic, sys_fgetxattr);             // * L?
DECL_TEMPLATE(generic, sys_listxattr);             // * L?
DECL_TEMPLATE(generic, sys_llistxattr);            // * L?
DECL_TEMPLATE(generic, sys_flistxattr);            // * L?
DECL_TEMPLATE(generic, sys_removexattr);           // * L?
DECL_TEMPLATE(generic, sys_lremovexattr);          // * L?
DECL_TEMPLATE(generic, sys_fremovexattr);          // * L?
DECL_TEMPLATE(generic, sys_sched_setaffinity);     // * L?
DECL_TEMPLATE(generic, sys_sched_getaffinity);     // * L?
DECL_TEMPLATE(generic, sys_lookup_dcookie);        // (*/32/64) L
DECL_TEMPLATE(generic, sys_statfs64);              // * (?)
DECL_TEMPLATE(generic, sys_fstatfs64);             // * (?)
DECL_TEMPLATE(generic, sys_mq_open);               // * P?
DECL_TEMPLATE(generic, sys_mq_unlink);             // * P?
DECL_TEMPLATE(generic, sys_mq_timedsend);          // * P?
DECL_TEMPLATE(generic, sys_mq_timedreceive);       // * P?
DECL_TEMPLATE(generic, sys_mq_notify);             // * P?
DECL_TEMPLATE(generic, sys_mq_getsetattr);         // * P?


/* ---------------------------------------------------------------------
   Wrappers for sockets and ipc-ery.  These are split into standalone
   procedures because x86-linux hides them inside multiplexors
   (sys_socketcall and sys_ipc).
   ------------------------------------------------------------------ */

#define TId ThreadId
#define UW  UWord
#define SR  SysRes

extern void   VG_(generic_PRE_sys_socketpair)   ( TId, UW, UW, UW, UW );
extern SysRes VG_(generic_POST_sys_socketpair)  ( TId, SR, UW, UW, UW, UW );
extern SysRes VG_(generic_POST_sys_socket)      ( TId, SR );
extern void   VG_(generic_PRE_sys_bind)         ( TId, UW, UW, UW );
extern void   VG_(generic_PRE_sys_accept)       ( TId, UW, UW, UW );
extern SysRes VG_(generic_POST_sys_accept)      ( TId, SR, UW, UW, UW );
extern void   VG_(generic_PRE_sys_sendto)       ( TId, UW, UW, UW, UW, UW, UW );
extern void   VG_(generic_PRE_sys_send)         ( TId, UW, UW, UW );
extern void   VG_(generic_PRE_sys_recvfrom)     ( TId, UW, UW, UW, UW, UW, UW );
extern void   VG_(generic_POST_sys_recvfrom)    ( TId, SR, UW, UW, UW, UW, UW, UW );
extern void   VG_(generic_PRE_sys_recv)         ( TId, UW, UW, UW );
extern void   VG_(generic_POST_sys_recv)        ( TId, UW, UW, UW, UW );
extern void   VG_(generic_PRE_sys_connect)      ( TId, UW, UW, UW );
extern void   VG_(generic_PRE_sys_setsockopt)   ( TId, UW, UW, UW, UW, UW );
extern void   VG_(generic_PRE_sys_getsockopt)   ( TId, UW, UW, UW, UW, UW );
extern void   VG_(generic_POST_sys_getsockopt)  ( TId, SR, UW, UW, UW, UW, UW );
extern void   VG_(generic_PRE_sys_getsockname)  ( TId, UW, UW, UW );
extern void   VG_(generic_POST_sys_getsockname) ( TId, SR, UW, UW, UW );
extern void   VG_(generic_PRE_sys_getpeername)  ( TId, UW, UW, UW );
extern void   VG_(generic_POST_sys_getpeername) ( TId, SR, UW, UW, UW );
extern void   VG_(generic_PRE_sys_sendmsg)      ( TId, UW, UW );
extern void   VG_(generic_PRE_sys_recvmsg)      ( TId, UW, UW );
extern void   VG_(generic_POST_sys_recvmsg)     ( TId, UW, UW );

extern void   VG_(generic_PRE_sys_semop)        ( TId, UW, UW, UW );
extern void   VG_(generic_PRE_sys_semtimedop)   ( TId, UW, UW, UW, UW );
extern void   VG_(generic_PRE_sys_semctl)       ( TId, UW, UW, UW, UW );
extern void   VG_(generic_POST_sys_semctl)      ( TId, UW, UW, UW, UW, UW );
extern void   VG_(generic_PRE_sys_msgsnd)       ( TId, UW, UW, UW, UW );
extern void   VG_(generic_PRE_sys_msgrcv)       ( TId, UW, UW, UW, UW, UW );
extern void   VG_(generic_POST_sys_msgrcv)      ( TId, UW, UW, UW, UW, UW, UW );
extern void   VG_(generic_PRE_sys_msgctl)       ( TId, UW, UW, UW );
extern void   VG_(generic_POST_sys_msgctl)      ( TId, UW, UW, UW, UW );
extern UWord  VG_(generic_PRE_sys_shmat)        ( TId, UW, UW, UW );
extern void   VG_(generic_POST_sys_shmat)       ( TId, UW, UW, UW, UW );
extern Bool   VG_(generic_PRE_sys_shmdt)        ( TId, UW );
extern void   VG_(generic_POST_sys_shmdt)       ( TId, UW, UW );
extern void   VG_(generic_PRE_sys_shmctl)       ( TId, UW, UW, UW );
extern void   VG_(generic_POST_sys_shmctl)      ( TId, UW, UW, UW, UW );

#undef TId
#undef UW
#undef SR


/////////////////////////////////////////////////////////////////


#endif   // __PRIV_SYSWRAP_GENERIC_H

/*--------------------------------------------------------------------*/
/*--- end                                                          ---*/
/*--------------------------------------------------------------------*/
