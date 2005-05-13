/*--------------------------------------------------------------------*/
/*--- Private syscalls header.                     priv_syscalls.h ---*/
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

#ifndef __PRIV_SYSCALLS_H
#define __PRIV_SYSCALLS_H

// This is ugly, but the alternatives (ie. doing it "properly" with -I options
// and directories and more Makefiles) are even worse.
#if defined(VGP_amd64_linux)
#  include "priv_syscalls-amd64-linux.h"
#elif defined(VGP_arm_linux)
#  include "priv_syscalls-arm-linux.h"
#elif defined(VGP_x86_linux)
#  include "priv_syscalls-x86-linux.h"
#else
#  error Unknown platform
#endif

// Offsets for the shadow state
#define O_SYSCALL_NUM   (offsetof(VexGuestArchState, VGP_SYSCALL_NUM))
#define O_SYSCALL_ARG1  (offsetof(VexGuestArchState, VGP_SYSCALL_ARG1))
#define O_SYSCALL_ARG2  (offsetof(VexGuestArchState, VGP_SYSCALL_ARG2))
#define O_SYSCALL_ARG3  (offsetof(VexGuestArchState, VGP_SYSCALL_ARG3))
#define O_SYSCALL_ARG4  (offsetof(VexGuestArchState, VGP_SYSCALL_ARG4))
#define O_SYSCALL_ARG5  (offsetof(VexGuestArchState, VGP_SYSCALL_ARG5))
#define O_SYSCALL_ARG6  (offsetof(VexGuestArchState, VGP_SYSCALL_ARG6))
#define O_SYSCALL_RET   (offsetof(VexGuestArchState, VGP_SYSCALL_RET))

// The syscall table
struct SyscallTableEntry {
   UInt  *flags_ptr;
   void (*before)(ThreadId tid, ThreadState *tst /*, UInt *flags*/);
   void (*after) (ThreadId tid, ThreadState *tst);
};

/* This table is the mapping from __NR_xxx syscall numbers to the PRE/POST
   wrappers for the relevant syscalls used in the OS kernel for that number.
   Note that the constant names don't always match the wrapper names in a
   straightforward way.  For example, on x86/Linux: 
      
      __NR_lchown       --> sys_lchown16()
      __NR_lchown32     --> sys_lchown()
      __NR_select       --> old_select()
      __NR__newselect   --> sys_select()
*/
extern const struct SyscallTableEntry VGA_(syscall_table)[];

extern const UInt VGA_(syscall_table_size);
   
extern Int VG_(clone) ( Int (*fn)(void *), void *stack, Int flags, void *arg, 
			Int *child_tid, Int *parent_tid, vki_modify_ldt_t * );

/*
  Perform a syscall on behalf of a client thread, using a specific
  signal mask.  On completion, the signal mask is set to restore_mask
  (which presumably blocks almost everything).  If a signal happens
  during the syscall, the handler should call
  VGA_(interrupted_syscall)() to adjust the thread's context to do the
  right thing.
*/
extern void VGA_(client_syscall)(Int syscallno, ThreadState *tst,
				 const vki_sigset_t *syscall_mask);

/* Simple Valgrind-internal atfork mechanism */
extern void VG_(do_atfork_pre)   (ThreadId tid);
extern void VG_(do_atfork_parent)(ThreadId tid);
extern void VG_(do_atfork_child) (ThreadId tid);

// Return true if address range entirely contained within client
// address space.
Bool VG_(valid_client_addr)(Addr start, SizeT size, ThreadId tid,
                            const Char *syscallname);

// Return true if we're allowed to use or create this fd.
Bool VG_(fd_allowed)(Int fd, const Char *syscallname, ThreadId tid, Bool soft);

void VG_(record_fd_open)(ThreadId tid, Int fd, char *pathname);

// Used when killing threads -- we must not kill a thread if it's the thread
// that would do Valgrind's final cleanup and output.
Bool VG_(do_sigkill)(Int pid, Int tgid);
   
// Flags describing syscall wrappers
#define Special    (1 << 0)	/* handled specially			*/
#define MayBlock   (1 << 1)	/* may block				*/
#define PostOnFail (1 << 2)	/* call POST() function on failure	*/
#define PadAddr	   (1 << 3)	/* pad+unpad address space around syscall */
#define Done       (1 << 4)	/* used if a PRE() did the syscall	*/

// Templates for generating the PRE and POST macros.  For ones that must be
// publically visible, use an empty 'qual', 'prefix' should start with
// "vgArch_" or similar, and there should be corresponding global
// declarations (like the GEN_SYSCALL_WRAPPER ones below).  Otherwise, use
// "static" for 'qual', and "vgArch_" should not be in the 'prefix'.
#define PRE_TEMPLATE(qual, prefix, name, f) \
   qual UInt prefix##_##name##_flags = f; \
   qual void prefix##_##name##_before(ThreadId tid, ThreadState *tst)
#define POST_TEMPLATE(qual, prefix, name) \
   qual void prefix##_##name##_after (ThreadId tid, ThreadState *tst)

// This macro is used to write other macros which making writing syscall
// tables easier.
#define SYS_WRAPPER_ENTRY_X_(prefix, const, name) \
   [const] = { &prefix##_##name##_flags, \
                prefix##_##name##_before, NULL }
#define SYS_WRAPPER_ENTRY_XY(prefix, const, name) \
   [const] = { &prefix##_##name##_flags, \
                prefix##_##name##_before, \
                prefix##_##name##_after }

// Macros for adding generic wrappers to a syscall table.
#define GENX_(const, name)    SYS_WRAPPER_ENTRY_X_(vgArch_gen, const, name)
#define GENXY(const, name)    SYS_WRAPPER_ENTRY_XY(vgArch_gen, const, name)

// Space-saving macros for syscall PRE() and POST() wrappers
#define RES    ((tst->arch).vex.VGP_SYSCALL_RET)
#define SYSNO  ((tst->arch).vex.VGP_SYSCALL_NUM)
#define ARG1   ((tst->arch).vex.VGP_SYSCALL_ARG1)
#define ARG2   ((tst->arch).vex.VGP_SYSCALL_ARG2)
#define ARG3   ((tst->arch).vex.VGP_SYSCALL_ARG3)
#define ARG4   ((tst->arch).vex.VGP_SYSCALL_ARG4)
#define ARG5   ((tst->arch).vex.VGP_SYSCALL_ARG5)
#define ARG6   ((tst->arch).vex.VGP_SYSCALL_ARG6)

// For setting the result of a syscall in a wrapper
#define SET_RESULT(val)                            \
   do { VGP_SET_SYSCALL_RESULT(tst->arch, (val));  \
        tst->syscall_result_set = True;            \
   } while (0)

#define PRINT(format, args...)  \
   if (VG_(clo_trace_syscalls))        \
      VG_(printf)(format, ## args)

// Generic (platform-independent) syscall wrappers.  These are generally
// POSIX or something like that;  those that are not POSIX are annotated
// with what standards they are part of, as stated in the Linux man pages.
// For many of them, it's unclear if they are generic, or Linux-specific, or
// x86/Linux-specific, or something else again.
//
// Nb: This list may change over time... ones thought at first to be generic
// may turn out not to be, and so be moved into OS-specific or
// platform-specific files.  If there's any doubt, I'm leaving them in here.
//
// Nb 2: if porting to a new OS, you should really check all these generic
// wrappers to make sure they match your OS, painful as it might be.
//
// For each generic ("gen") wrapper, we declare the pre-wrapper, the
// post-wrapper (which is actually not always needed), and the associated
// flags.
#define GEN_SYSCALL_WRAPPER(x) \
   extern UInt VGA_(gen_##x##_flags); \
   extern void VGA_(gen_##x##_before)(ThreadId tid, ThreadState *tst); \
   extern void VGA_(gen_##x##_after) (ThreadId tid, ThreadState *tst)

GEN_SYSCALL_WRAPPER(sys_ni_syscall);            // * P -- unimplemented
GEN_SYSCALL_WRAPPER(sys_exit);
GEN_SYSCALL_WRAPPER(sys_fork);
GEN_SYSCALL_WRAPPER(sys_read);
GEN_SYSCALL_WRAPPER(sys_write);
GEN_SYSCALL_WRAPPER(sys_open);
GEN_SYSCALL_WRAPPER(sys_close);
GEN_SYSCALL_WRAPPER(sys_waitpid);
GEN_SYSCALL_WRAPPER(sys_creat);
GEN_SYSCALL_WRAPPER(sys_link);
GEN_SYSCALL_WRAPPER(sys_unlink);
GEN_SYSCALL_WRAPPER(sys_execve);    // (*??) P
GEN_SYSCALL_WRAPPER(sys_chdir);
GEN_SYSCALL_WRAPPER(sys_time);
GEN_SYSCALL_WRAPPER(sys_mknod);
GEN_SYSCALL_WRAPPER(sys_chmod);
GEN_SYSCALL_WRAPPER(sys_lseek);
GEN_SYSCALL_WRAPPER(sys_getpid);
GEN_SYSCALL_WRAPPER(sys_alarm);
GEN_SYSCALL_WRAPPER(sys_pause);
GEN_SYSCALL_WRAPPER(sys_utime);
GEN_SYSCALL_WRAPPER(sys_access);
GEN_SYSCALL_WRAPPER(sys_kill);
GEN_SYSCALL_WRAPPER(sys_rename);
GEN_SYSCALL_WRAPPER(sys_mkdir);
GEN_SYSCALL_WRAPPER(sys_rmdir);
GEN_SYSCALL_WRAPPER(sys_dup);
GEN_SYSCALL_WRAPPER(sys_times);
GEN_SYSCALL_WRAPPER(sys_fcntl);        // POSIX (but complicated)
GEN_SYSCALL_WRAPPER(sys_setpgid);
GEN_SYSCALL_WRAPPER(sys_umask);
GEN_SYSCALL_WRAPPER(sys_dup2);
GEN_SYSCALL_WRAPPER(sys_getppid);
GEN_SYSCALL_WRAPPER(sys_getpgrp);
GEN_SYSCALL_WRAPPER(sys_setsid);
GEN_SYSCALL_WRAPPER(sys_munmap);
GEN_SYSCALL_WRAPPER(sys_truncate);
GEN_SYSCALL_WRAPPER(sys_ftruncate);
GEN_SYSCALL_WRAPPER(sys_fchmod);
GEN_SYSCALL_WRAPPER(sys_msync);
GEN_SYSCALL_WRAPPER(sys_readv);
GEN_SYSCALL_WRAPPER(sys_writev);
GEN_SYSCALL_WRAPPER(sys_getsid);
GEN_SYSCALL_WRAPPER(sys_fdatasync);
GEN_SYSCALL_WRAPPER(sys_mlock);
GEN_SYSCALL_WRAPPER(sys_munlock);
GEN_SYSCALL_WRAPPER(sys_mlockall);
GEN_SYSCALL_WRAPPER(sys_munlockall);
GEN_SYSCALL_WRAPPER(sys_sched_setparam);
GEN_SYSCALL_WRAPPER(sys_sched_getparam);
GEN_SYSCALL_WRAPPER(sys_sched_rr_get_interval);
GEN_SYSCALL_WRAPPER(sys_sched_setscheduler);
GEN_SYSCALL_WRAPPER(sys_sched_getscheduler);
GEN_SYSCALL_WRAPPER(sys_sched_yield);
GEN_SYSCALL_WRAPPER(sys_sched_get_priority_max);
GEN_SYSCALL_WRAPPER(sys_sched_get_priority_min);
GEN_SYSCALL_WRAPPER(sys_nanosleep);
GEN_SYSCALL_WRAPPER(sys_mremap);    // POSIX, but Linux arg order may be odd
GEN_SYSCALL_WRAPPER(sys_getuid);
GEN_SYSCALL_WRAPPER(sys_getgid);
GEN_SYSCALL_WRAPPER(sys_geteuid);
GEN_SYSCALL_WRAPPER(sys_getegid);
GEN_SYSCALL_WRAPPER(sys_getpgid);
GEN_SYSCALL_WRAPPER(sys_fsync);
GEN_SYSCALL_WRAPPER(sys_wait4);
GEN_SYSCALL_WRAPPER(sys_mprotect);
GEN_SYSCALL_WRAPPER(sys_sigprocmask);
GEN_SYSCALL_WRAPPER(sys_timer_create);    // Linux: varies across archs?
GEN_SYSCALL_WRAPPER(sys_timer_settime);
GEN_SYSCALL_WRAPPER(sys_timer_gettime);
GEN_SYSCALL_WRAPPER(sys_timer_getoverrun);
GEN_SYSCALL_WRAPPER(sys_timer_delete);
GEN_SYSCALL_WRAPPER(sys_clock_settime);
GEN_SYSCALL_WRAPPER(sys_clock_gettime);
GEN_SYSCALL_WRAPPER(sys_clock_getres);
GEN_SYSCALL_WRAPPER(sys_clock_nanosleep);
GEN_SYSCALL_WRAPPER(sys_getcwd);
GEN_SYSCALL_WRAPPER(sys_symlink);
GEN_SYSCALL_WRAPPER(sys_getgroups);
GEN_SYSCALL_WRAPPER(sys_setgroups);             // SVr4, SVID, X/OPEN, 4.3BSD
GEN_SYSCALL_WRAPPER(sys_chown);
GEN_SYSCALL_WRAPPER(sys_setuid);
GEN_SYSCALL_WRAPPER(sys_gettimeofday);
GEN_SYSCALL_WRAPPER(sys_madvise);
GEN_SYSCALL_WRAPPER(sys_sigpending);

// These ones aren't POSIX, but are in some standard and look reasonably
// generic, and are the same for all architectures under Linux.
GEN_SYSCALL_WRAPPER(sys_nice);      // SVr4, SVID EXT, AT&T, X/OPEN, BSD 4.3
GEN_SYSCALL_WRAPPER(sys_sync);      // SVr4, SVID, X/OPEN, BSD 4.3
GEN_SYSCALL_WRAPPER(sys_brk);       // 4.3BSD
GEN_SYSCALL_WRAPPER(sys_acct);      // SVR4, non-POSIX
GEN_SYSCALL_WRAPPER(sys_chroot);    // SVr4, SVID, 4.4BSD, X/OPEN
GEN_SYSCALL_WRAPPER(sys_readlink);  // X/OPEN, 4.4BSD
GEN_SYSCALL_WRAPPER(sys_fchdir);    // SVr4, SVID, POSIX, X/OPEN, 4.4BSD
GEN_SYSCALL_WRAPPER(sys_getdents);  // SVr4,SVID
GEN_SYSCALL_WRAPPER(sys_select);    // 4.4BSD
GEN_SYSCALL_WRAPPER(sys_flock);     // 4.4BSD
GEN_SYSCALL_WRAPPER(sys_poll);      // XPG4-UNIX
GEN_SYSCALL_WRAPPER(sys_getrusage); // SVr4, 4.3BSD
GEN_SYSCALL_WRAPPER(sys_stime);	    // SVr4, SVID, X/OPEN
GEN_SYSCALL_WRAPPER(sys_settimeofday); // SVr4, 4.3BSD (non-POSIX)
GEN_SYSCALL_WRAPPER(sys_getpriority);  // SVr4, 4.4BSD
GEN_SYSCALL_WRAPPER(sys_setpriority);  // SVr4, 4.4BSD
GEN_SYSCALL_WRAPPER(sys_setitimer);    // SVr4, 4.4BSD
GEN_SYSCALL_WRAPPER(sys_getitimer);    // SVr4, 4.4BSD
GEN_SYSCALL_WRAPPER(sys_setreuid);     // 4.3BSD
GEN_SYSCALL_WRAPPER(sys_setregid);     // 4.3BSD
GEN_SYSCALL_WRAPPER(sys_fchown);       // SVr4,4.3BSD
GEN_SYSCALL_WRAPPER(sys_setgid);       // SVr4,SVID
GEN_SYSCALL_WRAPPER(sys_utimes);       // 4.3BSD

// These ones may be Linux specific... not sure.  They use 16-bit gid_t and
// uid_t types.  The similarly named (minus the "16" suffix) ones below use
// 32-bit versions of these types.
GEN_SYSCALL_WRAPPER(sys_setuid16);              // ## P
GEN_SYSCALL_WRAPPER(sys_getuid16);              // ## P
GEN_SYSCALL_WRAPPER(sys_setgid16);              // ## SVr4,SVID
GEN_SYSCALL_WRAPPER(sys_getgid16);              // ## P
GEN_SYSCALL_WRAPPER(sys_geteuid16);             // ## P
GEN_SYSCALL_WRAPPER(sys_getegid16);             // ## P
GEN_SYSCALL_WRAPPER(sys_setreuid16);            // ## BSD4.3
GEN_SYSCALL_WRAPPER(sys_setregid16);            // ## BSD4.3
GEN_SYSCALL_WRAPPER(sys_getgroups16);           // ## P
GEN_SYSCALL_WRAPPER(sys_setgroups16);           // ## SVr4, SVID, X/OPEN, 4.3BSD
GEN_SYSCALL_WRAPPER(sys_fchown16);              // ## SVr4,BSD4.3
GEN_SYSCALL_WRAPPER(sys_chown16);               // ## P

// Linux's funny many-in-one socketcall is certainly not generic, but I
// didn't want to move it until necessary because it's big and has a lot of
// associated junk.
GEN_SYSCALL_WRAPPER(sys_socketcall);

// Some archs on Linux do not match the generic wrapper for sys_pipe().
GEN_SYSCALL_WRAPPER(sys_pipe);

// May not be generic for every architecture under Linux.
GEN_SYSCALL_WRAPPER(sys_sigaction);             // (x86) P

// Funny names, not sure...
GEN_SYSCALL_WRAPPER(sys_newstat);               // * P
GEN_SYSCALL_WRAPPER(sys_newlstat);              // *
GEN_SYSCALL_WRAPPER(sys_newfstat);              // * P (SVr4,BSD4.3)

// For the remainder, not really sure yet
GEN_SYSCALL_WRAPPER(old_mmap);                  // x86, weird arg passing
GEN_SYSCALL_WRAPPER(sys_ptrace);                // (x86?) (almost-P)
GEN_SYSCALL_WRAPPER(sys_sigsuspend);            // POSIX, but L (proto varies across archs)
GEN_SYSCALL_WRAPPER(sys_setrlimit);             // SVr4, 4.3BSD
GEN_SYSCALL_WRAPPER(sys_ioctl);                 // x86? (various)
GEN_SYSCALL_WRAPPER(sys_old_getrlimit);         // SVr4, 4.3BSD L?
GEN_SYSCALL_WRAPPER(sys_statfs);                // * L?
GEN_SYSCALL_WRAPPER(sys_fstatfs);               // * L?
GEN_SYSCALL_WRAPPER(sys_iopl);                  // (x86/amd64) L
GEN_SYSCALL_WRAPPER(sys_ipc);                   // (x86) L
GEN_SYSCALL_WRAPPER(sys_newuname);              // * P
GEN_SYSCALL_WRAPPER(sys_init_module);           // * L?
GEN_SYSCALL_WRAPPER(sys_quotactl);              // * (?)
GEN_SYSCALL_WRAPPER(sys_rt_sigaction);          // (x86) ()
GEN_SYSCALL_WRAPPER(sys_rt_sigprocmask);        // * ?
GEN_SYSCALL_WRAPPER(sys_rt_sigpending);         // * ?
GEN_SYSCALL_WRAPPER(sys_rt_sigtimedwait);       // * ?
GEN_SYSCALL_WRAPPER(sys_rt_sigqueueinfo);       // * ?
GEN_SYSCALL_WRAPPER(sys_rt_sigsuspend);         // () ()
GEN_SYSCALL_WRAPPER(sys_pread64);               // * (Unix98?)
GEN_SYSCALL_WRAPPER(sys_pwrite64);              // * (Unix98?)
GEN_SYSCALL_WRAPPER(sys_capget);                // * L?
GEN_SYSCALL_WRAPPER(sys_capset);                // * L?
GEN_SYSCALL_WRAPPER(sys_sigaltstack);           // (x86) (XPG4-UNIX)
GEN_SYSCALL_WRAPPER(sys_getpmsg);               // (?) (?)
GEN_SYSCALL_WRAPPER(sys_putpmsg);               // (?) (?)
GEN_SYSCALL_WRAPPER(sys_getrlimit);             // * (?)
GEN_SYSCALL_WRAPPER(sys_mmap2);                 // (x86?) P?
GEN_SYSCALL_WRAPPER(sys_truncate64);            // %% (P?)
GEN_SYSCALL_WRAPPER(sys_ftruncate64);           // %% (P?)
GEN_SYSCALL_WRAPPER(sys_stat64);                // %% (?)
GEN_SYSCALL_WRAPPER(sys_lstat64);               // %% (?)
GEN_SYSCALL_WRAPPER(sys_fstat64);               // %% (?)
GEN_SYSCALL_WRAPPER(sys_lchown);                // * (L?)
GEN_SYSCALL_WRAPPER(sys_mincore);               // * L?
GEN_SYSCALL_WRAPPER(sys_getdents64);            // * (SVr4,SVID?)
GEN_SYSCALL_WRAPPER(sys_fcntl64);               // * P?
GEN_SYSCALL_WRAPPER(sys_setxattr);              // * L?
GEN_SYSCALL_WRAPPER(sys_lsetxattr);             // * L?
GEN_SYSCALL_WRAPPER(sys_fsetxattr);             // * L?
GEN_SYSCALL_WRAPPER(sys_getxattr);              // * L?
GEN_SYSCALL_WRAPPER(sys_lgetxattr);             // * L?
GEN_SYSCALL_WRAPPER(sys_fgetxattr);             // * L?
GEN_SYSCALL_WRAPPER(sys_listxattr);             // * L?
GEN_SYSCALL_WRAPPER(sys_llistxattr);            // * L?
GEN_SYSCALL_WRAPPER(sys_flistxattr);            // * L?
GEN_SYSCALL_WRAPPER(sys_removexattr);           // * L?
GEN_SYSCALL_WRAPPER(sys_lremovexattr);          // * L?
GEN_SYSCALL_WRAPPER(sys_fremovexattr);          // * L?
GEN_SYSCALL_WRAPPER(sys_sched_setaffinity);     // * L?
GEN_SYSCALL_WRAPPER(sys_sched_getaffinity);     // * L?
GEN_SYSCALL_WRAPPER(sys_lookup_dcookie);        // (*/32/64) L
GEN_SYSCALL_WRAPPER(sys_set_tid_address);       // * ?
GEN_SYSCALL_WRAPPER(sys_statfs64);              // * (?)
GEN_SYSCALL_WRAPPER(sys_fstatfs64);             // * (?)
GEN_SYSCALL_WRAPPER(sys_mq_open);               // * P?
GEN_SYSCALL_WRAPPER(sys_mq_unlink);             // * P?
GEN_SYSCALL_WRAPPER(sys_mq_timedsend);          // * P?
GEN_SYSCALL_WRAPPER(sys_mq_timedreceive);       // * P?
GEN_SYSCALL_WRAPPER(sys_mq_notify);             // * P?
GEN_SYSCALL_WRAPPER(sys_mq_getsetattr);         // * P?

#undef GEN_SYSCALL_WRAPPER

// Macros used in syscall wrappers
/* PRRAn == "pre-register-read-argument"
   PRRSN == "pre-register-read-syscall"
*/

#define PRRSN \
      VG_(tdict).track_pre_reg_read(Vg_CoreSysCall, tid, "(syscallno)", \
                                    O_SYSCALL_NUM, sizeof(UWord));
#define PRRAn(n,s,t,a) \
      VG_(tdict).track_pre_reg_read(Vg_CoreSysCall, tid, s"("#a")", \
                                    O_SYSCALL_ARG##n, sizeof(t));
#define PRE_REG_READ0(tr, s) \
   if (VG_(tdict).track_pre_reg_read) { \
      PRRSN; \
   }
#define PRE_REG_READ1(tr, s, t1, a1) \
   if (VG_(tdict).track_pre_reg_read) { \
      PRRSN; \
      PRRAn(1,s,t1,a1); \
   }
#define PRE_REG_READ2(tr, s, t1, a1, t2, a2) \
   if (VG_(tdict).track_pre_reg_read) { \
      PRRSN; \
      PRRAn(1,s,t1,a1); PRRAn(2,s,t2,a2); \
   }
#define PRE_REG_READ3(tr, s, t1, a1, t2, a2, t3, a3) \
   if (VG_(tdict).track_pre_reg_read) { \
      PRRSN; \
      PRRAn(1,s,t1,a1); PRRAn(2,s,t2,a2); PRRAn(3,s,t3,a3); \
   }
#define PRE_REG_READ4(tr, s, t1, a1, t2, a2, t3, a3, t4, a4) \
   if (VG_(tdict).track_pre_reg_read) { \
      PRRSN; \
      PRRAn(1,s,t1,a1); PRRAn(2,s,t2,a2); PRRAn(3,s,t3,a3); \
      PRRAn(4,s,t4,a4); \
   }
#define PRE_REG_READ5(tr, s, t1, a1, t2, a2, t3, a3, t4, a4, t5, a5) \
   if (VG_(tdict).track_pre_reg_read) { \
      PRRSN; \
      PRRAn(1,s,t1,a1); PRRAn(2,s,t2,a2); PRRAn(3,s,t3,a3); \
      PRRAn(4,s,t4,a4); PRRAn(5,s,t5,a5); \
   }
#define PRE_REG_READ6(tr, s, t1, a1, t2, a2, t3, a3, t4, a4, t5, a5, t6, a6) \
   if (VG_(tdict).track_pre_reg_read) { \
      PRRSN; \
      PRRAn(1,s,t1,a1); PRRAn(2,s,t2,a2); PRRAn(3,s,t3,a3); \
      PRRAn(4,s,t4,a4); PRRAn(5,s,t5,a5); PRRAn(6,s,t6,a6); \
   }

#define PRE_MEM_READ(zzname, zzaddr, zzlen) \
   VG_TRACK( pre_mem_read, Vg_CoreSysCall, tid, zzname, zzaddr, zzlen)

#define PRE_MEM_RASCIIZ(zzname, zzaddr) \
   VG_TRACK( pre_mem_read_asciiz, Vg_CoreSysCall, tid, zzname, zzaddr)

#define PRE_MEM_WRITE(zzname, zzaddr, zzlen) \
   VG_TRACK( pre_mem_write, Vg_CoreSysCall, tid, zzname, zzaddr, zzlen)

#define POST_MEM_WRITE(zzaddr, zzlen) \
   VG_TRACK( post_mem_write, Vg_CoreSysCall, tid, zzaddr, zzlen)


//////////////////////////////////////////////////////////

#define TId ThreadId
#define UW  UWord

extern void  VG_(generic_PRE_sys_socketpair)   ( TId, UW, UW, UW, UW );
extern UWord VG_(generic_POST_sys_socketpair)  ( TId, UW, UW, UW, UW, UW );
extern UWord VG_(generic_POST_sys_socket)      ( TId, UW );
extern void  VG_(generic_PRE_sys_bind)         ( TId, UW, UW, UW );
extern void  VG_(generic_PRE_sys_accept)       ( TId, UW, UW, UW );
extern UWord VG_(generic_POST_sys_accept)      ( TId, UW, UW, UW, UW );
extern void  VG_(generic_PRE_sys_sendto)       ( TId, UW, UW, UW, UW, UW, UW );
extern void  VG_(generic_PRE_sys_send)         ( TId, UW, UW, UW );
extern void  VG_(generic_PRE_sys_recvfrom)     ( TId, UW, UW, UW, UW, UW, UW );
extern void  VG_(generic_POST_sys_recvfrom)    ( TId, UW, UW, UW, UW, UW, UW, UW );
extern void  VG_(generic_PRE_sys_recv)         ( TId, UW, UW, UW );
extern void  VG_(generic_POST_sys_recv)        ( TId, UW, UW, UW, UW );
extern void  VG_(generic_PRE_sys_connect)      ( TId, UW, UW, UW );
extern void  VG_(generic_PRE_sys_setsockopt)   ( TId, UW, UW, UW, UW, UW );
extern void  VG_(generic_PRE_sys_getsockopt)   ( TId, UW, UW, UW, UW, UW );
extern void  VG_(generic_POST_sys_getsockopt)  ( TId, UW, UW, UW, UW, UW, UW );
extern void  VG_(generic_PRE_sys_getsockname)  ( TId, UW, UW, UW );
extern void  VG_(generic_POST_sys_getsockname) ( TId, UW, UW, UW, UW );
extern void  VG_(generic_PRE_sys_getpeername)  ( TId, UW, UW, UW );
extern void  VG_(generic_POST_sys_getpeername) ( TId, UW, UW, UW, UW );
extern void  VG_(generic_PRE_sys_sendmsg)      ( TId, UW, UW );
extern void  VG_(generic_PRE_sys_recvmsg)      ( TId, UW, UW );
extern void  VG_(generic_POST_sys_recvmsg)     ( TId, UW, UW, UW );

extern void  VG_(generic_PRE_sys_semop)        ( TId, UW, UW, UW );
extern void  VG_(generic_PRE_sys_semtimedop)   ( TId, UW, UW, UW, UW );
extern void  VG_(generic_PRE_sys_semctl)       ( TId, UW, UW, UW, UW );
extern void  VG_(generic_POST_sys_semctl)      ( TId, UW, UW, UW, UW, UW );
extern void  VG_(generic_PRE_sys_msgsnd)       ( TId, UW, UW, UW, UW );
extern void  VG_(generic_PRE_sys_msgrcv)       ( TId, UW, UW, UW, UW, UW );
extern void  VG_(generic_POST_sys_msgrcv)      ( TId, UW, UW, UW, UW, UW, UW );
extern void  VG_(generic_PRE_sys_msgctl)       ( TId, UW, UW, UW );
extern void  VG_(generic_POST_sys_msgctl)      ( TId, UW, UW, UW, UW );
extern UWord VG_(generic_PRE_sys_shmat)        ( TId, UW, UW, UW );
extern void  VG_(generic_POST_sys_shmat)       ( TId, UW, UW, UW, UW );
extern Bool  VG_(generic_PRE_sys_shmdt)        ( TId, UW );
extern void  VG_(generic_POST_sys_shmdt)       ( TId, UW, UW );
extern void  VG_(generic_PRE_sys_shmctl)       ( TId, UW, UW, UW );
extern void  VG_(generic_POST_sys_shmctl)      ( TId, UW, UW, UW, UW );

#undef TId
#undef UW

#endif   // __PRIV_SYSCALLS_H

/*--------------------------------------------------------------------*/
/*--- end                                                          ---*/
/*--------------------------------------------------------------------*/

