
/*--------------------------------------------------------------------*/
/*--- Private syscalls header.              priv_syswrap-generic.h ---*/
/*--------------------------------------------------------------------*/

/*
   This file is part of Valgrind, a dynamic binary instrumentation
   framework.

   Copyright (C) 2000-2017 Julian Seward
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
   along with this program; if not, see <http://www.gnu.org/licenses/>.

   The GNU General Public License is contained in the file COPYING.
*/

#ifndef __PRIV_SYSWRAP_GENERIC_H
#define __PRIV_SYSWRAP_GENERIC_H

#include "pub_core_basics.h"      // ThreadId
#include "pub_core_vki.h"         // vki_msghdr
#include "priv_types_n_macros.h"  // DECL_TEMPLATE


/* Guess the client stack from the segment in which sp is mapped.
   Register the guessed stack using VG_(register_stack).
   Setup tst client_stack_highest_byte and client_stack_szB.
   If sp is not in a mapped segment, does nothing. */
extern void ML_(guess_and_register_stack) (Addr sp, ThreadState* tst);

// Return true if address range entirely contained within client
// address space.
extern
Bool ML_(valid_client_addr)(Addr start, SizeT size, ThreadId tid,
                            const HChar *syscallname);

/* Handy small function to help stop wrappers from segfaulting when
   presented with bogus client addresses.  Is not used for generating
   user-visible errors. */
extern Bool ML_(safe_to_deref) ( const void *start, SizeT size );

// Returns True if the signal is OK for the client to use.
extern Bool ML_(client_signal_OK)(Int sigNo);

// Return true if we're allowed to use or create this fd.
extern
Bool ML_(fd_allowed)(Int fd, const HChar *syscallname, ThreadId tid,
                     Bool isNewFD);

extern void ML_(record_fd_close)               (ThreadId tid, Int fd);
extern Int  ML_(get_fd_count)                  (void);
extern void ML_(record_fd_close_range)         (ThreadId tid, Int fd);
extern void ML_(record_fd_open_named)          (ThreadId tid, Int fd);
extern void ML_(record_fd_open_nameless)       (ThreadId tid, Int fd);
extern void ML_(record_fd_open_with_given_name)(ThreadId tid, Int fd,
                                                const HChar *pathname);

// Return true if a given file descriptor is already recorded.
extern Bool ML_(fd_recorded)(Int fd);
// Returns a pathname representing a recorded fd.
// Returned string must not be modified nor free'd.
extern const HChar *ML_(find_fd_recorded_by_fd)(Int fd);

extern int ML_(get_next_new_fd)(Int fd);

// Used when killing threads -- we must not kill a thread if it's the thread
// that would do Valgrind's final cleanup and output.
extern
Bool ML_(do_sigkill)(Int pid, Int tgid);

/* When a client mmap or munmap has been successfully done, both the core 
   and the tool need to be notified of the new mapping.  Hence this fn. */
extern void 
ML_(notify_core_and_tool_of_mmap) ( Addr a, SizeT len, UInt prot, 
                                    UInt mm_flags, Int fd, Off64T offset );
extern void 
ML_(notify_core_and_tool_of_munmap) ( Addr a, SizeT len );
extern void 
ML_(notify_core_and_tool_of_mprotect) ( Addr a, SizeT len, Int prot );

extern void
ML_(pre_mem_read_sockaddr) ( ThreadId tid, const HChar *description,
                             struct vki_sockaddr *sa, UInt salen );
extern void
ML_(buf_and_len_pre_check) ( ThreadId tid, Addr buf_p, Addr buflen_p,
                             const HChar* buf_s, const HChar* buflen_s );
extern void
ML_(buf_and_len_post_check) ( ThreadId tid, SysRes res,
                              Addr buf_p, Addr buflen_p, const HChar* s );

/* PRE and POST for unknown ioctls based on ioctl request encoding */
extern 
void ML_(PRE_unknown_ioctl)(ThreadId tid, UWord request, UWord arg);
extern 
void ML_(POST_unknown_ioctl)(ThreadId tid, UInt res, UWord request, UWord arg);

extern
void ML_(pre_argv_envp)(Addr a, ThreadId tid, const HChar *s1, const HChar *s2);

extern Bool
ML_(handle_auxv_open)(SyscallStatus *status, const HChar *filename,
                      int flags);

extern Bool
ML_(handle_self_exe_open)(SyscallStatus *status, const HChar *filename,
                          int flags);

/* Helper function for generic mprotect and linux pkey_mprotect. */
extern void handle_sys_mprotect (ThreadId tid, SyscallStatus *status,
                                 Addr *addr, SizeT *len, Int *prot);
/* Helper functions for preadv/preadv2. */
extern
void handle_pre_sys_preadv(ThreadId tid, SyscallStatus* status,
                           Int fd, Addr vector, Int count,
                           const char *str);
extern
void handle_post_sys_preadv(ThreadId tid, SyscallStatus* status, Addr vector, Int count);

/* Helper function for pwritev/pwritev2. */
extern
void handle_sys_pwritev(ThreadId tid, SyscallStatus* status,
                        Int fd, Addr vector, Int count,
                        const char *str);

typedef enum {
   EXECVE,
   EXECVEAT,
   FEXECVE
} ExecveType;

extern
void handle_pre_sys_execve(ThreadId tid, SyscallStatus *status, Addr pathname,
                           Addr arg_2, Addr arg_3, ExecveType execveType,
                           Bool check_pathptr);

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
DECL_TEMPLATE(generic, sys_getpid);
DECL_TEMPLATE(generic, sys_alarm);
DECL_TEMPLATE(generic, sys_pause);
DECL_TEMPLATE(generic, sys_access);
DECL_TEMPLATE(generic, sys_kill);
DECL_TEMPLATE(generic, sys_rename);
DECL_TEMPLATE(generic, sys_mkdir);
DECL_TEMPLATE(generic, sys_rmdir);
DECL_TEMPLATE(generic, sys_dup);
DECL_TEMPLATE(generic, sys_times);
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
DECL_TEMPLATE(generic, sys_getcwd);
DECL_TEMPLATE(generic, sys_symlink);
DECL_TEMPLATE(generic, sys_getgroups);
DECL_TEMPLATE(generic, sys_setgroups);             // SVr4, SVID, X/OPEN, 4.3BSD
DECL_TEMPLATE(generic, sys_chown);
DECL_TEMPLATE(generic, sys_setuid);
DECL_TEMPLATE(generic, sys_gettimeofday);
DECL_TEMPLATE(generic, sys_madvise);
DECL_TEMPLATE(generic, sys_sethostname);

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

// May not be generic for every architecture under Linux.
DECL_TEMPLATE(generic, sys_sigaction);             // (x86) P

// Funny names, not sure...
DECL_TEMPLATE(generic, sys_newstat);               // * P
DECL_TEMPLATE(generic, sys_newlstat);              // *
DECL_TEMPLATE(generic, sys_newfstat);              // * P (SVr4,BSD4.3)

// For the remainder, not really sure yet
DECL_TEMPLATE(generic, sys_ptrace);                // (x86?) (almost-P)
DECL_TEMPLATE(generic, sys_setrlimit);             // SVr4, 4.3BSD
DECL_TEMPLATE(generic, sys_old_getrlimit);         // SVr4, 4.3BSD L?
DECL_TEMPLATE(generic, sys_statfs);                // * L?
DECL_TEMPLATE(generic, sys_fstatfs);               // * L?
DECL_TEMPLATE(generic, sys_iopl);                  // (x86/amd64) L
DECL_TEMPLATE(generic, sys_newuname);              // * P
DECL_TEMPLATE(generic, sys_pread64);               // * (Unix98?)
DECL_TEMPLATE(generic, sys_pwrite64);              // * (Unix98?)
DECL_TEMPLATE(generic, sys_sigaltstack);           // (x86) (XPG4-UNIX)
DECL_TEMPLATE(generic, sys_getpmsg);               // (?) (?)
DECL_TEMPLATE(generic, sys_putpmsg);               // (?) (?)
DECL_TEMPLATE(generic, sys_getrlimit);             // * (?)
DECL_TEMPLATE(generic, sys_truncate64);            // %% (P?)
DECL_TEMPLATE(generic, sys_ftruncate64);           // %% (P?)
DECL_TEMPLATE(generic, sys_lchown);                // * (L?)
DECL_TEMPLATE(generic, sys_mincore);               // * L?
DECL_TEMPLATE(generic, sys_getdents64);            // * (SVr4,SVID?)
DECL_TEMPLATE(generic, sys_statfs64);              // * (?)
DECL_TEMPLATE(generic, sys_fstatfs64);             // * (?)
DECL_TEMPLATE(generic, sys_mlock2);                // * L


/* ---------------------------------------------------------------------
   Wrappers for sockets and ipc-ery.  These are split into standalone
   procedures because some platforms hides them inside multiplexors
   (sys_socketcall and sys_ipc).
   ------------------------------------------------------------------ */

#define TId ThreadId
#define UW  UWord
#define SR  SysRes

extern void   ML_(generic_PRE_sys_socketpair)   ( TId, UW, UW, UW, UW );
extern SysRes ML_(generic_POST_sys_socketpair)  ( TId, SR, UW, UW, UW, UW );
extern SysRes ML_(generic_POST_sys_socket)      ( TId, SR );
extern void   ML_(generic_PRE_sys_bind)         ( TId, UW, UW, UW );
extern void   ML_(generic_PRE_sys_accept)       ( TId, UW, UW, UW );
extern SysRes ML_(generic_POST_sys_accept)      ( TId, SR, UW, UW, UW );
extern void   ML_(generic_PRE_sys_sendto)       ( TId, UW, UW, UW, UW, UW, UW );
extern void   ML_(generic_PRE_sys_send)         ( TId, UW, UW, UW );
extern void   ML_(generic_PRE_sys_recvfrom)     ( TId, UW, UW, UW, UW, UW, UW );
extern void   ML_(generic_POST_sys_recvfrom)    ( TId, SR, UW, UW, UW, UW, UW, UW );
extern void   ML_(generic_PRE_sys_recv)         ( TId, UW, UW, UW );
extern void   ML_(generic_POST_sys_recv)        ( TId, UW, UW, UW, UW );
extern void   ML_(generic_PRE_sys_connect)      ( TId, UW, UW, UW );
extern void   ML_(generic_PRE_sys_setsockopt)   ( TId, UW, UW, UW, UW, UW );
extern void   ML_(generic_PRE_sys_getsockname)  ( TId, UW, UW, UW );
extern void   ML_(generic_POST_sys_getsockname) ( TId, SR, UW, UW, UW );
extern void   ML_(generic_PRE_sys_getpeername)  ( TId, UW, UW, UW );
extern void   ML_(generic_POST_sys_getpeername) ( TId, SR, UW, UW, UW );
extern void   ML_(generic_PRE_sys_sendmsg)      ( TId, const HChar *,
                                                  struct vki_msghdr * );
extern void   ML_(generic_PRE_sys_recvmsg)      ( TId, const HChar *,
                                                  struct vki_msghdr * );
extern void   ML_(generic_POST_sys_recvmsg)     ( TId, const HChar *,
                                                  struct vki_msghdr *, UInt );

extern void   ML_(generic_PRE_sys_semop)        ( TId, UW, UW, UW );
extern void   ML_(generic_PRE_sys_semtimedop)   ( TId, UW, UW, UW, UW );
extern void   ML_(generic_PRE_sys_semctl)       ( TId, UW, UW, UW, UW );
extern void   ML_(generic_POST_sys_semctl)      ( TId, UW, UW, UW, UW, UW );
extern UWord  ML_(generic_PRE_sys_shmat)        ( TId, UW, UW, UW );
extern void   ML_(generic_POST_sys_shmat)       ( TId, UW, UW, UW, UW );
extern Bool   ML_(generic_PRE_sys_shmdt)        ( TId, UW );
extern void   ML_(generic_POST_sys_shmdt)       ( TId, UW, UW );
extern void   ML_(generic_PRE_sys_shmctl)       ( TId, UW, UW, UW );
extern void   ML_(generic_POST_sys_shmctl)      ( TId, UW, UW, UW, UW );

extern SysRes ML_(generic_PRE_sys_mmap)         ( TId, UW, UW, UW, UW, UW, Off64T );

#define PRE_timeval_READ(zzname, zzarg)                         \
   do {                                                         \
      struct vki_timeval *zztv = (struct vki_timeval *)(zzarg); \
      PRE_FIELD_READ(zzname, zztv->tv_sec);                     \
      PRE_FIELD_READ(zzname, zztv->tv_usec);                    \
   } while (0)
#define PRE_timeval_WRITE(zzname, zzarg)                        \
   do {                                                         \
      struct vki_timeval *zztv = (struct vki_timeval *)(zzarg); \
      PRE_FIELD_WRITE(zzname, zztv->tv_sec);                    \
      PRE_FIELD_WRITE(zzname, zztv->tv_usec);                   \
   } while (0)
#define POST_timeval_WRITE(zzarg)                               \
   do {                                                         \
      struct vki_timeval *zztv = (struct vki_timeval *)(zzarg); \
      POST_FIELD_WRITE(zztv->tv_sec);                           \
      POST_FIELD_WRITE(zztv->tv_usec);                          \
   } while (0)


#undef TId
#undef UW
#undef SR

/* Helper macro for POST handlers that return a new file in RES.
   If possible sets RES (through SET_STATUS_Success) to a new
   (not yet seem before) file descriptor.  */
#define POST_newFd_RES                       \
  do {                                       \
    if (VG_(clo_modify_fds) == 1) {           \
      int newFd = ML_(get_next_new_fd)(RES); \
      if (newFd != RES)                      \
        SET_STATUS_Success(newFd);           \
    }                                        \
  } while (0)

/////////////////////////////////////////////////////////////////


#endif   // __PRIV_SYSWRAP_GENERIC_H

/*--------------------------------------------------------------------*/
/*--- end                                                          ---*/
/*--------------------------------------------------------------------*/
