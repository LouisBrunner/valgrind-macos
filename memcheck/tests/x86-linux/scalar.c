#define _GNU_SOURCE

#include "../../memcheck.h"
#include "scalar.h"
#include <unistd.h>
#include <sched.h>
#include <signal.h>
#include <sys/mman.h> // MREMAP_FIXED
#include <sys/prctl.h>

// Here we are trying to trigger every syscall error (scalar errors and
// memory errors) for every syscall.  We do this by passing a lot of bogus
// arguments, mostly 0 and 1 (often it's 1 because NULL ptr args often aren't
// checked for memory errors, or in order to have a non-zero length used
// with some buffer).  So most of the syscalls don't actually succeed and do
// anything.
//
// Occasionally we have to be careful not to cause Valgrind to seg fault in
// its pre-syscall wrappers;  it does so because it can't know in general
// when memory is unaddressable, and so tries to dereference it when doing
// PRE_MEM_READ/PRE_MEM_WRITE calls.  (Note that Memcheck will
// always issue an error message immediately before these seg faults occur).
//
// The output has numbers like "3s 2m" for each syscall.  "s" is short for
// "scalar", ie. the argument itself is undefined.  "m" is short for "memory",
// ie. the argument points to memory which is unaddressable.

int main(void)
{
   // uninitialised, but we know px[0] is 0x0
   long* px  = malloc(sizeof(long));
   long  x0  = px[0];
   long  res;

   // All __NR_xxx numbers are taken from x86

   // __NR_restart_syscall 0  // XXX: not yet handled, perhaps should be...
   GO(__NR_restart_syscall, "n/a");
 //SY(__NR_restart_syscall); // (Not yet handled by Valgrind) FAIL;

   // __NR_exit 1 
   GO(__NR_exit, "below");
   // (see below)

   // __NR_fork 2
   GO(__NR_fork, "other");
   // (sse scalar_fork.c)

   // __NR_read 3
   // Nb: here we are also getting an error from the syscall arg itself.
   GO(__NR_read, "1+3s 1m");
   SY(__NR_read+x0, x0, x0, x0+1); FAILx(EFAULT);

   // __NR_write 4
   GO(__NR_write, "3s 1m");
   SY(__NR_write, x0, x0, x0+1); FAIL;

   // __NR_open 5
   GO(__NR_open, "(2-args) 2s 1m");
   SY(__NR_open, x0, x0); FAIL;

   // Only 1s 0m errors -- the other 2s 1m have been checked in the previous
   // open test, and if we test them they may be commoned up but they also
   // may not.
   GO(__NR_open, "(3-args) 1s 0m");    
   SY(__NR_open, "scalar.c", O_CREAT|O_EXCL, x0); FAIL;

   // __NR_close 6
   GO(__NR_close, "1s 0m");
   SY(__NR_close, x0-1); FAIL;

   // __NR_waitpid 7
   GO(__NR_waitpid, "3s 1m");
   SY(__NR_waitpid, x0, x0+1, x0); FAIL;

   // __NR_creat 8
   GO(__NR_creat, "2s 1m");
   SY(__NR_creat, x0, x0); FAIL;

   // __NR_link 9
   GO(__NR_link, "2s 2m");
   SY(__NR_link, x0, x0); FAIL;

   // __NR_unlink 10
   GO(__NR_unlink, "1s 1m");
   SY(__NR_unlink, x0); FAIL;

   // __NR_execve 11
   GO(__NR_execve, "3s 1m");
   SY(__NR_execve, x0 + 1, x0 + 1, x0); FAIL;

   GO(__NR_execve, "3s 1m");
   SY(__NR_execve, x0 + 1, x0, x0 + 1); FAIL;

   char *argv_envp[] = {(char *) (x0 + 1), NULL};
   GO(__NR_execve, "4s 2m");
   SY(__NR_execve, x0 + 1, x0 + argv_envp, x0); FAIL;
   char *argv_ok[] = {"frob", NULL};
   GO(__NR_execve, "4s 2m");
   SY(__NR_execve, x0 + 1, x0 + argv_ok, x0 + argv_envp); FAIL;

   // __NR_chdir 12
   GO(__NR_chdir, "1s 1m");
   SY(__NR_chdir, x0); FAIL;

   // __NR_time 13
   GO(__NR_time, "1s 1m");
   SY(__NR_time, x0+1); FAIL;

   // __NR_mknod 14
   GO(__NR_mknod, "3s 1m");
   SY(__NR_mknod, x0, x0, x0); FAIL;

   // __NR_chmod 15
   GO(__NR_chmod, "2s 1m");
   SY(__NR_chmod, x0, x0); FAIL;

   // __NR_lchown 16
   GO(__NR_lchown, "n/a");
 //SY(__NR_lchown); // (Not yet handled by Valgrind) FAIL;

   // __NR_break 17
   GO(__NR_break, "ni");
   SY(__NR_break); FAIL;

   // __NR_oldstat 18
   GO(__NR_oldstat, "n/a");
   // (obsolete, not handled by Valgrind)

   // __NR_lseek 19
   GO(__NR_lseek, "3s 0m");
   SY(__NR_lseek, x0-1, x0, x0); FAILx(EBADF);

   // __NR_getpid 20
   GO(__NR_getpid, "0s 0m");
   SY(__NR_getpid); SUCC;

   // __NR_mount 21
   GO(__NR_mount, "5s 3m");
   SY(__NR_mount, x0, x0, x0, x0, x0); FAIL;
   
   // __NR_umount 22
   GO(__NR_umount, "1s 1m");
   SY(__NR_umount, x0); FAIL;

   // __NR_setuid 23
   GO(__NR_setuid, "1s 0m");
   SY(__NR_setuid, x0-1); FAIL;

   // __NR_getuid 24
   GO(__NR_getuid, "0s 0m");
   SY(__NR_getuid); SUCC;

   // __NR_stime 25
   GO(__NR_stime, "n/a");
 //SY(__NR_stime); // (Not yet handled by Valgrind) FAIL;

   // __NR_ptrace 26
   // XXX: memory pointed to be arg3 goes unchecked... otherwise would be 2m
   GO(__NR_ptrace, "4s 1m");
   SY(__NR_ptrace, x0+PTRACE_GETREGS, x0, x0, x0); FAIL;

   // __NR_alarm 27
   GO(__NR_alarm, "1s 0m");
   SY(__NR_alarm, x0); SUCC;

   // __NR_oldfstat 28
   GO(__NR_oldfstat, "n/a");
   // (obsolete, not handled by Valgrind)

   // __NR_pause 29
   GO(__NR_pause, "ignore");
   // (hard to test, and no args so not much to be gained -- don't bother)

   // __NR_utime 30
   GO(__NR_utime, "2s 2m");
   SY(__NR_utime, x0, x0+1); FAIL;

   // __NR_stty 31
   GO(__NR_stty, "ni");
   SY(__NR_stty); FAIL;

   // __NR_gtty 32
   GO(__NR_gtty, "ni");
   SY(__NR_gtty); FAIL;

   // __NR_access 33
   GO(__NR_access, "2s 1m");
   SY(__NR_access, x0, x0); FAIL;

   // __NR_nice 34
   GO(__NR_nice, "1s 0m");
   SY(__NR_nice, x0); SUCC;

   // __NR_ftime 35
   GO(__NR_ftime, "ni");
   SY(__NR_ftime); FAIL;

   // __NR_sync 36
   GO(__NR_sync, "0s 0m");
   SY(__NR_sync); SUCC;

   // __NR_kill 37
   GO(__NR_kill, "2s 0m");
   SY(__NR_kill, x0, x0); SUCC;

   // __NR_rename 38
   GO(__NR_rename, "2s 2m");
   SY(__NR_rename, x0, x0); FAIL;

   // __NR_mkdir 39
   GO(__NR_mkdir, "2s 1m");
   SY(__NR_mkdir, x0, x0); FAIL;

   // __NR_rmdir 40
   GO(__NR_rmdir, "1s 1m");
   SY(__NR_rmdir, x0); FAIL;

   // __NR_dup 41
   GO(__NR_dup, "1s 0m");
   SY(__NR_dup, x0-1); FAIL;

   // __NR_pipe 42
   GO(__NR_pipe, "1s 1m");
   SY(__NR_pipe, x0); FAIL;

   // __NR_times 43
   GO(__NR_times, "1s 1m");
   SY(__NR_times, x0+1); FAIL;

   // __NR_prof 44
   GO(__NR_prof, "ni");
   SY(__NR_prof); FAIL;

   // __NR_brk 45
   GO(__NR_brk, "1s 0m");
   SY(__NR_brk, x0); SUCC;

   // __NR_setgid 46
   GO(__NR_setgid, "1s 0m");
   SY(__NR_setgid, x0-1); FAIL;

   // __NR_getgid 47
   GO(__NR_getgid, "0s 0m");
   SY(__NR_getgid); SUCC;

   // __NR_signal 48
   GO(__NR_signal, "n/a");
 //SY(__NR_signal); // (Not yet handled by Valgrind) FAIL;

   // __NR_geteuid 49
   GO(__NR_geteuid, "0s 0m");
   SY(__NR_geteuid); SUCC;

   // __NR_getegid 50
   GO(__NR_getegid, "0s 0m");
   SY(__NR_getegid); SUCC;

   // __NR_acct 51
   GO(__NR_acct, "1s 1m");
   SY(__NR_acct, x0-1); FAIL;

   // __NR_umount2 52
   GO(__NR_umount2, "2s 1m");
   SY(__NR_umount2, x0, x0); FAIL;

   // __NR_lock 53
   GO(__NR_lock, "ni");
   SY(__NR_lock); FAIL;

   // __NR_ioctl 54
   #include <sys/ioctl.h>
   GO(__NR_ioctl, "3s 1m");
   SY(__NR_ioctl, x0, x0+TCSETS, x0); FAIL;

   // __NR_fcntl 55
   // As with sys_open(), the 'fd' error is suppressed for the later ones.
   // For F_GETFD the 3rd arg is ignored
   GO(__NR_fcntl, "(GETFD) 2s 0m");
   SY(__NR_fcntl, x0-1, x0+F_GETFD, x0); FAILx(EBADF);

   // For F_DUPFD the 3rd arg is 'arg'.  We don't check the 1st two args
   // because any errors may or may not be commoned up with the ones from
   // the previous fcntl call.
   GO(__NR_fcntl, "(DUPFD) 1s 0m");
   SY(__NR_fcntl, -1, F_DUPFD, x0); FAILx(EBADF);

   // For F_GETLK the 3rd arg is 'lock'.  On x86, this fails w/EBADF.  But
   // on amd64 in 32-bit mode it fails w/EFAULT.  We don't check the 1st two
   // args for the reason given above.
   GO(__NR_fcntl, "(GETLK) 1s 5m");
   SY(__NR_fcntl, -1, F_GETLK, x0); FAIL; //FAILx(EBADF);

   // __NR_mpx 56
   GO(__NR_mpx, "ni");
   SY(__NR_mpx); FAIL;

   // __NR_setpgid 57
   GO(__NR_setpgid, "2s 0m");
   SY(__NR_setpgid, x0, x0-1); FAIL;

   // __NR_ulimit 58
   GO(__NR_ulimit, "ni");
   SY(__NR_ulimit); FAIL;

   // __NR_oldolduname 59
   GO(__NR_oldolduname, "n/a");
   // (obsolete, not handled by Valgrind)

   // __NR_umask 60
   GO(__NR_umask, "1s 0m");
   SY(__NR_umask, x0+022); SUCC;

   // __NR_chroot 61
   GO(__NR_chroot, "1s 1m");
   SY(__NR_chroot, x0); FAIL;

   // __NR_ustat 62
   GO(__NR_ustat, "n/a");
   // (deprecated, not handled by Valgrind)

   // __NR_dup2 63
   GO(__NR_dup2, "2s 0m");
   SY(__NR_dup2, x0-1, x0); FAIL;

   // __NR_getppid 64
   GO(__NR_getppid, "0s 0m");
   SY(__NR_getppid); SUCC;

   // __NR_getpgrp 65
   GO(__NR_getpgrp, "0s 0m");
   SY(__NR_getpgrp); SUCC;

   // __NR_setsid 66
   GO(__NR_setsid, "0s 0m");
   SY(__NR_setsid); SUCC_OR_FAIL;

   // __NR_sigaction 67
   GO(__NR_sigaction, "3s 4m");
   SY(__NR_sigaction, x0, x0+&px[1], x0+&px[1]); FAIL;

   // __NR_sgetmask 68 sys_sgetmask()
   GO(__NR_sgetmask, "n/a");
 //SY(__NR_sgetmask); // (Not yet handled by Valgrind) FAIL;

   // __NR_ssetmask 69
   GO(__NR_ssetmask, "n/a");
 //SY(__NR_ssetmask); // (Not yet handled by Valgrind) FAIL;

   // __NR_setreuid 70
   GO(__NR_setreuid, "2s 0m");
   SY(__NR_setreuid, x0-1, x0-1); SUCC;

   // __NR_setregid 71
   GO(__NR_setregid, "2s 0m");
   SY(__NR_setregid, x0-1, x0-1); SUCC;

   // __NR_sigsuspend 72
   // XXX: how do you use this function?
   GO(__NR_sigsuspend, "ignore");
   // (I don't know how to test this...)

   // __NR_sigpending 73
   GO(__NR_sigpending, "1s 1m");
   SY(__NR_sigpending, x0); FAIL;

   // __NR_sethostname 74
   GO(__NR_sethostname, "n/a");
 //SY(__NR_sethostname); // (Not yet handled by Valgrind) FAIL;

   // __NR_setrlimit 75
   GO(__NR_setrlimit, "2s 1m");
   SY(__NR_setrlimit, x0, x0); FAIL;

   // __NR_getrlimit 76
   GO(__NR_getrlimit, "2s 1m");
   SY(__NR_getrlimit, x0, x0); FAIL;

   // __NR_getrusage 77
   GO(__NR_getrusage, "2s 1m");
   SY(__NR_getrusage, x0, x0); FAIL;

   // __NR_gettimeofday 78
   GO(__NR_gettimeofday, "2s 2m");
   SY(__NR_gettimeofday, x0+1, x0+1); FAIL;

   // __NR_settimeofday 79
   GO(__NR_settimeofday, "2s 2m");
   SY(__NR_settimeofday, x0+1, x0+1); FAIL;

   // __NR_getgroups 80
   GO(__NR_getgroups, "2s 1m");
   SY(__NR_getgroups, x0+1, x0+1); FAIL;

   // __NR_setgroups 81
   GO(__NR_setgroups, "2s 1m");
   SY(__NR_setgroups, x0+1, x0+1); FAIL;

   // __NR_select 82
   {
      long args[5] = { x0+8, x0+0xffffffee, x0+1, x0+1, x0+1 };
      GO(__NR_select, "1s 5m");
      SY(__NR_select, args+x0); FAIL;
   }

   // __NR_symlink 83
   GO(__NR_symlink, "2s 2m");
   SY(__NR_symlink, x0, x0); FAIL;

   // __NR_oldlstat 84
   GO(__NR_oldlstat, "n/a");
   // (obsolete, not handled by Valgrind)

   // __NR_readlink 85
   GO(__NR_readlink, "3s 2m");
   SY(__NR_readlink, x0+1, x0+1, x0+1); FAIL;

   // __NR_uselib 86
   GO(__NR_uselib, "n/a");
 //SY(__NR_uselib); // (Not yet handled by Valgrind) FAIL;

   // __NR_swapon 87
   GO(__NR_swapon, "n/a");
 //SY(__NR_swapon); // (Not yet handled by Valgrind) FAIL;

   // __NR_reboot 88
   GO(__NR_reboot, "n/a");
 //SY(__NR_reboot); // (Not yet handled by Valgrind) FAIL;

   // __NR_readdir 89
   GO(__NR_readdir, "n/a");
   // (superseded, not handled by Valgrind)

   // __NR_mmap 90
   {
      long args[6] = { x0, x0, x0, x0, x0-1, x0 };
      GO(__NR_mmap, "1s 1m");
      SY(__NR_mmap, args+x0); FAIL;
   }

   // __NR_munmap 91
   GO(__NR_munmap, "2s 0m");
   SY(__NR_munmap, x0, x0); FAIL;

   // __NR_truncate 92
   GO(__NR_truncate, "2s 1m");
   SY(__NR_truncate, x0, x0); FAIL;

   // __NR_ftruncate 93
   GO(__NR_ftruncate, "2s 0m");
   SY(__NR_ftruncate, x0, x0); FAIL;

   // __NR_fchmod 94
   GO(__NR_fchmod, "2s 0m");
   SY(__NR_fchmod, x0-1, x0); FAIL;

   // __NR_fchown 95
   GO(__NR_fchown, "3s 0m");
   SY(__NR_fchown, x0-1, x0, x0); FAIL;

   // __NR_getpriority 96
   GO(__NR_getpriority, "2s 0m");
   SY(__NR_getpriority, x0-1, x0); FAIL;

   // __NR_setpriority 97
   GO(__NR_setpriority, "3s 0m");
   SY(__NR_setpriority, x0-1, x0, x0); FAIL;

   // __NR_profil 98
   GO(__NR_profil, "ni");
   SY(__NR_profil); FAIL;

   // __NR_statfs 99
   GO(__NR_statfs, "2s 2m");
   SY(__NR_statfs, x0, x0); FAIL;

   // __NR_fstatfs 100
   GO(__NR_fstatfs, "2s 1m");
   SY(__NR_fstatfs, x0, x0); FAIL;

   // __NR_ioperm 101
   GO(__NR_ioperm, "3s 0m");
   SY(__NR_ioperm, x0, x0, x0); FAIL;

   // __NR_socketcall 102
   GO(__NR_socketcall, "XXX");
   // (XXX: need to do all sub-cases properly)

   // __NR_syslog 103
   GO(__NR_syslog, "3s 1m");
   SY(__NR_syslog, x0+2, x0, x0+1); FAIL;

   // __NR_setitimer 104
   GO(__NR_setitimer, "3s 2m");
   SY(__NR_setitimer, x0, x0+1, x0+1); FAIL;

   // __NR_getitimer 105
   GO(__NR_getitimer, "2s 1m");
   SY(__NR_getitimer, x0, x0, x0); FAIL;

   // __NR_stat 106
   GO(__NR_stat, "2s 2m");
   SY(__NR_stat, x0, x0); FAIL;

   // __NR_lstat 107
   GO(__NR_lstat, "2s 2m");
   SY(__NR_lstat, x0, x0); FAIL;

   // __NR_fstat 108
   GO(__NR_fstat, "2s 1m");
   SY(__NR_fstat, x0, x0); FAIL;

   // __NR_olduname 109
   GO(__NR_olduname, "n/a");
   // (obsolete, not handled by Valgrind)

   // __NR_iopl 110
   GO(__NR_iopl, "1s 0m");
   SY(__NR_iopl, x0+100); FAIL;

   // __NR_vhangup 111
   GO(__NR_vhangup, "0s 0m");
   SY(__NR_vhangup); SUCC_OR_FAIL;  // Will succeed for superuser
   
   // __NR_idle 112
   GO(__NR_idle, "ni");
   SY(__NR_idle); FAIL;

   // __NR_vm86old 113
   GO(__NR_vm86old, "n/a");
   // (will probably never be handled by Valgrind)

   // __NR_wait4 114
   GO(__NR_wait4, "4s 2m");
   SY(__NR_wait4, x0, x0+1, x0, x0+1); FAIL;

   // __NR_swapoff 115
   GO(__NR_swapoff, "n/a");
 //SY(__NR_swapoff); // (Not yet handled by Valgrind) FAIL;

   // __NR_sysinfo 116
   GO(__NR_sysinfo, "1s 1m");
   SY(__NR_sysinfo, x0); FAIL;

   // __NR_ipc 117
   // XXX: This is simplistic -- need to do all the sub-cases properly.
   // XXX: Also, should be 6 scalar errors, except glibc's syscall() doesn't
   //      use the 6th one!
   GO(__NR_ipc, "5s 0m");
   SY(__NR_ipc, x0+4, x0, x0, x0, x0, x0); FAIL;

   // __NR_fsync 118
   GO(__NR_fsync, "1s 0m");
   SY(__NR_fsync, x0-1); FAIL;

   // __NR_sigreturn 119
   GO(__NR_sigreturn, "n/a");
 //SY(__NR_sigreturn); // (Not yet handled by Valgrind) FAIL;

   // __NR_clone 120
#ifndef CLONE_PARENT_SETTID
#define CLONE_PARENT_SETTID	0x00100000
#endif
   GO(__NR_clone, "5s 3m");
   SY(__NR_clone, x0|CLONE_PARENT_SETTID|CLONE_SETTLS|CLONE_CHILD_SETTID|SIGCHLD, x0, x0, x0, x0); FAIL;
   if (0 == res) {
      SY(__NR_exit, 0); FAIL;
   }

   // __NR_setdomainname 121
   GO(__NR_setdomainname, "n/a");
 //SY(__NR_setdomainname); // (Not yet handled by Valgrind) FAIL;

   // __NR_uname 122
   GO(__NR_uname, "1s 1m");
   SY(__NR_uname, x0); FAIL;

   // __NR_modify_ldt 123
   GO(__NR_modify_ldt, "3s 1m");
   SY(__NR_modify_ldt, x0+1, x0, x0+1); FAILx(EINVAL);

   // __NR_adjtimex 124
   // XXX: need to do properly, but deref'ing NULL causing Valgrind to crash...
     GO(__NR_adjtimex, "XXX");
//   SY(__NR_adjtimex, x0); FAIL;

   // __NR_mprotect 125
   GO(__NR_mprotect, "3s 0m");
   SY(__NR_mprotect, x0+1, x0, x0); FAILx(EINVAL);

   // __NR_sigprocmask 126
   GO(__NR_sigprocmask, "3s 2m");
   SY(__NR_sigprocmask, x0, x0+&px[1], x0+&px[1]); SUCC;

   // __NR_create_module 127
   GO(__NR_create_module, "ni");
   SY(__NR_create_module); FAIL;

   // __NR_init_module 128
   GO(__NR_init_module, "3s 2m");
   SY(__NR_init_module, x0, x0+1, x0); FAIL;

   // __NR_delete_module 129
   GO(__NR_delete_module, "n/a");
 //SY(__NR_delete_module); // (Not yet handled by Valgrind) FAIL;

   // __NR_get_kernel_syms 130
   GO(__NR_get_kernel_syms, "ni");
   SY(__NR_get_kernel_syms); FAIL;

   // __NR_quotactl 131
   GO(__NR_quotactl, "4s 1m");
   SY(__NR_quotactl, x0, x0, x0, x0); FAIL;

   // __NR_getpgid 132
   GO(__NR_getpgid, "1s 0m");
   SY(__NR_getpgid, x0-1); FAIL;

   // __NR_fchdir 133
   GO(__NR_fchdir, "1s 0m");
   SY(__NR_fchdir, x0-1); FAIL;

   // __NR_bdflush 134
   GO(__NR_bdflush, "n/a");
 //SY(__NR_bdflush); // (Not yet handled by Valgrind) FAIL;

   // __NR_sysfs 135
   GO(__NR_sysfs, "n/a");
 //SY(__NR_sysfs); // (Not yet handled by Valgrind) FAIL;

   // __NR_personality 136
   GO(__NR_personality, "1s 0m");
   SY(__NR_personality, x0+0xffffffff); SUCC;

   // __NR_afs_syscall 137
   GO(__NR_afs_syscall, "ni");
   SY(__NR_afs_syscall); FAIL;

   // __NR_setfsuid 138
   GO(__NR_setfsuid, "1s 0m");
   SY(__NR_setfsuid, x0); SUCC;  // This syscall has a stupid return value

   // __NR_setfsgid 139
   GO(__NR_setfsgid, "1s 0m");
   SY(__NR_setfsgid, x0); SUCC;  // This syscall has a stupid return value

   // __NR__llseek 140
   GO(__NR__llseek, "5s 1m");
   SY(__NR__llseek, x0, x0, x0, x0, x0); FAIL;

   // __NR_getdents 141
   GO(__NR_getdents, "3s 1m");
   SY(__NR_getdents, x0, x0, x0+1); FAIL;

   // __NR__newselect 142
   GO(__NR__newselect, "5s 4m");
   SY(__NR__newselect, x0+8, x0+0xffffffff, x0+1, x0+1, x0+1); FAIL;

   // __NR_flock 143
   GO(__NR_flock, "2s 0m");
   SY(__NR_flock, x0, x0); FAIL;

   // __NR_msync 144
   GO(__NR_msync, "3s 1m");
   SY(__NR_msync, x0, x0+1, x0); FAIL;

   // __NR_readv 145
   GO(__NR_readv, "3s 1m");
   SY(__NR_readv, x0, x0, x0+1); FAIL;

   // __NR_writev 146
   GO(__NR_writev, "3s 1m");
   SY(__NR_writev, x0, x0, x0+1); FAIL;

   // __NR_getsid 147
   GO(__NR_getsid, "1s 0m");
   SY(__NR_getsid, x0-1); FAIL;

   // __NR_fdatasync 148
   GO(__NR_fdatasync, "1s 0m");
   SY(__NR_fdatasync, x0-1); FAIL;

   // __NR__sysctl 149
   GO(__NR__sysctl, "1s 1m");
   SY(__NR__sysctl, x0); FAIL;

   // __NR_mlock 150
   GO(__NR_mlock, "2s 0m");
   SY(__NR_mlock, x0, x0+1); FAIL;

   // __NR_munlock 151
   GO(__NR_munlock, "2s 0m");
   SY(__NR_munlock, x0, x0+1); FAIL;

   // __NR_mlockall 152
   GO(__NR_mlockall, "1s 0m");
   SY(__NR_mlockall, x0-1); FAIL;

   // __NR_munlockall 153
   GO(__NR_munlockall, "0s 0m");
   SY(__NR_munlockall); SUCC_OR_FAILx(EPERM);

   // __NR_sched_setparam 154
   GO(__NR_sched_setparam, "2s 1m");
   SY(__NR_sched_setparam, x0, x0); FAIL;

   // __NR_sched_getparam 155
   GO(__NR_sched_getparam, "2s 1m");
   SY(__NR_sched_getparam, x0, x0); FAIL;

   // __NR_sched_setscheduler 156
   GO(__NR_sched_setscheduler, "3s 1m");
   SY(__NR_sched_setscheduler, x0-1, x0, x0+1); FAIL;

   // __NR_sched_getscheduler 157
   GO(__NR_sched_getscheduler, "1s 0m");
   SY(__NR_sched_getscheduler, x0-1); FAIL;

   // __NR_sched_yield 158
   GO(__NR_sched_yield, "0s 0m");
   SY(__NR_sched_yield); SUCC;

   // __NR_sched_get_priority_max 159
   GO(__NR_sched_get_priority_max, "1s 0m");
   SY(__NR_sched_get_priority_max, x0-1); FAIL;

   // __NR_sched_get_priority_min 160
   GO(__NR_sched_get_priority_min, "1s 0m");
   SY(__NR_sched_get_priority_min, x0-1); FAIL;

   // __NR_sched_rr_get_interval 161
   GO(__NR_sched_rr_get_interval, "n/a");
 //SY(__NR_sched_rr_get_interval); // (Not yet handled by Valgrind) FAIL;

   // __NR_nanosleep 162
   GO(__NR_nanosleep, "2s 2m");
   SY(__NR_nanosleep, x0, x0+1); FAIL;

   // __NR_mremap 163
   GO(__NR_mremap, "5s 0m");
   SY(__NR_mremap, x0+1, x0, x0, x0+MREMAP_FIXED, x0); FAILx(EINVAL);

   // __NR_setresuid 164
   GO(__NR_setresuid, "3s 0m");
   SY(__NR_setresuid, x0-1, x0-1, x0-1); SUCC;

   // __NR_getresuid 165
   GO(__NR_getresuid, "3s 3m");
   SY(__NR_getresuid, x0, x0, x0); FAIL;

   // __NR_vm86 166
   GO(__NR_vm86, "n/a");
   // (will probably never be handled by Valgrind)

   // __NR_query_module 167
   GO(__NR_query_module, "ni");
   SY(__NR_query_module); FAIL;

   // __NR_poll 168
   GO(__NR_poll, "3s 1m");
   SY(__NR_poll, x0, x0+1, x0); FAIL;

   // __NR_nfsservctl 169
   GO(__NR_nfsservctl, "n/a");
 //SY(__NR_nfsservctl); // (Not yet handled by Valgrind) FAIL;

   // __NR_setresgid 170
   GO(__NR_setresgid, "3s 0m");
   SY(__NR_setresgid, x0-1, x0-1, x0-1); SUCC;

   // __NR_getresgid 171
   GO(__NR_getresgid, "3s 3m");
   SY(__NR_getresgid, x0, x0, x0); FAIL;

   // __NR_prctl 172
   GO(__NR_prctl, "5s 0m");
   SY(__NR_prctl, x0, x0, x0, x0, x0); FAIL;

   char buf16[16] = "123456789012345.";
   buf16[15] = x0; // this will cause 'using unitialised value'
   GO(__NR_prctl, "2s 0m");
   SY(__NR_prctl, x0 + PR_SET_NAME, buf16); SUCC;

   char buf17[17] = "1234567890123456.";
   buf17[16] = x0; // this must not cause 'using unitialised value'
   GO(__NR_prctl, "1s 0m");
   SY(__NR_prctl, x0 + PR_SET_NAME, buf17); SUCC;

   // __NR_rt_sigreturn 173
   GO(__NR_rt_sigreturn, "n/a");
 //SY(__NR_rt_sigreturn); // (Not yet handled by Valgrind) FAIL;

   // __NR_rt_sigaction 174
   GO(__NR_rt_sigaction, "4s 4m");
   SY(__NR_rt_sigaction, x0, x0+&px[2], x0+&px[2], x0); FAIL;

   // __NR_rt_sigprocmask 175
   GO(__NR_rt_sigprocmask, "4s 2m");
   SY(__NR_rt_sigprocmask, x0, x0+1, x0+1, x0); FAIL;

   // __NR_rt_sigpending 176
   GO(__NR_rt_sigpending, "2s 1m");
   SY(__NR_rt_sigpending, x0, x0+1); FAIL;

   // __NR_rt_sigtimedwait 177
   GO(__NR_rt_sigtimedwait, "4s 3m");
   SY(__NR_rt_sigtimedwait, x0+1, x0+1, x0+1, x0); FAIL;

   // __NR_rt_sigqueueinfo 178
   GO(__NR_rt_sigqueueinfo, "3s 1m");
   SY(__NR_rt_sigqueueinfo, x0, x0+1, x0); FAIL;

   // __NR_rt_sigsuspend 179
   GO(__NR_rt_sigsuspend, "2s 1m");
   SY(__NR_rt_sigsuspend, x0 + 1, x0 + sizeof(sigset_t)); FAILx(EFAULT);

   // __NR_pread64 180
   GO(__NR_pread64, "5s 1m");
   SY(__NR_pread64, x0, x0, x0+1, x0, x0); FAIL;

   // __NR_pwrite64 181
   GO(__NR_pwrite64, "5s 1m");
   SY(__NR_pwrite64, x0, x0, x0+1, x0, x0); FAIL;

   // __NR_chown 182
   GO(__NR_chown, "3s 1m");
   SY(__NR_chown, x0, x0, x0); FAIL;

   // __NR_getcwd 183
   GO(__NR_getcwd, "2s 1m");
   SY(__NR_getcwd, x0, x0+1); FAIL;

   // __NR_capget 184
   GO(__NR_capget, "2s 2m");
   SY(__NR_capget, x0, x0+1); FAIL;

   // __NR_capset 185
   GO(__NR_capset, "2s 2m");
   SY(__NR_capset, x0, x0); FAIL;

   // __NR_sigaltstack 186
   {
      struct our_sigaltstack {
              void *ss_sp;
              int ss_flags;
              size_t ss_size;
      } ss;
      ss.ss_sp     = NULL;
      ss.ss_flags  = 0;
      ss.ss_size   = 0;
      VALGRIND_MAKE_MEM_NOACCESS(& ss, sizeof(struct our_sigaltstack));
      GO(__NR_sigaltstack, "2s 2m");
      SY(__NR_sigaltstack, x0+&ss, x0+&ss); SUCC;
   }

   // __NR_sendfile 187
   GO(__NR_sendfile, "4s 1m");
   SY(__NR_sendfile, x0, x0, x0+1, x0); FAIL;

   // __NR_getpmsg 188
   // Could do 5s 4m with more effort, but I can't be bothered for this
   // crappy non-standard syscall.
   GO(__NR_getpmsg, "5s 0m");
   SY(__NR_getpmsg, x0, x0, x0, x0); FAIL;

   // __NR_putpmsg 189
   // Could do 5s 2m with more effort, but I can't be bothered for this
   // crappy non-standard syscall.
   GO(__NR_putpmsg, "5s 0m");
   SY(__NR_putpmsg, x0, x0, x0, x0, x0); FAIL;

   // __NR_vfork 190
   GO(__NR_vfork, "other");
   // (sse scalar_vfork.c)

   // __NR_ugetrlimit 191
   GO(__NR_ugetrlimit, "2s 1m");
   SY(__NR_ugetrlimit, x0, x0); FAIL;

   // __NR_mmap2 192
   GO(__NR_mmap2, "6s 0m");
   SY(__NR_mmap2, x0, x0, x0, x0, x0-1, x0); FAIL;

   // __NR_truncate64 193
   GO(__NR_truncate64, "3s 1m");
   SY(__NR_truncate64, x0, x0, x0); FAIL;

   // __NR_ftruncate64 194
   GO(__NR_ftruncate64, "3s 0m");
   SY(__NR_ftruncate64, x0, x0, x0); FAIL;

   // __NR_stat64 195
   GO(__NR_stat64, "2s 2m");
   SY(__NR_stat64, x0, x0); FAIL;

   // __NR_lstat64 196
   GO(__NR_lstat64, "2s 2m");
   SY(__NR_lstat64, x0, x0); FAIL;

   // __NR_fstat64 197
   GO(__NR_fstat64, "2s 1m");
   SY(__NR_fstat64, x0, x0); FAIL;

   // __NR_lchown32 198
   GO(__NR_lchown32, "3s 1m");
   SY(__NR_lchown32, x0, x0, x0); FAIL;

   // __NR_getuid32 199
   GO(__NR_getuid32, "0s 0m");
   SY(__NR_getuid32); SUCC;

   // __NR_getgid32 200
   GO(__NR_getgid32, "0s 0m");
   SY(__NR_getgid32); SUCC;

   // __NR_geteuid32 201
   GO(__NR_geteuid32, "0s 0m");
   SY(__NR_geteuid32); SUCC;

   // __NR_getegid32 202
   GO(__NR_getegid32, "0s 0m");
   SY(__NR_getegid32); SUCC;

   // __NR_setreuid32 203
   GO(__NR_setreuid32, "2s 0m");
   SY(__NR_setreuid32, x0-1, x0-1); SUCC;

   // __NR_setregid32 204
   GO(__NR_setregid32, "2s 0m");
   SY(__NR_setregid32, x0-1, x0-1); SUCC;

   // __NR_getgroups32 205
   GO(__NR_getgroups32, "2s 1m");
   SY(__NR_getgroups32, x0+1, x0+1); FAIL;

   // __NR_setgroups32 206
   GO(__NR_setgroups32, "2s 1m");
   SY(__NR_setgroups32, x0+1, x0+1); FAIL;

   // __NR_fchown32 207
   GO(__NR_fchown32, "3s 0m");
   SY(__NR_fchown32, x0-1, x0, x0); FAIL;

   // __NR_setresuid32 208
   GO(__NR_setresuid32, "3s 0m");
   SY(__NR_setresuid32, x0-1, x0-1, x0-1); SUCC;

   // __NR_getresuid32 209
   GO(__NR_getresuid32, "3s 3m");
   SY(__NR_getresuid32, x0, x0, x0); FAIL;

   // __NR_setresgid32 210
   GO(__NR_setresgid32, "3s 0m");
   SY(__NR_setresgid32, x0-1, x0-1, x0-1); SUCC;

   // __NR_getresgid32 211
   GO(__NR_getresgid32, "3s 3m");
   SY(__NR_getresgid32, x0, x0, x0); FAIL;

   // __NR_chown32 212
   GO(__NR_chown32, "3s 1m");
   SY(__NR_chown32, x0, x0, x0); FAIL;

   // __NR_setuid32 213
   GO(__NR_setuid32, "1s 0m");
   SY(__NR_setuid32, x0-1); FAIL;

   // __NR_setgid32 214
   GO(__NR_setgid32, "1s 0m");
   SY(__NR_setgid32, x0-1); FAIL;

   // __NR_setfsuid32 215
   GO(__NR_setfsuid32, "1s 0m");
   SY(__NR_setfsuid32, x0); SUCC;  // This syscall has a stupid return value

   // __NR_setfsgid32 216
   GO(__NR_setfsgid32, "1s 0m");
   SY(__NR_setfsgid32, x0); SUCC;  // This syscall has a stupid return value

   // __NR_pivot_root 217
   GO(__NR_pivot_root, "n/a");
 //SY(__NR_pivot_root); // (Not yet handled by Valgrind) FAIL;

   // __NR_mincore 218
   GO(__NR_mincore, "3s 1m");
   SY(__NR_mincore, x0, x0+40960, x0); FAIL;

   // __NR_madvise 219
   GO(__NR_madvise, "3s 0m");
   SY(__NR_madvise, x0, x0+1, x0); FAILx(ENOMEM);

   // __NR_getdents64 220
   GO(__NR_getdents64, "3s 1m");
   SY(__NR_getdents64, x0, x0, x0+1); FAIL;

   // __NR_fcntl64 221
   // As with sys_open(), we don't trigger errors for the 1st two args for
   // the later ones.
   // For F_GETFD the 3rd arg is ignored.
   GO(__NR_fcntl64, "(GETFD) 2s 0m");
   SY(__NR_fcntl64, x0-1, x0+F_GETFD, x0); FAILx(EBADF);

   // For F_DUPFD the 3rd arg is 'arg'
   GO(__NR_fcntl64, "(DUPFD) 1s 0m");
   SY(__NR_fcntl64, -1, F_DUPFD, x0); FAILx(EBADF);

   // For F_GETLK the 3rd arg is 'lock'.
   // On x86, this fails w/EBADF.  But on amd64 in 32-bit mode it fails
   // w/EFAULT.
   GO(__NR_fcntl64, "(GETLK) 1s 0m"); 
   SY(__NR_fcntl64, -1, +F_GETLK, x0); FAIL; //FAILx(EBADF);

   // 222
   GO(222, "ni");
   SY(222); FAIL;

   // 223
   GO(223, "ni");
   SY(223); FAIL;

   // __NR_gettid 224
   GO(__NR_gettid, "n/a");
 //SY(__NR_gettid); // (Not yet handled by Valgrind) FAIL;

   // __NR_readahead 225
   GO(__NR_readahead, "n/a");
 //SY(__NR_readahead); // (Not yet handled by Valgrind) FAIL;

   // __NR_setxattr 226
   GO(__NR_setxattr, "5s 3m");
   SY(__NR_setxattr, x0, x0, x0, x0+1, x0); FAIL;

   // __NR_lsetxattr 227
   GO(__NR_lsetxattr, "5s 3m");
   SY(__NR_lsetxattr, x0, x0, x0, x0+1, x0); FAIL;

   // __NR_fsetxattr 228
   GO(__NR_fsetxattr, "5s 2m");
   SY(__NR_fsetxattr, x0, x0, x0, x0+1, x0); FAIL;

   // __NR_getxattr 229
   GO(__NR_getxattr, "4s 3m");
   SY(__NR_getxattr, x0, x0, x0, x0+1); FAIL;

   // __NR_lgetxattr 230
   GO(__NR_lgetxattr, "4s 3m");
   SY(__NR_lgetxattr, x0, x0, x0, x0+1); FAIL;

   // __NR_fgetxattr 231
   GO(__NR_fgetxattr, "4s 2m");
   SY(__NR_fgetxattr, x0, x0, x0, x0+1); FAIL;

   // __NR_listxattr 232
   GO(__NR_listxattr, "3s 2m");
   SY(__NR_listxattr, x0, x0, x0+1); FAIL;

   // __NR_llistxattr 233
   GO(__NR_llistxattr, "3s 2m");
   SY(__NR_llistxattr, x0, x0, x0+1); FAIL;

   // __NR_flistxattr 234
   GO(__NR_flistxattr, "3s 1m");
   SY(__NR_flistxattr, x0-1, x0, x0+1); FAIL; /* kernel returns EBADF, but both seem correct */

   // __NR_removexattr 235
   GO(__NR_removexattr, "2s 2m");
   SY(__NR_removexattr, x0, x0); FAIL;

   // __NR_lremovexattr 236
   GO(__NR_lremovexattr, "2s 2m");
   SY(__NR_lremovexattr, x0, x0); FAIL;

   // __NR_fremovexattr 237
   GO(__NR_fremovexattr, "2s 1m");
   SY(__NR_fremovexattr, x0, x0); FAIL;

   // __NR_tkill 238
   GO(__NR_tkill, "n/a");
 //SY(__NR_tkill); // (Not yet handled by Valgrind) FAIL;

   // __NR_sendfile64 239
   GO(__NR_sendfile64, "4s 1m");
   SY(__NR_sendfile64, x0, x0, x0+1, x0); FAIL;

   // __NR_futex 240
   #ifndef FUTEX_WAIT
   #define FUTEX_WAIT   0
   #endif
   // XXX: again, glibc not doing 6th arg means we have only 5s errors
   GO(__NR_futex, "4s 2m");
   SY(__NR_futex, x0+FUTEX_WAIT, x0, x0, x0+1); FAIL;

   // __NR_sched_setaffinity 241
   GO(__NR_sched_setaffinity, "3s 1m");
   SY(__NR_sched_setaffinity, x0, x0+1, x0); FAIL;

   // __NR_sched_getaffinity 242
   GO(__NR_sched_getaffinity, "3s 1m");
   SY(__NR_sched_getaffinity, x0, x0+1, x0); FAIL;

   // __NR_set_thread_area 243
   GO(__NR_set_thread_area, "1s 1m");
   SY(__NR_set_thread_area, x0); FAILx(EFAULT);

   // __NR_get_thread_area 244
   GO(__NR_get_thread_area, "1s 1m");
   SY(__NR_get_thread_area, x0); FAILx(EFAULT);

   // __NR_io_setup 245
   GO(__NR_io_setup, "2s 1m");
   SY(__NR_io_setup, x0, x0); FAIL;

   // __NR_io_destroy 246
   {
      // jump through hoops to prevent the PRE(io_destroy) wrapper crashing.
      struct fake_aio_ring {   
        unsigned        id;     /* kernel internal index number */
        unsigned        nr;     /* number of io_events */
        // There are more fields in the real aio_ring, but the 'nr' field is
        // the only one used by the PRE() wrapper.
      } ring = { 0, 0 };
      struct fake_aio_ring* ringptr = &ring;
      GO(__NR_io_destroy, "1s 0m");
      SY(__NR_io_destroy, x0+&ringptr); FAIL;
   }

   // __NR_io_getevents 247
   GO(__NR_io_getevents, "5s 2m");
   SY(__NR_io_getevents, x0, x0, x0+1, x0, x0+1); FAIL;

   // __NR_io_submit 248
   GO(__NR_io_submit, "3s 1m");
   SY(__NR_io_submit, x0, x0+1, x0); FAIL;

   // __NR_io_cancel 249
   GO(__NR_io_cancel, "3s 2m");
   SY(__NR_io_cancel, x0, x0, x0); FAIL;

   // __NR_fadvise64 250
   GO(__NR_fadvise64, "n/a");
 //SY(__NR_fadvise64); // (Not yet handled by Valgrind) FAIL;

   // 251
   GO(251, "ni");
   SY(251); FAIL;

   // __NR_exit_group 252
   GO(__NR_exit_group, "other");
   // (see scalar_exit_group.c)

   // __NR_lookup_dcookie 253
   GO(__NR_lookup_dcookie, "4s 1m");
   SY(__NR_lookup_dcookie, x0, x0, x0, x0+1); FAIL;

   // __NR_epoll_create 254
   GO(__NR_epoll_create, "1s 0m");
   SY(__NR_epoll_create, x0); SUCC_OR_FAIL;

   // __NR_epoll_ctl 255
   GO(__NR_epoll_ctl, "4s 1m");
   SY(__NR_epoll_ctl, x0, x0, x0, x0); FAIL;

   // __NR_epoll_wait 256
   GO(__NR_epoll_wait, "4s 1m");
   SY(__NR_epoll_wait, x0, x0, x0+1, x0); FAIL;

   // __NR_remap_file_pages 257
   GO(__NR_remap_file_pages, "n/a");
 //SY(__NR_remap_file_pages); // (Not yet handled by Valgrind) FAIL;

   // __NR_set_tid_address 258
   GO(__NR_set_tid_address, "1s 0m");
   SY(__NR_set_tid_address, x0); SUCC_OR_FAILx(ENOSYS);

   // __NR_timer_create 259
   GO(__NR_timer_create, "3s 2m");
   SY(__NR_timer_create, x0, x0+1, x0); FAIL;

   // __NR_timer_settime (__NR_timer_create+1)
   GO(__NR_timer_settime, "4s 2m");
   SY(__NR_timer_settime, x0, x0, x0, x0+1); FAIL;

   // __NR_timer_gettime (__NR_timer_create+2)
   GO(__NR_timer_gettime, "2s 1m");
   SY(__NR_timer_gettime, x0, x0); FAIL;

   // __NR_timer_getoverrun (__NR_timer_create+3)
   GO(__NR_timer_getoverrun, "1s 0m");
   SY(__NR_timer_getoverrun, x0); FAIL;

   // __NR_timer_delete (__NR_timer_create+4)
   GO(__NR_timer_delete, "1s 0m");
   SY(__NR_timer_delete, x0); FAIL;

   // __NR_clock_settime (__NR_timer_create+5)
   GO(__NR_clock_settime, "2s 1m");
   SY(__NR_clock_settime, x0, x0);  FAIL; FAIL;

   // __NR_clock_gettime (__NR_timer_create+6)
   GO(__NR_clock_gettime, "2s 1m");
   SY(__NR_clock_gettime, x0, x0); FAIL;

   // __NR_clock_getres (__NR_timer_create+7)
   GO(__NR_clock_getres, "2s 1m");
   SY(__NR_clock_getres, x0+1, x0+1); FAIL; FAIL;

   // __NR_clock_nanosleep (__NR_timer_create+8)
   GO(__NR_clock_nanosleep, "n/a");
 //SY(__NR_clock_nanosleep); // (Not yet handled by Valgrind) FAIL;

   // __NR_statfs64 268
   GO(__NR_statfs64, "3s 2m");
   SY(__NR_statfs64, x0, x0+1, x0); FAIL;

   // __NR_fstatfs64 269
   GO(__NR_fstatfs64, "3s 1m");
   SY(__NR_fstatfs64, x0, x0+1, x0); FAIL;

   // __NR_tgkill 270
   GO(__NR_tgkill, "n/a");
 //SY(__NR_tgkill); // (Not yet handled by Valgrind) FAIL;

   // __NR_utimes 271
   GO(__NR_utimes, "2s 2m");
   SY(__NR_utimes, x0, x0+1); FAIL;

   // __NR_fadvise64_64 272
   GO(__NR_fadvise64_64, "n/a");
 //SY(__NR_fadvise64_64); // (Not yet handled by Valgrind) FAIL;

   // __NR_vserver 273
   GO(__NR_vserver, "ni");
   SY(__NR_vserver); FAIL;

   // __NR_mbind 274
   GO(__NR_mbind, "n/a");
 //SY(__NR_mbind); // (Not yet handled by Valgrind) FAIL;

   // __NR_get_mempolicy 275
   GO(__NR_get_mempolicy, "n/a");
 //SY(__NR_get_mempolicy); // (Not yet handled by Valgrind) FAIL;

   // __NR_set_mempolicy 276
   GO(__NR_set_mempolicy, "n/a");
 //SY(__NR_set_mempolicy); // (Not yet handled by Valgrind) FAIL;

   // __NR_mq_open 277
   GO(__NR_mq_open, "4s 3m");
   SY(__NR_mq_open, x0, x0+O_CREAT, x0, x0+1); FAIL;

   // __NR_mq_unlink (__NR_mq_open+1)
   GO(__NR_mq_unlink, "1s 1m");
   SY(__NR_mq_unlink, x0); FAIL;

   // __NR_mq_timedsend (__NR_mq_open+2)
   GO(__NR_mq_timedsend, "5s 2m");
   SY(__NR_mq_timedsend, x0, x0, x0+1, x0, x0+1); FAIL;

   // __NR_mq_timedreceive (__NR_mq_open+3)
   GO(__NR_mq_timedreceive, "5s 3m");
   SY(__NR_mq_timedreceive, x0, x0, x0+1, x0+1, x0+1); FAIL;
  
   // __NR_mq_notify (__NR_mq_open+4)
   GO(__NR_mq_notify, "2s 1m");
   SY(__NR_mq_notify, x0, x0+1); FAIL;

   // __NR_mq_getsetattr (__NR_mq_open+5)
   GO(__NR_mq_getsetattr, "3s 2m");
   SY(__NR_mq_getsetattr, x0, x0+1, x0+1); FAIL;
   
   // __NR_sys_kexec_load 283
   GO(__NR_sys_kexec_load, "ni");
   SY(__NR_sys_kexec_load); FAIL;

   // __NR_epoll_create1 329
   GO(__NR_epoll_create1, "1s 0m");
   SY(__NR_epoll_create1, x0); SUCC_OR_FAIL;

   // __NR_process_vm_readv 347
   GO(__NR_process_vm_readv, "6s 2m");
   SY(__NR_process_vm_readv, x0, x0, x0+1, x0, x0+1, x0); FAIL;

   // __NR_process_vm_writev 348
   GO(__NR_process_vm_writev, "6s 2m");
   SY(__NR_process_vm_writev, x0, x0, x0+1, x0, x0+1, x0); FAIL;

   // no such syscall...
   GO(9999, "1e");
   SY(9999); FAIL;

   // __NR_exit 1
   GO(__NR_exit, "1s 0m");
   SY(__NR_exit, x0); FAIL;

   assert(0);
}

