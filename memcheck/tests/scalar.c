#include "scalar.h"

int main(void)
{
   // uninitialised, but we know px[0] is 0x0
   long* px  = malloc(sizeof(long));
   long  x0  = px[0];

   // All __NR_xxx numbers are taken from x86

   // __NR_restart_syscall 0  XXX ???
   // (see below)

   // __NR_exit 1 
   GO(__NR_exit, "other");
   // (see below)

   // __NR_fork 2
   GO(__NR_fork, "0s 0m");
   // (sse scalar_fork.c)

   // __NR_read 3
   // Nb: here we are also getting an error from the syscall arg itself.
   GO(__NR_read, "1+3s 1m");
   SY(__NR_read+x0, x0, x0, x0+1);

   // __NR_write 4
   GO(__NR_write, "3s 1m");
   SY(__NR_write, x0, x0, x0+1);

   // __NR_open 5
   GO(__NR_open, "(2-args) 2s 1m");
   SY(__NR_open, x0, x0, x0+1);

   GO(__NR_open, "(3-args) 1s 0m");
   SY(__NR_open, "tmp_write_file_foo", O_CREAT, x0);

   // __NR_close 6
   GO(__NR_close, "1s 0m");
   SY(__NR_close, x0-1);

   // __NR_waitpid 7
   GO(__NR_waitpid, "3s 1m");
   SY(__NR_waitpid, x0, x0+1, x0);

   // __NR_creat 8
   GO(__NR_creat, "2s 1m");
   SY(__NR_creat, x0, x0);

   // __NR_link 9
   GO(__NR_link, "2s 2m");
   SY(__NR_link, x0, x0);

   // __NR_unlink 10
   GO(__NR_unlink, "1s 1m");
   SY(__NR_unlink, x0);

   // __NR_execve 11
   // Nb: could have 3 memory errors if we pass x0+1 as the 2nd and 3rd
   // args, except for bug #93174.
   GO(__NR_execve, "3s 1m");
   SY(__NR_execve, x0, x0, x0);

   // __NR_chdir 12
   GO(__NR_chdir, "1s 1m");
   SY(__NR_chdir, x0);

   // __NR_time 13
   GO(__NR_time, "1s 1m");
   SY(__NR_time, x0+1);

   // __NR_mknod 14
   GO(__NR_mknod, "3s 1m");
   SY(__NR_mknod, x0, x0, x0);

   // __NR_chmod 15
   GO(__NR_chmod, "2s 1m");
   SY(__NR_chmod, x0, x0);

   // __NR_lchown 16
   GO(__NR_lchown, "n/a");
 //SY(__NR_lchown); // (Not yet handled by Valgrind)

   // __NR_break 17
   GO(__NR_break, "ni");
   SY(__NR_break);

   // __NR_oldstat 18
   GO(__NR_oldstat, "n/a");
   // (obsolete, not handled by Valgrind)

   // __NR_lseek 19
   GO(__NR_lseek, "3s 0m");
   SY(__NR_lseek, x0, x0, x0);

   // __NR_getpid 20
   GO(__NR_getpid, "0s 0m");
   SY(__NR_getpid);

   // __NR_mount 21
   GO(__NR_mount, "5s 3m");
   SY(__NR_mount, x0, x0, x0, x0, x0);
   
   // __NR_umount 22
   GO(__NR_umount, "1s 1m");
   SY(__NR_umount, x0);

   // __NR_setuid 23
   GO(__NR_setuid, "1s 0m");
   SY(__NR_setuid, x0);

   // __NR_getuid 24
   GO(__NR_getuid, "0s 0m");
   SY(__NR_getuid);

   // __NR_stime 25
   GO(__NR_stime, "n/a");
 //SY(__NR_stime); // (Not yet handled by Valgrind)

   // __NR_ptrace 26
   // XXX: memory pointed to be arg3 is never checked...
   GO(__NR_ptrace, "4s 2m");
   SY(__NR_ptrace, x0+PTRACE_GETREGS, x0, x0, x0);

   // __NR_alarm 27
   GO(__NR_alarm, "1s 0m");
   SY(__NR_alarm, x0);

   // __NR_oldfstat 28
   GO(__NR_oldfstat, "n/a");
   // (obsolete, not handled by Valgrind)

   // __NR_pause 29
   // XXX: will have to be tested separately

   // __NR_utime 30
   GO(__NR_utime, "2s 2m");
   SY(__NR_utime, x0, x0+1);

   // __NR_stty 31
   GO(__NR_stty, "ni");
   SY(__NR_stty);

   // __NR_gtty 32
   GO(__NR_gtty, "ni");
   SY(__NR_gtty);

   // __NR_access 33
   GO(__NR_access, "2s 1m");
   SY(__NR_access, x0, x0);

   // __NR_nice 34
   GO(__NR_nice, "1s 0m");
   SY(__NR_nice, x0);

   // __NR_ftime 35
   GO(__NR_ftime, "ni");
   SY(__NR_ftime);

   // __NR_sync 36
   GO(__NR_sync, "0s 0m");
   SY(__NR_sync);

   // __NR_kill 37
   GO(__NR_kill, "2s 0m");
   SY(__NR_kill, x0, x0);

   // __NR_rename 38
   GO(__NR_rename, "2s 2m");
   SY(__NR_rename, x0, x0);

   // __NR_mkdir 39
   GO(__NR_mkdir, "2s 1m");
   SY(__NR_mkdir, x0, x0);

   // __NR_rmdir 40
   GO(__NR_rmdir, "1s 1m");
   SY(__NR_rmdir, x0);

   // __NR_dup 41
   GO(__NR_dup, "1s 0m");
   SY(__NR_dup, x0);

   // __NR_pipe 42
   GO(__NR_pipe, "1s 1m");
   SY(__NR_pipe, x0);

   // __NR_times 43
   GO(__NR_times, "1s 1m");
   SY(__NR_times, x0);

   // __NR_prof 44
   GO(__NR_prof, "ni");
   SY(__NR_prof);

   // __NR_brk 45
   GO(__NR_brk, "1s 0m");
   SY(__NR_brk, x0);

   // __NR_setgid 46
   GO(__NR_setgid, "1s 0m");
   SY(__NR_setgid, x0);

   // __NR_getgid 47
   GO(__NR_getgid, "0s 0m");
   SY(__NR_getgid);

   // __NR_signal 48
   GO(__NR_signal, "n/a");
 //SY(__NR_signal); // (Not yet handled by Valgrind)

   // __NR_geteuid 49
   GO(__NR_geteuid, "0s 0m");
   SY(__NR_geteuid);

   // __NR_getegid 50
   GO(__NR_getegid, "0s 0m");
   SY(__NR_getegid);

   // __NR_acct 51
   GO(__NR_acct, "1s 1m");
   SY(__NR_acct, x0);

   // __NR_umount2 52
   GO(__NR_umount2, "2s 1m");
   SY(__NR_umount2, x0, x0);

   // __NR_lock 53
   GO(__NR_lock, "ni");
   SY(__NR_lock);

   // __NR_ioctl 54
   #include <asm/ioctls.h>
   GO(__NR_ioctl, "3s 1m");
   SY(__NR_ioctl, x0, x0+TCSETS, x0);

   // __NR_fcntl 55
   GO(__NR_fcntl, "3s 0m");
   SY(__NR_fcntl, x0, x0, x0);

   // __NR_mpx 56
   GO(__NR_mpx, "ni");
   SY(__NR_mpx);

   // __NR_setpgid 57
   GO(__NR_setpgid, "2s 0m");
   SY(__NR_setpgid, x0, x0);

   // __NR_ulimit 58
   GO(__NR_ulimit, "ni");
   SY(__NR_ulimit);

   // __NR_oldolduname 59
   GO(__NR_oldolduname, "n/a");
   // (obsolete, not handled by Valgrind)

   // __NR_umask 60
   GO(__NR_umask, "1s 0m");
   SY(__NR_umask, x0);

   // __NR_chroot 61
   GO(__NR_chroot, "1s 1m");
   SY(__NR_chroot, x0);

   // __NR_ustat 62
   GO(__NR_ustat, "n/a");
   // (deprecated, not handled by Valgrind)

   // __NR_dup2 63
   GO(__NR_dup2, "2s 0m");
   SY(__NR_dup2, x0, x0);

   // __NR_getppid 64
   GO(__NR_getppid, "0s 0m");
   SY(__NR_getppid);

   // __NR_getpgrp 65
   GO(__NR_getpgrp, "0s 0m");
   SY(__NR_getpgrp);

   // __NR_setsid 66
   GO(__NR_setsid, "0s 0m");
   SY(__NR_setsid);

   // __NR_sigaction 67
   GO(__NR_sigaction, "3s 2m");
   SY(__NR_sigaction, x0, x0+1, x0+1);

   // __NR_sgetmask 68 sys_sgetmask()
   GO(__NR_sgetmask, "n/a");
 //SY(__NR_sgetmask); // (Not yet handled by Valgrind)

   // __NR_ssetmask 69
   GO(__NR_ssetmask, "n/a");
 //SY(__NR_ssetmask); // (Not yet handled by Valgrind)

   // __NR_setreuid 70
   GO(__NR_setreuid, "2s 0m");
   SY(__NR_setreuid, x0, x0);

   // __NR_setregid 71
   GO(__NR_setregid, "2s 0m");
   SY(__NR_setregid, x0, x0);

   // __NR_sigsuspend 72
   // XXX: how do you use this function?
// GO(__NR_sigsuspend, ".s .m");
// SY(__NR_sigsuspend);

   // __NR_sigpending 73
   GO(__NR_sigpending, "1s 1m");
   SY(__NR_sigpending, x0);

   // __NR_sethostname 74
   GO(__NR_sethostname, "n/a");
 //SY(__NR_sethostname); // (Not yet handled by Valgrind)

   // __NR_setrlimit 75
   GO(__NR_setrlimit, "2s 1m");
   SY(__NR_setrlimit, x0, x0);

   // __NR_getrlimit 76
   GO(__NR_getrlimit, "2s 1m");
   SY(__NR_getrlimit, x0, x0);

   // __NR_getrusage 77
   GO(__NR_getrusage, "2s 1m");
   SY(__NR_getrusage, x0, x0);

   // __NR_gettimeofday 78
   GO(__NR_gettimeofday, "2s 2m");
   SY(__NR_gettimeofday, x0, x0+1);

   // __NR_settimeofday 79
   GO(__NR_settimeofday, "2s 2m");
   SY(__NR_settimeofday, x0, x0+1);

   // __NR_getgroups 80
   GO(__NR_getgroups, "2s 1m");
   SY(__NR_getgroups, x0+1, x0+1);

   // __NR_setgroups 81
   GO(__NR_setgroups, "2s 1m");
   SY(__NR_setgroups, x0+1, x0+1);

   // __NR_select 82
   {
      long args[5] = { x0+8, x0+0xffffffee, x0+1, x0+1, x0+1 };
      GO(__NR_select, "1s 4m");
      SY(__NR_select, args+x0);
   }

   // __NR_symlink 83
   GO(__NR_symlink, "2s 2m");
   SY(__NR_symlink, x0, x0);

   // __NR_oldlstat 84
   GO(__NR_oldlstat, "n/a");
   // (obsolete, not handled by Valgrind)

   // __NR_readlink 85
   GO(__NR_readlink, "3s 2m");
   SY(__NR_readlink, x0+1, x0+1, x0+1);

   // __NR_uselib 86
   GO(__NR_uselib, "n/a");
 //SY(__NR_uselib); // (Not yet handled by Valgrind)

   // __NR_swapon 87
   GO(__NR_swapon, "n/a");
 //SY(__NR_swapon); // (Not yet handled by Valgrind)

   // __NR_reboot 88
   GO(__NR_reboot, "n/a");
 //SY(__NR_reboot); // (Not yet handled by Valgrind)

   // __NR_readdir 89
   GO(__NR_readdir, "n/a");
   // (superseded, not handled by Valgrind)

   // __NR_mmap 90
   {
      long args[6] = { x0, x0, x0, x0, x0-1, x0 };
      GO(__NR_mmap, "1s 0m");
      SY(__NR_mmap, args+x0);
   }

   // __NR_munmap 91
   GO(__NR_munmap, "2s 0m");
   SY(__NR_munmap, x0, x0);

   // __NR_truncate 92
   GO(__NR_truncate, "2s 1m");
   SY(__NR_truncate, x0, x0);

   // __NR_ftruncate 93
   GO(__NR_ftruncate, "2s 0m");
   SY(__NR_ftruncate, x0, x0);

   // __NR_fchmod 94
   GO(__NR_fchmod, "2s 0m");
   SY(__NR_fchmod, x0-1, x0);

   // __NR_fchown 95
   GO(__NR_fchown, "3s 0m");
   SY(__NR_fchown, x0, x0, x0);

   // __NR_getpriority 96
   GO(__NR_getpriority, "2s 0m");
   SY(__NR_getpriority, x0, x0);

   // __NR_setpriority 97
   GO(__NR_setpriority, "3s 0m");
   SY(__NR_setpriority, x0, x0, x0);

   // __NR_profil 98
   GO(__NR_profil, "ni");
   SY(__NR_profil);

   // __NR_statfs 99
   GO(__NR_statfs, "2s 2m");
   SY(__NR_statfs, x0, x0);

   // __NR_fstatfs 100
   GO(__NR_fstatfs, "2s 1m");
   SY(__NR_fstatfs, x0, x0);

   // __NR_ioperm 101
   GO(__NR_ioperm, "3s 0m");
   SY(__NR_ioperm, x0, x0, x0);

   // __NR_socketcall 102
   // XXX: need to do properly
// GO(__NR_socketcall, "2s 1m");
// SY(__NR_socketcall, x0+SYS_SOCKETPAIR, x0);

   // __NR_syslog 103
   GO(__NR_syslog, "3s 1m");
   SY(__NR_syslog, x0+2, x0, x0+1);

   // __NR_setitimer 104
   GO(__NR_setitimer, "3s 2m");
   SY(__NR_setitimer, x0, x0+1, x0+1);

   // __NR_getitimer 105
   GO(__NR_getitimer, "2s 1m");
   SY(__NR_getitimer, x0, x0, x0);

   // __NR_stat 106
   GO(__NR_stat, "2s 2m");
   SY(__NR_stat, x0, x0);

   // __NR_lstat 107
   GO(__NR_lstat, "2s 2m");
   SY(__NR_lstat, x0, x0);

   // __NR_fstat 108
   GO(__NR_fstat, "2s 1m");
   SY(__NR_fstat, x0, x0);

   // __NR_olduname 109
   GO(__NR_olduname, "n/a");
   // (obsolete, not handled by Valgrind)

   // __NR_iopl 110
   GO(__NR_iopl, "1s 0m");
   SY(__NR_iopl, x0);

   // __NR_vhangup 111
   GO(__NR_vhangup, "0s 0m");
   SY(__NR_vhangup);
   
   // __NR_idle 112
   GO(__NR_idle, "ni");
   SY(__NR_idle);

   // __NR_vm86old 113
   GO(__NR_vm86old, "n/a");
   // (will probably never be handled by Valgrind)

   // __NR_wait4 114
   GO(__NR_wait4, "4s 2m");
   SY(__NR_wait4, x0, x0+1, x0, x0+1);

   // __NR_swapoff 115
 //GO(__NR_swapoff, ".s .m");
 //SY(__NR_swapoff);

   // __NR_sysinfo 116
   GO(__NR_sysinfo, "1s 1m");
   SY(__NR_sysinfo, x0);

   // __NR_ipc 117
   // XXX: This is simplistic -- doesn't treat any of the sub-ops.
   // XXX: Also, should be 6 scalar errors, except glibc's syscall() doesn't
   //      use the 6th one!
   #include <asm/ipc.h>
   GO(__NR_ipc, "5s 0m");
   SY(__NR_ipc, x0+4, x0, x0, x0, x0, x0);

   // __NR_fsync 118
   GO(__NR_fsync, "1s 0m");
   SY(__NR_fsync, x0-1);

   // __NR_sigreturn 119
 //GO(__NR_sigreturn, ".s .m");
 //SY(__NR_sigreturn);

   // __NR_clone 120
   #include <sched.h>
   #include <signal.h>
#ifndef CLONE_PARENT_SETTID
#define CLONE_PARENT_SETTID	0x00100000
#endif
   // XXX: should really be "4s 2m"?  Not sure... (see PRE(sys_clone))
   GO(__NR_clone, "4s 0m");
   if (SY(__NR_clone, x0|CLONE_PARENT_SETTID|SIGCHLD, x0, x0, x0) == 0)
   {
      SY(__NR_exit, 0);
   }

   // __NR_setdomainname 121
   GO(__NR_setdomainname, "n/a");
 //SY(__NR_setdomainname); // (Not yet handled by Valgrind)

   // __NR_uname 122
   GO(__NR_uname, "1s 1m");
   SY(__NR_uname, x0);

   // __NR_modify_ldt 123
 //GO(__NR_modify_ldt, ".s .m");
 //SY(__NR_modify_ldt);

   // __NR_adjtimex 124
   // XXX: need to do properly, but deref'ing NULL...
//   GO(__NR_adjtimex, "1s 1m");
//   SY(__NR_adjtimex, x0);

   // __NR_mprotect 125
   GO(__NR_mprotect, "3s 0m");
   SY(__NR_mprotect, x0, x0, x0);

   // __NR_sigprocmask 126
 //GO(__NR_sigprocmask, ".s .m");
 //SY(__NR_sigprocmask);

   // __NR_create_module 127
   GO(__NR_create_module, "ni");
   SY(__NR_create_module);

   // __NR_init_module 128
 //GO(__NR_init_module, ".s .m");
 //SY(__NR_init_module);

   // __NR_delete_module 129
   GO(__NR_delete_module, "n/a");
 //SY(__NR_delete_module); // (Not yet handled by Valgrind)

   // __NR_get_kernel_syms 130
   GO(__NR_get_kernel_syms, "ni");
   SY(__NR_get_kernel_syms);

   // __NR_quotactl 131
   GO(__NR_quotactl, "4s 1m");
   SY(__NR_quotactl, x0, x0, x0, x0);

   // __NR_getpgid 132
   GO(__NR_getpgid, "1s 0m");
   SY(__NR_getpgid, x0);

   // __NR_fchdir 133
   GO(__NR_fchdir, "1s 0m");
   SY(__NR_fchdir, x0-1);

   // __NR_bdflush 134
 //GO(__NR_bdflush, ".s .m");
 //SY(__NR_bdflush);

   // __NR_sysfs 135
 //GO(__NR_sysfs, ".s .m");
 //SY(__NR_sysfs);

   // __NR_personality 136
   GO(__NR_personality, "1s 0m");
   SY(__NR_personality, x0);

   // __NR_afs_syscall 137
   GO(__NR_afs_syscall, "ni");
   SY(__NR_afs_syscall);

   // __NR_setfsuid 138
   GO(__NR_setfsuid, "1s 0m");
   SY(__NR_setfsuid, x0);

   // __NR_setfsgid 139
   GO(__NR_setfsgid, "1s 0m");
   SY(__NR_setfsgid, x0);

   // __NR__llseek 140
 //GO(__NR__llseek, ".s .m");
 //SY(__NR__llseek);

   // __NR_getdents 141
   GO(__NR_getdents, "3s 1m");
   SY(__NR_getdents, x0, x0, x0+1);

   // __NR__newselect 142
   GO(__NR__newselect, "5s 4m");
   SY(__NR__newselect, x0+8, x0+0xffffffff, x0+1, x0+1, x0+1);

   // __NR_flock 143
   GO(__NR_flock, "2s 0m");
   SY(__NR_flock, x0, x0);

   // __NR_msync 144
   GO(__NR_msync, "3s 0m");
   SY(__NR_msync, x0, x0, x0);

   // __NR_readv 145
 //GO(__NR_readv, ".s .m");
 //SY(__NR_readv);

   // __NR_writev 146
 //GO(__NR_writev, ".s .m");
 //SY(__NR_writev);

   // __NR_getsid 147
   GO(__NR_getsid, "1s 0m");
   SY(__NR_getsid, x0);

   // __NR_fdatasync 148
   GO(__NR_fdatasync, "1s 0m");
   SY(__NR_fdatasync, x0-1);

   // __NR__sysctl 149
 //GO(__NR__sysctl, ".s .m");
 //SY(__NR__sysctl);

   // __NR_mlock 150
   GO(__NR_mlock, "2s 0m");
   SY(__NR_mlock, x0, x0);

   // __NR_munlock 151
   GO(__NR_munlock, "2s 0m");
   SY(__NR_munlock, x0, x0);

   // __NR_mlockall 152
   GO(__NR_mlockall, "2s 0m");
   SY(__NR_mlockall, x0, x0);

   // __NR_munlockall 153
   GO(__NR_munlockall, "0s 0m");
   SY(__NR_munlockall);

   // __NR_sched_setparam 154
   GO(__NR_sched_setparam, "2s 1m");
   SY(__NR_sched_setparam, x0, x0);

   // __NR_sched_getparam 155
   GO(__NR_sched_getparam, "2s 1m");
   SY(__NR_sched_getparam, x0, x0);

   // __NR_sched_setscheduler 156
   GO(__NR_sched_setscheduler, "3s 1m");
   SY(__NR_sched_setscheduler, x0, x0, x0+1);

   // __NR_sched_getscheduler 157
   GO(__NR_sched_getscheduler, "1s 0m");
   SY(__NR_sched_getscheduler, x0);

   // __NR_sched_yield 158
 //GO(__NR_sched_yield, ".s .m");
 //SY(__NR_sched_yield);

   // __NR_sched_get_priority_max 159
   GO(__NR_sched_get_priority_max, "1s 0m");
   SY(__NR_sched_get_priority_max, x0);

   // __NR_sched_get_priority_min 160
   GO(__NR_sched_get_priority_min, "1s 0m");
   SY(__NR_sched_get_priority_min, x0);

   // __NR_sched_rr_get_interval 161
   GO(__NR_sched_rr_get_interval, "n/a");
 //SY(__NR_sched_rr_get_interval); // (Not yet handled by Valgrind)

   // __NR_nanosleep 162
   GO(__NR_nanosleep, "2s 2m");
   SY(__NR_nanosleep, x0, x0+1);

   // __NR_mremap 163
 //GO(__NR_mremap, ".s .m");
 //SY(__NR_mremap);

   // __NR_setresuid 164
   GO(__NR_setresuid, "3s 0m");
   SY(__NR_setresuid, x0, x0, x0);

   // __NR_getresuid 165
   GO(__NR_getresuid, "3s 3m");
   SY(__NR_getresuid, x0, x0, x0);

   // __NR_vm86 166
   GO(__NR_vm86, "n/a");
   // (will probably never be handled by Valgrind)

   // __NR_query_module 167
   GO(__NR_query_module, "ni");
   SY(__NR_query_module);

   // __NR_poll 168
   GO(__NR_poll, "3s 1m");
   SY(__NR_poll, x0, x0+1, x0);

   // __NR_nfsservctl 169
   GO(__NR_nfsservctl, "n/a");
 //SY(__NR_nfsservctl); // (Not yet handled by Valgrind)

   // __NR_setresgid 170
   GO(__NR_setresgid, "3s 0m");
   SY(__NR_setresgid, x0, x0, x0);

   // __NR_getresgid 171
   GO(__NR_getresgid, "3s 3m");
   SY(__NR_getresgid, x0, x0, x0);

   // __NR_prctl              172
 //GO(__NR_prctl, ".s .m");
 //SY(__NR_prctl);

   // __NR_rt_sigreturn 173
   GO(__NR_rt_sigreturn, "n/a");
 //SY(__NR_rt_sigreturn); // (Not yet handled by Valgrind)

   // __NR_rt_sigaction 174
   GO(__NR_rt_sigaction, "4s 2m");
   SY(__NR_rt_sigaction, x0, x0+1, x0+1, x0);

   // __NR_rt_sigprocmask 175
 //GO(__NR_rt_sigprocmask, ".s .m");
 //SY(__NR_rt_sigprocmask);

   // __NR_rt_sigpending 176
 //GO(__NR_rt_sigpending, ".s .m");
 //SY(__NR_rt_sigpending);

   // __NR_rt_sigtimedwait 177
 //GO(__NR_rt_sigtimedwait, ".s .m");
 //SY(__NR_rt_sigtimedwait);

   // __NR_rt_sigqueueinfo 178
 //GO(__NR_rt_sigqueueinfo, ".s .m");
 //SY(__NR_rt_sigqueueinfo);

   // __NR_rt_sigsuspend 179
 //GO(__NR_rt_sigsuspend, ".s .m");
 //SY(__NR_rt_sigsuspend);

   // __NR_pread64 180
 //GO(__NR_pread64, ".s .m");
 //SY(__NR_pread64);

   // __NR_pwrite64 181
 //GO(__NR_pwrite64, ".s .m");
 //SY(__NR_pwrite64);

   // __NR_chown 182
   GO(__NR_chown, "3s 1m");
   SY(__NR_chown, x0, x0, x0);

   // __NR_getcwd 183
 //GO(__NR_getcwd, ".s .m");
 //SY(__NR_getcwd);

   // __NR_capget 184
   GO(__NR_capget, "2s 2m");
   SY(__NR_capget, x0, x0);

   // __NR_capset 185
   GO(__NR_capset, "2s 2m");
   SY(__NR_capset, x0, x0);

   // __NR_sigaltstack 186
 //GO(__NR_sigaltstack, ".s .m");
 //SY(__NR_sigaltstack);

   // __NR_sendfile 187
 //GO(__NR_sendfile, ".s .m");
 //SY(__NR_sendfile);

   // __NR_getpmsg 188
 //GO(__NR_getpmsg, ".s .m");
 //SY(__NR_getpmsg);

   // __NR_putpmsg 189
 //GO(__NR_putpmsg, ".s .m");
 //SY(__NR_putpmsg);

   // __NR_vfork 190
   GO(__NR_vfork, "0s 0m");
   // (sse scalar_vfork.c)

   // __NR_ugetrlimit 191
   GO(__NR_ugetrlimit, "2s 1m");
   SY(__NR_ugetrlimit, x0, x0);

   // __NR_mmap2 192
   GO(__NR_mmap2, "5s 0m");
   SY(__NR_mmap2, x0, x0, x0, x0, x0-1, x0);

   // __NR_truncate64 193
   GO(__NR_truncate64, "3s 1m");
   SY(__NR_truncate64, x0, x0, x0);

   // __NR_ftruncate64 194
   GO(__NR_ftruncate64, "3s 0m");
   SY(__NR_ftruncate64, x0, x0, x0);

   // __NR_stat64 195
   GO(__NR_stat64, "2s 2m");
   SY(__NR_stat64, x0, x0);

   // __NR_lstat64 196
   GO(__NR_lstat64, "2s 2m");
   SY(__NR_lstat64, x0, x0);

   // __NR_fstat64 197
   GO(__NR_fstat64, "2s 1m");
   SY(__NR_fstat64, x0, x0);

   // __NR_lchown32 198
   GO(__NR_lchown32, "3s 1m");
   SY(__NR_lchown32, x0, x0, x0);

   // __NR_getuid32 199
   GO(__NR_getuid32, "0s 0m");
   SY(__NR_getuid32);

   // __NR_getgid32 200
   GO(__NR_getgid32, "0s 0m");
   SY(__NR_getgid32);

   // __NR_geteuid32 201
   GO(__NR_geteuid32, "0s 0m");
   SY(__NR_geteuid32);

   // __NR_getegid32 202
   GO(__NR_getegid32, "0s 0m");
   SY(__NR_getegid32);

   // __NR_setreuid32 203
   GO(__NR_setreuid32, "2s 0m");
   SY(__NR_setreuid32, x0, x0);

   // __NR_setregid32 204
   GO(__NR_setregid32, "2s 0m");
   SY(__NR_setregid32, x0, x0);

   // __NR_getgroups32 205
   GO(__NR_getgroups32, "2s 1m");
   SY(__NR_getgroups32, x0+1, x0+1);

   // __NR_setgroups32 206
   GO(__NR_setgroups32, "2s 1m");
   SY(__NR_setgroups32, x0+1, x0+1);

   // __NR_fchown32 207
   GO(__NR_fchown32, "3s 0m");
   SY(__NR_fchown32, x0, x0, x0);

   // __NR_setresuid32 208
   GO(__NR_setresuid32, "3s 0m");
   SY(__NR_setresuid32, x0, x0, x0);

   // __NR_getresuid32 209
   GO(__NR_getresuid32, "3s 3m");
   SY(__NR_getresuid32, x0, x0, x0);

   // __NR_setresgid32 210
   GO(__NR_setresgid32, "3s 0m");
   SY(__NR_setresgid32, x0, x0, x0);

   // __NR_getresgid32 211
   GO(__NR_getresgid32, "3s 3m");
   SY(__NR_getresgid32, x0, x0, x0);

   // __NR_chown32 212
   GO(__NR_chown32, "3s 1m");
   SY(__NR_chown32, x0, x0, x0);

   // __NR_setuid32 213
   GO(__NR_setuid32, "1s 0m");
   SY(__NR_setuid32, x0);

   // __NR_setgid32 214
   GO(__NR_setgid32, "1s 0m");
   SY(__NR_setgid32, x0);

   // __NR_setfsuid32 215
   GO(__NR_setfsuid32, "1s 0m");
   SY(__NR_setfsuid32, x0);

   // __NR_setfsgid32 216
   GO(__NR_setfsgid32, "1s 0m");
   SY(__NR_setfsgid32, x0);

   // __NR_pivot_root 217
   GO(__NR_pivot_root, "n/a");
 //SY(__NR_pivot_root); // (Not yet handled by Valgrind)

   // __NR_mincore 218
 //GO(__NR_mincore, ".s .m");
 //SY(__NR_mincore);

   // __NR_madvise 219
 //GO(__NR_madvise, ".s .m");
 //SY(__NR_madvise);

   // __NR_getdents64 220
   GO(__NR_getdents64, "3s 1m");
   SY(__NR_getdents64, x0, x0, x0+1);

   // __NR_fcntl64 221
   GO(__NR_fcntl64, "3s 0m");
   SY(__NR_fcntl64, x0, x0, x0);

   // 222
   GO(222, "ni");
   SY(222);

   // 223
   GO(223, "ni");
   SY(223);

   // __NR_gettid 224
   GO(__NR_gettid, "n/a");
 //SY(__NR_gettid); // (Not yet handled by Valgrind)

   // __NR_readahead 225
   GO(__NR_readahead, "n/a");
 //SY(__NR_readahead); // (Not yet handled by Valgrind)

   // __NR_setxattr 226
   GO(__NR_setxattr, "5s 3m");
   SY(__NR_setxattr, x0, x0, x0, x0+1, x0);

   // __NR_lsetxattr 227
   GO(__NR_lsetxattr, "5s 3m");
   SY(__NR_lsetxattr, x0, x0, x0, x0+1, x0);

   // __NR_fsetxattr 228
   GO(__NR_fsetxattr, "5s 2m");
   SY(__NR_fsetxattr, x0, x0, x0, x0+1, x0);

   // __NR_getxattr 229
   GO(__NR_getxattr, "4s 3m");
   SY(__NR_getxattr, x0, x0, x0, x0+1);

   // __NR_lgetxattr 230
   GO(__NR_lgetxattr, "4s 3m");
   SY(__NR_lgetxattr, x0, x0, x0, x0+1);

   // __NR_fgetxattr 231
   GO(__NR_fgetxattr, "4s 2m");
   SY(__NR_fgetxattr, x0, x0, x0, x0+1);

   // __NR_listxattr 232
   GO(__NR_listxattr, "3s 2m");
   SY(__NR_listxattr, x0, x0, x0+1);

   // __NR_llistxattr 233
   GO(__NR_llistxattr, "3s 2m");
   SY(__NR_llistxattr, x0, x0, x0+1);

   // __NR_flistxattr 234
   GO(__NR_flistxattr, "3s 1m");
   SY(__NR_flistxattr, x0, x0, x0+1);

   // __NR_removexattr 235
   GO(__NR_removexattr, "2s 2m");
   SY(__NR_removexattr, x0, x0);

   // __NR_lremovexattr 236
   GO(__NR_lremovexattr, "2s 2m");
   SY(__NR_lremovexattr, x0, x0);

   // __NR_fremovexattr 237
   GO(__NR_fremovexattr, "2s 1m");
   SY(__NR_fremovexattr, x0, x0);

   // __NR_tkill 238
   GO(__NR_tkill, "n/a");
 //SY(__NR_tkill); // (Not yet handled by Valgrind)

   // __NR_sendfile64 239
 //GO(__NR_sendfile64, ".s .m");
 //SY(__NR_sendfile64);

   // __NR_futex 240
 //GO(__NR_futex, ".s .m");
 //SY(__NR_futex);

   // __NR_sched_setaffinity 241
   GO(__NR_sched_setaffinity, "3s 1m");
   SY(__NR_sched_setaffinity, x0, x0+1, x0);

   // __NR_sched_getaffinity 242
   GO(__NR_sched_getaffinity, "3s 1m");
   SY(__NR_sched_getaffinity, x0, x0+1, x0);

   // __NR_set_thread_area 243
 //GO(__NR_set_thread_area, ".s .m");
 //SY(__NR_set_thread_area);

   // __NR_get_thread_area 244
 //GO(__NR_get_thread_area, ".s .m");
 //SY(__NR_get_thread_area);

   // __NR_io_setup 245
 //GO(__NR_io_setup, ".s .m");
 //SY(__NR_io_setup);

   // __NR_io_destroy 246
 //GO(__NR_io_destroy, ".s .m");
 //SY(__NR_io_destroy);

   // __NR_io_getevents 247
 //GO(__NR_io_getevents, ".s .m");
 //SY(__NR_io_getevents);

   // __NR_io_submit 248
 //GO(__NR_io_submit, ".s .m");
 //SY(__NR_io_submit);

   // __NR_io_cancel 249
 //GO(__NR_io_cancel, ".s .m");
 //SY(__NR_io_cancel);

   // __NR_fadvise64 250
   GO(__NR_fadvise64, "n/a");
 //SY(__NR_fadvise64); // (Not yet handled by Valgrind)

   // 251
   GO(251, "ni");
   SY(251);

   // __NR_exit_group 252
   GO(__NR_exit_group, "other");
   // (see scalar_exit_group.c)

   // __NR_lookup_dcookie 253
   GO(__NR_lookup_dcookie, "4s 1m");
   SY(__NR_lookup_dcookie, x0, x0, x0, x0+1);

   // __NR_epoll_create 254
   GO(__NR_epoll_create, "1s 0m");
   SY(__NR_epoll_create, x0);

   // __NR_epoll_ctl 255
   GO(__NR_epoll_ctl, "4s 1m");
   SY(__NR_epoll_ctl, x0, x0, x0, x0);

   // __NR_epoll_wait 256
   GO(__NR_epoll_wait, "4s 1m");
   SY(__NR_epoll_wait, x0, x0, x0+1, x0);

   // __NR_remap_file_pages 257
 //GO(__NR_remap_file_pages, ".s .m");
 //SY(__NR_remap_file_pages);

   // __NR_set_tid_address 258
   GO(__NR_set_tid_address, "1s 0m");
   SY(__NR_set_tid_address, x0);

   // __NR_timer_create 259
 //GO(__NR_timer_create, ".s .m");
 //SY(__NR_timer_create);

   // __NR_timer_settime (__NR_timer_create+1)
 //GO(__NR_timer_settime, ".s .m");
 //SY(__NR_timer_settime);

   // __NR_timer_gettime (__NR_timer_create+2)
 //GO(__NR_timer_gettime, ".s .m");
 //SY(__NR_timer_gettime);

   // __NR_timer_getoverrun (__NR_timer_create+3)
 //GO(__NR_timer_getoverrun, ".s .m");
 //SY(__NR_timer_getoverrun);

   // __NR_timer_delete (__NR_timer_create+4)
 //GO(__NR_timer_delete, ".s .m");
 //SY(__NR_timer_delete);

   // __NR_clock_settime (__NR_timer_create+5)
 //GO(__NR_clock_settime, ".s .m");
 //SY(__NR_clock_settime);

   // __NR_clock_gettime (__NR_timer_create+6)
 //GO(__NR_clock_gettime, ".s .m");
 //SY(__NR_clock_gettime);

   // __NR_clock_getres (__NR_timer_create+7)
 //GO(__NR_clock_getres, ".s .m");
 //SY(__NR_clock_getres);

   // __NR_clock_nanosleep (__NR_timer_create+8)
 //GO(__NR_clock_nanosleep, ".s .m");
 //SY(__NR_clock_nanosleep);

   // __NR_statfs64 268
   GO(__NR_statfs64, "3s 2m");
   SY(__NR_statfs64, x0, x0+1, x0);

   // __NR_fstatfs64 269
   GO(__NR_fstatfs64, "3s 1m");
   SY(__NR_fstatfs64, x0, x0+1, x0);

   // __NR_tgkill 270
   GO(__NR_tgkill, "n/a");
 //SY(__NR_tgkill); // (Not yet handled by Valgrind)

   // __NR_utimes 271
 //GO(__NR_utimes, ".s .m");
 //SY(__NR_utimes);

   // __NR_fadvise64_64 272
   GO(__NR_fadvise64_64, "n/a");
 //SY(__NR_fadvise64_64); // (Not yet handled by Valgrind)

   // __NR_vserver 273
   GO(__NR_vserver, "ni");
   SY(__NR_vserver);

   // __NR_mbind 274
   GO(__NR_mbind, "n/a");
 //SY(__NR_mbind); // (Not yet handled by Valgrind)

   // __NR_get_mempolicy 275
   GO(__NR_get_mempolicy, "n/a");
 //SY(__NR_get_mempolicy); // (Not yet handled by Valgrind)

   // __NR_set_mempolicy 276
   GO(__NR_set_mempolicy, "n/a");
 //SY(__NR_set_mempolicy); // (Not yet handled by Valgrind)

   // __NR_mq_open 277
 //GO(__NR_mq_open, ".s .m");
 //SY(__NR_mq_open);

   // __NR_mq_unlink (__NR_mq_open+1)
 //GO(__NR_mq_unlink, ".s .m");
 //SY(__NR_mq_unlink);

   // __NR_mq_timedsend (__NR_mq_open+2)
 //GO(__NR_mq_timedsend, ".s .m");
 //SY(__NR_mq_timedsend);

   // __NR_mq_timedreceive (__NR_mq_open+3)
 //GO(__NR_mq_timedreceive, ".s .m");
 //SY(__NR_mq_timedreceive);

   // __NR_mq_notify (__NR_mq_open+4)
 //GO(__NR_mq_notify, ".s .m");
 //SY(__NR_mq_notify);

   // __NR_mq_getsetattr (__NR_mq_open+5)
 //GO(__NR_mq_getsetattr, ".s .m");
 //SY(__NR_mq_getsetattr);
   
   // __NR_sys_kexec_load 283
   GO(__NR_sys_kexec_load, "ni");
   SY(__NR_sys_kexec_load);

   // no such syscall...
   GO(9999, "1e");
   SY(9999);

   // __NR_exit 1
   GO(__NR_exit, "1s 0m");
   SY(__NR_exit, x0);

   assert(0);
}

