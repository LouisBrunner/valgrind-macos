#include "scalar.h"

int main(void)
{
   // uninitialised, but we know px[0] is 0x0
   long* px  = malloc(sizeof(long));
   long  x0  = px[0];

   // All __NR_xxx numbers are taken from x86

   // 0--49
   
   // __NR_restart_syscall 1  XXX ???
   // (see below)

   // __NR_exit 1 
   // (see below)

   // __NR_fork 2

   // __NR_read 3 --> sys_read()
   // Nb: here we are also getting an error from the syscall arg itself.
   GO(__NR_read, "1+3s 1m");
   SY(__NR_read+x0, x0, x0, x0+1);

   // __NR_write 4 --> sys_write()
   GO(__NR_write, "3s 1m");
   SY(__NR_write, x0, x0, x0+1);

   // __NR_open 5 --> sys_open()
   GO(__NR_open, "(2-args) 2s 1m");
   SY(__NR_open, x0, x0, x0+1);

   GO(__NR_open, "(3-args) 1s 0m");
   SY(__NR_open, "tmp_write_file_foo", O_CREAT, x0);

   // __NR_close 6 --> sys_close()
   GO(__NR_close, "1s 0m");
   SY(__NR_close, x0-1);

   // __NR_waitpid 7 --> sys_waitpid()
   GO(__NR_waitpid, "3s 1m");
   SY(__NR_waitpid, x0, x0+1, x0);

   // __NR_creat 8 --> sys_creat()
   GO(__NR_creat, "2s 1m");
   SY(__NR_creat, x0, x0);

   // __NR_link 9 --> sys_link()
   GO(__NR_link, "2s 2m");
   SY(__NR_link, x0, x0);

   // __NR_unlink 10 --> sys_unlink()
   GO(__NR_unlink, "1s 1m");
   SY(__NR_unlink, x0);

   // __NR_execve 11 --> sys_execve()
   // Nb: could have 3 memory errors if we pass x0+1 as the 2nd and 3rd
   // args, except for bug #93174.
   GO(__NR_execve, "3s 1m");
   SY(__NR_execve, x0, x0, x0);

   // __NR_chdir 12 --> sys_chdir()
   GO(__NR_chdir, "1s 1m");
   SY(__NR_chdir, x0);

   // __NR_time 13 --> sys_time()
   GO(__NR_time, "1s 1m");
   SY(__NR_time, x0+1);

   // __NR_mknod 14 --> sys_mknod()
   GO(__NR_mknod, "3s 1m");
   SY(__NR_mknod, x0, x0, x0);

   // __NR_chmod 15 --> sys_chmod()
   GO(__NR_chmod, "2s 1m");
   SY(__NR_chmod, x0, x0);

   // __NR_lchown 16
   // (Not yet handled by Valgrind)

   // __NR_break 17 --> sys_ni_syscall()
   GO(__NR_break, "0e");
   SY(__NR_break);

   // __NR_oldstat 18
   // (obsolete, not handled by Valgrind)

   // __NR_lseek 19 --> sys_lseek()
   GO(__NR_lseek, "3s 0m");
   SY(__NR_lseek, x0, x0, x0);

   // __NR_getpid 20 --> sys_getpid()
   GO(__NR_getpid, "0s 0m");
   SY(__NR_getpid);

   // __NR_mount 21 --> sys_mount()
   GO(__NR_mount, "5s 3m");
   SY(__NR_mount, x0, x0, x0, x0, x0);
   
   // __NR_umount 22 --> sys_oldumount()
   GO(__NR_umount, "1s 1m");
   SY(__NR_umount, x0);

   // __NR_setuid 23 --> sys_setuid16()
   GO(__NR_setuid, "1s 0m");
   SY(__NR_setuid, x0);

   // __NR_getuid 24 --> sys_getuid16()
   GO(__NR_getuid, "0e");
   SY(__NR_getuid);

   // __NR_stime 25
   // (Not yet handled by Valgrind)

   // __NR_ptrace 26 --> arch/sys_ptrace()
   // XXX: memory pointed to be arg3 is never checked...
   GO(__NR_ptrace, "4s 2m");
   SY(__NR_ptrace, x0+PTRACE_GETREGS, x0, x0, x0);

   // __NR_alarm 27 --> sys_alarm()
   GO(__NR_alarm, "1s 0m");
   SY(__NR_alarm, x0);

   // __NR_oldfstat 28
   // (obsolete, not handled by Valgrind)

   // __NR_pause 29 --> sys_pause()
   // XXX: will have to be tested separately

   // __NR_utime 30 --> sys_utime()
   GO(__NR_utime, "2s 2m");
   SY(__NR_utime, x0, x0+1);

   // __NR_stty 31 --> sys_ni_syscall()
   GO(__NR_stty, "0e");
   SY(__NR_stty);

   // __NR_gtty 32 --> sys_ni_syscall()
   GO(__NR_gtty, "0e");
   SY(__NR_gtty);

   // __NR_access 33 --> sys_access()
   GO(__NR_access, "2s 1m");
   SY(__NR_access, x0, x0);

   // __NR_nice 34 --> sys_nice()
   GO(__NR_nice, "1s 0m");
   SY(__NR_nice, x0);

   // __NR_ftime 35 --> sys_ni_syscall()
   GO(__NR_ftime, "0e");
   SY(__NR_ftime);

   // __NR_sync 36 --> sys_sync()
   GO(__NR_sync, "0e");
   SY(__NR_sync);

   // __NR_kill 37 --> sys_kill()
   GO(__NR_kill, "2s 0m");
   SY(__NR_kill, x0, x0);

   // __NR_rename 38 --> sys_rename()
   GO(__NR_rename, "2s 2m");
   SY(__NR_rename, x0, x0);

   // __NR_mkdir 39 --> sys_mkdir()
   GO(__NR_mkdir, "2s 1m");
   SY(__NR_mkdir, x0, x0);

   // __NR_rmdir 40 --> sys_rmdir()
   GO(__NR_rmdir, "1s 1m");
   SY(__NR_rmdir, x0);

   // __NR_dup 41 --> sys_dup()
   GO(__NR_dup, "1s 0m");
   SY(__NR_dup, x0);

   // __NR_pipe 42 --> arch/sys_pipe()
   GO(__NR_pipe, "1s 1m");
   SY(__NR_pipe, x0);

   // __NR_times 43 --> sys_times()
   GO(__NR_times, "1s 1m");
   SY(__NR_times, x0);

   // __NR_prof 44 --> sys_ni_syscall()
   GO(__NR_prof, "0e");
   SY(__NR_prof);

   // __NR_brk 45 --> sys_brk()
   GO(__NR_brk, "1s 0m");
   SY(__NR_brk, x0);

   // __NR_setgid 46 --> sys_setgid16()
   GO(__NR_setgid, "1s 0m");
   SY(__NR_setgid, x0);

   // __NR_getgid 47 --> sys_getgid16()
   GO(__NR_getgid, "0e");
   SY(__NR_getgid);

   // __NR_signal 48
   // (Not yet handled by Valgrind)

   // __NR_geteuid 49 --> sys_geteuid16()
   GO(__NR_geteuid, "0e");
   SY(__NR_geteuid);

   // __NR_getegid 50 --> sys_getegid16()
   GO(__NR_getegid, "0e");
   SY(__NR_getegid);

   // __NR_acct 51 --> sys_acct()
   GO(__NR_acct, "1s 1m");
   SY(__NR_acct, x0);

   // __NR_umount2 52 --> sys_umount()
   GO(__NR_umount2, "2s 1m");
   SY(__NR_umount2, x0, x0);

   // __NR_lock 53 --> sys_ni_syscall()
   GO(__NR_lock, "0e");
   SY(__NR_lock);

   // __NR_ioctl 54 --> sys_ioctl()
   #include <asm/ioctls.h>
   GO(__NR_ioctl, "3s 1m");
   SY(__NR_ioctl, x0, x0+TCSETS, x0);

   // __NR_fcntl 55 --> sys_fcntl()
   GO(__NR_fcntl, "3s 0m");
   SY(__NR_fcntl, x0, x0, x0);

   // __NR_mpx 56 --> sys_ni_syscall()
   GO(__NR_mpx, "0e");
   SY(__NR_mpx);

   // __NR_setpgid 57
   GO(__NR_setpgid, "2s 0m");
   SY(__NR_setpgid, x0, x0);

   // __NR_ulimit 58 --> sys_ni_syscall()
   GO(__NR_ulimit, "0e");
   SY(__NR_ulimit);

   // __NR_oldolduname 59
   // (obsolete, not handled by Valgrind)

   // __NR_umask 60
   GO(__NR_umask, "1s 0m");
   SY(__NR_umask, x0);

   // __NR_chroot 61
   GO(__NR_chroot, "1s 1m");
   SY(__NR_chroot, x0);

   // __NR_ustat 62
   // (deprecated, not handled by Valgrind)

   // __NR_dup2 63
   GO(__NR_dup2, "2s 0m");
   SY(__NR_dup2, x0, x0);

   // __NR_getppid 64 --> sys_getppid()
   GO(__NR_getppid, "0e");
   SY(__NR_getppid);

   // __NR_getpgrp 65 --> sys_getpgrp()
   GO(__NR_getpgrp, "0e");
   SY(__NR_getpgrp);

   // __NR_setsid 66 --> sys_setsid()
   GO(__NR_setsid, "0e");
   SY(__NR_setsid);

   // __NR_sigaction 67 --> sys_sigaction()
   GO(__NR_sigaction, "3s 2m");
   SY(__NR_sigaction, x0, x0+1, x0+1);

   // __NR_sgetmask 68
   // (Not yet handled by Valgrind)

   // __NR_ssetmask 69
   // (Not yet handled by Valgrind)

   // __NR_setreuid 70 --> sys_setreuid16()
   GO(__NR_setreuid, "2s 0m");
   SY(__NR_setreuid, x0, x0);

   // __NR_setregid 71 --> sys_setregid16()
   GO(__NR_setregid, "2s 0m");
   SY(__NR_setregid, x0, x0);

   // __NR_sigsuspend 72 --> sys_sigsuspend()
   // XXX: how do you use this function?
// GO(__NR_sigsuspend, ".s .m");
// SY(__NR_sigsuspend);

   // __NR_sigpending 73 --> sys_sigpending()
   GO(__NR_sigpending, "1s 1m");
   SY(__NR_sigpending, x0);

   // __NR_sethostname 74
   // (Not yet handled by Valgrind)

   // __NR_setrlimit 75 --> sys_setrlimit()
   GO(__NR_setrlimit, "2s 1m");
   SY(__NR_setrlimit, x0, x0);

   // __NR_getrlimit 76
   GO(__NR_getrlimit, "2s 1m");
   SY(__NR_getrlimit, x0, x0);

   // __NR_getrusage 77
   GO(__NR_getrusage, "2s 1m");
   SY(__NR_getrusage, x0, x0);

   // __NR_gettimeofday 78 --> sys_gettimeofday()
   GO(__NR_gettimeofday, "2s 2m");
   SY(__NR_gettimeofday, x0, x0+1);

   // __NR_settimeofday 79 --> sys_settimeofday()
   GO(__NR_settimeofday, "2s 2m");
   SY(__NR_settimeofday, x0, x0+1);

   // __NR_getgroups 80 --> sys_getgroups16()
   GO(__NR_getgroups, "2s 1m");
   SY(__NR_getgroups, x0+1, x0+1);

   // __NR_setgroups 81 --> sys_setgroups16()
   GO(__NR_setgroups, "2s 1m");
   SY(__NR_setgroups, x0+1, x0+1);

   // __NR_select 82 --> old_select()
   {
      long args[5] = { x0+8, x0+0xffffffee, x0+1, x0+1, x0+1 };
      GO(__NR_select, "1s 4m");
      SY(__NR_select, args+x0);
   }

   // __NR_symlink 83 --> sys_symlink()
   GO(__NR_symlink, "2s 2m");
   SY(__NR_symlink, x0, x0);

   // __NR_oldlstat 84
   // (obsolete, not handled by Valgrind)

   // __NR_readlink 85
   GO(__NR_readlink, "3s 2m");
   SY(__NR_readlink);

   // __NR_uselib 86
   // (Not yet handled by Valgrind)

   // __NR_swapon 87
   // (Not yet handled by Valgrind)

   // __NR_reboot 88
   // (Not yet handled by Valgrind)

   // __NR_readdir 89
   // (superseded, not handled by Valgrind)

   // __NR_mmap 90
 //GO(__NR_mmap, ".s .m");
 //SY(__NR_mmap);

   // __NR_munmap 91 --> sys_munmap()
   GO(__NR_munmap, "2s 0m");
   SY(__NR_munmap, x0, x0);

   // __NR_truncate 92 --> sys_truncate()
   GO(__NR_truncate, "2s 1m");
   SY(__NR_truncate, x0, x0);

   // __NR_ftruncate 93 --> sys_ftruncate()
   GO(__NR_ftruncate, "2s 0m");
   SY(__NR_ftruncate, x0, x0);

   // __NR_fchmod 94
 //GO(__NR_fchmod, ".s .m");
 //SY(__NR_fchmod);

   // __NR_fchown 95 --> sys_fchown16
   GO(__NR_fchown, "3s 0m");
   SY(__NR_fchown, x0, x0, x0);

   // __NR_getpriority 96
 //GO(__NR_getpriority, ".s .m");
 //SY(__NR_getpriority);

   // __NR_setpriority 97
 //GO(__NR_setpriority, ".s .m");
 //SY(__NR_setpriority);

   // __NR_profil 98
 //GO(__NR_profil, ".s .m");
 //SY(__NR_profil);

   // __NR_statfs 99
 //GO(__NR_statfs, ".s .m");
 //SY(__NR_statfs);

   // __NR_fstatfs 100
 //GO(__NR_fstatfs, ".s .m");
 //SY(__NR_fstatfs);

   // __NR_ioperm 101
 //GO(__NR_ioperm, ".s .m");
 //SY(__NR_ioperm);

   // __NR_socketcall 102
 //GO(__NR_socketcall, ".s .m");
 //SY(__NR_socketcall);

   // __NR_syslog 103
 //GO(__NR_syslog, ".s .m");
 //SY(__NR_syslog);

   // __NR_setitimer 104
 //GO(__NR_setitimer, ".s .m");
 //SY(__NR_setitimer);

   // __NR_getitimer 105
 //GO(__NR_getitimer, ".s .m");
 //SY(__NR_getitimer);

   // __NR_stat 106 --> sys_newstat()
   GO(__NR_stat, "2s 2m");
   SY(__NR_stat, x0, x0);

   // __NR_lstat 107 --> sys_newlstat()
   GO(__NR_lstat, "2s 2m");
   SY(__NR_lstat, x0, x0);

   // __NR_fstat 108 --> sys_newfstat()
   GO(__NR_fstat, "2s 1m");
   SY(__NR_fstat, x0, x0);

   // __NR_olduname 109
   // (obsolete, not handled by Valgrind)

   // __NR_iopl 110
 //GO(__NR_iopl, ".s .m");
 //SY(__NR_iopl);

   // __NR_vhangup 111 --> sys_vhangup()
   GO(__NR_vhangup, "0e");
   SY(__NR_vhangup);
   
   // __NR_idle 112 --> sys_ni_syscall()
   GO(__NR_idle, "0e");
   SY(__NR_idle);

   // __NR_vm86old 113
 //GO(__NR_vm86old, ".s .m");
 //SY(__NR_vm86old);

   // __NR_wait4 114
 //GO(__NR_wait4, ".s .m");
 //SY(__NR_wait4);

   // __NR_swapoff 115
 //GO(__NR_swapoff, ".s .m");
 //SY(__NR_swapoff);

   // __NR_sysinfo 116
 //GO(__NR_sysinfo, ".s .m");
 //SY(__NR_sysinfo);

   // __NR_ipc 117
 //GO(__NR_ipc, ".s .m");
 //SY(__NR_ipc);

   // __NR_fsync 118
 //GO(__NR_fsync, ".s .m");
 //SY(__NR_fsync);

   // __NR_sigreturn 119
 //GO(__NR_sigreturn, ".s .m");
 //SY(__NR_sigreturn);

   // __NR_clone 120
 //GO(__NR_clone, ".s .m");
 //SY(__NR_clone);

   // __NR_setdomainname 121
 //GO(__NR_setdomainname, ".s .m");
 //SY(__NR_setdomainname);

   // __NR_uname 122
   GO(__NR_uname, "1s 1m");
   SY(__NR_uname, x0);

   // __NR_modify_ldt 123
 //GO(__NR_modify_ldt, ".s .m");
 //SY(__NR_modify_ldt);

   // __NR_adjtimex 124
 //GO(__NR_adjtimex, ".s .m");
 //SY(__NR_adjtimex);

   // __NR_mprotect 125 --> sys_mprotect()
   GO(__NR_mprotect, "3s 0m");
   SY(__NR_mprotect, x0, x0, x0);

   // __NR_sigprocmask 126
 //GO(__NR_sigprocmask, ".s .m");
 //SY(__NR_sigprocmask);

   // __NR_create_module 127 --> sys_ni_syscall()
   GO(__NR_create_module, "0e");
   SY(__NR_create_module);

   // __NR_init_module 128
 //GO(__NR_init_module, ".s .m");
 //SY(__NR_init_module);

   // __NR_delete_module 129
 //GO(__NR_delete_module, ".s .m");
 //SY(__NR_delete_module);

   // __NR_get_kernel_syms 130 --> sys_ni_syscall()
   GO(__NR_get_kernel_syms, "0e");
   SY(__NR_get_kernel_syms);

   // __NR_quotactl 131
 //GO(__NR_quotactl, ".s .m");
 //SY(__NR_quotactl);

   // __NR_getpgid 132
 //GO(__NR_getpgid, ".s .m");
 //SY(__NR_getpgid);

   // __NR_fchdir 133
 //GO(__NR_fchdir, ".s .m");
 //SY(__NR_fchdir);

   // __NR_bdflush 134
 //GO(__NR_bdflush, ".s .m");
 //SY(__NR_bdflush);

   // __NR_sysfs 135
 //GO(__NR_sysfs, ".s .m");
 //SY(__NR_sysfs);

   // __NR_personality 136
 //GO(__NR_personality, ".s .m");
 //SY(__NR_personality);

   // __NR_afs_syscall 137 --> sys_ni_syscall()
   GO(__NR_afs_syscall, "0e");
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

   // __NR_getdents 141 --> sys_getdents()
   GO(__NR_getdents, "3s 1m");
   SY(__NR_getdents, x0, x0, x0+1);

   // __NR__newselect 142 --> sys_select()
   GO(__NR__newselect, "5s 4m");
   SY(__NR__newselect, x0+8, x0+0xffffffff, x0+1, x0+1, x0+1);

   // __NR_flock 143
   GO(__NR_flock, "2s 0m");
   SY(__NR_flock, x0, x0);

   // __NR_msync 144 --> sys_msync()
   GO(__NR_msync, "3s 0m");
   SY(__NR_msync, x0, x0, x0);

   // __NR_readv 145
 //GO(__NR_readv, ".s .m");
 //SY(__NR_readv);

   // __NR_writev 146
 //GO(__NR_writev, ".s .m");
 //SY(__NR_writev);

   // __NR_getsid 147
 //GO(__NR_getsid, ".s .m");
 //SY(__NR_getsid);

   // __NR_fdatasync 148
 //GO(__NR_fdatasync, ".s .m");
 //SY(__NR_fdatasync);

   // __NR__sysctl 149
 //GO(__NR__sysctl, ".s .m");
 //SY(__NR__sysctl);

   // __NR_mlock 150 --> sys_mlock()
   GO(__NR_mlock, "2s 0m");
   SY(__NR_mlock, x0, x0);

   // __NR_munlock 151 --> sys_munlock()
   GO(__NR_munlock, "2s 0m");
   SY(__NR_munlock, x0, x0);

   // __NR_mlockall 152 --> sys_mlockall()
   GO(__NR_mlockall, "2s 0m");
   SY(__NR_mlockall);

   // __NR_munlockall 153 --> sys_munlockall()
   GO(__NR_munlockall, "0e");
   SY(__NR_munlockall);

   // __NR_sched_setparam 154
 //GO(__NR_sched_setparam, ".s .m");
 //SY(__NR_sched_setparam);

   // __NR_sched_getparam 155
 //GO(__NR_sched_getparam, ".s .m");
 //SY(__NR_sched_getparam);

   // __NR_sched_setscheduler 156
 //GO(__NR_sched_setscheduler, ".s .m");
 //SY(__NR_sched_setscheduler);

   // __NR_sched_getscheduler 157
 //GO(__NR_sched_getscheduler, ".s .m");
 //SY(__NR_sched_getscheduler);

   // __NR_sched_yield 158
 //GO(__NR_sched_yield, ".s .m");
 //SY(__NR_sched_yield);

   // __NR_sched_get_priority_max 159
 //GO(__NR_sched_get_priority_max, ".s .m");
 //SY(__NR_sched_get_priority_max);

   // __NR_sched_get_priority_min 160
 //GO(__NR_sched_get_priority_min, ".s .m");
 //SY(__NR_sched_get_priority_min);

   // __NR_sched_rr_get_interval 161
 //GO(__NR_sched_rr_get_interval, ".s .m");
 //SY(__NR_sched_rr_get_interval);

   // __NR_nanosleep 162
 //GO(__NR_nanosleep, ".s .m");
 //SY(__NR_nanosleep);

   // __NR_mremap 163
 //GO(__NR_mremap, ".s .m");
 //SY(__NR_mremap);

   // __NR_setresuid 164 --> sys_setresuid16()
   GO(__NR_setresuid, "3s 0m");
   SY(__NR_setresuid, x0, x0, x0);

   // __NR_getresuid 165 --> sys_getresuid16()
   GO(__NR_getresuid, "3s 3m");
   SY(__NR_getresuid, x0, x0, x0);

   // __NR_vm86 166
 //GO(__NR_vm86, ".s .m");
 //SY(__NR_vm86);

   // __NR_query_module 167 --> sys_ni_syscall()
   GO(__NR_query_module, "0e");
   SY(__NR_query_module);

   // __NR_poll 168
 //GO(__NR_poll, ".s .m");
 //SY(__NR_poll);

   // __NR_nfsservctl 169
 //GO(__NR_nfsservctl, ".s .m");
 //SY(__NR_nfsservctl);

   // __NR_setresgid 170 --> sys_setresgid16()
   GO(__NR_setresgid, "3s 0m");
   SY(__NR_setresgid, x0, x0, x0);

   // __NR_getresgid 171 --> sys_getresgid16()
   GO(__NR_getresgid, "3s 3m");
   SY(__NR_getresgid, x0, x0, x0);

   // __NR_prctl              172
 //GO(__NR_prctl, ".s .m");
 //SY(__NR_prctl);

   // __NR_rt_sigreturn 173
 //GO(__NR_rt_sigreturn, ".s .m");
 //SY(__NR_rt_sigreturn);

   // __NR_rt_sigaction 174 --> sys_rt_sigaction()
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

   // __NR_chown 182 --> sys_chown16()
   GO(__NR_chown, "3s 1m");
   SY(__NR_chown, x0, x0, x0);

   // __NR_getcwd 183
 //GO(__NR_getcwd, ".s .m");
 //SY(__NR_getcwd);

   // __NR_capget 184
 //GO(__NR_capget, ".s .m");
 //SY(__NR_capget);

   // __NR_capset 185
 //GO(__NR_capset, ".s .m");
 //SY(__NR_capset);

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
 //GO(__NR_vfork, ".s .m");
 //SY(__NR_vfork);

   // __NR_ugetrlimit 191
   GO(__NR_ugetrlimit, "2s 1m");
   SY(__NR_ugetrlimit, x0, x0);

   // __NR_mmap2 192
 //GO(__NR_mmap2, ".s .m");
 //SY(__NR_mmap2);

   // __NR_truncate64 193 --> sys_truncate64()
   GO(__NR_truncate64, "3s 1m");
   SY(__NR_truncate64, x0, x0, x0);

   // __NR_ftruncate64 194 --> sys_ftruncate64()
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

   // __NR_lchown32 198 --> sys_chown()
   GO(__NR_lchown32, "3s 1m");
   SY(__NR_lchown32, x0, x0, x0);

   // __NR_getuid32 199 --> sys_getuid()
   GO(__NR_getuid32, "0e");
   SY(__NR_getuid32);

   // __NR_getgid32 200 --> sys_getgid()
   GO(__NR_getgid32, "0e");
   SY(__NR_getgid32);

   // __NR_geteuid32 201 --> sys_geteuid()
   GO(__NR_geteuid32, "0e");
   SY(__NR_geteuid32);

   // __NR_getegid32 202 --> sys_getegid()
   GO(__NR_getegid32, "0e");
   SY(__NR_getegid32);

   // __NR_setreuid32 203
   GO(__NR_setreuid32, "2s 0m");
   SY(__NR_setreuid32, x0, x0);

   // __NR_setregid32 204
   GO(__NR_setregid32, "2s 0m");
   SY(__NR_setregid32, x0, x0);

   // __NR_getgroups32 205 --> sys_getgroups()
   GO(__NR_getgroups32, "2s 1m");
   SY(__NR_getgroups32, x0+1, x0+1);

   // __NR_setgroups32 206 --> sys_setgroups()
   GO(__NR_setgroups32, "2s 1m");
   SY(__NR_setgroups32, x0+1, x0+1);

   // __NR_fchown32 207 --> sys_fchown()
   GO(__NR_fchown32, "3s 0m");
   SY(__NR_fchown32, x0, x0, x0);

   // __NR_setresuid32 208 --> sys_setresuid()
   GO(__NR_setresuid32, "3s 0m");
   SY(__NR_setresuid32, x0, x0, x0);

   // __NR_getresuid32 209 --> sys_getresuid()
   GO(__NR_getresuid32, "3s 3m");
   SY(__NR_getresuid32, x0, x0, x0);

   // __NR_setresgid32 210 --> sys_setresgid()
   GO(__NR_setresgid32, "3s 0m");
   SY(__NR_setresgid32, x0, x0, x0);

   // __NR_getresgid32 211 --> sys_getresgid()
   GO(__NR_getresgid32, "3s 3m");
   SY(__NR_getresgid32, x0, x0, x0);

   // __NR_chown32 212 --> sys_chown()
   GO(__NR_chown32, "3s 1m");
   SY(__NR_chown32, x0, x0, x0);

   // __NR_setuid32 213 --> sys_setuid()
   GO(__NR_setuid32, "1s 0m");
   SY(__NR_setuid32, x0);

   // __NR_setgid32 214
   GO(__NR_setgid32, "1s 0m");
   SY(__NR_setgid32, x0);

   // __NR_setfsuid32 215 --> sys_setfsuid()
   GO(__NR_setfsuid32, "1s 0m");
   SY(__NR_setfsuid32, x0);

   // __NR_setfsgid32 216 --> sys_setfsgid()
   GO(__NR_setfsgid32, "1s 0m");
   SY(__NR_setfsgid32, x0);

   // __NR_pivot_root 217
 //GO(__NR_pivot_root, ".s .m");
 //SY(__NR_pivot_root);

   // __NR_mincore 218
 //GO(__NR_mincore, ".s .m");
 //SY(__NR_mincore);

   // __NR_madvise 219
 //GO(__NR_madvise, ".s .m");
 //SY(__NR_madvise);

   // __NR_getdents64 220 --> sys_getdents64
   GO(__NR_getdents64, "3s 1m");
   SY(__NR_getdents64, x0, x0, x0+1);

   // __NR_fcntl64 221
 //GO(__NR_fcntl64, ".s .m");
 //SY(__NR_fcntl64);

   // 222 --> sys_ni_syscall()
   GO(222, "0e");
   SY(222);

   // 223 --> sys_ni_syscall()
   GO(223, "0e");
   SY(223);

   // __NR_gettid 224
 //GO(__NR_gettid, ".s .m");
 //SY(__NR_gettid);

   // __NR_readahead 225
 //GO(__NR_readahead, ".s .m");
 //SY(__NR_readahead);

   // __NR_setxattr 226 --> sys_xattr()
   GO(__NR_setxattr, "5s 3m");
   SY(__NR_setxattr, x0, x0, x0, x0+1, x0);

   // __NR_lsetxattr 227 --> sys_lsetxattr()
   GO(__NR_lsetxattr, "5s 3m");
   SY(__NR_lsetxattr, x0, x0, x0, x0+1, x0);

   // __NR_fsetxattr 228 --> sys_fsetxattr()
   GO(__NR_fsetxattr, "5s 2m");
   SY(__NR_fsetxattr, x0, x0, x0, x0+1, x0);

   // __NR_getxattr 229 --> sys_getxattr()
   GO(__NR_getxattr, "4s 3m");
   SY(__NR_getxattr, x0, x0, x0, x0+1);

   // __NR_lgetxattr 230 --> sys_lgetxattr()
   GO(__NR_lgetxattr, "4s 3m");
   SY(__NR_lgetxattr, x0, x0, x0, x0+1);

   // __NR_fgetxattr 231 --> sys_fgetxattr()
   GO(__NR_fgetxattr, "4s 2m");
   SY(__NR_fgetxattr, x0, x0, x0, x0+1);

   // __NR_listxattr 232 --> sys_listxattr()
   GO(__NR_listxattr, "3s 2m");
   SY(__NR_listxattr, x0, x0, x0+1);

   // __NR_llistxattr 233 --> sys_llistxattr()
   GO(__NR_llistxattr, "3s 2m");
   SY(__NR_llistxattr, x0, x0, x0+1);

   // __NR_flistxattr 234 --> sys_flistxattr()
   GO(__NR_flistxattr, "3s 1m");
   SY(__NR_flistxattr, x0, x0, x0+1);

   // __NR_removexattr 235 --> sys_removexattr()
   GO(__NR_removexattr, "2s 2m");
   SY(__NR_removexattr, x0, x0);

   // __NR_lremovexattr 236 --> sys_lremovexattr()
   GO(__NR_lremovexattr, "2s 2m");
   SY(__NR_lremovexattr, x0, x0);

   // __NR_fremovexattr 237 --> sys_fremovexattr()
   GO(__NR_fremovexattr, "2s 1m");
   SY(__NR_fremovexattr, x0, x0);

   // __NR_tkill 238
 //GO(__NR_tkill, ".s .m");
 //SY(__NR_tkill);

   // __NR_sendfile64 239
 //GO(__NR_sendfile64, ".s .m");
 //SY(__NR_sendfile64);

   // __NR_futex 240
 //GO(__NR_futex, ".s .m");
 //SY(__NR_futex);

   // __NR_sched_setaffinity 241
 //GO(__NR_sched_setaffinity, ".s .m");
 //SY(__NR_sched_setaffinity);

   // __NR_sched_getaffinity 242
 //GO(__NR_sched_getaffinity, ".s .m");
 //SY(__NR_sched_getaffinity);

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
 //GO(__NR_fadvise64, ".s .m");
 //SY(__NR_fadvise64);

   // 251 --> sys_ni_syscall()
   GO(251, "0e");
   SY(251);

   // __NR_exit_group 252
   // See below

   // __NR_lookup_dcookie 253 --> sys_lookup_dcookie()
   GO(__NR_lookup_dcookie, "4s 1m");
   SY(__NR_lookup_dcookie, x0, x0, x0, x0+1);

   // __NR_epoll_create 254
 //GO(__NR_epoll_create, ".s .m");
 //SY(__NR_epoll_create);

   // __NR_epoll_ctl 255
 //GO(__NR_epoll_ctl, ".s .m");
 //SY(__NR_epoll_ctl);

   // __NR_epoll_wait 256
 //GO(__NR_epoll_wait, ".s .m");
 //SY(__NR_epoll_wait);

   // __NR_remap_file_pages 257
 //GO(__NR_remap_file_pages, ".s .m");
 //SY(__NR_remap_file_pages);

   // __NR_set_tid_address 258
 //GO(__NR_set_tid_address, ".s .m");
 //SY(__NR_set_tid_address);

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
 //GO(__NR_statfs64, ".s .m");
 //SY(__NR_statfs64);

   // __NR_fstatfs64 269
 //GO(__NR_fstatfs64, ".s .m");
 //SY(__NR_fstatfs64);

   // __NR_tgkill 270
 //GO(__NR_tgkill, ".s .m");
 //SY(__NR_tgkill);

   // __NR_utimes 271
 //GO(__NR_utimes, ".s .m");
 //SY(__NR_utimes);

   // __NR_fadvise64_64 272
 //GO(__NR_fadvise64_64, ".s .m");
 //SY(__NR_fadvise64_64);

   // __NR_vserver 273 --> sys_ni_syscall()
 //GO(__NR_vserver, "0e");
 //SY(__NR_vserver);

   // __NR_mbind 274
 //GO(__NR_mbind, ".s .m");
 //SY(__NR_mbind);

   // __NR_get_mempolicy 275
 //GO(__NR_get_mempolicy, ".s .m");
 //SY(__NR_get_mempolicy);

   // __NR_set_mempolicy 276
 //GO(__NR_set_mempolicy, ".s .m");
 //SY(__NR_set_mempolicy);

   // __NR_mq_open  277
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
   
   // __NR_sys_kexec_load 283 --> sys_ni_syscall()
 //GO(__NR_sys_kexec_load, "0e");
 //SY(__NR_sys_kexec_load);

   GO(9999, "1e");
   SY(9999);

   // __NR_exit 1 --> sys_exit()
   GO(__NR_exit, "1s 0m");
   SY(__NR_exit, x0);

   assert(0);
}

