#include "scalar.h"

int main(void)
{
   // uninitialised, but we know px[0] is 0x0
   long* px  = malloc(sizeof(long));
   long  x0  = px[0];

   // All __NR_xxx numbers are taken from x86
   
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
   SY(__NR_waitpid, x0-1);

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
   SY(__NR_setgid);

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

   // __NR_sigpending 73 --> sys_sigpending()
   GO(__NR_sigpending, "1s 1m");
   SY(__NR_sigpending, x0);

   // __NR_sethostname 74
   // (Not yet handled by Valgrind)

   // __NR_setrlimit 75 --> sys_setrlimit()
   GO(__NR_setrlimit, "2s 1m");
   SY(__NR_setrlimit, x0, x0);

   // __NR_getrlimit 76
   // __NR_getrusage 77
   // __NR_gettimeofday 78
   // __NR_settimeofday 79
   // __NR_getgroups 80
   // __NR_setgroups 81
   // __NR_select 82
   // __NR_symlink 83

   // __NR_oldlstat 84
   // (obsolete, not handled by Valgrind)

   // __NR_readlink 85

   // __NR_uselib 86
   // (Not yet handled by Valgrind)

   // __NR_swapon 87
   // (Not yet handled by Valgrind)

   // __NR_reboot 88
   // (Not yet handled by Valgrind)

   // __NR_readdir 89
   // (superseded, not handled by Valgrind)

   // __NR_mmap 90
   // __NR_munmap 91
   // __NR_truncate 92
   // __NR_ftruncate 93
   // __NR_fchmod 94
   // __NR_fchown 95
   // __NR_getpriority 96
   // __NR_setpriority 97
   // __NR_profil 98
   // __NR_statfs 99
   // __NR_fstatfs 100
   // __NR_ioperm 101
   // __NR_socketcall 102
   // __NR_syslog 103
   // __NR_setitimer 104
   // __NR_getitimer 105
   // __NR_stat 106
   // __NR_lstat 107
   // __NR_fstat 108

   // __NR_olduname 109
   // (obsolete, not handled by Valgrind)

   // __NR_iopl 110

   // __NR_vhangup 111 --> sys_vhangup()
   GO(__NR_vhangup, "0e");
   SY(__NR_vhangup);
   
   // __NR_idle 112 --> sys_ni_syscall()
   GO(__NR_idle, "0e");
   SY(__NR_idle);

   // __NR_vm86old 113
   // __NR_wait4 114
   // __NR_swapoff 115
   // __NR_sysinfo 116
   // __NR_ipc 117
   // __NR_fsync 118
   // __NR_sigreturn 119
   // __NR_clone 120
   // __NR_setdomainname 121

   // __NR_uname 122
   GO(__NR_uname, "1s 1m");
   SY(__NR_uname, x0);

   // __NR_modify_ldt 123
   // __NR_adjtimex 124
   // __NR_mprotect 125
   // __NR_sigprocmask 126

   // __NR_create_module 127 --> sys_ni_syscall()
   GO(__NR_create_module, "0e");
   SY(__NR_create_module);

   // __NR_init_module 128
   // __NR_delete_module 129

   // __NR_get_kernel_syms 130 --> sys_ni_syscall()
   GO(__NR_get_kernel_syms, "0e");
   SY(__NR_get_kernel_syms);

   // __NR_quotactl 131
   // __NR_getpgid 132
   // __NR_fchdir 133
   // __NR_bdflush 134
   // __NR_sysfs 135
   // __NR_personality 136

   // __NR_afs_syscall 137 --> sys_ni_syscall()
   GO(__NR_afs_syscall, "0e");
   SY(__NR_afs_syscall);

   // __NR_setfsuid 138
   // __NR_setfsgid 139
   // __NR__llseek 140
   // __NR_getdents 141
   // __NR__newselect 142
   // __NR_flock 143
   // __NR_msync 144
   // __NR_readv 145
   // __NR_writev 146
   // __NR_getsid 147
   // __NR_fdatasync 148
   // __NR__sysctl 149
   // __NR_mlock 150
   // __NR_munlock 151
   // __NR_mlockall 152

   // __NR_munlockall 153 --> sys_munlockall()
   GO(__NR_munlockall, "0e");
   SY(__NR_munlockall);

   // __NR_sched_setparam 154
   // __NR_sched_getparam 155
   // __NR_sched_setscheduler 156
   // __NR_sched_getscheduler 157
   // __NR_sched_yield 158
   // __NR_sched_get_priority_max 159
   // __NR_sched_get_priority_min 160
   // __NR_sched_rr_get_interval 161
   // __NR_nanosleep 162
   // __NR_mremap 163
   // __NR_setresuid 164
   // __NR_getresuid 165
   // __NR_vm86 166

   // __NR_query_module 167 --> sys_ni_syscall()
   GO(__NR_query_module, "0e");
   SY(__NR_query_module);

   // __NR_poll 168
   // __NR_nfsservctl 169
   // __NR_setresgid 170
   // __NR_getresgid 171
   // __NR_prctl              172
   // __NR_rt_sigreturn 173
   // __NR_rt_sigaction 174
   // __NR_rt_sigprocmask 175
   // __NR_rt_sigpending 176
   // __NR_rt_sigtimedwait 177
   // __NR_rt_sigqueueinfo 178
   // __NR_rt_sigsuspend 179
   // __NR_pread64 180
   // __NR_pwrite64 181
   // __NR_chown 182
   // __NR_getcwd 183
   // __NR_capget 184
   // __NR_capset 185
   // __NR_sigaltstack 186
   // __NR_sendfile 187
   // __NR_getpmsg 188
   // __NR_putpmsg 189
   // __NR_vfork 190
   // __NR_ugetrlimit 191
   // __NR_mmap2 192
   // __NR_truncate64 193
   // __NR_ftruncate64 194
   // __NR_stat64 195
   // __NR_lstat64 196
   // __NR_fstat64 197
   // __NR_lchown32 198

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

   // __NR_getgroups32 205
   // __NR_setgroups32 206
   // __NR_fchown32 207
   // __NR_setresuid32 208
   // __NR_getresuid32 209
   // __NR_setresgid32 210
   // __NR_getresgid32 211
   // __NR_chown32 212

   // __NR_setuid32 213 --> sys_setuid()
   GO(__NR_setuid32, "1s 0m");
   SY(__NR_setuid32, x0);

   // __NR_setgid32 214
   GO(__NR_setgid32, "1s 0m");
   SY(__NR_setgid32);

   // __NR_setfsuid32 215
   // __NR_setfsgid32 216
   // __NR_pivot_root 217
   // __NR_mincore 218
   // __NR_madvise 219
   // __NR_getdents64 220
   // __NR_fcntl64 221

   // XXX: from here on in, not present in early 2.4 kernels

   // 222 --> sys_ni_syscall()
   GO(222, "0e");
   SY(222);

   // 223 --> sys_ni_syscall()
   GO(223, "0e");
   SY(223);

   // __NR_gettid 224
   // __NR_readahead 225
   // __NR_setxattr 226
   // __NR_lsetxattr 227
   // __NR_fsetxattr 228
   // __NR_getxattr 229
   // __NR_lgetxattr 230
   // __NR_fgetxattr 231
   // __NR_listxattr 232
   // __NR_llistxattr 233
   // __NR_flistxattr 234
   // __NR_removexattr 235
   // __NR_lremovexattr 236
   // __NR_fremovexattr 237
   // __NR_tkill 238
   // __NR_sendfile64 239
   // __NR_futex 240
   // __NR_sched_setaffinity 241
   // __NR_sched_getaffinity 242
   // __NR_set_thread_area 243
   // __NR_get_thread_area 244
   // __NR_io_setup 245
   // __NR_io_destroy 246
   // __NR_io_getevents 247
   // __NR_io_submit 248
   // __NR_io_cancel 249
   // __NR_fadvise64 250

   // 251 --> sys_ni_syscall()
   GO(251, "0e");
   SY(251);

   // __NR_exit_group 252

   // __NR_lookup_dcookie 253 --> sys_lookup_dcookie()
   GO(__NR_lookup_dcookie, "4s 1m");
   SY(__NR_lookup_dcookie, x0, x0, x0, x0+1);

   // __NR_epoll_create 254
   // __NR_epoll_ctl 255
   // __NR_epoll_wait 256
   // __NR_remap_file_pages 257
   // __NR_set_tid_address 258
   // __NR_timer_create 259
   // __NR_timer_settime (__NR_timer_create+1)
   // __NR_timer_gettime (__NR_timer_create+2)
   // __NR_timer_getoverrun (__NR_timer_create+3)
   // __NR_timer_delete (__NR_timer_create+4)
   // __NR_clock_settime (__NR_timer_create+5)
   // __NR_clock_gettime (__NR_timer_create+6)
   // __NR_clock_getres (__NR_timer_create+7)
   // __NR_clock_nanosleep (__NR_timer_create+8)
   // __NR_statfs64 268
   // __NR_fstatfs64 269
   // __NR_tgkill 270
   // __NR_utimes 271
   // __NR_fadvise64_64 272

   // __NR_vserver 273 --> sys_ni_syscall()
   //GO(__NR_vserver, "0e");
   //SY(__NR_vserver);

   // __NR_mbind 274
   // __NR_get_mempolicy 275
   // __NR_set_mempolicy 276
   // __NR_mq_open  277
   // __NR_mq_unlink (__NR_mq_open+1)
   // __NR_mq_timedsend (__NR_mq_open+2)
   // __NR_mq_timedreceive (__NR_mq_open+3)
   // __NR_mq_notify (__NR_mq_open+4)
   // __NR_mq_getsetattr (__NR_mq_open+5)
   
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

