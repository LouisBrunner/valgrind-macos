#include <assert.h>
#include <fcntl.h>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <sys/stat.h>
#include <sys/syscall.h>
#include <sys/types.h>

// Thorough syscall scalar arg checking.  Also serves as thorough checking
// for (very) basic syscall use.  Generally not trying to do anything
// meaningful with the syscalls.

void GO(char* s) {
   fprintf(stderr, "---------- %s\n", s);
}

int main(void)
{
   // uninitialised, but we know pi[0] is 0x0
   int* pi  = malloc(sizeof(int));
   int  i0  = pi[0];

   // uninitialised, but we know pc[0] points to 0x0
   char** pc = malloc(sizeof(char*));
   char*  s0 = pc[0];

   // All __NR_xxx numbers are taken from x86
   
   // __NR_exit 1 
   // (see below)

   // __NR_fork 2

   // __NR_read 3 --> sys_read()
   // Nb: here we are also getting an error from the syscall arg itself.
   GO("__NR_read, 1+3 scalar errors");
   syscall(i0+__NR_read, i0, s0, i0);

   // __NR_write 4 --> sys_write()
   GO("__NR_write, 3 scalar errors, 1 memory error");
   syscall(__NR_write, i0, s0, i0+1);

   // __NR_open 5
   GO("__NR_open(2), 2 scalar errors, 1 memory error");
   syscall(__NR_open, s0, i0, i0+1);

   GO("__NR_open(3), 1 scalar error");
   syscall(__NR_open, "tmp_write_file_foo", O_CREAT, i0);

   // __NR_close 6
   // __NR_waitpid 7
   // __NR_creat 8
   // __NR_link 9
   // __NR_unlink 10
   // __NR_execve 11
   // __NR_chdir 12
   // __NR_time 13
   // __NR_mknod 14
   // __NR_chmod 15
   // __NR_lchown 16
   // __NR_break 17
   // __NR_oldstat 18
   // __NR_lseek 19
   // __NR_getpid 20 --> sys_getpid()
   GO("__NR_getpid, 0 errors");
   syscall(__NR_getpid);

   // __NR_mount 21
   GO("__NR_mount, 4 scalar errors, 3 memory errors");
   syscall(__NR_mount, s0, s0, s0, i0, s0);
   
   // __NR_umount 22
   // __NR_setuid 23
   // __NR_getuid 24
   // __NR_stime 25
   // __NR_ptrace 26
   // __NR_alarm 27
   // __NR_oldfstat 28
   // __NR_pause 29
   // __NR_utime 30
   // __NR_stty 31
   // __NR_gtty 32
   // __NR_access 33
   // __NR_nice 34
   // __NR_ftime 35
   // __NR_sync 36
   // __NR_kill 37
   // __NR_rename 38
   // __NR_mkdir 39
   // __NR_rmdir 40
   // __NR_dup 41
   // __NR_pipe 42
   // __NR_times 43
   // __NR_prof 44
   // __NR_brk 45
   // __NR_setgid 46
   // __NR_getgid 47
   // __NR_signal 48
   // __NR_geteuid 49
   // __NR_getegid 50
   // __NR_acct 51
   // __NR_umount2 52
   // __NR_lock 53
   // __NR_ioctl 54
   // __NR_fcntl 55
   // __NR_mpx 56
   // __NR_setpgid 57
   // __NR_ulimit 58
   // __NR_oldolduname 59
   // __NR_umask 60
   // __NR_chroot 61
   // __NR_ustat 62
   // __NR_dup2 63

   // __NR_getppid 64
   GO("__NR_getppid, 0 errors");
   syscall(__NR_getppid);

   // __NR_getpgrp 65
   // __NR_setsid 66
   // __NR_sigaction 67
   // __NR_sgetmask 68
   // __NR_ssetmask 69
   // __NR_setreuid 70
   // __NR_setregid 71
   // __NR_sigsuspend 72
   // __NR_sigpending 73
   // __NR_sethostname 74
   // __NR_setrlimit 75
   // __NR_getrlimit 76	/* Back compatible 2Gig limited rlimit */
   // __NR_getrusage 77
   // __NR_gettimeofday 78
   // __NR_settimeofday 79
   // __NR_getgroups 80
   // __NR_setgroups 81
   // __NR_select 82
   // __NR_symlink 83
   // __NR_oldlstat 84
   // __NR_readlink 85
   // __NR_uselib 86
   // __NR_swapon 87
   // __NR_reboot 88
   // __NR_readdir 89
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
   // __NR_iopl 110
   // __NR_vhangup 111
   // __NR_idle 112
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
   // __NR_modify_ldt 123
   // __NR_adjtimex 124
   // __NR_mprotect 125
   // __NR_sigprocmask 126
   // __NR_create_module 127
   // __NR_init_module 128
   // __NR_delete_module 129
   // __NR_get_kernel_syms 130
   // __NR_quotactl 131
   // __NR_getpgid 132
   // __NR_fchdir 133
   // __NR_bdflush 134
   // __NR_sysfs 135
   // __NR_personality 136
   // __NR_afs_syscall 137 /* Syscall for Andrew File System */
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
   // __NR_munlockall 153
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
   // __NR_query_module 167
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
   // __NR_getpmsg 188 /* some people actually want streams */
   // __NR_putpmsg 189 /* some people actually want streams */
   // __NR_vfork 190
   // __NR_ugetrlimit 191 /* SuS compliant getrlimit */
   // __NR_mmap2 192
   // __NR_truncate64 193
   // __NR_ftruncate64 194
   // __NR_stat64 195
   // __NR_lstat64 196
   // __NR_fstat64 197
   // __NR_lchown32 198
   // __NR_getuid32 199
   // __NR_getgid32 200
   // __NR_geteuid32 201
   // __NR_getegid32 202
   // __NR_setreuid32 203
   // __NR_setregid32 204
   // __NR_getgroups32 205
   // __NR_setgroups32 206
   // __NR_fchown32 207
   // __NR_setresuid32 208
   // __NR_getresuid32 209
   // __NR_setresgid32 210
   // __NR_getresgid32 211
   // __NR_chown32 212
   // __NR_setuid32 213
   // __NR_setgid32 214
   // __NR_setfsuid32 215
   // __NR_setfsgid32 216
   // __NR_pivot_root 217
   // __NR_mincore 218
   // __NR_madvise 219
   // __NR_madvise1 219 /* delete when C lib stub is removed */
   // __NR_getdents64 220
   // __NR_fcntl64 221
/* 223 is unused */
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

   // __NR_exit_group 252
   // __NR_lookup_dcookie 253
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
   // __NR_vserver 273
   // __NR_mbind 274
   // __NR_get_mempolicy 275
   // __NR_set_mempolicy 276
   // __NR_mq_open  277
   // __NR_mq_unlink (__NR_mq_open+1)
   // __NR_mq_timedsend (__NR_mq_open+2)
   // __NR_mq_timedreceive (__NR_mq_open+3)
   // __NR_mq_notify (__NR_mq_open+4)
   // __NR_mq_getsetattr (__NR_mq_open+5)
   // __NR_sys_kexec_load 283

   GO("bogus constant, 1 message");
   syscall(9999);

   // __NR_exit 1 --> sys_exit() 
   GO("__NR_exit, 1 scalar error");
   syscall(__NR_exit, i0);

   assert(0);
}

