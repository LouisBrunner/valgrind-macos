
#include "scalar.h"

int main(void)
{
   // uninitialised, but we know px[0] is 0x0
   long* px  = malloc(sizeof(long));
   long  x0  = px[0];

   // All __NR_xxx numbers are taken from x86

   // 150--199
   
   // __NR_mlock 150
 //GO(__NR_mlock, ".s .m");
 //SY(__NR_mlock);

   // __NR_munlock 151
 //GO(__NR_munlock, ".s .m");
 //SY(__NR_munlock);

   // __NR_mlockall 152
 //GO(__NR_mlockall, ".s .m");
 //SY(__NR_mlockall);

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

   // __NR_truncate64 193
 //GO(__NR_truncate64, ".s .m");
 //SY(__NR_truncate64);

   // __NR_ftruncate64 194
 //GO(__NR_ftruncate64, ".s .m");
 //SY(__NR_ftruncate64);

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

   return 0;
}

