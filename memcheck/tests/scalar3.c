
#include "scalar.h"

int main(void)
{
   // uninitialised, but we know px[0] is 0x0
   long* px  = malloc(sizeof(long));
   long  x0  = px[0];

   // All __NR_xxx numbers are taken from x86

   // 100--149
   
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

   // __NR_mprotect 125
 //GO(__NR_mprotect, ".s .m");
 //SY(__NR_mprotect);

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

   // __NR_getdents 141
 //GO(__NR_getdents, ".s .m");
 //SY(__NR_getdents);

   // __NR__newselect 142 --> sys_select()
   GO(__NR__newselect, "5s 4m");
   SY(__NR__newselect, x0+8, x0+0xffffffff, x0+1, x0+1, x0+1);

   // __NR_flock 143
 //GO(__NR_flock, ".s .m");
 //SY(__NR_flock);

   // __NR_msync 144
 //GO(__NR_msync, ".s .m");
 //SY(__NR_msync);

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

   return 0;
}

