
#include "scalar.h"

int main(void)
{
   // uninitialised, but we know px[0] is 0x0
   long* px  = malloc(sizeof(long));
   long  x0  = px[0];

   // All __NR_xxx numbers are taken from x86

   // 50--99
   
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

   // __NR_symlink 83
 //GO(__NR_symlink, ".s .m");
 //SY(__NR_symlink);

   // __NR_oldlstat 84
   // (obsolete, not handled by Valgrind)

   // __NR_readlink 85
 //GO(__NR_readlink, ".s .m");
 //SY(__NR_readlink);

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

   // __NR_munmap 91
 //GO(__NR_munmap, ".s .m");
 //SY(__NR_munmap);

   // __NR_truncate 92
 //GO(__NR_truncate, ".s .m");
 //SY(__NR_truncate);

   // __NR_ftruncate 93
 //GO(__NR_ftruncate, ".s .m");
 //SY(__NR_ftruncate);

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

   return 0;
}

