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

   // __NR_exit 1 --> sys_exit()
   GO(__NR_exit, "1s 0m");
   SY(__NR_exit, x0);

   assert(0);
}

