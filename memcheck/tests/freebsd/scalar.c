/* Basic syscall test, see memcheck/tests/x86-linux/scalar.c for more info. */

#include <unistd.h>
#include <fcntl.h>
#include <sys/types.h>
#include <sys/ptrace.h>
#include <ufs/ufs/quota.h>
#include <machine/sysarch.h>
#include <sys/mman.h>
#include <sys/sem.h>
#include <sys/procctl.h>
#include <mqueue.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <poll.h>
#include "scalar.h"
#include "config.h"
#include "../../memcheck.h"

/* Helper functions.  These are necessary if we've got two tests for a single
   syscall.  In that case, Memcheck can sometimes merge error messages.  Doing
   each test in its own function prevents that. */


int main(void)
{
   /* Uninitialised, but we know px[0] is 0x0. */
   /* PJF why ? */
   long *px = malloc(2*sizeof(long));
   x0 = px[0];

   /* SYS_syscall                 0 */
   /* does this need a specific test? There are two diffeent IDs for syscall, see 198 */

   /* SYS_exit                    1 */
   /* obviously an exit syscall needs to be last */
   GO(SYS_exit, "below");

   /* SYS_fork                    2 */
    GO(SYS_fork, "other");

   /* SYS_read                    3 */
   GO(SYS_read, "1+3s 0m");
   SY(SYS_read+x0, 0, x0, x0 + 1); FAILx(EFAULT);

   /* SYS_write                   4 */
   GO(SYS_write, "3s 1m");
   SY(SYS_write, x0 + 2, x0, x0 + 1); FAIL;

   /* SYS_open                    5 */
   GO(SYS_open, "(2-args) 2s 1m");
   SY(SYS_open, x0, x0); FAIL;

   // Only 1s 0m errors -- the other 2s 1m have been checked in the previous
   // open test, and if we test them they may be commoned up but they also
   // may not.
   GO(SYS_open, "(3-args) 1s 0m");
   SY(SYS_open, "scalar.c", O_CREAT|O_EXCL, x0); FAIL;

   /* SYS_close                   6 */
   GO(SYS_close, "1s 0m");
   SY(SYS_close, x0-1); FAIL;

   /* SYS_wait4                   7 */
   GO(SYS_wait4, "4s 2m");
   SY(SYS_wait4, x0, x0+1, x0, x0+1); FAIL;

   /* old creat                   8 */

   /* SYS_link                    9 */
   GO(SYS_link, "2s 2m");
   SY(SYS_link, x0, x0); FAIL;

   /* SYS_unlink                  10 */
   GO(SYS_unlink, "1s 1m");
   SY(SYS_unlink, x0); FAIL;

   /* obs execv                   11 */

   /* chdir                       12 */
   GO(SYS_chdir, "1s 1m");
   SY(SYS_chdir, x0); FAIL;

   /* fchdir                      13 */
   GO(SYS_fchdir, "1s 0m");
   SY(SYS_fchdir, x0-1); FAIL;

   /* SYS_freebsd11_mknod         14 */
#if (FREEBSD_VERS >= FREEBSD_12)
   GO(SYS_freebsd11_mknod, "3s 1m");
   SY(SYS_freebsd11_mknod, x0, x0, x0); FAIL;
#else
   GO(SYS_mknod, "3s 1m");
   SY(SYS_mknod, x0, x0, x0); FAIL;
#endif

   /* chmod                       15 */
   GO(SYS_chmod, "2s 1m");
   SY(SYS_chmod, x0, x0); FAIL;

   /* chown                       16 */
   GO(SYS_chown, "3s 1m");
   SY(SYS_chown, x0, x0, x0); FAIL;

   /* break                       17 */
   GO(SYS_break, "1s 1m");
   SY(SYS_break, x0+1); SUCC;

   /* freebsd4 getfsstat          18 */

   /* old lseek                   19 */

   /* getpid                      20 */
   GO(SYS_getpid, "0s 0m");
   SY(SYS_getpid); SUCC;

   /* mount	                      21 */
   /* data not read otherwise this would ne 3m */
   GO(SYS_mount, "4s 2m");
   SY(SYS_mount, x0, x0, x0, x0); FAIL;

   /* unmount                     22 */
   GO(SYS_unmount, "2s 1m");
   SY(SYS_unmount, x0+1, x0); FAIL;

   /* setuid                      23 */
   GO(SYS_setuid, "1s 0m");
   SY(SYS_setuid, x0-1); FAIL;

   /* getuid                      24 */
   GO(SYS_getuid, "0s 0m");
   SY(SYS_getuid); SUCC;

   /* geteuid                     25 */
   GO(SYS_geteuid, "0s 0m");
   SY(SYS_geteuid); SUCC;

   /* ptrace                      26 */
   // XXX: memory pointed to be arg3 goes unchecked... otherwise would be 1m
   GO(SYS_ptrace, "4s 0m");
   SY(SYS_ptrace, x0+PTRACE_EXEC, x0, x0, x0); FAIL;

   /* recvmsg                     27 */
   GO(SYS_recvmsg, "3s 0m");
   SY(SYS_recvmsg, x0, x0, x0); FAIL;

   /* sendmsg                     28 */
   GO(SYS_sendmsg, "3s 0m");
   SY(SYS_sendmsg, x0, x0, x0); FAIL;

   /* recvfrom                    29 */
   GO(SYS_recvfrom, "6+1s 0m");
   SY(SYS_recvfrom, x0-1, x0+1, x0+16, x0+2, x0+3, x0+4); FAIL;

   /* accept                      30 */
   GO(SYS_accept, "3+1s 0m");
   SY(SYS_accept, x0-1, x0+1, x0+1); FAIL;

   /* getpeername                 31 */
   GO(SYS_getpeername, "3s 1m");
   SY(SYS_getpeername, x0, x0, x0); FAIL;

   /* getsockname                 32 */
   GO(SYS_getsockname, "3s 1m");
   SY(SYS_getsockname, x0, x0, x0); FAIL;

   /* access                      33 */
   GO(SYS_geteuid, "0s 0m");
   SY(SYS_geteuid); SUCC;

   /* chflags                     34 */
   GO(SYS_chflags, "2s 1m");
   SY(SYS_chflags, x0, x0); FAIL;

   /* fchflags                    35 */
   GO(SYS_fchflags, "2s 0m");
   SY(SYS_fchflags, x0+10, x0); FAIL;

   /* sync                        36 */
   GO(SYS_sync, "0s 0m");
   SY(SYS_sync); SUCC;

   /* kill                        37 */
   GO(SYS_kill, "2s 0m");
   SY(SYS_kill, x0, x0); SUCC;

   /* old stat                    38 */

   /* getppid                     39 */
   GO(SYS_getppid, "0s 0m");
   SY(SYS_getppid); SUCC;

   /* old lstat                   40 */

   /* dup                         41 */
   GO(SYS_dup, "1s 0m");
   SY(SYS_dup, x0-1); FAIL;

   /* freebsd10_pipe              42 */
#if (FREEBSD_VERS >= FREEBSD_11)
   GO(SYS_freebsd10_pipe, "0s 0m");
   SY(SYS_freebsd10_pipe, x0); SUCC;
#else
   GO(SYS_pipe, "0s 0m");
   SY(SYS_pipe, x0); SUCC;
#endif

   /* getegid                     43 */
   GO(SYS_getegid, "0s 0m");
   SY(SYS_getegid); SUCC;

   /* profil                      44 */
   GO(SYS_profil, "ni");
   SY(SYS_profil, x0, x0, x0, x0);

   /* ktrace                      45 */
   GO(SYS_ktrace, "ni");
   SY(SYS_ktrace, x0, x0, x0, x0);

   /* old sigaction               46 */

   /* getgid                      47 */
   GO(SYS_getgid, "0s 0m");
   SY(SYS_getgid); SUCC;

   /* old sigprocmask             48 */

   /* getlogin                    49 */
   GO(SYS_getlogin, "2s 1m");
   SY(SYS_getlogin, x0, x0+20); FAIL;

   /* setlogin                    50 */
   GO(SYS_setlogin, "1s 1m");
   SY(SYS_setlogin, x0); FAIL;

   /* acct                        51 */
   GO(SYS_acct, "1s 1m");
   SY(SYS_acct, x0-1); FAIL;

   /* 4.3 sigpending              52 */
   /* sigaltstack                 53 */
   {
      struct our_sigaltstack {
              char *ss_sp;
              size_t ss_size;
              int ss_flags;
      } ss;
      ss.ss_sp     = NULL;
      ss.ss_flags  = 0;
      ss.ss_size   = 0;
      VALGRIND_MAKE_MEM_NOACCESS(& ss, sizeof(struct our_sigaltstack));
      GO(SYS_sigaltstack, "2s 2m");
      SY(SYS_sigaltstack, x0+&ss, x0+&ss); SUCC; /* FAIL when run standalone */
   }

   /* SYS_ioctl                   54 */
   #include <termios.h>
   GO(SYS_ioctl, "3s 1m");
   SY(SYS_ioctl, x0, x0+TIOCGWINSZ, x0); FAIL;

   /* SYS_reboot                  55 */
   /* how to test that and be sure of not rebooting? */

   /* SYS_revoke                  56 */
   GO(SYS_revoke, "1s 1m");
   SY(SYS_revoke, x0); FAIL;

   /* SYS_symlink                 57 */
   GO(SYS_symlink, "2s 2m");
   SY(SYS_symlink, x0, x0); FAIL;

   /* SYS_readlink                58 */
   GO(SYS_readlink, "3s 2m");
   SY(SYS_readlink, x0+1, x0+1, x0+1); FAIL;

   /* SYS_execve                  59 */
   GO(SYS_execve, "3s 1m");
   SY(SYS_execve, x0 + 1, x0 + 1, x0); FAIL;

   /* SYS_umask                   60 */
   GO(SYS_umask, "1s 0m");
   SY(SYS_umask, x0+022); SUCC;

   /* SYS_chroot                  61 */
   GO(SYS_chroot, "1s 1m");
   SY(SYS_chroot, x0); FAIL;

   /* 4.3 fstat                   62 */

   /* 4.3 getgerninfo             63 */

   /* 4.3 getpagesize             64 */

   /* SYS_msync                   65 */
   GO(SYS_msync, "3s 1m");
   SY(SYS_msync, x0, x0+1, x0); FAIL;

   /* SYS_vfork                   66 */
    GO(SYS_vfork, "other");
   // (see scalar_vfork.c)

   /* obsol vread                 67 */

   /* obsol vwrite                68 */

   /* SYS_sbrk                    69 */
   GO(SYS_sbrk, "1s 1m");
   SY(SYS_sbrk, x0); FAIL;

   /* not implemented on OS SYS_sstk 70 */

   /* 4.3 mmap                    71 */

   /* 4.2 vadvise                 72 */

   /* SYS_munmap                  73 */
   GO(SYS_munmap, "2s 0m");
   SY(SYS_munmap, x0, x0); FAIL;

   /* SYS_mprotect                74 */
   GO(SYS_mprotect, "3s 0m");
   /* PJF why does this succeed? */
   SY(SYS_mprotect, x0+1, x0, x0); SUCC;

   /* SYS_madvise                 75 */
   GO(SYS_madvise, "3s 0m");
   SY(SYS_madvise, x0, x0+1, x0); FAILx(EINVAL);

   /* obsol vhangup               76 */

   /* obsol vlimit                77 */

   /* SYS_mincore                 78 */
   GO(SYS_mincore, "3s 1m");
   SY(SYS_mincore, x0, x0+40960, x0); FAIL;

   /* SYS_getgroups               79 */
   GO(SYS_getgroups, "2s 1m");
   SY(SYS_getgroups, x0+1, x0+1); FAIL;

   /* SYS_setgroups               80 */
   GO(SYS_setgroups, "2s 1m");
   SY(SYS_setgroups, x0+1, x0+1); FAIL;

   /* SYS_getpgrp                 81 */
   GO(SYS_getpgrp, "0s 0m");
   SY(SYS_getpgrp); SUCC;

   /* SYS_setpgid                 82 */
   GO(SYS_setpgid, "2s 0m");
   SY(SYS_setpgid, x0, x0-1); FAIL;

   /* SYS_setitimer               83 */
   GO(SYS_setitimer, "3s 2m");
   SY(SYS_setitimer, x0, x0+1, x0+1); FAIL;

   /* 4.3 wait                    84 */

   /* SYS_swapon                  85 */
   GO(SYS_swapon, "1s 1m");
   SY(SYS_swapon, x0); FAIL;

   /* SYS_getitimer               86 */
   GO(SYS_getitimer, "2s 1m");
   SY(SYS_getitimer, x0+4, x0+1); FAIL;

   /* 4.3 gethostname             87 */

   /* 4.3 sethostname             88 */

   /* SYS_getdtablesize           89 */
   GO(SYS_getdtablesize, "0s 0m");
   SY(SYS_getdtablesize); SUCC;

   /* SYS_dup2                    90 */
   GO(SYS_dup2, "2s 0m");
   SY(SYS_dup2, x0-1, x0); FAIL;

   /* unimpl getdopt              91 */

   /* SYS_fcntl                   92 */
   /* takes varargs so two versions of test */
   GO(SYS_fcntl, "(GETFD) 2s 0m");
   SY(SYS_fcntl, x0-1, x0+F_GETFD, x0); FAILx(EBADF);

   GO(SYS_fcntl, "(DUPFD) 1s 0m");
   SY(SYS_fcntl, -1, F_DUPFD, x0); FAILx(EBADF);

   /* SYS_select                  93 */
   GO(SYS_select, "5s 4m");
   SY(SYS_select, x0+8, x0+0xffffffee, x0+1, x0+1, x0+1); FAIL;

   /* unimpl setdopt              94 */

   /* SYS_fsync                   95 */
   GO(SYS_fsync, "1s 0m");
   SY(SYS_fsync, x0-1); FAIL;

   /* SYS_setpriority             96 */
   GO(SYS_setpriority, "3s 0m");
   SY(SYS_setpriority, x0-1, x0, x0); FAIL;

   /* SYS_socket                  97 */
   GO(SYS_socket, "3s 0m");
   SY(SYS_socket, x0, x0, x0); FAIL;

   /* SYS_connect                  98 */
   /* needs a socket for the 1m ? */
   GO(SYS_connect, "3s 0m");
   SY(SYS_connect, x0, x0, x0); FAIL;

   /* 4.3 accept                  99 */

   /* SYS_getpriority             100 */
   GO(SYS_getpriority, "2s 0m");
   SY(SYS_getpriority, x0-1, x0); FAIL;

   /* 4.3 send                    101 */
   /* 4.3 recv                    102 */
   /* 4.3 sigreturn               103 */

   /* SYS_bind                    104 */
   GO(SYS_bind, "3s 0m");
   /* as connect, needs a socket */
   SY(SYS_bind, x0, x0, x0); FAIL;

   /* SYS_setsockopt              105 */
   GO(SYS_setsockopt, "5s 0m");
   SY(SYS_setsockopt, x0, x0, x0, x0, x0); FAIL;

   /* SYS_listen                  106 */
   GO(SYS_listen, "2s 0m");
   SY(SYS_listen, x0, x0); FAIL;

   /* obsol vtimes                107 */

   /* 4.3 sigvec                  108 */

   /* 4.3 sigblock                109 */

   /* 4.3 sigsetmask              110 */

   /* 4.3 sigsuspend              111 */

   /* 4.3 sigstack                112 */

   /* 4.3 recvmsg                 113 */

   /* 4.3 sendmsg                 114 */

   /* 4.3 vtrace                  115 */

   /* SYS_gettimeofday            116 */
   GO(SYS_gettimeofday, "2s 2m");
   SY(SYS_gettimeofday, x0+1, x0+1); FAIL;

   /* SYS_getrusage               117 */
   GO(SYS_getrusage, "2s 1m");
   SY(SYS_getrusage, x0, x0); FAIL;

   /* SYS_getsockopt              118 */
   GO(SYS_setsockopt, "5s 1m");
   SY(SYS_setsockopt, x0, x0, x0, x0, x0); FAIL;

   /* unimpl resuba               119 */

   /* SYS_readv                   120 */
   GO(SYS_readv, "3s 1m");
   SY(SYS_readv, x0, x0, x0+1); FAIL;

   /* SYS_writev                  121 */
   GO(SYS_writev, "3s 1m");
   SY(SYS_writev, x0, x0, x0+1); FAIL;

   /* SYS_settimeofday            122 */
   GO(SYS_settimeofday, "2s 2m");
   SY(SYS_settimeofday, x0+1, x0+1); FAIL;

   /* SYS_fchown                  123 */
   GO(SYS_fchown, "3s 0m");
   SY(SYS_fchown, x0-1, x0, x0); FAIL;

   /* SYS_fchmod                  124 */
   GO(SYS_fchmod, "2s 0m");
   SY(SYS_fchmod, x0-1, x0); FAIL;

   /* 4.3 recvfrom                125 */

   /* SYS_setreuid                126 */
   GO(SYS_setreuid, "2s 0m");
   SY(SYS_setreuid, x0-1, x0-1); SUCC;

   /* SYS_setregid                127 */
   GO(SYS_setregid, "2s 0m");
   SY(SYS_setregid, x0-1, x0-1); SUCC;

   /* SYS_rename                  128 */
   GO(SYS_rename, "2s 2m");
   SY(SYS_rename, x0, x0); FAIL;

   /* 4.3 truncate                129 */

   /* 4.3 ftruncate               130 */

   /* SYS_flock                   131 */
   GO(SYS_flock, "2s 0m");
   SY(SYS_flock, x0, x0); FAIL;

   /* SYS_mkfifo                  132 */
   GO(SYS_mkfifo, "2s 1m");
   SY(SYS_mkfifo, x0, x0); FAIL;

   /* SYS_sendto                  133 */
   GO(SYS_sendto, "6s 0m");
   SY(SYS_sendto, x0, x0, x0, x0, x0, x0); FAIL;

   /* SYS_shutdown                134 */
   /* don't really want to to this */

   /* SYS_socketpair              135 */
   GO(SYS_socketpair, "4s 1m");
   SY(SYS_socketpair, x0, x0, x0, x0); FAIL;

   /* SYS_mkdir                   136 */
   GO(SYS_mkdir, "2s 1m");
   SY(SYS_mkdir, x0, x0); FAIL;

   /* SYS_rmdir                   137 */
   GO(SYS_rmdir, "1s 1m");
   SY(SYS_rmdir, x0); FAIL;

   /* SYS_utimes                  138 */
   GO(SYS_utimes, "2s 2m");
   SY(SYS_utimes, x0, x0+1); FAIL;

   /* 4.2 sigreturn               139 */

   /* SYS_adjtime                 140 */
   GO(SYS_adjtime, "2s 1m");
   /* succeeds? need non-null arg2 for 2m */
   SY(SYS_adjtime, x0, x0); SUCC;

   /* 4.3 getpeername             141 */

   /* 4.3 gethostid               142 */

   /* 4.3 sethostid               143 */

   /* 4.3 getrlimit`              144 */

   /* 4.3 setrlimit               145 */

   /* 4.3 killpg                  146 */

   /* SYS_setsid                  147 */
   GO(SYS_setsid, "0s 0m");
   SY(SYS_setsid); SUCC; /* FAIL when run standalone */

   /* SYS_quotactl                148 */
   GO(SYS_quotactl, "(Q_QUOTAOFF) 2s 0m");
   SY(SYS_quotactl, x0, x0+Q_QUOTAOFF, x0, x0); FAIL;

   GO(SYS_quotactl, "(Q_QUOTAON) 4s 2m");
   SY(SYS_quotactl, x0, x0+Q_QUOTAON, x0, x0); FAIL;

   /* 4.3 quota                   149 */

   /* 4.3 getsockname             150 */

   /* bsd/os sem_lock             151 */

   /* bsd/os sem_wakeup           152 */

   /* bsd/os asyncdaemon          153 */

   /* SYS_nlm_syscall             154 */

   // BSDXY(__NR_nfssvc,        sys_nfssvc),            // 155

   /* 4.3 getdirentries           156 */

   /* freebsd 4 statfs            157 */

   /* freebsd 4 fstatfs           158 */

   /* SYS_lgetfh                  160 */
   GO(SYS_lgetfh, "2s 2m");
   SY(SYS_lgetfh, x0, x0); FAIL;

   /* SYS_getfh                   161 */
   GO(SYS_getfh, "2s 2m");
   SY(SYS_getfh, x0, x0); FAIL;

#if (FREEBSD_VERS <= FREEBSD_10)
   /* SYS_getdomainname          162 */
   GO(SYS_freebsd4_getdomainname, "2s 1m");
   SY(SYS_freebsd4_getdomainname, x0, x0); FAIL;

   /* SYS_setdomainname           163 */
   GO(SYS_freebsd4_setdomainname, "2s 0m");
   SY(SYS_freebsd4_setdomainname, x0, x0); FAIL;

   /* SYS_uname                   164 */
   GO(SYS_freebsd4_uname, "1s 1m");
   SY(SYS_freebsd4_uname, x0); FAIL;
#endif

   /* SYS_sysarch                 165 */
#if defined (VGP_x86_freebsd)
   GO(SYS_sysarch, "2s 1m");
   SY(SYS_sysarch, x0+I386_GET_GSBASE, x0); FAIL;

   GO(SYS_sysarch, "2s 0m");
   SY(SYS_sysarch, x0+I386_SET_GSBASE, x0); FAIL;
#elif defined(VGP_amd64_freebsd)
   GO(SYS_sysarch, "2s 1m");
   SY(SYS_sysarch, x0+AMD64_GET_FSBASE, x0); FAIL;

   GO(SYS_sysarch, "2s 0m");
   SY(SYS_sysarch, x0+AMD64_SET_FSBASE, x0); FAIL;
#else
#error "freebsd platform not defined"
#endif

   /* SYS_rtprio                   166 */
   GO(SYS_rtprio, "(GET) 3s 1m");
   SY(SYS_rtprio, x0, x0, x0); FAIL;

   GO(SYS_rtprio, "(SET) 3s 1m");
   SY(SYS_rtprio, x0+1, x0, x0); FAIL;

   /* following 3 not implemented in OS */

   /* SYS_semsys                  169 */

   /* SYS_msgsys                  170 */

   /* SYS_shmsys                  171 */

#if (FREEBSD_VERS <= FREEBSD_10)

   /* @todo PJF maybe one day */

   /* SYS_freebsd6_pread          173 */

   /* SYS_freebsd6_pwrite         174 */
#endif

   /* SYS_setfib                  175 */
   GO(SYS_setfib, "1s 0m");
   SY(SYS_setfib, x0+10); FAIL;

   // BSDXY(__NR_ntp_adjtime,   sys_ntp_adjtime),       // 176

   /* bsd/os sfork                177 */

   /* bsd/os getdescriptor        178 */

   /* bsd/os setdescriptor        179 */

   /* SYS_setgid,                 181 */
   GO(SYS_setgid, "1s 0m");
   SY(SYS_setgid, x0-1); FAIL;

   /* SYS_setegid                 182 */
   GO(SYS_setegid, "1s 0m");
   SY(SYS_setegid, x0-1); FAIL;

   /* SYS_seteuid                 183 */
   GO(SYS_seteuid, "1s 0m");
   SY(SYS_seteuid, x0-1); FAIL;


   /* unimpl lfs_bmapv            184 */

   /* unimpl lfs_markv            185 */

   /* unimpl lfs_segclean         186 */

   /* unimpl lfs_segwait          187 */

 #if (FREEBSD_VERS >= FREEBSD_12)
   /* SYS_freebsd11_stat          188 */
   GO(SYS_freebsd11_stat, "2s 2m");
   SY(SYS_freebsd11_stat, x0, x0); FAIL;

   /* SYS_freebsd11_fstat         189 */
   GO(SYS_freebsd11_fstat, "2s 1m");
   SY(SYS_freebsd11_fstat, x0, x0); FAIL;

   /* SYS_freebsd11_lstat         190 */
   GO(SYS_freebsd11_lstat, "2s 2m");
   SY(SYS_freebsd11_lstat, x0, x0); FAIL;

 #else
   /* SYS_stat          188 */
   GO(SYS_stat, "2s 2m");
   SY(SYS_stat, x0, x0); FAIL;

   /* SYS_fstat                   189 */
   GO(SYS_fstat, "2s 1m");
   SY(SYS_fstat, x0, x0); FAIL;

   /* SYS_lstat         190 */
   GO(SYS_lstat, "2s 2m");
   SY(SYS_lstat, x0, x0); FAIL;
 #endif

   /* SYS_pathconf                191 */
   GO(SYS_pathconf, "2s 1m");
   SY(SYS_pathconf, x0, x0); FAIL;

   /* SYS_fpathconf               192 */
   GO(SYS_fpathconf, "2s 0m");
   SY(SYS_fpathconf, x0, x0); FAIL;

   /* nosys                       193 */

   /* SYS_getrlimit               194 */
   GO(SYS_getrlimit, "2s 1m");
   SY(SYS_getrlimit, x0, x0); FAIL;

   /* SYS_3etrlimit               195 */
   GO(SYS_setrlimit, "2s 1m");
   SY(SYS_setrlimit, x0, x0); FAIL;

   /* SYS_freebsd11_getdirentries 196 */
 #if (FREEBSD_VERS >= FREEBSD_12)
   GO(SYS_freebsd11_getdirentries, "4s 2m");
   SY(SYS_freebsd11_getdirentries, x0, x0, x0+3, x0+1); FAIL;
#else
   GO(SYS_getdirentries, "4s 2m");
   SY(SYS_getdirentries, x0, x0, x0+3, x0+1); FAIL;
#endif

#if (FREEBSD_VERS <= FREEBSD_10)
   /* SYS_freebsd6_mmap           197*/
#endif

   /* __syscall (handled specially) 198 */

#if (FREEBSD_VERS <= FREEBSD_10)
   /* SYS_freebsd6_lseek          199 */

   /* SYS_freebsd6_truncate       200 */

   /* SYS_freebsd6_ftruncate      201 */
#endif

   /* SYS___sysctl                202 */
   GO(SYS___sysctl, "(getoldlen) 3s 2m");
   SY(SYS___sysctl, x0, x0+1, NULL, x0+1, NULL, x0); FAIL;

   GO(SYS___sysctl, "(getold) 4s 2m");
   SY(SYS___sysctl, x0, x0+1, x0+1, x0+1, NULL, x0); FAIL;

   GO(SYS___sysctl, "(putnew) 4s 2m");
   SY(SYS___sysctl, x0, x0+1, NULL, NULL, x0+1, x0+2); FAIL;

   /* SYS_mlock                   203 */
   GO(SYS_mlock, "2s 0m");
   SY(SYS_mlock, x0, x0+1); FAIL;

   /* SYS_munlock                 204 */
   GO(SYS_munlock, "2s 0m");
   SY(SYS_munlock, x0, x0+1); FAIL;

   /* SYS_undelete                205 */
   GO(SYS_undelete, "1s 1m");
   SY(SYS_undelete, x0); FAIL;

   /* SYS_futimes                 206 */
   GO(SYS_futimes, "2s 0m");
   /* not 1m? */
   SY(SYS_futimes, x0+5, x0); FAIL;

   /* SYS_getpgid                 207 */
   GO(SYS_getpgid, "1s 0m");
   SY(SYS_getpgid, x0-1); FAIL;

   /* netbsd newreboot            208 */

   /* SYS_poll                    209 */
   GO(SYS_poll, "2s 2m");
   SY(SYS_poll, x0, x0+1, x0); FAIL;

   {
      struct pollfd fds = { x0, x0, x0 };
      GO(SYS_poll, "0s 2m");
      SY(SYS_poll, &fds, 1, 1); SUCC;
   }

   /* SYS_freebsd7___semctl       220 */
   GO(SYS_freebsd7___semctl, "(IPC_INFO) 4s 1m");
   SY(SYS_freebsd7___semctl, x0, x0, x0+IPC_INFO, x0+1); FAIL;

   GO(SYS_freebsd7___semctl, "(bogus cmd) 3s 0m");
   SY(SYS_freebsd7___semctl, x0, x0, x0-1, x0+1); FAIL;

   /* SYS_semget                  221 */
   GO(SYS_semget, "3s 0m");
   SY(SYS_semget, x0, x0, x0); FAIL;

   /* SYS_semop                   222 */
   GO(SYS_semop, "3s 0m");
   SY(SYS_semop, x0, x0, x0); FAIL;

   /* unimpl semconfig            223 */

   /* SYS_freebsd7_msgctl         224 */
   GO(SYS_freebsd7_msgctl, "(set) 3s 1m");
   SY(SYS_freebsd7_msgctl, x0, x0+1, x0); FAIL;

   GO(SYS_freebsd7_msgctl, "(stat) 3s 1m");
   SY(SYS_freebsd7_msgctl, x0, x0+2, x0); FAIL;

   /* SYS_msgget                  225 */
   GO(SYS_msgget, "2s 0m");
   SY(SYS_msgget, x0-1, x0); FAIL;

   /* SYS_msgsnd                  226 */
   GO(SYS_msgsnd, "4s 1m");
   SY(SYS_msgsnd, x0, x0, x0, x0); FAIL;

   /* SYS_msgrcv                  227 */
   GO(SYS_msgrcv, "4+1s 1m");
   SY(SYS_msgrcv, x0-1, x0+1, x0+4, x0, x0); FAIL;

   /* SYS_shmat                   228 */
   GO(SYS_shmat, "3s 0m");
   SY(SYS_shmat, x0, x0, x0); FAIL;

   /* SYS_freebsd7_shmctl         229 */
   GO(SYS_freebsd7_shmctl, "3s 0m");
   SY(SYS_freebsd7_shmctl, x0, x0, x0); FAIL;

   GO(SYS_freebsd7_shmctl, "(bogus cmd) 3s 0m");
   SY(SYS_freebsd7_shmctl, x0, x0-1, x0+1); FAIL;


   /* SYS_shmdt                   230 */
   GO(SYS_shmdt, "1s 0m");
   SY(SYS_shmdt, x0); FAIL;

   /* SYS_shmget                  231 */
   GO(SYS_shmget, "3s 0m");
   SY(SYS_shmget, x0, x0, x0); FAIL;

   /* SYS_clock_gettime           232 */
   GO(SYS_clock_gettime, "2s 1m");
   SY(SYS_clock_gettime, x0, x0+1); FAIL;

   /* SYS_clock_settime           233 */
   GO(SYS_clock_settime, "2s 1m");
   SY(SYS_clock_settime, x0, x0); FAIL;

   /* SYS_clock_getres            234 */
   GO(SYS_clock_getres, "2s 1m");
   SY(SYS_clock_getres, x0, x0+1); FAIL;

   /* SYS_ktimer_create           235 */
   GO(SYS_ktimer_create, "3s 2m");
   SY(SYS_ktimer_create, x0-1, x0+1, x0+1); FAIL;

   /* SYS_ktimer_delete           236 */
   GO(SYS_ktimer_delete, "1s 0m");
   SY(SYS_ktimer_delete, x0); FAIL;

   /* SYS_ktimer_settime          237 */
   GO(SYS_ktimer_settime, "4s 2m");
   SY(SYS_ktimer_settime, x0-1, x0+1, x0+1, x0+1); FAIL;

   /* SYS_ktimer_gettime          238 */
   GO(SYS_ktimer_gettime, "2s 1m");
   SY(SYS_ktimer_gettime, x0-1, x0+1); FAIL;

   /* SYS_ktimer_getoverrun       239 */
   GO(SYS_ktimer_getoverrun, "1s 0m");
   SY(SYS_ktimer_getoverrun, x0+1); FAIL;

   /* SYS_nanosleep               240 */
   GO(SYS_nanosleep, "2s 2m");
   SY(SYS_nanosleep, x0, x0+1); FAIL;

   // unimpl SYS_ffclock_getcounter                        241

   // unimpl SYS_ffclock_setestimate                       242

   // unimpl SYS_ffclock_getestimate                       243

#if (FREEBSD_VERS >= FREEBSD_11)
   /* SYS_clock_nanosleep         244 */
   /* this succeeds ? */
   GO(SYS_clock_nanosleep, "4s 2m");
   SY(SYS_clock_nanosleep, x0+5000, x0+3000, x0, x0+1); SUCC;
#endif

   // SYS_clock_getcpuclockid2                             247
   GO(SYS_clock_getcpuclockid2, "3s 1m");
   SY(SYS_clock_getcpuclockid2, x0+1, x0+1, x0+1); FAIL;

   // BSDXY(__NR_ntp_gettime,      sys_ntp_gettime),       // 248

   /* SYS_minherit                250 */
   // @todo PJF causes Valgrind to crash
   //GO(SYS_minherit, "3s 1m");
   //SY(SYS_minherit, x0, x0+1024, x0+1); SUCC;
   GO(SYS_minherit, "@todo");

   /* SYS_rfork                   251 */
   GO(SYS_rfork, "other");

   /* openbsd_poll                252 */

   /* SYS_issetugid               253 */
   GO(SYS_issetugid, "0s 0m");
   SY(SYS_issetugid); SUCC;

   /* SYS_lchown                  254 */
   GO(SYS_lchown, "3s 1m");
   SY(SYS_lchown, x0, x0+1234, x0+2345); FAIL;

   /* SYS_aio_read                255 */
   GO(SYS_aio_read, "1s 1m");
   SY(SYS_aio_read, x0+1); FAIL;

   /* SYS_aio_write               256 */
   GO(SYS_aio_write, "1s 1m");
   SY(SYS_aio_write, x0+1); FAIL;

   /* SYS_lio_listio              257 */
   GO(SYS_lio_listio, "4s 2m");
   SY(SYS_lio_listio, x0+0, x0+1, x0+10, x0+1); FAIL;

   /* SYS_freebsd11_getdents      272 */
#if (FREEBSD_VERS >= FREEBSD_12)
   GO(SYS_freebsd11_getdents, "3s 1m");
   SY(SYS_freebsd11_getdents, x0+9, x0+1, x0+2); FAIL;
#else
   GO(SYS_getdents, "3s 1m");
   SY(SYS_getdents, x0+9, x0+1, x0+2); FAIL;
#endif

   /* SYS_lchmod                  274 */
   GO(SYS_lchmod, "2s 1m");
   SY(SYS_lchmod, x0, x0+1234); FAIL;

   /* netbsd_lchown               275 */

   /* SYS_lutimes                 276 */
   GO(SYS_lutimes, "3s 2m");
   SY(SYS_lutimes, x0+9, x0+1, x0+2); FAIL;

   /* netbsd msync                277 */

   /* netbsd stat                 278 */

   /* netbsd fstat                279 */

   /* netbsd lstat                280 */

   /* SYS_preadv                  289 */
#if defined(VGP_amd64_freebsd)
   GO(SYS_preadv, "4s 0m");
   /* 0m because of the bogus fd */
   SY(SYS_preadv, x0+9999999, x0+1, x0+16, x0+20); FAIL;
#else
   GO(SYS_preadv, "5s 0m");
   SY(SYS_preadv, x0+9999999, x0+1, x0+16, x0, x0+20); FAIL;
#endif

   /* SYS_pwritev                    290 */
#if defined(VGP_amd64_freebsd)
   GO(SYS_pwritev, "4s 0m");
   SY(SYS_pwritev, x0+9999999, x0+1, x0+16, x0+20); FAIL;
#else
   GO(SYS_pwritev, "5s 0m");
   SY(SYS_pwritev, x0+9999999, x0+1, x0+16, x0, x0+20); FAIL;
#endif

   /* freebsd 4 fhstatfs          297 */

   /* SYS_fhopen                  298 */
   GO(SYS_fhopen, "2s 1m");
   SY(SYS_fhopen, x0+1, x0); FAIL;

   /* SYS_fhstat                  299 */
   GO(SYS_fhstat, "2s 2m");
   SY(SYS_fhstat, x0+1, x0+2); FAIL;

   /* SYS_modnext                 300 */
   GO(SYS_modnext, "1s 0m");
   SY(SYS_modnext, x0+1); SUCC;

   /* SYS_modstat                 301 */
   GO(SYS_modstat, "2s 1m");
   SY(SYS_modstat, x0+1234, x0+1); FAIL;

   /* SYS_modfnext                302 */
   GO(SYS_modfnext, "1s 0m");
   SY(SYS_modfnext, x0+1); SUCC;

   /* SYS_modfind                 303 */
   GO(SYS_modfind, "1s 1m");
   SY(SYS_modfind, x0+1234, x0+1); FAIL;

   /* SYS_kldload                  304 */
   GO(SYS_kldload, "1s 1m");
   SY(SYS_kldload, x0+1); FAIL;

   /* SYS_kldunload               305 */
   GO(SYS_kldunload, "1s 0m");
   SY(SYS_kldunload, x0+1); FAIL;

   /* SYS_kldfind                 306 */
   GO(SYS_kldfind, "1s 1m");
   SY(SYS_kldfind, x0+1); FAIL;

   /* SYS_kldnext                 307 */
   GO(SYS_kldnext, "1s 0m");
   SY(SYS_kldnext, x0+1000); FAIL;

   /* SYS_kldstat                 308 */
   GO(SYS_kldstat, "2s 1m");
   SY(SYS_kldstat, x0+1234, x0+1); FAIL;

   /* SYS_kldfirstmod             309 */
   GO(SYS_kldfirstmod, "1s 0m");
   SY(SYS_kldfirstmod, x0+1000); FAIL;

   /* SYS_getsid                  310 */
   GO(SYS_getsid, "1s 0m");
   SY(SYS_getsid, x0-1); FAIL;

   /* SYS_setresuid               311 */
   GO(SYS_setresuid, "3s 0m");
   SY(SYS_setresuid, x0+1, x0+2, x0+3); FAIL;

   /* SYS_setresgid               312 */
   GO(SYS_setresgid, "3s 0m");
   SY(SYS_setresgid, x0+1, x0+2, x0+3); FAIL;

   /* obsol signanosleep          313 */

   /* SYS_aio_return              314 */
   GO(SYS_aio_return, "1s 1m");
   SY(SYS_aio_return, x0+1); FAIL;

   /* SYS_aio_suspend             315 */
   GO(SYS_aio_suspend, "3s 2m");
   SY(SYS_aio_suspend, x0+1, x0+1, x0+1); FAIL;

   /* SYS_aio_cancel              316 */
   GO(SYS_aio_cancel, "2s 1m");
   SY(SYS_aio_cancel, x0-1, x0+1); FAIL;

   /* SYS_aio_error               317 */
   GO(SYS_aio_error, "1s 1m");
   SY(SYS_aio_error, x0+1); SUCC;

   /* freebsd 6 aio_read          318 */

   /* freebsd 6 aio_write         319 */

   /* freebsd 6 lio_listio        320 */

   /* SYS_yield                   321 */
   GO(SYS_yield, "0s 0m");
   SY(SYS_yield); SUCC;

   /* obs thr_sleep               322 */

   /* obs thr_wakeup              323 */

   /* SYS_mlockall                324 */
   GO(SYS_mlockall, "1s 0m");
   SY(SYS_mlockall, x0-1); FAIL;

   /* SYS_munlockall              325 */
   GO(SYS_munlockall, "0s 0m");
   SY(SYS_munlockall); SUCC;

   /* SYS___getcwd                326 */
   GO(SYS___getcwd, "2s 1m");
   SY(SYS___getcwd, x0+1, x0+1); FAIL;

   /* SYS_sched_setparam          327 */
   GO(SYS_sched_setparam, "2s 1m");
   SY(SYS_sched_setparam, x0+1, x0+1); FAIL;

   /* SYS_sched_getparam          328 */
   GO(SYS_sched_getparam, "2s 1m");
   SY(SYS_sched_getparam, x0+1, x0+1); FAIL;

   /* SYS_sched_setscheduler      329 */
   GO(SYS_sched_setscheduler, "3s 1m");
   SY(SYS_sched_setscheduler, x0+1, x0+1, x0+1); FAIL;

   /* SYS_sched_getscheduler      330*/
   GO(SYS_sched_getscheduler, "1s 0m");
   SY(SYS_sched_getscheduler, x0+486486); FAIL;

   /* SYS_sched_yield             331 */
   GO(SYS_sched_yield, "0s 0m");
   SY(SYS_sched_yield); SUCC;

   /* SYS_sched_get_priority_max  332 */
   GO(SYS_sched_get_priority_max, "1s 0m");
   SY(SYS_sched_get_priority_max, x0+5678); FAIL;

   /* SYS_sched_get_priority_min  333 */
   GO(SYS_sched_get_priority_min, "1s 0m");
   SY(SYS_sched_get_priority_min, x0+9876); FAIL;

   /* SYS_sched_rr_get_interval   334 */
   GO(SYS_sched_rr_get_interval, "2s 1m");
   SY(SYS_sched_rr_get_interval, x0+999999, x0+1); FAIL;

   /* SYS_utrace                  335*/
   GO(SYS_utrace, "2s 1m");
   SY(SYS_utrace, x0+1, x0+16); SUCC;

   // freebsd 4 sendfile          336

   /* SYS_kldsym                  337 */
   GO(SYS_kldsym, "3s 1m");
   SY(SYS_kldsym, x0-1, x0+16, x0+1); FAIL;

   /* SYS_jail                    338 */
   GO(SYS_jail, "1s 1m");
   SY(SYS_jail, x0+1); FAIL;

   // unimpl pioctl               339

   /* SYS_sigprocmask             340 */
   GO(SYS_sigprocmask, "2s 1m");
   SY(SYS_sigprocmask, x0+13, NULL, x0+1); FAIL;

   GO(SYS_sigprocmask, "3s 2m");
   SY(SYS_sigprocmask, x0+13, x0+2, x0+1); FAIL;

   /* SYS_sigsuspend              341 */
   GO(SYS_sigsuspend, "1s 1m");
   SY(SYS_sigsuspend, x0+1); FAIL;

   // freebsd 4 sigaction         342

   /* SYS_sigpending              343 */
   GO(SYS_sigpending, "1s 1m");
   SY(SYS_sigpending, x0+1); FAIL;

   /* freebsd 4 sigreturn         344 */

   /* SYS_sigtimedwait            345 */
   GO(SYS_sigtimedwait, "3s 3m");
   SY(SYS_sigtimedwait, x0+1, x0+2, x0+3); FAIL;

   /* SYS_sigwaitinfo             346 */
   GO(SYS_sigwaitinfo, "2s 2m");
   SY(SYS_sigwaitinfo, x0+1, x0+2, x0+3); FAIL;

   /* SYS___acl_get_file          347 */
   GO(SYS___acl_get_file, "3s 2m");
   SY(SYS___acl_get_file, x0+1, x0+4567, x0+3); FAIL;

   /* SYS___acl_set_file          348 */
   GO(SYS___acl_set_file, "3s 2m");
   SY(SYS___acl_set_file, x0+1, x0+4567, x0+3); FAIL;

   /* SYS___acl_get_fd            349 */
   GO(SYS___acl_get_fd, "3s 1m");
   SY(SYS___acl_get_fd, x0-1, x0+4567, x0+3); FAIL;

   /* SYS___acl_set_fd            350 */
   GO(SYS___acl_set_fd, "3s 1m");
   SY(SYS___acl_set_fd, x0-1, x0+4567, x0+3); FAIL;

   /* SYS___acl_delete_file       351 */
   GO(SYS___acl_delete_file, "2s 1m");
   SY(SYS___acl_delete_file, x0+1, x0+3); FAIL;

   /* SYS___acl_delete_fd         352 */
   GO(SYS___acl_delete_fd, "2s 0m");
   SY(SYS___acl_delete_fd, x0-1, x0+3); FAIL;

   /* SYS___acl_aclcheck_file     353 */
   GO(SYS___acl_aclcheck_file, "3s 2m");
   SY(SYS___acl_aclcheck_file, x0+1, x0+4567, x0+3); FAIL;

   /* SYS___acl_aclcheck_fd       354 */
   GO(SYS___acl_aclcheck_fd, "3s 1m");
   SY(SYS___acl_aclcheck_fd, x0-1, x0+4567, x0+3); FAIL;

   /* SYS_extattrctl              355 */
   GO(SYS_extattrctl, "5s 3m");
   SY(SYS_extattrctl, x0+1, x0, x0+2, x0, x0+3); FAIL;

   /* SYS_extattr_set_file        356 */
   GO(SYS_extattr_set_file, "5s 3m");
   SY(SYS_extattr_set_file, x0+1, x0, x0+2, x0+3, x0); FAIL;

   /* SYS_extattr_get_file        357 */
   GO(SYS_extattr_get_file, "5s 3m");
   SY(SYS_extattr_get_file, x0+1, x0+2, x0+3, x0+4, x0+5); FAIL;

   /* SYS_extattr_delete_file     358 */
   GO(SYS_extattr_delete_file, "3s 2m");
   SY(SYS_extattr_delete_file, x0+1, x0+2, x0+3); FAIL;

   /* SYS_aio_waitcomplete        359 */
   GO(SYS_aio_waitcomplete, "2s 2m");
   SY(SYS_aio_waitcomplete, x0+1, x0+1); FAIL;

   /* SYS_getresuid               360 */
   GO(SYS_getresuid, "3s 3m");
   SY(SYS_getresuid, x0+1, x0+4567, x0+3); FAIL;

   /* SYS_getresgid               361 */
   GO(SYS_getresgid, "3s 3m");
   SY(SYS_getresgid, x0+1, x0+4567, x0+3); FAIL;

   /* SYS_kqueue                  362 */
   GO(SYS_kqueue, "0s 0m");
   SY(SYS_kqueue); SUCC;

   /* SYS_freebsd11_kevent        363 */
#if (FREEBSD_VERS >= FREEBSD_12)
   GO(SYS_freebsd11_kevent, "6s 3m");
   SY(SYS_freebsd11_kevent, x0+1, x0+2, x0+3, x0+4, x0+5, x0+6); FAIL;
#else
   GO(SYS_kevent, "6s 3m");
   SY(SYS_kevent, x0+1, x0+2, x0+3, x0+4, x0+5, x0+6); FAIL;
#endif

   /* obs __cap_get* / __cap_set* 364 to 369 */

   /* SYS_extattr_set_fd          371 */
   GO(SYS_extattr_set_fd, "5s 2m");
   SY(SYS_extattr_set_fd, x0+999999, x0, x0+1, x0+1, x0+16); FAIL;

   /* SYS_extattr_get_fd          372 */
   GO(SYS_extattr_get_fd, "5s 2m");
   SY(SYS_extattr_get_fd, x0+999999, x0, x0+1, x0+1, x0+16); FAIL;

   /* SYS_extattr_delete_fd       373 */
   GO(SYS_extattr_delete_fd, "3s 1m");
   SY(SYS_extattr_delete_fd, x0+999999, x0, x0+1); FAIL;

   /* SYS___setugid               374 */
   GO(SYS___setugid, "1s 0m");
   SY(SYS___setugid, x0); FAIL;

   // nfsclnt                                              375

   /* SYS_eaccess                 376 */
   GO(SYS_eaccess, "2s 1m");
   SY(SYS_eaccess, x0+1, x0+3); FAIL;

   // afs_syscall                                          377

   /* SYS_nmount                  378 */
   GO(SYS_nmount, "3s 1m");
   SY(SYS_nmount, x0+1, x0+3, x0); FAIL;

   // kse_exit                                             379

   // kse_wakeup                                           380

   // kse_create                                           381

   // kse_thr_interrupt                                    382

   // kse_release                                          383

   // __mac_get_proc                                       384

   // __mac_set_proc                                       385

   // __mac_get_fd                                         386

   // __mac_get_file                                       387

   // __mac_set_fd                                         388

   // __mac_set_file                                       389

   /* SYS_kenv                    390 */
   GO(SYS_kenv, "(KENV_GET) 4s 1m");
   SY(SYS_kenv, x0+0, x0+2, x0+3, x0+4); FAIL;

   GO(SYS_kenv, "(KENV_DUMP) 4s 0m");
   SY(SYS_kenv, x0+3, x0+2, x0+3, x0+4); FAIL;

   GO(SYS_kenv, "(bogus) 4s 0m");
   SY(SYS_kenv, x0+20, x0+2, x0+3, x0+4); FAIL;

   /* SYS_lchflags                391 */
   GO(SYS_lchflags, "2s 1m");
   SY(SYS_lchflags, x0+1, x0+2); FAIL;

   /* SYS_uuidgen                 392 */
   GO(SYS_uuidgen, "2s 1m");
   SY(SYS_uuidgen, x0+1, x0+2); FAIL;

   /* SYS_sendfile                393 */
   GO(SYS_sendfile, "7s 2m");
   SY(SYS_sendfile, x0-1, x0+2, x0+3, x0+4, x0+1, x0+1, x0+3); FAIL;

   // mac_syscall                                          394

#if (FREEBSD_VERS >= FREEBSD_12)
   /* SYS_freebsd11_getfsstat     395*/
   GO(SYS_freebsd11_getfsstat, "3s 1m");
   SY(SYS_freebsd11_getfsstat, x0+1, x0+2, x0+3); FAIL;

   /* SYS_freebsd11_statfs        396 */
   GO(SYS_freebsd11_statfs, "2s 2m");
   SY(SYS_freebsd11_statfs, x0+1, x0+2); FAIL;

   /* SYS_freebsd11_fstatfs       397 */
   GO(SYS_freebsd11_fstatfs, "2s 1m");
   SY(SYS_freebsd11_fstatfs, x0+1, x0+2); FAIL;

   /* SYS_freebsd11_fhstatfs      398 */
   GO(SYS_freebsd11_fhstatfs, "2s 2m");
   SY(SYS_freebsd11_fhstatfs, x0+1, x0+2); FAIL;
#else
   /* SYS_getfsstat     395*/
   GO(SYS_getfsstat, "3s 1m");
   SY(SYS_getfsstat, x0+1, x0+2, x0+3); FAIL;

   /* SYS_statfs        396 */
   GO(SYS_statfs, "2s 2m");
   SY(SYS_statfs, x0+1, x0+2); FAIL;

   /* SYS_fstatfs       397 */
   GO(SYS_fstatfs, "2s 1m");
   SY(SYS_fstatfs, x0+1, x0+2); FAIL;

   /* SYS_fhstatfs      398 */
   GO(SYS_fhstatfs, "2s 2m");
   SY(SYS_fhstatfs, x0+1, x0+2); FAIL;

#endif

   // ksem_close                                           400

   // ksem_post                                            401

   // ksem_wait                                            402

   // ksem_trywait                                         403

   // ksem_init                                            404

   // ksem_open                                            405

   // ksem_unlink                                          406

   // ksem_getvalue                                        407

   // ksem_destroy                                         408

   // __mac_get_pid                                        409

   // __mac_get_link                                       410

   // __mac_set_link                                       411

   /* SYS_extattr_set_link        412 */
   GO(SYS_extattr_set_link, "5s 3m");
   SY(SYS_extattr_set_file, x0+1, x0, x0+2, x0+3, x0); FAIL;

   /* SYS_extattr_get_link        413 */
   GO(SYS_extattr_get_link, "5s 3m");
   SY(SYS_extattr_get_link, x0+1, x0+2, x0+3, x0+4, x0+5); FAIL;

   /* SYS_extattr_delete_link     414 */
   GO(SYS_extattr_delete_link, "3s 2m");
   SY(SYS_extattr_delete_link, x0+1, x0+2, x0+3); FAIL;

   // __mac_execve                                         415

   /* SYSR_sigaction              416 */
   GO(SYS_sigaction, "3s 2+2m");
   SY(SYS_sigaction, x0+1000, x0+2, x0+3); FAIL;

   /* SYS_sigreturn               417 */
   /* hope it fails because we're not in a signal context */
   GO(SYS_sigreturn, "1s 1m");
   SY(SYS_sigreturn, x0+1); FAIL;

   /* SYS_getcontext              421 */
   GO(SYS_getcontext, "1s 1m");
   SY(SYS_getcontext, x0+1); FAIL;

   /* SYS_setcontext              422 */
   GO(SYS_setcontext, "1s 1m");
   SY(SYS_setcontext, x0+1); FAIL;

   /* SYS_swapcontext             423 */
   GO(SYS_swapcontext, "2s 2m");
   SY(SYS_swapcontext, x0+1, x0+2); FAIL;

#if (FREEBSD_VERS >= FREEBSD_13_1)
   /* SYS_freebsd13_swapoff                 424 */
   GO(SYS_freebsd13_swapoff, "1s 1m");
   SY(SYS_freebsd13_swapoff, x0+1); FAIL;
#else
   /* SYS_swapoff                 424 */
   GO(SYS_swapoff, "1s 1m");
   SY(SYS_swapoff, x0+1); FAIL;
#endif

   /* SYS___acl_get_link          425 */
   GO(SYS___acl_get_link, "3s 2m");
   SY(SYS___acl_get_link, x0+1, x0+2, x0+3); FAIL;

   /* SYS___acl_set_link          426 */
   GO(SYS___acl_set_link, "3s 2m");
   SY(SYS___acl_set_link, x0+1, x0+2, x0+3); FAIL;

   /* SYS___acl_delete_link       427 */
   GO(SYS___acl_delete_link, "2s 1m");
   SY(SYS___acl_delete_link, x0+1, x0+2); FAIL;

   /* SYS___acl_aclcheck_link     428 */
   GO(SYS___acl_aclcheck_link, "3s 2m");
   SY(SYS___acl_aclcheck_link, x0+1, x0+2, x0+3); FAIL;

   /* SYS_sigwait                 429 */
   GO(SYS_sigwait, "2s 2m");
   SY(SYS_sigwait, x0+1, x0+2); SUCC;

   // thr_create                  430

   /* SYS_thr_exit                431 */
   GO(SYS_thr_exit, "other");

   /* SYS_thr_self                432 */
   GO(SYS_thr_self, "1s 1m");
   SY(SYS_thr_self, x0+1); FAIL;

   /* SYS_thr_kill                433 */
   GO(SYS_thr_kill, "2s 0m");
   SY(SYS_thr_kill, x0-10, x0-20); FAIL;

#if (FREEBSD_VERS <= FREEBSD_10)

   /* @todo PJF (maybe) FreeBSD 10 or earlier, hmmm */
   // BSDXY(__NR__umtx_lock,       sys__umtx_lock),        // 434

   // BSDXY(__NR__umtx_unlock,     sys__umtx_unlock),      // 435
#endif

   /* SYS_jail_attach             436 */
   GO(SYS_jail_attach, "1s 0m");
   SY(SYS_jail_attach, x0-1); FAIL;

   /* SYS_extattr_list_fd         437 */
   GO(SYS_extattr_list_fd, "4s 1m");
   SY(SYS_extattr_list_fd, x0+999999, x0+1, x0+1, x0+16); FAIL;

   /* SYS_extattr_list_file       438 */
   GO(SYS_extattr_list_file, "4s 2m");
   SY(SYS_extattr_list_file, x0+1, x0+1, x0+1, x0+16); FAIL;

   /* SYS_extattr_list_link       439 */
   GO(SYS_extattr_list_link, "4s 2m");
   SY(SYS_extattr_list_link, x0+1, x0+1, x0+1, x0+16); FAIL;


   // kse_switchin                                         440

   // ksem_timedwait                                       441

   // thr_suspend                                          442

   /* SYS_thr_wake                443 */
   GO(SYS_thr_wake, "1s 0m");
   SY(SYS_thr_wake, x0+99); FAIL;

   /* SYS_kldunloadf              444 */
   GO(SYS_kldunloadf, "1s 0m");
   SY(SYS_kldunloadf, x0+1, x0); FAIL;

   // audit                                                445

   // auditon                                              446

   // getauid                                              447

   // setauid                                              448

   // getaudit                                             449

   // setaudit                                             450

   // getaudit_addr                                        451

   // setaudit_addr                                        452

   // auditctl                                             453

// int _umtx_op(void *obj, int op, u_long val, void *uaddr, void *uaddr2);

   /* SYS__umtx_op                 454 */
   GO(SYS__umtx_op, "5s 2m");
   SY(SYS__umtx_op, x0+1, x0+15, x0+3, x0+4, x0+5); FAIL;

   /* SYS_thr_new                 455 */
   /* @todo needs some special testing, VG hangs if we go spawning threads willy-nilly */
   /*
   GO(SYS_thr_new, "2s 1m");
   SY(SYS_thr_new, x0+1, x0+2); FAIL;
   */

   /* SYS_sigqueue                456 */
   GO(SYS_sigqueue, "3s 0m");
   SY(SYS_sigqueue, x0+9999999, x0+2000, x0); FAIL;

   /* SYS_kmq_open                457 */
   GO(SYS_kmq_open, "4s 2m");
   SY(SYS_kmq_open, x0+1, x0+O_CREAT, x0, x0+1); FAIL;

   GO(SYS_kmq_open, "3s 1m");
   SY(SYS_kmq_open, x0+1, x0, x0); FAIL;

   {
       struct mq_attr mqa = { x0+1, x0+1 };
       GO(SYS_kmq_open, "3s 2m");
       SY(SYS_kmq_open, x0+1, x0+O_CREAT, x0, &mqa); FAIL;
   }

   /* SYS_kmq_setattr             458 */
   GO(SYS_kmq_setattr, "3s 2m");
   SY(SYS_kmq_setattr, x0+1, x0+2, x0+3); FAIL;

   /* SYS_kmq_timedreceive        459 */
   GO(SYS_kmq_timedreceive, "5s 2m");
   SY(SYS_kmq_timedreceive, x0+1, x0+2, x0+3, x0+4, x0+5); FAIL;

   /* SYS_kmq_timedsend           460 */
   GO(SYS_kmq_timedsend, "5s 1m");
   SY(SYS_kmq_timedsend, x0+1, x0+2, x0+3, x0+4, x0+5); FAIL;

   /* SYS_kmq_notify              461 */
   GO(SYS_kmq_notify, "2s 1m");
   SY(SYS_kmq_notify, x0+1, x0+2); FAIL;

   /* SYS_kmq_unlink              462 */
   GO(SYS_kmq_unlink, "1s 1m");
   SY(SYS_kmq_unlink, x0+1); FAIL;

   /* abort2                      463 */
   GO(SYS_abort2, "other");

   /* SYS_thr_set_name            464 */
   /* @todo PJF VG doesn't like this. Causes a SIGSEGV. Runs OK standalone */
   /*
   GO(SYS_thr_set_name, "2s 1m");
   SY(SYS_thr_set_name, x0+999999, x0+2); FAIL;
   */

   /* aio_fsync                   465 */
   GO(SYS_aio_fsync, "2s 1m");
   SY(SYS_aio_fsync, x0+1, x0+2); FAIL;

   /* SYS_rtprio_thread           466 */
   GO(SYS_rtprio_thread, "3s 1m");
   SY(SYS_rtprio_thread, x0+1, x0-2, x0+1); FAIL;

   // __getpath_fromfd            469

   // __getpath_fromaddr          470

   // sctp_peeloff                471

   // sctp_generic_sendmsg        472
   GO(SYS_sctp_generic_sendmsg, "7s 1m");
   SY(SYS_sctp_generic_sendmsg, x0+1, x0+2, x0+3, x0+4, x0+5, x0+6, x0+7); FAIL;

   // sctp_generic_sendmsg_iov    473

   // sctp_generic_recvmsg        474
   GO(SYS_sctp_generic_recvmsg, "7s 4m");
   SY(SYS_sctp_generic_recvmsg, x0+1, x0+2, x0+300, x0+4, x0+5, x0+6, x0+7); FAIL;

   {
      socklen_t fromlen = 64;
      struct iovec iov;
      GO(SYS_sctp_generic_recvmsg, "6s 4m");
      SY(SYS_sctp_generic_recvmsg, x0+1, x0+2, x0+300, x0+4, &fromlen, x0+6, x0+7); FAIL;

      iov.iov_base = (void*)(x0+8);
      iov.iov_len = x0+9;

      GO(SYS_sctp_generic_recvmsg, "6s 6m");
      SY(SYS_sctp_generic_recvmsg, x0+1, &iov, 1, x0+4, x0+5, x0+6, x0+7); FAIL;
   }

   /* SYS_pread                   475 */
   GO(SYS_pread, "4s 1m");
   SY(SYS_pread, x0+99, x0+1, x0+1, x0+2); FAIL;

   /* SYS_pwrite                  476 */
   GO(SYS_pwrite, "4s 1m");
   SY(SYS_pwrite, x0+99, x0+1, x0+1, x0+2); FAIL;

   /* SYS_mmap                    477 */
   GO(SYS_mmap, "6s 1m");
   SY(SYS_mmap, x0+1, x0, x0+123456, x0+234567, x0+99, x0+3); FAIL;

   /* SYS_lseek                   478 */
#if defined(VGP_amd64_freebsd)
   GO(SYS_lseek, "3s 0m");
   SY(SYS_lseek, x0+99, x0+1, x0+55); FAIL;
#else
   GO(SYS_lseek, "4s 0m");
   SY(SYS_lseek, x0+99, x0+1, x0+1, x0+55); FAIL;
#endif

   /* SYS_truncate                479 */
#if defined(VGP_amd64_freebsd)
   GO(SYS_truncate, "2s 1m");
   SY(SYS_truncate, x0+1, x0+1); FAIL;
#else
   GO(SYS_truncate, "3s 1m");
   SY(SYS_truncate, x0+1, x0, x0+1); FAIL;
#endif

   /* SYS_ftruncate               480 */
#if defined(VGP_amd64_freebsd)
   GO(SYS_ftruncate, "2s 0m");
   SY(SYS_ftruncate, x0+99, x0+1); FAIL;
#else
   GO(SYS_ftruncate, "3s 0m");
   SY(SYS_ftruncate, x0+99, x0, x0+1); FAIL;
#endif

   /* SYS_thr_kill2               481 */
   GO(SYS_thr_kill2, "3s 0m");
   SY(SYS_thr_kill2, x0-1, x0-1, x0+9999); FAIL;

   /* SYS_shm_open                482 */
#if (FREEBSD_VERS >= FREEBSD_13_0)
   GO(SYS_freebsd12_shm_open, "(SHM_ANON) 3s 0m");
   SY(SYS_freebsd12_shm_open, x0+SHM_ANON, x0+2, x0+9); SUCC;
#else
   GO(SYS_shm_open, "(SHM_ANON) 3s 0m");
   SY(SYS_shm_open, x0+SHM_ANON, x0+2, x0+9); SUCC;
#endif

   // @todo this was causing a VG crash
   // GO(SYS_shm_open, "3s 1m");
   //SY(SYS_shm_open, x0+2, x0+2, x0+9); SUCC;

   /* SYS_shm_unlink              483 */
   GO(SYS_shm_unlink, "1s 1m");
   SY(SYS_shm_unlink, x0+1); FAIL;

   /* cpuset                      484 */
   GO(SYS_cpuset, "1s 1m");
   SY(SYS_cpuset, x0+1); FAIL;

   /* cpuset_setid                485 */
#if defined (VGP_amd64_freebsd)
   GO(SYS_cpuset_setid, "3s 0m");
   SY(SYS_cpuset_setid, x0, x0, x0); FAIL;
#else
   GO(SYS_cpuset_setid, "4s 0m");
   SY(SYS_cpuset_setid, x0, x0, x0, x0); FAIL;
#endif

   /* cpuset_getid                486 */
   GO(SYS_cpuset_getid, "4s 1m");
   SY(SYS_cpuset_getid, x0, x0, x0, x0+1); FAIL;

   /* SYS_cpuset_getaffinity      487 */
   GO(SYS_cpuset_getaffinity, "5s 1m");
   SY(SYS_cpuset_getaffinity, x0+100, x0+100, x0+200, x0+500, x0+1); FAIL;

   /* SYS_cpuset_setaffinity      488 */
   GO(SYS_cpuset_setaffinity, "5s 1m");
   SY(SYS_cpuset_setaffinity, x0+100, x0+100, x0+200, x0+500, x0+1); FAIL;

   /* SYS_faccessat               489 */
   GO(SYS_faccessat, "3s 1m");
   SY(SYS_faccessat, x0+1, x0, x0); FAIL;

   /* SYS_fchmodat                490 */
   GO(SYS_fchmodat, "4s 1m");
   SY(SYS_fchmodat, x0, x0+1, x0, x0); FAIL;

   /* SYS_fchownat                491 */
   GO(SYS_fchownat, "5s 1m");
   SY(SYS_fchownat, x0, x0+1, x0, x0, x0); FAIL;

   /* SYS_fexecve                 492 */
   GO(SYS_fexecve, "3s 2m");
   SY(SYS_fexecve, x0-1, x0+1, x0+1); FAIL;

   /* SYS_freebsd11_fstatat       493 */
#if (FREEBSD_VERS >= FREEBSD_12)
   GO(SYS_freebsd11_fstatat, "4s 2m");
   SY(SYS_freebsd11_fstatat, x0, x0+1, x0+1, x0); FAIL;
#else
   GO(SYS_fstatat, "4s 2m");
   SY(SYS_fstatat, x0, x0+1, x0+1, x0); FAIL;
#endif

   /* SYS_futimesat               494 */
   GO(SYS_futimesat, "3s 2m");
   SY(SYS_futimesat, x0, x0+1, x0+1); FAIL;

   /* SYS_linkat                  495 */
   GO(SYS_linkat, "5s 2m");
   SY(SYS_linkat, x0, x0+1, x0, x0+1, x0); FAIL;

   /* SYS_mkdirat                 496 */
   GO(SYS_mkdirat, "3s 1m");
   SY(SYS_mkdirat, x0, x0+1, x0+1); FAIL;

   /* SYS_mkfifoat                497 */
   GO(SYS_mkfifoat, "3s 1m");
   SY(SYS_mkfifoat, x0, x0, x0); FAIL;

   /* SYS_freebsd11_mknodat       498 */
#if (FREEBSD_VERS >= FREEBSD_12)
   GO(SYS_freebsd11_mknodat, "4s 1m");
   SY(SYS_freebsd11_mknodat, x0, x0+1, x0, x0); FAIL;
#else
   GO(SYS_mknodat, "4s 1m");
   SY(SYS_mknodat, x0, x0+1, x0, x0); FAIL;
#endif

   /* SYS_openat                  499 */
   GO(SYS_openat, "3s 1m");
   SY(SYS_openat, x0, x0+1, x0); FAIL;

   GO(SYS_openat, "4s 1m");
   SY(SYS_openat, x0, x0+1, x0+O_CREAT, x0); FAIL;

   /* SYS _readlinkat             500 */
   GO(SYS_readlinkat, "4s 2m");
   SY(SYS_readlinkat, x0, x0+1, x0+1, x0+4); FAIL;

   /* SYS_renameat                501 */
   GO(SYS_renameat, "4s 2m");
   SY(SYS_renameat, x0, x0+1, x0+1, x0+1); FAIL;

   /* SYS_symlinkat               502 */
   GO(SYS_symlinkat, "3s 2m");
   SY(SYS_symlinkat, x0+1, x0, x0+1); FAIL;

   /* SYS_unlinkat                503 */
   GO(SYS_unlinkat, "3s 1m");
   SY(SYS_unlinkat, x0+9999999, x0+1, x0); FAIL;

   /* SYS_posix_openpt            504 */
   GO(SYS_posix_openpt, "1s 1m");
   SY(SYS_posix_openpt, x0+0x8); FAIL;

   // gssd_syscall                505

   /* SYS_jail_get                506 */
   GO(SYS_jail_get, "3s 1m");
   SY(SYS_jail_get, x0+1, x0+10, x0); FAIL;

   /* SYS_jail_set                507 */
   GO(SYS_jail_set, "3s 1m");
   SY(SYS_jail_set, x0+1, x0+10, x0); FAIL;

   /* SYS_jail_remove             508 */
   GO(SYS_jail_remove, "1s 0m");
   SY(SYS_jail_remove, x0+1); FAIL;

   /* SYS_closefrom               509 */
#if (FREEBSD_VERS >= FREEBSD_13_0)
   GO(SYS_freebsd12_closefrom, "1s 0m");
   SY(SYS_freebsd12_closefrom, x0+100000); SUCC;
#else
   GO(SYS_closefrom, "1s 0m");
   SY(SYS_closefrom, x0+100000); SUCC;
#endif

   /* SYS___semctl                510 */
   GO(SYS___semctl, "(IPC_INFO) 4s 1m");
   SY(SYS___semctl, x0, x0, x0+IPC_INFO, x0+1); FAIL;

   GO(SYS___semctl, "(other) 3s 0m");
   SY(SYS___semctl, x0, x0, x0+3000, x0+1); FAIL;

   /* msgctl                      511 */
   GO(SYS_msgctl, "3s 1m");
   SY(SYS_msgctl, x0, x0+IPC_STAT, x0+1); FAIL;

   /* SYS_shmctl                  512 */
   GO(SYS_shmctl, "3s 1m");
   SY(SYS_shmctl, x0, x0+IPC_STAT, x0+1); FAIL;

    /* lpathconf                  513 */
    GO(SYS_lpathconf, "2s 1m");
    SY(SYS_lpathconf, x0+1, x0); FAIL;

    // 514 is obsolete cap_new

    /* __cap_rights_get           515 */
    GO(SYS___cap_rights_get, "3s 1m");
    SY(SYS___cap_rights_get, x0+1, x0, x0+1); FAIL;

    /* SYS_cap_enter              516 */
    GO(SYS_cap_enter, "other");

    /* SYS_cap_getmode            517 */
    GO(SYS_cap_getmode, "1s 1m");
    SY(SYS_cap_getmode, x0+1); FAIL;

    /* SYS_pdfork                 518 */
    GO(SYS_pdfork, "other");

    /* SYS_pdkill                 519 */
    GO(SYS_pdkill, "2s 0m");
    SY(SYS_pdkill, x0-1, x0+1000); FAIL;

    /* SYS_pdgetpid               520 */
    GO(SYS_pdgetpid, "2s 1m");
    SY(SYS_pdgetpid, x0+100000, x0+1); FAIL;

    /* SYS_pselect                522 */
    GO(SYS_pselect, "6s 5m");
    SY(SYS_pselect, x0+64, x0+1, x0+2, x0+3, x0+4, x0+5); FAIL;

    /* SYS_getloginclass          523 */
    GO(SYS_getloginclass, "2s 1m");
    SY(SYS_getloginclass, x0+1, x0+16); FAIL;

    /* SYS_setloginclass          524 */
    GO(SYS_setloginclass, "1s 1m");
    SY(SYS_setloginclass, x0+1); FAIL;

    /* SYS_rctl_get_racct         525 */
    GO(SYS_rctl_get_racct, "4s 2m");
    SY(SYS_rctl_get_racct, x0+1, x0+1, x0+2, x0+16); FAIL;

    /* SYS_rctl_get_rules         526 */
    GO(SYS_rctl_get_rules, "4s 2m");
    SY(SYS_rctl_get_rules, x0+1, x0+1, x0+2, x0+16); FAIL;

    /* SYS_rctl_get_limits        527 */
    GO(SYS_rctl_get_limits, "4s 2m");
    SY(SYS_rctl_get_limits, x0+1, x0+1, x0+2, x0+16); FAIL;

    /* SYS_rctl_add_rule          528 */
    /* note arg3 and ar4 are not used as per the manpage */
    GO(SYS_rctl_add_rule, "2s 1m");
    SY(SYS_rctl_add_rule, x0+1, x0+1, x0+2, x0+16); FAIL;

    /* SYS_rctl_remove_rule       529 */
    GO(SYS_rctl_remove_rule, "2s 1m");
    /* note arg3 and ar4 are not used as per the manpage */
    SY(SYS_rctl_remove_rule, x0+1, x0+1, x0+2, x0+16); FAIL;

    /* SYS_posix_fallocate        530 */
#if defined(VGP_amd64_freebsd)
    GO(SYS_posix_fallocate, "3s 0m");
    SY(SYS_posix_fallocate, x0+99999, x0+10, x0+20); SUCC;
#else
    GO(SYS_posix_fallocate, "5s 0m");
    SY(SYS_posix_fallocate, x0+99999, x0, x0+10, x0, x0+20); SUCC;
#endif

    /* SYS_posix_fadvise          531 */
    GO(SYS_posix_fadvise, "4s 0m");
    SY(SYS_posix_fadvise, x0+99999, x0+10, x0+20, x0); SUCC;

    /* SYS_wait6                  532 */
    GO(SYS_wait6, "6s 3m");
    SY(SYS_wait6, x0, x0, x0+1, x0, x0+1, x0+1); FAIL;

    /* SYS_cap_rights_limit       533 */
    GO(SYS_cap_rights_limit, "2s 1m");
    SY(SYS_cap_rights_limit, x0+99999, x0+1); FAIL;

    /* SYS_cap_ioctls_limit       534 */
    GO(SYS_cap_ioctls_limit, "3s 1m");
    SY(SYS_cap_ioctls_limit, x0+99999, x0+1, x0+2); FAIL;

    /* cap_ioctls_get             535 */
    GO(SYS_cap_ioctls_get, "3s 1m");
    SY(SYS_cap_ioctls_get, x0+99999, x0+1, x0+10); FAIL;

    /* SYS_cap_fcntls_limit       536 */
    GO(SYS_cap_fcntls_limit, "2s 0m");
    SY(SYS_cap_fcntls_limit, x0+99999, x0); FAIL;

    /* SYS_cap_fcntls_get         537 */
    GO(SYS_cap_fcntls_get, "2s 1m");
    SY(SYS_cap_fcntls_get, x0+99999, x0+1); FAIL;

    /* SYS_bindat                 538 */
    GO(SYS_bindat, "4s 1m");
    SY(SYS_bindat, x0+99999, x0+1, x0+2, x0+16); FAIL;

    /* SYS_connectat              539 */
    GO(SYS_connectat, "4s 1m");
    SY(SYS_connectat, x0+99999, x0+1, x0+2, x0+16); FAIL;

    /* SYS_chflagsat              540*/
    GO(SYS_chflagsat, "4s 1m");
    SY(SYS_chflagsat, x0+99999, x0+1, x0+2, x0+3); FAIL;

   /* SYS_accept4                 541 */
   GO(SYS_accept4, "4s 1m");
   SY(SYS_accept4, x0+999999, x0+1, x0+16, x0); FAIL;

   {
      socklen_t socklen = 42;
      GO(SYS_accept4, "3s 1m");
      SY(SYS_accept4, x0+999999, x0+1, &socklen, x0); FAIL;
   }

   /* SYS_pipe2                   542 */
   GO(SYS_pipe2, "2s 1m");
   SY(SYS_pipe2, x0+1, x0); FAIL;

    /* SYS_aio_mlock              543*/
    GO(SYS_aio_mlock, "1s 1m");
    SY(SYS_aio_mlock, x0+1); FAIL;

    /* SYS_procctl                544 */
#if defined(VGP_amd64_freebsd)
    GO(SYS_procctl, "(PROC_REAP_RELEASE) 3s 0m");
    SY(SYS_procctl, x0+9999, x0+9999, x0+PROC_REAP_RELEASE); FAIL;

    GO(SYS_procctl, "(PROC_REAP_GETPIDS) 4s 1m");
    SY(SYS_procctl, x0+9999, x0+9999, x0+PROC_REAP_GETPIDS, x0+1); FAIL;
#else
    GO(SYS_procctl, "(PROC_REAP_RELEASE) 4s 0m");
    SY(SYS_procctl, x0+9999, x0, x0+9999, x0+PROC_REAP_RELEASE); FAIL;

    GO(SYS_procctl, "(PROC_REAP_GETPIDS) 5s 1m");
    SY(SYS_procctl, x0+9999, x0, x0+9999, x0+PROC_REAP_GETPIDS, x0+1); FAIL;
#endif

    // 544 is the highest syscall on FreeBSD 9

#if (FREEBSD_VERS >= FREEBSD_10)

   /* SYS_ppoll                   545 */
   GO(SYS_ppoll, "4s 2m");
   SY(SYS_ppoll, x0+8, x0+1, x0+1, x0+1); FAIL;

   {
       struct pollfd arg1;
       arg1.fd = arg1.events = arg1.revents = x0;
       GO(SYS_ppoll, "2s 2+2m");
       SY(SYS_ppoll, &arg1, 1, x0+1, x0+1); FAIL;
   }

   /* SYS_futimens                546 */
   GO(SYS_futimens, "2s 1m");
   SY(SYS_futimens, x0+99999999, x0+1); FAIL;

   /* SYS_utimensat               547 */
   GO(SYS_utimensat, "4s 2m");
   SY(SYS_utimensat, x0+99999999, x0+1, x0+1, x0); FAIL;

#endif // FREEBSD_VERS >= FREEBSD_11

#if (FREEBSD_VERS >= FREEBSD_11)

    // 548 is obsolete numa_getaffinity

    // 549 is obsolete numa_setaffinity

   /* SYS_fdatasync              550 */
   GO(SYS_fdatasync, "1s 0m");
   SY(SYS_fdatasync, x0+99999999); FAIL;

#endif // FREEBSD_VERS >= FREEBSD_11

#if (FREEBSD_VERS >= FREEBSD_12)

   /* SYS_fstat                   551 */
   GO(SYS_fstat, "2s 1m");
   SY(SYS_fstat, x0+99999999, x0+1); FAIL;

   /* SYS_fstatat                552 */
   GO(SYS_fstatat, "4s 2m");
   SY(SYS_fstatat, x0+99999999, x0+1, x0+1, x0); FAIL;

   /* SYS_fhstat                 553 */
   GO(SYS_fhstat, "2s 2m");
   SY(SYS_fhstat, x0+1, x0+1); FAIL;

   /* SYS_getdirentries           554 */
   GO(SYS_getdirentries, "4s 2m");
   SY(SYS_getdirentries, x0+999999, x0+1, x0+16, x0+1); FAIL;

   /* SYS_statfs                 555 */
   GO(SYS_statfs, "2s 2m");
   SY(SYS_statfs, x0+1, x0+1); FAIL;

   /* SYS_fstatfs                556 */
   GO(SYS_fstatfs, "2s 1m");
   SY(SYS_fstatfs, x0+999999, x0+1); FAIL;

   /* SYS_getfsstat              557 */
   GO(SYS_getfsstat, "3s 1m");
   SY(SYS_getfsstat, x0+999999, x0+1, x0); FAIL;

   /* SYS_fhstatfs               558 */
   GO(SYS_fhstatfs, "2s 2m");
   SY(SYS_fhstatfs, x0+1, x0+1); FAIL;

   /* SYS_mknodat                559 */
   GO(SYS_mknodat, "4s 1m");
   SY(SYS_mknodat, x0+999999, x0+1, x0, x0); FAIL;

   /* SYS_kevent                 560 */
   GO(SYS_kevent, "6s 3m");
   SY(SYS_kevent, x0+1, x0+2, x0+3, x0+4, x0+5, x0+6); FAIL;

   /* SYS_cpuset_getdomain        561 */
   GO(SYS_cpuset_getdomain, "6s 2m");
   SY(SYS_cpuset_getdomain, x0+1, x0+2, x0+3, x0+4, x0+5, x0+6); FAIL;

   /* SYS_cpuset_setdomain        562 */
   GO(SYS_cpuset_setdomain, "6s 1m");
   SY(SYS_cpuset_setdomain, x0+1, x0+2, x0+3, x0+4, x0+5, x0+6); FAIL;

   /* SYS_getrandom               563 */
   GO(SYS_getrandom, "3s 1m");
   SY(SYS_getrandom, x0+1, x0+1, x0); FAIL;

   /* SYS_getfhat                 564 */
   GO(SYS_getfhat, "4s 2m");
   SY(SYS_getfhat, x0, x0, x0, x0); FAIL;

   /* SYS_fhlink                  565 */
   GO(SYS_fhlink, "2s 2m");
   SY(SYS_fhlink, x0+1, x0+1);

   /* SYS_fhlinkat                566 */
   GO(SYS_fhlinkat, "3s 2m");
   SY(SYS_fhlinkat, x0+1, x0+1000, x0+1);

   /* SYS_fhreadlink              567 */
   GO(SYS_fhreadlink, "3s 2m");
   SY(SYS_fhreadlink, x0+1, x0+1, x0+10);

#endif

#if (FREEBSD_VERS >= FREEBSD_12_2)

      /* SYS___sysctlbyname       570 */
   GO(SYS___sysctlbyname, "(getoldlen) 3s 2m");
   SY(SYS___sysctlbyname, x0, x0+1, NULL, x0+1, NULL, x0); FAIL;

   GO(SYS___sysctlbyname, "(getold) 4s 2m");
   SY(SYS___sysctlbyname, x0, x0+1, x0+1, x0+1, NULL, x0); FAIL;

   GO(SYS___sysctlbyname, "(putnew) 4s 2m");
   SY(SYS___sysctlbyname, x0, x0+1, NULL, NULL, x0+1, x0+2); FAIL;

#endif

   /* SYS_exit                    1 */
   GO(SYS_exit, "1s 0m");
   SY(SYS_exit, x0); FAIL;
}
