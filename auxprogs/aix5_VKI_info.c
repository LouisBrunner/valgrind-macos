
/* Used to generate include/vki/vki-ppc{32,64}-aix5.h. */

#include <stdio.h>
#include <stdlib.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <sys/statfs.h>
#include <fcntl.h>
#include <unistd.h>
#include <sys/mman.h>
#include <errno.h>
#include <string.h>
#include <time.h>
#include <sys/ptrace.h>
#include <sys/uio.h>
#include <sys/ioctl.h>

#undef offsetof

/* This is so useful it should be visible absolutely everywhere. */
#if !defined(offsetof)
#   define offsetof(type,memb) ((int)&((type*)0)->memb)
#endif


int main ( void )
{
   printf ("aix5_VKI_info: sizeof(void*) = %d\n", (int)sizeof(void*));

   printf("/* ---------------- Errors ---------------- */\n");
   printf("\n");
   printf("#define VKI_EINVAL %d\n", EINVAL);
   printf("#define VKI_EINTR  %d\n", EINTR);
   printf("#define VKI_ENOSYS %d\n", ENOSYS);
   printf("#define VKI_EAGAIN %d\n", EAGAIN);
   printf("#define VKI_ENOMEM %d\n", ENOMEM);
   printf("#define VKI_EACCES %d\n", EACCES);
   printf("#define VKI_EEXIST %d\n", EEXIST);
   printf("#define VKI_EPERM  %d\n", EPERM);
   printf("#define VKI_ENOENT %d\n", ENOENT);
   printf("#define VKI_ESRCH  %d\n", ESRCH);
   printf("#define VKI_EBADF  %d\n", EBADF);
   printf("#define VKI_EFAULT %d\n", EFAULT);
   printf("#define VKI_EMFILE %d\n", EMFILE);
   printf("#define VKI_ECHILD %d\n", ECHILD);
   printf("#define VKI_EOVERFLOW %d\n", EOVERFLOW);
   printf("\n");
   printf("/* ---------------- File I/O ---------------- */\n");
   printf("\n");
   printf("#define VKI_O_WRONLY 0x%08x\n", O_WRONLY);
   printf("#define VKI_O_RDONLY 0x%08x\n", O_RDONLY);
   printf("#define VKI_O_APPEND 0x%08x\n", O_APPEND);
   printf("#define VKI_O_CREAT  0x%08x\n", O_CREAT);
   printf("#define VKI_O_RDWR   0x%08x\n", O_RDWR);
   printf("#define VKI_O_EXCL   0x%08x\n", O_EXCL);
   printf("#define VKI_O_TRUNC  0x%08x\n", O_TRUNC);
   printf("\n");
   printf("#define VKI_S_IRUSR  0x%08x\n", S_IRUSR);
   printf("#define VKI_S_IXUSR  0x%08x\n", S_IXUSR);
   printf("#define VKI_S_IXGRP  0x%08x\n", S_IXGRP);
   printf("#define VKI_S_IXOTH  0x%08x\n", S_IXOTH);
   printf("#define VKI_S_IWUSR  0x%08x\n", S_IWUSR);
   printf("#define VKI_S_IWOTH  0x%08x\n", S_IWOTH);
   printf("#define VKI_S_ISUID  0x%08x\n", S_ISUID);
   printf("#define VKI_S_ISGID  0x%08x\n", S_ISGID);
   printf("#define VKI_S_IFMT   0x%08x\n", S_IFMT);
   printf("#define VKI_S_IFDIR  0x%08x\n", S_IFDIR);
   printf("#define VKI_S_IFCHR  0x%08x\n", S_IFCHR);
   printf("#define VKI_S_IFBLK  0x%08x\n", S_IFBLK);
   printf("\n");
   printf("#define VKI_F_DUPFD  0x%08x\n", F_DUPFD);
   printf("#define VKI_F_SETFD  0x%08x\n", F_SETFD);
   printf("#define VKI_FD_CLOEXEC  0x%08x\n", FD_CLOEXEC);
   printf("\n");
   printf("#define VKI_R_OK 0x%08x\n", R_OK);
   printf("#define VKI_W_OK 0x%08x\n", W_OK);
   printf("#define VKI_X_OK 0x%08x\n", X_OK);

   /* info about struct stat */
   struct stat st;
   printf("\nsizeof(struct stat) = %d\n", (int)sizeof(struct stat));

   printf("   st_size: off %2d sz %d\n", 
              offsetof(struct stat, st_size),
              (int)sizeof(st.st_size));

   printf("   st_mode: off %2d sz %d\n", 
              offsetof(struct stat, st_mode),
              (int)sizeof(st.st_mode));

   printf("   st_uid: off %2d sz %d\n", 
              offsetof(struct stat, st_uid),
              (int)sizeof(st.st_uid));

   printf("   st_gid: off %2d sz %d\n", 
              offsetof(struct stat, st_gid),
              (int)sizeof(st.st_gid));

   printf("   st_dev: off %2d sz %d\n", 
              offsetof(struct stat, st_dev),
              (int)sizeof(st.st_dev));

   printf("   st_ino: off %2d sz %d\n", 
              offsetof(struct stat, st_ino),
              (int)sizeof(st.st_ino));

   printf("\n");
   printf("#define VKI_STX_NORMAL  %d\n", (int)STX_NORMAL);
   printf("\n");
   printf("sizeof(size_t) = %d\n", (int)sizeof(size_t) );
   printf("\n");
   printf("#define VKI_SEEK_SET %d\n", SEEK_SET);
   printf("#define VKI_PATH_MAX %d\n", PATH_MAX);

   /* info about struct iovec */
   struct iovec iov;
   printf("\nsizeof(struct iovec) = %d\n", (int)sizeof(struct iovec));
   printf("  iov_base: off %2d sz %d\n", 
             offsetof(struct iovec, iov_base),
             (int)sizeof(iov.iov_base));
   printf("  iov_len: off %2d sz %d\n", 
             offsetof(struct iovec, iov_len),
             (int)sizeof(iov.iov_len));

   printf("\n");
   printf("#define _VKI_IOC_NONE %d\n",  3 & (IOC_VOID >> 30));
   printf("#define _VKI_IOC_READ %d\n",  3 & (IOC_OUT >> 30));
   printf("#define _VKI_IOC_WRITE %d\n", 3 & (IOC_IN >> 30));
   printf("\n");
   printf("/* ---------------- MMappery ---------------- */\n");
   printf("\n");
   printf("#define VKI_PAGE_SIZE %d\n", (int)sysconf(_SC_PAGESIZE));
   printf("\n");
   printf("#define VKI_PROT_NONE  0x%08x\n", PROT_NONE);
   printf("#define VKI_PROT_READ  0x%08x\n", PROT_READ);
   printf("#define VKI_PROT_WRITE 0x%08x\n", PROT_WRITE);
   printf("#define VKI_PROT_EXEC  0x%08x\n", PROT_EXEC);
   printf("\n");
   printf("#define VKI_MAP_FIXED     0x%08x\n", MAP_FIXED);
   printf("#define VKI_MAP_PRIVATE   0x%08x\n", MAP_PRIVATE);
   printf("#define VKI_MAP_ANONYMOUS 0x%08x\n", MAP_ANONYMOUS);
   printf("\n");
   printf("/* ---------------- RLimitery ---------------- */\n");
   printf("\n");
   printf("#define VKI_RLIMIT_DATA   0x%08x\n", RLIMIT_DATA);
   printf("#define VKI_RLIMIT_NOFILE 0x%08x\n", RLIMIT_NOFILE);
   printf("#define VKI_RLIMIT_STACK  0x%08x\n", RLIMIT_STACK);
   printf("#define VKI_RLIMIT_CORE   0x%08x\n", RLIMIT_CORE);

   /* info about struct rlimit */
   struct rlimit rl;
   printf("\nsizeof(struct rlimit) = %d\n", (int)sizeof(struct rlimit));
   printf("  rlim_cur: off %2d sz %d\n", 
             offsetof(struct rlimit, rlim_cur),
             (int)sizeof(rl.rlim_cur));
   printf("  rlim_max: off %2d sz %d\n", 
             offsetof(struct rlimit, rlim_max),
             (int)sizeof(rl.rlim_max));
   printf("\n");
   printf("/* ---------------- Time ---------------- */\n");
   printf("\n");

   /* print info about struct timeval */
   struct timeval tv;
   printf("sizeof(struct timeval) = %d\n", (int)sizeof(struct timeval));
   printf("   tv_sec: off %2d sz %d\n", 
              offsetof(struct timeval, tv_sec),
              (int)sizeof(tv.tv_sec));
   printf("  tv_usec: off %2d sz %d\n", 
             offsetof(struct timeval, tv_usec),
             (int)sizeof(tv.tv_usec));

   /* print info about struct timespec */
   struct timespec ts;
   printf("\nsizeof(struct timespec) = %d\n", (int)sizeof(struct timespec));
   printf("   tv_sec: off %2d sz %d\n", 
              offsetof(struct timespec, tv_sec),
              (int)sizeof(ts.tv_sec));
   printf("  tv_nsec: off %2d sz %d\n", 
             offsetof(struct timespec, tv_nsec),
             (int)sizeof(ts.tv_nsec));

   printf("\n");
   printf("/* ---------------- Signals ---------------- */\n");
   printf("\n");
   printf("#define _VKI_NSIG       %ld\n", 8 * sizeof(sigset_t) );
   printf("\n");
   printf("#define VKI_SIGSEGV   %d\n", SIGSEGV);
   printf("#define VKI_SIGBUS    %d\n", SIGBUS);
   printf("#define VKI_SIGFPE    %d\n", SIGFPE);
   printf("#define VKI_SIGHUP    %d\n", SIGHUP);
   printf("#define VKI_SIGINT    %d\n", SIGINT);
   printf("#define VKI_SIGQUIT   %d\n", SIGQUIT);
   printf("#define VKI_SIGABRT   %d\n", SIGABRT);
   printf("#define VKI_SIGUSR1   %d\n", SIGUSR1);
   printf("#define VKI_SIGUSR2   %d\n", SIGUSR2);
   printf("#define VKI_SIGPIPE   %d\n", SIGPIPE);
   printf("#define VKI_SIGALRM   %d\n", SIGALRM);
   printf("#define VKI_SIGTERM   %d\n", SIGTERM);
   printf("/* VKI_SIGSTKFLT does not exist on AIX 5.2 */\n");
   printf("#define VKI_SIGTTIN   %d\n", SIGTTIN);
   printf("#define VKI_SIGTTOU   %d\n", SIGTTOU);
   printf("#define VKI_SIGXCPU   %d\n", SIGXCPU);
   printf("#define VKI_SIGXFSZ   %d\n", SIGXFSZ);
   printf("#define VKI_SIGVTALRM %d\n", SIGVTALRM);
   printf("#define VKI_SIGPROF   %d\n", SIGPROF);
   printf("#define VKI_SIGIO     %d\n", SIGIO);
   printf("#define VKI_SIGPWR    %d\n", SIGPWR);
   printf("/* VKI_SIGUNUSED does not exist on AIX 5.2 */\n");
   printf("#define VKI_SIGRTMIN  %d\n", SIGRTMIN);
   printf("#define VKI_SIGRTMAX  %d\n", SIGRTMAX);
   printf("#define VKI_SIGTRAP   %d\n", SIGTRAP);
   printf("#define VKI_SIGCONT   %d\n", SIGCONT);
   printf("#define VKI_SIGCHLD   %d\n", SIGCHLD);
   printf("#define VKI_SIGWINCH  %d\n", SIGWINCH);
   printf("#define VKI_SIGURG    %d\n", SIGURG);
   printf("#define VKI_SIGILL    %d\n", SIGILL);
   printf("#define VKI_SIGSTOP   %d\n", SIGSTOP);
   printf("#define VKI_SIGKILL   %d\n", SIGKILL);
   printf("#define VKI_SIGTSTP   %d\n", SIGTSTP);
   printf("#define VKI_SIGSYS    %d\n", SIGSYS);

   /* print info about struct sigaction */
   struct sigaction sa;
   printf("\n");
   printf("sizeof(struct sigaction) = %d\n", (int)sizeof(struct sigaction));
   printf("  sa_handler: off %2d sz %d\n", 
             offsetof(struct sigaction, sa_handler),
             (int)sizeof(sa.sa_handler));
   printf("     sa_mask: off %2d sz %d\n", 
                offsetof(struct sigaction, sa_mask),
                (int)sizeof(sa.sa_mask));
   printf("    sa_flags: off %2d sz %d\n", 
               offsetof(struct sigaction, sa_flags),
               (int)sizeof(sa.sa_flags));
   printf("sa_sigaction: off %2d sz %d\n", 
               offsetof(struct sigaction, sa_sigaction),
               (int)sizeof(sa.sa_sigaction));
   printf("\n");
   printf("#define VKI_SA_ONSTACK    %d\n",SA_ONSTACK );
   printf("#define VKI_SA_RESTART    %d\n",SA_RESTART );
   printf("#define VKI_SA_RESETHAND  %d\n",SA_RESETHAND );
   printf("#define VKI_SA_SIGINFO    %d\n",SA_SIGINFO);
   printf("#define VKI_SA_NODEFER    %d\n",SA_NODEFER );
   //  printf("#define VKI_SA_NOMASK %d\n",SA_NOMASK ) ;
   //  printf("#define VKI_SA_ONESHOT %d\n",SA_ONESHOT );
   printf("#define VKI_SA_NOCLDSTOP  %d\n",SA_NOCLDSTOP );
   printf("#define VKI_SA_NOCLDWAIT  %d\n",SA_NOCLDWAIT );
   //  printf("#define VKI_SA_RESTORER %d\n",SA_RESTORER );
   printf("\n");
   printf("#define VKI_SS_ONSTACK %d\n",SS_ONSTACK );
   printf("#define VKI_SS_DISABLE %d\n",SS_DISABLE );
   printf("\n");
   printf("#define VKI_MINSIGSTKSZ %ld\n",MINSIGSTKSZ );
   printf("\n");
   printf("#define VKI_SI_USER  %d\n",SI_USER );
   printf("\n");
   printf("#define VKI_SIG_BLOCK    %d\n",SIG_BLOCK );
   printf("#define VKI_SIG_SETMASK  %d\n",SIG_SETMASK );
   printf("#define VKI_SIG_UNBLOCK  %d\n",SIG_UNBLOCK );
   printf("#define VKI_SIG_IGN      (void*)%d\n",(int)SIG_IGN );
   printf("#define VKI_SIG_DFL      (void*)%d\n",(int)SIG_DFL );
   printf("\n");
   //  printf("#define VKI_SI_TKILL %d\n",SI_TKILL );
   printf("#define VKI_SI_USER  %d\n",SI_USER );
   printf("\n");
   printf("#define VKI_SEGV_ACCERR %d\n", SEGV_ACCERR);
   printf("#define VKI_SEGV_MAPERR %d\n", SEGV_MAPERR);
   printf("\n");
   printf("#define VKI_TRAP_TRACE %d\n", TRAP_TRACE);
   printf("#define VKI_TRAP_BRKPT %d\n", TRAP_BRKPT);
   printf("#define VKI_BUS_OBJERR %d\n", BUS_OBJERR);
   printf("#define VKI_BUS_ADRERR %d\n", BUS_ADRERR);
   printf("#define VKI_BUS_ADRALN %d\n", BUS_ADRALN);
   printf("#define VKI_FPE_FLTSUB %d\n", FPE_FLTSUB);
   printf("#define VKI_FPE_FLTINV %d\n", FPE_FLTINV);
   printf("#define VKI_FPE_FLTRES %d\n", FPE_FLTRES);
   printf("#define VKI_FPE_FLTUND %d\n", FPE_FLTUND);
   printf("#define VKI_FPE_FLTOVF %d\n", FPE_FLTOVF);
   printf("#define VKI_FPE_FLTDIV %d\n", FPE_FLTDIV);
   printf("#define VKI_FPE_INTOVF %d\n", FPE_INTOVF);
   printf("#define VKI_FPE_INTDIV %d\n", FPE_INTDIV);
   printf("\n");
   printf("#define VKI_ILL_BADSTK %d\n", ILL_BADSTK);
   printf("#define VKI_ILL_COPROC %d\n", ILL_COPROC);
   printf("#define VKI_ILL_PRVREG %d\n", ILL_PRVREG);
   printf("#define VKI_ILL_PRVOPC %d\n", ILL_PRVOPC);
   printf("#define VKI_ILL_ILLTRP %d\n", ILL_ILLTRP);
   printf("#define VKI_ILL_ILLADR %d\n", ILL_ILLADR);
   printf("#define VKI_ILL_ILLOPN %d\n", ILL_ILLOPN);
   printf("#define VKI_ILL_ILLOPC %d\n", ILL_ILLOPC);

   /* info about siginfo_t */
   siginfo_t si;
   printf("\nsizeof(siginfo_t) = %d\n", (int)sizeof(siginfo_t));
   printf("   si_signo: off %2d sz %d\n", 
                   offsetof(siginfo_t, si_signo),
                   (int)sizeof(si.si_signo));
   printf("    si_code: off %2d sz %d\n", 
                   offsetof(siginfo_t, si_code),
                   (int)sizeof(si.si_code));
   printf("     si_pid: off %2d sz %d\n",
                   offsetof(siginfo_t, si_pid),
                   (int)sizeof(si.si_pid));
   printf("    si_addr: off %2d sz %d\n", 
                   offsetof(siginfo_t, si_addr),
                   (int)sizeof(si.si_addr));

   /* info about sigaltstack */
   stack_t ss;
   printf("\nsizeof(stack_t) = %d\n", (int)sizeof(stack_t));
   printf("    ss_sp: off %2d sz %d\n", 
                   offsetof(stack_t, ss_sp),
                   (int)sizeof(ss.ss_sp));
   printf("  ss_size: off %2d sz %d\n", 
                   offsetof(stack_t, ss_size),
                   (int)sizeof(ss.ss_size));
   printf(" ss_flags: off %2d sz %d\n", 
                   offsetof(stack_t, ss_flags),
                   (int)sizeof(ss.ss_flags));

   printf("\n");
   printf("/* ---------------- Misc ---------------- */\n");
   printf("\n");
   printf("#define VKI_PTRACE_TRACEME %d\n", PT_TRACE_ME);
   printf("#define VKI_PTRACE_DETACH %d\n", PT_DETACH);
   printf("\n");

#if 0
   printf("#define VKI_  %d\n", );
   printf("#define VKI_  %d\n", );
   printf("#define VKI_  %d\n", );
   printf("#define VKI_  %d\n", );

   printf("#define VKI_  0x%08x\n", );
   printf("#define VKI_  0x%08x\n", );
   printf("#define VKI_  0x%08x\n", );
   printf("#define VKI_  0x%08x\n", );
#endif
   return 0;
}
