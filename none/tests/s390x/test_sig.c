#include <features.h>
#include <fpu_control.h>
#include <signal.h>
#include <sys/types.h>
#include <signal.h>
#include <stdio.h>
#include <stdlib.h>
#include <ucontext.h>
#include <unistd.h>

void handle_SIG(int sig)
{
   double d;

   _FPU_SETCW(0);
   d = 7;
   asm volatile ("":: "f" (d));
   printf("Got signal %d\n", sig);
   if (sig == SIGSEGV) {
      printf("SIGSEGV, exiting...\n");
      exit(0);
   }
}

void handle_rt_SIG(int sig, siginfo_t *info, void *uc)
{
   double d;

   _FPU_SETCW(0);
   d = 8;
   asm volatile ("":: "f" (d));
   printf("Got signal %d\n", sig);
   printf("si_signo: %d\n", info->si_signo);
   printf("si_errno: %d\n", info->si_errno);
   printf("si_code: %d\n", info->si_code);
   if (sig == SIGSEGV) {
      printf("SIGSEGV, exiting...\n");
      exit(0);
   }
}

int main(void)
{
   //   char *a;
   struct sigaction sa;
   double d1,d2,d3,d4,d5;

   _FPU_SETCW(1);
   d1 = d2 = d3 = d4 = d5 = 1;
   sa.sa_sigaction=handle_rt_SIG;
   sa.sa_flags =SA_SIGINFO;
   sigemptyset(&sa.sa_mask);
   sigaction(SIGALRM, &sa, NULL);
   signal(SIGUSR1, handle_SIG);
   signal(SIGSEGV, handle_SIG);
   kill(getpid(), SIGALRM);
   printf("One!\n");
   kill(getpid(), SIGUSR1);
   printf("floating point is now: %f %f %f %f %f\n", d1, d2, d3, d4, d5);
   {
      int fpc;
      _FPU_GETCW(fpc);
      printf("fpc= %d\n", fpc);
   }
   printf("Good Bye!\n");
//	a = (char *) 0x12345678;
//	*a = 1;
   exit(0);
}
