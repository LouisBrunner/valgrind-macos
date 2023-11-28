/* x86 variant of the amd64-solaris/context_rflags.c test. */

#include <assert.h>
#include <signal.h>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <sys/regset.h>
#include <sys/syscall.h>
#include <ucontext.h>

#define OBIT(eflags) (!!((eflags) & (1 << 11)))
#define SBIT(eflags) (!!((eflags) & (1 << 7)))

static siginfo_t si;
static ucontext_t uc;

static void sighandler(int sig, siginfo_t *sip, void *arg)
{
   si = *sip;
   uc = *((ucontext_t *) arg);
}

int main(void)
{
   struct sigaction sa;
   pid_t pid;
   int eflags;

   sa.sa_sigaction = sighandler;
   sa.sa_flags = SA_SIGINFO;
   if (sigfillset(&sa.sa_mask)) {
      perror("sigfillset");
      return 1;
   }
   if (sigaction(SIGUSR1, &sa, NULL)) {
      perror("sigaction");
      return 1;
   }

   pid = getpid();

   __asm__ __volatile__(
      /* Set overflow and sign flags. */
      "movl   $1, %%edx\n"
      "addl   $0x7fffffff, %%edx\n"

      /* Prepare syscall parameters. */
      "pushl  %[sig]\n"
      "pushl  %[pid]\n"
      "pushl  $0xdeadbeef\n"
      "movl   %[scall], %%eax\n"

      /* Trigger the signal handler. */
      "int    $0x91\n"
      "pushfl\n"
      "popl   %%edx\n"
      "addl   $12, %%esp\n"
      : "=d" (eflags)
      : [scall] "i" (SYS_kill), [pid] "a" (pid), [sig] "i" (SIGUSR1)
      : "cc", "memory");

   printf("Values in the signal handler:\n");
   printf("  overflow=%d, sign=%d\n",
          OBIT(uc.uc_mcontext.gregs[EFL]), SBIT(uc.uc_mcontext.gregs[EFL]));

   printf("Values after the return from the signal handler:\n");
   printf("  overflow=%d, sign=%d\n", OBIT(eflags), SBIT(eflags));

   return 0;
}

