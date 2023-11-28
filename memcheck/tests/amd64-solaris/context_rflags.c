/* Test if rflags values are correctly propagated in and out of a signal
   handler.  Note that we actually test only the propagation of the overflow
   and sign flags. */

#include <assert.h>
#include <signal.h>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <sys/regset.h>
#include <sys/syscall.h>
#include <ucontext.h>

#define OBIT(rflags) (!!((rflags) & (1 << 11)))
#define SBIT(rflags) (!!((rflags) & (1 << 7)))

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
   long rflags;

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

      /* Trigger the signal handler. */
      "syscall\n"
      "pushfq\n"
      "popq   %%rdx\n"
      : "=d" (rflags)
      : "a" (SYS_kill), "D" (pid), "S" (SIGUSR1)
      : "cc", "memory");

   printf("Values in the signal handler:\n");
   printf("  overflow=%d, sign=%d\n",
          OBIT(uc.uc_mcontext.gregs[REG_RFL]),
          SBIT(uc.uc_mcontext.gregs[REG_RFL]));

   printf("Values after the return from the signal handler:\n");
   printf("  overflow=%d, sign=%d\n", OBIT(rflags), SBIT(rflags));

   return 0;
}

