/* x86 variant of the amd64-solaris/context_gpr.c test. */

#include <assert.h>
#include <signal.h>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <sys/regset.h>
#include <sys/syscall.h>
#include <ucontext.h>

static siginfo_t si;
static ucontext_t uc;
/* x0 is always zero, but is visible to Valgrind as uninitialised. */
static int x0;

static void sighandler(int sig, siginfo_t *sip, void *arg)
{
   ucontext_t *ucp = (ucontext_t *) arg;

   si = *sip;
   uc = *ucp;

   ucp->uc_mcontext.gregs[ECX] = x0;
}

int main(void)
{
   struct sigaction sa;
   pid_t pid;
   int eax, ebx, ecx, edx, esi, edi;
   int y0;

   /* Uninitialised, but we know px[0] is 0x0. */
   int *px = malloc(sizeof(*px));
   x0 = px[0];

   /* Uninitialised, but we know py[0] is 0x0. */
   int *py = malloc(sizeof(*py));
   y0 = py[0];

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
      /* Set values in general purpose registers. */
      "movl   %[y0], %%ebx\n"
      "movl   $0xf1, %%ecx\n"
      "movl   $0xf2, %%edx\n"
      "movl   $0xf3, %%esi\n"
      "movl   $0xf4, %%edi\n"

      /* Prepare syscall parameters. */
      "pushl  %[sig]\n"
      "pushl  %[pid]\n"
      "pushl  $0xdeadbeef\n"
      "movl   %[scall], %%eax\n"

      /* Trigger the signal handler. */
      "int    $0x91\n"
      "addl   $12, %%esp\n"
      : "=a" (eax), "=b" (ebx), "=c" (ecx), "=d" (edx), "=S" (esi),
        "=D" (edi)
      : [scall] "i" (SYS_kill), [pid] "a" (pid), [sig] "i" (SIGUSR1),
        [y0] "m" (y0)
      : "cc", "memory");

   printf("Values in the signal handler:\n");
   printf("  eax=%#x, edx=%#x, esi=%#x, edi=%#x\n",
          uc.uc_mcontext.gregs[EAX], uc.uc_mcontext.gregs[EDX],
          uc.uc_mcontext.gregs[ESI], uc.uc_mcontext.gregs[EDI]);
   /* Check that ebx contains an uninitialised value (origin is py[0]). */
   if (uc.uc_mcontext.gregs[EBX])
      assert(0);

   printf("Values after the return from the signal handler:\n");
   printf("  eax=%#x, edx=%#x, esi=%#x, edi=%#x\n", eax, edx, esi, edi);
   /* Check that ebx and ecx contain uninitialised values (origin is py[0]
      and px[0], respectively). */
   if (ebx || ecx)
      assert(0);

   return 0;
}

