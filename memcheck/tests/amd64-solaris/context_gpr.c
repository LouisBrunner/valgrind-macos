/* Test if values in rax, rbx, rcx, rdx, rsi and rdi are correctly propagated
   into and out of a signal handler and also check that the same applies for
   uninitialised values and their origins. */

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
static long x0;

void break_out(void);

static void sighandler(int sig, siginfo_t *sip, void *arg)
{
   ucontext_t *ucp = (ucontext_t *) arg;

   si = *sip;
   uc = *ucp;

   ucp->uc_mcontext.gregs[REG_RCX] = x0;

   /* Break out of the endless loop. */
   *(uintptr_t*)&ucp->uc_mcontext.gregs[REG_RIP] = (uintptr_t)break_out;
}

int main(void)
{
   struct sigaction sa;
   long rax, rbx, rcx, rdx, rsi, rdi;
   long y0;

   /* Uninitialised, but we know px[0] is 0x0. */
   long *px = malloc(sizeof(*px));
   x0 = px[0];

   /* Uninitialised, but we know py[0] is 0x0. */
   long *py = malloc(sizeof(*py));
   y0 = py[0];

   sa.sa_sigaction = sighandler;
   sa.sa_flags = SA_SIGINFO;
   if (sigfillset(&sa.sa_mask)) {
      perror("sigfillset");
      return 1;
   }
   if (sigaction(SIGALRM, &sa, NULL)) {
      perror("sigaction");
      return 1;
   }

   alarm(2);

   __asm__ __volatile__(
      /* Set values in general purpose registers. */
      "movq   $0xf0, %%rax\n"
      "movq   %[y0], %%rbx\n"
      "movq   $0xf1, %%rcx\n"
      "movq   $0xf2, %%rdx\n"
      "movq   $0xf3, %%rsi\n"
      "movq   $0xf4, %%rdi\n"

      /* Loopity loop, this is where the SIGALRM is triggered. */
      "1:\n"
      "jmp    1b\n"

      "break_out:\n"
      : "=a" (rax), "=b" (rbx), "=c" (rcx), "=d" (rdx), "=S" (rsi),
        "=D" (rdi)
      : [y0] "m" (y0)
      : "cc", "memory");

   printf("Values in the signal handler:\n");
   printf("  rax=%#lx, rcx=%#lx, rdx=%#lx, rsi=%#lx, rdi=%#lx\n",
          uc.uc_mcontext.gregs[REG_RAX], uc.uc_mcontext.gregs[REG_RCX],
          uc.uc_mcontext.gregs[REG_RDX], uc.uc_mcontext.gregs[REG_RSI],
          uc.uc_mcontext.gregs[REG_RDI]);
   /* Check that rbx contains an uninitialised value (origin is py[0]). */
   if (uc.uc_mcontext.gregs[REG_RBX])
      assert(0);

   printf("Values after the return from the signal handler:\n");
   printf("  rax=%#lx, rdx=%#lx, rsi=%#lx, rdi=%#lx\n", rax, rdx, rsi, rdi);
   /* Check that rbx and rcx contain uninitialised values (origin is py[0]
      and px[0], respectively). */
   if (rbx || rcx)
      assert(0);

   return 0;
}

