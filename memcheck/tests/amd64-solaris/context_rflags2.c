/* Test if definedness of rflags values is correctly propagated in and out
   of a signal handler.  Note that actually only the propagation of the
   overflow and sign flags is tested.

   This test must use alarm(2) to trigger the signal and not kill(2) as other
   tests do because in the latter case the signal is actually delivered after
   the syscall finished.  This means that Valgrind had to save a correct carry
   flag value (indicating if the syscall succeeded) in the rflags.  This save
   operation unfortunately makes all rflags initialised (due to imprecise
   simulation). */

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

void break_out(void);

static void sighandler(int sig, siginfo_t *sip, void *arg)
{
   ucontext_t *ucp = (ucontext_t *) arg;

   si = *sip;
   uc = *ucp;

   /* Break out of the endless loop. */
   *(uintptr_t*)&ucp->uc_mcontext.gregs[REG_RIP] = (uintptr_t)break_out;
}

int main(void)
{
   struct sigaction sa;
   long rflags;
   int x1;

   /* Uninitialised, but we know px[0] is 0x0. */
   int *px = malloc(sizeof(*px));
   x1 = px[0] + 1;

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
      /* Set overflow and sign flags. */
      "movl   %[x1], %%edx\n"
      "addl   $0x7fffffff, %%edx\n"

      /* Loopity loop, this is where the SIGALRM is triggered. */
      "1:\n"
      "jmp    1b\n"

      "break_out:\n"
      "pushfq\n"
      "popq   %%rdx\n"
      : "=d" (rflags)
      : [x1] "m" (x1)
      : "cc", "memory");

   /* Check that the overflow and sign flags are uninitialised.

      Note: This actually fails because the rflags are only approximate
      (always initialised) in the signal handler. */
   if (!OBIT(uc.uc_mcontext.gregs[REG_RFL]) ||
       !SBIT(uc.uc_mcontext.gregs[REG_RFL]))
      assert(0);

   /* Check that the overflow and sign flags are uninitialised. */
   if (!OBIT(rflags) || !SBIT(rflags))
      assert(0);

   return 0;
}

