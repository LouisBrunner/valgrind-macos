/* Test that the stack correctly dies after a setcontext(2) call. */

#include <assert.h>
#include <signal.h>
#include <stdio.h>
#include <ucontext.h>

static volatile int *sp;

static void sighandler(int sig, siginfo_t *sip, void *arg)
{
   ucontext_t *ucp = (ucontext_t *) arg;
   sp = (int *) &ucp->uc_mcontext.gregs[0];
}

int main(void)
{
   struct sigaction sa;
   /* Always-null value that is used to prevent any possible compiler
      optimizations. */
   volatile int zero = 0;

   /* Setup a signal handler. */
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

   /* Raise a signal. */
   raise(SIGUSR1);

   /* Value pointed by sp should be at this point uninitialized. */
   if (*sp && zero)
      assert(0);

   return 0;
}

