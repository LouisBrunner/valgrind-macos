/* Test of correct simulation for uc->uc_link in a signal handler. */

#include <assert.h>
#include <signal.h>
#include <stdio.h>
#include <ucontext.h>

static void sighandler(int sig, siginfo_t *sip, ucontext_t *ucp)
{
   ucontext_t uc2;

   /* Current uc_link value has to be equal to ucp. */
   getcontext(&uc2);
   assert(uc2.uc_link == ucp);
}

int main(void)
{
   ucontext_t uc;
   struct sigaction sa;

   /* Current uc_link value has to be NULL. */
   if (getcontext(&uc)) {
      perror("getcontext");
      return 1;
   }
   assert(!uc.uc_link);

   sa.sa_handler = sighandler;
   sa.sa_flags = SA_SIGINFO;
   if (sigfillset(&sa.sa_mask)) {
      perror("sigfillset");
      return 1;
   }
   if (sigaction(SIGUSR1, &sa, NULL)) {
      perror("sigaction");
      return 1;
   }

   raise(SIGUSR1);

   /* Current uc_link value has to be NULL. */
   getcontext(&uc);
   assert(!uc.uc_link);

   return 0;
}

