/* Test of correct simulation for active stack. */

#include <assert.h>
#include <signal.h>
#include <stdio.h>
#include <ucontext.h>

static char altstack_map[8096];
static volatile stack_t *sp;

static void sighandler(int sig)
{
   /* Check that the alternate stack is active. */
   assert(sp->ss_sp == altstack_map);
   assert(sp->ss_size == sizeof(altstack_map));
   assert(sp->ss_flags == SS_ONSTACK);
}

int main(void)
{
   stack_t mainstack;
   stack_t altstack;
   struct sigaction sa;
   /* Obtain an address inside the stack using a dirty trick. */
   void *local = &sa;

   /* Get an address for stack definition. */
   if (getustack((stack_t**)&sp)) {
      perror("getustack");
      return 1;
   }

   /* Check the current stack. */
   assert(sp->ss_sp <= local);
   assert(local < (void*)((char*)sp->ss_sp + sp->ss_size));
   assert(sp->ss_flags == 0);

   /* Backup the current stack. */
   mainstack = *sp;

   /* Setup a signal handler. */
   sa.sa_handler = sighandler;
   sa.sa_flags = SA_ONSTACK;
   if (sigfillset(&sa.sa_mask)) {
      perror("sigfillset");
      return 1;
   }
   if (sigaction(SIGUSR1, &sa, NULL)) {
      perror("sigaction");
      return 1;
   }

   /* Setup an alternate stack. */
   altstack.ss_sp = altstack_map;
   altstack.ss_size = sizeof(altstack_map);
   altstack.ss_flags = 0;
   if (sigaltstack(&altstack, NULL)) {
      perror("sigaltstack");
      return 1;
   }

   /* Raise a signal. */
   raise(SIGUSR1);

   /* Check the current stack. */
   assert(mainstack.ss_sp == sp->ss_sp);
   assert(mainstack.ss_size == sp->ss_size);
   assert(mainstack.ss_flags == sp->ss_flags);

   return 0;
}

