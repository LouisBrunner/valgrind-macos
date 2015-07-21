/* Test to check that the mask parameter of the sigresend syscall is handled
   correctly. */

#include <assert.h>
#include <signal.h>
#include <stdio.h>
#include <string.h>
#include <sys/syscall.h>
#include <unistd.h>

static void signal_handler(int signo, siginfo_t *info, void *uc)
{
   ssize_t written;
   const char str[] = "Signal caught.\n";
   size_t len = sizeof(str) - 1;
   sigset_t current;

   written = write(STDOUT_FILENO, str, len);
   assert(written == len);

   /* Check that SIGUSR1 is already blocked in the signal handler. */
   assert(!sigprocmask(SIG_BLOCK, NULL, &current));
   assert(sigismember(&current, SIGUSR1));
}

int main(void)
{
   sigset_t block, current;
   struct sigaction sa;

   /* Check that SIGUSR1 is unblocked. */
   if (sigprocmask(0, NULL, &current)) {
      perror("sigprocmask");
      return 1;
   }
   assert(!sigismember(&current, SIGUSR1));

   /* Establish a SIGINT handler. */
   sa.sa_sigaction = signal_handler;
   sa.sa_flags = SA_RESTART | SA_SIGINFO;
   if (sigfillset(&sa.sa_mask)) {
      perror("sigfillset");
      return 1;
   }
   if (sigaction(SIGINT, &sa, NULL)) {
      perror("sigaction");
      return 1;
   }

   /* Send us a signal to handle and install a new sigmask. */
   if (sigemptyset(&block)) {
      perror("sigemptyset");
      return 1;
   }
   if (sigaddset(&block, SIGUSR1)) {
      perror("sigaddset");
      return 1;
   }
   if (syscall(SYS_sigresend, SIGINT, NULL, &block)) {
      fprintf(stderr, "Sigresend failed.\n");
      return 1;
   }

   /* Check that SIGUSR1 is now blocked. */
   if (sigprocmask(SIG_BLOCK, NULL, &current)) {
      perror("sigprocmask");
      return 1;
   }
   assert(sigismember(&current, SIGUSR1));

   return 0;
}

