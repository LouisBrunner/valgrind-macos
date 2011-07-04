#include <stdio.h>
#include <signal.h>
#include <unistd.h>
static int sigalrm_received = 0;

static void sigalrm_handler(int signr)
{
   sigalrm_received++;
}

int main (int argc, char *argv[])
{
   struct sigaction sa;
   fprintf(stderr, "starting ...\n");
   sa.sa_handler = sigalrm_handler;
   sigemptyset(&sa.sa_mask);
   sa.sa_flags = 0;

   if (sigaction (SIGALRM, &sa, NULL) != 0)
      perror("sigaction");
   if (kill(getpid(), SIGALRM) != 0)
      perror("kill 1");

   if (sigalrm_received == 1)
      fprintf (stderr, "ok: 1st SIGALRM received\n");
   else
      fprintf (stderr, "wrong 1st: unexpected value %d sigalrm_received\n",
               sigalrm_received);

   if (kill(getpid(), SIGALRM) != 0)
      perror("kill 2");

   if (sigalrm_received == 2)
      fprintf (stderr, "ok: 2nd SIGALRM received\n");
   else
      fprintf (stderr, "wrong 2nd: unexpected value %d sigalrm_received\n",
               sigalrm_received);

   return 0;
}
