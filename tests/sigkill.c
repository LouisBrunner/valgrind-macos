
#include <errno.h>
#include <stdio.h>
#include <signal.h>
#include <stdlib.h>

static void
abend (int sig)
{
  printf ("Abended on signal %d\n", sig);
  exit (2);
}

int
main (void)
{
  struct sigaction  sa;

  int i;
  for (i = 1; i <= 64; i++) {
     sa.sa_flags   = 0;
     sigemptyset( &sa.sa_mask );
     sa.sa_handler = abend;
     errno = 0;
     fprintf(stderr, "setting signal %d: ", i);
     sigaction (i /*SIGKILL*/, &sa, NULL);
     perror ("");
     errno = 0;
     fprintf(stderr, "getting signal %d: ", i);
     sigaction (i /*SIGKILL*/, NULL, &sa);
     perror ("");
     fprintf(stderr, "\n");
  }
  return 0;
}
