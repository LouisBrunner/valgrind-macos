
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
  int rc;
  for (i = 1; i <= 65; i++) {
     sa.sa_flags   = 0;
     sigemptyset( &sa.sa_mask );
     sa.sa_handler = abend;
     fprintf(stderr,"setting signal %d: ", i);
     rc = sigaction (i /*SIGKILL*/, &sa, NULL);
     if (rc) perror ("");
     else fprintf(stderr,"Success\n");
     fprintf(stderr,"getting signal %d: ", i);
     rc = sigaction (i /*SIGKILL*/, NULL, &sa);
     if (rc) perror ("");
     else fprintf(stderr,"Success\n");
     fprintf(stderr,"\n");
  }
  return 0;
}
