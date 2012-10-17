#include <signal.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

int
main (int argc, char **argv)
{
  fprintf(stderr, "starting ...\n");

  // Three ways of going away...
  if (argc > 1)
    {
      // Explicit exit() with exit code.
      if (strcmp (argv[1], "exit") == 0)
	{
	  fprintf(stderr, "exiting ...\n");
	  exit (1);
	}

      // Get killed by a signal.
      if (strcmp (argv[1], "abort") == 0)
	{
	  fprintf(stderr, "aborting ...\n");
	  kill(getpid(), SIGABRT);
	}
    }

  // And finally, just return from main with success.
  fprintf(stderr, "returning ...\n");
  return 0;
}
