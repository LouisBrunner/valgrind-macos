#include <limits.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <unistd.h>
#include <sys/stat.h>
#include <sys/types.h>

int
main (int argc, char **argv)
{
  char template[] = "/tmp/wd_test_XXXXXX";
  char *tmpdir = mkdtemp(template);
  if (tmpdir == NULL)
    {
      perror ("Couldn't mkdtemp");
      exit (-1);
    }

  if (chdir (tmpdir) != 0)
    {
      perror ("Couldn't chdir into tmpdir");
      exit (-1);
    }

  /* Go deep. */
  int dirslen = PATH_MAX;
  while (dirslen > 0)
    {
      /* We don't do any error checking in case some OS fails. */
      mkdir ("subdir", S_IRWXU);
      chdir ("subdir");
      dirslen -= strlen ("subdir");
    }

  /* Make one component inaccessible. */
  chmod(tmpdir, 0);

  /* Remove the current dir (don't check error, might fail). */
  rmdir ("../subdir");

  execlp ("echo", "echo", "Hello", "World", (char *) NULL);
  perror ("Couldn't execlp");
  return -1;
}
