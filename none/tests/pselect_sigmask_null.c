/* Make sure handling of NULL sigmask is correct.
   https://bugs.kde.org/show_bug.cgi?id=364413
   We might try to make a copy and adjust the mask.
   Testcase provided by Paul Eggert <eggert@cs.ucla.edu> */

#include <stdio.h>
#include <stdlib.h>
#include <sys/select.h>

int
main (void)
{
  struct timespec timeout;
  timeout.tv_sec = 1;
  timeout.tv_nsec = 0;
  switch (pselect (0, 0, 0, 0, &timeout, 0))
    {
    default:
      abort ();
    case -1:
      perror ("pselect");
      return 1;
    case 0:
      return 0;
    }
}
