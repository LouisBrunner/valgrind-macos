
#include <unistd.h>
#include <sys/types.h>
#include <stdio.h>

int main(void)
{
  pid_t pid;

  pid = fork ();

  /* Sometimes child goes first (non-zero), sometimes parent (zero).  This
     printing means we can detect if we correctly get a zero result and a
     non-zero result (--> three 'X's printed), but the output doesn't depend
     on the order. */

  printf("%s", pid==0 ? "X" : "XX");

  return 0;
}
