#include <stdlib.h>

int
main ()
{
  volatile int x;  /* make sure it isn't in a register */

  if (x == 0)
     return 0;
  else
     return 1;
}
