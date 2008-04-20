/* Repeatedly allocate and free memory. Tests whether drd */
/* really frees memory allocated by a client. See also    */
/* http://bugs.kde.org/show_bug.cgi?id=161036             */

#include <stdlib.h>

int main()
{
  int i;
  for (i = 0; i < 100000; i++)
  {
    free(malloc(40960));
  }
  return 0;
}
