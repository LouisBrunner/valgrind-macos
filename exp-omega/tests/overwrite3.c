#include <stdlib.h>

int main(int argc, char *argv[])
{
  char *pointer1 = 0;
  char *pointer2 = 0;

  pointer1 = malloc(64);     /* Line 8. */
  pointer2 = pointer1;
  pointer1 = NULL;
  pointer2 = NULL;           /* Leak report Line 11. */

  return 0;
}
