#include <stdlib.h>

int main(int argc, char *argv[])
{
  char *pointer1 = 0;

  pointer1 = malloc(64); /* Line 7 */

  {
    char *pointer2 = (char *)&pointer1;
    pointer2[1] = 1;                   /* Leak Line 11. */
  }

  return 0;
}
