#include <stdlib.h>

int main(int argc, char *argv[])
{
  char *pointer1 = 0;

  pointer1 = malloc(64);  /* Line 7 */
  pointer1 = malloc(32);  /* Leak report Line 8 */
  pointer1 = NULL;        /* Leak report Line 9 */

  return 0;
}
