#include <stdlib.h>

int main(int argc, char *argv[])
{
  char *pointer = 0;

  pointer = malloc(64); /* Line 7 */
  pointer = 0;         /* Leak report Line 8 */

  return 0;
}
