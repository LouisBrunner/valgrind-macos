#include <stdlib.h>

int main(int argc, char *argv[])
{
  char *pointer = 0;

  pointer = malloc(64); /* Line 7 */
  pointer++; /* Creates shadow to track the block. */

  return 0;
}                      /* Leak report Line 11. */
