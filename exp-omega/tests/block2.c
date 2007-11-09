#include <stdlib.h>

int main(int argc, char *argv[])
{
  typedef struct
  {
    char *spointer1;
  } structure;

  structure **pointer1 = 0;

  pointer1 = (structure **)malloc(4 * sizeof(structure *)); /* Line 12 */
  pointer1[1] = (structure *)malloc(sizeof(structure));     /* Line 13 */
  pointer1[1]->spointer1 = (char *)malloc(64);              /* Line 14 */

  free(pointer1);         /* Leak reports Line 16 */

  return 0;
} /* Leak reports actually on Line 19 due to timing of stack invalidation */
