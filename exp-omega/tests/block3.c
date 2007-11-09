#include <stdlib.h>

typedef struct
{
  char *spointer1;
} structure;

structure **func1(void)
{

  structure **pointer1 = 0;

  pointer1 = (structure **)malloc(4 * sizeof(structure *)); /* Line 13 */
  pointer1[1] = (structure *)malloc(sizeof(structure));     /* Line 14 */
  pointer1[1]->spointer1 = (char *)malloc(64);              /* Line 15 */

  return pointer1;
}

int main(int argc, char *argv[])
{
  structure **pointer1 = func1();

  free(pointer1);         /* Leak reports Line 24 */

  return 0;
}
