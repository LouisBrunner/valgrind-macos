#include <stdlib.h>

int main(int argc, char *argv[])
{
  typedef struct
  {
    char *spointer1;
  } structure;

  structure *pointer1 = 0;

  pointer1 = malloc(4 * sizeof(structure)); /* Line 12 */
  pointer1[1].spointer1 = malloc(64);       /* Line 13 */

  free(pointer1);         /* Leak report Line 15 */
  free(pointer1);         /* Double/Invalid free report Line 16 */

  return 0;
}
