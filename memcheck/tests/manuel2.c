#include <stdio.h>
#include <stdlib.h>

int main ()
{
  int y = 0;
  int *m = malloc(sizeof(int));
  int *x = m[0] + &y;      // we know m[0] will be zero

  printf ("x = %d\n", *x==0xDEADBEEF ? 99 : 88);

  return 0;
}
