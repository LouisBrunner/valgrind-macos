
/* test of plausible behaviour with malloc and stupid args */

#include <stdlib.h>
#include <stdio.h>

int main ( void )
{
  char* p;

  p = malloc(0);
  printf("malloc(0) = %p\n", p);
  free(p);

  p = malloc(-1);
  printf("malloc(-1) = %p\n", p);
  free(p);

  p = calloc(0,1);
  printf("calloc(0,1) = %p\n", p);
  free(p);

  p = calloc(0,-1);
  printf("calloc(0,-1) = %p\n", p);
  free(p);

  p = calloc(-1,-1);
  printf("calloc(-1,-1) = %p\n", p);
  free(p);

  return 0;
}
