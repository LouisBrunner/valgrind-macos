
/* test of plausible behaviour with malloc and stupid args */

#include <stdlib.h>
#include <stdio.h>

int main ( void )
{
  char* p;

  p = malloc(0);
  printf("malloc(0) = 0x%lx\n", (unsigned long)p);
  free(p);

  p = malloc(-1);
  printf("malloc(-1) = 0x%lx\n", (unsigned long)p);
  free(p);

  p = calloc(0,1);
  printf("calloc(0,1) = 0x%lx\n", (unsigned long)p);
  free(p);

  p = calloc(0,-1);
  printf("calloc(0,-1) = 0x%lx\n", (unsigned long)p);
  free(p);

  // We no longer get a warning with this due to the calloc overflow checking
  // done for bug 149878.  It's no great loss, it's extremely unlikely to
  // occur in practice.
  p = calloc(-1,-1);
  printf("calloc(-1,-1) = 0x%lx\n", (unsigned long)p);
  free(p);

  return 0;
}
