/* This test demonstrated an obscure bug in malloclists handling caused by
   multiple blocks hashing to the same list and one being overwritten at
   realloc time due to bad ordering of the things happening.  Now runs
   without error. */

#include <stdlib.h>
#include <stdio.h>

int main ( void )
{
  char* p;
  int i;
  for (i = 0; i < 10000; i++) {
    p = malloc(10 + 10 * (i % 100));
    p = realloc(p, 500);
    p = realloc(p, 600);
    free(p);
  }
  return 0;
}

