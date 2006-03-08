
#include <stdio.h>
#include <stdlib.h>
#include "../memcheck.h"

/* Program demonstrating copying of metadata in memcheck. */

int main ( void )
{
  int* a = malloc(10 * sizeof(int));
  int* b = malloc(10 * sizeof(int));
  int* v = malloc(10 * sizeof(int));
  int i, sum, res;

  for (i = 0; i < 10; i++) {
     if (i != 5) 
        a[i] = i;
  }

  /* a[0 .. 4] and [6 .. 9] are defined, [5] is undefined. */
  for (i = 0; i < 10; i++)
     b[i] = 0;

  /* b[0 .. 9] is defined. */

  /* Get metadata for a and put it in v. */
  res = VALGRIND_GET_VBITS(a, v, 10*sizeof(int) );
  printf("result of GET is %d (1 for success)\n", res);

  for (i = 0; i < 10; i++)
     printf("%d 0x%08x\n", i, v[i]);

  /* and copy to b. */
  res = VALGRIND_SET_VBITS(b, v, 10*sizeof(int) );
  printf("result of SET is %d (1 for success)\n", res);
  
  /* Now we should have that b[5] is undefined since a[5] is
     undefined. */
  sum = 100;
  for (i = 0; i < 10; i++)
     sum += b[i];

  /* V should yelp at this point, that sum is undefined. */
  if (sum == 0) 
    printf("sum == 0\n"); 
  else
    printf("sum != 0\n");

  return 0;
}
