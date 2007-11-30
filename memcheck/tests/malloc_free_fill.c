
/* Test for correct functioning of the --malloc-fill and --free-fill
   flags.  Needs --malloc-fill=0x55 and --free-fill=0x77. */

#include <stdio.h>
#include <assert.h>
#include <stdlib.h>

int main ( void )
{
  int *r, *oldr, *a;


  fprintf(stderr, "test simple malloc/free:\n");

  a = malloc(10 * sizeof(int)); assert(a);
  fprintf(stderr, "(should be malloc-filled)     a[4] = %x\n", a[4]);

  free(a);
  fprintf(stderr, "(should be free-filled)       a[5] = %x\n", a[5]);



  fprintf(stderr, "test realloc-larger:\n");

  r = malloc(30 * sizeof(int)); assert(r);
  fprintf(stderr, "(should be malloc-filled)    r[25] = %x\n", r[25]);

  /* Make larger */
  oldr = r;
  r = realloc(r, 40 * sizeof(int)); assert(r);

  fprintf(stderr, "(should be free-filled)   oldr[26] = %x\n", oldr[26]);
  fprintf(stderr, "(should be malloc-filled)    r[35] = %x\n", r[35]);

  free(r);



  fprintf(stderr, "test realloc-smaller:\n");

  r = malloc(30 * sizeof(int)); assert(r);
  fprintf(stderr, "(should be malloc-filled)    r[25] = %x\n", r[25]);

  /* Make smaller */
  oldr = r;
  r = realloc(r, 20 * sizeof(int)); assert(r);

  fprintf(stderr, "(should be free-filled)   oldr[26] = %x\n", oldr[26]);

  free(r);



  fprintf(stderr, "test calloc:\n");
  a = calloc(100, sizeof(int)); assert(r);

  fprintf(stderr, "(should be zero)             a[42] = %x\n", a[42]);

  free(a);


  return 0;
}
