
/* Test for correct functioning of the --malloc-fill and --free-fill
   flags.  Needs --malloc-fill=0x55 and --free-fill=0x77. */

#include <stdio.h>
#include <assert.h>
#include <stdlib.h>
#include "../memcheck.h"

int main ( void )
{
  int *r, *oldr, *a;

#define TEST(x, exp_x, desc) \
  (void)VALGRIND_MAKE_MEM_DEFINED(&x, sizeof(int));     \
  if (x == exp_x) { \
     fprintf(stderr, "PASSED: " desc "\n"); \
  } else { \
     fprintf(stderr, "FAILED: " desc "\n"); \
  }

  //-------------------------------------------------------------
  fprintf(stderr, "test simple malloc/free:\n");

  a = malloc(10 * sizeof(int)); assert(a);
  TEST(a[4], 0x55555555, "malloc-filled");

  free(a);
  TEST(a[5], 0x77777777, "  free-filled");

  //-------------------------------------------------------------
  fprintf(stderr, "\ntest realloc-larger:\n");

  r = malloc(30 * sizeof(int)); assert(r);
  TEST(r[25], 0x55555555, "malloc-filled");

  /* Make larger */
  oldr = r;
  r = realloc(r, 40 * sizeof(int)); assert(r);

  TEST(oldr[26], 0x77777777, "  free-filled");
  TEST(   r[35], 0x55555555, "malloc-filled");

  free(r);

  //-------------------------------------------------------------
  fprintf(stderr, "\ntest realloc-smaller:\n");

  r = malloc(30 * sizeof(int)); assert(r);
  TEST(r[25], 0x55555555,   "malloc-filled");

  /* Make smaller */
  oldr = r;
  r = realloc(r, 20 * sizeof(int)); assert(r);

  TEST(oldr[26], 0x77777777, "  free-filled");

  free(r);

  //-------------------------------------------------------------
  fprintf(stderr, "\ntest calloc:\n");
  a = calloc(100, sizeof(int)); assert(r);

  TEST(a[42], 0x00000000, "zero");

  free(a);

  return 0;
}
