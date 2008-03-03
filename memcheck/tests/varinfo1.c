
/* Basic check of variable location identification, in a zero-biased
   executable. */

/* Relevant compile flags are:

   -Wall -g -I$prefix/include/valgrind

   eg -Wall -g -I`pwd`/Inst/include/valgrind
*/

#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include "memcheck/memcheck.h"

/* Cause memcheck to complain about the address "a" and so to print
   its best guess as to what "a" actually is.  a must be
   addressible. */

void croak ( void* aV )
{
  char* a = (char*)aV;
  char* undefp = malloc(1);
  char saved = *a;
  assert(undefp);
  *a = *undefp;
  VALGRIND_CHECK_MEM_IS_DEFINED(a, 1);
  *a = saved;
  free(undefp);
}

#include <stdio.h>

int global_u1;

int global_i1 = 17;

char global_u2[10];

char global_i2[10] = { 1,2,3,4,5,6,7,8,9,10 };


int main ( void )
{
  int local;
  char* onheap = malloc(3);
  assert(onheap);
  croak(onheap+1);
  free(onheap);

  croak( &global_u1 );
  croak( &global_i1 );
  croak( &global_u2[3] );
  croak( &global_i2[7] );
  croak( &local );
  return 0;
}
