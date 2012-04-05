
/* Check of variable location identification when using .debug_types.  */

/* Relevant compile flags are:

   -Wall -g -I$prefix/include/valgrind -gdwarf-4 -fdebug-types-section

   eg -Wall -g -I`pwd`/Inst/include/valgrind -gdwarf-4 -fdebug-types-section
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

struct s1
{
  char c;
  short s;
  int i;
  long l;
  float f;
  double d;
};

struct s1 S2[30];

int main ( void )
{
  struct s1 local;
  struct s1* onheap = malloc(sizeof (struct s1));
  assert(onheap);
  croak(&onheap->i);

  croak( &S2[0].i );
  croak( &local.i );
  return 0;
}
