
/* A small demo of providing descriptions of structured types in error
   messages. */

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
  volatile char* undefp = malloc(1);
  char saved = *a;
  assert(undefp);
  *a = *undefp;
  (void) VALGRIND_CHECK_MEM_IS_DEFINED(a, 1);
  *a = saved;
  free((void*)undefp);
}

#include <stdio.h>
#include <string.h>

typedef struct { short c1; char* c2[3]; } XX;

typedef
   struct _str { int bing; int bong; XX xyzzy[77]; }
   Str;

__attribute__((noinline))
int blah ( int x, int y )
{
  Str a[10];
  memset(a, 0, sizeof(a));
  croak(1 + (char*)(&a[3].xyzzy[x*y].c1));
  croak( (char*)(&a[5].bong) );
  croak( 1 + (char*)(&a[3].xyzzy[x*y].c2[2]) );
  memset(a, 0, sizeof(a));
  return a[3].xyzzy[x*y].c1;
}

int main ( void )
{
  printf("answer is %d\n", blah(3,7) );
  return 0;
}
