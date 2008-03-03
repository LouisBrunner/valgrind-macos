
/* Check for correct handling of nested scopes in a zero-biased
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

void foo ( void )
{
  int var;
  var = 1;
  { char var[10];
    var[6] = 4;
    croak( &var[7] );
    { struct { double foo; float bar; } var;
      croak ( 2 + (char*)&var.bar );
    }
  }
  croak( 1 + (char*)&var );
}

int main ( void )
{
  foo();
  return 0;
}
