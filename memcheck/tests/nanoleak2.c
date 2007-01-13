
// Bruce Lowekamp <lowekamp@sipeerior.com> reported that in a program with a
// reachable leak, if you do:
//
//   valgrind --leak-check=yes --gen-suppressions=yes --show-reachable=no -q
//
// it gives the y/n/c suppression prompt for errors that aren't shown.  This
// test checks that is fixed.

#include <stdlib.h>

int* a;

int main ( void )
{
  a = malloc(1000);  // Becomes a reachable leak.
  a[0] = 0;
  return a[0];
}
