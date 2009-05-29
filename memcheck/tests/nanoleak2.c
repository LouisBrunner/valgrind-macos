
// Bruce Lowekamp <lowekamp@sipeerior.com> reported that in a program with a
// reachable leak, if you do:
//
//   valgrind --leak-check=yes --gen-suppressions=yes --show-reachable=no -q
//
// it gives the y/n/c suppression prompt for errors that aren't shown.  This
// test checks that is fixed.
//
// [The .vgtest file was later changed to use --leak-check=all so that if a
// suppression is (erroneously?) generated, the test doesn't wait for the user
// to press 'y', because that pauses the entire regtest system.]

#include <stdlib.h>

int* a;

int main ( void )
{
  a = malloc(1000);  // Becomes a reachable leak.
  a[0] = 0;
  return a[0];
}
