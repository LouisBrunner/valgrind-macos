/* For a long time (from Valgrind 1.0 to 1.9.6, AFAICT) when realloc() was
   called and made a block smaller, or didn't change its size, the
   ExeContext of the block was not updated;  therefore any errors that
   referred to it would state that it was allocated not by the realloc(),
   but by the previous malloc() or whatever.  While this is true in one
   sense, it is misleading and not what you'd expect.  This test
   demonstrates this -- 'x' and 'y' are unchanged and shrunk, and their
   ExeContexts should be updated upon their realloc().  I hope that's clear.
*/
#include <stdlib.h>

int main(void)
{
   int* x = malloc(5);
   int* y = malloc(10);
   int* z = malloc(2);
   int a, b, c;

   x = realloc(x, 5);   // same size
   y = realloc(y, 5);   // make smaller
   z = realloc(z, 5);   // make bigger

   a = (x[5] == 0xdeadbeef ? 1 : 0);
   b = (y[5] == 0xdeadbeef ? 1 : 0);
   c = (z[5] == 0xdeadbeef ? 1 : 0);

   return a + b + c;
}
