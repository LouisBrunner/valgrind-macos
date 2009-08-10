// Test for bug 100628:  need to allow custom MALLOCLIKE blocks to overlap
// with normal malloc() blocks in leak-checking -- if it happens, we ignore
// the malloc() block during the leak check.

#include <stdlib.h>
#include "valgrind.h"

int main(void)
{
   char* x;
   
   // For this one, the first custom block overlaps exactly with the start of
   // the malloc block.
   x = malloc(1000);
   VALGRIND_MALLOCLIKE_BLOCK(x,     /*szB*/ 16, /*rzB*/0, /*isZeroed*/0);
   VALGRIND_MALLOCLIKE_BLOCK(x+100, /*szB*/ 32, /*rzB*/0, /*isZeroed*/0);
   VALGRIND_MALLOCLIKE_BLOCK(x+200, /*szB*/ 64, /*rzB*/0, /*isZeroed*/0);
   VALGRIND_MALLOCLIKE_BLOCK(x+300, /*szB*/128, /*rzB*/0, /*isZeroed*/0);

   // For this one, the first custom block does not overlap exactly with the
   // start of the malloc block.
   x = malloc(1000);
   VALGRIND_MALLOCLIKE_BLOCK(x+100, /*szB*/ 32, /*rzB*/0, /*isZeroed*/0);
   VALGRIND_MALLOCLIKE_BLOCK(x+200, /*szB*/ 64, /*rzB*/0, /*isZeroed*/0);
   VALGRIND_MALLOCLIKE_BLOCK(x+300, /*szB*/128, /*rzB*/0, /*isZeroed*/0);

   return 0;
}
