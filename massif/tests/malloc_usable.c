#include <assert.h>
#include "tests/malloc.h"
#include <stdlib.h>
#include <stdio.h>

int main(void)
{
#  if !defined(VGO_darwin)
   // Because our allocations are in multiples of 8 or 16, 99 will round up
   // to 104 or 112.
   int* x = malloc(99);

   // XXX: would be better to have a HAVE_MALLOC_USABLE_SIZE variable here
   assert(104 == malloc_usable_size(x) ||
          112 == malloc_usable_size(x));
   assert(  0 == malloc_usable_size(NULL));
   assert(  0 == malloc_usable_size((void*)0xdeadbeef));
#  endif

   return 0;
}
