#include <assert.h>
#include "tests/malloc.h"
#include <stdlib.h>
#include <stdio.h>

int main(void)
{
#  if !defined(VGO_aix5) && !defined(VGO_darwin)
   // Because Memcheck marks any slop as inaccessible, it doesn't round up
   // sizes for malloc_usable_size().
   int* x = malloc(99);

   // XXX: would be better to have a HAVE_MALLOC_USABLE_SIZE variable here
   assert(99 == malloc_usable_size(x));
   assert( 0 == malloc_usable_size(NULL));
   assert( 0 == malloc_usable_size((void*)0xdeadbeef));
#  endif

   return 0;
}
