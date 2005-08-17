#include <assert.h>
#include <malloc.h>
#include <stdlib.h>
#include <stdio.h>

int main(void)
{
   // Since our allocations are in multiples of 8, 99 will round up to 104.
   int* x = malloc(99);
   assert(104 == malloc_usable_size(x));
   return 0;
}
