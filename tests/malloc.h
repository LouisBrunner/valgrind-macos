// Replacement for malloc.h which factors out platform differences.

#include <stdlib.h>
#include <malloc.h>

#include <assert.h>

// Allocates a 16-aligned block.  Asserts if the allocation fails.
__attribute__((unused))
static void* memalign16(size_t szB)
{
   void* x;
   x = memalign(16, szB);
   assert(x);
   assert(0 == ((16-1) & (unsigned long)x));
   return x;
} 

