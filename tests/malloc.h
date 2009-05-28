// Replacement for malloc.h which factors out platform differences.

#include <stdlib.h>
#if defined(VGO_darwin)
#  include <malloc/malloc.h>
#else
#  include <malloc.h>
#endif

#include <assert.h>

// Allocates a 16-aligned block.  Asserts if the allocation fails.
__attribute__((unused))
static void* memalign16(size_t szB)
{
   void* x;
#if defined(VGO_darwin)
   // Darwin lacks memalign, but its malloc is always 16-aligned anyway.
   x = malloc(szB);
#else
   x = memalign(16, szB);
#endif
   assert(x);
   assert(0 == ((16-1) & (unsigned long)x));
   return x;
} 

