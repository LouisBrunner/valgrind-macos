// Replacement for malloc.h which factors out platform differences.

#include <stdlib.h>
#include "config.h"
#if defined(VGO_darwin)
#  include <malloc/malloc.h>
#elif defined(VGO_freebsd)
#  include <stdlib.h>
#  include <malloc_np.h>
#else
#  include <malloc.h>
#endif

#include <assert.h>

// Allocates a 16-aligned block.  Asserts if the allocation fails.
__attribute__((unused))
static void* memalign16(size_t szB)
{
   void* x;
#if defined(VGO_darwin) || defined(VGO_freebsd)
   // Darwin lacks memalign, but its malloc is always 16-aligned anyway.
   posix_memalign((void **)&x, 16, szB);
#else
   x = memalign(16, szB);
#endif
   assert(x);
   assert(0 == ((16-1) & (unsigned long)x));
   return x;
}

// Allocates a 32-aligned block.  Asserts if the allocation fails.
__attribute__((unused))
static void* memalign32(size_t szB)
{
   void* x;
#if defined(VGO_darwin) || defined(VGO_freebsd)
   // Darwin lacks memalign
   posix_memalign((void **)&x, 32, szB);
#else
   x = memalign(32, szB);
#endif
   assert(x);
   assert(0 == ((32-1) & (unsigned long)x));
   return x;
}

// Allocates a 64-aligned block.  Asserts if the allocation fails.
__attribute__((unused))
static void* memalign64(size_t szB)
{
   void* x;
#if defined(VGO_darwin) || defined(VGO_freebsd)
   // Darwin lacks memalign
   posix_memalign((void **)&x, 64, szB);
#else
   x = memalign(64, szB);
#endif
   assert(x);
   assert(0 == ((64-1) & (unsigned long)x));
   return x;
}

