#include <stdlib.h>
#include <assert.h>
#include "../../../config.h"

int main(void)
{
   // @todo PJF this is a placeholder for 10.15 and later support
#if !defined(VGO_darwin)
   char* p = NULL;

   // zero size
   p = aligned_alloc(0, 8);
   assert(p == NULL);
   // non multiple of alignment fails on Darwin
   p = aligned_alloc(8, 25);
   assert(p == NULL);
   // align not power of 2
   p = aligned_alloc(40, 160);
   assert(p == NULL);
#endif

   // @todo PJF this works standalone
   // but for some reason it doesn't fail in arena_memalign
   // and I see
   // ==25899== Warning: set address range perms: large range [0x1000, 0x1000000001000) (defined)


#if 0
   // too big
   if (sizeof(size_t) == 8)
   {
      p = aligned_alloc(16, 1UL<<48);
   }
   else
   {
      p = NULL;
   }

   assert(p == NULL);
#endif
}


