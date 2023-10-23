#include <stdlib.h>
#include <assert.h>
#include <errno.h>
#include "../../../config.h"

int main(void)
{
#if defined (HAVE_GNU_LIBC_C17_ALIGNED_ALLOC)
   char* p = NULL;

   // zero size
   p = aligned_alloc(0, 8);
   assert(p == NULL);

   // non multiple of alignment
   p = aligned_alloc(8, 25);
   assert(p && ((size_t)p % 8U == 0U));
   free(p);

   // align not power of 2
   p = aligned_alloc(40, 160);
   assert(p == NULL);
   errno = 0;
#endif
#if defined(MUSL_LIBC)
   char* p = NULL;

   // zero size
   p = aligned_alloc(0, 8);
   assert(p && ((size_t)p % 8U == 0U));
   free(p);
   // non multiple of alignment passes on FreeBSD
   p = aligned_alloc(8, 25);
   assert(p && ((size_t)p % 8U == 0U));
   free(p);
   //errno = 0;
   // align not power of 2
   p = aligned_alloc(40, 160);
   assert(p == NULL);
   errno = 0;
   // the test below causes a segfault with musl 1.2.2
   // apparently it has been fixed in 1.2.3
#if 0
   // too big
   if (sizeof(size_t) == 8)
   {
      p = aligned_alloc(16, 1UL<<48);
   }
   else
   {
      p = NULL;
      errno = ENOMEM;
   }
   assert(p == NULL && errno == ENOMEM);
#endif
#endif
}


