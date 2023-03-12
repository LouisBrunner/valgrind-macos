#include <stdlib.h>
#include <assert.h>
#include <errno.h>

int main(void)
{
   char* p = NULL;


   // zero size
   p = aligned_alloc(0, 8);
   assert(p == NULL && errno == EINVAL);
   errno = 0;
   // non multiple of alignment passes on FreeBSD
   p = aligned_alloc(8, 25);
   assert(p && ((size_t)p % 8U == 0U));
   free(p);
   errno = 0;
   // align not power of 2
   p = aligned_alloc(40, 160);
   assert(p == NULL && errno == EINVAL);
   errno = 0;

   // too big aligment
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

}

