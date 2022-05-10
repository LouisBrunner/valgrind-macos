#include <stdlib.h>
#if defined(__FreeBSD__)
#include <malloc_np.h>
#endif
#include <assert.h>
#include <errno.h>

int main(void)
{
   char* p = NULL;
   int res;
   
   // zero alignment
   res = posix_memalign((void**)&p, 0, 8);
   assert(p == NULL && res == EINVAL);
   // non multiple of alignment passes on FreeBSD
   //res = posix_memalign((void**)&p, 8, 25);
   //assert(p == NULL && res == EINVAL);
   // align not multiple of sizeof(void*)
   res = posix_memalign((void**)&p, 2, 32);
   assert(p == NULL && res == EINVAL);
   // align not power of two
   res = posix_memalign((void**)&p, 40, 160);
   assert(p == NULL && res == EINVAL);
   // too big
   if (sizeof(size_t) == 8)
   {
      res = posix_memalign((void**)&p, 16, 1UL<<48);
   }
   else
   {
      // on x86 it's hard to actually get ENOMEM
      // if we ask for more than 2Gbytes the fishy
      // detector will kick in and not try to allocate
      // less than 2Gbytes and it's likely to succeed
      // (at least on a machine just tunning VG regtests)
      // so fake it
      p = NULL;
      res = ENOMEM;
   }
   assert(p == NULL && res == ENOMEM);
   errno = 0;
   
   
   // if ever we make this multi-platform, Solaris doesn't support this
   // zero size
   p = aligned_alloc(0, 8);
   assert(p == NULL && errno == EINVAL);
   errno = 0;
   // non multiple of alignment passes on FreeBSD
   //p = aligned_alloc(8, 25);
   //assert(p == NULL && errno == EINVAL);
   //errno = 0;
   // align not power of 2
   p = aligned_alloc(40, 160);
   assert(p == NULL && errno == EINVAL);
   errno = 0;
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
}


