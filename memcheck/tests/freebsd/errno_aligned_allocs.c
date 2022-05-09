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
   res = posix_memalign((void**)&p, 16, 1UL<<48);
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
   p = aligned_alloc(16, 1UL<<48);
   assert(p == NULL && errno == ENOMEM);
}


