#include <stdlib.h>
#include <unistd.h>
#if !defined(VGO_darwin)
#include <malloc.h>
#endif
#include "../../config.h"
#include "../memcheck.h"

int main(void)
{
   size_t size = 1024U;
   size_t align = 64U;
   char *mem;
   char *p;
   int res;
   (void)VALGRIND_MAKE_MEM_UNDEFINED(&size, sizeof(size));
   (void)VALGRIND_MAKE_MEM_UNDEFINED(&align, sizeof(align));
#if !defined(VGO_darwin)
   p = memalign(align, size);
   free(p);
#endif

   res = posix_memalign((void **)&mem,align,size);
   free(mem);
   
   p = aligned_alloc(align, size);
   free(p);
   
   p = valloc(size);
   free(p);
}
