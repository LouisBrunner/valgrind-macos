// Replacement for sys/mman.h which factors out platform differences.

#include <sys/mman.h>

#if defined(VGO_darwin)
#  define MAP_ANONYMOUS MAP_ANON
#endif


#include <assert.h>
#include <unistd.h>

// Map a page, then unmap it, then return that address.  That
// guarantees to give an address which will fault when accessed,
// without making any assumptions about the layout of the address
// space.

__attribute__((unused))
static void* get_unmapped_page(void)
{
   void* ptr;
   int r;
   long pagesz = sysconf(_SC_PAGE_SIZE);
   assert(pagesz == 4096 || pagesz == 8192 || pagesz == 16384 || pagesz == 32768
          || pagesz == 65536);
   ptr = mmap(0, pagesz, PROT_READ, MAP_ANONYMOUS|MAP_PRIVATE, -1, 0);
   assert(ptr != (void*)-1);
   r = munmap(ptr, pagesz);
   assert(r == 0);
   return ptr;
}

