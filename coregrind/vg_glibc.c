#include <errno.h>
#include <sys/mman.h>
#include "vg_include.h"

extern void *__curbrk;		/* in glibc */

/* libc overrides, so that things can use normal allocators if they
   wish. 
   XXX override malloc with VG_(arena_malloc)?
*/
int brk(void *end)
{
   void *res = VG_(brk)(end);

   if (res != end) {
      errno = -ENOMEM;
      return -1;
   }
   return 0;
}
int __brk(void *) __attribute__((alias ("brk")));

void *sbrk(ptrdiff_t inc)
{
   void *oldbrk = __curbrk;

   if (inc == 0)
      return __curbrk;
   if (__brk(__curbrk + inc) < 0)
      return (void *)-1;

   return oldbrk;
}
int __sbrk(void *) __attribute__((alias ("sbrk")));

#if 0
void *mmap(void *addr, size_t len, int prot, int flags, int fd, __off_t offset)
{
   return VG_(mmap)(addr, len, prot, flags, fd, offset);
}
#endif
