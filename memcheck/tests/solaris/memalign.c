#include <stdlib.h>
#include <stdio.h>
#include <assert.h>
#include "tests/malloc.h"
#include <errno.h>
#include "../../../config.h"


int main (void)
{
   int* p;
   int* piece;
   assert(sizeof(long int) == sizeof(void*));

   p = memalign(1024 * 1024, 4 * 1024 * 1024 + 1);
   assert(0 == (long)p % (1024 * 1024));
   piece = malloc(1024 * 1000); assert (piece);
   free (piece);
   free (p);
   
   p = memalign(1024 * 1024, 4 * 1024 * 1024 + 1);
   assert(0 == (long)p % (1024 * 1024));
   piece = malloc(1024 * 100); assert (piece);
   free (p);
   free (piece);
   
   // Illumos doesn't allow a size of 0
   errno = 0;
   p = memalign(64, 0);
   assert(!p);
   assert(errno == EINVAL);

   // Doesn't allow an alignment of 0 either
   p = memalign(0, 100);
   assert(!p);
   assert(errno == EINVAL);

   // and most peculiar compared to other memaligns
   // the alignment must be a multiple of 4
   // but not necessarily a power of 2

   // I think that the
   p = memalign(1, 100);
   assert(!p);
   assert(errno == EINVAL);
   p = memalign(2, 100);
   assert(!p);
   assert(errno == EINVAL);
   p = memalign(3, 100);
   assert(!p);
   assert(errno == EINVAL);
   p = memalign(4, 100);
   assert(p && 0 == (long)p % 8);
   p = memalign(5, 100);
   assert(!p);
   assert(errno == EINVAL);
   errno = 0;
   p = memalign(7, 100);
   assert(!p);
   assert(errno == EINVAL);
   p = memalign(8, 100);
   assert(p && 0 == (long)p % 8);
   errno= 0;
   p = memalign(9, 100);
   assert(!p);
   assert(errno == EINVAL);
   p = memalign(12, 100);
   assert(p && 0 == (long)p % 16);
   errno = 0;
   p = memalign(31, 100);
   assert(!p);
   assert(errno == EINVAL);
   p = memalign(32, 100);
   assert(p && 0 == (long)p % 32);
   errno = 0;
   p = memalign(33, 100);
   assert(!p);
   assert(errno == EINVAL);
   errno = 0;
   p = memalign(4095, 100);
   assert(!p);
   assert(errno == EINVAL);
   p = memalign(4096, 100);
   assert(p && 0 == (long)p % 4096);
   errno = 0;
   p = memalign(4097, 100);
   assert(!p);
   assert(errno == EINVAL);

   p = memalign(4 * 1024 * 1024, 100);
   assert(p && 0 == (long)p % (4 * 1024 * 1024));
   p = memalign(16 * 1024 * 1024, 100);
   assert(p && 0 == (long)p % (16 * 1024 * 1024));
}
