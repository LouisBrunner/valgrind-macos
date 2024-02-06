#include <stdlib.h>
#include <stdio.h>
#include <assert.h>
#include "tests/malloc.h"
#include <errno.h>
#include "../../../config.h"

int main ( void )
{
   // Nb: assuming VG_MIN_MALLOC_SZB is 8 or more...
   int* p;
   int* piece;
   assert(sizeof(long int) == sizeof(void*));

#if !defined(MUSL_LIBC)
   // Check behaviour of memalign/free for big alignment.
   // In particular, the below aims at checking that a
   // superblock with a big size is not marked as reclaimable
   // if the superblock is used to provide a big aligned block
   // (see bug 250101, comment #14).
   // Valgrind m_mallocfree.c will allocate a big superblock for the memalign
   // call and will split it in two. This split superblock was
   // wrongly marked as reclaimable, which was then causing
   // assert failures (as reclaimable blocks cannot be split).
   p = memalign(1024 * 1024, 4 * 1024 * 1024 + 1);
   assert(p && (0 == (long)p % (1024 * 1024)));
   // We allocate (and then free) a piece of memory smaller than
   // the hole created in the big superblock.
   // If the superblock is marked as reclaimable, the below free(s) will cause
   // an assert. Note that the test has to be run with a --free-list-vol
   // parameter smaller than the released blocks size to ensure the free is directly
   // executed (otherwise memcheck does not really release the memory and so
   // the bug is not properly tested).
   piece = malloc(1024 * 1000);
   assert (piece);
   free (piece);
   free (p);

   // Same as above but do the free in the reverse order.
   p = memalign(1024 * 1024, 4 * 1024 * 1024 + 1);
   assert(p && (0 == (long)p % (1024 * 1024)));
   piece = malloc(1024 * 100);
   assert (piece);
   free (p);
   free (piece);

   p = memalign(0, 100);
   assert(p && (0 == (long)p % 8));
   p = memalign(1, 100);
   assert(p && (0 == (long)p % 8));
   p = memalign(2, 100);
   assert(p && (0 == (long)p % 8));
   p = memalign(3, 100);
   assert(p && (0 == (long)p % 8));
   p = memalign(4, 100);
   assert(p && (0 == (long)p % 8));
   p = memalign(5, 100);
   assert(p && (0 == (long)p % 8));

   p = memalign(7, 100);
   assert(p && (0 == (long)p % 8));
   p = memalign(8, 100);
   assert(p && (0 == (long)p % 8));
   p = memalign(9, 100);
   assert(p && (0 == (long)p % 16));

   p = memalign(31, 100);
   assert(p && (0 == (long)p % 32));
   p = memalign(32, 100);
   assert(p && (0 == (long)p % 32));
   p = memalign(33, 100);
   assert(p && (0 == (long)p % 64));

   p = memalign(4095, 100);
   assert(p && (0 == (long)p % 4096));
   p = memalign(4096, 100);
   assert(p && (0 == (long)p % 4096));
   p = memalign(4097, 100);
   assert(p && (0 == (long)p % 8192));

   p = memalign(4 * 1024 * 1024, 100);
   assert(p && (0 == (long)p % (4 * 1024 * 1024)));
   p = memalign(16 * 1024 * 1024, 100);
   assert(p && (0 == (long)p % (16 * 1024 * 1024)));

   // size 0
   p = memalign(256, 0);
   assert(p && (0 == (long)p % 256));
#else
   p = memalign(1024 * 1024, 4 * 1024 * 1024 + 1);
   assert(p && (0 == (long)p % (1024 * 1024)));
   piece = malloc(1024 * 1000); assert (piece);
   free (piece);
   free (p);
   p = memalign(1024 * 1024, 4 * 1024 * 1024 + 1);
   assert(p && (0 == (long)p % (1024 * 1024)));
   piece = malloc(1024 * 100);
   assert (piece);
   free (p);
   free (piece);

   errno = 0;
   p = memalign(0, 100);
   assert(p && (0 == (long)p % 8));
   p = memalign(1, 100);
   assert(p && (0 == (long)p % 8));
   p = memalign(2, 100);
   assert(p && (0 == (long)p % 8));
   p = memalign(3, 100);
   assert(!p);
   //assert(errno == EINVAL);
   errno = 0;
   p = memalign(4, 100);
   assert(p && 0 == (long)p % 8);
   p = memalign(5, 100);
   assert(!p);
   //assert(errno == EINVAL);
   errno = 0;
   p = memalign(7, 100);
   assert(!p);
   //assert(errno == EINVAL);
   errno = 0;
   p = memalign(8, 100);
   assert(p && (0 == (long)p % 8));
   p = memalign(9, 100);
   assert(!p);
   //assert(errno == EINVAL);
   errno = 0;
   p = memalign(31, 100);
   assert(!p);
   //assert(errno == EINVAL);
   p = memalign(32, 100);
   assert(p && (0 == (long)p % 32));
   errno = 0;
   p = memalign(33, 100);
   assert(!p);
   //assert(errno == EINVAL);
   errno = 0;
   p = memalign(4095, 100);
   assert(!p);
   //assert(errno == EINVAL);
   p = memalign(4096, 100);
   assert(p && (0 == (long)p % 4096));
   errno = 0;
   p = memalign(4097, 100);
   assert(!p);
   //assert(errno == EINVAL);

   p = memalign(4 * 1024 * 1024, 100);
   assert(p && (0 == (long)p % (4 * 1024 * 1024)));
   p = memalign(16 * 1024 * 1024, 100);
   assert(p && (0 == (long)p % (16 * 1024 * 1024)));

   // size 0
   p = memalign(256, 0);
   assert(p && (0 == (long)p % 256));
#endif
}
