#include <unistd.h>
#include "tests/sys_mman.h"
#include <assert.h>
#include <stdlib.h>

#include "../drd.h"

#define SUPERBLOCK_SIZE    100000

//-------------------------------------------------------------------------
// Allocator
//-------------------------------------------------------------------------

void* get_superblock(void)
{
   void* p = mmap( 0, SUPERBLOCK_SIZE, PROT_READ|PROT_WRITE|PROT_EXEC,
                   MAP_PRIVATE|MAP_ANONYMOUS, -1, 0 );

   assert(p != ((void*)(-1)));

   return p;
}

// has a redzone
static void* custom_alloc(int size)
{
#define RZ  8
   static void* hp     = 0;    // current heap pointer
   static void* hp_lim = 0;    // maximum usable byte in current block
   int          size2  = size + RZ*2;
   void*        p;

   if (hp + size2 > hp_lim) {
      hp = get_superblock();
      hp_lim = hp + SUPERBLOCK_SIZE - 1;
   }

   p = hp + RZ;
   hp += size2;

   VALGRIND_MALLOCLIKE_BLOCK( p, size, RZ, /*is_zeroed*/1 );
   return (void*)p;
}

static void custom_free(void* p)
{
   // don't actually free any memory... but mark it as freed
   VALGRIND_FREELIKE_BLOCK( p, RZ );
}
#undef RZ



//-------------------------------------------------------------------------
// Rest
//-------------------------------------------------------------------------

void make_leak(void)
{
   int* array2 = custom_alloc(sizeof(int) * 10);
   array2 = 0;          // leak
   return;
}

int main(void)
{
   int* array;
   int* array3;

   array = custom_alloc(sizeof(int) * 10);
   array[8]  = 8;
   array[9]  = 8;
   array[10] = 10;      // invalid write (ok w/o MALLOCLIKE -- in superblock)

   custom_free(array);  // ok

   custom_free(NULL);   // invalid free (ok without MALLOCLIKE)

   array3 = malloc(sizeof(int) * 10);
   custom_free(array3); // mismatched free (ok without MALLOCLIKE)

   make_leak();
   return array[0];     // use after free (ok without MALLOCLIKE)
                        // (nb: initialised because is_zeroed==1 above)
                        // unfortunately not identified as being in a free'd
                        // block because the freeing of the block and shadow
                        // chunk isn't postponed.

   // leak from make_leak()
}
