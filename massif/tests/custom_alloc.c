#include <unistd.h>
#include "tests/sys_mman.h"
#include <assert.h>
#include <stdlib.h>

#include "valgrind.h"

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

int main(void)
{
   int* a = custom_alloc(400);   // All sizes are divisible by 16 -- no slop.
   custom_free(a);

   a = custom_alloc(800);
   custom_free(a);

   a = malloc(400);
   free(a);

   a = malloc(800);
   free(a);

   return 0;
}
