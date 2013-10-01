#include <unistd.h>
#include "tests/sys_mman.h"
#include <assert.h>
#include <stdlib.h>

#include "../memcheck.h"

#define SUPERBLOCK_SIZE    100000

//-------------------------------------------------------------------------
// Allocator
//-------------------------------------------------------------------------

void* get_superblock(void)
{
   void* p = mmap( 0, SUPERBLOCK_SIZE, PROT_READ|PROT_WRITE|PROT_EXEC,
                   MAP_PRIVATE|MAP_ANONYMOUS, -1, 0 );

   assert(p != ((void*)(-1)));

   // Mark it no access;  although it's addressible we don't want the 
   // program to be using it unless its handed out by custom_alloc()

   // with redzones, better not to have it
   (void) VALGRIND_MAKE_MEM_NOACCESS(p, SUPERBLOCK_SIZE);

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

static void checkredzone(void)
{
   /* check that accessing the redzone of a MALLOCLIKE block
      is detected  when the superblock was not marked as no access. */
   char superblock[1 + RZ + 20 + RZ + 1];
   char *p = 1 + RZ + superblock;
   assert(RZ > 0);

   // Indicate we have allocated p from our superblock:
   VALGRIND_MALLOCLIKE_BLOCK( p, 20, RZ, /*is_zeroed*/1 );
   p[0] = 0; 
   p[-1] = p[0]; // error expected
   p[-RZ] = p[0]; // error expected
   p[-RZ-1] = p[0]; // no error expected
   
   p[19] = 0; 
   p[19 + 1]  = p[0]; // error expected
   p[19 + RZ] = p[0]; // error expected
   p[19 + RZ + 1] = p[0]; // no error expected

   VALGRIND_FREELIKE_BLOCK( p, RZ );

   // Now, indicate we have re-allocated p from our superblock
   // but with only a size 10.
   VALGRIND_MALLOCLIKE_BLOCK( p, 10, RZ, /*is_zeroed*/1 );
   p[0] = 0; 
   p[-1] = p[0]; // error expected
   p[-RZ] = p[0]; // error expected
   p[-RZ-1] = p[0]; // no error expected
   
   p[9] = 0; 
   p[9 + 1]  = p[0]; // error expected
   p[9 + RZ] = p[0]; // error expected
   p[9 + RZ + 1] = p[0]; // no error expected

   VALGRIND_FREELIKE_BLOCK( p, RZ );

}



//-------------------------------------------------------------------------
// Rest
//-------------------------------------------------------------------------

void make_leak(void)
{
   int* array2 __attribute__((unused)) = custom_alloc(sizeof(int) * 10);
   array2 = 0;          // leak
   return;
}

int main(void)
{
   int *array, *array3;
   int x;

   array = custom_alloc(sizeof(int) * 10);
   array[8]  = 8;
   array[9]  = 8;
   array[10] = 10;      // invalid write (ok w/o MALLOCLIKE -- in superblock)

   VALGRIND_RESIZEINPLACE_BLOCK(array, sizeof(int) * 10, sizeof(int) * 5, RZ);
   array[4] = 7;
   array[5] = 9; // invalid write

   // Make the entire array defined again such that it can be verified whether
   // the red zone is marked properly when resizing in place.
   (void) VALGRIND_MAKE_MEM_DEFINED(array, sizeof(int) * 10);

   VALGRIND_RESIZEINPLACE_BLOCK(array, sizeof(int) * 5, sizeof(int) * 7, RZ);
   if (array[5]) array[4]++; // uninitialized read of array[5]
   array[5]  = 11;
   array[6]  = 7;
   array[7] = 8; // invalid write

   // invalid realloc
   VALGRIND_RESIZEINPLACE_BLOCK(array+1, sizeof(int) * 7, sizeof(int) * 8, RZ);

   custom_free(array);  // ok

   custom_free((void*)0x1);  // invalid free

   array3 = malloc(sizeof(int) * 10);
   custom_free(array3); // mismatched free (ok without MALLOCLIKE)

   make_leak();
   x = array[0];        // use after free (ok without MALLOCLIKE/MAKE_MEM_NOACCESS)

   // Bug 137073: passing 0 to MALLOCLIKE_BLOCK was causing an assertion
   // failure.  Test for this (and likewise for FREELIKE_BLOCK).
   VALGRIND_MALLOCLIKE_BLOCK(0,0,0,0);
   VALGRIND_FREELIKE_BLOCK(0,0);

   checkredzone();

   return x;

   // leak from make_leak()
}

#undef RZ
