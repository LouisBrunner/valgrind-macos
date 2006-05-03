
/* A test which involves copying (using realloc) a block containing
   some partially defined bytes.  Really this is to check that
   copy_address_range_perms in mc_main.c works.  I don't think it's a
   good test - it may well not exercise all the code in
   copy_address_range_perms. */

#include <assert.h>
#include <stdlib.h>
#include <stdio.h>
#include "memcheck/memcheck.h"

typedef unsigned char UChar;
typedef unsigned int  UInt;


static UInt seed = 0;
static inline UInt myrand ( UInt size )
{
   /* From "Numerical Recipes in C" 2nd Edition */
   seed = 1664525UL * seed + 1013904223UL;
   return seed % size;
}

static void barf ( int size, int offset )
{
   printf("pdb-realloc2: fail: size %d, offset %d\n", size,offset);
   exit(1);
}

void do_test ( int size )
{
   int i,j,r;
   UChar* v;
   UChar* p = malloc(size);
   assert(p);
   // fill
   seed = 0;
   for (i = 0; i < size; i++) {

      j = myrand( 256 * 25 );
      //printf("%d\n", j);
      if (j >= 256 * 13) {
         // def 1s
         p[i] = 0xFF;
      } else 
      if (j >= 256 && j < 256*13) {
         // def 0s
         p[i] = 0;
      } else {
         // pdb
         p[i] &= (UChar)j;
      }

   }

   // copy
   for (i = 1; i <= 100; i++) {
      p = realloc(p, size+i);
      assert(p);
   }

   // check
   v = malloc(size+100);
   assert(v);
   r = VALGRIND_GET_VBITS(p,v, size+100);
   assert(r == 1);

   //for (i = 0; i < size+100; i++)
   //  printf("%02x ", (UInt)v[i]);
   //printf("\n");

   seed = 0;
   for (i = 0; i < size; i++) {

      j = myrand( 256 * 25 );

      if (j >= 256) {
         // expecting a defined value
         if (v[i] != 0)
            barf(size, i);
      } else {
         // expecting a PDB == j
         if (v[i] != (UChar)j)
            barf(size,i);
      }

   }

   // in the extension area, everything should be undefined
   for (i = 0; i < 100; i++) {
      if (v[size+i] != 0xFF)
         barf(size, i);
   }

   free(v);
   free(p);
}

int main ( void )
{
  int z;
  for (z = 0; z < 100; z++) {
     printf("pdb_realloc: z = %d\n", z);
     do_test(z);
     do_test(z + 173);
     do_test(z + 1731);
  }
  printf("pdb-realloc2: done\n");
  return 0;
}
