#include <stdio.h>
#include <stdlib.h>
#include "tests/sys_mman.h"

int main(int argc, char **argv)
{
   void *p1;
   void *p2;

   if ( ( p1 = mmap( 0, 4096, PROT_READ, MAP_PRIVATE | MAP_ANONYMOUS, -1, 0 ) ) == MAP_FAILED )
   {
      perror( "aligned mmap failed" );
      exit( 1 );
   }

   if ( munmap( p1, 4096 ) != 0 )
   {
      perror( "aligned munmap failed" );
      exit( 1 );
   }

   // This fails because MAP_FIXED is specified and p1+1 isn't page-aligned.
   // (On Linux, just non-page-alignment is enough for failure, but on Darwin
   // MAP_FIXED has to also be specified.)
   if ( ( p2 = mmap( p1 + 1, 4096, PROT_READ, MAP_PRIVATE | MAP_ANONYMOUS | MAP_FIXED, -1, 0 ) ) == MAP_FAILED )
   {
      perror( "unaligned mmap failed" );
      exit( 1 );
   }

   if ( munmap( p2, 4096 ) != 0 )
   {
      perror( "unaligned munmap failed" );
      exit( 1 );
   }

   exit( 0 );
}
