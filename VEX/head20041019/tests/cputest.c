#include <stdio.h>
#include <stdlib.h>
#include <string.h>

static __inline__ void cpuid(unsigned int n,
                             unsigned int *a, unsigned int *b,
                             unsigned int *c, unsigned int *d)
{
   __asm__ __volatile__ (
      "cpuid"
      : "=a" (*a), "=b" (*b), "=c" (*c), "=d" (*d)      /* output */
      : "0" (n)         /* input */
   );
}

int main(int argc, char **argv)
{
   unsigned int level = 0;
   unsigned int mask = 0;
   unsigned int a;
   unsigned int b;
   unsigned int c;
   unsigned int d;

   if ( argc == 2 ) {
      if ( strcmp( argv[1], "fpu" ) == 0 ) {
        level = 1;
        mask = 1 << 0;
      } else if ( strcmp( argv[1], "cmov" ) == 0 ) {
        level = 1;
        mask = 1 << 15;
      } else if ( strcmp( argv[1], "mmx" ) == 0 ) {
        level = 1;
        mask = 1 << 23;
      } else if ( strcmp( argv[1], "mmxext" ) == 0 ) {
        level = 0x80000001;
        mask = 1 << 22;
      } else if ( strcmp( argv[1], "sse" ) == 0 ) {
        level = 1;
        mask = 1 << 25;
      } else if ( strcmp( argv[1], "sse2" ) == 0 ) {
        level = 1;
        mask = 1 << 26;
      }
   }

   if ( level == 0 || mask == 0 ) {
      fprintf( stderr, "usage: cputest [cmov|mmx|mmxext|sse|sse2]\n" );
      exit( 1 );
   }
   
   cpuid( level & 0x80000000, &a, &b, &c, &d );

   if ( a >= level ) {
      cpuid( level, &a, &b, &c, &d );

      if ( ( d & mask ) != 0 ) exit( 0 );
   }

   exit( 1 );
}
