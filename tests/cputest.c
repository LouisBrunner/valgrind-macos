#include <stdio.h>
#include <stdlib.h>
#include <string.h>

static __inline__ void cpuid(int n,
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
   unsigned int mask = 0;
   unsigned int a;
   unsigned int b;
   unsigned int c;
   unsigned int d;

   if ( argc == 2 )
   {
      if ( strcmp( argv[1], "mmx" ) == 0 ) mask = 1 << 23;
      else if ( strcmp( argv[1], "sse" ) == 0 ) mask = 1 << 25;
      else if ( strcmp( argv[1], "sse2" ) == 0 ) mask = 1 << 26;
   }

   if ( mask == 0 )
   {
      fprintf( stderr, "usage: cputest [mmx|sse|sse2]\n" );
      exit( 1 );
   }
   
   cpuid( 0, &a, &b, &c, &d );

   if ( a >= 1 )
   {
      cpuid( 1, &a, &b, &c, &d );

      if ( ( d & mask ) != 0 ) exit( 0 );
   }

   exit( 1 );
}
