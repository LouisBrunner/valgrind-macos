#include <stdio.h>
#include <stdlib.h>
#include <string.h>

// We return 0 if the machine matches the asked-for cpu, 1 otherwise.

#ifdef __x86__
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

static int go(char* cpu)
{ 
   unsigned int level = 0, mask = 0, a, b, c, d;

   if ( strcmp( cpu, "x86" ) == 0 ) {
     level = 1;
     mask = 1 << 0;
   } else if ( strcmp( cpu, "x86-fpu" ) == 0 ) {
     level = 1;
     mask = 1 << 0;
   } else if ( strcmp( cpu, "x86-cmov" ) == 0 ) {
     level = 1;
     mask = 1 << 15;
   } else if ( strcmp( cpu, "x86-mmx" ) == 0 ) {
     level = 1;
     mask = 1 << 23;
   } else if ( strcmp( cpu, "x86-mmxext" ) == 0 ) {
     level = 0x80000001;
     mask = 1 << 22;
   } else if ( strcmp( cpu, "x86-sse" ) == 0 ) {
     level = 1;
     mask = 1 << 25;
   } else if ( strcmp( cpu, "x86-sse2" ) == 0 ) {
     level = 1;
     mask = 1 << 26;
   } else {
     return 1;
   }

   cpuid( level & 0x80000000, &a, &b, &c, &d );

   if ( a >= level ) {
      cpuid( level, &a, &b, &c, &d );

      if ( ( d & mask ) != 0 ) return 0;
   }
   return 1;
}
#endif // __x86__


#ifdef __ppc__
static int go(char* cpu)
{
   if ( strcmp( cpu, "ppc" ) == 0 )
      return 0;
   else 
      return 1;
}
#endif // __ppc__


int main(int argc, char **argv)
{
   if ( argc != 2 ) {
      fprintf( stderr, "usage: cputest <cpu-type>\n" );
      exit( 1 );
   }
   return go( argv[1] );
}
