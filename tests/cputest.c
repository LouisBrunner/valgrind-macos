#include <stdio.h>
#include <stdlib.h>
#include <string.h>

// We return:
// - 0 if the machine matches the asked-for cpu
// - 1 if it didn't match, but did match the name of another arch
// - 2 otherwise

// When updating this file for a new architecture, add the name to
// 'all_archs' as well as adding go().

#define False  0
#define True   1
typedef int    Bool;

char* all_archs[] = {
   "x86",
   "arm",
   "amd64",
   "ppc",
   NULL
};

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

static Bool go(char* cpu)
{ 
   unsigned int level = 0, mask = 0, a, b, c, d;

   if ( strcmp( cpu, "x86" ) == 0 ) {
     return True;
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
     return False;
   }

   cpuid( level & 0x80000000, &a, &b, &c, &d );

   if ( a >= level ) {
      cpuid( level, &a, &b, &c, &d );

      if ( ( d & mask ) != 0 ) return True;
   }
   return False;
}
#endif // __x86__


#ifdef __arm__
static Bool go(char* cpu)
{
   if ( strcmp( cpu, "arm" ) == 0 )
      return True;
   else 
      return False;
}
#endif // __arm__

#ifdef __ppc__
static Bool go(char* cpu)
{
   if ( strcmp( cpu, "ppc" ) == 0 )
      return True;
   else 
      return False;
}
#endif // __ppc__

int main(int argc, char **argv)
{
   int i;
   if ( argc != 2 ) {
      fprintf( stderr, "usage: cputest <cpu-type>\n" );
      exit( 2 );
   }
   if (go( argv[1] )) {
      return 0;      // matched
   }
   for (i = 0; NULL != all_archs[i]; i++) {
      if ( strcmp( argv[1], all_archs[i] ) == 0 )
         return 1;
   }
   return 2;
}
