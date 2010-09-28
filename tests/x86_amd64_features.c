
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>

// This file determines x86/AMD64 features a processor supports.
//
// We return:
// - 0 if the machine matches the asked-for feature.
// - 1 if the machine does not.
// - 2 if the asked-for feature isn't recognised (this will be the case for
//     any feature if run on a non-x86/AMD64 machine).
// - 3 if there was a usage error (it also prints an error message).

#define False  0
#define True   1
typedef int    Bool;


#if defined(VGA_x86) || defined(VGA_amd64)
static void cpuid ( unsigned int n,
                    unsigned int* a, unsigned int* b,
                    unsigned int* c, unsigned int* d )
{
   __asm__ __volatile__ (
      "cpuid"
      : "=a" (*a), "=b" (*b), "=c" (*c), "=d" (*d)      /* output */
      : "0" (n)         /* input */
   );
}

static Bool vendorStringEquals ( char* str )
{
   char vstr[13];
   unsigned int a, b, c, d;
   cpuid(0, &a, &b, &c, &d);
   memcpy(&vstr[0], &b, 4);
   memcpy(&vstr[4], &d, 4);
   memcpy(&vstr[8], &c, 4);
   vstr[12] = 0;
   return 0 == strcmp(vstr, str);
}

static Bool go(char* cpu)
{ 
   unsigned int level = 0, cmask = 0, dmask = 0, a, b, c, d;
   Bool require_amd = False;

   if        ( strcmp( cpu, "x86-fpu" ) == 0 ) {
     level = 1;
     dmask = 1 << 0;
   } else if ( strcmp( cpu, "x86-cmov" ) == 0 ) {
     level = 1;
     dmask = 1 << 15;
   } else if ( strcmp( cpu, "x86-mmx" ) == 0 ) {
     level = 1;
     dmask = 1 << 23;
   } else if ( strcmp( cpu, "x86-mmxext" ) == 0 ) {
     level = 0x80000001;
     dmask = 1 << 22;
   } else if ( strcmp( cpu, "x86-sse" ) == 0 ) {
     level = 1;
     dmask = 1 << 25;
   } else if ( strcmp( cpu, "x86-sse2" ) == 0 ) {
     level = 1;
     dmask = 1 << 26;
   } else if ( strcmp( cpu, "x86-sse3" ) == 0 ) {
     level = 1;
     cmask = 1 << 0;
   } else if ( strcmp( cpu, "x86-ssse3" ) == 0 ) {
     level = 1;
     cmask = 1 << 9;
   } else if ( strcmp( cpu, "x86-lzcnt" ) == 0 ) {
     level = 0x80000001;
     cmask = 1 << 5;
     require_amd = True;
#if defined(VGA_amd64)
   } else if ( strcmp( cpu, "amd64-sse3" ) == 0 ) {
     level = 1;
     cmask = 1 << 0;
   } else if ( strcmp( cpu, "amd64-pclmulqdq" ) == 0 ) {
     level = 1;
     cmask = 1 << 1;
   } else if ( strcmp( cpu, "amd64-ssse3" ) == 0 ) {
     level = 1;
     cmask = 1 << 9;
   } else if ( strcmp( cpu, "amd64-cx16" ) == 0 ) {
     level = 1;
     cmask = 1 << 13;
   } else if ( strcmp( cpu, "amd64-lzcnt" ) == 0 ) {
     level = 0x80000001;
     cmask = 1 << 5;
     require_amd = True;
#endif
   } else {
     return 2;          // Unrecognised feature.
   }

   assert( !(cmask != 0 && dmask != 0) );
   assert( !(cmask == 0 && dmask == 0) );

   if (require_amd && !vendorStringEquals("AuthenticAMD"))
      return 1; // Feature not present
      // regardless of what that feature actually is

   cpuid( level & 0x80000000, &a, &b, &c, &d );

   if ( a >= level ) {
      cpuid( level, &a, &b, &c, &d );

      if (dmask > 0 && (d & dmask) != 0) return 0;    // Feature present.
      if (cmask > 0 && (c & cmask) != 0) return 0;    // Feature present.
   }
   return 1;                                          // Feature not present.
}

#else

static Bool go(char* cpu)
{
   return 2;      // Feature not recognised (non-x86/AMD64 machine!)
}

#endif   // defined(VGA_x86)  || defined(VGA_amd64)


//---------------------------------------------------------------------------
// main
//---------------------------------------------------------------------------
int main(int argc, char **argv)
{
   if ( argc != 2 ) {
      fprintf( stderr, "usage: x86_amd64_features <feature>\n" );
      exit(3);                // Usage error.
   }
   return go(argv[1]);
}
