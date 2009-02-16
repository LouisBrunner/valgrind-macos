#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>

// This file determines which architectures that this Valgrind installation
// supports, which depends on the machine's architecture.  It also depends
// on the configuration options;  for example, if Valgrind is installed on
// an AMD64 machine but has been configured with --enable-only32bit then
// this program will not match "amd64".
//
// We return:
// - 0 if the machine matches the asked-for cpu
// - 1 if it didn't match, but did match the name of another arch
// - 2 otherwise

// Nb: When updating this file for a new architecture, add the name to
// 'all_archs' as well as adding go().

#define False  0
#define True   1
typedef int    Bool;

char* all_archs[] = {
   "amd64",
   "ppc32",
   "ppc64",
   "x86",
   NULL
};

//-----------------------------------------------------------------------------
// ppc32-linux
//---------------------------------------------------------------------------
#if defined(VGP_ppc32_linux)
static Bool go(char* cpu)
{
   if ( strcmp( cpu, "ppc32" ) == 0 )
      return True;
   return False;
}
#endif   // VGP_ppc32_linux

//---------------------------------------------------------------------------
// ppc64-linux
//---------------------------------------------------------------------------
#if defined(VGP_ppc64_linux)
static Bool go(char* cpu)
{
   if ( strcmp( cpu, "ppc64" ) == 0 )
      return True;
   if ( strcmp( cpu, "ppc32" ) == 0 )
      return True;
   return False;
}
#endif   // VGP_ppc64_linux

//---------------------------------------------------------------------------
// ppc{32,64}-aix
//---------------------------------------------------------------------------
#if defined(VGP_ppc32_aix5) || defined(VGP_ppc64_aix5)
static Bool go(char* cpu)
{
   if (sizeof(void*) == 8) {
      /* cpu is in 64-bit mode */
      if ( strcmp( cpu, "ppc64" ) == 0 )
         return True;
      if ( strcmp( cpu, "ppc32" ) == 0 )
         return True;
   } else {
      if ( strcmp( cpu, "ppc32" ) == 0 )
         return True;
   }
   return False;
}
#endif   // VGP_ppc32_aix5 || VGP_ppc64_aix5

//---------------------------------------------------------------------------
// {x86,amd64}-linux (part 1 of 2)
//---------------------------------------------------------------------------
#if defined(VGP_x86_linux) || defined(VGP_amd64_linux)
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
#endif   // VGP_x86_linux || VGP_amd64_linux

//---------------------------------------------------------------------------
// {x86,amd64}-darwin (part 1 of 2)
//---------------------------------------------------------------------------
#if defined(VGP_x86_darwin) || defined(VGP_amd64_darwin)
static void cpuid ( unsigned int n,
                    unsigned int* a, unsigned int* b,
                    unsigned int* c, unsigned int* d )
{
   __asm__ __volatile__ (
       "pushl %%eax\n\t"
       "pushl %%ebx\n\t"
       "pushl %%ecx\n\t"
       "pushl %%edx\n\t"
       "movl %4, %%eax\n\t"
       "cpuid\n\t"
       "movl %%eax,%0\n\t"
       "movl %%ebx,%1\n\t"
       "movl %%ecx,%2\n\t"
       "movl %%edx,%3\n\t"
       "popl %%edx\n\t"
       "popl %%ecx\n\t"
       "popl %%ebx\n\t"
       "popl %%eax\n\t"
       : "=m" (*a), "=m" (*b), "=m" (*c), "=m" (*d)
       : "mr" (n)
       );
}
#endif   // VGP_x86_darwin || VGP_amd64_darwin

//---------------------------------------------------------------------------
// {x86,amd64}-{linux,darwin} (part 2 of 2)
//---------------------------------------------------------------------------
#if defined(VGP_x86_linux)  || defined(VGP_amd64_linux) || \
    defined(VGP_x86_darwin) || defined(VGP_amd64_darwin)
static Bool go(char* cpu)
{ 
   unsigned int level = 0, cmask = 0, dmask = 0, a, b, c, d;

   if ( strcmp( cpu, "x86" ) == 0 ) {
     return True;
   } else if ( strcmp( cpu, "x86-fpu" ) == 0 ) {
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
#if defined(VGA_amd64)
   } else if ( strcmp( cpu, "amd64" ) == 0 ) {
     return True;
   } else if ( strcmp( cpu, "amd64-sse3" ) == 0 ) {
     level = 1;
     cmask = 1 << 0;
   } else if ( strcmp( cpu, "amd64-ssse3" ) == 0 ) {
     level = 1;
     cmask = 1 << 9;
#endif
   } else {
     return False;
   }

   assert( !(cmask != 0 && dmask != 0) );
   assert( !(cmask == 0 && dmask == 0) );

   cpuid( level & 0x80000000, &a, &b, &c, &d );

   if ( a >= level ) {
      cpuid( level, &a, &b, &c, &d );

      if (dmask > 0 && (d & dmask) != 0) return True;
      if (cmask > 0 && (c & cmask) != 0) return True;
   }
   return False;
}
#endif   // VGP_x86_linux  || VGP_amd64_linux ||
         // VGP_x86_darwin || VGP_amd64_darwin


//---------------------------------------------------------------------------
// main
//---------------------------------------------------------------------------
int main(int argc, char **argv)
{
   int i;
   if ( argc != 2 ) {
      fprintf( stderr, "usage: arch_test <cpu-type>\n" );
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
