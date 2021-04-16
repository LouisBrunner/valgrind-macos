
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>

// This file determines x86/AMD64 features a processor supports.
//
// We return:
// - 0 if the machine has the asked-for feature.
// - 1 if the machine doesn't have the asked-for feature.
// - 2 if the asked-for feature isn't recognised (this will be the case for
//     any feature if run on a non-x86/AMD64 machine).
// - 3 if there was a usage error (it also prints an error message).
// viz:
#define FEATURE_PRESENT       0
#define FEATURE_NOT_PRESENT   1
#define UNRECOGNISED_FEATURE  2
#define USAGE_ERROR           3


#define False  0
#define True   1
typedef int    Bool;


#if defined(VGA_x86) || defined(VGA_amd64)
static void cpuid ( unsigned int n, unsigned int m,
                    unsigned int* a, unsigned int* b,
                    unsigned int* c, unsigned int* d )
{
   __asm__ __volatile__ (
      "cpuid"
      : "=a" (*a), "=b" (*b), "=c" (*c), "=d" (*d)      /* output */
      : "0" (n), "2" (m)         /* input */
   );
}

static Bool vendorStringEquals ( char* str )
{
   char vstr[13];
   unsigned int a, b, c, d;
   cpuid(0, 0, &a, &b, &c, &d);
   memcpy(&vstr[0], &b, 4);
   memcpy(&vstr[4], &d, 4);
   memcpy(&vstr[8], &c, 4);
   vstr[12] = 0;
   return 0 == strcmp(vstr, str);
}

static Bool have_xgetbv ( void )
{
#if defined(VGA_amd64)
   unsigned long long int w;
   __asm__ __volatile__("movq $0,%%rcx ; "
                        ".byte 0x0F,0x01,0xD0 ; " /* xgetbv */
                        "movq %%rax,%0"
                        :/*OUT*/"=r"(w) :/*IN*/
                        :/*TRASH*/"rdx","rcx");
   if ((w & 6) == 6) {
      /* OS has enabled both XMM and YMM state support */
      return True;
   } else {
      return False;
   }
#else
   return False;
#endif
}

static Bool go(char* cpu)
{ 
   unsigned int level = 0, sublevel = 0;
   unsigned int amask = 0, bmask = 0, cmask = 0, dmask = 0;
   unsigned int a, b, c, d;
   Bool require_amd = False;
   Bool require_xgetbv = False;
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
   } else if ( strcmp( cpu, "amd64-sse42" ) == 0 ) {
     level = 1;
     cmask = 1 << 20;
   } else if ( strcmp( cpu, "amd64-avx" ) == 0 ) {
     level = 1;
     cmask = (1 << 27) | (1 << 28);
     require_xgetbv = True;
   } else if (strcmp (cpu,  "amd64-fma4" ) == 0) {
     level = 0x80000001;
     cmask = 1 << 16;
     require_amd = True;
   } else if (strcmp (cpu,  "amd64-f16c" ) == 0) {
      level = 1;
      cmask = 1 << 29;
   } else if (strcmp (cpu,  "amd64-rdrand" ) == 0) {
      level = 1;
      cmask = 1 << 30;
   } else if (strcmp (cpu,  "amd64-rdseed" ) == 0) {
      level = 7;
      bmask = 1 << 18;
#endif
   } else {
     return UNRECOGNISED_FEATURE;
   }

   assert( !(cmask != 0 && dmask != 0 && bmask != 0) );
   assert( !(cmask == 0 && dmask == 0 && bmask == 0) );

   if (require_amd && !vendorStringEquals("AuthenticAMD"))
      return FEATURE_NOT_PRESENT;
      // regardless of what that feature actually is

   cpuid( level & 0x80000000, 0, &a, &b, &c, &d );

   if ( a >= level ) {
      cpuid( level, sublevel, &a, &b, &c, &d );

      if (amask > 0 && (a & amask) == amask)
         return FEATURE_PRESENT;

      if (bmask > 0 && (b & bmask) == bmask)
         return FEATURE_PRESENT;

      if (dmask > 0 && (d & dmask) == dmask) {
         if (require_xgetbv && !have_xgetbv())
            return FEATURE_NOT_PRESENT;
         else
            return FEATURE_PRESENT;
      }
      if (cmask > 0 && (c & cmask) == cmask) {
         if (require_xgetbv && !have_xgetbv())
            return FEATURE_NOT_PRESENT;
         else
            return FEATURE_PRESENT;
      }
   }
   return FEATURE_NOT_PRESENT;
}

#else

static Bool go(char* cpu)
{
   // Feature not recognised (non-x86/AMD64 machine!)
   return UNRECOGNISED_FEATURE;
}

#endif   // defined(VGA_x86)  || defined(VGA_amd64)


//---------------------------------------------------------------------------
// main
//---------------------------------------------------------------------------
int main(int argc, char **argv)
{
   if ( argc != 2 ) {
      fprintf( stderr, "usage: x86_amd64_features <feature>\n" );
      exit(USAGE_ERROR);
   }
   return go(argv[1]);
}
