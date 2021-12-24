
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

// This program determines which architectures that this Valgrind installation
// supports, which depends on the what was chosen at configure-time.  For
// example, if Valgrind is installed on an AMD64 machine but has been
// configured with --enable-only32bit then this program will match "x86" but
// not "amd64".
//
// We return:
// - 0 if the machine matches the asked-for arch
// - 1 if it doesn't match but does match the name of another arch
// - 2 if it doesn't match the name of any arch
// - 3 if there was a usage error (it also prints an error message)

// Nb: When updating this file for a new architecture, add the name to
// 'all_archs' as well as adding go().

#define False  0
#define True   1
typedef int    Bool;

char* all_archs[] = {
   "x86",
   "amd64",
   "ppc32",
   "ppc64",
   "ppc64le",
   "arm",
   "arm64",
   "s390x",
   "mips32",
   "mips64",
   "nanomips",
   NULL
};

static Bool go(char* arch)
{ 
#if defined(VGP_x86_linux) || defined(VGP_x86_darwin) \
   || defined(VGP_x86_solaris) || defined(VGP_x86_freebsd)
   if ( 0 == strcmp( arch, "x86"   ) ) return True;

#elif defined(VGP_amd64_linux) || defined(VGP_amd64_darwin) \
   || defined(VGP_amd64_solaris) || defined(VGP_amd64_freebsd)
#if defined(VGA_SEC_x86)
   if ( 0 == strcmp( arch, "x86"   ) ) return True;
#endif
   if ( 0 == strcmp( arch, "amd64" ) ) return True;

#elif defined(VGP_ppc32_linux)
   if ( 0 == strcmp( arch, "ppc32" ) ) return True;

#elif defined(VGP_ppc64be_linux)
   if ( 0 == strcmp( arch, "ppc64" ) ) return True;
#if defined(VGA_SEC_ppc32)
   if ( 0 == strcmp( arch, "ppc32" ) ) return True;
#endif

#elif defined(VGP_ppc64le_linux)
   if ( 0 == strcmp( arch, "ppc64" ) ) return True;

#elif defined(VGP_s390x_linux)
   if ( 0 == strcmp( arch, "s390x" ) ) return True;

#elif defined(VGP_arm_linux)
   if ( 0 == strcmp( arch, "arm" ) ) return True;

#elif defined(VGP_arm64_linux)
   if ( 0 == strcmp( arch, "arm64" ) ) return True;

#elif defined(VGP_mips32_linux)
   if ( 0 == strcmp( arch, "mips32" ) ) return True;

#elif defined(VGP_mips64_linux)
   if ( 0 == strcmp( arch, "mips64" ) ) return True;

#elif defined(VGP_nanomips_linux)
   if ( 0 == strcmp( arch, "nanomips" ) ) return True;
#else
#  error Unknown platform
#endif   // VGP_*

   return False;
}

//---------------------------------------------------------------------------
// main
//---------------------------------------------------------------------------
int main(int argc, char **argv)
{
   int i;
   if ( argc != 2 ) {
      fprintf( stderr, "usage: arch_test <arch-type>\n" );
      exit(3);             // Usage error.
   }
   if (go( argv[1] )) {
      return 0;            // Matched.
   }
   for (i = 0; NULL != all_archs[i]; i++) {
      if ( 0 == strcmp( argv[1], all_archs[i] ) )
         return 1;         // Didn't match, but named another arch.
   }
   return 2;               // Didn't match any archs.
}
