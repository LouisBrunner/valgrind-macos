// Compare given libc name and version number to system name and version.

// Returns
// - 0 if the libc name matches is at least the minimum version (if given).
// - 1 if the libc name doesn't match or the version is lower than requested.
// - 2 if the requested libc name isn't recognised.
// - 3 if there was a usage error (it also prints an error message).

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#ifdef __GLIBC__
#include <gnu/libc-version.h>
#endif

#define False  0
#define True   1
typedef int    Bool;

/* Assumes the versions are x.y.z, with y and z optional. */
static __attribute__((unused)) Bool matches_version(char *min_version) {
   int a1=0, a2=0, a3=0, g1=0, g2=0, g3=0;  // 'a' = actual;  'g' = given
   const char *aversion;

   if (min_version == NULL)  return True;  // no version specified

   // get actual version number
#ifdef __GLIBC__
   aversion = gnu_get_libc_version();
#else
   aversion = "unknown";
#endif
   // We expect at least one number.
   if (sscanf(aversion, "%d.%d.%d", &a1, &a2, &a3) < 1) return False;

   // parse given version number.
   if (sscanf(min_version, "%d.%d.%d", &g1, &g2, &g3) < 1) return False;

   if (a1 > g1) return True;
   if (a1 < g1) return False;
   if (a2 > g2) return True;
   if (a2 < g2) return False;
   if (a3 >= g3) return True;

   return False;
}

static Bool go(char* libc, char *min_version)
{
#ifdef __GLIBC__
   if ( 0 == strcmp( libc, "glibc" )
	&& matches_version( min_version ))
      return True;
#endif

   return False;
}

//---------------------------------------------------------------------------
// main
//---------------------------------------------------------------------------
int main(int argc, char **argv)
{
   if ( argc < 2 ) {
      fprintf( stderr, "usage: libc_test <libc-name> [<min-version>]\n" );
      exit(3);             // Usage error.
   }
   if (go( argv[1], argv[2] )) {
      return 0;            // Matched.
   }

   if ( 0 == strcmp ( argv[1], "glibc" ) ) {
     return 1;             // Requested libc name known, but this isn't it.
                           // Or it wasn't the minimum requested version.
   }
   return 2;               // Didn't match any known libc name.
}
