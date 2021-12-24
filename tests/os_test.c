
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

// This program determines which OS that this Valgrind installation
// supports, which depends on what was chosen at configure-time.
//
// We return:
// - 0 if the machine matches the asked-for OS and satisfies a
//     version requirement, if any
// - 1 if it doesn't match but does match the name of another OS
// - 2 if it doesn't match the name of any OS
// - 3 if there was a usage error (it also prints an error message)

// Nb: When updating this file for a new OS, add the name to
// 'all_OSes' as well as adding go().

#define False  0
#define True   1
typedef int    Bool;

char* all_OSes[] = {
   "linux",
   "darwin",
   "solaris",
   "freebsd",
   NULL
};

#if defined(VGO_linux)
static Bool matches_version(char *min_version)
{
   int a1, a2, a3, g1, g2, g3;  // 'a' = actual;  'g' = given

   if (min_version == NULL)  return True;  // no version specified

   // get actual version number
   FILE *fp = fopen("/proc/sys/kernel/osrelease", "r");
   if (fp == NULL || fscanf(fp, "%d.%d.%d", &a1, &a2, &a3) != 3) return False;
   fclose(fp);

   // parse given version number
   if (sscanf(min_version, "%d.%d.%d", &g1, &g2, &g3) != 3) return False;

//   printf("actual %d %d %d\n", a1, a2,a3);
//   printf("given  %d %d %d\n", g1, g2,g3);

   if (a1 > g1) return True;
   if (a1 < g1) return False;
   if (a2 > g2) return True;
   if (a2 < g2) return False;
   if (a3 >= g3) return True;

   return False;
}
#endif

static Bool go(char* OS, char *min_version)
{ 
#if defined(VGO_linux)
   if ( 0 == strcmp( OS, "linux" ) && matches_version( min_version )) return True;

#elif defined(VGO_darwin)
   if ( 0 == strcmp( OS, "darwin" ) ) return True;

#elif defined(VGO_solaris)
   if ( 0 == strcmp( OS, "solaris" ) ) return True;

#elif defined(VGO_freebsd)
   if ( 0 == strcmp( OS, "freebsd" ) ) return True;

#else
#  error Unknown OS
#endif   // VGO_*

   return False;
}

//---------------------------------------------------------------------------
// main
//---------------------------------------------------------------------------
int main(int argc, char **argv)
{
   int i;
   if ( argc < 2 ) {
      fprintf( stderr, "usage: os_test <OS-type> [<min-version>]\n" );
      exit(3);             // Usage error.
   }
   if (go( argv[1], argv[2] )) {
      return 0;            // Matched.
   }
   for (i = 0; NULL != all_OSes[i]; i++) {
      if ( 0 == strcmp( argv[1], all_OSes[i] ) )
         return 1;         // Didn't match, but named another OS.
   }
   return 2;               // Didn't match any OSes.
}

