
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

// This program determines which OS that this Valgrind installation
// supports, which depends on what was chosen at configure-time.
//
// We return:
// - 0 if the machine matches the asked-for OS
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
   "aix5",
   "darwin",
   NULL
};

static Bool go(char* OS)
{ 
#if defined(VGO_linux)
   if ( 0 == strcmp( OS, "linux" ) ) return True;

#elif defined(VGO_aix5)
   if ( 0 == strcmp( OS, "aix5" ) ) return True;

#elif defined(VGO_darwin)
   if ( 0 == strcmp( OS, "darwin" ) ) return True;

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
   if ( argc != 2 ) {
      fprintf( stderr, "usage: os_test <OS-type>\n" );
      exit(3);             // Usage error.
   }
   if (go( argv[1] )) {
      return 0;            // Matched.
   }
   for (i = 0; NULL != all_OSes[i]; i++) {
      if ( 0 == strcmp( argv[1], all_OSes[i] ) )
         return 1;         // Didn't match, but named another OS.
   }
   return 2;               // Didn't match any OSes.
}

