#include <stdio.h>
#include <stdlib.h>
#include <string.h>

/* main() */
int main(int argc, char **argv)
{
   /* This program is passed in a minimum ISA that the underlying hardwre
    * needs to support.  If the HW supports this ISA or newer, return 0
    * for supported.  Otherwise, return 1 for not supported.  Return 2 for
    * usage error.
    *
    *  First argument is required, it must be an ISA version number.
    *  Second argument "-debug" is optional.  If passed, then the defined ISA
    *  values are printed.
    */
   char *min_isa;
   int isa_level = 0;
   int debug = 0;

   /* set the isa_level set by the Make */

   if ((argc == 3) && (strcmp(argv[2], "-debug") == 0)) {
      debug = 1;

   } else if (argc != 2) {
      fprintf(stderr, "usage: min_power_ISA <ISA> [-debug]\n" );
      exit(2);
   }

   min_isa = argv[1];

#ifdef HAS_ISA_2_05
   if (debug) printf("HAS_ISA_2_05 is set\n");
   isa_level = 5;
#endif

#ifdef HAS_ISA_2_06
   if (debug) printf("HAS_ISA_2_06 is set\n");
   isa_level = 6;
#endif

#ifdef HAS_ISA_2_07
   if (debug) printf("HAS_ISA_2_07 is set\n");
   isa_level = 7;
#endif

   /* return 0 for supported (success), 1 for not supported (failure) */
   if (strcmp (min_isa, "2.05") == 0) {
      return !(isa_level >= 5);

   } else if (strcmp (min_isa, "2.06") == 0) {
      return !(isa_level >= 6);

   } else if (strcmp (min_isa, "2.07") == 0) {
      return !(isa_level >= 7);

   } else {
      fprintf(stderr, "ERROR: invalid ISA version.  Valid versions numbers are:\n" );
      fprintf(stderr, "       2.05, 2.06, 2.07\n" );
      exit(2);
   }

   return 1;
}
