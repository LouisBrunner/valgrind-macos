
#include <stdio.h>
#include <stdlib.h>
#include <stdio.h>

#include "../memcheck.h"

int baaaad ( void )
{
   int spacer0[10];
   int aaa[10] __attribute__((unused));
   int spacer1[10];
   int bbb[10] __attribute__((unused));
   int spacer2[10];
   int ccc[10] __attribute__((unused));
   int spacer3[10];
   VALGRIND_MAKE_NOACCESS_STACK(spacer0, sizeof spacer0);
   VALGRIND_MAKE_NOACCESS_STACK(spacer1, sizeof spacer1);
   VALGRIND_MAKE_NOACCESS_STACK(spacer2, sizeof spacer2);
   VALGRIND_MAKE_NOACCESS_STACK(spacer3, sizeof spacer3);
   printf("reading memory\n");
   if ((int*)0xDEADBEEF == &aaa[-3]) { printf("DEAD BEEF!\n"); }
   return aaa[-3];
}


int main ( void )
{
   int z = baaaad();
   return z;
}
