
#include <stdio.h>
#include <stdlib.h>
#include <stdio.h>

#include "valgrind.h"


int baaaad ( void )
{
   int i;
   int spacer0[10];
   int aaa[10];
   int spacer1[10];
   int bbb[10];
   int spacer2[10];
   int ccc[10];
   int spacer3[10];
   VALGRIND_MAKE_NOACCESS_STACK(spacer0, sizeof spacer0);
   VALGRIND_MAKE_NOACCESS_STACK(spacer1, sizeof spacer1);
   VALGRIND_MAKE_NOACCESS_STACK(spacer2, sizeof spacer2);
   VALGRIND_MAKE_NOACCESS_STACK(spacer3, sizeof spacer3);
   printf("reading %p\n", &aaa[-3]);
   return aaa[-3];
   for (i = 0; i < 10; i++) {
     printf("poking addr %p\n", & spacer1[i]);
     spacer0[i] = spacer1[i] = spacer2[i] = spacer3[i] = 0;
   }
}


int main ( void )
{
   int z = baaaad();
   return 0;
}
