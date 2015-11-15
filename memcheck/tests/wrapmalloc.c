#include <stdio.h>
#include <stdlib.h>

/* Test that a program that has malloc/free interposed in a shared
   library is also intercepted. */

int main ( void )
{
   printf ("start\n");
   void *p = malloc (1024);
   free (p);
   printf ("done\n");
   return 0;
}
