#include <stdio.h>
#include <stdlib.h>

/* Test that a program that has malloc/free interposed in the
   executable is also intercepted. */

int main ( void )
{
   printf ("start\n");
   void *p = malloc (1024);
   free (p);
   printf ("done\n");
   return 0;
}

/* Fake malloc/free functions that just print something. When run
   under memcheck these functions will be intercepted and not print
   anything. */

void *malloc ( size_t size )
{
  printf ("malloc\n");
  return NULL;
}

void free (void *ptr)
{
  printf ("free\n");
}
