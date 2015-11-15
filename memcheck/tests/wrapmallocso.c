#include <stdio.h>
#include <stdlib.h>

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
