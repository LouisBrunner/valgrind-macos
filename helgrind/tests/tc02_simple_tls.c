
#include <pthread.h>
#include <stdio.h>
#include <stdlib.h>

/* Simple test program, no race: parent only modified x after child
   has modified it and then joined with the parent.  Tests simple
   thread lifetime segment handling. */

int x = 0;

void* child_fn ( void* arg )
{
   /* Unprotected relative to parent, but in child's segment only */
   x++;
   return NULL;
}

int main ( void )
{
   pthread_t child;

   x++; /* happens in parent's segment */

   if (pthread_create(&child, NULL, child_fn, NULL)) {
      perror("pthread_create");
      exit(1);
   }

   if (pthread_join(child, NULL)) {
      perror("pthread join");
      exit(1);
   }

   /* Now back in parent's segment */
   x++;

   return 0;
}
