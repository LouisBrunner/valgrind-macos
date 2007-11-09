
#include <pthread.h>
#include <stdio.h>
#include <stdlib.h>

/* Simple test program, has a race.  Parent and child both modify x
   with no locking. */

int x = 0;

void* child_fn ( void* arg )
{
   /* Unprotected relative to parent */
   x++;
   return NULL;
}

int main ( void )
{
   pthread_t child;

   if (pthread_create(&child, NULL, child_fn, NULL)) {
      perror("pthread_create");
      exit(1);
   }

   /* Unprotected relative to child */
   x++;

   if (pthread_join(child, NULL)) {
      perror("pthread join");
      exit(1);
   }

   return 0;
}
