
#include <pthread.h>
#include <stdio.h>
#include <stdlib.h>

/* Simple test program, has a race.  Parent and child both modify y
   with no locking.  This is the program shown in Fig 2 of the
   original Eraser paper by Savage et al. */

int y = 0, v = 0;
pthread_mutex_t mu = PTHREAD_MUTEX_INITIALIZER;

void* child_fn ( void* arg )
{
   /* "Thread 2" in the paper */
   pthread_mutex_lock( &mu );
   v = v + 1;
   pthread_mutex_unlock( &mu );
   y = y + 1;
   return NULL;
}

int main ( void )
{
   pthread_t child;

   if (pthread_create(&child, NULL, child_fn, NULL)) {
      perror("pthread_create");
      exit(1);
   }

   /* "Thread 1" in the paper */
   y = y + 1;
   pthread_mutex_lock( &mu );
   v = v + 1;
   pthread_mutex_unlock( &mu );

   if (pthread_join(child, NULL)) {
      perror("pthread join");
      exit(1);
   }

   return 0;
}
