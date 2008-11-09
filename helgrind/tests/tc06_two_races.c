
#include <pthread.h>
#include <stdio.h>
#include <stdlib.h>

/* Simple test program, has two races.  A happens-before detector can only
   ever detect one of them, though.  XXX: apparently not so; Drd and H 3.4 detect both. */

int unprot1 = 0, unprot2 = 0, prot = 0;
pthread_mutex_t mu = PTHREAD_MUTEX_INITIALIZER;

void* child_fn ( void* arg )
{
   unprot1 ++;
   pthread_mutex_lock( &mu );
   prot ++;
   pthread_mutex_unlock( &mu );
   unprot2 ++;
   return NULL;
}

int main ( void )
{
   pthread_t child;

   if (pthread_create(&child, NULL, child_fn, NULL)) {
      perror("pthread_create");
      exit(1);
   }

   unprot1 ++;
   pthread_mutex_lock( &mu );
   prot ++;
   pthread_mutex_unlock( &mu );
   unprot2 ++;

   if (pthread_join(child, NULL)) {
      perror("pthread join");
      exit(1);
   }

   return 0;
}
