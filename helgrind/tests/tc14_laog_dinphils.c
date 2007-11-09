
#include <pthread.h>
#include <stdlib.h>
#include <unistd.h>

/* Naive dining philosophers with inconsistent lock acquisition
   ordering. */

static pthread_t phil[5];
static pthread_mutex_t chop[5];

void* dine ( void* arg )
{
   int i;
   long left = (long)arg;
   long right = (left + 1) % 5;
   for (i = 0; i < 1000/*arbitrary*/; i++) {
      pthread_mutex_lock(&chop[left]);
      pthread_mutex_lock(&chop[right]);
      /* eating */
      pthread_mutex_unlock(&chop[left]);
      pthread_mutex_unlock(&chop[right]);
   }
   return NULL;
}

int main ( void )
{
   long i;
   for (i = 0; i < 5; i++)
      pthread_mutex_init( &chop[i], NULL);

   for (i = 0; i < 5; i++)
      pthread_create(&phil[i], NULL, dine, (void*)i );

   sleep(1);

   for (i = 0; i < 5; i++)
      pthread_join(phil[i], NULL);

   return 0;
}
