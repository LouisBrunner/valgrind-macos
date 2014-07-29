
#include <pthread.h>
#include <stdlib.h>
#include <unistd.h>
#include <assert.h>
/* Naive dining philosophers with inconsistent lock acquisition
   ordering. */

static pthread_t phil[5];
static struct {
   pthread_mutex_t m;
   char pad[120 - sizeof(pthread_mutex_t)];
} chop[5];

void* dine ( void* arg )
{
   int i;
   long left = (long)arg;
   long right = (left + 1) % 5;
   for (i = 0; i < 1000/*arbitrary*/; i++) {
      pthread_mutex_lock(&chop[left].m);
      pthread_mutex_lock(&chop[right].m);
      /* eating */
      pthread_mutex_unlock(&chop[left].m);
      pthread_mutex_unlock(&chop[right].m);
   }
   return NULL;
}

int main ( void )
{
   long i;
   assert(sizeof(pthread_mutex_t) <= 120);

   for (i = 0; i < 5; i++)
      pthread_mutex_init( &chop[i].m, NULL);

   for (i = 0; i < 5; i++)
      pthread_create(&phil[i], NULL, dine, (void*)i );

   sleep(1);

   for (i = 0; i < 5; i++)
      pthread_join(phil[i], NULL);

   return 0;
}
