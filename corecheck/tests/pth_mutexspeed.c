
#include <stdio.h>
#include <assert.h>
#include <pthread.h>

int main ( void )
{
   const int n = 100000;
   int i, r;
   pthread_mutex_t mx = PTHREAD_MUTEX_INITIALIZER;
   printf("begin %d lock--unlocks\n", n);
   for (i = 0; i < n; i++) {
      r =  pthread_mutex_lock(&mx);
      r |= pthread_mutex_unlock(&mx);
      assert(r == 0);
   }
   printf("done  %d lock--unlocks\n", n);
   return 0;
}
