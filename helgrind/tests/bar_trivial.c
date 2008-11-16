
/* This is the most trivial test I could think of that involves
   barriers.  If H fails to notice the pthread_barrier_wait call then
   it will report a race.  Correct behaviour is not to report a race
   (there isn't one.) */
#define _GNU_SOURCE
#include <pthread.h>
#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include <unistd.h>

int x = 0;

pthread_barrier_t bar;

void* child_fn ( void* arg )
{
   long r, n = (long)arg;

   if (n == 1) x++;

   r = pthread_barrier_wait(&bar); 
   assert(r == 0 || r == PTHREAD_BARRIER_SERIAL_THREAD);

   if (n == 0) x++;

   sleep(1); /* ensure both threads get to this point before
                either exits. */
   return NULL;
}

#define NTHR 2

int main ( void )
{
   long i, r;
   pthread_t thr[NTHR];

   r = pthread_barrier_init(&bar, NULL, NTHR);
   assert(!r);

   for (i = 0; i < NTHR; i++) {
      r = pthread_create(&thr[i], NULL, child_fn, (void*)i);
      assert(!r);
   }

   for (i = 0; i < NTHR; i++) {
      r = pthread_join(thr[i], NULL);
      assert(!r);
   }

   r = pthread_barrier_destroy(&bar); assert(!r);

   printf("x = %d\n", x);
   return 0;
}
