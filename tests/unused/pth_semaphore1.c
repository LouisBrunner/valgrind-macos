
/* Dining philosophers, using semaphores.  From the Ben-Ari book. */

#include <stdio.h>
#include <assert.h>
#include <pthread.h>
#include <semaphore.h>

#define N_PHILOSOPHERS 5

sem_t room;
sem_t forc[N_PHILOSOPHERS];
pthread_t tid[N_PHILOSOPHERS];

void eat ( int i )
{
   printf("%d -> begin eat\n", i);
   i += 17;
   i *= 10000;
   while (i > 0) i--;
   printf("%d ->   end eat\n", i);
}

void think ( int i )
{
   printf("%d -> begin think\n", i);
   i += 23;
   i *= 9000;
   while (i > 0) i--;
   printf("%d ->   end think\n", i);
}

void* philosopher ( void* vi )
{
   int i = (int)vi;
   int res;
   int rounds;
   for (rounds = 0; rounds < 10; rounds++) {
      think(i);
      res = sem_wait(&room); assert(res == 0);
      res = sem_wait(&forc[i]); assert(res == 0);
      res = sem_wait(&forc[(i+1) % N_PHILOSOPHERS]); assert(res == 0);
      eat(i);
      res = sem_post(&forc[i]); assert(res == 0);
      res = sem_post(&forc[(i+1) % N_PHILOSOPHERS]); assert(res == 0);
      res = sem_post(&room); assert(res == 0);
   }
   return NULL;
}

int main ( void )
{
   int i, res;

   res = sem_init(&room, 0, 4); assert(res == 0);
   for (i = 0; i < N_PHILOSOPHERS; i++) {
      res = sem_init ( &forc[i], 0, 1 );
      assert(res == 0);
   }

   for (i = 0; i < N_PHILOSOPHERS; i++) {
      res = pthread_create ( &tid[i], NULL, philosopher, (void*)i );
      assert(res == 0);
   }
   for (i = 0; i < N_PHILOSOPHERS; i++) {
      res = pthread_join ( tid[i], NULL );
      assert(res == 0);
   }
   return 0;
}
