// this is a variation of bug484480.c using sem_timedwait

#include <pthread.h>
#include <semaphore.h>
#include <stdio.h>
#include <time.h>
#include <stdlib.h>
#include <string.h>

static char* result;
static sem_t sem;

static void* func(void* data)
{
   result = "Finished!";
   sem_post(&sem);
   return NULL;
}

int main(void)
{
   sem_init(&sem, 0, 0);

   pthread_t tid;
   pthread_create(&tid, NULL, func, NULL);

   struct timespec ts;

   if (clock_gettime(CLOCK_REALTIME, &ts) == -1) {
      perror("clock_gettime");
      exit(EXIT_FAILURE);
   }
   
   ts.tv_nsec += 100000;

   while (sem_timedwait(&sem, &ts))
      ; // do nothing but keep retrying

   printf("%s\n", result);

   pthread_join(tid, NULL);
}
