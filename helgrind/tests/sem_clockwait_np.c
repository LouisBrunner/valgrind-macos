// this is a variation of bug484480.c using sem_clockwait_np

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

   struct timespec ts = {0, 100000};
   struct timespec ts_remain;

   while (sem_clockwait_np(&sem, CLOCK_REALTIME, 0, &ts, &ts_remain))
      ; // do nothing but keep retrying

   printf("%s\n", result);

   pthread_join(tid, NULL);
}
