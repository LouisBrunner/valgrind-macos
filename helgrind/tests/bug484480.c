#include <pthread.h>
#include <semaphore.h>
#include <stdio.h>

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

   while (sem_trywait(&sem))
      ; // do nothing but keep retrying

   /* The above loop could be replaced this instead:
    * if (sem_trywait(&sem))
    *    sem_wait(&sem);
    */

   printf("%s\n", result);

   pthread_join(tid, NULL);
}
