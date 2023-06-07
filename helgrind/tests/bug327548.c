#include <pthread.h>
#include <stdio.h>
#include <semaphore.h>

sem_t sem;
pthread_cond_t cond;
pthread_mutex_t mutex;
int finished;

void *f(void *foo) {
   while(1)
   {
      /* Wait for main() to have built mutex/cond */
      sem_wait(&sem);

      pthread_mutex_lock(&mutex);
      finished = 1;
      pthread_cond_signal(&cond);
      pthread_mutex_unlock(&mutex);
   }
   return NULL;
}

int main(void) {
   pthread_t t;
   sem_init(&sem, 0, 0);
   int count = 1000;

   pthread_create(&t, NULL, f, NULL);

   while (count--)
   {
      pthread_mutex_init(&mutex, NULL);
      pthread_cond_init(&cond, NULL);

      pthread_mutex_lock(&mutex);
      /* Tell thread there is a new item to process */
      sem_post(&sem);
      while (!finished)
         pthread_cond_wait(&cond, &mutex);
      pthread_mutex_unlock(&mutex);

      finished = 0;

      pthread_cond_destroy(&cond);
      pthread_mutex_destroy(&mutex);
   }

   return 0;
}
