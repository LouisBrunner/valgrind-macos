/** Cancel a thread that holds a lock on a mutex. */


#include <assert.h>
#include <pthread.h>
#include <stdio.h>


pthread_cond_t  s_cond;
pthread_mutex_t s_mutex1;
pthread_mutex_t s_mutex2;


static void* thread(void* arg)
{
  /* Lock s_mutex2. */
  pthread_mutex_lock(&s_mutex2);
  /* Inform the main thread that s_mutex2 has been locked, and wait for pthread_cancel(). */
  pthread_mutex_lock(&s_mutex1);
  pthread_cond_signal(&s_cond);
  pthread_cond_wait(&s_cond, &s_mutex1);
  return 0;
}

int main(int argc, char** argv)
{
  pthread_t tid;

  /* Initialize synchronization objects. */
  pthread_cond_init(&s_cond, 0);
  pthread_mutex_init(&s_mutex1, 0);
  pthread_mutex_init(&s_mutex2, 0);

  /* Create thread. */
  pthread_mutex_lock(&s_mutex1);
  pthread_create(&tid, 0, &thread, 0);

  /* Wait until the created thread has locked s_mutex2. */
  pthread_cond_wait(&s_cond, &s_mutex1);
  pthread_mutex_unlock(&s_mutex1);

  /* Cancel the created thread. */
  pthread_cancel(tid);

  /* Join the created thread. */
  pthread_join(tid, 0);

  fprintf(stderr, "Test finished.\n");

  return 0;
}
