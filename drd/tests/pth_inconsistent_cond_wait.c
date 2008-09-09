/** Trigger two kinds of errors: once that condition variable s_cond is
 *  associated with two different mutexes (s_mutex1 and s_mutex2), and two
 *  times that pthread_cond_signal() is called without that the mutex
 *  associated with the condition variable is locked.
 */


#include <pthread.h>
#include <semaphore.h>
#include <unistd.h>

pthread_cond_t  s_cond;
pthread_mutex_t s_mutex1;
pthread_mutex_t s_mutex2;
sem_t           s_sem;

void* thread1(void* arg)
{
  pthread_mutex_lock(&s_mutex1);
  sem_post(&s_sem);
  pthread_cond_wait(&s_cond, &s_mutex1);
  pthread_mutex_unlock(&s_mutex1);
  return 0;
}

void* thread2(void* arg)
{
  pthread_mutex_lock(&s_mutex2);
  sem_post(&s_sem);
  pthread_cond_wait(&s_cond, &s_mutex2);
  pthread_mutex_unlock(&s_mutex2);
  return 0;
}

int main(int argc, char** argv)
{
  pthread_t tid1;
  pthread_t tid2;

  /* Initialize synchronization objects. */
  sem_init(&s_sem, 0, 0);
  pthread_cond_init(&s_cond, 0);
  pthread_mutex_init(&s_mutex1, 0);
  pthread_mutex_init(&s_mutex2, 0);

  /* Create two threads. */
  pthread_create(&tid1, 0, &thread1, 0);
  pthread_create(&tid2, 0, &thread2, 0);

  /* Wait until both threads have called sem_post(). */
  sem_wait(&s_sem);
  sem_wait(&s_sem);

  /* Wait until both threads are waiting inside pthread_cond_wait(). */
  pthread_mutex_lock(&s_mutex1);
  pthread_mutex_lock(&s_mutex2);
  pthread_mutex_unlock(&s_mutex2);
  pthread_mutex_unlock(&s_mutex1);

  /* Signal s_cond twice. */
  pthread_cond_signal(&s_cond);
  pthread_cond_signal(&s_cond);

  /* Join both threads. */
  pthread_join(tid1, 0);
  pthread_join(tid2, 0);

  return 0;
}
