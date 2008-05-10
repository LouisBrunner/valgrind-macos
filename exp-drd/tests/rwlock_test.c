/** Multithreaded test program that triggers various access patterns without
 *  triggering any race conditions.
 */


#define _GNU_SOURCE 1

#include <pthread.h>
#include <stdio.h>


static pthread_rwlock_t s_rwlock;
static int s_counter;

static void* thread_func(void* arg)
{
  int i;
  int sum = 0;

  for (i = 0; i < 1000; i++)
  {
    pthread_rwlock_rdlock(&s_rwlock);
    sum += s_counter;
    pthread_rwlock_unlock(&s_rwlock);
    pthread_rwlock_wrlock(&s_rwlock);
    s_counter++;
    pthread_rwlock_unlock(&s_rwlock);
  }

  return 0;
}

int main(int argc, char** argv)
{
  const int thread_count = 10;
  pthread_t tid[thread_count];
  int i;

  for (i = 0; i < thread_count; i++)
  {
    pthread_create(&tid[i], 0, thread_func, 0);
  }

  for (i = 0; i < thread_count; i++)
  {
    pthread_join(tid[i], 0);
  }

  fprintf(stderr, "Finished.\n");

  return 0;
}
