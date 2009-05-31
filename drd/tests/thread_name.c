/* Test whether assigning names to threads works properly. */


#include <pthread.h>
#include <stddef.h>
#include <stdio.h>
#include <string.h>
#include "../../drd/drd.h"


#define NUM_THREADS 10


static pthread_mutex_t s_mutex;
static pthread_cond_t  s_cond;
static int s_counter;

static void* thread_func(void* argp)
{
  const int thread_num = (ptrdiff_t)(argp);
  pthread_mutex_t invalid_mutex;
  char thread_name[32];

  snprintf(thread_name, sizeof(thread_name),
           "thread_func instance %d", thread_num + 1);
  ANNOTATE_THREAD_NAME(thread_name);

  memset(&invalid_mutex, 0xff, sizeof(invalid_mutex));

  pthread_mutex_lock(&s_mutex);
  while (s_counter != thread_num)
    pthread_cond_wait(&s_cond, &s_mutex);
  fprintf(stderr, "\n%s\n\n", thread_name);
  pthread_mutex_unlock(&invalid_mutex);
  s_counter++;
  pthread_cond_broadcast(&s_cond);
  pthread_mutex_unlock(&s_mutex);

  return 0;
}


int main(int arg, char** argv)
{
  int i;
  pthread_t tid[NUM_THREADS];

  pthread_mutex_init(&s_mutex, 0);
  pthread_cond_init(&s_cond, 0);

  for (i = 0; i < NUM_THREADS; i++)
    pthread_create(&tid[i], 0, thread_func, (void*)(ptrdiff_t)i);

  for (i = 0; i < NUM_THREADS; i++)
    pthread_join(tid[i], 0);

  pthread_cond_destroy(&s_cond);
  pthread_mutex_destroy(&s_mutex);

  return 0;
}
