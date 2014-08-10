/* Stress test for the --free-is-write command-line option. */

#include <assert.h>
#include <limits.h>
#include <pthread.h>
#include <pthread.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

#define MALLOC_SIZE 22816
#define THREAD_COUNT 10
#define MALLOC_COUNT 1000

static pthread_cond_t cond = PTHREAD_COND_INITIALIZER;
// 'mutex' protects 'count'.
static pthread_mutex_t mutex = PTHREAD_MUTEX_INITIALIZER;
static unsigned count;

void* thread_func(void* arg)
{
  unsigned i;

  for (i = 0; i < MALLOC_COUNT; ++i) {
    void* ptr;

    ptr = malloc(MALLOC_SIZE);
    memset(ptr, 0, MALLOC_SIZE);
    free(ptr);
  }

  pthread_mutex_lock(&mutex);
  ++count;
  pthread_cond_signal(&cond);
  pthread_mutex_unlock(&mutex);

  return 0;
}

int main(int argc, char **argv)
{
  pthread_t thread[THREAD_COUNT];
  int result;
  int i;

  for (i = 0; i < THREAD_COUNT; i++) {
    pthread_attr_t attr;
    pthread_attr_init(&attr);
    pthread_attr_setstacksize(&attr, PTHREAD_STACK_MIN + 4096);
    result = pthread_create(&thread[i], &attr, thread_func, 0);
    pthread_attr_destroy(&attr);
    assert(result == 0);
  }

  pthread_mutex_lock(&mutex);
  while (count < THREAD_COUNT && pthread_cond_wait(&cond, &mutex) == 0)
    ;
  pthread_mutex_unlock(&mutex);

  for (i = 0; i < THREAD_COUNT; i++)
    pthread_join(thread[i], 0);

  fflush(stdout);

  fprintf(stderr, "Done.\n");

  return 0;
}
