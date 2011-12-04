/* Stress test for the --free-is-write command-line option. */

#include <pthread.h>
#include <assert.h>
#include <unistd.h>
#include <stdlib.h>
#include <stdio.h>
#include <pthread.h>
#include <string.h>

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
    result = pthread_create(&thread[i], 0, thread_func, 0);
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
