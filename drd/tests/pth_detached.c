/* Test whether detached threads are handled properly. */

#include <assert.h>
#include <limits.h>  /* PTHREAD_STACK_MIN */
#include <pthread.h>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>

static int s_finished_count; /* protected by s_mutex */
static pthread_mutex_t s_mutex;
static pthread_cond_t s_cond;

static void increment_finished_count()
{
  pthread_mutex_lock(&s_mutex);
  s_finished_count++;
  pthread_cond_signal(&s_cond);
  pthread_mutex_unlock(&s_mutex);
}

static void* thread_func1(void* arg)
{
  write(STDOUT_FILENO, ".", 1);
  increment_finished_count();
  return 0;
}

static void* thread_func2(void* arg)
{
  pthread_detach(pthread_self());
  write(STDOUT_FILENO, ".", 1);
  increment_finished_count();
  return 0;
}

int main(int argc, char** argv)
{
  const int count1 = argc > 1 ? atoi(argv[1]) : 100;
  const int count2 = argc > 2 ? atoi(argv[2]) : 100;
  int i;
  int detachstate;
  pthread_attr_t attr;

  pthread_mutex_init(&s_mutex, 0);
  pthread_cond_init(&s_cond, 0);

  pthread_attr_init(&attr);
  pthread_attr_setdetachstate(&attr, PTHREAD_CREATE_DETACHED);
  assert(pthread_attr_getdetachstate(&attr, &detachstate) == 0);
  assert(detachstate == PTHREAD_CREATE_DETACHED);
  pthread_attr_setstacksize(&attr, PTHREAD_STACK_MIN + 4096);
  // Create count1 detached threads by setting the "detached" property via
  // thread attributes.
  for (i = 0; i < count1; i++)
  {
    pthread_t thread;
    pthread_create(&thread, &attr, thread_func1, NULL);
  }
  // Create count2 detached threads by letting the threads detach themselves.
  pthread_attr_setdetachstate(&attr, PTHREAD_CREATE_JOINABLE);
  assert(pthread_attr_getdetachstate(&attr, &detachstate) == 0);
  assert(detachstate == PTHREAD_CREATE_JOINABLE);
  for (i = 0; i < count2; i++)
  {
    pthread_t thread;
    pthread_create(&thread, &attr, thread_func2, NULL);
  }
  pthread_attr_destroy(&attr);

  // Wait until all detached threads have written their output to stdout.
  pthread_mutex_lock(&s_mutex);
  while (s_finished_count < count1 + count2) {
    const int ret = pthread_cond_wait(&s_cond, &s_mutex);
    assert(ret == 0);
  }
  pthread_mutex_unlock(&s_mutex);

  pthread_cond_destroy(&s_cond);
  pthread_mutex_destroy(&s_mutex);

  write(STDOUT_FILENO, "\n", 1);
  fprintf(stderr, "Done.\n");

  return 0;
}
