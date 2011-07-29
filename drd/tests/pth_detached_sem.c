/**
 * Test whether detached threads are handled properly.
 * This test program is based on pth_detached.c, with the difference that
 * in this test program the main thread uses a counting semaphore instead
 * of a counter protected by a mutex to wait until all detached threads
 * finished.
 */


#include <assert.h>
#include <pthread.h>
#include <semaphore.h>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>


static sem_t s_sem;


static void increment_finished_count()
{
  sem_post(&s_sem);
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
  int thread_arg[count1 > count2 ? count1 : count2];
  int i;
  int detachstate;
  pthread_attr_t attr;

  for (i = 0; i < count1 || i < count2; i++)
    thread_arg[i] = i;

  sem_init(&s_sem, 0, 0);

  pthread_attr_init(&attr);
  pthread_attr_setdetachstate(&attr, PTHREAD_CREATE_DETACHED);
  assert(pthread_attr_getdetachstate(&attr, &detachstate) == 0);
  assert(detachstate == PTHREAD_CREATE_DETACHED);
  pthread_attr_setstacksize(&attr, 16384);
  // Create count1 detached threads by setting the "detached" property via
  // thread attributes.
  for (i = 0; i < count1; i++)
  {
    pthread_t thread;
    pthread_create(&thread, &attr, thread_func1, &thread_arg[i]);
  }
  // Create count2 detached threads by letting the threads detach themselves.
  pthread_attr_setdetachstate(&attr, PTHREAD_CREATE_JOINABLE);
  assert(pthread_attr_getdetachstate(&attr, &detachstate) == 0);
  assert(detachstate == PTHREAD_CREATE_JOINABLE);
  for (i = 0; i < count2; i++)
  {
    pthread_t thread;
    pthread_create(&thread, &attr, thread_func2, &thread_arg[i]);
  }
  pthread_attr_destroy(&attr);

  // Wait until all detached threads have written their output to stdout.
  for (i = 0; i < count1 + count2; i++)
  {
    sem_wait(&s_sem);
  }

  write(STDOUT_FILENO, "\n", 1);
  fprintf(stderr, "Done.\n");

  sem_destroy(&s_sem);

  return 0;
}
