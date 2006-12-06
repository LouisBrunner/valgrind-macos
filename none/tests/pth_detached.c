/* Test whether detached threads are handled properly.
   Contributed by Bart Van Assche (bart.vanassche@gmail.com).
*/

#include <assert.h>
#include <pthread.h>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>

static int s_finished_count;

static void* thread_func1(void* arg)
{
  write(STDOUT_FILENO, ".", 1);
  s_finished_count++;
  return 0;
}

static void* thread_func2(void* arg)
{
  pthread_detach(pthread_self());
  write(STDOUT_FILENO, "*", 1);
  s_finished_count++;
  return 0;
}

int main(int argc, char** argv)
{
  const int count1 = argc > 1 ? atoi(argv[1]) : 100;
  const int count2 = argc > 2 ? atoi(argv[2]) : 100;
  int i;
  int detachstate;
  pthread_attr_t attr;
  
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
    pthread_create(&thread, &attr, thread_func1, 0);
  }
  // Create count2 detached threads by letting the threads detach themselves.
  pthread_attr_setdetachstate(&attr, PTHREAD_CREATE_JOINABLE);
  assert(pthread_attr_getdetachstate(&attr, &detachstate) == 0);
  assert(detachstate == PTHREAD_CREATE_JOINABLE);
  for (i = 0; i < count2; i++)
  {
    pthread_t thread;
    pthread_create(&thread, &attr, thread_func2, 0);
  }
  pthread_attr_destroy(&attr);

  // Wait until all detached threads have written their output to stdout.
  while (s_finished_count < count1 + count2)
  {
    struct timespec delay = { 0, 1 * 1000 * 1000 };
    nanosleep(&delay, 0);
  }

  printf("\n");
  return 0;
}
