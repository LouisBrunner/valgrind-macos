/* Test whether detached threads are handled properly.
   Contributed by Bart Van Assche (bart.vanassche@gmail.com).
*/

#include <assert.h>
#include <pthread.h>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include "../drd_clientreq.h"

static int s_finished_count;
static pthread_mutex_t s_mutex;

static void set_thread_name(const char* const fmt, const int arg)
{
  int res;
  char name[32];
  snprintf(name, sizeof(name), fmt, arg);
  name[sizeof(name) - 1] = 0;
  VALGRIND_DO_CLIENT_REQUEST(res, 0, VG_USERREQ__SET_THREAD_NAME,
                             name, 0, 0, 0, 0);
}

void increment_finished_count()
{
  pthread_mutex_lock(&s_mutex);
  s_finished_count++;
  pthread_mutex_unlock(&s_mutex);
}

int get_finished_count()
{
  int result;
  pthread_mutex_lock(&s_mutex);
  result = s_finished_count;
  pthread_mutex_unlock(&s_mutex);
  return result;
}

static void* thread_func1(void* arg)
{
  set_thread_name("thread_func1[%d]", *(int*)arg);
  write(STDOUT_FILENO, ".", 1);
  increment_finished_count();
  return 0;
}

static void* thread_func2(void* arg)
{
  set_thread_name("thread_func2[%d]", *(int*)arg);
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

  set_thread_name("main", 0);

  for (i = 0; i < count1 || i < count2; i++)
    thread_arg[i] = i;

  pthread_mutex_init(&s_mutex, 0);
  
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
  while (get_finished_count() < count1 + count2)
  {
    struct timespec delay = { 0, 1 * 1000 * 1000 };
    nanosleep(&delay, 0);
  }

  printf("\n");

  pthread_mutex_destroy(&s_mutex);

  return 0;
}
