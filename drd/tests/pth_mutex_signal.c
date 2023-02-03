/*
 * Verify that pthread_mutex_lock() is not interrupted by a signal.
 *
 * See also https://bugs.kde.org/show_bug.cgi?id=445743.
 */

#include <assert.h>
#include <pthread.h>
#include <signal.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

#define STACK_SIZE 1024 * 512
#define LONG_SLEEP_TIME 1000000

void *contender_start(void *arg)
{
  pthread_mutex_t *mutex = arg;
  int ret;

  ret = pthread_mutex_lock(mutex);
  assert(ret == 0);
  fprintf(stderr, "contender locked mutex\n");
  fprintf(stderr, "contender unlocking mutex\n");
  pthread_mutex_unlock(mutex);
  fprintf(stderr, "contender unlocked mutex\n");
  return NULL;
}

void nullHandler(int signal, siginfo_t *info, void *context)
{
  static const char *msg = "nullHandler running\n";

  write(STDERR_FILENO, msg, strlen(msg));
}

int main ()
{
  pthread_mutex_t mutex;
  pthread_mutexattr_t mutex_attr;
  pthread_attr_t thread_attr_contender;
  pthread_t contender;
  struct sigaction signalAction;

  // install signal handler
  signalAction.sa_sigaction = nullHandler;
  sigfillset(&signalAction.sa_mask);
  signalAction.sa_flags = SA_SIGINFO;
  sigaction(SIGINT, &signalAction, NULL);

  // initialize the mutex
  pthread_mutexattr_init(&mutex_attr);
  pthread_mutexattr_setprotocol(&mutex_attr, PTHREAD_PRIO_INHERIT);   
  pthread_mutex_init(&mutex, &mutex_attr);
  fprintf(stderr, "mutex initialized\n");

  // lock mutex
  pthread_mutex_lock(&mutex);

  // init and create contender
  pthread_attr_init(&thread_attr_contender);
  pthread_attr_setstacksize(&thread_attr_contender, STACK_SIZE);
  pthread_attr_setinheritsched(&thread_attr_contender, PTHREAD_EXPLICIT_SCHED);
  fprintf(stderr, "thread attributes initialized\n");   
  if (pthread_create(&contender, &thread_attr_contender, &contender_start,
                     &mutex) != 0) {
    fprintf(stderr, "failed to create thread\n");
    return 1;
  }
  fprintf(stderr, "thread created\n");
  pthread_attr_destroy(&thread_attr_contender);

  // Block signals in the current thread such that signals are delivered to the
  // 'contender' thread.
  {
    sigset_t mask;
    sigfillset(&mask);
    pthread_sigmask(SIG_BLOCK, &mask, NULL);
  }

  // wait until the thread is sleeping inside pthread_mutex_lock().
  fprintf(stderr, "sleeping\n");
  usleep(LONG_SLEEP_TIME);

  // signal thread
  fprintf(stderr, "signalling\n");
  pthread_kill(contender, SIGINT);

  fprintf(stderr, "sleeping\n");
  usleep(LONG_SLEEP_TIME);

  fprintf(stderr, "unlocking\n");
  pthread_mutex_unlock(&mutex);

  usleep(LONG_SLEEP_TIME);

  // finally wait for the thread
  fprintf(stderr, "joining thread\n");
  pthread_join(contender, NULL);

  return 0;
}
