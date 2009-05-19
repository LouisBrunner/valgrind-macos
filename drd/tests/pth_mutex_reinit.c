/* Test program that triggers mutex reinitialization. */


#define _GNU_SOURCE

#include <assert.h>
#include <pthread.h>
#include <stdio.h>
#include <stdlib.h>


int main(int argc, char** argv)
{
  pthread_mutex_t m;
  pthread_mutexattr_t attr;

  pthread_mutexattr_init(&attr);
  pthread_mutexattr_settype(&attr, PTHREAD_MUTEX_NORMAL);
  pthread_mutex_init(&m, &attr);
  pthread_mutexattr_destroy(&attr);
  pthread_mutex_lock(&m);
  pthread_mutex_unlock(&m);

  pthread_mutexattr_init(&attr);
  pthread_mutexattr_settype(&attr, PTHREAD_MUTEX_RECURSIVE);
  pthread_mutex_init(&m, &attr);
  pthread_mutexattr_destroy(&attr);
  pthread_mutex_lock(&m);
  pthread_mutex_unlock(&m);

  pthread_mutex_destroy(&m);

  fprintf(stderr, "Done.\n");

  return 0;
}
