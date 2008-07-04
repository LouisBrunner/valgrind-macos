/** Hold several types of synchronization objects locked as long as specified.
 */

#define _GNU_SOURCE 1

#include <assert.h>
#include <pthread.h>
#include <stdio.h>
#include <stdlib.h>
#include <time.h>
#include <unistd.h>


static void delay_ms(const int ms)
{
  struct timespec ts;

  assert(ms >= 0);
  ts.tv_sec = ms / 1000;
  ts.tv_nsec = (ms % 1000) * 1000 * 1000;
  nanosleep(&ts, 0);
}

int main(int argc, char** argv)
{
  int interval = 0;
  int optchar;
  pthread_mutex_t     mutex;
  pthread_mutexattr_t mutexattr;
  pthread_rwlock_t    rwlock;

  while ((optchar = getopt(argc, argv, "i:")) != EOF)
  {
    switch (optchar)
    {
    case 'i':
      interval = atoi(optarg);
      break;
    default:
      fprintf(stderr, "Usage: %s [-i <interval time in ms>].\n", argv[0]);
      break;
    }
  }

  fprintf(stderr, "Locking mutex ...\n");

  pthread_mutexattr_init(&mutexattr);
  pthread_mutexattr_settype(&mutexattr, PTHREAD_MUTEX_RECURSIVE);
  pthread_mutex_init(&mutex, &mutexattr);
  pthread_mutexattr_destroy(&mutexattr);
  pthread_mutex_lock(&mutex);
  delay_ms(interval);
  pthread_mutex_lock(&mutex);
  pthread_mutex_unlock(&mutex);
  pthread_mutex_unlock(&mutex);
  pthread_mutex_destroy(&mutex);

  fprintf(stderr, "Locking rwlock exclusively ...\n");

  pthread_rwlock_init(&rwlock, 0);
  pthread_rwlock_wrlock(&rwlock);
  delay_ms(interval);
  pthread_rwlock_unlock(&rwlock);
  pthread_rwlock_destroy(&rwlock);

  fprintf(stderr, "Locking rwlock shared ...\n");

  pthread_rwlock_init(&rwlock, 0);
  pthread_rwlock_rdlock(&rwlock);
  delay_ms(interval);
  pthread_rwlock_rdlock(&rwlock);
  pthread_rwlock_unlock(&rwlock);
  pthread_rwlock_unlock(&rwlock);
  pthread_rwlock_destroy(&rwlock);

  fprintf(stderr, "Done.\n");

  return 0;
}
