/*
 * Test program that locks and unlocks a process-shared mutex.
 * See also https://bugs.kde.org/show_bug.cgi?id=187048.
 */


#define _GNU_SOURCE

#include <stdio.h>
#include <pthread.h>


int main()
{
  pthread_mutex_t mutex;
  pthread_mutexattr_t attr;

  pthread_mutexattr_init(&attr);
  pthread_mutexattr_setpshared(&attr, PTHREAD_PROCESS_SHARED);
  pthread_mutex_init(&mutex, &attr);
  pthread_mutexattr_destroy(&attr);

  pthread_mutex_lock(&mutex);
  pthread_mutex_unlock(&mutex);
  pthread_mutex_destroy(&mutex);

  fprintf(stderr, "Finished.\n");

  return 0;
}
