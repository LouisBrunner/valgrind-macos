/** Test interception of the various pthread_timed*lock() and pthread_try*lock()
 *  functions. If any of these are not intercepted, an error message will be
 *  printed at unlock time.
 */


/* Needed for older glibc's (2.3 and older, at least) who don't
   otherwise "know" about pthread_rwlock_anything or about
   PTHREAD_MUTEX_RECURSIVE (amongst things). */

#define _GNU_SOURCE 1

#include "../../config.h"
#include <stdio.h>
#include <assert.h>
#include <pthread.h>


int main(int argc, char** argv)
{
  int r;
  pthread_mutex_t mutex;
  pthread_rwlock_t rwlock;
  struct timespec abs_timeout;

  time(&abs_timeout.tv_sec);
  abs_timeout.tv_nsec = 0;
  abs_timeout.tv_sec += 10;

  r = pthread_rwlock_init(&rwlock, NULL); assert(r == 0);
  fprintf(stderr, "Locking rwlock via pthread_rwlock_wrlock().\n");
  r = pthread_rwlock_wrlock(&rwlock); assert(r == 0);
  r = pthread_rwlock_unlock(&rwlock); assert(r == 0);
  fprintf(stderr, "Locking rwlock via pthread_rwlock_trywrlock().\n");
  r = pthread_rwlock_trywrlock(&rwlock); assert(r == 0);
  r = pthread_rwlock_unlock(&rwlock); assert(r == 0);
  fprintf(stderr, "Locking rwlock via pthread_rwlock_timedwrlock().\n");
#ifdef HAVE_PTHREAD_RWLOCK_TIMEDWRLOCK
  r = pthread_rwlock_timedwrlock(&rwlock, &abs_timeout); assert(r == 0);
  r = pthread_rwlock_unlock(&rwlock); assert(r == 0);
#endif
  fprintf(stderr, "Locking rwlock via pthread_rwlock_rdlock().\n");
  r = pthread_rwlock_rdlock(&rwlock); assert(r == 0);
  r = pthread_rwlock_rdlock(&rwlock); assert(r == 0);
  r = pthread_rwlock_rdlock(&rwlock); assert(r == 0);
  r = pthread_rwlock_unlock(&rwlock); assert(r == 0);
  r = pthread_rwlock_unlock(&rwlock); assert(r == 0);
  r = pthread_rwlock_unlock(&rwlock); assert(r == 0);
  fprintf(stderr, "Locking rwlock via pthread_rwlock_tryrdlock().\n");
  r = pthread_rwlock_tryrdlock(&rwlock); assert(r == 0);
  r = pthread_rwlock_unlock(&rwlock); assert(r == 0);
  fprintf(stderr, "Locking rwlock via pthread_rwlock_timedrdlock().\n");
#ifdef HAVE_PTHREAD_RWLOCK_TIMEDRDLOCK
  r = pthread_rwlock_timedrdlock(&rwlock, &abs_timeout); assert(r == 0);
  r = pthread_rwlock_unlock(&rwlock); assert(r == 0);
#endif
  fprintf(stderr, "Attempt to lock for writing recursively (not allowed).\n");
  r = pthread_rwlock_wrlock(&rwlock); assert(r == 0);
  r = pthread_rwlock_wrlock(&rwlock); assert(r != 0);
  r = pthread_rwlock_unlock(&rwlock); assert(r == 0);
  r = pthread_rwlock_destroy(&rwlock); assert(r == 0);

  r = pthread_mutex_init(&mutex, NULL); assert(r == 0);
  fprintf(stderr, "Locking mutex via pthread_mutex_trylock().\n");
  r = pthread_mutex_trylock(&mutex); assert(r == 0);
  r = pthread_mutex_unlock(&mutex); assert(r == 0);
  fprintf(stderr, "Locking mutex via pthread_mutex_lock().\n");
  r = pthread_mutex_lock(&mutex); assert(r == 0);
  r = pthread_mutex_unlock(&mutex); assert(r == 0);
  fprintf(stderr, "Locking mutex via pthread_mutex_timedlock().\n");
#ifdef HAVE_PTHREAD_MUTEX_TIMEDLOCK
  r = pthread_mutex_timedlock(&mutex, &abs_timeout); assert(r == 0);
  r = pthread_mutex_unlock(&mutex); assert(r == 0);
#endif
  r = pthread_mutex_destroy(&mutex);

  return 0;
}
