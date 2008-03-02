#include <stdio.h>
#include <assert.h>
#include <pthread.h>

int main(int argc, char** argv)
{
  int r;
  pthread_mutex_t mutex;
  pthread_rwlock_t rwlock;
  struct timespec abs_timeout;

  r = clock_gettime(CLOCK_REALTIME, &abs_timeout); assert(r == 0);
  abs_timeout.tv_sec += 10;

  r = pthread_rwlock_init(&rwlock, NULL); assert(r == 0);
  fprintf(stderr, "Locking rwlock via pthread_rwlock_wrlock().\n");
  r = pthread_rwlock_wrlock(&rwlock); assert(r == 0);
  r = pthread_rwlock_unlock(&rwlock); assert(r == 0);
  fprintf(stderr, "Locking rwlock via pthread_rwlock_trywrlock().\n");
  r = pthread_rwlock_trywrlock(&rwlock); assert(r == 0);
  r = pthread_rwlock_unlock(&rwlock); assert(r == 0);
  fprintf(stderr, "Locking rwlock via pthread_rwlock_timedwrlock().\n");
  r = pthread_rwlock_timedwrlock(&rwlock, &abs_timeout); assert(r == 0);
  r = pthread_rwlock_unlock(&rwlock); assert(r == 0);
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
  r = pthread_rwlock_timedrdlock(&rwlock, &abs_timeout); assert(r == 0);
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
  r = pthread_mutex_timedlock(&mutex, &abs_timeout); assert(r == 0);
  r = pthread_mutex_unlock(&mutex); assert(r == 0);
  r = pthread_mutex_destroy(&mutex); assert(r == 0);

  return 0;
}
