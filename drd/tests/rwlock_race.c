/** Cause a race inside code protected by a reader lock.
 */


/* Needed for older glibc's (2.3 and older, at least) who don't
   otherwise "know" about pthread_rwlock_anything or about
   PTHREAD_MUTEX_RECURSIVE (amongst things). */

#define _GNU_SOURCE 1

#include <pthread.h>
#include <stdio.h>
#include <unistd.h>



static pthread_rwlock_t s_rwlock;
static int s_racy;

static void sleep_ms(const int ms)
{
  struct timespec delay = { ms / 1000, (ms % 1000) * 1000 * 1000 };
  nanosleep(&delay, 0);
}

static void* thread_func(void* arg)
{
  pthread_rwlock_rdlock(&s_rwlock);
  s_racy++;
  pthread_rwlock_unlock(&s_rwlock);
  sleep_ms(100);
  return 0;
}

int main(int argc, char** argv)
{
  pthread_t thread1;
  pthread_t thread2;

#if 0

  VALGRIND_DO_CLIENT_REQUEST_STMT(VG_USERREQ__DRD_TRACE_ADDR,
                                  &s_racy, 0, 0, 0, 0);
#endif

  pthread_rwlock_init(&s_rwlock, 0);
  pthread_create(&thread1, 0, thread_func, 0);
  pthread_create(&thread2, 0, thread_func, 0);
  pthread_join(thread1, 0);
  pthread_join(thread2, 0);
  pthread_rwlock_destroy(&s_rwlock);

  fprintf(stderr, "Result: %d\n", s_racy);

  return 0;
}
