/** Initialize several kinds of mutexes and lock each mutex twice.
 *  Note: locking a regular mutex twice causes a deadlock.
 */

#define _GNU_SOURCE

#include <stdio.h>
#include <unistd.h>
#include <pthread.h>
#include "../../config.h"


static void lock_twice(pthread_mutex_t* const p)
{
  pthread_mutex_lock(p);
  pthread_mutex_lock(p);
  pthread_mutex_unlock(p);
  pthread_mutex_unlock(p);
}

int main(int argc, char** argv)
{
  /* Let the program abort after 3 seconds instead of leaving it deadlocked. */
  alarm(3);

#if defined(HAVE_PTHREAD_RECURSIVE_MUTEX_INITIALIZER_NP)
  {
    pthread_mutex_t m = PTHREAD_RECURSIVE_MUTEX_INITIALIZER_NP;

    printf("Recursive mutex (statically initialized).\n");
    lock_twice(&m);
    pthread_mutex_destroy(&m);
  }
#endif
#if defined(HAVE_PTHREAD_MUTEX_RECURSIVE_NP)
  {
    pthread_mutex_t m;
    pthread_mutexattr_t attr;

    printf("Recursive mutex (initialized via mutex attributes).\n");
    pthread_mutexattr_init(&attr);
    pthread_mutexattr_settype(&attr, PTHREAD_MUTEX_RECURSIVE_NP);
    pthread_mutex_init(&m, &attr);
    pthread_mutexattr_destroy(&attr);
    lock_twice(&m);
    pthread_mutex_destroy(&m);
  }
#endif
#if defined(HAVE_PTHREAD_MUTEX_ERRORCHECK_NP)
  {
    pthread_mutex_t m;
    pthread_mutexattr_t attr;

    printf("Error checking mutex.\n");
    pthread_mutexattr_init(&attr);
    pthread_mutexattr_settype(&attr, PTHREAD_MUTEX_ERRORCHECK_NP);
    pthread_mutex_init(&m, &attr);
    pthread_mutexattr_destroy(&attr);
    lock_twice(&m);
    pthread_mutex_destroy(&m);
  }
#endif
  {
    pthread_mutex_t m = PTHREAD_MUTEX_INITIALIZER;

    printf("Non-recursive mutex.\n");
    fflush(stdout);
    lock_twice(&m);
  }
  printf("Done.\n");
  return 0;
}
