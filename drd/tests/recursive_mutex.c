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
  if (pthread_mutex_trylock(p) != 0)
    fprintf(stderr, "first lock call failed !\n");
  if (pthread_mutex_trylock(p) != 0)
    fprintf(stderr, "second lock call failed !\n");
  if (pthread_mutex_unlock(p) != 0)
    fprintf(stderr, "first unlock call failed !\n");
  if (pthread_mutex_unlock(p) != 0)
    fprintf(stderr, "second unlock call failed !\n");
}

int main(int argc, char** argv)
{
#if defined(HAVE_PTHREAD_RECURSIVE_MUTEX_INITIALIZER_NP)
  {
    pthread_mutex_t m = PTHREAD_RECURSIVE_MUTEX_INITIALIZER_NP;

    fprintf(stderr, "Recursive mutex (statically initialized).\n");
    lock_twice(&m);
    pthread_mutex_destroy(&m);
  }
#endif
#if defined(HAVE_PTHREAD_MUTEX_RECURSIVE_NP)
  {
    pthread_mutex_t m;
    pthread_mutexattr_t attr;

    fprintf(stderr, "\nRecursive mutex (initialized via mutex attributes).\n");
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

    fprintf(stderr, "\nError checking mutex.\n");
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

    fprintf(stderr, "\nNon-recursive mutex.\n");
    lock_twice(&m);
  }

  fprintf(stderr, "\nDone.\n");

  return 0;
}
