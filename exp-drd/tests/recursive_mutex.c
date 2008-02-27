/** Initialize a recursive mutex and lock it twice.
 *  No error messages may be printed.
 */

#define _GNU_SOURCE

#include <stdio.h>
#include <pthread.h>

static void lock_twice(pthread_mutex_t* const p)
{
  pthread_mutex_lock(p);
  pthread_mutex_lock(p);
  pthread_mutex_unlock(p);
  pthread_mutex_unlock(p);
}

int main(int argc, char** argv)
{
  {
    pthread_mutex_t m = PTHREAD_RECURSIVE_MUTEX_INITIALIZER_NP;

    printf("Recursive mutex (statically initialized).\n");
    lock_twice(&m);
  } 
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
  {
    pthread_mutex_t m = PTHREAD_MUTEX_INITIALIZER;

    printf("Non-recursive mutex.\n");
    lock_twice(&m);
  } 
  return 0;
}
