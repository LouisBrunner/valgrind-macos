#include "config.h"
#include <time.h>
#include <pthread.h>
#include <assert.h>
#include <errno.h>

int main()
{
   struct timespec abstime;
   pthread_mutex_t mutex = PTHREAD_MUTEX_INITIALIZER;
   pthread_cond_t cond = PTHREAD_COND_INITIALIZER;

#ifdef HAVE_CLOCK_GETTIME
   assert(clock_gettime(CLOCK_REALTIME, &abstime)==0);
#else
   abstime.tv_sec = time(NULL) + 2;
   abstime.tv_nsec = 0;
#endif
   abstime.tv_nsec += 1000000000;
   
   assert(pthread_mutex_lock(&mutex)==0);
   assert(pthread_cond_timedwait(&cond, &mutex, &abstime)==EINVAL);
   assert(pthread_mutex_unlock(&mutex)==0);
   
   return 0;
}
