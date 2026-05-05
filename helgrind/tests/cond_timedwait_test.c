#include <pthread.h>
#include <string.h>
#include <assert.h>
#include <errno.h>

int main(void)
{
   pthread_mutex_t mutex = PTHREAD_MUTEX_INITIALIZER;
   pthread_cond_t cond = PTHREAD_COND_INITIALIZER;
   int res;

   // This time has most definitely passed already. (Epoch)
   struct timespec now;
   memset(&now, 0, sizeof(now));

   res = pthread_mutex_lock(&mutex);
   assert(res == 0);
   res = pthread_cond_timedwait(&cond, &mutex, &now);
   assert(res == ETIMEDOUT);

   res = pthread_mutex_unlock(&mutex);
   assert(res == 0);

   res = pthread_mutex_destroy(&mutex);
   assert(res == 0);
   res = pthread_cond_destroy(&cond);
   assert(res == 0);
}
