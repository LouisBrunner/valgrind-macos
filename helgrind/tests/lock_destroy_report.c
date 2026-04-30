/* Tests for --track-destroy. */

#include <pthread.h>

int main(void)
{
   pthread_mutex_t  mx;
   pthread_rwlock_t rw;

   pthread_mutex_init(&mx, NULL);
   pthread_rwlock_init(&rw, NULL);

   /* mx is re-initialised without a prior destroy, which should
      trigger a warning with --track-destroy=yes. */
   pthread_mutex_init(&mx, NULL);

   /* mx and rw are undestroyed at process exit, which should be
      reported by --track-destroy=all. */
   return 0;
}
