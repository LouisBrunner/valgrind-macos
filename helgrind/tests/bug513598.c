/* Verify that no assert failure occurs when memory holding a mutex
   is reused for an rwlock without calling pthread_mutex_destroy first. */

#include <pthread.h>

int main(void)
{
   /* Force both locks to occupy the same address. */
   union {
      pthread_mutex_t  mutex;
      pthread_rwlock_t rwlock;
   } u;

   pthread_mutex_init(&u.mutex, NULL);
   /* Deliberately skip pthread_mutex_destroy, simulating memory reuse
      with a different lock type. */
   pthread_rwlock_init(&u.rwlock, NULL);
   pthread_rwlock_rdlock(&u.rwlock);
   pthread_rwlock_unlock(&u.rwlock);
   pthread_rwlock_destroy(&u.rwlock);
   return 0;
}
