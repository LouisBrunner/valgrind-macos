
/* Check that Helgrind does not complain about semaphores with a
   nonzero initial value, when said semaphores are correctly used.
   Also useful for generating VCG of simple semaphore activity, for
   inspection. */

#include <pthread.h>
#include <semaphore.h>
#include <assert.h>

#ifdef __APPLE__
#define OS_IS_DARWIN 1
#else
#define OS_IS_DARWIN 0
#endif

#define N_THREADS 3

void* child_fn ( void* semV )
{
   int r;
   sem_t* sem = (sem_t*)semV;
   r= sem_wait(sem); assert(OS_IS_DARWIN || !r);
   return NULL;
}

int main ( void )
{
   int r, i;
   sem_t sem;
   pthread_t child[N_THREADS];

   r= sem_init(&sem, 0, N_THREADS); assert(OS_IS_DARWIN || !r);

   for (i = 0; i < N_THREADS; i++) {
      r= pthread_create( &child[i], NULL, child_fn, (void*)&sem );
      assert(!r);
   }

   for (i = 0; i < N_THREADS; i++) {
      r= pthread_join( child[i], NULL );
      assert(!r);
   }

   sem_destroy(&sem);
   return 0;
}
