/** pthread_spinloc_t test. */


/* Make sure pthread_spinlock_t is available when compiling with older glibc
 * versions (2.3 or before).
 */
#define _GNU_SOURCE

#include <pthread.h>
#include <stdio.h>   /* fprintf() */
#include <stdlib.h>  /* atoi() */


static pthread_barrier_t  s_barrier;
static pthread_spinlock_t s_spinlock;
static int s_iterations;
static int s_counter;


static void* thread_func(void* arg)
{
  int i;

  pthread_barrier_wait(&s_barrier);
  for (i = s_iterations; i > 0; i--)
  {
    pthread_spin_lock(&s_spinlock);
    s_counter++;
    pthread_spin_unlock(&s_spinlock);
  }
  return 0;
}

int main(int argc, char** argv)
{
  int i;
  const int n_threads = 10;
  pthread_t tid[n_threads];

  s_iterations = argc > 1 ? atoi(argv[1]) : 1000;

  fprintf(stderr, "Start of test.\n");
  pthread_barrier_init(&s_barrier, 0, n_threads);
  pthread_spin_init(&s_spinlock, 0);
  for (i = 0; i < n_threads; i++)
    pthread_create(&tid[i], 0, thread_func, 0);
  for (i = 0; i < n_threads; i++)
    pthread_join(tid[i], 0);
  pthread_spin_destroy(&s_spinlock);
  pthread_barrier_destroy(&s_barrier);
  if (s_counter == n_threads * s_iterations)
    fprintf(stderr, "Test successful.\n");
  else
    fprintf(stderr, "Test failed: counter = %d, should be %d\n",
            s_counter, n_threads * s_iterations);
  return 0;
}
