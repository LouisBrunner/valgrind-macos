/* Test whether all data races are detected in a multithreaded program with
 * barriers.
 */


#define _GNU_SOURCE

/***********************/
/* Include directives. */
/***********************/

#include <assert.h>
#include <limits.h>
#include <pthread.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>


/*********************/
/* Type definitions. */
/*********************/

struct threadinfo
{
  pthread_barrier_t* b;
  pthread_t          tid;
  int8_t*            array;
  int                iterations;
};


/********************/
/* Local variables. */
/********************/

static int s_silent;


/*************************/
/* Function definitions. */
/*************************/

/** Single thread, which touches p->iterations elements of array p->array.
 * Each modification of an element of p->array is a data race. */
static void* threadfunc(struct threadinfo* p)
{
  int i;
  int8_t* const array = p->array;
  pthread_barrier_t* const b = p->b;
  if (! s_silent)
    printf("thread %lx iteration 0\n", pthread_self());
  pthread_barrier_wait(b);
  for (i = 0; i < p->iterations; i++)
  {
    if (! s_silent)
      printf("thread %lx iteration %d; writing to %p\n",
             pthread_self(), i + 1, &array[i]);
    array[i] = i;
    pthread_barrier_wait(b);
  }
  return 0;
}

/** Actual test, consisting of nthread threads. */
static void barriers_and_races(const int nthread, const int iterations)
{
  int i, res;
  pthread_attr_t attr;
  struct threadinfo* t;
  pthread_barrier_t b;
  int8_t* array;

  t = malloc(nthread * sizeof(struct threadinfo));
  array = malloc(iterations * sizeof(array[0]));

  if (! s_silent)
    printf("&array[0] = %p\n", array);

  pthread_barrier_init(&b, 0, nthread);

  pthread_attr_init(&attr);
  res = pthread_attr_setstacksize(&attr, PTHREAD_STACK_MIN + 4096);
  assert(res == 0);

  for (i = 0; i < nthread; i++)
  {
    t[i].b = &b;
    t[i].array = array;
    t[i].iterations = iterations;
    res = pthread_create(&t[i].tid, &attr, (void*(*)(void*))threadfunc, &t[i]);
    if (res != 0) {
      fprintf(stderr, "Could not create thread #%d (of %d): %s\n",
              i, nthread, strerror(res));
      exit(1);
    }
  }

  pthread_attr_destroy(&attr);

  for (i = 0; i < nthread; i++)
  {
    pthread_join(t[i].tid, 0);
  }

  pthread_barrier_destroy(&b);

  free(array);
  free(t);
}

int main(int argc, char** argv)
{
  int nthread;
  int iterations;

  nthread    = (argc > 1) ? atoi(argv[1]) : 2;
  iterations = (argc > 2) ? atoi(argv[2]) : 3;
  s_silent   = (argc > 3) ? atoi(argv[3]) : 0;

  barriers_and_races(nthread, iterations);

  return 0;
}
