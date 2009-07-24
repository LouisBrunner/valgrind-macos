/**
 * @file  rwlock_test.c
 *
 * @brief Multithreaded test program that triggers various access patterns
 *        without triggering any race conditions.
 */


#define _GNU_SOURCE 1

#include <assert.h>
#include <limits.h>  /* PTHREAD_STACK_MIN */
#include <pthread.h>
#include <stdio.h>
#include <stdlib.h>  /* malloc() */
#include <string.h>  /* strerror() */
#include <unistd.h>  /* getopt() */

static int s_num_threads = 10;
static int s_num_iterations = 1000;
static pthread_mutex_t s_mutex;
static long long s_grand_sum; /* protected by s_mutex. */
static pthread_rwlock_t s_rwlock;
static int s_counter; /* protected by s_rwlock. */

static void* thread_func(void* arg)
{
  int i, r;
  int sum1 = 0, sum2 = 0;

  for (i = s_num_iterations; i > 0; i--)
  {
    r = pthread_rwlock_rdlock(&s_rwlock);
    assert(! r);
    sum1 += s_counter;
    r = pthread_rwlock_unlock(&s_rwlock);
    assert(! r);
    r = pthread_rwlock_wrlock(&s_rwlock);
    assert(! r);
    sum2 += s_counter++;
    r = pthread_rwlock_unlock(&s_rwlock);
    assert(! r);
  }

  pthread_mutex_lock(&s_mutex);
  s_grand_sum += sum2;
  pthread_mutex_unlock(&s_mutex);

  return 0;
}

int main(int argc, char** argv)
{
  pthread_attr_t attr;
  pthread_t* tid;
  int threads_created;
  int optchar;
  int err;
  int i;
  int expected_counter;
  long long expected_grand_sum;

  while ((optchar = getopt(argc, argv, "i:t:")) != EOF)
  {
    switch (optchar)
    {
    case 'i':
      s_num_iterations = atoi(optarg);
      break;
    case 't':
      s_num_threads = atoi(optarg);
      break;
    default:
      fprintf(stderr, "Error: unknown option '%c'.\n", optchar);
      return 1;
    }
  }

  pthread_mutex_init(&s_mutex, NULL);
  pthread_rwlock_init(&s_rwlock, NULL);

  pthread_attr_init(&attr);
  err = pthread_attr_setstacksize(&attr, PTHREAD_STACK_MIN + 4096);
  assert(err == 0);

  tid = calloc(s_num_threads, sizeof(*tid));
  threads_created = 0;
  for (i = 0; i < s_num_threads; i++)
  {
    err = pthread_create(&tid[i], &attr, thread_func, 0);
    if (err)
      printf("failed to create thread %d: %s\n", i, strerror(err));
    else
      threads_created++;
  }

  pthread_attr_destroy(&attr);

  for (i = 0; i < s_num_threads; i++)
  {
    if (tid[i])
      pthread_join(tid[i], 0);
  }
  free(tid);

  expected_counter = threads_created * s_num_iterations;
  fprintf(stderr, "s_counter - expected_counter = %d\n",
          s_counter - expected_counter);
  expected_grand_sum = 1ULL * expected_counter * (expected_counter - 1) / 2;
  fprintf(stderr, "s_grand_sum - expected_grand_sum = %lld\n",
          s_grand_sum - expected_grand_sum);
  fprintf(stderr, "Finished.\n");

  return 0;
}
