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

static pthread_rwlock_t s_rwlock;
static int s_counter;
static int s_num_threads = 10;
static int s_num_iterations = 1000;

static void* thread_func(void* arg)
{
  int i;
  int sum = 0;

  for (i = s_num_iterations; i > 0; i--)
  {
    pthread_rwlock_rdlock(&s_rwlock);
    sum += s_counter;
    pthread_rwlock_unlock(&s_rwlock);
    pthread_rwlock_wrlock(&s_rwlock);
    s_counter++;
    pthread_rwlock_unlock(&s_rwlock);
  }

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

  fprintf(stderr, "s_counter - thread_count * iterations = %d\n",
          s_counter - threads_created * s_num_iterations);
  fprintf(stderr, "Finished.\n");

  return 0;
}
