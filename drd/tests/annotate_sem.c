/**
 * @file  annotate_sem.c
 *
 * @brief Multithreaded test program that triggers various access patterns
 *        without triggering any race conditions using a binary semaphore
 *        implemented via busy-waiting. Annotations are used to tell DRD
 *        which higher-level semaphore operations are being performed.
 */

#include <assert.h>
#include <pthread.h>
#include <stdio.h>
#include "../../config.h"
#include "../../drd/drd.h"

#define THREADS 10
#define ITERATIONS 1000

typedef struct {
  volatile unsigned value;
} sem_t;

static sem_t s_sem;
static unsigned int s_counter;

static void sem_init(sem_t *p, unsigned value)
{
  DRD_IGNORE_VAR(*p);
  p->value = value;
  ANNOTATE_SEM_INIT_PRE(p, value);
}

static void sem_destroy(sem_t *p)
{
  ANNOTATE_SEM_DESTROY_POST(p);
}

static void sem_wait(sem_t *p)
{
  unsigned old, new;
  struct timespec ts = { 0, 0 };

  ANNOTATE_SEM_WAIT_PRE(p);
  do {
    old = p->value;
    new = old - 1;
    nanosleep(&ts, NULL);
    ts.tv_nsec = 1;
  } while (!old || !__sync_bool_compare_and_swap(&p->value, old, new));
  ANNOTATE_SEM_WAIT_POST(p);
}

static void sem_post(sem_t *p)
{
  ANNOTATE_SEM_POST_PRE(p);
  __sync_fetch_and_add(&p->value, 1);
}

static void *thread_func(void *arg)
{
  unsigned int i;
  unsigned int sum = 0;

  for (i = 0; i < ITERATIONS; i++) {
    sem_wait(&s_sem);
    sum += s_counter;
    sem_post(&s_sem);

    sem_wait(&s_sem);
    s_counter++;
    sem_post(&s_sem);
  }

  return 0;
}

int main(int argc, const char *argv[])
{
  pthread_t tid[THREADS];
  unsigned int i;

  sem_init(&s_sem, 1);
  for (i = 0; i < THREADS; i++)
    pthread_create(&tid[i], 0, thread_func, 0);

  for (i = 0; i < THREADS; i++)
    pthread_join(tid[i], 0);

  assert(s_counter == THREADS * ITERATIONS);
  assert(s_sem.value == 1);
  sem_destroy(&s_sem);

  fprintf(stderr, "Finished.\n");

  return 0;
}
