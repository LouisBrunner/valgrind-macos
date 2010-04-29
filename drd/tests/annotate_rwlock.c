/**
 * @file  annotate_rwlock.c
 *
 * @brief Multithreaded test program that triggers various access patterns
 *        without triggering any race conditions using a reader-writer lock
 *        implemented via busy-waiting. Annotations are used to tell DRD
 *        which higher-level rwlock operations are being performed.
 */


#define _GNU_SOURCE 1

#include <assert.h>
#include <pthread.h>
#include <stdio.h>
#include <unistd.h>        /* usleep() */
#include "../../config.h"
#include "../../drd/drd.h"


#ifndef HAVE_BUILTIN_ATOMIC
#error Sorry, but this test program can only be compiled by a compiler that\
has built-in functions for atomic memory access.
#endif


typedef struct {
  volatile int locked;
  int          writer_count;
  int          reader_count;
} rwlock_t;


static rwlock_t s_rwlock;
static int s_counter;


static void rwlock_init(rwlock_t* p)
{
  DRD_IGNORE_VAR(*p);
  p->locked       = 0;
  p->writer_count = 0;
  p->reader_count = 0;
  ANNOTATE_RWLOCK_CREATE(p);
}

static void rwlock_destroy(rwlock_t* p)
{
  ANNOTATE_RWLOCK_DESTROY(p);
  assert(p->locked       == 0);
  assert(p->writer_count == 0);
  assert(p->reader_count == 0);
}

static void rwlock_rdlock(rwlock_t* p)
{
  while (1)
  {
    while (__sync_val_compare_and_swap(&p->locked, 0, 1) == 1)
      ;
    if (p->writer_count == 0)
      break;
#ifndef HAVE_PTHREAD_YIELD
    /* Darwin doesn't have an implementation of pthread_yield(). */
    usleep(100 * 1000);
#else
    pthread_yield();
#endif
    (void) __sync_fetch_and_sub(&p->locked, 1);
  }
  p->reader_count++;
  assert(p->reader_count >= 0);
  assert(p->writer_count >= 0);
  assert(p->reader_count == 0 || p->writer_count == 0);
  (void) __sync_fetch_and_sub(&p->locked, 1);
  ANNOTATE_READERLOCK_ACQUIRED(p);
}

static void rwlock_wrlock(rwlock_t* p)
{
  while (1)
  {
    while (__sync_val_compare_and_swap(&p->locked, 0, 1) == 1)
      ;
    if (p->reader_count == 0)
      break;
#ifndef HAVE_PTHREAD_YIELD
    /* Darwin doesn't have an implementation of pthread_yield(). */
    usleep(100 * 1000);
#else
    pthread_yield();
#endif
    (void) __sync_fetch_and_sub(&p->locked, 1);
  }
  p->writer_count++;
  assert(p->reader_count >= 0);
  assert(p->writer_count >= 0);
  assert(p->reader_count == 0 || p->writer_count == 0);
  (void) __sync_fetch_and_sub(&p->locked, 1);
  ANNOTATE_WRITERLOCK_ACQUIRED(p);
}

static void rwlock_unlock(rwlock_t* p)
{
  while (__sync_val_compare_and_swap(&p->locked, 0, 1) == 1)
    ;
  if (p->reader_count > 0)
  {
    p->reader_count--;
    ANNOTATE_READERLOCK_RELEASED(p);
  }
  else
  {
    p->writer_count--;
    ANNOTATE_WRITERLOCK_RELEASED(p);
  }
  assert(p->reader_count >= 0);
  assert(p->writer_count >= 0);
  assert(p->reader_count == 0 || p->writer_count == 0);
  (void) __sync_fetch_and_sub(&p->locked, 1);
}

static void* thread_func(void* arg)
{
  int i;
  int sum = 0;

  for (i = 0; i < 1000; i++)
  {
    rwlock_rdlock(&s_rwlock);
    sum += s_counter;
    rwlock_unlock(&s_rwlock);
    rwlock_wrlock(&s_rwlock);
    s_counter++;
    rwlock_unlock(&s_rwlock);
  }

  return 0;
}

int main(int argc, char** argv)
{
  const int thread_count = 10;
  pthread_t tid[thread_count];
  int i;

  rwlock_init(&s_rwlock);
  for (i = 0; i < thread_count; i++)
  {
    pthread_create(&tid[i], 0, thread_func, 0);
  }

  for (i = 0; i < thread_count; i++)
  {
    pthread_join(tid[i], 0);
  }
  rwlock_destroy(&s_rwlock);

  fprintf(stderr, "Finished.\n");

  return 0;
}
