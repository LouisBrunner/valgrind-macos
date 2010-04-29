/* Test program that performs producer-consumer style communication through
 * a circular buffer. This test program is a slightly modified version of the
 * test program made available by Miguel Ojeda
 * -- see also http://article.gmane.org/gmane.comp.debugging.valgrind/8782.
 */


#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <unistd.h>
#include <time.h>
#include <pthread.h>
#include <semaphore.h>
#include <fcntl.h>
#include "../../config.h"


/** gcc versions 4.1.0 and later have support for atomic builtins. */

#ifndef HAVE_BUILTIN_ATOMIC
#error Sorry, but this test program can only be compiled by a compiler that\
has built-in functions for atomic memory access.
#endif


#define BUFFER_MAX (2)
#define DATA_SEMAPHORE_NAME "cb-data-semaphore"
#define FREE_SEMAPHORE_NAME "cb-free-semaphore"


typedef int data_t;

typedef struct {
  /* Counting semaphore representing the number of data items in the buffer. */
  sem_t* data;
  /* Counting semaphore representing the number of free elements. */
  sem_t* free;
  /* Position where a new elements should be written. */
  int in;
  /* Position from where an element can be removed. */
  int out;
  /* Mutex that protects 'in'. */
  pthread_mutex_t mutex_in;
  /* Mutex that protects 'out'. */
  pthread_mutex_t mutex_out;
  /* Data buffer. */
  data_t buffer[BUFFER_MAX];
} buffer_t;

static int quiet = 0;
static int use_locking = 1;

static __inline__
int fetch_and_add(int* p, int i)
{
  return __sync_fetch_and_add(p, i);
}

static sem_t* create_semaphore(const char* const name, const int value)
{
#ifdef __APPLE__
  sem_t* p = sem_open(name, O_CREAT, 0600, value);
  return p;
#else
  sem_t* p = malloc(sizeof(*p));
  if (p)
    sem_init(p, 0, value);
  return p;
#endif
}

static void destroy_semaphore(const char* const name, sem_t* p)
{
#ifdef __APPLE__
  sem_close(p);
  sem_unlink(name);
#else
  sem_destroy(p);
  free(p);
#endif
}

static void buffer_init(buffer_t * b)
{
  b->data = create_semaphore(DATA_SEMAPHORE_NAME, 0);
  b->free = create_semaphore(FREE_SEMAPHORE_NAME, BUFFER_MAX);

  pthread_mutex_init(&b->mutex_in, NULL);
  pthread_mutex_init(&b->mutex_out, NULL);

  b->in = 0;
  b->out = 0;
}

static void buffer_recv(buffer_t* b, data_t* d)
{
  int out;
  sem_wait(b->data);
  if (use_locking)
    pthread_mutex_lock(&b->mutex_out);
  out = fetch_and_add(&b->out, 1);
  if (out >= BUFFER_MAX)
  {
    fetch_and_add(&b->out, -BUFFER_MAX);
    out -= BUFFER_MAX;
  }
  *d = b->buffer[out];
  if (use_locking)
    pthread_mutex_unlock(&b->mutex_out);
  if (! quiet)
  {
    printf("received %d from buffer[%d]\n", *d, out);
    fflush(stdout);
  }
  sem_post(b->free);
}

static void buffer_send(buffer_t* b, data_t* d)
{
  int in;
  sem_wait(b->free);
  if (use_locking)
    pthread_mutex_lock(&b->mutex_in);
  in = fetch_and_add(&b->in, 1);
  if (in >= BUFFER_MAX)
  {
    fetch_and_add(&b->in, -BUFFER_MAX);
    in -= BUFFER_MAX;
  }
  b->buffer[in] = *d;
  if (use_locking)
    pthread_mutex_unlock(&b->mutex_in);
  if (! quiet)
  {
    printf("sent %d to buffer[%d]\n", *d, in);
    fflush(stdout);
  }
  sem_post(b->data);
}

static void buffer_destroy(buffer_t* b)
{
  destroy_semaphore(DATA_SEMAPHORE_NAME, b->data);
  destroy_semaphore(FREE_SEMAPHORE_NAME, b->free);

  pthread_mutex_destroy(&b->mutex_in);
  pthread_mutex_destroy(&b->mutex_out);
}

static buffer_t b;

static void producer(int* id)
{
  buffer_send(&b, id);
  pthread_exit(NULL);
}

#define MAXSLEEP (100 * 1000)

static void consumer(int* id)
{
  int d;
  usleep(rand() % MAXSLEEP);
  buffer_recv(&b, &d);
  if (! quiet)
  {
    printf("%i: %i\n", *id, d);
    fflush(stdout);
  }
  pthread_exit(NULL);
}

#define THREADS (10)

int main(int argc, char** argv)
{
  pthread_t producers[THREADS];
  pthread_t consumers[THREADS];
  int thread_arg[THREADS];
  int i;
  int optchar;

  while ((optchar = getopt(argc, argv, "nq")) != EOF)
  {
    switch (optchar)
    {
    case 'n':
      use_locking = 0;
      break;
    case 'q':
      quiet = 1;
      break;
    }
  }

  srand(time(NULL));

  buffer_init(&b);

  for (i = 0; i < THREADS; ++i)
  {
    thread_arg[i] = i;
    pthread_create(producers + i, NULL,
                   (void * (*)(void *)) producer, &thread_arg[i]);
  }

  for (i = 0; i < THREADS; ++i)
    pthread_create(consumers + i, NULL,
                   (void * (*)(void *)) consumer, &thread_arg[i]);

  for (i = 0; i < THREADS; ++i)
  {
    pthread_join(producers[i], NULL);
    pthread_join(consumers[i], NULL);
  }

  buffer_destroy(&b);

  return 0;
}
