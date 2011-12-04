/*
 * Test program that triggers pthread_barrier_wait() where each
 * pthread_barrier_wait() call is invoked by another thread. This is the only
 * test program that triggers the code guarded by if (q->thread_finished) in
 * DRD_(barrier_pre_wait)().
 */

#define _GNU_SOURCE

#include <assert.h>
#include <pthread.h>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>

static pthread_barrier_t* s_barrier;

static void* thread(void* arg)
{
  write(STDOUT_FILENO, ".", 1);
  pthread_barrier_wait(s_barrier);
  return NULL;
}

int main(int argc, char** argv)
{
  pthread_t *tid;
  int barriers = argc > 1 ? atoi(argv[1]) : 20;
  int barrier_participants = 2;
  int thread_count = barriers * barrier_participants;
  int res, i;

  s_barrier = malloc(sizeof(*s_barrier));
  res = pthread_barrier_init(s_barrier, NULL, barrier_participants);
  assert(res == 0);

  tid = malloc(thread_count * sizeof(*tid));
  assert(tid);
  for (i = 0; i < thread_count; i++) {
    res = pthread_create(&tid[i], NULL, thread, NULL);
    assert(res == 0);
  }
  for (i = 0; i < thread_count; i++) {
    res = pthread_join(tid[i], NULL);
    assert(res == 0);
  }
  free(tid);

  res = pthread_barrier_destroy(s_barrier);
  assert(res == 0);
  free(s_barrier);
  s_barrier = NULL;

  write(STDOUT_FILENO, "\n", 1);
  fprintf(stderr, "Done.\n");

  return 0;
}
