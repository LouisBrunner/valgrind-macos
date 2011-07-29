/* -*- mode: C; c-basic-offset: 2; indent-tabs-mode: nil; -*- */
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

static pthread_barrier_t s_barrier;

static void* thread(void* arg)
{
  write(STDOUT_FILENO, ".", 1);
  pthread_barrier_wait(&s_barrier);
  return NULL;
}

int main(int argc, char** argv)
{
  pthread_t *tid;
  int barriers = argc > 2 ? atoi(argv[1]) : 20;
  int barrier_participants = 2;
  int thread_count = barriers * barrier_participants;
  int i;

  pthread_barrier_init(&s_barrier, NULL, barrier_participants);

  tid = malloc(thread_count * sizeof(*tid));
  assert(tid);
  for (i = 0; i < thread_count; i++)
	  pthread_create(&tid[i], NULL, thread, NULL);
  for (i = 0; i < thread_count; i++)
	  pthread_join(tid[i], NULL);
  free(tid);

  pthread_barrier_destroy(&s_barrier);

  write(STDOUT_FILENO, "\n", 1);
  fprintf(stderr, "Done.\n");

  return 0;
}
