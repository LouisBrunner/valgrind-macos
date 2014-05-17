/**
 * This test program triggers a single race condition on variable s_y.
 * Although another variable (s_x) is also modified by both threads, no race
 * condition must be reported on this variable since it is only accessed via
 * atomic instructions.
 *
 * Note: for the i386 and x86_64 memory models, thread 2 must print y = 1.
 * On PPC however, both y = 0 and y = 1 are legal results. This is because
 * the PPC memory model allows different CPU's to observe stores to variables
 * in different cache lines in a different order.
 */

#define _GNU_SOURCE

#include <pthread.h>
#include <stdio.h>   /* fprintf() */
#include <stdlib.h>  /* atoi() */
#include "../../config.h"

/* Atomic builtins are only supported by gcc 4.1.0 and later. */
#ifndef HAVE_BUILTIN_ATOMIC
#error Sorry, but this test program can only be compiled by a compiler that\
has built-in functions for atomic memory access.
#endif

static __inline__
int sync_add_and_fetch(int* p, int i)
{
  return __sync_add_and_fetch(p, i);
}

static int s_x = 0;
/* g_dummy[] ensures that s_x and s_y are not in the same cache line. */
char g_dummy[512];
static int s_y = 0;

static void* thread_func_1(void* arg)
{
  s_y = 1;
  (void) sync_add_and_fetch(&s_x, 1);
  return 0;
}

static void* thread_func_2(void* arg)
{
  while (sync_add_and_fetch(&s_x, 0) == 0)
    ;
  fprintf(stderr, "y = %d\n", s_y);
  return 0;
}

int main(int argc, char** argv)
{
  int i;
  const int n_threads = 2;
  pthread_t tid[n_threads];

  fprintf(stderr, "Start of test.\n");
  pthread_create(&tid[0], 0, thread_func_1, 0);
  pthread_create(&tid[1], 0, thread_func_2, 0);
  for (i = 0; i < n_threads; i++)
    pthread_join(tid[i], 0);
  fprintf(stderr, "Test finished.\n");

  return 0;
}
