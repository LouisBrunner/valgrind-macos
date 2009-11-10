/// Test program that uses the QAtomicInt class.

#ifndef _GNU_SOURCE
#define _GNU_SOURCE
#endif

#include "config.h"
#include <QAtomicInt>     // class QAtomicInt
#include <cassert>
#include <cstdio>         // fprintf()
#include <cstdlib>        // atoi()
#include <new>
#include <pthread.h>      // pthread_barrier_t
#include <vector>


static pthread_barrier_t s_barrier;
static QAtomicInt* s_pAtomicInt;


void* thread_func(void* pArg)
{
  const int iArg = *reinterpret_cast<int*>(pArg);

  pthread_barrier_wait(&s_barrier);

  while (! s_pAtomicInt->testAndSetOrdered(iArg, iArg + 1))
    ;

  return NULL;
}

int main(int argc, char** argv)
{
  int i;
  const int n_threads = 10;
  std::vector<int>       thread_arg(n_threads);
  std::vector<pthread_t> tid(n_threads);

  fprintf(stderr, "Start of test.\n");

  pthread_barrier_init(&s_barrier, 0, n_threads);
  s_pAtomicInt = new QAtomicInt();
  for (i = 0; i < n_threads; i++)
  {
    thread_arg[i] = i;
    pthread_create(&tid[i], 0, thread_func, &thread_arg[i]);
  }
  for (i = 0; i < n_threads; i++)
  {
    pthread_join(tid[i], NULL);
  }
  pthread_barrier_destroy(&s_barrier);

  if (*s_pAtomicInt == n_threads)
    fprintf(stderr, "Test successful.\n");
  else
    fprintf(stderr, "Test failed: counter = %d, should be %d\n",
            static_cast<int>(*s_pAtomicInt), n_threads);

  delete s_pAtomicInt;
  s_pAtomicInt = 0;

  return 0;
}
