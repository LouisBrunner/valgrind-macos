/// Qt4 mutex test.

#ifndef _GNU_SOURCE
#define _GNU_SOURCE
#endif

#include "config.h"
#include <QMutex>         // class QMutex
#include <QThread>        // class QThread
#include <cassert>
#include <cstdio>         // fprintf()
#include <cstdlib>        // atoi()
#include <new>
#include <pthread.h>      // pthread_barrier_t
#include <vector>


static pthread_barrier_t s_barrier;
static QMutex* s_pMutex;
static int s_iterations;
static int s_counter;


class IncThread: public QThread
{
  virtual void run();
};

void IncThread::run()
{
  int i;

  pthread_barrier_wait(&s_barrier);
  for (i = s_iterations; i > 0; i--)
  {
    s_pMutex->lock();
    s_counter++;
    s_pMutex->unlock();
  }
}

int main(int argc, char** argv)
{
  int i;
  const int n_threads = 10;
  std::vector<QThread*> tid(n_threads);

  s_iterations = argc > 1 ? atoi(argv[1]) : 1000;

  fprintf(stderr, "Start of test.\n");

  {
    // Stack-allocated mutex.
    QMutex M(QMutex::Recursive);
    M.lock();
    assert(M.tryLock());
    M.unlock();
    M.unlock();
  }
#if defined(HAVE_QTCORE_QMUTEX_TRYLOCK_INT)
  {
    QMutex M(QMutex::NonRecursive);
    assert(M.tryLock(1));
    assert(! M.tryLock(1));
    M.unlock();
  }
#endif

  pthread_barrier_init(&s_barrier, 0, n_threads);
  s_pMutex = new QMutex();
  for (i = 0; i < n_threads; i++)
  {
    tid[i] = new IncThread;
    tid[i]->start();
  }
  for (i = 0; i < n_threads; i++)
  {
    tid[i]->wait();
    delete tid[i];
  }
  delete s_pMutex;
  s_pMutex = 0;
  pthread_barrier_destroy(&s_barrier);

  if (s_counter == n_threads * s_iterations)
    fprintf(stderr, "Test successful.\n");
  else
    fprintf(stderr, "Test failed: counter = %d, should be %d\n",
            s_counter, n_threads * s_iterations);

  return 0;
}
