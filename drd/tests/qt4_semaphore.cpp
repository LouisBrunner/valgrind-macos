/// Qt4 semaphore test.

#ifndef _GNU_SOURCE
#define _GNU_SOURCE
#endif

#include <QThread>           // class QMutex
#include <QSemaphore>        // class QSemaphore
#include <cstdio>            // fprintf()
#include <cstdlib>           // atoi()
#include <new>
#include <pthread.h>         // pthread_barrier_t
#include <vector>


static pthread_barrier_t s_barrier;
static QSemaphore* s_pSema;
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
    s_pSema->acquire();
    s_counter++;
    s_pSema->release();
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
    // Stack-allocated semaphore.
    QSemaphore S(1);
    S.acquire();
    S.release();
    S.acquire();
    S.release();
  }

  pthread_barrier_init(&s_barrier, 0, n_threads);
  s_pSema = new QSemaphore(1);
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
  delete s_pSema;
  s_pSema = 0;
  pthread_barrier_destroy(&s_barrier);

  if (s_counter == n_threads * s_iterations)
    fprintf(stderr, "Test successful.\n");
  else
    fprintf(stderr, "Test failed: counter = %d, should be %d\n",
            s_counter, n_threads * s_iterations);

  return 0;
}
