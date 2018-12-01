/** Broadcast a (POSIX threads) signal to all running threads, where the
 *  number of threads can be specified on the command line. This test program
 *  is intended not only to test the correctness of drd but also to test
 *  whether performance does not degrade too much when the number of threads
 *  increases.
 */


#include <assert.h>
#include <pthread.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>


// Counting semaphore.

struct csema
{
  pthread_mutex_t  m_mutex;
  pthread_cond_t   m_cond;
  int              m_count;
};

void csema_ctr(struct csema* p)
{
  memset(p, 0, sizeof(*p));
  pthread_mutex_init(&p->m_mutex, 0);
  pthread_cond_init(&p->m_cond, 0);
}

void csema_dtr(struct csema* p)
{
  pthread_cond_destroy(&p->m_cond);
  pthread_mutex_destroy(&p->m_mutex);
}

void csema_p(struct csema* p, const int n)
{
  pthread_mutex_lock(&p->m_mutex);
  while (p->m_count < n)
    pthread_cond_wait(&p->m_cond, &p->m_mutex);
  p->m_count -= n;
  pthread_cond_signal(&p->m_cond);
  pthread_mutex_unlock(&p->m_mutex);
}

void csema_v(struct csema* p)
{
  pthread_mutex_lock(&p->m_mutex);
  p->m_count++;
  pthread_cond_signal(&p->m_cond);
  pthread_mutex_unlock(&p->m_mutex);
}


struct cthread
{
  pthread_t     m_thread;
  int           m_threadnum;
  struct csema* m_sema;
};

void cthread_ctr(struct cthread* p)
{
  p->m_thread = 0;
  p->m_sema   = 0;
}

void cthread_dtr(struct cthread* p)
{ }


// Local variables.

static int s_debug = 0;
static int s_trace = 0;
static int s_signal_count;
static pthread_mutex_t s_mutex;
static pthread_cond_t  s_cond;


// Function definitions.

static void *thread_func(void *arg)
{
  struct cthread* thread_info = arg;
  int i;

  pthread_mutex_lock(&s_mutex);

  for (i = 0; i < s_signal_count; i++)
  {
    if (s_trace)
    {
      printf("thread %d [%d] (1)\n", thread_info->m_threadnum, i);
    }
    csema_v(thread_info->m_sema);

    // Wait until the main thread signals us via pthread_cond_broadcast().
    pthread_cond_wait(&s_cond, &s_mutex);
    if (s_trace)
    {
      printf("thread %d [%d] (2)\n", thread_info->m_threadnum, i);
    }
  }

  pthread_mutex_unlock(&s_mutex);

  return NULL;
}

int main(int argc, char** argv)
{
  int optchar;
  int thread_count;

  while ((optchar = getopt(argc, argv, "d")) != EOF)
  {
    switch (optchar)
    {
    case 'd':
      s_debug = 1;
      break;
    default:
      assert(0);
      break;
    }
  }

  /* This test should complete in 15s or less. If the test does not complete */
  /* within that time, abort the test via the signal SIGALRM.                */
  alarm(100);

  s_signal_count = argc > optind ? atoi(argv[optind]) : 10;
  thread_count = argc > optind + 1 ? atoi(argv[optind + 1]) : 10;

  if (s_debug)
    printf("&s_cond = %p\n", &s_cond);

  pthread_mutex_init(&s_mutex, 0);
  pthread_cond_init(&s_cond, 0);
  {
    int i;
    struct csema sema;
    struct cthread* p;
    struct cthread* thread_vec;

    csema_ctr(&sema);
    thread_vec = malloc(sizeof(struct cthread) * thread_count);
    for (p = thread_vec; p != thread_vec + thread_count; p++)
    {
      cthread_ctr(p);
      p->m_threadnum = p - thread_vec;
      p->m_sema = &sema;
      pthread_create(&p->m_thread, 0, thread_func, &*p);
    }
    for (i = 0; i < s_signal_count; i++)
    {
      if (s_trace)
        printf("main [%d] (1)\n", i);
      csema_p(&sema, thread_count);
      if (s_trace)
        printf("main [%d] (2)\n", i);
      pthread_mutex_lock(&s_mutex);
      pthread_cond_broadcast(&s_cond);
      pthread_mutex_unlock(&s_mutex);
      if (s_trace)
        printf("main [%d] (3)\n", i);
    }
    for (i = 0; i < thread_count; i++)
    {
      pthread_join(thread_vec[i].m_thread, 0);
      cthread_dtr(&thread_vec[i]);
    }
    free(thread_vec);
    csema_dtr(&sema);
  }
  pthread_cond_destroy(&s_cond);
  pthread_mutex_destroy(&s_mutex);

  fprintf(stderr, "Done.\n");

  return 0;
}
