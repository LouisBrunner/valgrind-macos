// Broadcast a (POSIX threads) signal to all running threads, where the 
// number of threads can be specified on the command line. This test program
// is intended not only to test the correctness of drd but also to test
// whether performance does not degrade too much when the number of threads
// increases.


#include <cassert>
#include <iostream>
#include <vector>
#include <pthread.h>


// Class definitions.

// Counting semaphore.

class CSema
{
public:
  CSema()
    : m_mutex(), m_cond(), m_count(0)
  {
    pthread_mutex_init(&m_mutex, 0);
    pthread_cond_init(&m_cond, 0);
  }
  ~CSema()
  {
    pthread_cond_destroy(&m_cond);
    pthread_mutex_destroy(&m_mutex);
  }
  void p(const int n)
  {
    pthread_mutex_lock(&m_mutex);
    while (m_count < n)
      pthread_cond_wait(&m_cond, &m_mutex);
    m_count -= n;
    pthread_cond_signal(&m_cond);
    pthread_mutex_unlock(&m_mutex);
  }
  void v()
  {
    pthread_mutex_lock(&m_mutex);
    m_count++;
    pthread_cond_signal(&m_cond);
    pthread_mutex_unlock(&m_mutex);
  }

private:
  CSema(CSema const&);
  CSema& operator=(CSema const&);

  pthread_mutex_t  m_mutex;
  pthread_cond_t   m_cond;
  int              m_count;
};

struct CThread
{
  CThread()
    : m_thread(), m_sema()
  { }
  ~CThread()
  { }

  pthread_t m_thread;
  int       m_threadnum;
  CSema*    m_sema;
};


// Local variables.

static bool s_debug = false;
static bool s_trace = false;
static int s_signal_count;
static pthread_mutex_t s_mutex;
static pthread_cond_t  s_cond;


// Function definitions.

static void thread_func(CThread* thread_info)
{
  pthread_mutex_lock(&s_mutex);

  for (int i = 0; i < s_signal_count; i++)
  {
    if (s_trace)
    {
      std::cout << "thread " << thread_info->m_threadnum
                << " [" << i << "] (1)" << std::endl;
    }
    thread_info->m_sema->v();
    
    // Wait until the main thread signals us via pthread_cond_broadcast().
    pthread_cond_wait(&s_cond, &s_mutex);
    if (s_trace)
    {
      std::cout << "thread " << thread_info->m_threadnum
                << " [" << i << "] (2)" << std::endl;
    }
  }

  pthread_mutex_unlock(&s_mutex);
}

int main(int argc, char** argv)
{
  int optchar;
  while ((optchar = getopt(argc, argv, "d")) != EOF)
  {
    switch (optchar)
    {
    case 'd':
      s_debug = true;
      break;
    default:
      assert(false);
      break;
    }
  }
  s_signal_count = argc > optind ? atoi(argv[optind]) : 10;
  const int thread_count = argc > optind + 1 ? atoi(argv[optind + 1]) : 10;

  if (s_debug)
    std::cout << "&s_cond = " << &s_cond << std::endl;

  pthread_mutex_init(&s_mutex, 0);
  pthread_cond_init(&s_cond, 0);
  {
    CSema sema;
    std::vector<CThread> thread_vec(thread_count);
    for (std::vector<CThread>::iterator p = thread_vec.begin();
         p != thread_vec.end(); p++)
    {
      p->m_threadnum = std::distance(thread_vec.begin(), p);
      p->m_sema = &sema;
      pthread_create(&p->m_thread, 0,
                     (void*(*)(void*))thread_func, &*p);
    }
    for (int i = 0; i < s_signal_count; i++)
    {
      if (s_trace)
        std::cout << "main [" << i << "] (1)\n";
      sema.p(thread_count);
      if (s_trace)
        std::cout << "main [" << i << "] (2)\n";
      pthread_mutex_lock(&s_mutex);
      pthread_cond_broadcast(&s_cond);
      pthread_mutex_unlock(&s_mutex);
      if (s_trace)
        std::cout << "main [" << i << "] (3)\n";
    }
    for (int i = 0; i < thread_count; i++)
    {
      pthread_join(thread_vec[i].m_thread, 0);
    }
  }
  pthread_cond_destroy(&s_cond);
  pthread_mutex_destroy(&s_mutex);
  return 0;
}
