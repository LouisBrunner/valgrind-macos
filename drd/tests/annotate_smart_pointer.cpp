#include <cassert>
#include <iostream>
#include <pthread.h>
#include <semaphore.h>

template<class T>
class smart_ptr
{
public:
  typedef unsigned counter_t;

  template <typename Q> friend class smart_ptr;

  explicit smart_ptr()
    : m_ptr(NULL), m_count_ptr(NULL)
  { }

  explicit smart_ptr(T* const pT)
    : m_ptr(NULL), m_count_ptr(NULL)
  {
    set(pT, pT ? new counter_t(0) : NULL);
  }

  template <typename Q>
  explicit smart_ptr(Q* const q)
    : m_ptr(NULL), m_count_ptr(NULL)
  {
    set(q, q ? new counter_t(0) : NULL);
  }

  ~smart_ptr()
  {
    set(NULL, NULL);
  }

  smart_ptr(const smart_ptr<T>& sp)
    : m_ptr(NULL), m_count_ptr(NULL)
  {
    set(sp.m_ptr, sp.m_count_ptr);
  }

  template <typename Q>
  smart_ptr(const smart_ptr<Q>& sp)
    : m_ptr(NULL), m_count_ptr(NULL)
  {
    set(sp.m_ptr, sp.m_count_ptr);
  }

  smart_ptr& operator=(const smart_ptr<T>& sp)
  {
    set(sp.m_ptr, sp.m_count_ptr);
    return *this;
  }

  smart_ptr& operator=(T* const p)
  {
    set(p, p ? new counter_t(0) : NULL);
    return *this;
  }

  template <typename Q>
  smart_ptr& operator=(Q* const q)
  {
    set(q, q ? new counter_t(0) : NULL);
    return *this;
  }

  T* operator->() const
  {
    assert(m_ptr);
    return m_ptr;
  }

  T& operator*() const
  {
    assert(m_ptr);
    return *m_ptr;
  }

private:
  void set(T* const pT, volatile counter_t* const count_ptr)
  {
    if (m_ptr != pT)
    {
      if (m_count_ptr && __sync_sub_and_fetch(m_count_ptr, 1) == 0)
      {
	delete m_ptr;
	delete m_count_ptr;
      }
      m_ptr = pT;
      m_count_ptr = count_ptr;
      if (count_ptr)
	__sync_add_and_fetch(count_ptr, 1);
    }
  }

  T*                  m_ptr;
  volatile counter_t* m_count_ptr;
};

class counter
{
public:
  counter()
    : m_mutex(), m_count()
  {
    pthread_mutex_init(&m_mutex, NULL);
  }
  ~counter()
  {
    m_count = -1;
    pthread_mutex_destroy(&m_mutex);
  }
  int get() const
  {
    int result;
    pthread_mutex_lock(&m_mutex);
    result = m_count;
    pthread_mutex_unlock(&m_mutex);
    return result;
  }
  int post_increment()
  {
    int result;
    pthread_mutex_lock(&m_mutex);
    result = m_count++;
    pthread_mutex_unlock(&m_mutex);
    return result;
  }

private:
  mutable pthread_mutex_t m_mutex;
  int                     m_count;
};

static sem_t s_sem;

static void* thread_func(void* arg)
{
  smart_ptr<counter> p(*reinterpret_cast<smart_ptr<counter>*>(arg));
  sem_post(&s_sem);
  p->post_increment();
  p = NULL;
  return NULL;
}

int main(int argc, char** argv)
{
  smart_ptr<counter> p(new counter);
  pthread_t tid;

  sem_init(&s_sem, 0, 0);
  p->post_increment();
  pthread_create(&tid, NULL, thread_func, &p);
  // Wait until the created thread has copied the shared pointer.
  sem_wait(&s_sem);
  p = NULL;
  pthread_join(tid, NULL);
  sem_destroy(&s_sem);
  std::cout << "Done.\n";
  return 0;
}
