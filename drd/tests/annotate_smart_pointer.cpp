#include <cassert>
#include <iostream>
#ifdef _WIN32
#include <process.h>
#include <windows.h>
#else
#include <pthread.h>
#include <semaphore.h>
#endif

#ifdef _WIN32
class AtomicInt32
{
public:
  AtomicInt32(const int value = 0) : m_value(value) { }
  ~AtomicInt32() { }
  LONG operator++() { return InterlockedIncrement(&m_value); }
  LONG operator--() { return InterlockedDecrement(&m_value); }

private:
  volatile LONG m_value;
};

class Mutex
{
public:
  Mutex() : m_mutex()
  { InitializeCriticalSection(&m_mutex); }
  ~Mutex()
  { DeleteCriticalSection(&m_mutex); }
  void Lock()
  { EnterCriticalSection(&m_mutex); }
  void Unlock()
  { LeaveCriticalSection(&m_mutex); }

private:
  CRITICAL_SECTION m_mutex;
};

class Semaphore
{
public:
  Semaphore()
    : m_sem(CreateSemaphore(NULL, 0, 1, NULL))
  { assert(m_sem != INVALID_HANDLE_VALUE); }
  ~Semaphore()
  { CloseHandle(m_sem); }
  void Post() const
  { ReleaseSemaphore(m_sem, 1, NULL); }
  void Wait() const
  { WaitForSingleObject(m_sem, INFINITE); }
private:
  const HANDLE m_sem;
};

class Thread
{
public:
  Thread() : m_thread(INVALID_HANDLE_VALUE) { }
  ~Thread() { }
  void Create(void* (*pf)(void*), void* arg)
  {
    wrapper_args* wrapper_arg_p = new wrapper_args(pf, arg);
    m_thread = reinterpret_cast<HANDLE>(_beginthreadex(NULL, 0, wrapper, wrapper_arg_p, 0, NULL));
  }
  void Join()
  { WaitForSingleObject(m_thread, INFINITE); }

private:
  struct wrapper_args
  {
    wrapper_args(void* (*pf)(void*), void* arg) : m_pf(pf), m_arg(arg) { }

    void* (*m_pf)(void*);
    void* m_arg;
  };
  static unsigned int __stdcall wrapper(void* arg)
  {
    wrapper_args* wrapper_arg_p = reinterpret_cast<wrapper_args*>(arg);
    wrapper_args wa = *wrapper_arg_p;
    delete wrapper_arg_p;
    return reinterpret_cast<unsigned>((wa.m_pf)(wa.m_arg));
  }
  HANDLE m_thread;
};
#else // _WIN32
class AtomicInt32
{
public:
  AtomicInt32(const int value = 0) : m_value(value) { }
  ~AtomicInt32() { }
  int operator++() { return __sync_add_and_fetch(&m_value, 1); }
  int operator--() { return __sync_sub_and_fetch(&m_value, 1); }
private:
  volatile int m_value;
};

class Mutex
{
public:
  Mutex() : m_mutex()
  { pthread_mutex_init(&m_mutex, NULL); }
  ~Mutex()
  { pthread_mutex_destroy(&m_mutex); }
  void Lock()
  { pthread_mutex_lock(&m_mutex); }
  void Unlock()
  { pthread_mutex_unlock(&m_mutex); }

private:
  pthread_mutex_t m_mutex;
};

class Semaphore
{
public:
  Semaphore() : m_sem()
  { sem_init(&m_sem, 0, 0); }
  ~Semaphore()
  { sem_destroy(&m_sem); }
  void Post()
  { sem_post(&m_sem); }
  void Wait()
  { sem_wait(&m_sem); }
private:
  sem_t m_sem;
};

class Thread
{
public:
  Thread() : m_tid() { }
  ~Thread() { }
  void Create(void* (*pf)(void*), void* arg)
  { pthread_create(&m_tid, NULL, pf, arg); }
  void Join()
  { pthread_join(m_tid, NULL); }
private:
  pthread_t m_tid;
};
#endif // _WIN32


template<class T>
class smart_ptr
{
public:
  typedef AtomicInt32 counter_t;

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
  void set(T* const pT, counter_t* const count_ptr)
  {
    if (m_ptr != pT)
    {
      if (m_count_ptr && --(*m_count_ptr) == 0)
      {
	delete m_ptr;
	delete m_count_ptr;
      }
      m_ptr = pT;
      m_count_ptr = count_ptr;
      if (count_ptr)
	++(*m_count_ptr);
    }
  }

  T*         m_ptr;
  counter_t* m_count_ptr;
};

class counter
{
public:
  counter()
    : m_mutex(), m_count()
  {
  }
  ~counter()
  { m_count = -1; }
  int get() const
  {
    int result;
    m_mutex.Lock();
    result = m_count;
    m_mutex.Unlock();
    return result;
  }
  int post_increment()
  {
    int result;
    m_mutex.Lock();
    result = m_count++;
    m_mutex.Unlock();
    return result;
  }

private:
  mutable Mutex m_mutex;
  int           m_count;
};

static Semaphore* s_sem;

static void* thread_func(void* arg)
{
  smart_ptr<counter> p(*reinterpret_cast<smart_ptr<counter>*>(arg));
  s_sem->Post();
  p->post_increment();
  p = NULL;
  return NULL;
}

int main(int argc, char** argv)
{
  smart_ptr<counter> p(new counter);
  Thread T;

  s_sem = new Semaphore();
  p->post_increment();
  T.Create(thread_func, &p);
  // Wait until the created thread has copied the shared pointer.
  s_sem->Wait();
  p = NULL;
  T.Join();
  delete s_sem;
  std::cout << "Done.\n";
  return 0;
}
