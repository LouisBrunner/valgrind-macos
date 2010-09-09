/*
 * Test program that illustrates how to annotate a smart pointer
 * implementation.  In a multithreaded program the following is relevant when
 * working with smart pointers:
 * - whether or not the objects pointed at are shared over threads.
 * - whether or not the methods of the objects pointed at are thread-safe.
 * - whether or not the smart pointer objects are shared over threads.
 * - whether or not the smart pointer object itself is thread-safe.
 *
 * Most smart pointer implemenations are not thread-safe
 * (e.g. boost::shared_ptr<>, tr1::shared_ptr<> and the smart_ptr<>
 * implementation below). This means that it is not safe to modify a shared
 * pointer object that is shared over threads without proper synchronization.
 *
 * Even for non-thread-safe smart pointers it is possible to have different
 * threads access the same object via smart pointers without triggering data
 * races on the smart pointer objects.
 *
 * A smart pointer implementation guarantees that the destructor of the object
 * pointed at is invoked after the last smart pointer that points to that
 * object has been destroyed or reset. Data race detection tools cannot detect
 * this ordering without explicit annotation for smart pointers that track
 * references without invoking synchronization operations recognized by data
 * race detection tools.
 */


#include <cassert>     // assert()
#include <climits>     // PTHREAD_STACK_MIN
#include <iostream>    // std::cerr
#include <stdlib.h>    // atoi()
#ifdef _WIN32
#include <process.h>   // _beginthreadex()
#include <windows.h>   // CRITICAL_SECTION
#else
#include <pthread.h>   // pthread_mutex_t
#endif
#include "unified_annotations.h"


static bool s_enable_annotations = true;


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

class Thread
{
public:
  Thread() : m_thread(INVALID_HANDLE_VALUE) { }
  ~Thread() { }
  void Create(void* (*pf)(void*), void* arg)
  {
    WrapperArgs* wrapper_arg_p = new WrapperArgs(pf, arg);
    m_thread = reinterpret_cast<HANDLE>(_beginthreadex(NULL, 0, wrapper,
						       wrapper_arg_p, 0, NULL));
  }
  void Join()
  { WaitForSingleObject(m_thread, INFINITE); }

private:
  struct WrapperArgs
  {
    WrapperArgs(void* (*pf)(void*), void* arg) : m_pf(pf), m_arg(arg) { }

    void* (*m_pf)(void*);
    void* m_arg;
  };
  static unsigned int __stdcall wrapper(void* arg)
  {
    WrapperArgs* wrapper_arg_p = reinterpret_cast<WrapperArgs*>(arg);
    WrapperArgs wa = *wrapper_arg_p;
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

class Thread
{
public:
  Thread() : m_tid() { }
  ~Thread() { }
  void Create(void* (*pf)(void*), void* arg)
  {
    pthread_attr_t attr;
    pthread_attr_init(&attr);
    pthread_attr_setstacksize(&attr, PTHREAD_STACK_MIN + 4096);
    pthread_create(&m_tid, &attr, pf, arg);
    pthread_attr_destroy(&attr);
  }
  void Join()
  { pthread_join(m_tid, NULL); }
private:
  pthread_t m_tid;
};

#endif // !defined(_WIN32)


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
      if (m_count_ptr)
      {
	if (s_enable_annotations)
	  U_ANNOTATE_HAPPENS_BEFORE(m_count_ptr);
	if (--(*m_count_ptr) == 0)
	{
	  if (s_enable_annotations)
	    U_ANNOTATE_HAPPENS_AFTER(m_count_ptr);
	  delete m_ptr;
	  m_ptr = NULL;
	  delete m_count_ptr;
	  m_count_ptr = NULL;
	}
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
  { }
  ~counter()
  {
    // Data race detection tools that do not recognize the
    // ANNOTATE_HAPPENS_BEFORE() / ANNOTATE_HAPPENS_AFTER() annotations in the
    // smart_ptr<> implementation will report that the assignment below
    // triggers a data race.
    m_count = -1;
  }
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

static void* thread_func(void* arg)
{
  smart_ptr<counter>* pp = reinterpret_cast<smart_ptr<counter>*>(arg);
  (*pp)->post_increment();
  *pp = NULL;
  delete pp;
  return NULL;
}

int main(int argc, char** argv)
{
  const int nthreads = std::max(argc > 1 ? atoi(argv[1]) : 1, 1);
  const int iterations = std::max(argc > 2 ? atoi(argv[2]) : 1, 1);
  s_enable_annotations = argc > 3 ? !!atoi(argv[3]) : true;

  for (int j = 0; j < iterations; ++j)
  {
    Thread T[nthreads];

    smart_ptr<counter> p(new counter);
    p->post_increment();
    for (int i = 0; i < nthreads; ++i)
      T[i].Create(thread_func, new smart_ptr<counter>(p));
    p = NULL;
    for (int i = 0; i < nthreads; ++i)
      T[i].Join();
  }
  std::cerr << "Done.\n";
  return 0;
}

// Local variables:
// c-basic-offset: 2
// End:
