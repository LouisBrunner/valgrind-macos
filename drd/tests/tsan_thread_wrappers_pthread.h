/*
  This file is part of Valgrind, a dynamic binary instrumentation
  framework.

  Copyright (C) 2008-2008 Google Inc
     opensource@google.com

  This program is free software; you can redistribute it and/or
  modify it under the terms of the GNU General Public License as
  published by the Free Software Foundation; either version 2 of the
  License, or (at your option) any later version.

  This program is distributed in the hope that it will be useful, but
  WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
  General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with this program; if not, see <http://www.gnu.org/licenses/>.

  The GNU General Public License is contained in the file COPYING.
*/

// Author: Konstantin Serebryany <opensource@google.com>
//
// Here we define few simple classes that wrap pthread primitives.
//
// We need this to create unit tests for helgrind (or similar tool)
// that will work with different threading frameworks.
//
// If one needs to test helgrind's support for another threading library,
// he/she can create a copy of this file and replace pthread_ calls
// with appropriate calls to his/her library.
//
// Note, that some of the methods defined here are annotated with
// ANNOTATE_* macros defined in dynamic_annotations.h.
//
// DISCLAIMER: the classes defined in this header file
// are NOT intended for general use -- only for unit tests.
//

#ifndef THREAD_WRAPPERS_PTHREAD_H
#define THREAD_WRAPPERS_PTHREAD_H

#include <pthread.h>
#include <semaphore.h>
#include <unistd.h>
#include <queue>
#include <stdio.h>
#include <limits.h>   // INT_MAX

#ifdef VGO_darwin
#include <libkern/OSAtomic.h>
#define NO_BARRIER
#define NO_TLS
#endif

#if __cplusplus >= 201103L
#include <cstdint>
#endif
#include <string>
using namespace std;

#include <sys/time.h>
#include <time.h>

#include "../../drd/drd.h"
#define ANNOTATE_NO_OP(arg) do { } while(0)
#define ANNOTATE_EXPECT_RACE(addr, descr)                \
    ANNOTATE_BENIGN_RACE_SIZED(addr, 4, "expected race")
static inline bool RunningOnValgrind() { return RUNNING_ON_VALGRIND; }

#include <assert.h>
#ifdef NDEBUG
# error "Pleeease, do not define NDEBUG"
#endif
#define CHECK assert

/// Set this to true if malloc() uses mutex on your platform as this may
/// introduce a happens-before arc for a pure happens-before race detector.
const bool kMallocUsesMutex = false;

/// Current time in milliseconds.
static inline int64_t GetCurrentTimeMillis() {
  struct timeval now;
  gettimeofday(&now, NULL);
  return now.tv_sec * 1000 + now.tv_usec / 1000;
}

/// Copy tv to ts adding offset in milliseconds.
static inline void timeval2timespec(timeval *const tv,
                                     timespec *ts,
                                     int64_t offset_milli) {
  const int64_t ten_9 = 1000000000LL;
  const int64_t ten_6 = 1000000LL;
  const int64_t ten_3 = 1000LL;
  int64_t now_nsec = (int64_t)tv->tv_sec * ten_9;
  now_nsec += (int64_t)tv->tv_usec * ten_3;
  int64_t then_nsec = now_nsec + offset_milli * ten_6;
  ts->tv_sec  = then_nsec / ten_9;
  ts->tv_nsec = then_nsec % ten_9;
}


class CondVar;

#ifndef NO_SPINLOCK
/// helgrind does not (yet) support spin locks, so we annotate them.

#ifndef VGO_darwin
class SpinLock {
 public:
  SpinLock() {
    CHECK(0 == pthread_spin_init(&mu_, 0));
    ANNOTATE_RWLOCK_CREATE((void*)&mu_);
  }
  ~SpinLock() {
    ANNOTATE_RWLOCK_DESTROY((void*)&mu_);
    CHECK(0 == pthread_spin_destroy(&mu_));
  }
  void Lock() {
    CHECK(0 == pthread_spin_lock(&mu_));
    ANNOTATE_RWLOCK_ACQUIRED((void*)&mu_, 1);
  }
  void Unlock() {
    ANNOTATE_RWLOCK_RELEASED((void*)&mu_, 1);
    CHECK(0 == pthread_spin_unlock(&mu_));
  }
 private:
  pthread_spinlock_t mu_;
};

#else

class SpinLock {
 public:
  // Mac OS X version.
  SpinLock() : mu_(OS_SPINLOCK_INIT) {
    ANNOTATE_RWLOCK_CREATE((void*)&mu_);
  }
  ~SpinLock() {
    ANNOTATE_RWLOCK_DESTROY((void*)&mu_);
  }
  void Lock() {
    OSSpinLockLock(&mu_);
    ANNOTATE_RWLOCK_ACQUIRED((void*)&mu_, 1);
  }
  void Unlock() {
    ANNOTATE_RWLOCK_RELEASED((void*)&mu_, 1);
    OSSpinLockUnlock(&mu_);
  }
 private:
  OSSpinLock mu_;
};
#endif // VGO_darwin

#endif // NO_SPINLOCK

/// Just a boolean condition. Used by Mutex::LockWhen and similar.
template <typename T>
class Condition {
 public:
  typedef bool (*func_t)(void*);

  Condition(bool (*func)(T*), T* arg)
  : func1_(func), arg_(arg) {}

  Condition(bool (*func)())
  : func0_(func), arg_(NULL) {}

  bool Eval() const { return func1_ ? func1_(arg_) : func0_(); }

 private:
  bool (*func0_)();
  bool (*func1_)(T*);
  T *arg_;
};


/// Wrapper for pthread_mutex_t.
///
/// pthread_mutex_t is *not* a reader-writer lock,
/// so the methods like ReaderLock() aren't really reader locks.
/// We can not use pthread_rwlock_t because it
/// does not work with pthread_cond_t.
///
/// TODO: We still need to test reader locks with this class.
/// Implement a mode where pthread_rwlock_t will be used
/// instead of pthread_mutex_t (only when not used with CondVar or LockWhen).
///
class Mutex {
  friend class CondVar;
 public:
  Mutex() {
    CHECK(0 == pthread_mutex_init(&mu_, NULL));
    CHECK(0 == pthread_cond_init(&cv_, NULL));
    signal_at_unlock_ = true;  // Always signal at Unlock to make
                               // Mutex more friendly to hybrid detectors.
  }
  ~Mutex() {
    CHECK(0 == pthread_cond_destroy(&cv_));
    CHECK(0 == pthread_mutex_destroy(&mu_));
  }
  void Lock()          { CHECK(0 == pthread_mutex_lock(&mu_));}
  bool TryLock()       { return (0 == pthread_mutex_trylock(&mu_));}
  void Unlock() {
    if (signal_at_unlock_) {
      CHECK(0 == pthread_cond_signal(&cv_));
    }
    CHECK(0 == pthread_mutex_unlock(&mu_));
  }
  void ReaderLock()    { Lock(); }
  bool ReaderTryLock() { return TryLock();}
  void ReaderUnlock()  { Unlock(); }

  template <typename T>
  void LockWhen(const Condition<T>& cond)       { Lock(); WaitLoop(cond); }
  template <typename T>
  void ReaderLockWhen(const Condition<T>& cond) { Lock(); WaitLoop(cond); }
  template <typename T>
  void Await(const Condition<T>& cond)          { WaitLoop(cond); }

  template <typename T>
  bool ReaderLockWhenWithTimeout(const Condition<T>& cond, int millis)
    { Lock(); return WaitLoopWithTimeout(cond, millis); }
  template <typename T>
  bool LockWhenWithTimeout(const Condition<T>& cond, int millis)
    { Lock(); return WaitLoopWithTimeout(cond, millis); }
  template <typename T>
  bool AwaitWithTimeout(const Condition<T>& cond, int millis)
    { return WaitLoopWithTimeout(cond, millis); }

 private:

  template <typename T>
  void WaitLoop(const Condition<T>& cond) {
    signal_at_unlock_ = true;
    while(cond.Eval() == false) {
      pthread_cond_wait(&cv_, &mu_);
    }
    ANNOTATE_CONDVAR_LOCK_WAIT(&cv_, &mu_);
  }

  template <typename T>
  bool WaitLoopWithTimeout(const Condition<T>& cond, int millis) {
    struct timeval now;
    struct timespec timeout;
    int retcode = 0;
    gettimeofday(&now, NULL);
    timeval2timespec(&now, &timeout, millis);

    signal_at_unlock_ = true;
    while (cond.Eval() == false && retcode == 0) {
      retcode = pthread_cond_timedwait(&cv_, &mu_, &timeout);
    }
    if(retcode == 0) {
      ANNOTATE_CONDVAR_LOCK_WAIT(&cv_, &mu_);
    }
    return cond.Eval();
  }

  // A hack. cv_ should be the first data member so that
  // ANNOTATE_CONDVAR_WAIT(&MU, &MU) and ANNOTATE_CONDVAR_SIGNAL(&MU) works.
  // (See also racecheck_unittest.cc)
  pthread_cond_t  cv_;
  pthread_mutex_t mu_;
  bool            signal_at_unlock_;  // Set to true if Wait was called.
};


class MutexLock {  // Scoped Mutex Locker/Unlocker
 public:
  MutexLock(Mutex *mu)
    : mu_(mu) {
    mu_->Lock();
  }
  ~MutexLock() {
    mu_->Unlock();
  }
 private:
  Mutex *mu_;
};


/// Wrapper for pthread_cond_t.
class CondVar {
 public:
  CondVar()   { CHECK(0 == pthread_cond_init(&cv_, NULL)); }
  ~CondVar()  { CHECK(0 == pthread_cond_destroy(&cv_)); }
  void Wait(Mutex *mu) { CHECK(0 == pthread_cond_wait(&cv_, &mu->mu_)); }
  bool WaitWithTimeout(Mutex *mu, int millis) {
    struct timeval now;
    struct timespec timeout;
    gettimeofday(&now, NULL);
    timeval2timespec(&now, &timeout, millis);
    return 0 != pthread_cond_timedwait(&cv_, &mu->mu_, &timeout);
  }
  void Signal() { CHECK(0 == pthread_cond_signal(&cv_)); }
  void SignalAll() { CHECK(0 == pthread_cond_broadcast(&cv_)); }
 private:
  pthread_cond_t cv_;
};


// pthreads do not allow to use condvar with rwlock so we can't make
// ReaderLock method of Mutex to be the real rw-lock.
// So, we need a special lock class to test reader locks.
#define NEEDS_SEPERATE_RW_LOCK
class RWLock {
 public:
  RWLock() { CHECK(0 == pthread_rwlock_init(&mu_, NULL)); }
  ~RWLock() { CHECK(0 == pthread_rwlock_destroy(&mu_)); }
  void Lock() { CHECK(0 == pthread_rwlock_wrlock(&mu_)); }
  void ReaderLock() { CHECK(0 == pthread_rwlock_rdlock(&mu_)); }
  void Unlock() { CHECK(0 == pthread_rwlock_unlock(&mu_)); }
  void ReaderUnlock() { CHECK(0 == pthread_rwlock_unlock(&mu_)); }
 private:
  pthread_cond_t dummy; // Damn, this requires some redesign...
  pthread_rwlock_t mu_;
};

class ReaderLockScoped {  // Scoped RWLock Locker/Unlocker
 public:
  ReaderLockScoped(RWLock *mu)
    : mu_(mu) {
    mu_->ReaderLock();
  }
  ~ReaderLockScoped() {
    mu_->ReaderUnlock();
  }
 private:
  RWLock *mu_;
};

class WriterLockScoped {  // Scoped RWLock Locker/Unlocker
 public:
  WriterLockScoped(RWLock *mu)
    : mu_(mu) {
    mu_->Lock();
  }
  ~WriterLockScoped() {
    mu_->Unlock();
  }
 private:
  RWLock *mu_;
};




/// Wrapper for pthread_create()/pthread_join().
class MyThread {
 public:
  MyThread(void* (*worker)(void *), void *arg = NULL, const char *name = NULL)
      :wpvpv_(worker), wvv_(), wvpv_(), arg_(arg), name_(name) {}
  MyThread(void (*worker)(void), void *arg = NULL, const char *name = NULL)
      :wpvpv_(), wvv_(worker), wvpv_(), arg_(arg), name_(name) {}
  MyThread(void (*worker)(void *), void *arg = NULL, const char *name = NULL)
      :wpvpv_(), wvv_(), wvpv_(worker), arg_(arg), name_(name) {}

  void Start() { CHECK(0 == pthread_create(&t_, NULL, ThreadBody, this));}
  void Join()  { CHECK(0 == pthread_join(t_, NULL));}
  pthread_t tid() const { return t_; }
 private:
  static void *ThreadBody(void *arg) {
    MyThread *my_thread = reinterpret_cast<MyThread*>(arg);
    if (my_thread->name_) {
      ANNOTATE_THREAD_NAME(my_thread->name_);
    }
    if (my_thread->wpvpv_)
      return my_thread->wpvpv_(my_thread->arg_);
    if (my_thread->wvpv_)
      my_thread->wvpv_(my_thread->arg_);
    if (my_thread->wvv_)
      my_thread->wvv_();
    return NULL;
  }
  pthread_t t_;
  void *(*wpvpv_)(void*);
  void (*wvv_)(void);
  void (*wvpv_)(void*);
  void     *arg_;
  const char *name_;
};


/// Just a message queue.
class ProducerConsumerQueue {
 public:
  ProducerConsumerQueue(int unused) {
    //ANNOTATE_PCQ_CREATE(this);
  }
  ~ProducerConsumerQueue() {
    CHECK(q_.empty());
    //ANNOTATE_PCQ_DESTROY(this);
  }

  // Put.
  void Put(void *item) {
    mu_.Lock();
      q_.push(item);
      ANNOTATE_CONDVAR_SIGNAL(&mu_); // LockWhen in Get()
      //ANNOTATE_PCQ_PUT(this);
    mu_.Unlock();
  }

  // Get.
  // Blocks if the queue is empty.
  void *Get() {
    mu_.LockWhen(Condition<typeof(q_)>(IsQueueNotEmpty, &q_));
      void * item = NULL;
      bool ok = TryGetInternal(&item);
      CHECK(ok);
    mu_.Unlock();
    return item;
  }

  // If queue is not empty,
  // remove an element from queue, put it into *res and return true.
  // Otherwise return false.
  bool TryGet(void **res) {
    mu_.Lock();
      bool ok = TryGetInternal(res);
    mu_.Unlock();
    return ok;
  }

 private:
  Mutex mu_;
  std::queue<void*> q_; // protected by mu_

  // Requires mu_
  bool TryGetInternal(void ** item_ptr) {
    if (q_.empty())
      return false;
    *item_ptr = q_.front();
    q_.pop();
    //ANNOTATE_PCQ_GET(this);
    return true;
  }

  static bool IsQueueNotEmpty(std::queue<void*> * queue) {
     return !queue->empty();
  }
};



/// Function pointer with zero, one or two parameters.
struct Closure {
  typedef void (*F0)();
  typedef void (*F1)(void *arg1);
  typedef void (*F2)(void *arg1, void *arg2);
  int  n_params;
  void *f;
  void *param1;
  void *param2;

  void Execute() {
    if (n_params == 0) {
      (F0(f))();
    } else if (n_params == 1) {
      (F1(f))(param1);
    } else {
      CHECK(n_params == 2);
      (F2(f))(param1, param2);
    }
    delete this;
  }
};

Closure *NewCallback(void (*f)()) {
  Closure *res = new Closure;
  res->n_params = 0;
  res->f = (void*)(f);
  res->param1 = NULL;
  res->param2 = NULL;
  return res;
}

template <class P1>
Closure *NewCallback(void (*f)(P1), P1 p1) {
  CHECK(sizeof(P1) <= sizeof(void*));
  Closure *res = new Closure;
  res->n_params = 1;
  res->f = (void*)(f);
  res->param1 = (void*)p1;
  res->param2 = NULL;
  return res;
}

template <class T, class P1, class P2>
Closure *NewCallback(void (*f)(P1, P2), P1 p1, P2 p2) {
  CHECK(sizeof(P1) <= sizeof(void*));
  Closure *res = new Closure;
  res->n_params = 2;
  res->f = (void*)(f);
  res->param1 = (void*)p1;
  res->param2 = (void*)p2;
  return res;
}

/*! A thread pool that uses ProducerConsumerQueue.
  Usage:
  {
    ThreadPool pool(n_workers);
    pool.StartWorkers();
    pool.Add(NewCallback(func_with_no_args));
    pool.Add(NewCallback(func_with_one_arg, arg));
    pool.Add(NewCallback(func_with_two_args, arg1, arg2));
    ... // more calls to pool.Add()

    // the ~ThreadPool() is called: we wait workers to finish
    // and then join all threads in the pool.
  }
*/
class ThreadPool {
 public:
  //! Create n_threads threads, but do not start.
  explicit ThreadPool(int n_threads)
    : queue_(INT_MAX) {
    for (int i = 0; i < n_threads; i++) {
      MyThread *thread = new MyThread(&ThreadPool::Worker, this);
      workers_.push_back(thread);
    }
  }

  //! Start all threads.
  void StartWorkers() {
    for (size_t i = 0; i < workers_.size(); i++) {
      workers_[i]->Start();
    }
  }

  //! Add a closure.
  void Add(Closure *closure) {
    queue_.Put(closure);
  }

  int num_threads() { return workers_.size();}

  //! Wait workers to finish, then join all threads.
  ~ThreadPool() {
    for (size_t i = 0; i < workers_.size(); i++) {
      Add(NULL);
    }
    for (size_t i = 0; i < workers_.size(); i++) {
      workers_[i]->Join();
      delete workers_[i];
    }
  }
 private:
  std::vector<MyThread*>   workers_;
  ProducerConsumerQueue  queue_;

  static void *Worker(void *p) {
    ThreadPool *pool = reinterpret_cast<ThreadPool*>(p);
    while (true) {
      Closure *closure = reinterpret_cast<Closure*>(pool->queue_.Get());
      if(closure == NULL) {
        return NULL;
      }
      closure->Execute();
    }
  }
};

#ifndef NO_BARRIER
/// Wrapper for pthread_barrier_t.
class Barrier{
 public:
  explicit Barrier(int n_threads) {CHECK(0 == pthread_barrier_init(&b_, 0, n_threads));}
  ~Barrier()                      {CHECK(0 == pthread_barrier_destroy(&b_));}
  void Block() {
    // helgrind 3.3.0 does not have an interceptor for barrier.
    // but our current local version does.
    // ANNOTATE_CONDVAR_SIGNAL(this);
    pthread_barrier_wait(&b_);
    // ANNOTATE_CONDVAR_WAIT(this, this);
  }
 private:
  pthread_barrier_t b_;
};

#endif // NO_BARRIER

class BlockingCounter {
 public:
  explicit BlockingCounter(int initial_count) :
    count_(initial_count) {}
  bool DecrementCount() {
    MutexLock lock(&mu_);
    count_--;
    return count_ == 0;
  }
  void Wait() {
    mu_.LockWhen(Condition<int>(&IsZero, &count_));
    mu_.Unlock();
  }
 private:
  static bool IsZero(int *arg) { return *arg == 0; }
  Mutex mu_;
  int count_;
};

int AtomicIncrement(volatile int *value, int increment);

#ifndef VGO_darwin
inline int AtomicIncrement(volatile int *value, int increment) {
  return __sync_add_and_fetch(value, increment);
}

#else
// Mac OS X version.
inline int AtomicIncrement(volatile int *value, int increment) {
  return OSAtomicAdd32(increment, value);
}

// TODO(timurrrr) this is a hack
#define memalign(A,B) malloc(B)

// TODO(timurrrr) this is a hack
int posix_memalign(void **out, size_t al, size_t size) {
  *out = memalign(al, size);
  return (*out == 0);
}
#endif // VGO_darwin

#endif // THREAD_WRAPPERS_PTHREAD_H
// vim:shiftwidth=2:softtabstop=2:expandtab:foldmethod=marker
