/*
   An example that shows how to implement the monitor synchronization concept.
   See also http://en.wikipedia.org/wiki/Monitor_(synchronization) for more
   information about this concept.
  
   ----------------------------------------------------------------

   Notice that the following BSD-style license applies to this one
   file (monitor_example.cpp) only.  The rest of Valgrind is licensed
   under the terms of the GNU General Public License, version 2,
   unless otherwise indicated.  See the COPYING file in the source
   distribution for details.

   ----------------------------------------------------------------

   This file is part of DRD, a heavyweight Valgrind tool for detecting
   errors in multithreaded programs.

   Copyright (C) 2008-2009 Bart Van Assche. All rights reserved.

   Redistribution and use in source and binary forms, with or without
   modification, are permitted provided that the following conditions
   are met:

   1. Redistributions of source code must retain the above copyright
      notice, this list of conditions and the following disclaimer.

   2. The origin of this software must not be misrepresented; you must 
      not claim that you wrote the original software.  If you use this 
      software in a product, an acknowledgment in the product 
      documentation would be appreciated but is not required.

   3. Altered source versions must be plainly marked as such, and must
      not be misrepresented as being the original software.

   4. The name of the author may not be used to endorse or promote 
      products derived from this software without specific prior written 
      permission.

   THIS SOFTWARE IS PROVIDED BY THE AUTHOR ``AS IS'' AND ANY EXPRESS
   OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
   WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
   ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY
   DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
   DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE
   GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
   INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
   WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
   NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
   SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

   ----------------------------------------------------------------

   Notice that the above BSD-style license applies to this one
   file (monitor_example.cpp) only.  The rest of Valgrind is licensed
   under the terms of the GNU General Public License, version 2,
   unless otherwise indicated.  See the COPYING file in the source
   distribution for details.

   ---------------------------------------------------------------- 
*/


#define _GNU_SOURCE 1


#include "config.h"
#include <cassert>
#include <iostream>
#include <pthread.h>


class Monitor
{
public:
  Monitor()
    : m_mutex()
    , m_cond()
    , m_owner()
    , m_recursion_count()
  {
    pthread_mutexattr_t mutexattr;
    pthread_mutexattr_init(&mutexattr);
    pthread_mutexattr_settype(&mutexattr, PTHREAD_MUTEX_RECURSIVE);
    pthread_mutex_init(&m_mutex, &mutexattr);
    pthread_mutexattr_destroy(&mutexattr);
    pthread_condattr_t condattr;
    pthread_condattr_init(&condattr);
#if defined(HAVE_PTHREAD_CONDATTR_SETCLOCK)
    pthread_condattr_setclock(&condattr, CLOCK_MONOTONIC);
#endif
    pthread_cond_init(&m_cond, &condattr);
    pthread_condattr_destroy(&condattr);
  }
  ~Monitor()
  {
    assert(m_recursion_count == 0);
    pthread_cond_destroy(&m_cond);
    pthread_mutex_destroy(&m_mutex);
  }
  void lock()
  {
    pthread_mutex_lock(&m_mutex);
    assert(m_recursion_count >= 0);
    if (++m_recursion_count == 1)
    {
      m_owner = pthread_self();
    }
  }
  void unlock()
  {
    m_recursion_count--;
    assert(m_recursion_count >= 0);
    pthread_mutex_unlock(&m_mutex);
  }
  void wait()
  {
    assert(m_recursion_count == 1);
    assert(m_owner == pthread_self());
    m_recursion_count--;
    pthread_cond_wait(&m_cond, &m_mutex);
    m_recursion_count++;
    m_owner = pthread_self();
  }
  void signal()
  {
    assert(m_recursion_count > 0);
    pthread_cond_signal(&m_cond);
  }
  void broadcast_signal()
  {
    assert(m_recursion_count > 0);
    pthread_cond_broadcast(&m_cond);
  }
  bool is_locked_by_self()
  {
    bool result;
    pthread_mutex_lock(&m_mutex);
    result = m_recursion_count > 0 && m_owner == pthread_self();
    pthread_mutex_unlock(&m_mutex);
    return result;
  }

private:
  Monitor(const Monitor&);
  Monitor& operator=(const Monitor&);

  pthread_mutex_t m_mutex;
  pthread_cond_t  m_cond;
  pthread_t       m_owner;
  int             m_recursion_count;
};


class ScopedLock
{
public:
  ScopedLock(Monitor& m)
    : m_monitor(m)
    , m_locked(false)
  { lock(); }
  ~ScopedLock()
  { if (m_locked) unlock(); }
  void lock()
  { assert(! m_locked); m_monitor.lock(); m_locked = true; }
  void unlock()
  { assert(m_locked); m_locked = false; m_monitor.unlock(); }

private:
  ScopedLock(const ScopedLock&);
  ScopedLock& operator=(const ScopedLock&);

  Monitor& m_monitor;
  bool     m_locked;
};


class StateVariable
{
public:
  StateVariable()
    : m_state()
  { }
  int get()
  {
    ScopedLock sl(m_monitor);
    return m_state;
  }
  void set(const int state)
  {
    ScopedLock sl(m_monitor);
    m_state = state;
    m_monitor.signal();
  }
  void wait(const int state)
  {
    ScopedLock sl(m_monitor);
    while (m_state != state)
      m_monitor.wait();
  }

private:
  Monitor m_monitor;
  int     m_state;
};


static StateVariable s_sv;


static void* thread_func(void*)
{
  s_sv.wait(1);
  s_sv.set(2);
  s_sv.wait(3);
  s_sv.set(4);
  return 0;
}

int main(int, char**)
{
  pthread_t tid;
  pthread_create(&tid, 0, thread_func, 0);
  s_sv.set(1);
  s_sv.wait(2);
  s_sv.set(3);
  s_sv.wait(4);
  pthread_join(tid, 0);
  std::cerr << "Finished successfully.\n";
  return 0;
}
