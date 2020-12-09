// Test whether no race conditions are reported on std::thread. Note: since
// the implementation of std::thread uses the shared pointer implementation,
// that implementation has to be annotated in order to avoid false positives.
// See also http://gcc.gnu.org/onlinedocs/libstdc++/manual/debug.html for more
// information.

#include "../../drd/drd.h"
#define _GLIBCXX_SYNCHRONIZATION_HAPPENS_BEFORE(addr) \
  ANNOTATE_HAPPENS_BEFORE(addr)
#define _GLIBCXX_SYNCHRONIZATION_HAPPENS_AFTER(addr) \
  ANNOTATE_HAPPENS_AFTER(addr)

#include <iostream>
#include <thread>

int main(int argc, char** argv)
{
  std::thread t( []() { } );
  t.join();
  std::cerr << "Done.\n";
  return 0;
}

#if defined(__GNUC__) && __GNUC__ -0 < 6 && !defined(__clang__)
//
// From libstdc++-v3/src/c++11/thread.cc
//

extern "C" void* _v_execute_native_thread_routine(void* __p)
{
  std::thread::_Impl_base* __t = static_cast<std::thread::_Impl_base*>(__p);
  std::thread::__shared_base_type __local;
  __local.swap(__t->_M_this_ptr);

  __try {
    __t->_M_run();
  } __catch(const __cxxabiv1::__forced_unwind&) {
    __throw_exception_again;
  } __catch(...) {
    std::terminate();
  }

  return 0;
}

#include <system_error>

namespace std
{
  void thread::_M_start_thread(__shared_base_type __b)
  {
    if (!__gthread_active_p())
#if __EXCEPTIONS
      throw system_error(make_error_code(errc::operation_not_permitted),
                         "Enable multithreading to use std::thread");
#else
      __throw_system_error(int(errc::operation_not_permitted));
#endif

    __b->_M_this_ptr = __b;
    int __e = __gthread_create(&_M_id._M_thread, _v_execute_native_thread_routine,
                               __b.get());
    if (__e) {
      __b->_M_this_ptr.reset();
      __throw_system_error(__e);
    }
  }
}
#endif
