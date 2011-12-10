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
#define _GLIBCXX_EXTERN_TEMPLATE -1

#include <iostream>
#include <thread>

int main(int argc, char** argv)
{
  std::thread t( []() { } );
  t.join();
  std::cerr << "Done.\n";
  return 0;
}
