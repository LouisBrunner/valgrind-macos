// mismatches in the parent directory has a size
// mismatch, but only with GCC, hence a testcase that
// explicitly has delete size mismatches

#include <new>
#include <cassert>
#include "valgrind.h"

int main()
{
   std::size_t size(32);
   std::size_t badsize(33);

   void *mem = nullptr;

   mem = operator new(size);
   operator delete(mem, badsize);

   mem = operator new[](size);
   operator delete[](mem, badsize);

}
