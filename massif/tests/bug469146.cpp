#include <cstdlib>

// this is inline so it filters as "filter_function1"
static inline int* filter_function1(std::size_t size)
{
   return new int[size];
}

// this is out of line C++
// int is deliberately used here instead of size_t
// so that it is 32/b4 bit portable
// this filters as "filter_function2(int)"
int* __attribute__((optnone)) __attribute__((noinline)) filter_function2(int size)
{
   return new int[static_cast<std::size_t>(size)];
}

// finally extern "C"
// this filters as "filter_function3"
extern "C"
int*  __attribute__((optnone)) __attribute__((noinline)) filter_function3(std::size_t size)
{
   return new int[size];
}

size_t func()
{
   int * mem1 = filter_function1(1000U);
   int * mem2 = filter_function2(1000);
   int * mem3 = filter_function3(1000U);
   delete [] mem1;
   delete [] mem2;
   delete [] mem3;
   
   return (size_t)mem1/2 + (size_t)mem2/2 + (size_t)mem3/2;
}

int main()
{
   return func();
}
