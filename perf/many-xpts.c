#include <stdlib.h>

#define nth_bit(x, n)   ((x >> n) & 1)
#define Fn(N, Np1) \
   void* a##N(int x) { return ( nth_bit(x, N) ? a##Np1(x) : a##Np1(x) ); }

// This test allocates a lot of heap memory, and every allocation features a
// different stack trace -- the stack traces are effectively a
// representation of the number 'i', where each function represents a bit in
// 'i', and if it's a 1 the first function is called, and if it's a 0 the
// second function is called.

void* a999(int x)
{
   return malloc(100);
}

Fn(17, 999)
Fn(16, 17)
Fn(15, 16)
Fn(14, 15)
Fn(13, 14)
Fn(12, 13)
Fn(11, 12)
Fn(10, 11)
Fn( 9, 10)
Fn( 8, 9)
Fn( 7, 8)
Fn( 6, 7)
Fn( 5, 6)
Fn( 4, 5)
Fn( 3, 4)
Fn( 2, 3)
Fn( 1, 2)
Fn( 0, 1)

int main(void)
{
   int i;

   // Create a large XTree.
   for (i = 0; i < (1 << 18); i++)
      a0(i);

   // Do a lot of allocations so it gets dup'd a lot of times.
   for (i = 0; i < 100000; i++) {
      free(a1(234));
      free(a2(111));
   }

   return 0;
}
