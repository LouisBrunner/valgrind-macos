
#include <stdio.h>

typedef    signed int  Int;
typedef  unsigned int  UInt;

__attribute__((noinline)) UInt do_udiv32 ( UInt x, UInt y )
{
  UInt res;
  __asm__ __volatile__(
     "mov r9, %1 ; mov r10, %2 ; udiv r3,r9,r10 ; mov %0, r3"
     : "=r"(res) : "r"(x), "r"(y) : "r3", "r9", "r10" 
  );
  return res;
}

__attribute__((noinline)) Int do_sdiv32 ( Int x, Int y )
{
  UInt res;
  __asm__ __volatile__(
     "mov r9, %1 ; mov r10, %2 ; sdiv r3,r9,r10 ; mov %0, r3"
     : "=r"(res) : "r"(x), "r"(y) : "r3", "r9", "r10" 
  );
  return res;
}

void test ( UInt x, UInt y )
{
  UInt ru = do_udiv32(x,y);
  Int rs = do_sdiv32(x,y);
  printf( "%08x %08x -> u:%08x s:%08x\n", x, y, ru, (UInt)rs);
}

int main ( void )
{
  // Check basic operation
  test( 500, 50 );
  test( 500, -50 );
  test( -500, 50 );
  test( -500, -50 );
  // Check for rounding towards zero
  test( 100, 7 );   // 14.285
  test( -100, 7 );
  test( 100, -7 );
  test( -100, -7 );
  // Division by zero produces zero
  test( 1, 0 );
  test( 0, 0 );
  test( -1, 0 );
  test( 0x80000000, 0 );
  test( 0x7FFFFFFF, 0 );
  // Test signed range ends
  test( 0x80000000, -1 ); // unrepresentable as signed 32
  return 0;
}
