
#include <stdio.h>

typedef unsigned int UInt;
typedef unsigned long long ULong;

#if defined(__x86_64__)

#define BSWAPQ(_lval) \
  do { \
  __asm__ __volatile__("bswapq %0" \
		       : /*out*/ "+r"(_lval) ); \
  } while (0)

ULong bswapq ( ULong x )
{
  BSWAPQ(x); BSWAPQ(x);
  BSWAPQ(x); BSWAPQ(x);
  BSWAPQ(x); BSWAPQ(x);
  BSWAPQ(x); BSWAPQ(x);
  BSWAPQ(x); BSWAPQ(x);
  BSWAPQ(x); BSWAPQ(x);
  BSWAPQ(x); BSWAPQ(x);
  BSWAPQ(x); BSWAPQ(x);
  BSWAPQ(x); BSWAPQ(x);
  BSWAPQ(x); BSWAPQ(x);
  BSWAPQ(x); BSWAPQ(x);
  BSWAPQ(x); BSWAPQ(x);
  BSWAPQ(x); BSWAPQ(x);
  BSWAPQ(x); BSWAPQ(x);
  BSWAPQ(x); BSWAPQ(x);
  BSWAPQ(x); BSWAPQ(x);
  BSWAPQ(x); BSWAPQ(x);
  BSWAPQ(x); BSWAPQ(x);
  BSWAPQ(x); BSWAPQ(x);
  BSWAPQ(x); BSWAPQ(x);
  BSWAPQ(x); BSWAPQ(x);
  BSWAPQ(x); BSWAPQ(x);
  BSWAPQ(x); BSWAPQ(x);
  BSWAPQ(x); BSWAPQ(x);
  BSWAPQ(x); BSWAPQ(x);
  BSWAPQ(x); BSWAPQ(x);
  BSWAPQ(x); BSWAPQ(x);
  BSWAPQ(x); BSWAPQ(x);
  BSWAPQ(x); BSWAPQ(x);
  BSWAPQ(x); BSWAPQ(x);
  BSWAPQ(x); BSWAPQ(x);
  BSWAPQ(x); BSWAPQ(x);
  BSWAPQ(x);
  return x;
}

#endif /* defined(__x86_64__) */

#define BSWAPL(_lval) \
  do { \
  __asm__ __volatile__("bswapl %0" \
		       : /*out*/ "+r"(_lval) ); \
  } while (0)

UInt bswapl ( UInt x )
{
  BSWAPL(x); BSWAPL(x);
  BSWAPL(x); BSWAPL(x);
  BSWAPL(x); BSWAPL(x);
  BSWAPL(x); BSWAPL(x);
  BSWAPL(x); BSWAPL(x);
  BSWAPL(x); BSWAPL(x);
  BSWAPL(x); BSWAPL(x);
  BSWAPL(x); BSWAPL(x);
  BSWAPL(x); BSWAPL(x);
  BSWAPL(x); BSWAPL(x);
  BSWAPL(x); BSWAPL(x);
  BSWAPL(x); BSWAPL(x);
  BSWAPL(x); BSWAPL(x);
  BSWAPL(x); BSWAPL(x);
  BSWAPL(x); BSWAPL(x);
  BSWAPL(x); BSWAPL(x);
  BSWAPL(x); BSWAPL(x);
  BSWAPL(x); BSWAPL(x);
  BSWAPL(x); BSWAPL(x);
  BSWAPL(x); BSWAPL(x);
  BSWAPL(x); BSWAPL(x);
  BSWAPL(x); BSWAPL(x);
  BSWAPL(x); BSWAPL(x);
  BSWAPL(x); BSWAPL(x);
  BSWAPL(x); BSWAPL(x);
  BSWAPL(x); BSWAPL(x);
  BSWAPL(x); BSWAPL(x);
  BSWAPL(x); BSWAPL(x);
  BSWAPL(x); BSWAPL(x);
  BSWAPL(x); BSWAPL(x);
  BSWAPL(x); BSWAPL(x);
  BSWAPL(x); BSWAPL(x);
  BSWAPL(x);
  return x;
}

int main ( void )
{
#if defined(__x86_64__)
   printf("0x%llx\n", bswapq( 0x8877665544332211ULL ));
#endif
   printf("0x%x\n", bswapl( 0x44332211ULL ));
   return 0;
}
