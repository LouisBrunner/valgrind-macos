
/*
This is a regression test for the following problem, noticed by
Greg Parker:

vex ppc64 generates bad code for instruction sequences like this:

    li    r0, 2
    stdx  r3, r1, r0

gcc emits code like this when manipulating packed structures 
with 8-byte fields on 2-byte boundaries.

First, vex's optimizer substitutes a constant 0x2 for r0:

    ------ IMark(0x100000F34, 4) ------
    PUT(1024) = 0x100000F34:I64
    t3 = GET:I64(24)
    t14 = GET:I64(8)
    t13 = Add64(t14,0x2:I64)
    STbe(t13) = t3

Then instruction selection chooses `std` with an index not divisible by 4:

    -- STbe(Add64(GET:I64(8),0x2:I64)) = GET:I64(24)
    ldz %vR22,8(%r31)
    ldz %vR23,24(%r31)
    std %vR23,2(%vR22)

Finally, the assembler silently strips the index&3 part, 
because `std` can't encode that:

    std %r6,2(%r5)
    F8 C5 00 00 

...but 0xF8C50000 is `std r6, 0(r5)`, which writes to the wrong address.
*/

#include <stdio.h>
#include <stdlib.h>
#include <assert.h>

typedef 
struct __attribute__ ((__packed__)) {
  char before[2];
  unsigned long long int w64;
  char after[6];
}
T;

void foo (T* t, unsigned long long int w)
{
  __asm__ __volatile__(
     "stdx %0,%1,%2"
     : : "b"(w), "b"(t), "b"(2) : "memory"
  );
}

int main ( void )
{
  T* t;
  unsigned char* p;
  int i;
  assert(sizeof(T) == 16);
  t = calloc(sizeof(T),1);
  assert(t);
  /* check t is 8-aligned.  This causes the write done by 'foo' to be
     misaligned by 2 as desired, triggering the bug. */
  assert(0 == (((unsigned long)t) & 7));
  foo(t, 0x1122334455667788);
  p = (unsigned char*)t;
  for (i = 0; i < 16; i++)
    if (p[i] == 0)
      printf(".."); 
    else
      printf("%02x", (int)p[i]);
  printf("\n");
  return 0;
}
