
/* Test for long-form encodings of push %reg */

#include <stdio.h>

int foo ( int x )
{
  int block[2];
  block[0] = x;
  block[1] = 0;
  __asm__ __volatile__(
    "movl $0,%%edi\n\t"
    "movl $0,%%esi\n\t"
    "movl %0,%%edi\n\t"
    ".byte   0xFF,0xF7\n\t" /*pushl %edi */
    "popl %%esi\n\t"
    "movl %%esi, %1"
    : : /*in*/ "m"(block[0]), "m"(block[1]) : "esi","edi","memory"
  );
  return block[1]; 
}

int main ( void )
{
  int i;
  for (i = 0; i < 100000000; i += 11111111)
    printf("%d %d\n",i,foo(i));
  return 0;
}
