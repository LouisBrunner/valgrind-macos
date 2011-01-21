#include <stdio.h>

int
main (void)
{
  long rcx = 0x200000005UL;
  long rax = 5UL;
  asm volatile ("1: addq $1, %0; loopnel 1b" : "+a" (rax), "+c" (rcx) : : "cc");
  printf ("%ld %ld\n", rax, rcx);
  return 0;
}
