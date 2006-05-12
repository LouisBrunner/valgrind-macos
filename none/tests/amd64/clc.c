
#include <stdio.h>

typedef unsigned long long int ULong;

ULong do_clc ( void )
{
  ULong res;
  __asm__ __volatile__(
     "pushq $0x8d5\n\t" /* OSZACP */
     "popfq\n\t"
     "clc\n\t"
     "pushfq\n\t"
     "popq %0"
     : "=r"(res)
     :
     : "memory", "cc"
     );
  return res;
}

ULong do_stc ( void )
{
  ULong res;
  __asm__ __volatile__(
     "pushq $0x0\n\t"
     "popfq\n\t"
     "stc\n\t"
     "pushfq\n\t"
     "popq %0"
     : "=r"(res)
     :
     : "memory", "cc"
     );
  return res;
}

ULong do_cmc ( void )
{
  ULong res;
  __asm__ __volatile__(
     "pushq $0x0\n\t"
     "popfq\n\t"
     "stc\n\t"
     "cmc\n\t"
     "pushfq\n\t"
     "popq %0"
     : "=r"(res)
     :
     : "memory", "cc"
     );
  return res;
}

int main ( void )
{
  printf("clc: 0x%016llx\n", 0x8d5 & do_clc());
  printf("stc: 0x%016llx\n", 0x8d5 & do_stc());
  printf("cmc: 0x%016llx\n", 0x8d5 & do_cmc());
  return 0;
}
