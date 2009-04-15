#include "tests/asm.h"
#include <stdio.h>

double zzz;

int main ( void )
{
  zzz = 1.234;
  printf("zzz = %f\n", zzz);
  __asm__ __volatile__(
    "finit\n\t"
    "fldpi\n\t"
    "fldl2e\n\t"
    "pushl %esi\n\t"
    "movl $0,%esi\n\t"
    "add %esi,%esi\n\t"
    "fcmovnu %st(1), %st(0)\n\t"
    "fstl " VG_SYM(zzz) "\n\t"
    "finit\n\t"
    "popl %esi\n\t"
    );
  printf("zzz = %f\n", zzz);
  return 0;
}
