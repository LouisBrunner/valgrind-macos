
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
    "pushq %r15\n\t"
    "movq $0,%r15\n\t"
    "add %r15,%r15\n\t"
    "fcmovnu %st(1), %st(0)\n\t"
    "fstl zzz\n\t"
    "finit\n\t"
    "popq %r15\n\t"
    );
  printf("zzz = %f\n", zzz);
  return 0;
}
