
/* Marginally test fprem/fprem1; these are hard to check otherwise */

#include <stdio.h>

double do_fprem ( void )
{
  double res;
  __asm__ __volatile__(
    "ffree %%st(0)\n\t"
    "ffree %%st(1)\n\t"
    "fldpi\n\t"
    "fldln2\n\t"
    "fprem\n\t"
    "fstpl 0(%0)"
    : : "r"(&res)  
  );
  return res;
}

double do_fprem1 ( void )
{
  double res;
  __asm__ __volatile__(
    "ffree %%st(0)\n\t"
    "ffree %%st(1)\n\t"
    "fldpi\n\t"
    "fldln2\n\t"
    "fprem1\n\t"
    "fstpl 0(%0)"
    : : "r"(&res)  
  );
  return res;
}

int main ( void )
{
  __asm__ __volatile__("finit");
  printf("fprem  %f\n", do_fprem());
  printf("fprem1 %f\n", do_fprem1());
  return 0;
}
