
#include <stdio.h>
#include <math.h>

typedef unsigned long long int ULong;

typedef
   struct { double d; int i; } Res;

static void do_fprem ( Res* res, double x, double y )
{
  ULong c3210;
  double f64;
  double xx = x;
  double yy = y;
  __asm__ __volatile__(
     "finit\n\t"
     "fldl    %2\n\t"
     "fldl    %3\n\t"
     "fprem\n\t"
     "fstpl   %1\n\t"
     "movq    %%rax,%%r15\n\t"
     "xorq    %%rax,%%rax\n\t"
     "fnstsw  %%ax\n\t"
     "movq    %%rax,%0\n\t"
     "movq    %%r15,%%rax"
     : /*out*/ "=r" (c3210)
     : /*in*/  "m" (f64), "m" (xx), "m" (yy)
     : /*trash*/ "r15", "rax", "%st", "%st(1)", "cc"
   );
  res->d = f64;
  res->i = (int)(c3210 & 0x4700); /* mask for C3,2,1,0 */
}

static void show ( char* s, Res* res )
{
  printf("%s -> 0x%04x %f\n", s, (int)res->i, (double)res->d);
}

int main ( void )
{
  Res r;
  int i;
  double theta;
 
  do_fprem(&r, 10.1, 200.2); show("xx1", &r);
  do_fprem(&r, 20.3, 1.44);  show("xx2", &r);

  for (i = 0; i < 20; i++) {
    theta = (2.0 * 3.14159) / 10.0 * (double)i;
    do_fprem(&r, 12.3*sin(theta), cos(theta)); show("xx", &r);
  }

  return 0;
}
