
#include <stdio.h>
#include <math.h>

double d;
int i;

extern void do_fxam ( void );

asm(
"\n"
"do_fxam:\n"
"\txorl %eax,%eax\n"
"\tfld d\n"
"\tfxam\n"
"\tfnstsw %ax\n"
"\tffree %st(0)\n"
"\tmovl %eax, i\n"
"\tret\n"
);


double inf ( void ) { return 1.0 / 0.0; }
double nAn ( void ) { return 0.0 / 0.0; }
double den ( void ) { return 9.1e-220 / 1e100; }

/* Try positive and negative variants of: zero, infinity,
   nAn, and denorm */

int main ( void )
{
   d =  0.0;   do_fxam(); printf("0x%4x: %f\n", i, d );
   d = -0.0;   do_fxam(); printf("0x%4x: %f\n", i, d );

   d =  inf(); do_fxam(); printf("0x%4x: %f\n", i, d );
   d = -inf(); do_fxam(); printf("0x%4x: %f\n", i, d );

   d =  nAn(); do_fxam(); printf("0x%4x: %f\n", i, d );
   d = -nAn(); do_fxam(); printf("0x%4x: %f\n", i, d );

   d =  den(); do_fxam(); printf("0x%4x: %f\n", i, d );
   d = -den(); do_fxam(); printf("0x%4x: %f\n", i, d );
   return 0;
}
