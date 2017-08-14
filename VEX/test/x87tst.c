
#include <stdio.h>
#include <math.h>

double d;
int i;

extern void do_tst ( void );

asm(
"\n"
"do_tst:\n"
"\txorl %eax,%eax\n"
"\tfld d\n"
"\tftst\n"
"\tfnstsw %ax\n"
"\tmovl %eax, i\n"
"\tret\n"
);

int main ( void )
{
   d = -1.23; do_tst(); printf("%f -> 0x%x\n", d, i );
   d = 0.0;   do_tst(); printf("%f -> 0x%x\n", d, i );
   d = 9.87;  do_tst(); printf("%f -> 0x%x\n", d, i );
   return 0;
}
