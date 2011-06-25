#include <stdio.h>
#include <string.h>
#include "opcodes.h"

int main()
{
   register long g asm("r7");
   register double f asm("f8");
   double f1;

   memset(&f1, 0x0f, sizeof(double));
   f = f1;
   g = 42;
   printf("test LGDR\n\n");
   printf("before g = %ld\n", g);
   printf("before f = %a\n", f);
   printf("copy f to g\n");
   asm volatile ( LGDR(7,8) : "=d"(g) : "f"(f));
   printf("after  g = %16.16lx\n", g);  /* 0x0x0x0...... */
   printf("after  f = %a\n", f);

   printf("\ntest LDGR\n\n");
   f = 3.14;
   printf("before g = %16.16lx\n", g);  /* 0x0x0x0...... */
   printf("before f = %a\n", f);
   printf("copy g to f\n");
   asm volatile ( LDGR(8,7) : "=f"(f) : "d"(g));
   printf("after  g = %16.16lx\n", g);  /* 0x0x0x0...... */
   printf("after  f = %a\n", f);

   return 0;
}

