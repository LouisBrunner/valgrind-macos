#include <stdio.h>

char base[] ="0123456789012345678901234567890123456789";

void
stmg_no_wrap(void)
{
   char buf[24];

   /* No-wrap around case; copies 24 bytes from BASE to BUF */
   asm volatile( "lg   5,  0(%1)\n\t"
                 "lg   6,  8(%1)\n\t"
                 "lg   7, 16(%1)\n\t"
                 "stmg 5, 7, %0\n\t"
                 :"=m" (buf)
                 : "a" (base)
                 : "5", "6", "7");
   /* Write out BUF */
   fwrite(buf, sizeof(buf), 1, stdout);
}

void
stmg_wrap(void)
{
   char buf[32];

   /* Wrap around case; copies 32 bytes from BASE to BUF */
   asm volatile( "lgr   3, 15\n\t"     /* save stack pointer */
                 "lg    0,  8(%1)\n\t"
                 "lg    1, 16(%1)\n\t"
                 "lg    2, 24(%1)\n\t"
                 "lg   15,  0(%1)\n\t"
                 "stmg 15, 2, %0\n\t"
                 "lgr  15, 3"          /* restore stack pointer */
                 :"=S" (buf)
                 : "a" (base)
                 : "0", "1", "2", "3");
   /* Write out BUF */
   fwrite(buf, sizeof(buf), 1, stdout);
}


int main(void)
{
   stmg_no_wrap();
   putchar('\n');
   stmg_wrap();
   putchar('\n');

   return 0;
}
