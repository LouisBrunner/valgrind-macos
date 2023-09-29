#include <stdio.h>

static const union words {
   char c[5 * 8];
   unsigned long i[5];
}  base = { .c = "0123456789abcdefghijklmnopqrstuvwxyzABCD" },
   init = { .c = "._,-'-._,-'-._,-'-._,-'-._,-'-._,-'-._,-" };

static void stmg_no_wrap(void)
{
   union words buf = init;
   register unsigned long a asm("5") = base.i[0];
   register unsigned long b asm("6") = base.i[1];
   register unsigned long c asm("7") = base.i[2];

   /* No-wrap around case; copies 24 bytes from BASE to BUF */
   asm ("stmg %[a], %[c], %[buf]"
        : [buf] "=S"(buf)
        : [a] "d"(a), "d"(b), [c] "d"(c)
        : );

   /* Write out BUF */
   fwrite(buf.c, sizeof(buf), 1, stdout);
}

static void stmg_wrap(void)
{
   union words buf = init;
   register unsigned long sp asm("15");
   register unsigned long a asm("0") = base.i[2];
   register unsigned long b asm("1") = base.i[3];
   register unsigned long c asm("2") = base.i[4];

   /* Wrap around case: avoid changing r15, but ensure it's stored */
   asm ("stmg 15, %[c], %[buf]"
        : [buf] "=S"(buf), "=d"(sp)
        : "d"(a), "d"(b), [c] "d"(c)
        : );
   buf.i[0] ^= sp ^ base.i[1];

   /* Write out BUF */
   fwrite(buf.c, sizeof(buf), 1, stdout);
}


int main(void)
{
   stmg_no_wrap();
   putchar('\n');
   stmg_wrap();
   putchar('\n');

   return 0;
}
