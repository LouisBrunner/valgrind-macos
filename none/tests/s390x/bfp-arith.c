#include <stdio.h>
#include <math.h>
#include <assert.h>

/* Making sure that the condition code of arithmetic BFP ops is
   set correctly.

   Multiply and square root are covered by bfp-emit.pl
*/

volatile int cc;

#define TEST_INSN(insn, fmt, opnd1, opnd2)                 \
   do {                                                    \
      __typeof__(opnd1) v1 = opnd1;                        \
      __typeof__(opnd2) v2 = opnd2;                        \
      __asm__ volatile(insn " %[r1],%[r2]\n\t"             \
                       "ipm   %[psw]\n\t"                  \
                       "srl   %[psw],28\n\t"               \
                       : [psw]"=d"(cc)                     \
                       : [r1]"f"(v1), [r2]"f"(v2)          \
                       : "cc");                            \
      printf("%s  r1 = "fmt"  r2 = "fmt"  -->  cc = %d\n", \
             insn, opnd1, opnd2, cc);                      \
   } while (0)


static void aebr(float r1, float r2)
{
   TEST_INSN("aebr", "%g", r1, r2);
}

static void adbr(double r1, double r2)
{
   TEST_INSN("adbr", "%g", r1, r2);
}

static void axbr(long double r1, long double r2)
{
   TEST_INSN("axbr", "%Lg", r1, r2);
}

static void sebr(float r1, float r2)
{
   TEST_INSN("sebr", "%g", r1, r2);
}

static void sdbr(double r1, double r2)
{
   TEST_INSN("sdbr", "%g", r1, r2);
}

static void sxbr(long double r1, long double r2)
{
   TEST_INSN("sxbr", "%Lg", r1, r2);
}

int main()
{
   assert(sizeof(long double) == 16);  // ensure 128 bit wide

   printf("32-bit ADD\n");
   aebr(2.5f, -2.5f);
   aebr(4.75f, -6.25f);
   aebr(-2.5f, 8.5f);
   aebr(NAN, 0);

   printf("64-bit ADD\n");
   adbr(2.5, -2.5);
   adbr(4.75, -6.25);
   adbr(-2.5, 8.5);
   adbr(NAN, 0);

   printf("128-bit ADD\n");
   axbr(2.5L, -2.5L);
   axbr(4.75L, -6.25L);
   axbr(-2.5L, 8.5L);
   axbr(NAN, 0);

   printf("32-bit SUBTRACT\n");
   sebr(2.5f, 2.5f);
   sebr(4.75f, 6.25f);
   sebr(8.5f, 2.5f);
   sebr(NAN, 0);

   printf("64-bit SUBTRACT\n");
   sdbr(2.5, 2.5);
   sdbr(4.75, 6.25);
   sdbr(8.5, 2.5f);
   sdbr(NAN, 0);

   printf("128-bit SUBTRACT\n");
   sxbr(2.5f, 2.5);
   sxbr(4.75f, 6.25);
   sxbr(8.5f, 2.5);
   sxbr(NAN, 0);

   return 0;
}
