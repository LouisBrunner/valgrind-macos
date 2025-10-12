#include <stdio.h>
#include <assert.h>
#include <math.h>

/* Test various BFP ops:
   - load and test
   - load zero
*/

#define LOADxxx(insn, initial, type, fmt)                          \
   do {                                                            \
      int cc;                                                      \
      type in  = initial;                                          \
      type out = 17;                                               \
      __asm__ volatile("cr 0,0\n\t          /* clear cc */\n\t"    \
                       insn " %[r1],%[r2]\n\t"                     \
                       "ipm %[psw]\n\t"                            \
                       "srl %[psw],28\n\t"                         \
                       : [r1]"=f"(out), [psw]"=d"(cc)              \
                       : [r2]"f"(in)                               \
                       : "cc");                                    \
      printf(insn " " fmt " -> " fmt "   cc = %d\n", in, out, cc); \
   } while (0)

void lzxr(void)
{
   long double value;

   __builtin_memset(&value, 0xff, sizeof value);
   __asm__ volatile("lzxr %[r1]" : [r1]"=f"(value));

   printf("lzxr --> ");
   for (unsigned i = 0; i < sizeof(long double); ++i) {
      if (i % 4 == 0) putchar(' ');
      unsigned byte = ((char *)&value)[i];
      printf("%02u", byte);
   }
   putchar ('\n');
}

int main(void)
{
   assert(sizeof(long double) == 16);

   printf("LOAD AND TEST short BFP\n");
   LOADxxx("ltebr", 100.0f, float, "%f");
   LOADxxx("ltebr", -10.5f, float, "%f");
   LOADxxx("ltebr",   0.0f, float, "%f");
   LOADxxx("ltebr",   NAN,  float, "%f");
   LOADxxx("ltebr",  -NAN,  float, "%f");
   LOADxxx("ltebr",  INFINITY, float, "%f");
   LOADxxx("ltebr", -INFINITY, float, "%f");

   printf("LOAD AND TEST long BFP\n");
   LOADxxx("ltdbr", 100.0,  double, "%f");
   LOADxxx("ltdbr", -10.5,  double, "%f");
   LOADxxx("ltdbr",   0.0,  double, "%f");
   LOADxxx("ltdbr",   NAN,  double, "%f");
   LOADxxx("ltdbr",  INFINITY, double, "%f");
   LOADxxx("ltdbr", -INFINITY, double, "%f");

   printf("LOAD AND TEST extended BFP\n");
   LOADxxx("ltxbr", 100.0L, long double, "%Lf");
   LOADxxx("ltxbr", -10.5L, long double, "%Lf");
   LOADxxx("ltxbr",   0.0L, long double, "%Lf");
   LOADxxx("ltxbr",    NAN, long double, "%Lf");
   LOADxxx("ltxbr",  INFINITY, long double, "%Lf");
   LOADxxx("ltxbr", -INFINITY, long double, "%Lf");

   putchar('\n');
   printf("LOAD NEGATIVE short BFP\n");
   LOADxxx("lnebr",   0.0f, float, "%f");
   LOADxxx("lnebr",  -0.0f, float, "%f");
   LOADxxx("lnebr", 123.0f, float, "%f");
   LOADxxx("lnebr", -12.0f, float, "%f");
   LOADxxx("lnebr",    NAN, float, "%f");
   LOADxxx("lnebr",   -NAN, float, "%f");
   LOADxxx("lnebr",  INFINITY, float, "%f");
   LOADxxx("lnebr", -INFINITY, float, "%f");

   printf("LOAD NEGATIVE long BFP\n");
   LOADxxx("lndbr",   0.0, double, "%f");
   LOADxxx("lndbr",  -0.0, double, "%f");
   LOADxxx("lndbr", 123.0, double, "%f");
   LOADxxx("lndbr", -12.0, double, "%f");
   LOADxxx("lndbr",   NAN, double, "%f");
   LOADxxx("lndbr",  -NAN, double, "%f");
   LOADxxx("lndbr",  INFINITY, double, "%f");
   LOADxxx("lndbr", -INFINITY, double, "%f");

   printf("LOAD NEGATIVE extended BFP\n");
   LOADxxx("lnxbr",   0.0L, long double, "%Lf");
   LOADxxx("lnxbr",  -0.0L, long double, "%Lf");
   LOADxxx("lnxbr", 123.0L, long double, "%Lf");
   LOADxxx("lnxbr", -12.0L, long double, "%Lf");
   LOADxxx("lnxbr",    NAN, long double, "%Lf");
   LOADxxx("lnxbr",   -NAN, long double, "%Lf");
   LOADxxx("lnxbr",   INFINITY, long double, "%Lf");
   LOADxxx("lnxbr",  -INFINITY, long double, "%Lf");

   putchar('\n');
   printf("LOAD POSITIVE short BFP\n");
   LOADxxx("lpebr",   0.0f, float, "%f");
   LOADxxx("lpebr",  -0.0f, float, "%f");
   LOADxxx("lpebr", 123.0f, float, "%f");
   LOADxxx("lpebr", -12.0f, float, "%f");
   LOADxxx("lpebr",    NAN, float, "%f");
   LOADxxx("lpebr",   -NAN, float, "%f");
   LOADxxx("lpebr",  INFINITY, float, "%f");
   LOADxxx("lpebr", -INFINITY, float, "%f");

   printf("LOAD POSITIVE long BFP\n");
   LOADxxx("lpdbr",   0.0, double, "%f");
   LOADxxx("lpdbr",  -0.0, double, "%f");
   LOADxxx("lpdbr", 123.0, double, "%f");
   LOADxxx("lpdbr", -12.0, double, "%f");
   LOADxxx("lpdbr",   NAN, double, "%f");
   LOADxxx("lpdbr",  -NAN, double, "%f");
   LOADxxx("lpdbr",  INFINITY, double, "%f");
   LOADxxx("lpdbr", -INFINITY, double, "%f");

   printf("LOAD POSITIVE extended BFP\n");
   LOADxxx("lpxbr",   0.0L, long double, "%Lf");
   LOADxxx("lpxbr",  -0.0L, long double, "%Lf");
   LOADxxx("lpxbr", 123.0L, long double, "%Lf");
   LOADxxx("lpxbr", -12.0L, long double, "%Lf");
   LOADxxx("lpxbr",    NAN, long double, "%Lf");
   LOADxxx("lpxbr",   -NAN, long double, "%Lf");
   LOADxxx("lpxbr",   INFINITY, long double, "%Lf");
   LOADxxx("lpxbr",  -INFINITY, long double, "%Lf");

   putchar('\n');
   printf("LOAD COMPLEMENT short BFP\n");
   LOADxxx("lcebr",   0.0f, float, "%f");
   LOADxxx("lcebr",  -0.0f, float, "%f");
   LOADxxx("lcebr", 123.0f, float, "%f");
   LOADxxx("lcebr", -12.0f, float, "%f");
   LOADxxx("lcebr",    NAN, float, "%f");
   LOADxxx("lcebr",   -NAN, float, "%f");
   LOADxxx("lcebr",  INFINITY, float, "%f");
   LOADxxx("lcebr", -INFINITY, float, "%f");

   printf("LOAD COMPLEMENT long BFP\n");
   LOADxxx("lcdbr",   0.0, double, "%f");
   LOADxxx("lcdbr",  -0.0, double, "%f");
   LOADxxx("lcdbr", 123.0, double, "%f");
   LOADxxx("lcdbr", -12.0, double, "%f");
   LOADxxx("lcdbr",   NAN, double, "%f");
   LOADxxx("lcdbr",  -NAN, double, "%f");
   LOADxxx("lcdbr",  INFINITY, double, "%f");
   LOADxxx("lcdbr", -INFINITY, double, "%f");

   printf("LOAD COMPLEMENT extended BFP\n");
   LOADxxx("lcxbr",   0.0L, long double, "%Lf");
   LOADxxx("lcxbr",  -0.0L, long double, "%Lf");
   LOADxxx("lcxbr", 123.0L, long double, "%Lf");
   LOADxxx("lcxbr", -12.0L, long double, "%Lf");
   LOADxxx("lcxbr",    NAN, long double, "%Lf");
   LOADxxx("lcxbr",   -NAN, long double, "%Lf");
   LOADxxx("lcxbr",   INFINITY, long double, "%Lf");
   LOADxxx("lcxbr",  -INFINITY, long double, "%Lf");

   putchar('\n');
   printf("LOAD ZERO\n");
   lzxr();

   return 0;
}
