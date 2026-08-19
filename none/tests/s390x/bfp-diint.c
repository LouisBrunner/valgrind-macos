/* Test "divide to integer" instructions. */

#include <math.h>
#include <stdio.h>

#define GEN_DIXBR(insn, type, m4)                                              \
   static void insn##_##m4(type dividend, type divisor)                        \
   {                                                                           \
      type     op1 = dividend;                                                 \
      type     op2 = divisor;                                                  \
      type     op3;                                                            \
      unsigned cc;                                                             \
                                                                               \
      __asm__(#insn " %[r1],%[r3],%[r2]," #m4 "\n\t"                           \
                    "ipm   %[cc]\n\t"                                          \
                    "srl   %[cc],28\n\t"                                       \
              : [r1] "+&f"(op1), [r3] "=&f"(op3), [cc] "=d"(cc)                \
              : [r2] "f"(op2)                                                  \
              : "cc");                                                         \
      printf("\t%g / %g = %g  rem=%g  cc=%u\n", dividend, divisor, op3, op1,   \
             cc);                                                              \
   }

GEN_DIXBR(diebr, float, 5);
GEN_DIXBR(didbr, double, 1);
GEN_DIXBR(didbr, double, 4);
GEN_DIXBR(didbr, double, 5);
GEN_DIXBR(didbr, double, 6);

static const double data[][2] = {{7, 2},        {-6, 2},     {-5, INFINITY},
                                 {INFINITY, 3}, {5, 0},      {NAN, 3},
                                 {3, NAN},      {1.5e-45, 1}, {5, 1e-40}};

enum { n_data = sizeof(data) / sizeof(data[0]) };

int main(void)
{
   puts("diebr (m4=5)");
   for (int i = 0; i < n_data; i++)
      diebr_5(data[i][0], data[i][1]);

   puts("didbr (m4=5)");
   for (int i = 0; i < n_data; i++)
      didbr_5(data[i][0], data[i][1]);

   puts("didbr (m4=1)");        /* ties away from zero */
   didbr_1(5, 2);
   didbr_1(-5, 2);
   puts("didbr (m4=4)");        /* ties to even */
   didbr_4(7, 4);
   didbr_4(5, 2);
   puts("didbr (m4=6)");        /* toward +inf */
   didbr_6(5, 2);
   didbr_6(5, -2);

   return 0;
}
