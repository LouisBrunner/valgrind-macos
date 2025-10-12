/* TEST DATA CLASS

   Test for subnormal numbers is missing. I do not know how */
#include <stdio.h>
#include <math.h>      // NAN
#include <float.h>     // DBL_MIN
#include <assert.h>

#define ZERO_P    (1 << 11)
#define ZERO_N    (1 << 10)
#define NORM_P    (1 << 9)
#define NORM_N    (1 << 8)
#define SUBNORM_P (1 << 7)
#define SUBNORM_N (1 << 6)
#define INF_P     (1 << 5)
#define INF_N     (1 << 4)
#define QNAN_P    (1 << 3)
#define QNAN_N    (1 << 2)
#define SNAN_P    (1 << 1)
#define SNAN_N    (1 << 0)

static struct what {
   unsigned bitno;
   unsigned mask;
   const char *str;
} whatsit[] = {
   { 63, SNAN_N,    "Signaling Nan with sign bit negative" },
   { 62, SNAN_P,    "Signaling Nan with sign bit posative" },
   { 61, QNAN_N,    "Quiet Nan with sign bit negative" },
   { 60, QNAN_P,    "Quiet Nan with sign bit posative" },
   { 59, INF_N,     "Infinity with sign bit negative" },
   { 58, INF_P,     "Infinity with sign bit positive" },
   { 57, SUBNORM_N, "Subnormal number with sign bit negative" },
   { 56, SUBNORM_N, "Subnormal number with sign bit positive" },
   { 55, NORM_N,    "Normal number with sign bit negative" },
   { 54, NORM_P,    "Normal number with sign bit positive" },
   { 53, ZERO_N,    "Zero with sign bit negative" },
   { 52, ZERO_P,    "Zero with sign bit positive" }
};

#define TDC(insn, type, value, m)                       \
   ({                                                   \
      int cc;                                           \
      type r1 = value;                                  \
                                                        \
      __asm__ volatile(#insn " %[r1],0(%[mask])\n\t"    \
                       "ipm  %[psw]\n\t"                \
                       "srl  %[psw],28\n\t"             \
                       : [psw]"+d"(cc)                  \
                       : [r1]"f"(r1), [mask]"a"(m)   \
                       : "cc");                         \
      cc;                                               \
   })

static void
do_tceb(float value)
{
   for (int i = 0; i < sizeof(whatsit) / sizeof(*whatsit); ++i) {
      int ccv = TDC(tceb, float, value, whatsit[i].mask);
      if (ccv == 1)
         printf("value = %f\t\tcc = %d   %s\n", value, ccv,
                whatsit[i].str);
   }
}

static void
do_tcdb(double value)
{
   for (int i = 0; i < sizeof(whatsit) / sizeof(*whatsit); ++i) {
      int ccv = TDC(tcdb, double, value, whatsit[i].mask);
      if (ccv == 1)
         printf("value = %f\t\tcc = %d   %s\n", value, ccv,
                whatsit[i].str);
   }
}

static void
do_tcxb(long double value)
{
   for (int i = 0; i < sizeof(whatsit) / sizeof(*whatsit); ++i) {
      int ccv = TDC(tcxb, long double, value, whatsit[i].mask);
      if (ccv == 1)
         printf("value = %Lf\t\tcc = %d   %s\n", value, ccv,
                whatsit[i].str);
   }
}

int
main(void)
{
   assert(sizeof(long double) == 16);

   printf("32-bit tests\n");
   do_tceb(0.0f);
   do_tceb(-0.0f);
   do_tceb(3.0f);
   do_tceb(-3.0f);
   do_tceb(NAN);
   do_tceb(-NAN);
   do_tceb(INFINITY);
   do_tceb(-INFINITY);
   do_tceb(__builtin_nansf(""));
   do_tceb(-__builtin_nansf(""));
   do_tceb(FLT_MIN / 2);
   do_tceb(-FLT_MIN / 2);

   printf("\n64-bit tests\n");
   do_tcdb(0.0);
   do_tcdb(-0.0);
   do_tcdb(3.0);
   do_tcdb(-3.0);
   do_tcdb(NAN);
   do_tcdb(-NAN);
   do_tcdb(INFINITY);
   do_tcdb(-INFINITY);
   do_tcdb(__builtin_nans(""));
   do_tcdb(-__builtin_nans(""));
   do_tcdb(DBL_MIN / 2);
   do_tcdb(-DBL_MIN / 2);

   printf("\n128-bit tests\n");
   do_tcxb(0.0L);
   do_tcxb(-0.0L);
   do_tcxb(3.0L);
   do_tcxb(-3.0L);
   do_tcxb(NAN);
   do_tcxb(-NAN);
   do_tcxb(INFINITY);
   do_tcxb(-INFINITY);
   do_tcxb(__builtin_nansl(""));
   do_tcxb(-__builtin_nansl(""));
#if 0
   /* Figuring out subnormal numbers for 128-bit BFP is left as
      an exercise.. */
   do_tcdb(LDBL_MIN / 2);
   do_tcdb(-LDBL_MIN / 2);
#endif

   return 0;
}
