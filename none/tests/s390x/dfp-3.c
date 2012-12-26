#include <stdio.h>
#include <sys/types.h>
#include "opcodes.h"
#include "dfp_utils.h"

/* Following macros adopted from dfp/math.h from libdfp */
#define DEC_INFINITY    __builtin_infd64()
#define DEC_NAN         (0.0DF * DEC_INFINITY)

/* Test DFP value and exponent comparison for  64/128-bit. */

#define CMP_DFP(insn, op1, op2, type, cc)                               \
  ({                                                                    \
  register type d1 asm("f0") =  op1;                                    \
  register type d2 asm("f1") =  op2;                                    \
  /* cc = d1 (cmp) d2    */                                             \
  asm volatile(insn(0,1)                                                \
               "ipm %0\n\t"                                             \
               "srl %0,28\n\t"                                          \
               :"=d" (cc)                                               \
               :"f"(d1), "f"(d2)                                        \
               );                                                       \
  cc;                                                                   \
})


#define COMPARE(insn, v1, v2, type)                                     \
  {                                                                     \
    int cc;                                                             \
    CMP_DFP(insn, v1, v2, type, cc);                                    \
    DFP_VAL_PRINT(v1, type);                                            \
    switch (cc) {                                                       \
    case 0:                                                             \
      printf(" == ");                                                   \
      break;                                                            \
    case 1:                                                             \
      printf(" < ");                                                    \
      break;                                                            \
    case 2:                                                             \
      printf(" > ");                                                    \
      break;                                                            \
    case 3:                                                             \
      printf(" <> ");                                                   \
      break;                                                            \
    }                                                                   \
    DFP_VAL_PRINT(v2, type);                                            \
    printf(" (cc == %d)\n", cc);                                        \
}

int main(void)
{
   _Decimal64 d64_1, d64_2;
   _Decimal128 d128_1, d128_2;

   /* compare 8 bytes DFP value */
   printf("cdtr:\n");
   d64_1 = 5.000005DD;
   d64_2 = 50000000000000000.000005DD;
   COMPARE(CDTR, d64_1, d64_1, _Decimal64);
   COMPARE(CDTR, d64_1, d64_2, _Decimal64);
   COMPARE(CDTR, d64_2, d64_1, _Decimal64);

   /* compare NAN and INF operands */
   d64_1 = DEC_INFINITY;
   d64_2 = DEC_NAN;
   COMPARE(CDTR, d64_1, d64_2, _Decimal64);
   COMPARE(CDTR, d64_1, d64_1, _Decimal64);
   COMPARE(CDTR, d64_2, d64_2, _Decimal64);

   /* compare 16 bytes DFP value */
   printf("cxtr:\n");
   d128_1 = 5.00005DL;
   d128_2 = 5000000000000000.5DL;
   COMPARE(CXTR, d128_1, d128_1, _Decimal128);
   COMPARE(CXTR, d128_1, d128_2, _Decimal128);
   COMPARE(CXTR, d128_2, d128_1, _Decimal128);

   /* compare NAN and INF operands */
   d128_1 = DEC_INFINITY;
   d128_2 = DEC_NAN;
   COMPARE(CXTR, d128_1, d128_2, _Decimal128);
   COMPARE(CXTR, d128_1, d128_1, _Decimal128);
   COMPARE(CXTR, d128_2, d128_2, _Decimal128);

   /* compare exponents of 8 bytes DFP value */
   printf("cedtr:\n");
   d64_1 = 5.000005DD;
   d64_2 = 50000000000000000.000005DD;
   COMPARE(CEDTR, d64_1, d64_1, _Decimal64);
   COMPARE(CEDTR, d64_1, d64_2, _Decimal64);
   COMPARE(CEDTR, d64_2, d64_1, _Decimal64);

   /* compare NAN and INF operands */
   d64_1 = DEC_INFINITY;
   d64_2 = DEC_NAN;
   COMPARE(CEDTR, d64_1, d64_2, _Decimal64);
   COMPARE(CEDTR, d64_1, d64_1, _Decimal64);
   COMPARE(CEDTR, d64_2, d64_2, _Decimal64);

   /* compare exponents of 16 bytes DFP value */
   printf("cextr:\n");
   d128_1 = 5.00005DL;
   d128_2 = 5000000000000000.5DL;
   COMPARE(CEXTR, d128_1, d128_1, _Decimal128);
   COMPARE(CEXTR, d128_1, d128_2, _Decimal128);
   COMPARE(CEXTR, d128_2, d128_1, _Decimal128);

   /* compare NAN and INF operands */
   d128_1 = DEC_INFINITY;
   d128_2 = DEC_NAN;
   COMPARE(CEXTR, d128_1, d128_2, _Decimal128);
   COMPARE(CEXTR, d128_1, d128_1, _Decimal128);
   COMPARE(CEXTR, d128_2, d128_2, _Decimal128);

   return 0;
}
