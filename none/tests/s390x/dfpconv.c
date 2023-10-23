#include "dfp_utils.h"
#include <stdio.h>

#define DO_PRINT_I2D(insn, l, type, round)                                     \
   ({                                                                          \
      long source = l;                                                         \
      type d;                                                                  \
      printf(#insn " round=%d %ld -> ", round, source);                        \
      asm(".insn rrf," insn() "0000,%[r1],%[r2],%[m],0"                        \
          : [r1] "=f"(d.f)                                                     \
          : [r2] "d"(source), [m] "i"(round));                                 \
      DFP_VAL_PRINT(d, type);                                                  \
      printf("\n");                                                            \
   })

#define DO_INSN_I2D(insn, round, type)                                         \
   ({                                                                          \
      DO_PRINT_I2D(insn, 0, type, round);                                      \
      DO_PRINT_I2D(insn, 1, type, round);                                      \
      DO_PRINT_I2D(insn, 0xffffffff, type, round);                             \
      DO_PRINT_I2D(insn, 0x80000000, type, round);                             \
      DO_PRINT_I2D(insn, 0x7fffffff, type, round);                             \
   })

#define DO_PRINT_D2I(insn, d, type, round)                                     \
   ({                                                                          \
      long target = 0;                                                         \
      int  cc;                                                                 \
      printf(#insn " round=%d ", round);                                       \
      DFP_VAL_PRINT(d, type);                                                  \
      asm(".insn rrf," insn() "0000,%[r1],%[r2],%[m],0\n\t"                    \
                              "ipm %[c]\n\t"                                   \
                              "srl %[c],28\n\t"                                \
          : [r1] "+d"(target), [c] "=d"(cc)                                    \
          : [r2] "f"(d.f), [m] "i"(round)                                      \
          : "cc");                                                             \
      printf(" -> %ld cc=%d\n", target, cc);                                   \
   })

static const pun_d64 d64_vals[] = {
   {0xa234000000000011}, /* -1.1DD */
   {0x2238000000000000}, /* 0.DD */
   {0x2238000000000001}, /* 1.DD */
   {0x2234000000000014}, /* 1.4DD */
   {0x2234000000000015}, /* 1.5DD */
   {0x2234000000000016}, /* 1.6DD */
   {0x2244000000000016}, /* 1.6E+4DD */
   {0x2254000000000016}, /* 1.6E+8DD */
   {0x2264000000000016}, /* 1.6E+12DD */
   {0x2284000000000016}, /* 1.6E+20DD */
   {0x4154000000000016}, /* 1.6E+200DD */
   {0x2224000000000016}, /* 1.6E-4DD */
   {0x20bc000000000001}, /* DEC32_MIN */
   {0x23a000000093fcff}, /* DEC32_MAX */
   {0x003c000000000001}, /* DEC64_MIN */
   {0x77fcff3fcff3fcff}, /* DEC64_MAX */
};

static const pun_d128 d128_vals[] = {
   {{0xa207c00000000000, 0x0000000000000011}},
   {{0x2208000000000000, 0x0000000000000000}},
   {{0x2208000000000000, 0x0000000000000001}},
   {{0x2207c00000000000, 0x0000000000000014}},
   {{0x2207c00000000000, 0x0000000000000015}},
   {{0x2207c00000000000, 0x0000000000000016}},
   {{0x2208c00000000000, 0x0000000000000016}},
   {{0x2209c00000000000, 0x0000000000000016}},
   {{0x220ac00000000000, 0x0000000000000016}},
   {{0x220cc00000000000, 0x0000000000000016}},
   {{0x2239c00000000000, 0x0000000000000016}},
   {{0x2206c00000000000, 0x0000000000000016}},
   {{0x21f0400000000000, 0x0000000000000001}},
   {{0x221e800000000000, 0x000000000093fcff}},
   {{0x21a8400000000000, 0x0000000000000001}},
   {{0x2264400000000000, 0x0024ff3fcff3fcff}},
};

#define DO_INSN_D2I(insn, round, t)                                            \
   ({                                                                          \
      for (unsigned j = 0; j < sizeof(t##_vals) / sizeof(t##_vals[0]); j++) {  \
         DO_PRINT_D2I(insn, t##_vals[j], pun_##t, round);                      \
      }                                                                        \
   })

#define DO_D2I(round)                                                          \
   ({                                                                          \
      DO_INSN_D2I(CGDTRA, round, d64);                                         \
      DO_INSN_D2I(CGXTRA, round, d128);                                        \
   })

#define CGDTRA() "0xb3e1"
#define CGXTRA() "0xb3e9"
#define CDGTRA() "0xb3f1"
#define CXGTRA() "0xb3f9"

int main()
{
   /* rounding mode is not used for the I64 -> D128 conversion */
   DO_INSN_I2D(CXGTRA, 0, pun_d128);

   /* Omit rounding mode value 0 and 2 as the current DFP rounding
      mode is chosen for these values. */
   DO_INSN_I2D(CDGTRA, 1, pun_d64);
   DO_D2I(1);

   DO_INSN_I2D(CDGTRA, 3, pun_d64);
   DO_D2I(3);

   DO_INSN_I2D(CDGTRA, 4, pun_d64);
   DO_D2I(4);

   DO_INSN_I2D(CDGTRA, 5, pun_d64);
   DO_D2I(5);

   DO_INSN_I2D(CDGTRA, 6, pun_d64);
   DO_D2I(6);

   DO_INSN_I2D(CDGTRA, 7, pun_d64);
   DO_D2I(7);

   DO_INSN_I2D(CDGTRA, 8, pun_d64);
   DO_D2I(8);

   DO_INSN_I2D(CDGTRA, 9, pun_d64);
   DO_D2I(9);

   DO_INSN_I2D(CDGTRA, 10, pun_d64);
   DO_D2I(10);

   DO_INSN_I2D(CDGTRA, 11, pun_d64);
   DO_D2I(11);

   DO_INSN_I2D(CDGTRA, 12, pun_d64);
   DO_D2I(12);

   DO_INSN_I2D(CDGTRA, 13, pun_d64);
   DO_D2I(13);

   DO_INSN_I2D(CDGTRA, 14, pun_d64);
   DO_D2I(14);

   DO_INSN_I2D(CDGTRA, 15, pun_d64);
   DO_D2I(15);

   return 0;
}
