#include "dfp_utils.h"
#include <stdio.h>

#define DO_PRINT_I2D(insn, l, type, round)                                     \
   ({                                                                          \
      int  source = l;                                                         \
      type d;                                                                  \
      printf(#insn " round=%d %d -> ", round, source);                         \
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
      DO_PRINT_I2D(insn, -1, type, round);                                     \
      DO_PRINT_I2D(insn, -2147483648, type, round);                            \
      DO_PRINT_I2D(insn, 0x7fffffff, type, round);                             \
   })

#define DO_PRINT_L2D(insn, l, type, round)                                     \
   ({                                                                          \
      unsigned long source = l;                                                \
      type          d;                                                         \
      printf(#insn " round=%d %lu -> ", round, source);                        \
      asm(".insn rrf," insn() "0000,%[r1],%[r2],%[m],0"                        \
          : [r1] "=f"(d.f)                                                     \
          : [r2] "d"(source), [m] "i"(round));                                 \
      DFP_VAL_PRINT(d, type);                                                  \
      printf("\n");                                                            \
   })

#define DO_INSN_L2D(insn, round, type)                                         \
   ({                                                                          \
      DO_PRINT_L2D(insn, 0UL, type, round);                                    \
      DO_PRINT_L2D(insn, 1UL, type, round);                                    \
      DO_PRINT_L2D(insn, 0xffffffffUL, type, round);                           \
      DO_PRINT_L2D(insn, 0x80000000UL, type, round);                           \
      DO_PRINT_L2D(insn, 0x7fffffffUL, type, round);                           \
      DO_PRINT_L2D(insn, 0x100000000UL, type, round);                          \
      DO_PRINT_L2D(insn, 0xffffffffffffffffUL, type, round);                   \
      DO_PRINT_L2D(insn, 0x8000000000000000UL, type, round);                   \
      DO_PRINT_L2D(insn, 0x7fffffffffffffffUL, type, round);                   \
   })

#define DO_PRINT_D2I(insn, d, type, round, ity, fmt)                           \
   ({                                                                          \
      ity target = 0;                                                          \
      int cc;                                                                  \
      printf(#insn " round=%d ", round);                                       \
      DFP_VAL_PRINT(d, type);                                                  \
      asm(".insn rrf," insn() "0000,%[r1],%[r2],%[m],0\n\t"                    \
                              "ipm %[c]\n\t"                                   \
                              "srl %[c],28\n\t"                                \
          : [r1] "+d"(target), [c] "=d"(cc)                                    \
          : [r2] "f"(d.f), [m] "i"(round)                                      \
          : "cc");                                                             \
      printf(" -> " fmt " cc=%d\n", target, cc);                               \
   })

static const pun_d64 d64_vals[] = {
   {0xa234000000000011}, /* -1.DD */
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

#define DO_INSN_D2I(insn, round, t, ity, fmt)                                  \
   ({                                                                          \
      for (unsigned j = 0; j < sizeof(t##_vals) / sizeof(t##_vals[0]); j++) {  \
         DO_PRINT_D2I(insn, t##_vals[j], pun_##t, round, ity, fmt);            \
      }                                                                        \
   })

#define CLFDTR() "0xb943"
#define CLGDTR() "0xb942"
#define CLFXTR() "0xb94b"
#define CLGXTR() "0xb94a"
#define CFDTR()  "0xb941"
#define CFXTR()  "0xb949"

#define CDFTR()  "0xb951"
#define CXFTR()  "0xb959"
#define CDLFTR() "0xb953"
#define CXLFTR() "0xb95b"
#define CDLGTR() "0xb952"
#define CXLGTR() "0xb95a"

#define DO_D2L(round)                                                          \
   ({                                                                          \
      DO_INSN_D2I(CLFDTR, round, d64, unsigned, "%u");                         \
      DO_INSN_D2I(CLGDTR, round, d64, unsigned long, "%lu");                   \
      DO_INSN_D2I(CFDTR, round, d64, int, "%d");                               \
      DO_INSN_D2I(CLFXTR, round, d128, unsigned, "%u");                        \
      DO_INSN_D2I(CLGXTR, round, d128, unsigned long, "%lu");                  \
      DO_INSN_D2I(CFXTR, round, d128, int, "%d");                              \
   })

int main()
{
   /* rounding mode is not used for the following insns */
   DO_INSN_I2D(CDFTR, 0, pun_d64);
   DO_INSN_I2D(CXFTR, 0, pun_d128);
   DO_INSN_L2D(CDLFTR, 0, pun_d64);
   DO_INSN_L2D(CXLFTR, 0, pun_d128);
   DO_INSN_L2D(CXLGTR, 0, pun_d128);

   /* Omit rounding mode value 0 and 2 as the current DFP rounding
      mode is chosen for these values. */
   DO_INSN_L2D(CDLGTR, 1, pun_d64);
   DO_D2L(1);

   DO_INSN_L2D(CDLGTR, 3, pun_d64);
   DO_D2L(3);

   DO_INSN_L2D(CDLGTR, 4, pun_d64);
   DO_D2L(4);

   DO_INSN_L2D(CDLGTR, 5, pun_d64);
   DO_D2L(5);

   DO_INSN_L2D(CDLGTR, 6, pun_d64);
   DO_D2L(6);

   DO_INSN_L2D(CDLGTR, 7, pun_d64);
   DO_D2L(7);

   DO_INSN_L2D(CDLGTR, 8, pun_d64);
   DO_D2L(8);

   DO_INSN_L2D(CDLGTR, 9, pun_d64);
   DO_D2L(9);

   DO_INSN_L2D(CDLGTR, 10, pun_d64);
   DO_D2L(10);

   DO_INSN_L2D(CDLGTR, 11, pun_d64);
   DO_D2L(11);

   DO_INSN_L2D(CDLGTR, 12, pun_d64);
   DO_D2L(12);

   DO_INSN_L2D(CDLGTR, 13, pun_d64);
   DO_D2L(13);

   DO_INSN_L2D(CDLGTR, 14, pun_d64);
   DO_D2L(14);

   DO_INSN_L2D(CDLGTR, 15, pun_d64);
   DO_D2L(15);

   return 0;
}
