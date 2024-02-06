#include "dfp_utils.h"
#include <stdio.h>

/* Following instructions are tested:
test data class tests for
   _Decimal32  - TDCET
   _Decimal64  - TDCDT
   _decimal128 - TDCXT
test data group tests for
   _Decimal32  - TDGET
   _Decimal64  - TDGDT
   _decimal128 - TDGXT
*/

static const pun_d32 d32_vals[] = {
   {0x22400000}, /* +0.0DF */
   {0xa2400000}, /* -0.0DF */
   {0x22400022}, /* +2.2DF */
   {0xa2400022}, /* -2.2DF */
   {0x78000000}, /* DEC_INFINITY */
   {0xf8000000}, /* -DEC_INFINITY */
   {0x7c000000}, /* +DEC_NAN */
   {0xfc000000}, /* -DEC_NAN */
};

static const pun_d64 d64_vals[] = {
   {0x2234000000000000}, {0xa234000000000000}, {0x2234000000000022},
   {0xa234000000000022}, {0x7800000000000000}, {0xf800000000000000},
   {0x7c00000000000000}, {0xfc00000000000000},
};

static const pun_d128 d128_vals[] = {
   {{0x2207c00000000000, 0x0000000000000000}},
   {{0xa207c00000000000, 0x0000000000000000}},
   {{0x2207c00000000000, 0x0000000000000022}},
   {{0xa207c00000000000, 0x0000000000000022}},
   {{0x7800000000000000, 0x0000000000000000}},
   {{0xf800000000000000, 0x0000000000000000}},
   {{0x7c00000000000000, 0x0000000000000000}},
   {{0xfc00000000000000, 0x0000000000000000}},
};

#define TEST(opcode, ty, n)                                                    \
   ({                                                                          \
      const long num = n;                                                      \
      for (unsigned j = 0; j != sizeof(ty##_vals) / sizeof(ty##_vals[0]);      \
           j++) {                                                              \
         int cc;                                                               \
         asm(".insn rxe, " opcode ", %[dec],0(%[bits])\n\t"                    \
             "ipm %[cc]\n"                                                     \
             "srl %[cc],28"                                                    \
             : [cc] "=d"(cc)                                                   \
             : [dec] "f"(ty##_vals[j].f), [bits] "a"(num)                      \
             : "cc");                                                          \
         printf("%d", cc);                                                     \
      }                                                                        \
   })

int main()
{
   int i;

   /* The rightmost 12 bits 52:63 of the second operand are set and tested */
   for (i = 0; i < 12; i++) {
      TEST("0xed0000000058", d128, 1UL << i); /* TDCXT */
      TEST("0xed0000000059", d128, 1UL << i); /* TDGXT */
      TEST("0xed0000000054", d64, 1UL << i);  /* TDCDT */
      TEST("0xed0000000055", d64, 1UL << i);  /* TDGDT */
      TEST("0xed0000000050", d32, 1UL << i);  /* TDCET */
      TEST("0xed0000000051", d32, 1UL << i);  /* TDGET */
      printf("\n");
   }
   return 0;
}
