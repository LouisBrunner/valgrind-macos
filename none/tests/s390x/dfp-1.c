#include "dfp_utils.h"
#include <stdio.h>

#define DFP_BINOP(type, opc, binop, op1, op2, round)                           \
   ({                                                                          \
      int        cc;                                                           \
      reg_##type result;                                                       \
      asm("cr 0,0\n\t" /* clear CC */                                          \
          ".insn rrf," #opc "0000,%[res],%[r1],%[r2],%[mode]\n\t"              \
          "ipm %[cc]\n\t"                                                      \
          "srl %[cc],28\n\t"                                                   \
          : [res] "=f"(result), [cc] "=d"(cc)                                  \
          : [r1] "f"(op1.f), [r2] "f"(op2.f), [mode] "i"(round)                \
          : "cc");                                                             \
      DFP_VAL_PRINT(op1.f, reg_##type);                                        \
      printf(" " binop " ");                                                   \
      DFP_VAL_PRINT(op2.f, reg_##type);                                        \
      printf(" = ");                                                           \
      DFP_VAL_PRINT(result, reg_##type);                                       \
      printf(" cc = %d\n", cc);                                                \
   })

static const pun_d64 dd_3_14  = {0x2230000000000194}; /* 3.14DD */
static const pun_d64 dd_m3_14 = {0xa230000000000194}; /* -3.14DD */
static const pun_d64 dd_0_005 = {0x222c000000000005}; /* 0.005DD */
static const pun_d64 dd_7     = {0x2238000000000007}; /* 7.DD */
static const pun_d64 dd_0     = {0x2238000000000000}; /* 0.DD */
static const pun_d64 dd_22    = {0x2238000000000022}; /* 22.DD */
static const pun_d64 dd_m22   = {0xa238000000000022}; /* -22.DD */

static const pun_d128 dl_3_14  = {{0x2207800000000000, 0x0000000000000194}};
static const pun_d128 dl_m3_14 = {{0xa207800000000000, 0x0000000000000194}};
static const pun_d128 dl_0_005 = {{0x2207400000000000, 0x0000000000000005}};
static const pun_d128 dl_7     = {{0x2208000000000000, 0x0000000000000007}};
static const pun_d128 dl_0     = {{0x2208000000000000, 0x0000000000000000}};
static const pun_d128 dl_22    = {{0x2208000000000000, 0x0000000000000022}};
static const pun_d128 dl_m22   = {{0xa208000000000000, 0x0000000000000022}};

int main()
{
   printf("Decimal floating point arithmetic\n");

   printf("64-bit ADD\n");
   DFP_BINOP(d64, 0xb3d2, "+", dd_3_14, dd_0_005, 1);  /* cc = 2 */
   DFP_BINOP(d64, 0xb3d2, "+", dd_m3_14, dd_0_005, 1); /* cc = 1 */
   DFP_BINOP(d64, 0xb3d2, "+", dd_3_14, dd_m3_14, 3);  /* cc = 0 */

   printf("64-bit SUBTRACT\n");
   DFP_BINOP(d64, 0xb3d3, "-", dd_3_14, dd_0_005, 1);  /* cc = 2 */
   DFP_BINOP(d64, 0xb3d3, "-", dd_m3_14, dd_0_005, 1); /* cc = 1 */
   DFP_BINOP(d64, 0xb3d3, "-", dd_3_14, dd_3_14, 3);   /* cc = 0 */

   printf("64-bit MULTIPLY\n");
   DFP_BINOP(d64, 0xb3d0, "*", dd_3_14, dd_7, 6);
   DFP_BINOP(d64, 0xb3d0, "*", dd_m3_14, dd_7, 7);
   DFP_BINOP(d64, 0xb3d0, "*", dd_m3_14, dd_0, 7);

   printf("64-bit DIVIDE\n");
   DFP_BINOP(d64, 0xb3d1, "/", dd_22, dd_7, 13);
   DFP_BINOP(d64, 0xb3d1, "/", dd_m22, dd_7, 14);
   DFP_BINOP(d64, 0xb3d1, "/", dd_0, dd_7, 14);

   printf("128-bit ADD\n");
   DFP_BINOP(d128, 0xb3da, "+", dl_3_14, dl_0_005, 1);  /* cc = 2 */
   DFP_BINOP(d128, 0xb3da, "+", dl_m3_14, dl_0_005, 1); /* cc = 1 */
   DFP_BINOP(d128, 0xb3da, "+", dl_3_14, dl_m3_14, 1);  /* cc = 0 */

   printf("128-bit SUBTRACT\n");
   DFP_BINOP(d128, 0xb3db, "-", dl_3_14, dl_0_005, 1);  /* cc = 2 */
   DFP_BINOP(d128, 0xb3db, "-", dl_m3_14, dl_0_005, 1); /* cc = 1 */
   DFP_BINOP(d128, 0xb3db, "-", dl_3_14, dl_3_14, 1);   /* cc = 0 */

   printf("128-bit MULTIPLY\n");
   DFP_BINOP(d128, 0xb3d8, "*", dl_3_14, dl_7, 6);
   DFP_BINOP(d128, 0xb3d8, "*", dl_m3_14, dl_7, 7);
   DFP_BINOP(d128, 0xb3d8, "*", dl_3_14, dl_0, 7);

   printf("128-bit DIVIDE\n");
   DFP_BINOP(d128, 0xb3d9, "/", dl_22, dl_7, 13);
   DFP_BINOP(d128, 0xb3d9, "/", dl_m22, dl_7, 14);
   DFP_BINOP(d128, 0xb3d9, "/", dl_0, dl_7, 14);

   return 0;
}
