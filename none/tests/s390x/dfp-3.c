#include "dfp_utils.h"
#include <stdio.h>

/* Test DFP value and exponent comparison for 64/128-bit. */

#define COMPARE(opc, v1, v2, type)                                             \
   {                                                                           \
      int cc;                                                                  \
      asm(".insn rre," opc "0000,%[r1],%[r2]\n\t"                              \
          "ipm %[cc]\n\t"                                                      \
          "srl %[cc],28\n\t"                                                   \
          : [cc] "=d"(cc)                                                      \
          : [r1] "f"(v1.f), [r2] "f"(v2.f)                                     \
          : "cc");                                                             \
      DFP_VAL_PRINT(v1, type);                                                 \
      switch (cc) {                                                            \
      case 0:                                                                  \
         printf(" == ");                                                       \
         break;                                                                \
      case 1:                                                                  \
         printf(" < ");                                                        \
         break;                                                                \
      case 2:                                                                  \
         printf(" > ");                                                        \
         break;                                                                \
      case 3:                                                                  \
         printf(" <> ");                                                       \
         break;                                                                \
      }                                                                        \
      DFP_VAL_PRINT(v2, type);                                                 \
      printf(" (cc == %d)\n", cc);                                             \
   }

/* 64-bit decimal constants */
static const pun_d64 dd_inf = {0x7800000000000000}; /* infinity */
static const pun_d64 dd_nan = {0x7c00000000000000}; /* NaN */
static const pun_d64 dd_A   = {0x2220000000500005}; /* 5.000005DD */
static const pun_d64 dd_B   = {0x363c000000000000}; /* 5.E16DD + 5.E-6DD */

/* 128-bit decimal constants */
static const pun_d128 dl_inf = {{0x7800000000000000, 0x0000000000000000}};
static const pun_d128 dl_nan = {{0x7c00000000000000, 0x0000000000000000}};
static const pun_d128 dl_A   = {{0x2206c00000000000, 0x00000000000a0005}};
static const pun_d128 dl_B   = {{0x2207c00000000000, 0x0140000000000005}};

#define CDTR  "0xb3e4"
#define CXTR  "0xb3ec"
#define CEDTR "0xb3f4"
#define CEXTR "0xb3fc"

int main(void)
{
   /* compare 8 bytes DFP value */
   printf("cdtr:\n");
   COMPARE(CDTR, dd_A, dd_A, pun_d64);
   COMPARE(CDTR, dd_A, dd_B, pun_d64);
   COMPARE(CDTR, dd_B, dd_A, pun_d64);
   COMPARE(CDTR, dd_inf, dd_nan, pun_d64);
   COMPARE(CDTR, dd_inf, dd_inf, pun_d64);
   COMPARE(CDTR, dd_nan, dd_nan, pun_d64);

   /* compare 16 bytes DFP value */
   printf("cxtr:\n");
   COMPARE(CXTR, dl_A, dl_A, pun_d128);
   COMPARE(CXTR, dl_A, dl_B, pun_d128);
   COMPARE(CXTR, dl_B, dl_A, pun_d128);
   COMPARE(CXTR, dl_inf, dl_nan, pun_d128);
   COMPARE(CXTR, dl_inf, dl_inf, pun_d128);
   COMPARE(CXTR, dl_nan, dl_nan, pun_d128);

   /* compare exponents of 8 bytes DFP value */
   printf("cedtr:\n");
   COMPARE(CEDTR, dd_A, dd_A, pun_d64);
   COMPARE(CEDTR, dd_A, dd_B, pun_d64);
   COMPARE(CEDTR, dd_B, dd_A, pun_d64);
   COMPARE(CEDTR, dd_inf, dd_nan, pun_d64);
   COMPARE(CEDTR, dd_inf, dd_inf, pun_d64);
   COMPARE(CEDTR, dd_nan, dd_nan, pun_d64);

   /* compare exponents of 16 bytes DFP value */
   printf("cextr:\n");
   COMPARE(CEXTR, dl_A, dl_A, pun_d128);
   COMPARE(CEXTR, dl_A, dl_B, pun_d128);
   COMPARE(CEXTR, dl_B, dl_A, pun_d128);
   COMPARE(CEXTR, dl_inf, dl_nan, pun_d128);
   COMPARE(CEXTR, dl_inf, dl_inf, pun_d128);
   COMPARE(CEXTR, dl_nan, dl_nan, pun_d128);

   return 0;
}
