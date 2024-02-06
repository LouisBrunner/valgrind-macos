#include "dfp_utils.h"
#include <stdio.h>

#define CONV(opc, from, to, val)                                               \
   {                                                                           \
      pun_d##to out;                                                           \
      printf("%-11s : ", "D" #from " -> D" #to);                               \
      DFP_VAL_PRINT(val, pun_d##from);                                         \
      asm(".insn rrf," opc "0000,%[r1],%[r2],0,0"                              \
          : [r1] "=f"(out.f)                                                   \
          : [r2] "f"(val.f));                                                  \
      printf(" -> ");                                                          \
      DFP_VAL_PRINT(out, pun_d##to);                                           \
      putchar('\n');                                                           \
   }

static const pun_d64 dd_A = {0x22340000000c0004}; /* 60000.4DD */
static const pun_d32 df_B = {0x2df00002};         /* 3.000002DF  */

/* 100000000.000028DL */
static const pun_d128 dl_C = {{0x2206800000000000, 0x0000800000000028}};

int main(void)
{
   CONV("0xb3d4", 32, 64, df_B);  /* LDETR (load lengthened) */
   CONV("0xb3dc", 64, 128, dd_A); /* LXDTR (load lengthened) */
   CONV("0xb3d5", 64, 32, dd_A);  /* LEDTR (load rounded) */
   CONV("0xb3dd", 128, 64, dl_C); /* LDXTR (load rounded) */
   return 0;
}
