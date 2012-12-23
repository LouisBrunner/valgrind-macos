#include <stdio.h>
#include <sys/types.h>
#include "dfp_utils.h"

volatile _Decimal32 d32_1, d32_2;
volatile _Decimal64 d64_1, d64_2;
volatile _Decimal128 d128_1, d128_2;

int main(void)
{
   d64_1 = 5.000005DD;
   d64_2 = 60000.4DD;
   d32_1 = 3.000002DF;
   d32_2 = 500000.000005DF;
   d128_1 = 100000000.000028DL;

   d64_1 = (_Decimal64) d32_1; //Exercise LDETR (load lengthened)
   printf("D32 -> D64  : ");
   DFP_VAL_PRINT(d32_1, _Decimal32);
   printf(" -> ");
   DFP_VAL_PRINT(d64_1, _Decimal64);

   d128_2 = (_Decimal128) d64_2; //Exercise LXDTR (load lengthened)
   printf("\nD64 -> D128 : ");
   DFP_VAL_PRINT(d64_2, _Decimal64);
   printf(" -> ");
   DFP_VAL_PRINT(d128_2, _Decimal128);

   d32_2 = (_Decimal32) d64_2; //Exercise LEDTR (load rounded)
   printf("\nD64 -> D32  : ");
   DFP_VAL_PRINT(d64_2, _Decimal64);
   printf(" -> ");
   DFP_VAL_PRINT(d32_2, _Decimal32);

   d64_2 = (_Decimal64) d128_1; //Exercise LDXTR (load rounded)
   printf("\nD128 -> D64 : ");
   DFP_VAL_PRINT(d128_1, _Decimal128);
   printf(" -> ");
   DFP_VAL_PRINT(d64_2, _Decimal64);
   printf("\n");

   return 0;
}
