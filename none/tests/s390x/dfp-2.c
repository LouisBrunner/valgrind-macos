#include <stdio.h>
#include "dfp_utils.h"

/* Test various DFP ops:
   - extract significance 64/128 bit
   - load and test 64/128 bit
*/

void esdtr(_Decimal64 in)
{
  long out;
  asm volatile(".insn rre, 0xb3e70000, %[out], %[in]\n\t"
               :[out] "=d" (out) :[in] "f" (in));
  printf("ESDTR ");
  DFP_VAL_PRINT(in, _Decimal64);
  printf(" -> %ld\n", out);
}

void esxtr(_Decimal128 in)
{
  long out;
  asm volatile(".insn rre, 0xb3ef0000, %[out], %[in]\n\t"
               :[out] "=d" (out) :[in] "f" (in));
  printf("ESXTR ");
  DFP_VAL_PRINT(in, _Decimal128);
  printf(" -> %ld\n", out);
}

void ltdtr(_Decimal64 in)
{
  _Decimal64 out;
  int cc;
  asm volatile(".insn rre, 0xb3d60000, %[out], %[in]\n\t"
               "ipm %1\n\t"
               "srl %1,28\n\t"
               :[out] "=d" (out), "=d" (cc)
               :[in] "f" (in));
  printf("LTDTR ");
  DFP_VAL_PRINT(in, _Decimal64);
  printf(" -> %d\n", cc);
}

void ltxtr(_Decimal128 in)
{
  _Decimal128 out;
  int cc;
  asm volatile(".insn rre, 0xb3de0000, %[out], %[in]\n\t"
               "ipm %1\n\t"
               "srl %1,28\n\t"
               :[out] "=f" (out), "=d" (cc)
               :[in] "f" (in));
  printf("LTXTR ");
  DFP_VAL_PRINT(in, _Decimal128);
  printf(" -> %d\n", cc);
}

int main() {
  _Decimal64 d64 = 50.0005DD;
  _Decimal128 d128 = 50.0005DL;

  esdtr(d64);
  esdtr(-d64);
  esdtr(0.DD);
  esxtr(d128);
  esxtr(-d128);
  esxtr(0.DL);

  ltdtr(d64);
  ltdtr(-d64);
  ltdtr(0.0DD);
  ltxtr(d128);
  ltxtr(-d128);
  ltxtr(0.0DL);

  return 0;
}
