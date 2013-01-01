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

void sldt(_Decimal64 in, unsigned long amount)
{
  _Decimal64 out;
  int *shift = (int *) amount;

  asm volatile (".insn rxf, 0xed0000000040, %[out], %[in], 0(%[amount])\n\t"
                :[out]"=f"(out)
                :[in]"f"(in),[amount]"a"(shift));

  printf("SLDT ");
  DFP_VAL_PRINT(in, _Decimal64);
  printf(" -> ");
  DFP_VAL_PRINT(out, _Decimal64);
  printf("\n");
}

void slxt(_Decimal128 in, unsigned long amount)
{
  _Decimal128 out;
  int *shift = (int *) amount;

  asm volatile (".insn rxf, 0xed0000000048, %[out], %[in], 0(%[amount])\n\t"
                :[out]"=f"(out)
                :[in]"f"(in),[amount]"a"(shift));

  printf("SLXT ");
  DFP_VAL_PRINT(in, _Decimal128);
  printf(" -> ");
  DFP_VAL_PRINT(out, _Decimal128);
  printf("\n");
}

void srdt(_Decimal64 in, unsigned long amount)
{
  _Decimal64 out;
  int *shift = (int *) amount;

  asm volatile (".insn rxf, 0xed0000000041, %[out], %[in], 0(%[amount])\n\t"
                :[out]"=f"(out)
                :[in]"f"(in),[amount]"a"(shift));

  printf("SRDT ");
  DFP_VAL_PRINT(in, _Decimal64);
  printf(" -> ");
  DFP_VAL_PRINT(out, _Decimal64);
  printf("\n");
}

void srxt(_Decimal128 in, unsigned long amount)
{
  _Decimal128 out;
  int *shift = (int *) amount;

  asm volatile (".insn rxf, 0xed0000000049, %[out], %[in], 0(%[amount])\n\t"
                :[out]"=f"(out)
                :[in]"f"(in),[amount]"a"(shift));

  printf("SRXT ");
  DFP_VAL_PRINT(in, _Decimal128);
  printf(" -> ");
  DFP_VAL_PRINT(out, _Decimal128);
  printf("\n");
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

  d64 = 12345678.54321DD;
  sldt(d64, 10);
  sldt(-d64, 2);
  sldt(0.DD, 2);
  sldt(-0.DD, 2);

  srdt(d64, 5);
  srdt(-d64, 2);
  srdt(0.DD, 2);
  srdt(-0.DD, 2);

  d128 = 12345678.54321DL;
  slxt(d128, 10);
  slxt(-d128, 2);
  slxt(0.DL, 2);
  slxt(-0.DL, 2);

  srxt(d128, 10);
  srxt(-d128, 2);
  srxt(0.DL, 2);
  srxt(-0.DL, 2);

  return 0;
}
