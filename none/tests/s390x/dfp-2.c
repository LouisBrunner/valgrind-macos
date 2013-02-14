#include <stdio.h>
#include <stdint.h>
#include "dfp_utils.h"

/* Test various DFP ops:
   - extract biased exponent 64/128 bit
   - extract significance 64/128 bit
   - insert biased exponent 64/128 bit
   - load and test 64/128 bit
   - shift left/right 64/128 bit
   - reround 64/128 bit
*/

void eedtr(_Decimal64 in)
{
  long out;
  asm volatile(".insn rre, 0xb3e50000, %[out], %[in]\n\t"
               :[out] "=d" (out) :[in] "f" (in));
  printf("EEDTR ");
  DFP_VAL_PRINT(in, _Decimal64);
  printf(" -> %ld\n", out);
}

void eextr(_Decimal128 in)
{
  long out;
  asm volatile(".insn rre, 0xb3ed0000, %[out], %[in]\n\t"
               :[out] "=d" (out) :[in] "f" (in));
  printf("EEXTR ");
  DFP_VAL_PRINT(in, _Decimal128);
  printf(" -> %ld\n", out);
}

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

void iedtr(_Decimal64 in, long amount)
{
  _Decimal64 out;

  asm volatile (".insn rrf, 0xb3f60000, %[out], %[amount], %[in], 0\n\t"
                :[out]"=f"(out)
                :[in]"f"(in), [amount]"d"(amount));

  printf("IEDTR ");
  DFP_VAL_PRINT(in, _Decimal64);
  printf(", %ld -> ", amount);
  DFP_VAL_PRINT(out, _Decimal64);
  printf("\n");
}

void iextr(_Decimal128 in, long amount)
{
  _Decimal128 out;

  asm volatile (".insn rrf, 0xb3fe0000, %[out], %[amount], %[in], 0\n\t"
                :[out]"=f"(out)
                :[in]"f"(in), [amount]"d"(amount));

  printf("IEXTR ");
  DFP_VAL_PRINT(in, _Decimal128);
  printf(", %ld -> ", amount);
  DFP_VAL_PRINT(out, _Decimal128);
  printf("\n");
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

void qadtr(_Decimal64 op, _Decimal64 quan, uint8_t rm)
{
  _Decimal64 out;

  asm volatile (
                ".insn rrf, 0xb3f50000, %[out], %[quan], %[op], %[rm]\n\t"
                :[out]"=f"(out)
                :[op]"f"(op), [quan]"f"(quan), [rm]"d"(rm)
                );
  printf("QADTR ");
  DFP_VAL_PRINT(op, _Decimal64);
  printf(", ");
  DFP_VAL_PRINT(quan, _Decimal64);
  printf(", %x -> ", rm);
  DFP_VAL_PRINT(out, _Decimal64);
  printf("\n");
}

void quantize64(_Decimal64 op, _Decimal64 quan)
{
  uint8_t i;

  for (i = 0; i < 16; i++)
    qadtr(op, quan, i);
}

void qaxtr(_Decimal128 op, _Decimal128 quan, uint8_t rm)
{
  _Decimal128 out;

  asm volatile (
                ".insn rrf, 0xb3fd0000, %[out], %[quan], %[op], %[rm]\n\t"
                :[out]"=f"(out)
                :[op]"f"(op), [quan]"f"(quan), [rm]"d"(rm)
                );
  printf("QAXTR ");
  DFP_VAL_PRINT(op, _Decimal128);
  printf(", ");
  DFP_VAL_PRINT(quan, _Decimal128);
  printf(", %x -> ", rm);
  DFP_VAL_PRINT(out, _Decimal128);
  printf("\n");
}

void quantize128(_Decimal128 op, _Decimal128 quan)
{
  uint8_t i;

  for (i = 0; i < 16; i++)
    qaxtr(op, quan, i);
}

void rrdtr(_Decimal64 op, uint8_t sig, uint8_t rm)
{
  _Decimal64 out;

  asm volatile (
                ".insn rrf, 0xb3f70000, %[out], %[sig], %[op], %[rm]\n\t"
                :[out]"=f"(out)
                :[op]"f"(op), [sig]"d"(sig), [rm]"d"(rm)
                );
  printf("RRDTR ");
  DFP_VAL_PRINT(op, _Decimal64);
  printf(", %d, %x -> ", sig, rm);
  DFP_VAL_PRINT(out, _Decimal64);
  printf("\n");
}

void reround64(_Decimal64 op, uint8_t sig)
{
  uint8_t i;

  for (i = 0; i < 16; i++)
    rrdtr(op, sig, i);
}

void rrxtr(_Decimal128 op, uint8_t sig, uint8_t rm)
{
  _Decimal128 out;

  asm volatile (
                ".insn rrf, 0xb3ff0000, %[out], %[sig], %[op], %[rm]\n\t"
                :[out]"=f"(out)
                :[op]"f"(op), [sig]"d"(sig), [rm]"d"(rm)
                );
  printf("RRXTR ");
  DFP_VAL_PRINT(op, _Decimal128);
  printf(", %d, %x -> ", sig, rm);
  DFP_VAL_PRINT(out, _Decimal128);
  printf("\n");
}

void reround128(_Decimal128 op, uint8_t sig)
{
  uint8_t i;

  for (i = 0; i < 16; i++)
    rrxtr(op, sig, i);
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

  eedtr(d64);
  eedtr(-d64);
  eedtr(0.DD);
  eextr(d128);
  eextr(-d128);
  eextr(0.DL);

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

  d64 = 5.000005DD;
  iedtr(d64, 391);
  iedtr(d64, 392);
  iedtr(d64, 393);
  iedtr(-d64, 391);
  iedtr(-d64, 392);
  iedtr(-d64, 393);
  iedtr(0.DD, 393);
  iedtr(-0.DD, 393);
  iedtr(1.DD, 393);

  d128 = 5.000005DL;
  iextr(d128, 6169);
  iextr(d128, 6170);
  iextr(d128, 6171);
  iextr(-d128, 6169);
  iextr(-d128, 6170);
  iextr(-d128, 6171);
  iextr(0.DL, 6171);
  iextr(-0.DL, 6171);
  iextr(1.DL, 6171);

  d64 = 2.171234DD;
  quantize64(d64, 0.001DD);
  quantize64(-d64, 0.001DD);
  quantize64(-d64, 0.DD);
  quantize64(0.DD, 0.001DD);

  d128 = 26365343648.171234DL;
  quantize128(d128, 230.01DL);
  quantize128(-d128, 230.01DL);
  quantize128(d128, 0.DL);
  quantize128(-0.DL, 230.01DL);

  d64 = 2.174598DD;
  reround64(d64, 3);
  reround64(d64, 4);
  reround64(d64, 5);
  reround64(-d64, 3);
  reround64(-d64, 4);
  reround64(-d64, 5);
  reround64(0.DD, 0);

  d128 = 2.174598DL;
  reround128(d128, 3);
  reround128(d128, 4);
  reround128(d128, 5);
  reround128(-d128, 3);
  reround128(-d128, 4);
  reround128(-d128, 5);
  reround128(0.DL, 0);

  return 0;
}
