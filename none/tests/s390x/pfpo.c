#include <stdio.h>
#include <stdint.h>
#include "dfp_utils.h"
#define __STDC_WANT_DEC_FP__ 1
#include <float.h>

#ifndef PFPO_FUNCTIONS
#define PFPO_FUNCTIONS
#define PFPO_F64_TO_D64   0x01090600
#define PFPO_D64_TO_F64   0x01060900
#define PFPO_F64_TO_D128  0x010A0600
#define PFPO_D128_TO_F64  0x01060A00
#define PFPO_F128_TO_D128 0x010A0700
#define PFPO_D128_TO_F128 0x01070A00
#endif

/* Test BFP <-> DFP conversions */

void pfpo_test(unsigned long fn_code)
{
  register _Decimal64 d64 asm("f0");
  register unsigned long fn asm("0") = fn_code;
  register unsigned int ret asm("1");
  int cc;

  asm volatile(
               ".short 0x010a\n\t"
               "ipm %2\n\t"
               "srl %2,28\n\t"
               :"=f"(d64),"=d"(ret), "=d" (cc)
               :"d"(fn)
               );
  printf("pfpo test: function=%lx ret=%d cc=%d\n", fn_code, ret, cc);
}

void pfpo_f64_to_d64(double src, uint8_t rm)
{
  register double f64 asm("f4") = src;
  register _Decimal64 d64 asm("f0");
  register unsigned long fn asm("0") = PFPO_F64_TO_D64 | (rm & 0xf);
  register unsigned int ret asm("1");
  _Decimal64 dest;
  unsigned int ret_code;
  int cc;
  asm volatile(
               ".short 0x010a\n\t"
               "ipm %2\n\t"
               "srl %2,28\n\t"
               :"=f"(d64), "=d"(ret), "=d" (cc)
               :"f"(f64), "d"(fn)
               );
  ret_code = ret;
  dest = d64;

  printf("round=%x %lf -> ", rm, src);
  DFP_VAL_PRINT(dest, _Decimal64);
  printf(" ret=%d cc=%d\n", ret_code, cc);
}

void pfpo_d64_to_f64(_Decimal64 src, uint8_t rm)
{
  register _Decimal64 d64 asm("f4") = src;
  register double f64 asm("f0");
  register unsigned long fn asm("0") = PFPO_D64_TO_F64 | (rm & 0xf);
  register unsigned int ret asm("1");
  double dest;
  unsigned int ret_code;
  int cc;
  asm volatile(
               ".short 0x010a\n\t"
               "ipm %2\n\t"
               "srl %2,28\n\t"
               :"=f"(f64), "=d"(ret), "=d" (cc)
               :"f"(d64), "d"(fn)
               );
  ret_code = ret;
  dest = f64;

  printf("round=%x ", rm);
  DFP_VAL_PRINT(src, _Decimal64);
  printf(" -> %lf ret=%d cc=%d\n", dest, ret_code, cc);
}

void pfpo_f64_to_d128(double src, uint8_t rm)
{
  register double f64 asm("f4") = src;
  register _Decimal128 d128 asm("f0");
  register unsigned long fn asm("0") = PFPO_F64_TO_D128 | (rm & 0xf);
  register unsigned int ret asm("1");
  _Decimal128 dest;
  unsigned int ret_code;
  int cc;
  asm volatile(
               ".short 0x010a\n\t"
               "ipm %2\n\t"
               "srl %2,28\n\t"
               :"=f"(d128), "=d"(ret), "=d" (cc)
               :"f"(f64), "d"(fn)
               );
  ret_code = ret;
  dest = d128;

  printf("round=%x %lf -> ", rm, src);
  DFP_VAL_PRINT(dest, _Decimal128);
  printf(" ret=%d cc=%d\n", ret_code, cc);
}

void pfpo_d128_to_f64(_Decimal128 src, uint8_t rm)
{
  register _Decimal128 d128 asm("f4") = src;
  register double f64 asm("f0");
  register unsigned long fn asm("0") = PFPO_D128_TO_F64 | (rm & 0xf);
  register unsigned int ret asm("1");
  double dest;
  unsigned int ret_code;
  int cc;
  asm volatile(
               ".short 0x010a\n\t"
               "ipm %2\n\t"
               "srl %2,28\n\t"
               :"=f"(f64), "=d"(ret), "=d" (cc)
               :"f"(d128), "d"(fn)
               );
  ret_code = ret;
  dest = f64;

  printf("round=%x ", rm);
  DFP_VAL_PRINT(src, _Decimal128);
  printf(" -> %lf ret=%d cc=%d\n", dest, ret_code, cc);
}

void pfpo_f128_to_d128(long double src, uint8_t rm)
{
  register long double f128 asm("f4") = src;
  register _Decimal128 d128 asm("f0");
  register unsigned long fn asm("0") = PFPO_F128_TO_D128 | (rm & 0xf);
  register unsigned int ret asm("1");
  _Decimal128 dest;
  unsigned int ret_code;
  int cc;
  asm volatile(
               ".short 0x010a\n\t"
               "ipm %2\n\t"
               "srl %2,28\n\t"
               :"=f"(d128), "=d"(ret), "=d" (cc)
               :"f"(f128), "d"(fn)
               );
  ret_code = ret;
  dest = d128;

  printf("round=%x %Lf -> ", rm, src);
  DFP_VAL_PRINT(dest, _Decimal128);
  printf(" ret=%d cc=%d\n", ret_code, cc);
}

void pfpo_d128_to_f128(_Decimal128 src, uint8_t rm)
{
  register _Decimal128 d128 asm("f4") = src;
  register long double f128 asm("f0");
  register unsigned long fn asm("0") = PFPO_D128_TO_F128 | (rm & 0xf);
  register unsigned int ret asm("1");
  long double dest;
  unsigned int ret_code;
  int cc;
  asm volatile(
               ".short 0x010a\n\t"
               "ipm %2\n\t"
               "srl %2,28\n\t"
               :"=f"(f128), "=d"(ret), "=d" (cc)
               :"f"(d128), "d"(fn)
               );
  ret_code = ret;
  dest = f128;

  printf("round=%x ", rm);
  DFP_VAL_PRINT(src, _Decimal128);
  printf(" -> %Lf ret=%d cc=%d\n", dest, ret_code, cc);
}

int main()
{
  uint8_t i;

  pfpo_test(0x81090600); /* valid function code */
  pfpo_test(0x81990600); /* invalid function code */

  for (i = 0; i < 16; i++) {
    if (i < 2 || i > 7) {
      pfpo_f64_to_d64(123456789999.5656789, i);
      pfpo_f64_to_d64(DBL_MIN, i);
      pfpo_f64_to_d64(DBL_MAX, i);

      pfpo_d64_to_f64(123456789999.5656789DD, i);
      pfpo_d64_to_f64(DEC64_MIN, i);
      pfpo_d64_to_f64(DEC64_MAX, i);

      pfpo_f64_to_d128(123456789999.5656789, i);
      pfpo_f64_to_d128(DBL_MIN, i);
      pfpo_f64_to_d128(DBL_MAX, i);

      pfpo_d128_to_f64(1234567899999999.5656789DL, i);
      pfpo_d128_to_f64(DEC128_MIN, i);
      pfpo_d128_to_f64(DEC128_MAX, i);

      pfpo_f128_to_d128(1234567812345678912345678912.5656789L, i);
      pfpo_f128_to_d128(LDBL_MIN, i);
      /*   pfpo_f128_to_d128(LDBL_MAX, i); */
      pfpo_f128_to_d128(1.6E+200L, i);

      pfpo_d128_to_f128(1234567812345678912345678912.5656789DL, i);
      pfpo_d128_to_f128(DEC128_MIN, i);
      /*   pfpo_d128_to_f128(DEC128_MAX, i); */
      pfpo_d128_to_f128(1.6E+200DL, i);
    }
  }
  return 0;
}
