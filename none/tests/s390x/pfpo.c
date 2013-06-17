#include <stdio.h>
#include <stdint.h>
#include "dfp_utils.h"
#define __STDC_WANT_DEC_FP__ 1
#include <float.h>

#ifndef PFPO_FUNCTIONS
#define PFPO_FUNCTIONS
#define PFPO_F32_TO_D32   0x01080500
#define PFPO_D32_TO_F32   0x01050800
#define PFPO_F32_TO_D64   0x01090500
#define PFPO_D32_TO_F64   0x01060800
#define PFPO_F32_TO_D128  0x010A0500
#define PFPO_D32_TO_F128  0x01070800
#define PFPO_F64_TO_D32   0x01080600
#define PFPO_D64_TO_F32   0x01050900
#define PFPO_F64_TO_D64   0x01090600
#define PFPO_D64_TO_F64   0x01060900
#define PFPO_F64_TO_D128  0x010A0600
#define PFPO_D64_TO_F128  0x01070900
#define PFPO_D128_TO_F64  0x01060A00
#define PFPO_F128_TO_D32  0x01080700
#define PFPO_D128_TO_F32  0x01050A00
#define PFPO_F128_TO_D64  0x01090700
#define PFPO_D128_TO_F64  0x01060A00
#define PFPO_F128_TO_D128 0x010A0700
#define PFPO_D128_TO_F128 0x01070A00

#define PFPO(initial, src_type, dst_type, fn_code, round, ret_code, cc) \
({                                                                      \
  register src_type src_reg asm("f4") = initial;                        \
  register dst_type dst_reg asm("f0");                                  \
  register unsigned long fn asm("0") = fn_code | (round & 0xf);         \
  register unsigned int ret asm("1");                                   \
  asm volatile(".short 0x010a\n\t"                                      \
               "ipm %2\n\t"                                             \
               "srl %2,28\n\t"                                          \
               :"=f"(dst_reg), "=d"(ret), "=d" (cc)                     \
               : "f"(src_reg), "d"(fn));                                \
  ret_code = ret;                                                       \
  dst_reg;                                                              \
})

#endif /* PFPO_FUNCTIONS */

/* Test BFP <-> DFP conversions */
int main()
{
  int cc;
  uint8_t i, j;
  unsigned int ret_code;

  float f32;
  double f64;
  long double f128;

  _Decimal32 d32;
  _Decimal64 d64;
  _Decimal128 d128;

  float f32_in[] = {123.5656789, FLT_MAX, FLT_MIN};
  double f64_in[] = {123456789999.5656789, DBL_MIN, DBL_MAX};
  long double f128_in[] = {1234567812345678912345678912.5656789L,
                           LDBL_MIN, LDBL_MAX};

  _Decimal32 d32_in[] = {123.5656789DF, DEC32_MAX, DEC32_MIN};
  _Decimal64 d64_in[] = {123456789999.5656789DD, DEC64_MIN, DEC64_MAX};
  _Decimal128 d128_in[] = {1234567812345678912345678912.5656789DL,
                           DEC128_MIN, DEC128_MAX};

 /* valid function code */
  PFPO(0., double, _Decimal64, 0x81090600, 0, ret_code, cc);
  printf("pfpo test: function=%x ret=%d cc=%d\n", 0x81090600, ret_code, cc);

 /* invalid function code */
  PFPO(0., double, _Decimal64, 0x81990600, 0, ret_code, cc);
  printf("pfpo test: function=%x ret=%d cc=%d\n", 0x81990600, ret_code, cc);

  for (i = 0; i < 16; i++) {
    if (i < 2 || i > 7) {

      /* f32 -> d32 */
      for(j = 0; j < 3; j++) {
        printf("f32 -> d32: round=%x ", i);
        printf("%f -> ", f32_in[j]);
        d32 = PFPO(f32_in[j], float, _Decimal32, PFPO_F32_TO_D32,
                   i, ret_code, cc);
        DFP_VAL_PRINT(d32, _Decimal32);
        printf(" ret=%d cc=%d\n", ret_code, cc);
      }

      /* f32 -> d64 */
      for(j = 0; j < 3; j++) {
        printf("f32 -> d64: round=%x ", i);
        printf("%f -> ", f32_in[j]);
        d64 = PFPO(f32_in[j], float, _Decimal64, PFPO_F32_TO_D64,
                   i, ret_code, cc);
        DFP_VAL_PRINT(d64, _Decimal64);
        printf(" ret=%d cc=%d\n", ret_code, cc);
      }

      /* f32 -> d128 */
      for(j = 0; j < 3; j++) {
        printf("f32 -> d128: round=%x ", i);
        printf("%f -> ", f32_in[j]);
        d128 = PFPO(f32_in[j], float, _Decimal128, PFPO_F32_TO_D128,
                    i, ret_code, cc);
        DFP_VAL_PRINT(d128, _Decimal128);
        printf(" ret=%d cc=%d\n", ret_code, cc);
      }

      /* f64 -> d32 */
      for(j = 0; j < 3; j++) {
        printf("f64 -> d32: round=%x ", i);
        printf("%lf -> ", f64_in[j]);
        d32 = PFPO(f64_in[j], double, _Decimal32, PFPO_F64_TO_D32,
                   i, ret_code, cc);
        DFP_VAL_PRINT(d32, _Decimal32);
        printf(" ret=%d cc=%d\n", ret_code, cc);
      }

      /* f64 -> d64 */
      for(j = 0; j < 3; j++) {
        printf("f64 -> d64: round=%x ", i);
        printf("%lf -> ", f64_in[j]);
        d64 = PFPO(f64_in[j], double, _Decimal64, PFPO_F64_TO_D64,
                   i, ret_code, cc);
        DFP_VAL_PRINT(d64, _Decimal64);
        printf(" ret=%d cc=%d\n", ret_code, cc);
      }

      /* f64 -> d128 */
      for(j = 0; j < 3; j++) {
        printf("f64 -> d128: round=%x ", i);
        printf("%lf -> ", f64_in[j]);
        d128 = PFPO(f64_in[j], double, _Decimal128, PFPO_F64_TO_D128,
                   i, ret_code, cc);
        DFP_VAL_PRINT(d128, _Decimal128);
        printf(" ret=%d cc=%d\n", ret_code, cc);
      }

      /* f128 -> d32 */
      for(j = 0; j < 3; j++) {
        printf("f128 -> d32: round=%x ", i);
        printf("%Lf -> ", f128_in[j]);
        d32 = PFPO(f128_in[j], long double, _Decimal32, PFPO_F128_TO_D32,
                   i, ret_code, cc);
        DFP_VAL_PRINT(d32, _Decimal32);
        printf(" ret=%d cc=%d\n", ret_code, cc);
      }

      /* f128 -> d64 */
      for(j = 0; j < 3; j++) {
        printf("f128 -> d6: round=%x ", i);
        printf("%Lf -> ", f128_in[j]);
        d64 = PFPO(f128_in[j], long double, _Decimal64, PFPO_F128_TO_D64,
                   i, ret_code, cc);
        DFP_VAL_PRINT(d64, _Decimal64);
        printf(" ret=%d cc=%d\n", ret_code, cc);
      }

      /* f128 -> d128 */
      for(j = 0; j < 3; j++) {
        printf("f128 -> d128: round=%x ", i);
        printf("%Lf -> ", f128_in[j]);
        d128 = PFPO(f128_in[j], long double, _Decimal128, PFPO_F128_TO_D128,
                   i, ret_code, cc);
        DFP_VAL_PRINT(d128, _Decimal128);
        printf(" ret=%d cc=%d\n", ret_code, cc);
      }

      /* d32 -> f32 */
      for(j = 0; j < 3; j++) {
        printf("d32 -> f32: round=%x ", i);
        DFP_VAL_PRINT(d32_in[j], _Decimal32);
        printf(" -> ");
        f32 = PFPO(d32_in[j], _Decimal32, float, PFPO_D32_TO_F32,
                   i, ret_code, cc);
        printf("%f", f32);
        printf(" ret=%d cc=%d\n", ret_code, cc);
      }

      /* d32 -> f64 */
      for(j = 0; j < 3; j++) {
        printf("d32 -> f64: round=%x ", i);
        DFP_VAL_PRINT(d32_in[j], _Decimal32);
        printf(" -> ");
        f64 = PFPO(d32_in[j], _Decimal32, double, PFPO_D32_TO_F64,
                   i, ret_code, cc);
        printf("%lf", f64);
        printf(" ret=%d cc=%d\n", ret_code, cc);
      }

      /* d32 -> f128 */
      for(j = 0; j < 3; j++) {
        printf("d32 -> f128: round=%x ", i);
        DFP_VAL_PRINT(d32_in[j], _Decimal32);
        printf(" -> ");
        f128 = PFPO(d32_in[j], _Decimal32, long double, PFPO_D32_TO_F128,
                   i, ret_code, cc);
        printf("%Lf", f128);
        printf(" ret=%d cc=%d\n", ret_code, cc);
      }

      /* d64 -> f32 */
      for(j = 0; j < 3; j++) {
        printf("d64 -> f32: round=%x ", i);
        DFP_VAL_PRINT(d64_in[j], _Decimal64);
        printf(" -> ");
        f32 = PFPO(d64_in[j], _Decimal64, float, PFPO_D64_TO_F32,
                   i, ret_code, cc);
        printf("%f", f32);
        printf(" ret=%d cc=%d\n", ret_code, cc);
      }

      /* d64 -> f64 */
      for(j = 0; j < 3; j++) {
        printf("d64 -> f64: round=%x ", i);
        DFP_VAL_PRINT(d64_in[j], _Decimal64);
        printf(" -> ");
        f64 = PFPO(d64_in[j], _Decimal64, double, PFPO_D64_TO_F64,
                   i, ret_code, cc);
        printf("%lf", f64);
        printf(" ret=%d cc=%d\n", ret_code, cc);
      }

      /* d64 -> f128 */
      for(j = 0; j < 3; j++) {
        printf("d64 -> f128: round=%x ", i);
        DFP_VAL_PRINT(d64_in[j], _Decimal64);
        printf(" -> ");
        f128 = PFPO(d64_in[j], _Decimal64, long double, PFPO_D64_TO_F128,
                   i, ret_code, cc);
        printf("%Lf", f128);
        printf(" ret=%d cc=%d\n", ret_code, cc);
      }

      /* d128 -> f32 */
      for(j = 0; j < 3; j++) {
        printf("d128 -> f32: round=%x ", i);
        DFP_VAL_PRINT(d128_in[j], _Decimal128);
        printf(" -> ");
        f32 = PFPO(d128_in[j], _Decimal128, float, PFPO_D128_TO_F32,
                   i, ret_code, cc);
        printf("%f", f32);
        printf(" ret=%d cc=%d\n", ret_code, cc);
      }

      /* d128 -> f64 */
      for(j = 0; j < 3; j++) {
        printf("d128 -> f64: round=%x ", i);
        DFP_VAL_PRINT(d128_in[j], _Decimal128);
        printf(" -> ");
        f64 = PFPO(d128_in[j], _Decimal128, double, PFPO_D128_TO_F64,
                   i, ret_code, cc);
        printf("%lf", f64);
        printf(" ret=%d cc=%d\n", ret_code, cc);
      }

      /* d128 -> f128 */
      for(j = 0; j < 3; j++) {
        printf("d128 -> f128: round=%x ", i);
        DFP_VAL_PRINT(d128_in[j], _Decimal128);
        printf(" -> ");
        f128 = PFPO(d128_in[j], _Decimal128, long double, PFPO_D128_TO_F128,
                   i, ret_code, cc);
        printf("%Lf", f128);
        printf(" ret=%d cc=%d\n", ret_code, cc);
      }
    }
  }
  return 0;
}
