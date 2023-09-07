#include "dfp_utils.h"
#include <stdio.h>

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

static const pun_d32 f32_in[] = {
   {.f = 123.5656789},
   {.f = 0x1.fffffep+127}, /* FLT_MAX */
   {.f = 0x1p-126},        /* FLT_MIN */
};

static const pun_d64 f64_in[] = {
   {.f = 123456789999.5656789},
   {.f = 0x1p-1022},               /* DBL_MIN */
   {.f = 0x1.fffffffffffffp+1023}, /* DBL_MAX */
};

static const pun_d128 f128_in[] = {
   {.f = 1234567812345678912345678912.5656789L},
   {.f = 0x1p-16382L},                              /* LDBL_MIN */
   {.f = 0x1.ffffffffffffffffffffffffffffp+16383L}, /* LDBL_MAX */
};

static const pun_d32 d32_in[] = {
   {0x2614d757}, /* 123.5656789DF */
   {0x77f3fcff}, /* DEC32_MAX */
   {0x00600001}, /* DEC32_MAX */
};

static const pun_d64 d64_in[] = {
   {0x262934b9c7fa7f57}, /* 123456789999.5656789DD */
   {0x003c000000000001}, /* DEC64_MIN */
   {0x77fcff3fcff3fcff}, /* DEC64_MAX */
};

static const pun_d128 d128_in[] = {
   {{0x2606934b9d1c7177, 0x8671c5de19cb9779}},
   {{0x0008400000000000, 0x0000000000000001}},
   {{0x77ffcff3fcff3fcf, 0xf3fcff3fcff3fcff}},
};

#define PFPO(initial, src_type, dst_type, fn_code, round, ret_code, cc)        \
   ({                                                                          \
      pun_##dst_type          dst;                                             \
      register reg_##src_type src_reg asm("f4") = initial.f;                   \
      register reg_##dst_type dst_reg asm("f0");                               \
      register unsigned long  fn asm("0") = fn_code | (round & 0xf);           \
      register unsigned int   ret asm("1");                                    \
      asm(".insn e,0x010a\n\t"                                                 \
          "ipm %2\n\t"                                                         \
          "srl %2,28\n\t"                                                      \
          : "=f"(dst_reg), "=d"(ret), "=d"(cc)                                 \
          : "f"(src_reg), "d"(fn)                                              \
          : "cc");                                                             \
      ret_code = ret;                                                          \
      dst.f    = dst_reg;                                                      \
      dst;                                                                     \
   })

#define TEST_TO_DEC(in_bits, out_bits, flg, round)                             \
   ({                                                                          \
      pun_d##out_bits result;                                                  \
      for (unsigned j = 0; j < 3; j++) {                                       \
         printf("f" #in_bits " -> d" #out_bits ": round=%x ", round);          \
         printf("%" flg "a -> ", f##in_bits##_in[j].f);                        \
         result = PFPO(f##in_bits##_in[j], d##in_bits, d##out_bits,            \
                       PFPO_F##in_bits##_TO_D##out_bits, round, ret_code, cc); \
         DFP_VAL_PRINT(result, pun_d##out_bits);                               \
         printf(" ret=%d cc=%d\n", ret_code, cc);                              \
      }                                                                        \
   })

#define TEST_FROM_DEC(in_bits, out_bits, flg, round)                           \
   ({                                                                          \
      pun_d##out_bits result;                                                  \
      for (unsigned j = 0; j < 3; j++) {                                       \
         printf("d" #in_bits " -> f" #out_bits ": round=%x ", round);          \
         DFP_VAL_PRINT(d##in_bits##_in[j], pun_d##in_bits);                    \
         result = PFPO(d##in_bits##_in[j], d##in_bits, d##out_bits,            \
                       PFPO_D##in_bits##_TO_F##out_bits, round, ret_code, cc); \
         printf(" -> %" flg "a", result.f);                                    \
         printf(" ret=%d cc=%d\n", ret_code, cc);                              \
      }                                                                        \
   })

/* Test BFP <-> DFP conversions */
int main()
{
   int                  cc;
   unsigned             ret_code;
   static const pun_d64 f64_zero = {.f = 0.};

   /* valid function code */
   PFPO(f64_zero, d64, d64, 0x81090600, 0, ret_code, cc);
   printf("pfpo test: function=%x ret=%d cc=%d\n", 0x81090600, ret_code, cc);

   /* invalid function code */
   PFPO(f64_zero, d64, d64, 0x81990600, 0, ret_code, cc);
   printf("pfpo test: function=%x ret=%d cc=%d\n", 0x81990600, ret_code, cc);

   for (unsigned i = 0; i < 16; i++) {
      if (i < 2 || i > 7) {
         TEST_TO_DEC(32, 32, "", i);
         TEST_TO_DEC(32, 64, "", i);
         TEST_TO_DEC(32, 128, "", i);
         TEST_TO_DEC(64, 32, "", i);
         TEST_TO_DEC(64, 64, "", i);
         TEST_TO_DEC(64, 128, "", i);
         TEST_TO_DEC(128, 32, "L", i);
         TEST_TO_DEC(128, 64, "L", i);
         TEST_TO_DEC(128, 128, "L", i);

         TEST_FROM_DEC(32, 32, "", i);
         TEST_FROM_DEC(32, 64, "", i);
         TEST_FROM_DEC(32, 128, "L", i);
         TEST_FROM_DEC(64, 32, "", i);
         TEST_FROM_DEC(64, 64, "", i);
         TEST_FROM_DEC(64, 128, "L", i);
         TEST_FROM_DEC(128, 32, "", i);
         TEST_FROM_DEC(128, 64, "", i);
         TEST_FROM_DEC(128, 128, "L", i);
      }
   }
   return 0;
}
