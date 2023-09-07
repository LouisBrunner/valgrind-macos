#include "dfp_utils.h"
#include <stdint.h>
#include <stdio.h>

#define ITERATE_0_15(x)                                                        \
   x(0);                                                                       \
   x(1);                                                                       \
   x(2);                                                                       \
   x(3);                                                                       \
   x(4);                                                                       \
   x(5);                                                                       \
   x(6);                                                                       \
   x(7);                                                                       \
   x(8);                                                                       \
   x(9);                                                                       \
   x(10);                                                                      \
   x(11);                                                                      \
   x(12);                                                                      \
   x(13);                                                                      \
   x(14);                                                                      \
   x(15);

/* Extract biased exponent; extract significance */

#define MAKE_EXTRACT(fun, name, opcode, type)                                  \
   static void fun(type in)                                                    \
   {                                                                           \
      long out;                                                                \
      asm(".insn rre," #opcode "0000, %[out], %[in]"                           \
          : [out] "=d"(out)                                                    \
          : [in] "f"(in.f));                                                   \
      printf("%s ", #name);                                                    \
      DFP_VAL_PRINT(in, type);                                                 \
      printf(" -> %ld\n", out);                                                \
   }

MAKE_EXTRACT(eedtr, EEDTR, 0xb3e5, pun_d64)
MAKE_EXTRACT(eextr, EEXTR, 0xb3ed, pun_d128)
MAKE_EXTRACT(esdtr, ESDTR, 0xb3e7, pun_d64)
MAKE_EXTRACT(esxtr, ESXTR, 0xb3ef, pun_d128)

/* Insert biased exponent */

#define MAKE_INSERT(fun, name, opcode, type)                                   \
   static void fun(type in, long amount)                                       \
   {                                                                           \
      type out;                                                                \
      asm(".insn rrf," #opcode "0000, %[out], %[amount], %[in], 0"             \
          : [out] "=f"(out.f)                                                  \
          : [in] "f"(in.f), [amount] "d"(amount));                             \
      printf("%s ", #name);                                                    \
      DFP_VAL_PRINT(in, type);                                                 \
      printf(", %ld -> ", amount);                                             \
      DFP_VAL_PRINT(out, type);                                                \
      printf("\n");                                                            \
   }

MAKE_INSERT(iedtr, IEDTR, 0xb3f6, pun_d64)
MAKE_INSERT(iextr, IEXTR, 0xb3fe, pun_d128)

/* Load and test */

#define MAKE_LOAD_AND_TEST(fun, name, opcode, type)                            \
   static void fun(type in)                                                    \
   {                                                                           \
      type out;                                                                \
      int  cc;                                                                 \
      asm(".insn rre," #opcode "0000, %[out], %[in]\n\t"                       \
          "ipm %[cc]\n\t"                                                      \
          "srl %[cc],28"                                                       \
          : [out] "=f"(out.f), [cc] "=d"(cc)                                   \
          : [in] "f"(in.f));                                                   \
      printf("%s ", #name);                                                    \
      DFP_VAL_PRINT(in, type);                                                 \
      printf(" -> %d\n", cc);                                                  \
   }

MAKE_LOAD_AND_TEST(ltdtr, LTDTR, 0xb3d6, pun_d64)
MAKE_LOAD_AND_TEST(ltxtr, LTXTR, 0xb3de, pun_d128)

/* Quantize */

#define MAKE_QUANTIZE(fun, name, opcode, type, rm)                             \
   static void fun##rm(type op, type quan)                                     \
   {                                                                           \
      type out;                                                                \
      asm(".insn rrf," #opcode "0000, %[out], %[quan], %[op], %[m]"            \
          : [out] "=f"(out.f)                                                  \
          : [op] "f"(op.f), [quan] "f"(quan.f), [m] "i"(rm));                  \
      printf("%s ", #name);                                                    \
      DFP_VAL_PRINT(op, type);                                                 \
      printf(", ");                                                            \
      DFP_VAL_PRINT(quan, type);                                               \
      printf(", %x -> ", rm);                                                  \
      DFP_VAL_PRINT(out, type);                                                \
      printf("\n");                                                            \
   }

#define MAKE_QADTR(rm) MAKE_QUANTIZE(qadtr, QADTR, 0xb3f5, pun_d64, rm)
#define CALL_QADTR(rm) qadtr##rm(op, quan)

ITERATE_0_15(MAKE_QADTR)

void quantize64(pun_d64 op, pun_d64 quan) { ITERATE_0_15(CALL_QADTR); }

#define MAKE_QAXTR(rm) MAKE_QUANTIZE(qaxtr, QAXTR, 0xb3fd, pun_d128, rm)
#define CALL_QAXTR(rm) qaxtr##rm(op, quan)

ITERATE_0_15(MAKE_QAXTR)

void quantize128(pun_d128 op, pun_d128 quan) { ITERATE_0_15(CALL_QAXTR); }

/* Reround */

#define MAKE_REROUND(fun, name, opcode, type, rm)                              \
   static void fun##rm(type op, uint8_t sig)                                   \
   {                                                                           \
      type out;                                                                \
      asm(".insn rrf," #opcode "0000, %[out], %[sig], %[op], %[m]"             \
          : [out] "=f"(out.f)                                                  \
          : [op] "f"(op.f), [sig] "d"(sig), [m] "i"(rm));                      \
      printf("%s ", #name);                                                    \
      DFP_VAL_PRINT(op, type);                                                 \
      printf(", %d, %x -> ", sig, rm);                                         \
      DFP_VAL_PRINT(out, type);                                                \
      printf("\n");                                                            \
   }

#define MAKE_RRDTR(rm) MAKE_REROUND(rrdtr, RRDTR, 0xb3f7, pun_d64, rm)
#define CALL_RRDTR(rm) rrdtr##rm(op, sig)

ITERATE_0_15(MAKE_RRDTR)

void reround64(pun_d64 op, uint8_t sig) { ITERATE_0_15(CALL_RRDTR); }

#define MAKE_RRXTR(rm) MAKE_REROUND(rrxtr, RRXTR, 0xb3ff, pun_d128, rm)
#define CALL_RRXTR(rm) rrxtr##rm(op, sig)

ITERATE_0_15(MAKE_RRXTR)

void reround128(pun_d128 op, uint8_t sig) { ITERATE_0_15(CALL_RRXTR); }

/* Shift significand left/right */

#define MAKE_SHIFT(fun, name, opcode, type)                                    \
   static void fun(type in, unsigned long amount)                              \
   {                                                                           \
      type out;                                                                \
      int* shift = (int*)amount;                                               \
      asm(".insn rxf, " #opcode ", %[out], %[in], 0(%[amount])"                \
          : [out] "=f"(out.f)                                                  \
          : [in] "f"(in.f), [amount] "a"(shift));                              \
      printf("%s ", #name);                                                    \
      DFP_VAL_PRINT(in, type);                                                 \
      printf(" -> ");                                                          \
      DFP_VAL_PRINT(out, type);                                                \
      printf("\n");                                                            \
   }

MAKE_SHIFT(sldt, SLDT, 0xed0000000040, pun_d64)
MAKE_SHIFT(slxt, SLXT, 0xed0000000048, pun_d128)
MAKE_SHIFT(srdt, SRDT, 0xed0000000041, pun_d64)
MAKE_SHIFT(srxt, SRXT, 0xed0000000049, pun_d128)

/* 64-bit decimal constants */
static const pun_d64 dd_0  = {0x2238000000000000}; /* 0.DD */
static const pun_d64 dd_00 = {0x2234000000000000}; /* 0.0DD */
static const pun_d64 dd_m0 = {0xa238000000000000}; /* -0.DD */
static const pun_d64 dd_1  = {0x2238000000000001}; /* 1.DD */
static const pun_d64 dd_A  = {0x22280000000a0005}; /* 50.0005DD */
static const pun_d64 dd_mA = {0xa2280000000a0005}; /* -50.0005DD */
static const pun_d64 dd_B  = {0x2224014d2e7971a1}; /* 12345678.54321DD */
static const pun_d64 dd_mB = {0xa224014d2e7971a1}; /* -12345678.54321DD */
static const pun_d64 dd_C  = {0x2220000000500005}; /* 5.000005DD */
static const pun_d64 dd_mC = {0xa220000000500005}; /* -5.000005DD */
static const pun_d64 dd_D  = {0x222000000023c534}; /* 2.171234DD */
static const pun_d64 dd_mD = {0xa22000000023c534}; /* -2.171234DD */
static const pun_d64 dd_DQ = {0x222c000000000001}; /* 0.001DD */
static const pun_d64 dd_E  = {0x222000000023d2de}; /* 2.174598DD */
static const pun_d64 dd_mE = {0xa22000000023d2de}; /* -2.174598DD */

/* 128-bit versions of the same constants, except:
   dl_D  = 26365343648.171234DL
   dl_DQ = 230.01DL */
static const pun_d128 dl_0  = {{0x2208000000000000, 0x0000000000000000}};
static const pun_d128 dl_00 = {{0x2207c00000000000, 0x0000000000000000}};
static const pun_d128 dl_m0 = {{0xa208000000000000, 0x0000000000000000}};
static const pun_d128 dl_1  = {{0x2208000000000000, 0x0000000000000001}};
static const pun_d128 dl_A  = {{0x2207000000000000, 0x00000000000a0005}};
static const pun_d128 dl_mA = {{0xa207000000000000, 0x00000000000a0005}};
static const pun_d128 dl_B  = {{0x2206c00000000000, 0x0000014d2e7971a1}};
static const pun_d128 dl_mB = {{0xa206c00000000000, 0x0000014d2e7971a1}};
static const pun_d128 dl_C  = {{0x2206800000000000, 0x0000000000500005}};
static const pun_d128 dl_mC = {{0xa206800000000000, 0x0000000000500005}};
static const pun_d128 dl_D  = {{0x2206800000000000, 0x0099e570f483c534}};
static const pun_d128 dl_mD = {{0xa206800000000000, 0x0099e570f483c534}};
static const pun_d128 dl_DQ = {{0x2207800000000000, 0x0000000000008c01}};
static const pun_d128 dl_E  = {{0x2206800000000000, 0x000000000023d2de}};
static const pun_d128 dl_mE = {{0xa206800000000000, 0x000000000023d2de}};

int main()
{
   eedtr(dd_A);
   eedtr(dd_mA);
   eedtr(dd_0);
   eextr(dl_A);
   eextr(dl_mA);
   eextr(dl_0);

   esdtr(dd_A);
   esdtr(dd_mA);
   esdtr(dd_0);
   esxtr(dl_A);
   esxtr(dl_mA);
   esxtr(dl_0);

   ltdtr(dd_A);
   ltdtr(dd_mA);
   ltdtr(dd_00);
   ltxtr(dl_A);
   ltxtr(dl_mA);
   ltxtr(dl_00);

   sldt(dd_B, 10);
   sldt(dd_mB, 2);
   sldt(dd_0, 2);
   sldt(dd_m0, 2);

   srdt(dd_B, 5);
   srdt(dd_mB, 2);
   srdt(dd_0, 2);
   srdt(dd_m0, 2);

   slxt(dl_B, 10);
   slxt(dl_mB, 2);
   slxt(dl_0, 2);
   slxt(dl_m0, 2);

   srxt(dl_B, 10);
   srxt(dl_mB, 2);
   srxt(dl_0, 2);
   srxt(dl_m0, 2);

   iedtr(dd_C, 391);
   iedtr(dd_C, 392);
   iedtr(dd_C, 393);
   iedtr(dd_mC, 391);
   iedtr(dd_mC, 392);
   iedtr(dd_mC, 393);
   iedtr(dd_0, 393);
   iedtr(dd_m0, 393);
   iedtr(dd_1, 393);

   iextr(dl_C, 6169);
   iextr(dl_C, 6170);
   iextr(dl_C, 6171);
   iextr(dl_mC, 6169);
   iextr(dl_mC, 6170);
   iextr(dl_mC, 6171);
   iextr(dl_0, 6171);
   iextr(dl_m0, 6171);
   iextr(dl_1, 6171);

   quantize64(dd_D, dd_DQ);
   quantize64(dd_mD, dd_DQ);
   quantize64(dd_mD, dd_0);
   quantize64(dd_0, dd_DQ);

   quantize128(dl_D, dl_DQ);
   quantize128(dl_mD, dl_DQ);
   quantize128(dl_D, dl_0);
   quantize128(dl_m0, dl_DQ);

   reround64(dd_E, 3);
   reround64(dd_E, 4);
   reround64(dd_E, 5);
   reround64(dd_mE, 3);
   reround64(dd_mE, 4);
   reround64(dd_mE, 5);
   reround64(dd_0, 0);

   reround128(dl_E, 3);
   reround128(dl_E, 4);
   reround128(dl_E, 5);
   reround128(dl_mE, 3);
   reround128(dl_mE, 4);
   reround128(dl_mE, 5);
   reround128(dl_0, 0);

   return 0;
}
