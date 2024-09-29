#ifndef ARM64_SIMD_H
#define ARM64_SIMD_H

#include <assert.h>
#include <stdio.h>
#include <string.h>  // memset

typedef  unsigned char           UChar;
typedef  unsigned short int      UShort;
typedef  unsigned int            UInt;
typedef  signed int              Int;
typedef  unsigned char           UChar;
typedef  unsigned long long int  ULong;
typedef  signed long long int    Long;
typedef  double                  Double;
typedef  float                   Float;
/* Half-precision floating point is not universally available, so use a
   synthesized 16 bit type. This allows the testing framework to be shared
   across all SIMD tests. The functions halfToSingleFPAsInt() and
   shortToSingle() below are used to create a Float16 type for testing purposes.
*/
typedef  unsigned short int      Float16;

typedef  unsigned char           Bool;
#define False ((Bool)0)
#define True  ((Bool)1)


#define ITERS 1

typedef
  enum { TyHF=1234, TySF, TyDF, TyB, TyH, TyS, TyD, TyNONE }
  LaneTy;

union _V128 {
   UChar   u8[16];
   UShort  u16[8];
   UInt    u32[4];
   ULong   u64[2];
   Float16 f16[8];
   Float   f32[4];
   Double  f64[2];
};
typedef  union _V128   V128;

/* Conversion based on IEEE half-precision, as described in the IEEE 754-2008
   standard and Arm Reference Manual 'A1.4.2 Half-precision floating-point
   formats' where hardware capability supports __fp16 (VEX_HWCAPS_ARM64_FP16
   and VEX_HWCAPS_ARM64_VFP16 set).
*/
UInt halfToSingleFPAsInt(UShort y);

static inline float shortToSingle(UShort imm)
{
   union { float f; UInt i; } v;
   v.i = halfToSingleFPAsInt(imm);
   return v.f;
}

UChar randUChar ( void );

static inline ULong randULong ( LaneTy ty )
{
   Int i;
   ULong r = 0;
   for (i = 0; i < 8; i++) {
      r = (r << 8) | (ULong)(0xFF & randUChar());
   }
   return r;
}

/* Generates a random V128. Ensures that that it contains normalised FP numbers
   when viewed as either F16x8, F32x4 or F64x2, so that it is reasonable to use
   in FP test cases. */
void randV128 ( /*OUT*/V128* v, LaneTy ty );

static inline void showV128 ( V128* v )
{
   Int i;
   for (i = 15; i >= 0; i--)
      printf("%02x", (Int)v->u8[i]);
}

static inline void showBlock ( const char* msg, V128* block, Int nBlock )
{
   Int i;
   printf("%s\n", msg);
   for (i = 0; i < nBlock; i++) {
      printf("  ");
      showV128(&block[i]);
      printf("\n");
   }
}

static inline ULong dup4x16 ( UInt x )
{
   ULong r = x & 0xF;
   r |= (r << 4);
   r |= (r << 8);
   r |= (r << 16);
   r |= (r << 32);
   return r;
}

// Generate a random double- or single-precision number. About 1 time in 2,
// instead return a special value (+/- Inf, +/-Nan, denorm). This ensures that
// many of the groups of 4 calls here will return a special value.
Double randDouble ( void );
Float randFloat ( void );

void randBlock_Doubles ( V128* block, Int nBlock );
void randBlock_Floats ( V128* block, Int nBlock );


/* ---------------------------------------------------------------- */
/* -- Parameterisable test macros                                -- */
/* ---------------------------------------------------------------- */

#define DO50(_action) \
   do { \
      Int _qq; for (_qq = 0; _qq < 50; _qq++) { _action ; } \
   } while (0)


/* Note this also sets the destination register to a known value (0x55..55)
   since it can sometimes be an input to the instruction too. */
#define GEN_UNARY_TEST(INSN,SUFFIXD,SUFFIXN) \
  __attribute__((noinline)) \
  static void test_##INSN##_##SUFFIXD##_##SUFFIXN ( LaneTy ty ) { \
     Int i; \
     for (i = 0; i < ITERS; i++) { \
        V128 block[2+1]; \
        memset(block, 0x55, sizeof(block)); \
        randV128(&block[0], ty); \
        randV128(&block[1], ty); \
        __asm__ __volatile__( \
           "mov   x30, #0 ; msr fpsr, x30 ; " \
           "ldr   q7, [%0, #0]   ; " \
           "ldr   q8, [%0, #16]   ; " \
           #INSN " v8." #SUFFIXD ", v7." #SUFFIXN " ; " \
           "str   q8, [%0, #16] ; " \
           "mrs   x30, fpsr ; str x30, [%0, #32] " \
           : : "r"(&block[0]) : "memory", "v7", "v8", "x30" \
        ); \
        printf(#INSN   " v8." #SUFFIXD ", v7." #SUFFIXN); \
        UInt fpsr = 0xFFFFFF60 & block[2].u32[0]; \
        showV128(&block[0]); printf("  "); \
        showV128(&block[1]); printf(" fpsr=%08x\n", fpsr); \
     } \
  }


/* Note this also sets the destination register to a known value (0x55..55)
   since it can sometimes be an input to the instruction too. */
#define GEN_BINARY_TEST(INSN,SUFFIXD,SUFFIXN,SUFFIXM)  \
  __attribute__((noinline)) \
  static void test_##INSN##_##SUFFIXD##_##SUFFIXN##_##SUFFIXM ( LaneTy ty ) { \
     Int i; \
     for (i = 0; i < ITERS; i++) { \
        V128 block[3+1]; \
        memset(block, 0x55, sizeof(block)); \
        randV128(&block[0], ty); \
        randV128(&block[1], ty); \
        randV128(&block[2], ty); \
        __asm__ __volatile__( \
           "mov   x30, #0 ; msr fpsr, x30 ; " \
           "ldr   q7, [%0, #0]   ; " \
           "ldr   q8, [%0, #16]   ; " \
           "ldr   q9, [%0, #32]   ; " \
           #INSN " v9." #SUFFIXD ", v7." #SUFFIXN ", v8." #SUFFIXM " ; " \
           "str   q9, [%0, #32] ; " \
           "mrs   x30, fpsr ; str x30, [%0, #48] " \
           : : "r"(&block[0]) : "memory", "v7", "v8", "v9", "x30" \
        ); \
        printf(#INSN   " v9." #SUFFIXD \
               ", v7." #SUFFIXN ", v8." #SUFFIXM "  ");   \
        UInt fpsr = 0xFFFFFF60 & block[3].u32[0]; \
        showV128(&block[0]); printf("  "); \
        showV128(&block[1]); printf("  "); \
        showV128(&block[2]); printf(" fpsr=%08x\n", fpsr); \
     } \
  }


/* Note this also sets the destination register to a known value (0x55..55)
   since it can sometimes be an input to the instruction too. */
#define GEN_SHIFT_TEST(INSN,SUFFIXD,SUFFIXN,AMOUNT) \
  __attribute__((noinline)) \
  static void test_##INSN##_##SUFFIXD##_##SUFFIXN##_##AMOUNT ( LaneTy ty ) { \
     Int i; \
     for (i = 0; i < ITERS; i++) { \
        V128 block[2+1]; \
        memset(block, 0x55, sizeof(block)); \
        randV128(&block[0], ty); \
        randV128(&block[1], ty); \
        __asm__ __volatile__( \
           "mov   x30, #0 ; msr fpsr, x30 ; " \
           "ldr   q7, [%0, #0]   ; " \
           "ldr   q8, [%0, #16]   ; " \
           #INSN " v8." #SUFFIXD ", v7." #SUFFIXN ", #" #AMOUNT " ; " \
           "str   q8, [%0, #16] ; " \
           "mrs   x30, fpsr ; str x30, [%0, #32] " \
           : : "r"(&block[0]) : "memory", "v7", "v8", "x30" \
        ); \
        printf(#INSN   " v8." #SUFFIXD ", v7." #SUFFIXN ", #" #AMOUNT "  "); \
        UInt fpsr = 0xFFFFFF60 & block[2].u32[0]; \
        showV128(&block[0]); printf("  "); \
        showV128(&block[1]); printf(" fpsr=%08x\n", fpsr); \
     } \
  }


/* Generate a test that involves one integer reg and one vector reg,
   with no bias as towards which is input or output. */
#define GEN_ONEINT_ONEVEC_TEST(TESTNAME,INSN,INTREGNO,VECREGNO) \
  __attribute__((noinline)) \
  static void test_##TESTNAME ( LaneTy ty ) { \
     Int i; \
     assert(INTREGNO != 30); \
     for (i = 0; i < ITERS; i++) { \
        V128 block[4+1]; \
        memset(block, 0x55, sizeof(block)); \
        randV128(&block[0], ty); \
        randV128(&block[1], ty); \
        randV128(&block[2], ty); \
        randV128(&block[3], ty); \
        __asm__ __volatile__( \
           "mov   x30, #0 ; msr fpsr, x30 ; " \
           "ldr   q"#VECREGNO", [%0, #0]  ; " \
           "ldr   x"#INTREGNO", [%0, #16] ; " \
           INSN " ; " \
           "str   q"#VECREGNO", [%0, #32] ; " \
           "str   x"#INTREGNO", [%0, #48] ; " \
           "mrs   x30, fpsr ; str x30, [%0, #64] " \
           : : "r"(&block[0]) : "memory", "v"#VECREGNO, "x"#INTREGNO, "x30" \
        ); \
        printf(INSN   "   "); \
        UInt fpsr = 0xFFFFFF60 & block[4].u32[0]; \
        showV128(&block[0]); printf("  "); \
        showV128(&block[1]); printf("  "); \
        showV128(&block[2]); printf("  "); \
        showV128(&block[3]); printf(" fpsr=%08x\n", fpsr); \
     } \
  }


/* Generate a test that involves two vector regs,
   with no bias as towards which is input or output.
   It's OK to use x10 as scratch.*/
#define GEN_TWOVEC_TEST(TESTNAME,INSN,VECREG1NO,VECREG2NO) \
  __attribute__((noinline)) \
  static void test_##TESTNAME ( LaneTy ty ) { \
     Int i; \
     for (i = 0; i < ITERS; i++) { \
        V128 block[4+1]; \
        memset(block, 0x55, sizeof(block)); \
        randV128(&block[0], ty); \
        randV128(&block[1], ty); \
        randV128(&block[2], ty); \
        randV128(&block[3], ty); \
        __asm__ __volatile__( \
           "mov   x30, #0 ; msr fpsr, x30 ; " \
           "ldr   q"#VECREG1NO", [%0, #0]  ; " \
           "ldr   q"#VECREG2NO", [%0, #16] ; " \
           INSN " ; " \
           "str   q"#VECREG1NO", [%0, #32] ; " \
           "str   q"#VECREG2NO", [%0, #48] ; " \
           "mrs   x30, fpsr ; str x30, [%0, #64] " \
           : : "r"(&block[0]) \
             : "memory", "v"#VECREG1NO, "v"#VECREG2NO, "x10", "x30" \
        ); \
        printf(INSN   "   "); \
        UInt fpsr = 0xFFFFFF60 & block[4].u32[0]; \
        showV128(&block[0]); printf("  "); \
        showV128(&block[1]); printf("  "); \
        showV128(&block[2]); printf("  "); \
        showV128(&block[3]); printf(" fpsr=%08x\n", fpsr); \
     } \
  }


/* Generate a test that involves three vector regs,
   with no bias as towards which is input or output.  It's also OK
   to use v16, v17, v18 as scratch. */
#define GEN_THREEVEC_TEST(TESTNAME,INSN,VECREG1NO,VECREG2NO,VECREG3NO)  \
  __attribute__((noinline)) \
  static void test_##TESTNAME ( LaneTy ty ) { \
     Int i; \
     for (i = 0; i < ITERS; i++) { \
        V128 block[6+1]; \
        memset(block, 0x55, sizeof(block)); \
        randV128(&block[0], ty); \
        randV128(&block[1], ty); \
        randV128(&block[2], ty); \
        randV128(&block[3], ty); \
        randV128(&block[4], ty); \
        randV128(&block[5], ty); \
        __asm__ __volatile__( \
           "mov   x30, #0 ; msr fpsr, x30 ; " \
           "ldr   q"#VECREG1NO", [%0, #0]  ; " \
           "ldr   q"#VECREG2NO", [%0, #16] ; " \
           "ldr   q"#VECREG3NO", [%0, #32] ; " \
           INSN " ; " \
           "str   q"#VECREG1NO", [%0, #48] ; " \
           "str   q"#VECREG2NO", [%0, #64] ; " \
           "str   q"#VECREG3NO", [%0, #80] ; " \
           "mrs   x30, fpsr ; str x30, [%0, #96] " \
           : : "r"(&block[0]) \
           : "memory", "v"#VECREG1NO, "v"#VECREG2NO, "v"#VECREG3NO, \
             "v16", "v17", "v18", "x30" \
        ); \
        printf(INSN   "   "); \
        UInt fpsr = 0xFFFFFF60 & block[6].u32[0]; \
        showV128(&block[0]); printf("  "); \
        showV128(&block[1]); printf("  "); \
        showV128(&block[2]); printf("  "); \
        showV128(&block[3]); printf("  "); \
        showV128(&block[4]); printf("  "); \
        showV128(&block[5]); printf(" fpsr=%08x\n", fpsr); \
     } \
  }


/* Generate a test that involves four vector regs,
   with no bias as towards which is input or output.  It's also OK
   to use v16, v17, v18 as scratch. */
#define GEN_FOURVEC_TEST(TESTNAME,INSN,VECREG1NO,VECREG2NO, \
                                       VECREG3NO,VECREG4NO)  \
  __attribute__((noinline)) \
  static void test_##TESTNAME ( LaneTy ty ) { \
     Int i; \
     for (i = 0; i < ITERS; i++) { \
        V128 block[8+1]; \
        memset(block, 0x55, sizeof(block)); \
        randV128(&block[0], ty); \
        randV128(&block[1], ty); \
        randV128(&block[2], ty); \
        randV128(&block[3], ty); \
        randV128(&block[4], ty); \
        randV128(&block[5], ty); \
        randV128(&block[6], ty); \
        randV128(&block[7], ty); \
        __asm__ __volatile__( \
           "mov   x30, #0 ; msr fpsr, x30 ; " \
           "ldr   q"#VECREG1NO", [%0, #0]  ; " \
           "ldr   q"#VECREG2NO", [%0, #16] ; " \
           "ldr   q"#VECREG3NO", [%0, #32] ; " \
           "ldr   q"#VECREG4NO", [%0, #48] ; " \
           INSN " ; " \
           "str   q"#VECREG1NO", [%0, #64] ; " \
           "str   q"#VECREG2NO", [%0, #80] ; " \
           "str   q"#VECREG3NO", [%0, #96] ; " \
           "str   q"#VECREG4NO", [%0, #112] ; " \
           "mrs   x30, fpsr ; str x30, [%0, #128] " \
           : : "r"(&block[0]) \
           : "memory", "v"#VECREG1NO, "v"#VECREG2NO, \
                       "v"#VECREG3NO, "v"#VECREG4NO, \
             "v16", "v17", "v18", "x30" \
        ); \
        printf(INSN   "   "); \
        UInt fpsr = 0xFFFFFF60 & block[8].u32[0]; \
        showV128(&block[0]); printf("  "); \
        showV128(&block[1]); printf("  "); \
        showV128(&block[2]); printf("  "); \
        showV128(&block[3]); printf("  "); \
        showV128(&block[4]); printf("  "); \
        showV128(&block[5]); printf("  "); \
        showV128(&block[6]); printf("  "); \
        showV128(&block[7]); printf(" fpsr=%08x\n", fpsr); \
     } \
  }


#endif /* ARM64_SIMD_H */
