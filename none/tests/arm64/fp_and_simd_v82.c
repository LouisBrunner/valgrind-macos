
#include <stdio.h>
#include <assert.h>
#include <malloc.h>  // memalign
#include <string.h>  // memset
#include "tests/malloc.h"
#include <math.h>    // isnormal

typedef  unsigned char           UChar;
typedef  unsigned short int      UShort;
typedef  unsigned int            UInt;
typedef  signed int              Int;
typedef  unsigned char           UChar;
typedef  unsigned long long int  ULong;
typedef  signed long long int    Long;
typedef  double                  Double;
typedef  float                   Float;
/* To test half-precision floating point instructions a synthesized 16 bit type
   is used rather than native __fp16. This allows gradual support of v8.2
   instructions without test binaries like this failing to run with Valgrind
   because a half-precision instruction which is not supported appears in the
   test binary. The functions halfToSingleFPAsInt() and shortToSingle() below
   are used to create a Float16 type for testing purposes. Float16 should be
   typedefed to __fp16 when all v8.2 instructions are supported.
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
static UInt halfToSingleFPAsInt(UShort y)
{
   int s = (y >> 15) & 0x00000001;  // Sign bit
   int e = (y >> 10) & 0x0000001f;  // Exponent
   int f =  y        & 0x000003ff;  // Fraction

   // Handle +/- INF (7c00 and fc00 -INF) and +/-0
   if (e == 0) {
      if (f == 0)
         return s << 31;
      else {                           // Normalize
         while (!(f & 0x00000400)) {
            f <<= 1;
            e -=  1;
         }
         e += 1;
         f &= ~0x00000400;
      }
   } else if (e == 31) {
      if (f == 0)                         // INF
         return (s << 31) | 0x7f800000;
      else                                // NaN
         return (s << 31) | 0x7f800000 | (f << 13);
   }

   e = e + (127 - 15);
   f = f << 13;

   return ((s << 31) | (e << 23) | f);
}

static float shortToSingle(UShort imm)
{
   union { float f; UInt i; } v;
   v.i = halfToSingleFPAsInt(imm);
   return v.f;
}

static inline UChar randUChar ( void )
{
   static UInt seed = 80021;
   seed = 1103515245 * seed + 12345;
   return (seed >> 17) & 0xFF;
}

static ULong randULong ( LaneTy ty )
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
static void randV128 ( /*OUT*/V128* v, LaneTy ty )
{
   static UInt nCalls = 0, nIters = 0;
   Int i;
   nCalls++;
   while (1) {
      nIters++;
      for (i = 0; i < 16; i++) {
         v->u8[i] = randUChar();
      }
      if (isnormal(v->f32[0]) && isnormal(v->f32[1]) && isnormal(v->f32[2])
          && isnormal(v->f32[3]) && isnormal(v->f64[0]) && isnormal(v->f64[1])
          && isnormal(shortToSingle(v->f16[0])) && isnormal(shortToSingle(v->f16[1]))
          && isnormal(shortToSingle(v->f16[2])) && isnormal(shortToSingle(v->f16[3]))
          && isnormal(shortToSingle(v->f16[4])) && isnormal(shortToSingle(v->f16[5]))
          && isnormal(shortToSingle(v->f16[6])) && isnormal(shortToSingle(v->f16[7]))) {
        break;
     }
   }
   if (0 == (nCalls & 0xFF))
      printf("randV128: %u calls, %u iters\n", nCalls, nIters);
}

static void showV128 ( V128* v )
{
   Int i;
   for (i = 15; i >= 0; i--)
      printf("%02x", (Int)v->u8[i]);
}

static void showBlock ( const char* msg, V128* block, Int nBlock )
{
   Int i;
   printf("%s\n", msg);
   for (i = 0; i < nBlock; i++) {
      printf("  ");
      showV128(&block[i]);
      printf("\n");
   }
}

static ULong dup4x16 ( UInt x )
{
   ULong r = x & 0xF;
   r |= (r << 4);
   r |= (r << 8);
   r |= (r << 16);
   r |= (r << 32);
   return r;
}

// Generate a random double-precision number.  About 1 time in 2,
// instead return a special value (+/- Inf, +/-Nan, denorm).
// This ensures that many of the groups of 4 calls here will
// return a special value.

static Double special_values[10];
static Bool   special_values_initted = False;

static __attribute__((noinline))
Double negate ( Double d ) { return -d; }
static __attribute__((noinline))
Double divf64 ( Double x, Double y ) { return x/y; }

static __attribute__((noinline))
Double plusZero  ( void ) { return 0.0; }
static __attribute__((noinline))
Double minusZero ( void ) { return negate(plusZero()); }

static __attribute__((noinline))
Double plusOne  ( void ) { return 1.0; }
static __attribute__((noinline))
Double minusOne ( void ) { return negate(plusOne()); }

static __attribute__((noinline))
Double plusInf   ( void ) { return 1.0 / 0.0; }
static __attribute__((noinline))
Double minusInf  ( void ) { return negate(plusInf()); }

static __attribute__((noinline))
Double plusNaN  ( void ) { return divf64(plusInf(),plusInf()); }
static __attribute__((noinline))
Double minusNaN ( void ) { return negate(plusNaN()); }

static __attribute__((noinline))
Double plusDenorm  ( void ) { return 1.23e-315 / 1e3; }
static __attribute__((noinline))
Double minusDenorm ( void ) { return negate(plusDenorm()); }


static void ensure_special_values_initted ( void )
{
   if (special_values_initted) return;
   special_values[0] = plusZero();
   special_values[1] = minusZero();
   special_values[2] = plusOne();
   special_values[3] = minusOne();
   special_values[4] = plusInf();
   special_values[5] = minusInf();
   special_values[6] = plusNaN();
   special_values[7] = minusNaN();
   special_values[8] = plusDenorm();
   special_values[9] = minusDenorm();
   special_values_initted = True;
   int i;
   printf("\n");
   for (i = 0; i < 10; i++) {
      printf("special value %d = %e\n", i, special_values[i]);
   }
   printf("\n");
}

static Double randDouble ( void )
{
   ensure_special_values_initted();
   UChar c = randUChar();
   if (c >= 128) {
      // return a normal number about half of the time.
      // 0 .. 2^63-1
      ULong u64 = randULong(TyDF);
      // -2^62 .. 2^62-1
      Long s64 = (Long)u64;
      // -2^55 .. 2^55-1
      s64 >>= (62-55);
      // and now as a float
      return (Double)s64;
   }
   c = randUChar() % 10;
   return special_values[c];
}

static Float randFloat ( void )
{
   ensure_special_values_initted();
   UChar c = randUChar();
   if (c >= 128) {
      // return a normal number about half of the time.
      // 0 .. 2^63-1
      ULong u64 = randULong(TyDF);
      // -2^62 .. 2^62-1
      Long s64 = (Long)u64;
      // -2^25 .. 2^25-1
      s64 >>= (62-25);
      // and now as a float
      return (Float)s64;
   }
   c = randUChar() % 10;
   return special_values[c];
}

void randBlock_Doubles ( V128* block, Int nBlock )
{
   Int i;
   for (i = 0; i < nBlock; i++) {
      block[i].f64[0] = randDouble();
      block[i].f64[1] = randDouble();
   }
}

void randBlock_Floats ( V128* block, Int nBlock )
{
   Int i;
   for (i = 0; i < nBlock; i++) {
      block[i].f32[0] = randFloat();
      block[i].f32[1] = randFloat();
      block[i].f32[2] = randFloat();
      block[i].f32[3] = randFloat();
   }
}


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


/* ---------------------------------------------------------------- */
/* -- Test functions and non-parameterisable test macros         -- */
/* ---------------------------------------------------------------- */

void test_UMINV ( void )
{
  int i;
  V128 block[2];

  /* -- 4s -- */

  for (i = 0; i < 10; i++) {
    memset(&block, 0x55, sizeof(block));
    randV128(&block[0], TyS);
    randV128(&block[1], TyS);
    __asm__ __volatile__(
       "ldr   q7, [%0, #0]   ; "
       "uminv s8, v7.4s   ; "
       "str   q8, [%0, #16] "
       : : "r"(&block[0]) : "memory", "v7", "v8"
                         );
    printf("UMINV v8, v7.4s  ");
    showV128(&block[0]); printf("  ");
    showV128(&block[1]); printf("\n");
  }

  /* -- 8h -- */

  for (i = 0; i < 10; i++) {
    memset(&block, 0x55, sizeof(block));
    randV128(&block[0], TyH);
    randV128(&block[1], TyH);
    __asm__ __volatile__(
       "ldr   q7, [%0, #0]   ; "
       "uminv h8, v7.8h   ; "
       "str   q8, [%0, #16] "
       : : "r"(&block[0]) : "memory", "v7", "v8"
                         );
    printf("UMINV h8, v7.8h  ");
    showV128(&block[0]); printf("  ");
    showV128(&block[1]); printf("\n");
  }

  /* -- 4h -- */

  for (i = 0; i < 10; i++) {
    memset(&block, 0x55, sizeof(block));
    randV128(&block[0], TyH);
    randV128(&block[1], TyH);
    __asm__ __volatile__(
       "ldr   q7, [%0, #0]   ; "
       "uminv h8, v7.4h   ; "
       "str   q8, [%0, #16] "
       : : "r"(&block[0]) : "memory", "v7", "v8"
                         );
    printf("UMINV h8, v7.4h  ");
    showV128(&block[0]); printf("  ");
    showV128(&block[1]); printf("\n");
  }

  /* -- 16b -- */

  for (i = 0; i < 10; i++) {
    memset(&block, 0x55, sizeof(block));
    randV128(&block[0], TyB);
    randV128(&block[1], TyB);
    __asm__ __volatile__(
       "ldr   q7, [%0, #0]   ; "
       "uminv b8, v7.16b   ; "
       "str   q8, [%0, #16] "
       : : "r"(&block[0]) : "memory", "v7", "v8"
                         );
    printf("UMINV b8, v7.16b  ");
    showV128(&block[0]); printf("  ");
    showV128(&block[1]); printf("\n");
  }

  /* -- 8b -- */

  for (i = 0; i < 10; i++) {
    memset(&block, 0x55, sizeof(block));
    randV128(&block[0], TyB);
    randV128(&block[1], TyB);
    __asm__ __volatile__(
       "ldr   q7, [%0, #0]   ; "
       "uminv b8, v7.8b   ; "
       "str   q8, [%0, #16] "
       : : "r"(&block[0]) : "memory", "v7", "v8"
                         );
    printf("UMINV b8, v7.8b  ");
    showV128(&block[0]); printf("  ");
    showV128(&block[1]); printf("\n");
  }

}


void test_UMAXV ( void )
{
  int i;
  V128 block[2];

  /* -- 4s -- */

  for (i = 0; i < 10; i++) {
    memset(&block, 0x55, sizeof(block));
    randV128(&block[0], TyS);
    randV128(&block[1], TyS);
    __asm__ __volatile__(
       "ldr   q7, [%0, #0]   ; "
       "umaxv s8, v7.4s   ; "
       "str   q8, [%0, #16] "
       : : "r"(&block[0]) : "memory", "v7", "v8"
                         );
    printf("UMAXV v8, v7.4s  ");
    showV128(&block[0]); printf("  ");
    showV128(&block[1]); printf("\n");
  }

  /* -- 8h -- */

  for (i = 0; i < 10; i++) {
    memset(&block, 0x55, sizeof(block));
    randV128(&block[0], TyH);
    randV128(&block[1], TyH);
    __asm__ __volatile__(
       "ldr   q7, [%0, #0]   ; "
       "umaxv h8, v7.8h   ; "
       "str   q8, [%0, #16] "
       : : "r"(&block[0]) : "memory", "v7", "v8"
                         );
    printf("UMAXV h8, v7.8h  ");
    showV128(&block[0]); printf("  ");
    showV128(&block[1]); printf("\n");
  }

  /* -- 4h -- */

  for (i = 0; i < 10; i++) {
    memset(&block, 0x55, sizeof(block));
    randV128(&block[0], TyH);
    randV128(&block[1], TyH);
    __asm__ __volatile__(
       "ldr   q7, [%0, #0]   ; "
       "umaxv h8, v7.4h   ; "
       "str   q8, [%0, #16] "
       : : "r"(&block[0]) : "memory", "v7", "v8"
                         );
    printf("UMAXV h8, v7.4h  ");
    showV128(&block[0]); printf("  ");
    showV128(&block[1]); printf("\n");
  }

  /* -- 16b -- */

  for (i = 0; i < 10; i++) {
    memset(&block, 0x55, sizeof(block));
    randV128(&block[0], TyB);
    randV128(&block[1], TyB);
    __asm__ __volatile__(
       "ldr   q7, [%0, #0]   ; "
       "umaxv b8, v7.16b   ; "
       "str   q8, [%0, #16] "
       : : "r"(&block[0]) : "memory", "v7", "v8"
                         );
    printf("UMAXV b8, v7.16b  ");
    showV128(&block[0]); printf("  ");
    showV128(&block[1]); printf("\n");
  }

  /* -- 8b -- */

  for (i = 0; i < 10; i++) {
    memset(&block, 0x55, sizeof(block));
    randV128(&block[0], TyB);
    randV128(&block[1], TyB);
    __asm__ __volatile__(
       "ldr   q7, [%0, #0]   ; "
       "umaxv b8, v7.8b   ; "
       "str   q8, [%0, #16] "
       : : "r"(&block[0]) : "memory", "v7", "v8"
                         );
    printf("UMAXV b8, v7.8b  ");
    showV128(&block[0]); printf("  ");
    showV128(&block[1]); printf("\n");
  }

}


void test_INS_general ( void )
{
  V128 block[3];

  /* -- D[0..1] -- */

  memset(&block, 0x55, sizeof(block));
  block[1].u64[0] = randULong(TyD);
  __asm__ __volatile__(
     "ldr q7, [%0, #0]   ; "
     "ldr x19, [%0, #16] ; "
     "ins v7.d[0], x19   ; "
     "str q7, [%0, #32] "
     : : "r"(&block[0]) : "memory", "x19", "v7"
  );
  printf("INS v7.u64[0],x19  ");
  showV128(&block[0]); printf("  %016llx  ", block[1].u64[0]);
  showV128(&block[2]); printf("\n");

  memset(&block, 0x55, sizeof(block));
  block[1].u64[0] = randULong(TyD);
  __asm__ __volatile__(
     "ldr q7, [%0, #0]   ; "
     "ldr x19, [%0, #16] ; "
     "ins v7.d[1], x19   ; "
     "str q7, [%0, #32] "
     : : "r"(&block[0]) : "memory", "x19", "v7"
  );
  printf("INS v7.d[1],x19  ");
  showV128(&block[0]); printf("  %016llx  ", block[1].u64[0]);
  showV128(&block[2]); printf("\n");

  /* -- S[0..3] -- */

  memset(&block, 0x55, sizeof(block));
  block[1].u64[0] = randULong(TyS);
  __asm__ __volatile__(
     "ldr q7, [%0, #0]   ; "
     "ldr x19, [%0, #16] ; "
     "ins v7.s[0], w19   ; "
     "str q7, [%0, #32] "
     : : "r"(&block[0]) : "memory", "x19", "v7"
  );
  printf("INS v7.s[0],x19  ");
  showV128(&block[0]); printf("  %016llx  ", block[1].u64[0]);
  showV128(&block[2]); printf("\n");

  memset(&block, 0x55, sizeof(block));
  block[1].u64[0] = randULong(TyS);
  __asm__ __volatile__(
     "ldr q7, [%0, #0]   ; "
     "ldr x19, [%0, #16] ; "
     "ins v7.s[1], w19   ; "
     "str q7, [%0, #32] "
     : : "r"(&block[0]) : "memory", "x19", "v7"
  );
  printf("INS v7.s[1],x19  ");
  showV128(&block[0]); printf("  %016llx  ", block[1].u64[0]);
  showV128(&block[2]); printf("\n");

  memset(&block, 0x55, sizeof(block));
  block[1].u64[0] = randULong(TyS);
  __asm__ __volatile__(
     "ldr q7, [%0, #0]   ; "
     "ldr x19, [%0, #16] ; "
     "ins v7.s[2], w19   ; "
     "str q7, [%0, #32] "
     : : "r"(&block[0]) : "memory", "x19", "v7"
  );
  printf("INS v7.s[2],x19  ");
  showV128(&block[0]); printf("  %016llx  ", block[1].u64[0]);
  showV128(&block[2]); printf("\n");

  memset(&block, 0x55, sizeof(block));
  block[1].u64[0] = randULong(TyS);
  __asm__ __volatile__(
     "ldr q7, [%0, #0]   ; "
     "ldr x19, [%0, #16] ; "
     "ins v7.s[3], w19   ; "
     "str q7, [%0, #32] "
     : : "r"(&block[0]) : "memory", "x19", "v7"
  );
  printf("INS v7.s[3],x19  ");
  showV128(&block[0]); printf("  %016llx  ", block[1].u64[0]);
  showV128(&block[2]); printf("\n");

  /* -- H[0..7] -- */

  memset(&block, 0x55, sizeof(block));
  block[1].u64[0] = randULong(TyH);
  __asm__ __volatile__(
     "ldr q7, [%0, #0]   ; "
     "ldr x19, [%0, #16] ; "
     "ins v7.h[0], w19   ; "
     "str q7, [%0, #32] "
     : : "r"(&block[0]) : "memory", "x19", "v7"
  );
  printf("INS v7.h[0],x19  ");
  showV128(&block[0]); printf("  %016llx  ", block[1].u64[0]);
  showV128(&block[2]); printf("\n");

  memset(&block, 0x55, sizeof(block));
  block[1].u64[0] = randULong(TyH);
  __asm__ __volatile__(
     "ldr q7, [%0, #0]   ; "
     "ldr x19, [%0, #16] ; "
     "ins v7.h[1], w19   ; "
     "str q7, [%0, #32] "
     : : "r"(&block[0]) : "memory", "x19", "v7"
  );
  printf("INS v7.h[1],x19  ");
  showV128(&block[0]); printf("  %016llx  ", block[1].u64[0]);
  showV128(&block[2]); printf("\n");

  memset(&block, 0x55, sizeof(block));
  block[1].u64[0] = randULong(TyH);
  __asm__ __volatile__(
     "ldr q7, [%0, #0]   ; "
     "ldr x19, [%0, #16] ; "
     "ins v7.h[2], w19   ; "
     "str q7, [%0, #32] "
     : : "r"(&block[0]) : "memory", "x19", "v7"
  );
  printf("INS v7.h[2],x19  ");
  showV128(&block[0]); printf("  %016llx  ", block[1].u64[0]);
  showV128(&block[2]); printf("\n");

  memset(&block, 0x55, sizeof(block));
  block[1].u64[0] = randULong(TyH);
  __asm__ __volatile__(
     "ldr q7, [%0, #0]   ; "
     "ldr x19, [%0, #16] ; "
     "ins v7.h[3], w19   ; "
     "str q7, [%0, #32] "
     : : "r"(&block[0]) : "memory", "x19", "v7"
  );
  printf("INS v7.h[3],x19  ");
  showV128(&block[0]); printf("  %016llx  ", block[1].u64[0]);
  showV128(&block[2]); printf("\n");

  memset(&block, 0x55, sizeof(block));
  block[1].u64[0] = randULong(TyH);
  __asm__ __volatile__(
     "ldr q7, [%0, #0]   ; "
     "ldr x19, [%0, #16] ; "
     "ins v7.h[4], w19   ; "
     "str q7, [%0, #32] "
     : : "r"(&block[0]) : "memory", "x19", "v7"
  );
  printf("INS v7.h[4],x19  ");
  showV128(&block[0]); printf("  %016llx  ", block[1].u64[0]);
  showV128(&block[2]); printf("\n");

  memset(&block, 0x55, sizeof(block));
  block[1].u64[0] = randULong(TyH);
  __asm__ __volatile__(
     "ldr q7, [%0, #0]   ; "
     "ldr x19, [%0, #16] ; "
     "ins v7.h[5], w19   ; "
     "str q7, [%0, #32] "
     : : "r"(&block[0]) : "memory", "x19", "v7"
  );
  printf("INS v7.h[5],x19  ");
  showV128(&block[0]); printf("  %016llx  ", block[1].u64[0]);
  showV128(&block[2]); printf("\n");

  memset(&block, 0x55, sizeof(block));
  block[1].u64[0] = randULong(TyH);
  __asm__ __volatile__(
     "ldr q7, [%0, #0]   ; "
     "ldr x19, [%0, #16] ; "
     "ins v7.h[6], w19   ; "
     "str q7, [%0, #32] "
     : : "r"(&block[0]) : "memory", "x19", "v7"
  );
  printf("INS v7.h[6],x19  ");
  showV128(&block[0]); printf("  %016llx  ", block[1].u64[0]);
  showV128(&block[2]); printf("\n");

  memset(&block, 0x55, sizeof(block));
  block[1].u64[0] = randULong(TyH);
  __asm__ __volatile__(
     "ldr q7, [%0, #0]   ; "
     "ldr x19, [%0, #16] ; "
     "ins v7.h[7], w19   ; "
     "str q7, [%0, #32] "
     : : "r"(&block[0]) : "memory", "x19", "v7"
  );
  printf("INS v7.h[7],x19  ");
  showV128(&block[0]); printf("  %016llx  ", block[1].u64[0]);
  showV128(&block[2]); printf("\n");

  /* -- B[0,15] -- */

  memset(&block, 0x55, sizeof(block));
  block[1].u64[0] = randULong(TyB);
  __asm__ __volatile__(
     "ldr q7, [%0, #0]   ; "
     "ldr x19, [%0, #16] ; "
     "ins v7.b[0], w19   ; "
     "str q7, [%0, #32] "
     : : "r"(&block[0]) : "memory", "x19", "v7"
  );
  printf("INS v7.b[0],x19  ");
  showV128(&block[0]); printf("  %016llx  ", block[1].u64[0]);
  showV128(&block[2]); printf("\n");

  memset(&block, 0x55, sizeof(block));
  block[1].u64[0] = randULong(TyB);
  __asm__ __volatile__(
     "ldr q7, [%0, #0]   ; "
     "ldr x19, [%0, #16] ; "
     "ins v7.b[15], w19   ; "
     "str q7, [%0, #32] "
     : : "r"(&block[0]) : "memory", "x19", "v7"
  );
  printf("INS v7.b[15],x19 ");
  showV128(&block[0]); printf("  %016llx  ", block[1].u64[0]);
  showV128(&block[2]); printf("\n");
}



void test_SMINV ( void )
{
  int i;
  V128 block[2];

  /* -- 4s -- */

  for (i = 0; i < 10; i++) {
    memset(&block, 0x55, sizeof(block));
    randV128(&block[0], TyS);
    randV128(&block[1], TyS);
    __asm__ __volatile__(
       "ldr   q7, [%0, #0]   ; "
       "sminv s8, v7.4s   ; "
       "str   q8, [%0, #16] "
       : : "r"(&block[0]) : "memory", "v7", "v8"
                         );
    printf("SMINV v8, v7.4s  ");
    showV128(&block[0]); printf("  ");
    showV128(&block[1]); printf("\n");
  }

  /* -- 8h -- */

  for (i = 0; i < 10; i++) {
    memset(&block, 0x55, sizeof(block));
    randV128(&block[0], TyH);
    randV128(&block[1], TyH);
    __asm__ __volatile__(
       "ldr   q7, [%0, #0]   ; "
       "sminv h8, v7.8h   ; "
       "str   q8, [%0, #16] "
       : : "r"(&block[0]) : "memory", "v7", "v8"
                         );
    printf("SMINV h8, v7.8h  ");
    showV128(&block[0]); printf("  ");
    showV128(&block[1]); printf("\n");
  }

  /* -- 4h -- */

  for (i = 0; i < 10; i++) {
    memset(&block, 0x55, sizeof(block));
    randV128(&block[0], TyH);
    randV128(&block[1], TyH);
    __asm__ __volatile__(
       "ldr   q7, [%0, #0]   ; "
       "sminv h8, v7.4h   ; "
       "str   q8, [%0, #16] "
       : : "r"(&block[0]) : "memory", "v7", "v8"
                         );
    printf("SMINV h8, v7.4h  ");
    showV128(&block[0]); printf("  ");
    showV128(&block[1]); printf("\n");
  }

  /* -- 16b -- */

  for (i = 0; i < 10; i++) {
    memset(&block, 0x55, sizeof(block));
    randV128(&block[0], TyB);
    randV128(&block[1], TyB);
    __asm__ __volatile__(
       "ldr   q7, [%0, #0]   ; "
       "sminv b8, v7.16b   ; "
       "str   q8, [%0, #16] "
       : : "r"(&block[0]) : "memory", "v7", "v8"
                         );
    printf("SMINV b8, v7.16b  ");
    showV128(&block[0]); printf("  ");
    showV128(&block[1]); printf("\n");
  }

  /* -- 8b -- */

  for (i = 0; i < 10; i++) {
    memset(&block, 0x55, sizeof(block));
    randV128(&block[0], TyB);
    randV128(&block[1], TyB);
    __asm__ __volatile__(
       "ldr   q7, [%0, #0]   ; "
       "sminv b8, v7.8b   ; "
       "str   q8, [%0, #16] "
       : : "r"(&block[0]) : "memory", "v7", "v8"
                         );
    printf("SMINV b8, v7.8b  ");
    showV128(&block[0]); printf("  ");
    showV128(&block[1]); printf("\n");
  }

}


void test_SMAXV ( void )
{
  int i;
  V128 block[2];

  /* -- 4s -- */

  for (i = 0; i < 10; i++) {
    memset(&block, 0x55, sizeof(block));
    randV128(&block[0], TyS);
    randV128(&block[1], TyS);
    __asm__ __volatile__(
       "ldr   q7, [%0, #0]   ; "
       "smaxv s8, v7.4s   ; "
       "str   q8, [%0, #16] "
       : : "r"(&block[0]) : "memory", "v7", "v8"
                         );
    printf("SMAXV v8, v7.4s  ");
    showV128(&block[0]); printf("  ");
    showV128(&block[1]); printf("\n");
  }

  /* -- 8h -- */

  for (i = 0; i < 10; i++) {
    memset(&block, 0x55, sizeof(block));
    randV128(&block[0], TyH);
    randV128(&block[1], TyH);
    __asm__ __volatile__(
       "ldr   q7, [%0, #0]   ; "
       "smaxv h8, v7.8h   ; "
       "str   q8, [%0, #16] "
       : : "r"(&block[0]) : "memory", "v7", "v8"
                         );
    printf("SMAXV h8, v7.8h  ");
    showV128(&block[0]); printf("  ");
    showV128(&block[1]); printf("\n");
  }

  /* -- 4h -- */

  for (i = 0; i < 10; i++) {
    memset(&block, 0x55, sizeof(block));
    randV128(&block[0], TyH);
    randV128(&block[1], TyH);
    __asm__ __volatile__(
       "ldr   q7, [%0, #0]   ; "
       "smaxv h8, v7.4h   ; "
       "str   q8, [%0, #16] "
       : : "r"(&block[0]) : "memory", "v7", "v8"
                         );
    printf("SMAXV h8, v7.4h  ");
    showV128(&block[0]); printf("  ");
    showV128(&block[1]); printf("\n");
  }

  /* -- 16b -- */

  for (i = 0; i < 10; i++) {
    memset(&block, 0x55, sizeof(block));
    randV128(&block[0], TyB);
    randV128(&block[1], TyB);
    __asm__ __volatile__(
       "ldr   q7, [%0, #0]   ; "
       "smaxv b8, v7.16b   ; "
       "str   q8, [%0, #16] "
       : : "r"(&block[0]) : "memory", "v7", "v8"
                         );
    printf("SMAXV b8, v7.16b  ");
    showV128(&block[0]); printf("  ");
    showV128(&block[1]); printf("\n");
  }

  /* -- 8b -- */

  for (i = 0; i < 10; i++) {
    memset(&block, 0x55, sizeof(block));
    randV128(&block[0], TyB);
    randV128(&block[1], TyB);
    __asm__ __volatile__(
       "ldr   q7, [%0, #0]   ; "
       "smaxv b8, v7.8b   ; "
       "str   q8, [%0, #16] "
       : : "r"(&block[0]) : "memory", "v7", "v8"
                         );
    printf("SMAXV b8, v7.8b  ");
    showV128(&block[0]); printf("  ");
    showV128(&block[1]); printf("\n");
  }

}


//======== FCCMP_D ========//

#define GEN_test_FCCMP_D_D_0xF_EQ \
  __attribute__((noinline)) static void test_FCCMP_D_D_0xF_EQ ( void ) \
  { \
     V128 block[4]; \
     randBlock_Doubles(&block[0], 3); \
     block[3].u64[0] = dup4x16(0x5); block[3].u64[1] = dup4x16(0xA); \
     showBlock("FCCMP_D_D_0xF_EQ before", &block[0], 4); \
     __asm__ __volatile__( \
        "ldr x9,  [%0, 48];  msr nzcv, x9; " \
        "ldr q29, [%0, #0];  ldr q11, [%0, #16];  ldr q9, [%0, #32]; " \
        "fccmp d29, d11, #0xf, eq; " \
        "mrs x9, nzcv; str x9, [%0, 48]; " \
        "str q29, [%0, #0];  str q11, [%0, #16];  str q9, [%0, #32]; " \
        ::"r"(&block[0]) : "x9","cc","memory","v9","v11","v29" \
     ); \
     showBlock("FCCMP_D_D_0xF_EQ after", &block[0], 4); \
     printf("\n"); \
  }

#define GEN_test_FCCMP_D_D_0xF_NE \
  __attribute__((noinline)) static void test_FCCMP_D_D_0xF_NE ( void ) \
  { \
     V128 block[4]; \
     randBlock_Doubles(&block[0], 3); \
     block[3].u64[0] = dup4x16(0x5); block[3].u64[1] = dup4x16(0xA); \
     showBlock("FCCMP_D_D_0xF_NE before", &block[0], 4); \
     __asm__ __volatile__( \
        "ldr x9,  [%0, 48];  msr nzcv, x9; " \
        "ldr q29, [%0, #0];  ldr q11, [%0, #16];  ldr q9, [%0, #32]; " \
        "fccmp d29, d11, #0xf, ne; " \
        "mrs x9, nzcv; str x9, [%0, 48]; " \
        "str q29, [%0, #0];  str q11, [%0, #16];  str q9, [%0, #32]; " \
        ::"r"(&block[0]) : "x9","cc","memory","v9","v11","v29" \
     ); \
     showBlock("FCCMP_D_D_0xF_NE after", &block[0], 4); \
     printf("\n"); \
  }

#define GEN_test_FCCMP_D_D_0x0_EQ \
  __attribute__((noinline)) static void test_FCCMP_D_D_0x0_EQ ( void ) \
  { \
     V128 block[4]; \
     randBlock_Doubles(&block[0], 3); \
     block[3].u64[0] = dup4x16(0x5); block[3].u64[1] = dup4x16(0xA); \
     showBlock("FCCMP_D_D_0x0_EQ before", &block[0], 4); \
     __asm__ __volatile__( \
        "ldr x9,  [%0, 48];  msr nzcv, x9; " \
        "ldr q29, [%0, #0];  ldr q11, [%0, #16];  ldr q9, [%0, #32]; " \
        "fccmp d29, d11, #0x0, eq; " \
        "mrs x9, nzcv; str x9, [%0, 48]; " \
        "str q29, [%0, #0];  str q11, [%0, #16];  str q9, [%0, #32]; " \
        ::"r"(&block[0]) : "x9","cc","memory","v9","v11","v29" \
     ); \
     showBlock("FCCMP_D_D_0x0_EQ after", &block[0], 4); \
     printf("\n"); \
  }

#define GEN_test_FCCMP_D_D_0x0_NE \
  __attribute__((noinline)) static void test_FCCMP_D_D_0x0_NE ( void ) \
  { \
     V128 block[4]; \
     randBlock_Doubles(&block[0], 3); \
     block[3].u64[0] = dup4x16(0x5); block[3].u64[1] = dup4x16(0xA); \
     showBlock("FCCMP_D_D_0x0_NE before", &block[0], 4); \
     __asm__ __volatile__( \
        "ldr x9,  [%0, 48];  msr nzcv, x9; " \
        "ldr q29, [%0, #0];  ldr q11, [%0, #16];  ldr q9, [%0, #32]; " \
        "fccmp d29, d11, #0x0, ne; " \
        "mrs x9, nzcv; str x9, [%0, 48]; " \
        "str q29, [%0, #0];  str q11, [%0, #16];  str q9, [%0, #32]; " \
        ::"r"(&block[0]) : "x9","cc","memory","v9","v11","v29" \
     ); \
     showBlock("FCCMP_D_D_0x0_NE after", &block[0], 4); \
     printf("\n"); \
  }

//======== FCCMP_S ========//

#define GEN_test_FCCMP_S_S_0xF_EQ \
  __attribute__((noinline)) static void test_FCCMP_S_S_0xF_EQ ( void ) \
  { \
     V128 block[4]; \
     randBlock_Floats(&block[0], 3); \
     block[3].u64[0] = dup4x16(0x5); block[3].u64[1] = dup4x16(0xA); \
     showBlock("FCCMP_S_S_0xF_EQ before", &block[0], 4); \
     __asm__ __volatile__( \
        "ldr x9,  [%0, 48];  msr nzcv, x9; " \
        "ldr q29, [%0, #0];  ldr q11, [%0, #16];  ldr q9, [%0, #32]; " \
        "fccmp s29, s11, #0xf, eq; " \
        "mrs x9, nzcv; str x9, [%0, 48]; " \
        "str q29, [%0, #0];  str q11, [%0, #16];  str q9, [%0, #32]; " \
        ::"r"(&block[0]) : "x9","cc","memory","v9","v11","v29" \
     ); \
     showBlock("FCCMP_S_S_0xF_EQ after", &block[0], 4); \
     printf("\n"); \
  }

#define GEN_test_FCCMP_S_S_0xF_NE \
  __attribute__((noinline)) static void test_FCCMP_S_S_0xF_NE ( void ) \
  { \
     V128 block[4]; \
     randBlock_Floats(&block[0], 3); \
     block[3].u64[0] = dup4x16(0x5); block[3].u64[1] = dup4x16(0xA); \
     showBlock("FCCMP_S_S_0xF_NE before", &block[0], 4); \
     __asm__ __volatile__( \
        "ldr x9,  [%0, 48];  msr nzcv, x9; " \
        "ldr q29, [%0, #0];  ldr q11, [%0, #16];  ldr q9, [%0, #32]; " \
        "fccmp s29, s11, #0xf, ne; " \
        "mrs x9, nzcv; str x9, [%0, 48]; " \
        "str q29, [%0, #0];  str q11, [%0, #16];  str q9, [%0, #32]; " \
        ::"r"(&block[0]) : "x9","cc","memory","v9","v11","v29" \
     ); \
     showBlock("FCCMP_S_S_0xF_NE after", &block[0], 4); \
     printf("\n"); \
  }

#define GEN_test_FCCMP_S_S_0x0_EQ \
  __attribute__((noinline)) static void test_FCCMP_S_S_0x0_EQ ( void ) \
  { \
     V128 block[4]; \
     randBlock_Floats(&block[0], 3); \
     block[3].u64[0] = dup4x16(0x5); block[3].u64[1] = dup4x16(0xA); \
     showBlock("FCCMP_S_S_0x0_EQ before", &block[0], 4); \
     __asm__ __volatile__( \
        "ldr x9,  [%0, 48];  msr nzcv, x9; " \
        "ldr q29, [%0, #0];  ldr q11, [%0, #16];  ldr q9, [%0, #32]; " \
        "fccmp s29, s11, #0x0, eq; " \
        "mrs x9, nzcv; str x9, [%0, 48]; " \
        "str q29, [%0, #0];  str q11, [%0, #16];  str q9, [%0, #32]; " \
        ::"r"(&block[0]) : "x9","cc","memory","v9","v11","v29" \
     ); \
     showBlock("FCCMP_S_S_0x0_EQ after", &block[0], 4); \
     printf("\n"); \
  }

#define GEN_test_FCCMP_S_S_0x0_NE \
  __attribute__((noinline)) static void test_FCCMP_S_S_0x0_NE ( void ) \
  { \
     V128 block[4]; \
     randBlock_Floats(&block[0], 3); \
     block[3].u64[0] = dup4x16(0x5); block[3].u64[1] = dup4x16(0xA); \
     showBlock("FCCMP_S_S_0x0_NE before", &block[0], 4); \
     __asm__ __volatile__( \
        "ldr x9,  [%0, 48];  msr nzcv, x9; " \
        "ldr q29, [%0, #0];  ldr q11, [%0, #16];  ldr q9, [%0, #32]; " \
        "fccmp s29, s11, #0x0, ne; " \
        "mrs x9, nzcv; str x9, [%0, 48]; " \
        "str q29, [%0, #0];  str q11, [%0, #16];  str q9, [%0, #32]; " \
        ::"r"(&block[0]) : "x9","cc","memory","v9","v11","v29" \
     ); \
     showBlock("FCCMP_S_S_0x0_NE after", &block[0], 4); \
     printf("\n"); \
  }

//======== FCCMPE_D ========//

#define GEN_test_FCCMPE_D_D_0xF_EQ \
  __attribute__((noinline)) static void test_FCCMPE_D_D_0xF_EQ ( void ) \
  { \
     V128 block[4]; \
     randBlock_Doubles(&block[0], 3); \
     block[3].u64[0] = dup4x16(0x5); block[3].u64[1] = dup4x16(0xA); \
     showBlock("FCCMPE_D_D_0xF_EQ before", &block[0], 4); \
     __asm__ __volatile__( \
        "ldr x9,  [%0, 48];  msr nzcv, x9; " \
        "ldr q29, [%0, #0];  ldr q11, [%0, #16];  ldr q9, [%0, #32]; " \
        "fccmpe d29, d11, #0xf, eq; " \
        "mrs x9, nzcv; str x9, [%0, 48]; " \
        "str q29, [%0, #0];  str q11, [%0, #16];  str q9, [%0, #32]; " \
        ::"r"(&block[0]) : "x9","cc","memory","v9","v11","v29" \
     ); \
     showBlock("FCCMPE_D_D_0xF_EQ after", &block[0], 4); \
     printf("\n"); \
  }

#define GEN_test_FCCMPE_D_D_0xF_NE \
  __attribute__((noinline)) static void test_FCCMPE_D_D_0xF_NE ( void ) \
  { \
     V128 block[4]; \
     randBlock_Doubles(&block[0], 3); \
     block[3].u64[0] = dup4x16(0x5); block[3].u64[1] = dup4x16(0xA); \
     showBlock("FCCMPE_D_D_0xF_NE before", &block[0], 4); \
     __asm__ __volatile__( \
        "ldr x9,  [%0, 48];  msr nzcv, x9; " \
        "ldr q29, [%0, #0];  ldr q11, [%0, #16];  ldr q9, [%0, #32]; " \
        "fccmpe d29, d11, #0xf, ne; " \
        "mrs x9, nzcv; str x9, [%0, 48]; " \
        "str q29, [%0, #0];  str q11, [%0, #16];  str q9, [%0, #32]; " \
        ::"r"(&block[0]) : "x9","cc","memory","v9","v11","v29" \
     ); \
     showBlock("FCCMPE_D_D_0xF_NE after", &block[0], 4); \
     printf("\n"); \
  }

#define GEN_test_FCCMPE_D_D_0x0_EQ \
  __attribute__((noinline)) static void test_FCCMPE_D_D_0x0_EQ ( void ) \
  { \
     V128 block[4]; \
     randBlock_Doubles(&block[0], 3); \
     block[3].u64[0] = dup4x16(0x5); block[3].u64[1] = dup4x16(0xA); \
     showBlock("FCCMPE_D_D_0x0_EQ before", &block[0], 4); \
     __asm__ __volatile__( \
        "ldr x9,  [%0, 48];  msr nzcv, x9; " \
        "ldr q29, [%0, #0];  ldr q11, [%0, #16];  ldr q9, [%0, #32]; " \
        "fccmpe d29, d11, #0x0, eq; " \
        "mrs x9, nzcv; str x9, [%0, 48]; " \
        "str q29, [%0, #0];  str q11, [%0, #16];  str q9, [%0, #32]; " \
        ::"r"(&block[0]) : "x9","cc","memory","v9","v11","v29" \
     ); \
     showBlock("FCCMPE_D_D_0x0_EQ after", &block[0], 4); \
     printf("\n"); \
  }

#define GEN_test_FCCMPE_D_D_0x0_NE \
  __attribute__((noinline)) static void test_FCCMPE_D_D_0x0_NE ( void ) \
  { \
     V128 block[4]; \
     randBlock_Doubles(&block[0], 3); \
     block[3].u64[0] = dup4x16(0x5); block[3].u64[1] = dup4x16(0xA); \
     showBlock("FCCMPE_D_D_0x0_NE before", &block[0], 4); \
     __asm__ __volatile__( \
        "ldr x9,  [%0, 48];  msr nzcv, x9; " \
        "ldr q29, [%0, #0];  ldr q11, [%0, #16];  ldr q9, [%0, #32]; " \
        "fccmpe d29, d11, #0x0, ne; " \
        "mrs x9, nzcv; str x9, [%0, 48]; " \
        "str q29, [%0, #0];  str q11, [%0, #16];  str q9, [%0, #32]; " \
        ::"r"(&block[0]) : "x9","cc","memory","v9","v11","v29" \
     ); \
     showBlock("FCCMPE_D_D_0x0_NE after", &block[0], 4); \
     printf("\n"); \
  }

//======== FCCMPE_S ========//

#define GEN_test_FCCMPE_S_S_0xF_EQ \
  __attribute__((noinline)) static void test_FCCMPE_S_S_0xF_EQ ( void ) \
  { \
     V128 block[4]; \
     randBlock_Floats(&block[0], 3); \
     block[3].u64[0] = dup4x16(0x5); block[3].u64[1] = dup4x16(0xA); \
     showBlock("FCCMP_S_S_0xF_EQ before", &block[0], 4); \
     __asm__ __volatile__( \
        "ldr x9,  [%0, 48];  msr nzcv, x9; " \
        "ldr q29, [%0, #0];  ldr q11, [%0, #16];  ldr q9, [%0, #32]; " \
        "fccmpe s29, s11, #0xf, eq; " \
        "mrs x9, nzcv; str x9, [%0, 48]; " \
        "str q29, [%0, #0];  str q11, [%0, #16];  str q9, [%0, #32]; " \
        ::"r"(&block[0]) : "x9","cc","memory","v9","v11","v29" \
     ); \
     showBlock("FCCMPE_S_S_0xF_EQ after", &block[0], 4); \
     printf("\n"); \
  }

#define GEN_test_FCCMPE_S_S_0xF_NE \
  __attribute__((noinline)) static void test_FCCMPE_S_S_0xF_NE ( void ) \
  { \
     V128 block[4]; \
     randBlock_Floats(&block[0], 3); \
     block[3].u64[0] = dup4x16(0x5); block[3].u64[1] = dup4x16(0xA); \
     showBlock("FCCMPE_S_S_0xF_NE before", &block[0], 4); \
     __asm__ __volatile__( \
        "ldr x9,  [%0, 48];  msr nzcv, x9; " \
        "ldr q29, [%0, #0];  ldr q11, [%0, #16];  ldr q9, [%0, #32]; " \
        "fccmpe s29, s11, #0xf, ne; " \
        "mrs x9, nzcv; str x9, [%0, 48]; " \
        "str q29, [%0, #0];  str q11, [%0, #16];  str q9, [%0, #32]; " \
        ::"r"(&block[0]) : "x9","cc","memory","v9","v11","v29" \
     ); \
     showBlock("FCCMPE_S_S_0xF_NE after", &block[0], 4); \
     printf("\n"); \
  }

#define GEN_test_FCCMPE_S_S_0x0_EQ \
  __attribute__((noinline)) static void test_FCCMPE_S_S_0x0_EQ ( void ) \
  { \
     V128 block[4]; \
     randBlock_Floats(&block[0], 3); \
     block[3].u64[0] = dup4x16(0x5); block[3].u64[1] = dup4x16(0xA); \
     showBlock("FCCMP_S_S_0x0_EQ before", &block[0], 4); \
     __asm__ __volatile__( \
        "ldr x9,  [%0, 48];  msr nzcv, x9; " \
        "ldr q29, [%0, #0];  ldr q11, [%0, #16];  ldr q9, [%0, #32]; " \
        "fccmpe s29, s11, #0x0, eq; " \
        "mrs x9, nzcv; str x9, [%0, 48]; " \
        "str q29, [%0, #0];  str q11, [%0, #16];  str q9, [%0, #32]; " \
        ::"r"(&block[0]) : "x9","cc","memory","v9","v11","v29" \
     ); \
     showBlock("FCCMPE_S_S_0x0_EQ after", &block[0], 4); \
     printf("\n"); \
  }

#define GEN_test_FCCMPE_S_S_0x0_NE \
  __attribute__((noinline)) static void test_FCCMPE_S_S_0x0_NE ( void ) \
  { \
     V128 block[4]; \
     randBlock_Floats(&block[0], 3); \
     block[3].u64[0] = dup4x16(0x5); block[3].u64[1] = dup4x16(0xA); \
     showBlock("FCCMP_S_S_0x0_NE before", &block[0], 4); \
     __asm__ __volatile__( \
        "ldr x9,  [%0, 48];  msr nzcv, x9; " \
        "ldr q29, [%0, #0];  ldr q11, [%0, #16];  ldr q9, [%0, #32]; " \
        "fccmpe s29, s11, #0x0, ne; " \
        "mrs x9, nzcv; str x9, [%0, 48]; " \
        "str q29, [%0, #0];  str q11, [%0, #16];  str q9, [%0, #32]; " \
        ::"r"(&block[0]) : "x9","cc","memory","v9","v11","v29" \
     ); \
     showBlock("FCCMPE_S_S_0x0_NE after", &block[0], 4); \
     printf("\n"); \
  }

//======== FCMEQ_D_D_D ========//

#define GEN_test_FCMEQ_D_D_D \
  __attribute__((noinline)) static void test_FCMEQ_D_D_D ( void ) \
  { \
     V128 block[4]; \
     randBlock_Doubles(&block[0], 3); \
     block[3].u64[0] = dup4x16(0x5); block[3].u64[1] = dup4x16(0xA); \
     showBlock("FCMEQ_D_D_D before", &block[0], 4); \
     __asm__ __volatile__( \
        "ldr x9,  [%0, 48];  msr nzcv, x9; " \
        "ldr q29, [%0, #0];  ldr q11, [%0, #16];  ldr q9, [%0, #32]; " \
        "fcmeq d29, d11, d9; " \
        "mrs x9, nzcv; str x9, [%0, 48]; " \
        "str q29, [%0, #0];  str q11, [%0, #16];  str q9, [%0, #32]; " \
        ::"r"(&block[0]) : "x9","cc","memory","v9","v11","v29" \
     ); \
     showBlock("FCMEQ_D_D_D after", &block[0], 4); \
     printf("\n"); \
  }

//======== FCMEQ_S_S_S ========//

#define GEN_test_FCMEQ_S_S_S \
  __attribute__((noinline)) static void test_FCMEQ_S_S_S ( void ) \
  { \
     V128 block[4]; \
     randBlock_Floats(&block[0], 3); \
     block[3].u64[0] = dup4x16(0x5); block[3].u64[1] = dup4x16(0xA); \
     showBlock("FCMEQ_S_S_S before", &block[0], 4); \
     __asm__ __volatile__( \
        "ldr x9,  [%0, 48];  msr nzcv, x9; " \
        "ldr q29, [%0, #0];  ldr q11, [%0, #16];  ldr q9, [%0, #32]; " \
        "fcmeq s29, s11, s9; " \
        "mrs x9, nzcv; str x9, [%0, 48]; " \
        "str q29, [%0, #0];  str q11, [%0, #16];  str q9, [%0, #32]; " \
        ::"r"(&block[0]) : "x9","cc","memory","v9","v11","v29" \
     ); \
     showBlock("FCMEQ_S_S_S after", &block[0], 4); \
     printf("\n"); \
  }

//======== FCMGE_D_D_D ========//

#define GEN_test_FCMGE_D_D_D \
  __attribute__((noinline)) static void test_FCMGE_D_D_D ( void ) \
  { \
     V128 block[4]; \
     randBlock_Doubles(&block[0], 3); \
     block[3].u64[0] = dup4x16(0x5); block[3].u64[1] = dup4x16(0xA); \
     showBlock("FCMGE_D_D_D before", &block[0], 4); \
     __asm__ __volatile__( \
        "ldr x9,  [%0, 48];  msr nzcv, x9; " \
        "ldr q29, [%0, #0];  ldr q11, [%0, #16];  ldr q9, [%0, #32]; " \
        "fcmge d29, d11, d9; " \
        "mrs x9, nzcv; str x9, [%0, 48]; " \
        "str q29, [%0, #0];  str q11, [%0, #16];  str q9, [%0, #32]; " \
        ::"r"(&block[0]) : "x9","cc","memory","v9","v11","v29" \
     ); \
     showBlock("FCMGE_D_D_D after", &block[0], 4); \
     printf("\n"); \
  }

//======== FCMGE_S_S_S ========//

#define GEN_test_FCMGE_S_S_S \
  __attribute__((noinline)) static void test_FCMGE_S_S_S ( void ) \
  { \
     V128 block[4]; \
     randBlock_Floats(&block[0], 3); \
     block[3].u64[0] = dup4x16(0x5); block[3].u64[1] = dup4x16(0xA); \
     showBlock("FCMGE_S_S_S before", &block[0], 4); \
     __asm__ __volatile__( \
        "ldr x9,  [%0, 48];  msr nzcv, x9; " \
        "ldr q29, [%0, #0];  ldr q11, [%0, #16];  ldr q9, [%0, #32]; " \
        "fcmge s29, s11, s9; " \
        "mrs x9, nzcv; str x9, [%0, 48]; " \
        "str q29, [%0, #0];  str q11, [%0, #16];  str q9, [%0, #32]; " \
        ::"r"(&block[0]) : "x9","cc","memory","v9","v11","v29" \
     ); \
     showBlock("FCMGE_S_S_S after", &block[0], 4); \
     printf("\n"); \
  }

//======== FCMGT_D_D_D ========//

#define GEN_test_FCMGT_D_D_D \
  __attribute__((noinline)) static void test_FCMGT_D_D_D ( void ) \
  { \
     V128 block[4]; \
     randBlock_Doubles(&block[0], 3); \
     block[3].u64[0] = dup4x16(0x5); block[3].u64[1] = dup4x16(0xA); \
     showBlock("FCMGT_D_D_D before", &block[0], 4); \
     __asm__ __volatile__( \
        "ldr x9,  [%0, 48];  msr nzcv, x9; " \
        "ldr q29, [%0, #0];  ldr q11, [%0, #16];  ldr q9, [%0, #32]; " \
        "fcmgt d29, d11, d9; " \
        "mrs x9, nzcv; str x9, [%0, 48]; " \
        "str q29, [%0, #0];  str q11, [%0, #16];  str q9, [%0, #32]; " \
        ::"r"(&block[0]) : "x9","cc","memory","v9","v11","v29" \
     ); \
     showBlock("FCMGT_D_D_D after", &block[0], 4); \
     printf("\n"); \
  }

//======== FCMGT_S_S_S ========//

#define GEN_test_FCMGT_S_S_S \
  __attribute__((noinline)) static void test_FCMGT_S_S_S ( void ) \
  { \
     V128 block[4]; \
     randBlock_Floats(&block[0], 3); \
     block[3].u64[0] = dup4x16(0x5); block[3].u64[1] = dup4x16(0xA); \
     showBlock("FCMGT_S_S_S before", &block[0], 4); \
     __asm__ __volatile__( \
        "ldr x9,  [%0, 48];  msr nzcv, x9; " \
        "ldr q29, [%0, #0];  ldr q11, [%0, #16];  ldr q9, [%0, #32]; " \
        "fcmgt s29, s11, s9; " \
        "mrs x9, nzcv; str x9, [%0, 48]; " \
        "str q29, [%0, #0];  str q11, [%0, #16];  str q9, [%0, #32]; " \
        ::"r"(&block[0]) : "x9","cc","memory","v9","v11","v29" \
     ); \
     showBlock("FCMGT_S_S_S after", &block[0], 4); \
     printf("\n"); \
  }

//======== FACGT_D_D_D ========//

#define GEN_test_FACGT_D_D_D \
  __attribute__((noinline)) static void test_FACGT_D_D_D ( void ) \
  { \
     V128 block[4]; \
     randBlock_Doubles(&block[0], 3); \
     block[3].u64[0] = dup4x16(0x5); block[3].u64[1] = dup4x16(0xA); \
     showBlock("FACGT_D_D_D before", &block[0], 4); \
     __asm__ __volatile__( \
        "ldr x9,  [%0, 48];  msr nzcv, x9; " \
        "ldr q29, [%0, #0];  ldr q11, [%0, #16];  ldr q9, [%0, #32]; " \
        "facgt d29, d11, d9; " \
        "mrs x9, nzcv; str x9, [%0, 48]; " \
        "str q29, [%0, #0];  str q11, [%0, #16];  str q9, [%0, #32]; " \
        ::"r"(&block[0]) : "x9","cc","memory","v9","v11","v29" \
     ); \
     showBlock("FACGT_D_D_D after", &block[0], 4); \
     printf("\n"); \
  }

//======== FACGT_S_S_S ========//

#define GEN_test_FACGT_S_S_S \
  __attribute__((noinline)) static void test_FACGT_S_S_S ( void ) \
  { \
     V128 block[4]; \
     randBlock_Floats(&block[0], 3); \
     block[3].u64[0] = dup4x16(0x5); block[3].u64[1] = dup4x16(0xA); \
     showBlock("FACGT_S_S_S before", &block[0], 4); \
     __asm__ __volatile__( \
        "ldr x9,  [%0, 48];  msr nzcv, x9; " \
        "ldr q29, [%0, #0];  ldr q11, [%0, #16];  ldr q9, [%0, #32]; " \
        "facgt s29, s11, s9; " \
        "mrs x9, nzcv; str x9, [%0, 48]; " \
        "str q29, [%0, #0];  str q11, [%0, #16];  str q9, [%0, #32]; " \
        ::"r"(&block[0]) : "x9","cc","memory","v9","v11","v29" \
     ); \
     showBlock("FACGT_S_S_S after", &block[0], 4); \
     printf("\n"); \
  }

//======== FACGE_D_D_D ========//

#define GEN_test_FACGE_D_D_D \
  __attribute__((noinline)) static void test_FACGE_D_D_D ( void ) \
  { \
     V128 block[4]; \
     randBlock_Doubles(&block[0], 3); \
     block[3].u64[0] = dup4x16(0x5); block[3].u64[1] = dup4x16(0xA); \
     showBlock("FACGE_D_D_D before", &block[0], 4); \
     __asm__ __volatile__( \
        "ldr x9,  [%0, 48];  msr nzcv, x9; " \
        "ldr q29, [%0, #0];  ldr q11, [%0, #16];  ldr q9, [%0, #32]; " \
        "facge d29, d11, d9; " \
        "mrs x9, nzcv; str x9, [%0, 48]; " \
        "str q29, [%0, #0];  str q11, [%0, #16];  str q9, [%0, #32]; " \
        ::"r"(&block[0]) : "x9","cc","memory","v9","v11","v29" \
     ); \
     showBlock("FACGE_D_D_D after", &block[0], 4); \
     printf("\n"); \
  }

//======== FACGE_S_S_S ========//

#define GEN_test_FACGE_S_S_S \
  __attribute__((noinline)) static void test_FACGE_S_S_S ( void ) \
  { \
     V128 block[4]; \
     randBlock_Floats(&block[0], 3); \
     block[3].u64[0] = dup4x16(0x5); block[3].u64[1] = dup4x16(0xA); \
     showBlock("FACGE_S_S_S before", &block[0], 4); \
     __asm__ __volatile__( \
        "ldr x9,  [%0, 48];  msr nzcv, x9; " \
        "ldr q29, [%0, #0];  ldr q11, [%0, #16];  ldr q9, [%0, #32]; " \
        "facge s29, s11, s9; " \
        "mrs x9, nzcv; str x9, [%0, 48]; " \
        "str q29, [%0, #0];  str q11, [%0, #16];  str q9, [%0, #32]; " \
        ::"r"(&block[0]) : "x9","cc","memory","v9","v11","v29" \
     ); \
     showBlock("FACGE_S_S_S after", &block[0], 4); \
     printf("\n"); \
  }

//======== FCMEQ_D_D_Z ========//

#define GEN_test_FCMEQ_D_D_Z \
  __attribute__((noinline)) static void test_FCMEQ_D_D_Z ( void ) \
  { \
     V128 block[4]; \
     randBlock_Doubles(&block[0], 3); \
     block[3].u64[0] = dup4x16(0x5); block[3].u64[1] = dup4x16(0xA); \
     showBlock("FCMEQ_D_D_Z before", &block[0], 4); \
     __asm__ __volatile__( \
        "ldr x9,  [%0, 48];  msr nzcv, x9; " \
        "ldr q29, [%0, #0];  ldr q11, [%0, #16];  ldr q9, [%0, #32]; " \
        "fcmeq d29, d11, #0; " \
        "mrs x9, nzcv; str x9, [%0, 48]; " \
        "str q29, [%0, #0];  str q11, [%0, #16];  str q9, [%0, #32]; " \
        ::"r"(&block[0]) : "x9","cc","memory","v9","v11","v29" \
     ); \
     showBlock("FCMEQ_D_D_Z after", &block[0], 4); \
     printf("\n"); \
  }

//======== FCMEQ_S_S_Z ========//

#define GEN_test_FCMEQ_S_S_Z \
  __attribute__((noinline)) static void test_FCMEQ_S_S_Z ( void ) \
  { \
     V128 block[4]; \
     randBlock_Floats(&block[0], 3); \
     block[3].u64[0] = dup4x16(0x5); block[3].u64[1] = dup4x16(0xA); \
     showBlock("FCMEQ_S_S_Z before", &block[0], 4); \
     __asm__ __volatile__( \
        "ldr x9,  [%0, 48];  msr nzcv, x9; " \
        "ldr q29, [%0, #0];  ldr q11, [%0, #16];  ldr q9, [%0, #32]; " \
        "fcmeq s29, s11, #0; " \
        "mrs x9, nzcv; str x9, [%0, 48]; " \
        "str q29, [%0, #0];  str q11, [%0, #16];  str q9, [%0, #32]; " \
        ::"r"(&block[0]) : "x9","cc","memory","v9","v11","v29" \
     ); \
     showBlock("FCMEQ_S_S_Z after", &block[0], 4); \
     printf("\n"); \
  }

//======== FCMGE_D_D_Z ========//

#define GEN_test_FCMGE_D_D_Z \
  __attribute__((noinline)) static void test_FCMGE_D_D_Z ( void ) \
  { \
     V128 block[4]; \
     randBlock_Doubles(&block[0], 3); \
     block[3].u64[0] = dup4x16(0x5); block[3].u64[1] = dup4x16(0xA); \
     showBlock("FCMGE_D_D_Z before", &block[0], 4); \
     __asm__ __volatile__( \
        "ldr x9,  [%0, 48];  msr nzcv, x9; " \
        "ldr q29, [%0, #0];  ldr q11, [%0, #16];  ldr q9, [%0, #32]; " \
        "fcmge d29, d11, #0; " \
        "mrs x9, nzcv; str x9, [%0, 48]; " \
        "str q29, [%0, #0];  str q11, [%0, #16];  str q9, [%0, #32]; " \
        ::"r"(&block[0]) : "x9","cc","memory","v9","v11","v29" \
     ); \
     showBlock("FCMGE_D_D_Z after", &block[0], 4); \
     printf("\n"); \
  }

//======== FCMGE_S_S_Z ========//

#define GEN_test_FCMGE_S_S_Z \
  __attribute__((noinline)) static void test_FCMGE_S_S_Z ( void ) \
  { \
     V128 block[4]; \
     randBlock_Floats(&block[0], 3); \
     block[3].u64[0] = dup4x16(0x5); block[3].u64[1] = dup4x16(0xA); \
     showBlock("FCMGE_S_S_Z before", &block[0], 4); \
     __asm__ __volatile__( \
        "ldr x9,  [%0, 48];  msr nzcv, x9; " \
        "ldr q29, [%0, #0];  ldr q11, [%0, #16];  ldr q9, [%0, #32]; " \
        "fcmge s29, s11, #0; " \
        "mrs x9, nzcv; str x9, [%0, 48]; " \
        "str q29, [%0, #0];  str q11, [%0, #16];  str q9, [%0, #32]; " \
        ::"r"(&block[0]) : "x9","cc","memory","v9","v11","v29" \
     ); \
     showBlock("FCMGE_S_S_Z after", &block[0], 4); \
     printf("\n"); \
  }

//======== FCMGT_D_D_Z ========//

#define GEN_test_FCMGT_D_D_Z \
  __attribute__((noinline)) static void test_FCMGT_D_D_Z ( void ) \
  { \
     V128 block[4]; \
     randBlock_Doubles(&block[0], 3); \
     block[3].u64[0] = dup4x16(0x5); block[3].u64[1] = dup4x16(0xA); \
     showBlock("FCMGT_D_D_Z before", &block[0], 4); \
     __asm__ __volatile__( \
        "ldr x9,  [%0, 48];  msr nzcv, x9; " \
        "ldr q29, [%0, #0];  ldr q11, [%0, #16];  ldr q9, [%0, #32]; " \
        "fcmgt d29, d11, #0; " \
        "mrs x9, nzcv; str x9, [%0, 48]; " \
        "str q29, [%0, #0];  str q11, [%0, #16];  str q9, [%0, #32]; " \
        ::"r"(&block[0]) : "x9","cc","memory","v9","v11","v29" \
     ); \
     showBlock("FCMGT_D_D_Z after", &block[0], 4); \
     printf("\n"); \
  }

//======== FCMGT_S_S_Z ========//

#define GEN_test_FCMGT_S_S_Z \
  __attribute__((noinline)) static void test_FCMGT_S_S_Z ( void ) \
  { \
     V128 block[4]; \
     randBlock_Floats(&block[0], 3); \
     block[3].u64[0] = dup4x16(0x5); block[3].u64[1] = dup4x16(0xA); \
     showBlock("FCMGT_S_S_Z before", &block[0], 4); \
     __asm__ __volatile__( \
        "ldr x9,  [%0, 48];  msr nzcv, x9; " \
        "ldr q29, [%0, #0];  ldr q11, [%0, #16];  ldr q9, [%0, #32]; " \
        "fcmgt s29, s11, #0; " \
        "mrs x9, nzcv; str x9, [%0, 48]; " \
        "str q29, [%0, #0];  str q11, [%0, #16];  str q9, [%0, #32]; " \
        ::"r"(&block[0]) : "x9","cc","memory","v9","v11","v29" \
     ); \
     showBlock("FCMGT_S_S_Z after", &block[0], 4); \
     printf("\n"); \
  }

//======== FCMLE_D_D_Z ========//

#define GEN_test_FCMLE_D_D_Z \
  __attribute__((noinline)) static void test_FCMLE_D_D_Z ( void ) \
  { \
     V128 block[4]; \
     randBlock_Doubles(&block[0], 3); \
     block[3].u64[0] = dup4x16(0x5); block[3].u64[1] = dup4x16(0xA); \
     showBlock("FCMLE_D_D_Z before", &block[0], 4); \
     __asm__ __volatile__( \
        "ldr x9,  [%0, 48];  msr nzcv, x9; " \
        "ldr q29, [%0, #0];  ldr q11, [%0, #16];  ldr q9, [%0, #32]; " \
        "fcmle d29, d11, #0; " \
        "mrs x9, nzcv; str x9, [%0, 48]; " \
        "str q29, [%0, #0];  str q11, [%0, #16];  str q9, [%0, #32]; " \
        ::"r"(&block[0]) : "x9","cc","memory","v9","v11","v29" \
     ); \
     showBlock("FCMLE_D_D_Z after", &block[0], 4); \
     printf("\n"); \
  }

//======== FCMLE_S_S_Z ========//

#define GEN_test_FCMLE_S_S_Z \
  __attribute__((noinline)) static void test_FCMLE_S_S_Z ( void ) \
  { \
     V128 block[4]; \
     randBlock_Floats(&block[0], 3); \
     block[3].u64[0] = dup4x16(0x5); block[3].u64[1] = dup4x16(0xA); \
     showBlock("FCMLE_S_S_Z before", &block[0], 4); \
     __asm__ __volatile__( \
        "ldr x9,  [%0, 48];  msr nzcv, x9; " \
        "ldr q29, [%0, #0];  ldr q11, [%0, #16];  ldr q9, [%0, #32]; " \
        "fcmle s29, s11, #0; " \
        "mrs x9, nzcv; str x9, [%0, 48]; " \
        "str q29, [%0, #0];  str q11, [%0, #16];  str q9, [%0, #32]; " \
        ::"r"(&block[0]) : "x9","cc","memory","v9","v11","v29" \
     ); \
     showBlock("FCMLE_S_S_Z after", &block[0], 4); \
     printf("\n"); \
  }

//======== FCMLT_D_D_Z ========//

#define GEN_test_FCMLT_D_D_Z \
  __attribute__((noinline)) static void test_FCMLT_D_D_Z ( void ) \
  { \
     V128 block[4]; \
     randBlock_Doubles(&block[0], 3); \
     block[3].u64[0] = dup4x16(0x5); block[3].u64[1] = dup4x16(0xA); \
     showBlock("FCMLT_D_D_Z before", &block[0], 4); \
     __asm__ __volatile__( \
        "ldr x9,  [%0, 48];  msr nzcv, x9; " \
        "ldr q29, [%0, #0];  ldr q11, [%0, #16];  ldr q9, [%0, #32]; " \
        "fcmlt d29, d11, #0; " \
        "mrs x9, nzcv; str x9, [%0, 48]; " \
        "str q29, [%0, #0];  str q11, [%0, #16];  str q9, [%0, #32]; " \
        ::"r"(&block[0]) : "x9","cc","memory","v9","v11","v29" \
     ); \
     showBlock("FCMLT_D_D_Z after", &block[0], 4); \
     printf("\n"); \
  }

//======== FCMLT_S_S_Z ========//

#define GEN_test_FCMLT_S_S_Z \
  __attribute__((noinline)) static void test_FCMLT_S_S_Z ( void ) \
  { \
     V128 block[4]; \
     randBlock_Floats(&block[0], 3); \
     block[3].u64[0] = dup4x16(0x5); block[3].u64[1] = dup4x16(0xA); \
     showBlock("FCMLT_S_S_Z before", &block[0], 4); \
     __asm__ __volatile__( \
        "ldr x9,  [%0, 48];  msr nzcv, x9; " \
        "ldr q29, [%0, #0];  ldr q11, [%0, #16];  ldr q9, [%0, #32]; " \
        "fcmlt s29, s11, #0; " \
        "mrs x9, nzcv; str x9, [%0, 48]; " \
        "str q29, [%0, #0];  str q11, [%0, #16];  str q9, [%0, #32]; " \
        ::"r"(&block[0]) : "x9","cc","memory","v9","v11","v29" \
     ); \
     showBlock("FCMLT_S_S_Z after", &block[0], 4); \
     printf("\n"); \
  }

//======== FCMP_D_D ========//

#define GEN_test_FCMP_D_D \
  __attribute__((noinline)) static void test_FCMP_D_D ( void ) \
  { \
     V128 block[4]; \
     randBlock_Doubles(&block[0], 3); \
     block[3].u64[0] = dup4x16(0x5); block[3].u64[1] = dup4x16(0xA); \
     showBlock("FCMP_D_D before", &block[0], 4); \
     __asm__ __volatile__( \
        "ldr x9,  [%0, 48];  msr nzcv, x9; " \
        "ldr q29, [%0, #0];  ldr q11, [%0, #16];  ldr q9, [%0, #32]; " \
        "fcmp d29, d11; " \
        "mrs x9, nzcv; str x9, [%0, 48]; " \
        "str q29, [%0, #0];  str q11, [%0, #16];  str q9, [%0, #32]; " \
        ::"r"(&block[0]) : "x9","cc","memory","v9","v11","v29" \
     ); \
     showBlock("FCMP_D_D after", &block[0], 4); \
     printf("\n"); \
  }

//======== FCMP_S_S ========//

#define GEN_test_FCMP_S_S \
  __attribute__((noinline)) static void test_FCMP_S_S ( void ) \
  { \
     V128 block[4]; \
     randBlock_Floats(&block[0], 3); \
     block[3].u64[0] = dup4x16(0x5); block[3].u64[1] = dup4x16(0xA); \
     showBlock("FCMP_S_S before", &block[0], 4); \
     __asm__ __volatile__( \
        "ldr x9,  [%0, 48];  msr nzcv, x9; " \
        "ldr q29, [%0, #0];  ldr q11, [%0, #16];  ldr q9, [%0, #32]; " \
        "fcmp s29, s11; " \
        "mrs x9, nzcv; str x9, [%0, 48]; " \
        "str q29, [%0, #0];  str q11, [%0, #16];  str q9, [%0, #32]; " \
        ::"r"(&block[0]) : "x9","cc","memory","v9","v11","v29" \
     ); \
     showBlock("FCMP_S_S after", &block[0], 4); \
     printf("\n"); \
  }

//======== FCMPE_D_D ========//

#define GEN_test_FCMPE_D_D \
  __attribute__((noinline)) static void test_FCMPE_D_D ( void ) \
  { \
     V128 block[4]; \
     randBlock_Doubles(&block[0], 3); \
     block[3].u64[0] = dup4x16(0x5); block[3].u64[1] = dup4x16(0xA); \
     showBlock("FCMPE_D_D before", &block[0], 4); \
     __asm__ __volatile__( \
        "ldr x9,  [%0, 48];  msr nzcv, x9; " \
        "ldr q29, [%0, #0];  ldr q11, [%0, #16];  ldr q9, [%0, #32]; " \
        "fcmpe d29, d11; " \
        "mrs x9, nzcv; str x9, [%0, 48]; " \
        "str q29, [%0, #0];  str q11, [%0, #16];  str q9, [%0, #32]; " \
        ::"r"(&block[0]) : "x9","cc","memory","v9","v11","v29" \
     ); \
     showBlock("FCMPE_D_D after", &block[0], 4); \
     printf("\n"); \
  }

//======== FCMPE_S_S ========//

#define GEN_test_FCMPE_S_S \
  __attribute__((noinline)) static void test_FCMPE_S_S ( void ) \
  { \
     V128 block[4]; \
     randBlock_Floats(&block[0], 3); \
     block[3].u64[0] = dup4x16(0x5); block[3].u64[1] = dup4x16(0xA); \
     showBlock("FCMPE_S_S before", &block[0], 4); \
     __asm__ __volatile__( \
        "ldr x9,  [%0, 48];  msr nzcv, x9; " \
        "ldr q29, [%0, #0];  ldr q11, [%0, #16];  ldr q9, [%0, #32]; " \
        "fcmpe s29, s11; " \
        "mrs x9, nzcv; str x9, [%0, 48]; " \
        "str q29, [%0, #0];  str q11, [%0, #16];  str q9, [%0, #32]; " \
        ::"r"(&block[0]) : "x9","cc","memory","v9","v11","v29" \
     ); \
     showBlock("FCMPE_S_S after", &block[0], 4); \
     printf("\n"); \
  }

//======== FCMP_D_Z ========//

#define GEN_test_FCMP_D_Z \
  __attribute__((noinline)) static void test_FCMP_D_Z ( void ) \
  { \
     V128 block[4]; \
     randBlock_Doubles(&block[0], 3); \
     block[3].u64[0] = dup4x16(0x5); block[3].u64[1] = dup4x16(0xA); \
     showBlock("FCMP_D_Z before", &block[0], 4); \
     __asm__ __volatile__( \
        "ldr x9,  [%0, 48];  msr nzcv, x9; " \
        "ldr q29, [%0, #0];  ldr q11, [%0, #16];  ldr q9, [%0, #32]; " \
        "fcmp d29, #0; " \
        "mrs x9, nzcv; str x9, [%0, 48]; " \
        "str q29, [%0, #0];  str q11, [%0, #16];  str q9, [%0, #32]; " \
        ::"r"(&block[0]) : "x9","cc","memory","v9","v11","v29" \
     ); \
     showBlock("FCMP_D_Z after", &block[0], 4); \
     printf("\n"); \
  }

//======== FCMP_S_Z ========//

#define GEN_test_FCMP_S_Z \
  __attribute__((noinline)) static void test_FCMP_S_Z ( void ) \
  { \
     V128 block[4]; \
     randBlock_Floats(&block[0], 3); \
     block[3].u64[0] = dup4x16(0x5); block[3].u64[1] = dup4x16(0xA); \
     showBlock("FCMP_S_Z before", &block[0], 4); \
     __asm__ __volatile__( \
        "ldr x9,  [%0, 48];  msr nzcv, x9; " \
        "ldr q29, [%0, #0];  ldr q11, [%0, #16];  ldr q9, [%0, #32]; " \
        "fcmp s29, #0; " \
        "mrs x9, nzcv; str x9, [%0, 48]; " \
        "str q29, [%0, #0];  str q11, [%0, #16];  str q9, [%0, #32]; " \
        ::"r"(&block[0]) : "x9","cc","memory","v9","v11","v29" \
     ); \
     showBlock("FCMP_S_Z after", &block[0], 4); \
     printf("\n"); \
  }

//======== FCMPE_D_Z ========//

#define GEN_test_FCMPE_D_Z \
  __attribute__((noinline)) static void test_FCMPE_D_Z ( void ) \
  { \
     V128 block[4]; \
     randBlock_Doubles(&block[0], 3); \
     block[3].u64[0] = dup4x16(0x5); block[3].u64[1] = dup4x16(0xA); \
     showBlock("FCMPE_D_Z before", &block[0], 4); \
     __asm__ __volatile__( \
        "ldr x9,  [%0, 48];  msr nzcv, x9; " \
        "ldr q29, [%0, #0];  ldr q11, [%0, #16];  ldr q9, [%0, #32]; " \
        "fcmpe d29, #0; " \
        "mrs x9, nzcv; str x9, [%0, 48]; " \
        "str q29, [%0, #0];  str q11, [%0, #16];  str q9, [%0, #32]; " \
        ::"r"(&block[0]) : "x9","cc","memory","v9","v11","v29" \
     ); \
     showBlock("FCMPE_D_Z after", &block[0], 4); \
     printf("\n"); \
  }

//======== FCMPE_S_Z ========//

#define GEN_test_FCMPE_S_Z \
  __attribute__((noinline)) static void test_FCMPE_S_Z ( void ) \
  { \
     V128 block[4]; \
     randBlock_Floats(&block[0], 3); \
     block[3].u64[0] = dup4x16(0x5); block[3].u64[1] = dup4x16(0xA); \
     showBlock("FCMPE_S_Z before", &block[0], 4); \
     __asm__ __volatile__( \
        "ldr x9,  [%0, 48];  msr nzcv, x9; " \
        "ldr q29, [%0, #0];  ldr q11, [%0, #16];  ldr q9, [%0, #32]; " \
        "fcmpe s29, #0; " \
        "mrs x9, nzcv; str x9, [%0, 48]; " \
        "str q29, [%0, #0];  str q11, [%0, #16];  str q9, [%0, #32]; " \
        ::"r"(&block[0]) : "x9","cc","memory","v9","v11","v29" \
     ); \
     showBlock("FCMPE_S_Z after", &block[0], 4); \
     printf("\n"); \
  }

//======== FCSEL_D_D_D_EQ ========//

#define GEN_test_FCSEL_D_D_D_EQ \
  __attribute__((noinline)) static void test_FCSEL_D_D_D_EQ ( void ) \
  { \
     V128 block[4]; \
     randBlock_Doubles(&block[0], 3); \
     block[3].u64[0] = dup4x16(0x5); block[3].u64[1] = dup4x16(0xA); \
     showBlock("FCSEL_D_D_D_EQ before", &block[0], 4); \
     __asm__ __volatile__( \
        "ldr x9,  [%0, 48];  msr nzcv, x9; " \
        "ldr q29, [%0, #0];  ldr q11, [%0, #16];  ldr q9, [%0, #32]; " \
        "fcsel d29, d11, d9, eq; " \
        "mrs x9, nzcv; str x9, [%0, 48]; " \
        "str q29, [%0, #0];  str q11, [%0, #16];  str q9, [%0, #32]; " \
        ::"r"(&block[0]) : "x9","cc","memory","v9","v11","v29" \
     ); \
     showBlock("FCSEL_D_D_D_EQ after", &block[0], 4); \
     printf("\n"); \
  }

//======== FCSEL_D_D_D_NE ========//

#define GEN_test_FCSEL_D_D_D_NE \
  __attribute__((noinline)) static void test_FCSEL_D_D_D_NE ( void ) \
  { \
     V128 block[4]; \
     randBlock_Doubles(&block[0], 3); \
     block[3].u64[0] = dup4x16(0x5); block[3].u64[1] = dup4x16(0xA); \
     showBlock("FCSEL_D_D_D_NE before", &block[0], 4); \
     __asm__ __volatile__( \
        "ldr x9,  [%0, 48];  msr nzcv, x9; " \
        "ldr q29, [%0, #0];  ldr q11, [%0, #16];  ldr q9, [%0, #32]; " \
        "fcsel d29, d11, d9, ne; " \
        "mrs x9, nzcv; str x9, [%0, 48]; " \
        "str q29, [%0, #0];  str q11, [%0, #16];  str q9, [%0, #32]; " \
        ::"r"(&block[0]) : "x9","cc","memory","v9","v11","v29" \
     ); \
     showBlock("FCSEL_D_D_D_NE after", &block[0], 4); \
     printf("\n"); \
  }

//======== FCSEL_S_S_S_EQ ========//

#define GEN_test_FCSEL_S_S_S_EQ \
  __attribute__((noinline)) static void test_FCSEL_S_S_S_EQ ( void ) \
  { \
     V128 block[4]; \
     randBlock_Doubles(&block[0], 3); \
     block[3].u64[0] = dup4x16(0x5); block[3].u64[1] = dup4x16(0xA); \
     showBlock("FCSEL_S_S_S_EQ before", &block[0], 4); \
     __asm__ __volatile__( \
        "ldr x9,  [%0, 48];  msr nzcv, x9; " \
        "ldr q29, [%0, #0];  ldr q11, [%0, #16];  ldr q9, [%0, #32]; " \
        "fcsel s29, s11, s9, eq; " \
        "mrs x9, nzcv; str x9, [%0, 48]; " \
        "str q29, [%0, #0];  str q11, [%0, #16];  str q9, [%0, #32]; " \
        ::"r"(&block[0]) : "x9","cc","memory","v9","v11","v29" \
     ); \
     showBlock("FCSEL_S_S_S_EQ after", &block[0], 4); \
     printf("\n"); \
  }

//======== FCSEL_S_S_S_NE ========//

#define GEN_test_FCSEL_S_S_S_NE \
  __attribute__((noinline)) static void test_FCSEL_S_S_S_NE ( void ) \
  { \
     V128 block[4]; \
     randBlock_Doubles(&block[0], 3); \
     block[3].u64[0] = dup4x16(0x5); block[3].u64[1] = dup4x16(0xA); \
     showBlock("FCSEL_S_S_S_NE before", &block[0], 4); \
     __asm__ __volatile__( \
        "ldr x9,  [%0, 48];  msr nzcv, x9; " \
        "ldr q29, [%0, #0];  ldr q11, [%0, #16];  ldr q9, [%0, #32]; " \
        "fcsel s29, s11, s9, ne; " \
        "mrs x9, nzcv; str x9, [%0, 48]; " \
        "str q29, [%0, #0];  str q11, [%0, #16];  str q9, [%0, #32]; " \
        ::"r"(&block[0]) : "x9","cc","memory","v9","v11","v29" \
     ); \
     showBlock("FCSEL_S_S_S_NE after", &block[0], 4); \
     printf("\n"); \
  }


/* ---------------------------------------------------------------- */
/* -- Tests, in the same order that they appear in main()        -- */
/* -- The full list of instructions tested appear at the end of  -- */
/* -- this file.                                                 -- */
/* ---------------------------------------------------------------- */

// FADD <Hd>, <Hn>, <Hm>

GEN_THREEVEC_TEST(fadd_h_00_01_02, "fadd h0, h1, h2",    0, 1, 2)
GEN_THREEVEC_TEST(fadd_h_01_02_03, "fadd h1, h2, h3",    1, 2, 3)
GEN_THREEVEC_TEST(fadd_h_02_03_04, "fadd h2, h3, h4",    2, 3, 4)
GEN_THREEVEC_TEST(fadd_h_03_04_05, "fadd h3, h4, h5",    3, 4, 5)
GEN_THREEVEC_TEST(fadd_h_04_05_06, "fadd h4, h5, h6",    4, 5, 6)
GEN_THREEVEC_TEST(fadd_h_05_06_07, "fadd h5, h6, h7",    5, 6, 7)
GEN_THREEVEC_TEST(fadd_h_06_07_08, "fadd h6, h7, h8",    6, 7, 8)
GEN_THREEVEC_TEST(fadd_h_07_08_09, "fadd h7, h8, h9",    7, 8, 9)
GEN_THREEVEC_TEST(fadd_h_08_09_10, "fadd h8, h9, h10",    8, 9, 10)
GEN_THREEVEC_TEST(fadd_h_09_10_11, "fadd h9, h10, h11",    9, 10, 11)
GEN_THREEVEC_TEST(fadd_h_10_11_12, "fadd h10, h11, h12",    10, 11, 12)
GEN_THREEVEC_TEST(fadd_h_11_12_13, "fadd h11, h12, h13",    11, 12, 13)
GEN_THREEVEC_TEST(fadd_h_12_13_14, "fadd h12, h13, h14",    12, 13, 14)
GEN_THREEVEC_TEST(fadd_h_13_14_15, "fadd h13, h14, h15",    13, 14, 15)
GEN_THREEVEC_TEST(fadd_h_14_15_16, "fadd h14, h15, h16",    14, 15, 16)
GEN_THREEVEC_TEST(fadd_h_15_16_17, "fadd h15, h16, h17",    15, 16, 17)
GEN_THREEVEC_TEST(fadd_h_16_17_18, "fadd h16, h17, h18",    16, 17, 18)
GEN_THREEVEC_TEST(fadd_h_17_18_19, "fadd h17, h18, h19",    17, 18, 19)
GEN_THREEVEC_TEST(fadd_h_18_19_20, "fadd h18, h19, h20",    18, 19, 20)
GEN_THREEVEC_TEST(fadd_h_19_20_21, "fadd h19, h20, h21",    19, 20, 21)
GEN_THREEVEC_TEST(fadd_h_20_21_22, "fadd h20, h21, h22",    20, 21, 22)
GEN_THREEVEC_TEST(fadd_h_21_22_23, "fadd h21, h22, h23",    21, 22, 23)
GEN_THREEVEC_TEST(fadd_h_22_23_24, "fadd h22, h23, h24",    22, 23, 24)
GEN_THREEVEC_TEST(fadd_h_23_24_25, "fadd h23, h24, h25",    23, 24, 25)
GEN_THREEVEC_TEST(fadd_h_24_25_26, "fadd h24, h25, h26",    24, 25, 26)
GEN_THREEVEC_TEST(fadd_h_25_26_27, "fadd h25, h26, h27",    25, 26, 27)
GEN_THREEVEC_TEST(fadd_h_26_27_28, "fadd h26, h27, h28",    26, 27, 28)
GEN_THREEVEC_TEST(fadd_h_27_28_29, "fadd h27, h28, h29",    27, 28, 29)
GEN_THREEVEC_TEST(fadd_h_28_29_30, "fadd h28, h29, h30",    28, 29, 30)
GEN_THREEVEC_TEST(fadd_h_29_30_31, "fadd h29, h30, h31",    29, 30, 31)

// FADD <Vd>.<T>, <Vn>.<T>, <Vm>.<T>

GEN_THREEVEC_TEST(fadd_8h_00_01_02, "fadd v0.8h, v1.8h, v2.8h",    0, 1, 2)
GEN_THREEVEC_TEST(fadd_8h_01_02_03, "fadd v1.8h, v2.8h, v3.8h",    1, 2, 3)
GEN_THREEVEC_TEST(fadd_8h_02_03_04, "fadd v2.8h, v3.8h, v4.8h",    2, 3, 4)
GEN_THREEVEC_TEST(fadd_8h_03_04_05, "fadd v3.8h, v4.8h, v5.8h",    3, 4, 5)
GEN_THREEVEC_TEST(fadd_8h_04_05_06, "fadd v4.8h, v5.8h, v6.8h",    4, 5, 6)
GEN_THREEVEC_TEST(fadd_8h_05_06_07, "fadd v5.8h, v6.8h, v7.8h",    5, 6, 7)
GEN_THREEVEC_TEST(fadd_8h_06_07_08, "fadd v6.8h, v7.8h, v8.8h",    6, 7, 8)
GEN_THREEVEC_TEST(fadd_8h_07_08_09, "fadd v7.8h, v8.8h, v9.8h",    7, 8, 9)
GEN_THREEVEC_TEST(fadd_8h_08_09_10, "fadd v8.8h, v9.8h, v10.8h",    8, 9, 10)
GEN_THREEVEC_TEST(fadd_8h_09_10_11, "fadd v9.8h, v10.8h, v11.8h",    9, 10, 11)
GEN_THREEVEC_TEST(fadd_8h_10_11_12, "fadd v10.8h, v11.8h, v12.8h",    10, 11, 12)
GEN_THREEVEC_TEST(fadd_8h_11_12_13, "fadd v11.8h, v12.8h, v13.8h",    11, 12, 13)
GEN_THREEVEC_TEST(fadd_8h_12_13_14, "fadd v12.8h, v13.8h, v14.8h",    12, 13, 14)
GEN_THREEVEC_TEST(fadd_8h_13_14_15, "fadd v13.8h, v14.8h, v15.8h",    13, 14, 15)
GEN_THREEVEC_TEST(fadd_8h_14_15_16, "fadd v14.8h, v15.8h, v16.8h",    14, 15, 16)
GEN_THREEVEC_TEST(fadd_8h_15_16_17, "fadd v15.8h, v16.8h, v17.8h",    15, 16, 17)
GEN_THREEVEC_TEST(fadd_8h_16_17_18, "fadd v16.8h, v17.8h, v18.8h",    16, 17, 18)
GEN_THREEVEC_TEST(fadd_8h_17_18_19, "fadd v17.8h, v18.8h, v19.8h",    17, 18, 19)
GEN_THREEVEC_TEST(fadd_8h_18_19_20, "fadd v18.8h, v19.8h, v20.8h",    18, 19, 20)
GEN_THREEVEC_TEST(fadd_8h_19_20_21, "fadd v19.8h, v20.8h, v21.8h",    19, 20, 21)
GEN_THREEVEC_TEST(fadd_8h_20_21_22, "fadd v20.8h, v21.8h, v22.8h",    20, 21, 22)
GEN_THREEVEC_TEST(fadd_8h_21_22_23, "fadd v21.8h, v22.8h, v23.8h",    21, 22, 23)
GEN_THREEVEC_TEST(fadd_8h_22_23_24, "fadd v22.8h, v23.8h, v24.8h",    22, 23, 24)
GEN_THREEVEC_TEST(fadd_8h_23_24_25, "fadd v23.8h, v24.8h, v25.8h",    23, 24, 25)
GEN_THREEVEC_TEST(fadd_8h_24_25_26, "fadd v24.8h, v25.8h, v26.8h",    24, 25, 26)
GEN_THREEVEC_TEST(fadd_8h_25_26_27, "fadd v25.8h, v26.8h, v27.8h",    25, 26, 27)
GEN_THREEVEC_TEST(fadd_8h_26_27_28, "fadd v26.8h, v27.8h, v28.8h",    26, 27, 28)
GEN_THREEVEC_TEST(fadd_8h_27_28_29, "fadd v27.8h, v28.8h, v29.8h",    27, 28, 29)
GEN_THREEVEC_TEST(fadd_8h_28_29_30, "fadd v28.8h, v29.8h, v30.8h",    28, 29, 30)
GEN_THREEVEC_TEST(fadd_8h_29_30_31, "fadd v29.8h, v30.8h, v31.8h",    29, 30, 31)

GEN_THREEVEC_TEST(fadd_4h_00_01_02, "fadd v0.4h, v1.4h, v2.4h",    0, 1, 2)
GEN_THREEVEC_TEST(fadd_4h_01_02_03, "fadd v1.4h, v2.4h, v3.4h",    1, 2, 3)
GEN_THREEVEC_TEST(fadd_4h_02_03_04, "fadd v2.4h, v3.4h, v4.4h",    2, 3, 4)
GEN_THREEVEC_TEST(fadd_4h_03_04_05, "fadd v3.4h, v4.4h, v5.4h",    3, 4, 5)
GEN_THREEVEC_TEST(fadd_4h_04_05_06, "fadd v4.4h, v5.4h, v6.4h",    4, 5, 6)
GEN_THREEVEC_TEST(fadd_4h_05_06_07, "fadd v5.4h, v6.4h, v7.4h",    5, 6, 7)
GEN_THREEVEC_TEST(fadd_4h_06_07_08, "fadd v6.4h, v7.4h, v8.4h",    6, 7, 8)
GEN_THREEVEC_TEST(fadd_4h_07_08_09, "fadd v7.4h, v8.4h, v9.4h",    7, 8, 9)
GEN_THREEVEC_TEST(fadd_4h_08_09_10, "fadd v8.4h, v9.4h, v10.4h",    8, 9, 10)
GEN_THREEVEC_TEST(fadd_4h_09_10_11, "fadd v9.4h, v10.4h, v11.4h",    9, 10, 11)
GEN_THREEVEC_TEST(fadd_4h_10_11_12, "fadd v10.4h, v11.4h, v12.4h",    10, 11, 12)
GEN_THREEVEC_TEST(fadd_4h_11_12_13, "fadd v11.4h, v12.4h, v13.4h",    11, 12, 13)
GEN_THREEVEC_TEST(fadd_4h_12_13_14, "fadd v12.4h, v13.4h, v14.4h",    12, 13, 14)
GEN_THREEVEC_TEST(fadd_4h_13_14_15, "fadd v13.4h, v14.4h, v15.4h",    13, 14, 15)
GEN_THREEVEC_TEST(fadd_4h_14_15_16, "fadd v14.4h, v15.4h, v16.4h",    14, 15, 16)
GEN_THREEVEC_TEST(fadd_4h_15_16_17, "fadd v15.4h, v16.4h, v17.4h",    15, 16, 17)
GEN_THREEVEC_TEST(fadd_4h_16_17_18, "fadd v16.4h, v17.4h, v18.4h",    16, 17, 18)
GEN_THREEVEC_TEST(fadd_4h_17_18_19, "fadd v17.4h, v18.4h, v19.4h",    17, 18, 19)
GEN_THREEVEC_TEST(fadd_4h_18_19_20, "fadd v18.4h, v19.4h, v20.4h",    18, 19, 20)
GEN_THREEVEC_TEST(fadd_4h_19_20_21, "fadd v19.4h, v20.4h, v21.4h",    19, 20, 21)
GEN_THREEVEC_TEST(fadd_4h_20_21_22, "fadd v20.4h, v21.4h, v22.4h",    20, 21, 22)
GEN_THREEVEC_TEST(fadd_4h_21_22_23, "fadd v21.4h, v22.4h, v23.4h",    21, 22, 23)
GEN_THREEVEC_TEST(fadd_4h_22_23_24, "fadd v22.4h, v23.4h, v24.4h",    22, 23, 24)
GEN_THREEVEC_TEST(fadd_4h_23_24_25, "fadd v23.4h, v24.4h, v25.4h",    23, 24, 25)
GEN_THREEVEC_TEST(fadd_4h_24_25_26, "fadd v24.4h, v25.4h, v26.4h",    24, 25, 26)
GEN_THREEVEC_TEST(fadd_4h_25_26_27, "fadd v25.4h, v26.4h, v27.4h",    25, 26, 27)
GEN_THREEVEC_TEST(fadd_4h_26_27_28, "fadd v26.4h, v27.4h, v28.4h",    26, 27, 28)
GEN_THREEVEC_TEST(fadd_4h_27_28_29, "fadd v27.4h, v28.4h, v29.4h",    27, 28, 29)
GEN_THREEVEC_TEST(fadd_4h_28_29_30, "fadd v28.4h, v29.4h, v30.4h",    28, 29, 30)
GEN_THREEVEC_TEST(fadd_4h_29_30_31, "fadd v29.4h, v30.4h, v31.4h",    29, 30, 31)

// FADDP <V><d>, <Vn>.<T>

GEN_TWOVEC_TEST(faddp_h_2h_00_01,    "faddp h0, v1.2h",    0, 1)
GEN_TWOVEC_TEST(faddp_h_2h_01_02,    "faddp h1, v2.2h",    1, 2)
GEN_TWOVEC_TEST(faddp_h_2h_02_03,    "faddp h2, v3.2h",    2, 3)
GEN_TWOVEC_TEST(faddp_h_2h_03_04,    "faddp h3, v4.2h",    3, 4)
GEN_TWOVEC_TEST(faddp_h_2h_04_05,    "faddp h4, v5.2h",    4, 5)
GEN_TWOVEC_TEST(faddp_h_2h_05_06,    "faddp h5, v6.2h",    5, 6)
GEN_TWOVEC_TEST(faddp_h_2h_06_07,    "faddp h6, v7.2h",    6, 7)
GEN_TWOVEC_TEST(faddp_h_2h_07_08,    "faddp h7, v8.2h",    7, 8)
GEN_TWOVEC_TEST(faddp_h_2h_08_09,    "faddp h8, v9.2h",    8, 9)
GEN_TWOVEC_TEST(faddp_h_2h_09_10,    "faddp h9, v10.2h",    9, 10)
GEN_TWOVEC_TEST(faddp_h_2h_10_11,    "faddp h10, v11.2h",    10, 11)
GEN_TWOVEC_TEST(faddp_h_2h_11_12,    "faddp h11, v12.2h",    11, 12)
GEN_TWOVEC_TEST(faddp_h_2h_12_13,    "faddp h12, v13.2h",    12, 13)
GEN_TWOVEC_TEST(faddp_h_2h_13_14,    "faddp h13, v14.2h",    13, 14)
GEN_TWOVEC_TEST(faddp_h_2h_14_15,    "faddp h14, v15.2h",    14, 15)
GEN_TWOVEC_TEST(faddp_h_2h_15_16,    "faddp h15, v16.2h",    15, 16)
GEN_TWOVEC_TEST(faddp_h_2h_16_17,    "faddp h16, v17.2h",    16, 17)
GEN_TWOVEC_TEST(faddp_h_2h_17_18,    "faddp h17, v18.2h",    17, 18)
GEN_TWOVEC_TEST(faddp_h_2h_18_19,    "faddp h18, v19.2h",    18, 19)
GEN_TWOVEC_TEST(faddp_h_2h_19_20,    "faddp h19, v20.2h",    19, 20)
GEN_TWOVEC_TEST(faddp_h_2h_20_21,    "faddp h20, v21.2h",    20, 21)
GEN_TWOVEC_TEST(faddp_h_2h_21_22,    "faddp h21, v22.2h",    21, 22)
GEN_TWOVEC_TEST(faddp_h_2h_22_23,    "faddp h22, v23.2h",    22, 23)
GEN_TWOVEC_TEST(faddp_h_2h_23_24,    "faddp h23, v24.2h",    23, 24)
GEN_TWOVEC_TEST(faddp_h_2h_24_25,    "faddp h24, v25.2h",    24, 25)
GEN_TWOVEC_TEST(faddp_h_2h_25_26,    "faddp h25, v26.2h",    25, 26)
GEN_TWOVEC_TEST(faddp_h_2h_26_27,    "faddp h26, v27.2h",    26, 27)
GEN_TWOVEC_TEST(faddp_h_2h_27_28,    "faddp h27, v28.2h",    27, 28)
GEN_TWOVEC_TEST(faddp_h_2h_28_29,    "faddp h28, v29.2h",    28, 29)
GEN_TWOVEC_TEST(faddp_h_2h_29_30,    "faddp h29, v30.2h",    29, 30)
GEN_TWOVEC_TEST(faddp_h_2h_30_31,    "faddp h30, v31.2h",    30, 31)

// FADDP <Vd>.<T>, <Vn>.<T>, <Vm>.<T>

GEN_THREEVEC_TEST(faddp_8h_00_01_02, "faddp v0.8h, v1.8h, v2.8h",    0, 1, 2)
GEN_THREEVEC_TEST(faddp_8h_01_02_03, "faddp v1.8h, v2.8h, v3.8h",    1, 2, 3)
GEN_THREEVEC_TEST(faddp_8h_02_03_04, "faddp v2.8h, v3.8h, v4.8h",    2, 3, 4)
GEN_THREEVEC_TEST(faddp_8h_03_04_05, "faddp v3.8h, v4.8h, v5.8h",    3, 4, 5)
GEN_THREEVEC_TEST(faddp_8h_04_05_06, "faddp v4.8h, v5.8h, v6.8h",    4, 5, 6)
GEN_THREEVEC_TEST(faddp_8h_05_06_07, "faddp v5.8h, v6.8h, v7.8h",    5, 6, 7)
GEN_THREEVEC_TEST(faddp_8h_06_07_08, "faddp v6.8h, v7.8h, v8.8h",    6, 7, 8)
GEN_THREEVEC_TEST(faddp_8h_07_08_09, "faddp v7.8h, v8.8h, v9.8h",    7, 8, 9)
GEN_THREEVEC_TEST(faddp_8h_08_09_10, "faddp v8.8h, v9.8h, v10.8h",    8, 9, 10)
GEN_THREEVEC_TEST(faddp_8h_09_10_11, "faddp v9.8h, v10.8h, v11.8h",    9, 10, 11)
GEN_THREEVEC_TEST(faddp_8h_10_11_12, "faddp v10.8h, v11.8h, v12.8h",    10, 11, 12)
GEN_THREEVEC_TEST(faddp_8h_11_12_13, "faddp v11.8h, v12.8h, v13.8h",    11, 12, 13)
GEN_THREEVEC_TEST(faddp_8h_12_13_14, "faddp v12.8h, v13.8h, v14.8h",    12, 13, 14)
GEN_THREEVEC_TEST(faddp_8h_13_14_15, "faddp v13.8h, v14.8h, v15.8h",    13, 14, 15)
GEN_THREEVEC_TEST(faddp_8h_14_15_16, "faddp v14.8h, v15.8h, v16.8h",    14, 15, 16)
GEN_THREEVEC_TEST(faddp_8h_15_16_17, "faddp v15.8h, v16.8h, v17.8h",    15, 16, 17)
GEN_THREEVEC_TEST(faddp_8h_16_17_18, "faddp v16.8h, v17.8h, v18.8h",    16, 17, 18)
GEN_THREEVEC_TEST(faddp_8h_17_18_19, "faddp v17.8h, v18.8h, v19.8h",    17, 18, 19)
GEN_THREEVEC_TEST(faddp_8h_18_19_20, "faddp v18.8h, v19.8h, v20.8h",    18, 19, 20)
GEN_THREEVEC_TEST(faddp_8h_19_20_21, "faddp v19.8h, v20.8h, v21.8h",    19, 20, 21)
GEN_THREEVEC_TEST(faddp_8h_20_21_22, "faddp v20.8h, v21.8h, v22.8h",    20, 21, 22)
GEN_THREEVEC_TEST(faddp_8h_21_22_23, "faddp v21.8h, v22.8h, v23.8h",    21, 22, 23)
GEN_THREEVEC_TEST(faddp_8h_22_23_24, "faddp v22.8h, v23.8h, v24.8h",    22, 23, 24)
GEN_THREEVEC_TEST(faddp_8h_23_24_25, "faddp v23.8h, v24.8h, v25.8h",    23, 24, 25)
GEN_THREEVEC_TEST(faddp_8h_24_25_26, "faddp v24.8h, v25.8h, v26.8h",    24, 25, 26)
GEN_THREEVEC_TEST(faddp_8h_25_26_27, "faddp v25.8h, v26.8h, v27.8h",    25, 26, 27)
GEN_THREEVEC_TEST(faddp_8h_26_27_28, "faddp v26.8h, v27.8h, v28.8h",    26, 27, 28)
GEN_THREEVEC_TEST(faddp_8h_27_28_29, "faddp v27.8h, v28.8h, v29.8h",    27, 28, 29)
GEN_THREEVEC_TEST(faddp_8h_28_29_30, "faddp v28.8h, v29.8h, v30.8h",    28, 29, 30)
GEN_THREEVEC_TEST(faddp_8h_29_30_31, "faddp v29.8h, v30.8h, v31.8h",    29, 30, 31)

GEN_THREEVEC_TEST(faddp_4h_00_01_02, "faddp v0.4h, v1.4h, v2.4h",    0, 1, 2)
GEN_THREEVEC_TEST(faddp_4h_01_02_03, "faddp v1.4h, v2.4h, v3.4h",    1, 2, 3)
GEN_THREEVEC_TEST(faddp_4h_02_03_04, "faddp v2.4h, v3.4h, v4.4h",    2, 3, 4)
GEN_THREEVEC_TEST(faddp_4h_03_04_05, "faddp v3.4h, v4.4h, v5.4h",    3, 4, 5)
GEN_THREEVEC_TEST(faddp_4h_04_05_06, "faddp v4.4h, v5.4h, v6.4h",    4, 5, 6)
GEN_THREEVEC_TEST(faddp_4h_05_06_07, "faddp v5.4h, v6.4h, v7.4h",    5, 6, 7)
GEN_THREEVEC_TEST(faddp_4h_06_07_08, "faddp v6.4h, v7.4h, v8.4h",    6, 7, 8)
GEN_THREEVEC_TEST(faddp_4h_07_08_09, "faddp v7.4h, v8.4h, v9.4h",    7, 8, 9)
GEN_THREEVEC_TEST(faddp_4h_08_09_10, "faddp v8.4h, v9.4h, v10.4h",    8, 9, 10)
GEN_THREEVEC_TEST(faddp_4h_09_10_11, "faddp v9.4h, v10.4h, v11.4h",    9, 10, 11)
GEN_THREEVEC_TEST(faddp_4h_10_11_12, "faddp v10.4h, v11.4h, v12.4h",    10, 11, 12)
GEN_THREEVEC_TEST(faddp_4h_11_12_13, "faddp v11.4h, v12.4h, v13.4h",    11, 12, 13)
GEN_THREEVEC_TEST(faddp_4h_12_13_14, "faddp v12.4h, v13.4h, v14.4h",    12, 13, 14)
GEN_THREEVEC_TEST(faddp_4h_13_14_15, "faddp v13.4h, v14.4h, v15.4h",    13, 14, 15)
GEN_THREEVEC_TEST(faddp_4h_14_15_16, "faddp v14.4h, v15.4h, v16.4h",    14, 15, 16)
GEN_THREEVEC_TEST(faddp_4h_15_16_17, "faddp v15.4h, v16.4h, v17.4h",    15, 16, 17)
GEN_THREEVEC_TEST(faddp_4h_16_17_18, "faddp v16.4h, v17.4h, v18.4h",    16, 17, 18)
GEN_THREEVEC_TEST(faddp_4h_17_18_19, "faddp v17.4h, v18.4h, v19.4h",    17, 18, 19)
GEN_THREEVEC_TEST(faddp_4h_18_19_20, "faddp v18.4h, v19.4h, v20.4h",    18, 19, 20)
GEN_THREEVEC_TEST(faddp_4h_19_20_21, "faddp v19.4h, v20.4h, v21.4h",    19, 20, 21)
GEN_THREEVEC_TEST(faddp_4h_20_21_22, "faddp v20.4h, v21.4h, v22.4h",    20, 21, 22)
GEN_THREEVEC_TEST(faddp_4h_21_22_23, "faddp v21.4h, v22.4h, v23.4h",    21, 22, 23)
GEN_THREEVEC_TEST(faddp_4h_22_23_24, "faddp v22.4h, v23.4h, v24.4h",    22, 23, 24)
GEN_THREEVEC_TEST(faddp_4h_23_24_25, "faddp v23.4h, v24.4h, v25.4h",    23, 24, 25)
GEN_THREEVEC_TEST(faddp_4h_24_25_26, "faddp v24.4h, v25.4h, v26.4h",    24, 25, 26)
GEN_THREEVEC_TEST(faddp_4h_25_26_27, "faddp v25.4h, v26.4h, v27.4h",    25, 26, 27)
GEN_THREEVEC_TEST(faddp_4h_26_27_28, "faddp v26.4h, v27.4h, v28.4h",    26, 27, 28)
GEN_THREEVEC_TEST(faddp_4h_27_28_29, "faddp v27.4h, v28.4h, v29.4h",    27, 28, 29)
GEN_THREEVEC_TEST(faddp_4h_28_29_30, "faddp v28.4h, v29.4h, v30.4h",    28, 29, 30)
GEN_THREEVEC_TEST(faddp_4h_29_30_31, "faddp v29.4h, v30.4h, v31.4h",    29, 30, 31)

// FABS <Hd>, <Hn>

GEN_TWOVEC_TEST(fabs_h_00_01, "fabs h0, h1",    0, 1)
GEN_TWOVEC_TEST(fabs_h_01_02, "fabs h1, h2",    1, 2)
GEN_TWOVEC_TEST(fabs_h_02_03, "fabs h2, h3",    2, 3)
GEN_TWOVEC_TEST(fabs_h_03_04, "fabs h3, h4",    3, 4)
GEN_TWOVEC_TEST(fabs_h_04_05, "fabs h4, h5",    4, 5)
GEN_TWOVEC_TEST(fabs_h_05_06, "fabs h5, h6",    5, 6)
GEN_TWOVEC_TEST(fabs_h_06_07, "fabs h6, h7",    6, 7)
GEN_TWOVEC_TEST(fabs_h_07_08, "fabs h7, h8",    7, 8)
GEN_TWOVEC_TEST(fabs_h_08_09, "fabs h8, h9",    8, 9)
GEN_TWOVEC_TEST(fabs_h_09_10, "fabs h9, h10",    9, 10)
GEN_TWOVEC_TEST(fabs_h_10_11, "fabs h10, h11",    10, 11)
GEN_TWOVEC_TEST(fabs_h_11_12, "fabs h11, h12",    11, 12)
GEN_TWOVEC_TEST(fabs_h_12_13, "fabs h12, h13",    12, 13)
GEN_TWOVEC_TEST(fabs_h_13_14, "fabs h13, h14",    13, 14)
GEN_TWOVEC_TEST(fabs_h_14_15, "fabs h14, h15",    14, 15)
GEN_TWOVEC_TEST(fabs_h_15_16, "fabs h15, h16",    15, 16)
GEN_TWOVEC_TEST(fabs_h_16_17, "fabs h16, h17",    16, 17)
GEN_TWOVEC_TEST(fabs_h_17_18, "fabs h17, h18",    17, 18)
GEN_TWOVEC_TEST(fabs_h_18_19, "fabs h18, h19",    18, 19)
GEN_TWOVEC_TEST(fabs_h_19_20, "fabs h19, h20",    19, 20)
GEN_TWOVEC_TEST(fabs_h_20_21, "fabs h20, h21",    20, 21)
GEN_TWOVEC_TEST(fabs_h_21_22, "fabs h21, h22",    21, 22)
GEN_TWOVEC_TEST(fabs_h_22_23, "fabs h22, h23",    22, 23)
GEN_TWOVEC_TEST(fabs_h_23_24, "fabs h23, h24",    23, 24)
GEN_TWOVEC_TEST(fabs_h_24_25, "fabs h24, h25",    24, 25)
GEN_TWOVEC_TEST(fabs_h_25_26, "fabs h25, h26",    25, 26)
GEN_TWOVEC_TEST(fabs_h_26_27, "fabs h26, h27",    26, 27)
GEN_TWOVEC_TEST(fabs_h_27_28, "fabs h27, h28",    27, 28)
GEN_TWOVEC_TEST(fabs_h_28_29, "fabs h28, h29",    28, 29)
GEN_TWOVEC_TEST(fabs_h_29_30, "fabs h29, h30",    29, 30)
GEN_TWOVEC_TEST(fabs_h_30_31, "fabs h30, h31",    30, 31)

// FABS <Vd>.<T>, <Vn>.<T>

GEN_TWOVEC_TEST(fabs_8h_00_01, "fabs v0.8h, v1.8h",    0, 1)
GEN_TWOVEC_TEST(fabs_8h_01_02, "fabs v1.8h, v2.8h",    1, 2)
GEN_TWOVEC_TEST(fabs_8h_02_03, "fabs v2.8h, v3.8h",    2, 3)
GEN_TWOVEC_TEST(fabs_8h_03_04, "fabs v3.8h, v4.8h",    3, 4)
GEN_TWOVEC_TEST(fabs_8h_04_05, "fabs v4.8h, v5.8h",    4, 5)
GEN_TWOVEC_TEST(fabs_8h_05_06, "fabs v5.8h, v6.8h",    5, 6)
GEN_TWOVEC_TEST(fabs_8h_06_07, "fabs v6.8h, v7.8h",    6, 7)
GEN_TWOVEC_TEST(fabs_8h_07_08, "fabs v7.8h, v8.8h",    7, 8)
GEN_TWOVEC_TEST(fabs_8h_08_09, "fabs v8.8h, v9.8h",    8, 9)
GEN_TWOVEC_TEST(fabs_8h_09_10, "fabs v9.8h, v10.8h",    9, 10)
GEN_TWOVEC_TEST(fabs_8h_10_11, "fabs v10.8h, v11.8h",    10, 11)
GEN_TWOVEC_TEST(fabs_8h_11_12, "fabs v11.8h, v12.8h",    11, 12)
GEN_TWOVEC_TEST(fabs_8h_12_13, "fabs v12.8h, v13.8h",    12, 13)
GEN_TWOVEC_TEST(fabs_8h_13_14, "fabs v13.8h, v14.8h",    13, 14)
GEN_TWOVEC_TEST(fabs_8h_14_15, "fabs v14.8h, v15.8h",    14, 15)
GEN_TWOVEC_TEST(fabs_8h_15_16, "fabs v15.8h, v16.8h",    15, 16)
GEN_TWOVEC_TEST(fabs_8h_16_17, "fabs v16.8h, v17.8h",    16, 17)
GEN_TWOVEC_TEST(fabs_8h_17_18, "fabs v17.8h, v18.8h",    17, 18)
GEN_TWOVEC_TEST(fabs_8h_18_19, "fabs v18.8h, v19.8h",    18, 19)
GEN_TWOVEC_TEST(fabs_8h_19_20, "fabs v19.8h, v20.8h",    19, 20)
GEN_TWOVEC_TEST(fabs_8h_20_21, "fabs v20.8h, v21.8h",    20, 21)
GEN_TWOVEC_TEST(fabs_8h_21_22, "fabs v21.8h, v22.8h",    21, 22)
GEN_TWOVEC_TEST(fabs_8h_22_23, "fabs v22.8h, v23.8h",    22, 23)
GEN_TWOVEC_TEST(fabs_8h_23_24, "fabs v23.8h, v24.8h",    23, 24)
GEN_TWOVEC_TEST(fabs_8h_24_25, "fabs v24.8h, v25.8h",    24, 25)
GEN_TWOVEC_TEST(fabs_8h_25_26, "fabs v25.8h, v26.8h",    25, 26)
GEN_TWOVEC_TEST(fabs_8h_26_27, "fabs v26.8h, v27.8h",    26, 27)
GEN_TWOVEC_TEST(fabs_8h_27_28, "fabs v27.8h, v28.8h",    27, 28)
GEN_TWOVEC_TEST(fabs_8h_28_29, "fabs v28.8h, v29.8h",    28, 29)
GEN_TWOVEC_TEST(fabs_8h_29_30, "fabs v29.8h, v30.8h",    29, 30)
GEN_TWOVEC_TEST(fabs_8h_30_31, "fabs v30.8h, v31.8h",    30, 31)

GEN_TWOVEC_TEST(fabs_4h_00_01, "fabs v0.4h, v1.4h",    0, 1)
GEN_TWOVEC_TEST(fabs_4h_01_02, "fabs v1.4h, v2.4h",    1, 2)
GEN_TWOVEC_TEST(fabs_4h_02_03, "fabs v2.4h, v3.4h",    2, 3)
GEN_TWOVEC_TEST(fabs_4h_03_04, "fabs v3.4h, v4.4h",    3, 4)
GEN_TWOVEC_TEST(fabs_4h_04_05, "fabs v4.4h, v5.4h",    4, 5)
GEN_TWOVEC_TEST(fabs_4h_05_06, "fabs v5.4h, v6.4h",    5, 6)
GEN_TWOVEC_TEST(fabs_4h_06_07, "fabs v6.4h, v7.4h",    6, 7)
GEN_TWOVEC_TEST(fabs_4h_07_08, "fabs v7.4h, v8.4h",    7, 8)
GEN_TWOVEC_TEST(fabs_4h_08_09, "fabs v8.4h, v9.4h",    8, 9)
GEN_TWOVEC_TEST(fabs_4h_09_10, "fabs v9.4h, v10.4h",    9, 10)
GEN_TWOVEC_TEST(fabs_4h_10_11, "fabs v10.4h, v11.4h",    10, 11)
GEN_TWOVEC_TEST(fabs_4h_11_12, "fabs v11.4h, v12.4h",    11, 12)
GEN_TWOVEC_TEST(fabs_4h_12_13, "fabs v12.4h, v13.4h",    12, 13)
GEN_TWOVEC_TEST(fabs_4h_13_14, "fabs v13.4h, v14.4h",    13, 14)
GEN_TWOVEC_TEST(fabs_4h_14_15, "fabs v14.4h, v15.4h",    14, 15)
GEN_TWOVEC_TEST(fabs_4h_15_16, "fabs v15.4h, v16.4h",    15, 16)
GEN_TWOVEC_TEST(fabs_4h_16_17, "fabs v16.4h, v17.4h",    16, 17)
GEN_TWOVEC_TEST(fabs_4h_17_18, "fabs v17.4h, v18.4h",    17, 18)
GEN_TWOVEC_TEST(fabs_4h_18_19, "fabs v18.4h, v19.4h",    18, 19)
GEN_TWOVEC_TEST(fabs_4h_19_20, "fabs v19.4h, v20.4h",    19, 20)
GEN_TWOVEC_TEST(fabs_4h_20_21, "fabs v20.4h, v21.4h",    20, 21)
GEN_TWOVEC_TEST(fabs_4h_21_22, "fabs v21.4h, v22.4h",    21, 22)
GEN_TWOVEC_TEST(fabs_4h_22_23, "fabs v22.4h, v23.4h",    22, 23)
GEN_TWOVEC_TEST(fabs_4h_23_24, "fabs v23.4h, v24.4h",    23, 24)
GEN_TWOVEC_TEST(fabs_4h_24_25, "fabs v24.4h, v25.4h",    24, 25)
GEN_TWOVEC_TEST(fabs_4h_25_26, "fabs v25.4h, v26.4h",    25, 26)
GEN_TWOVEC_TEST(fabs_4h_26_27, "fabs v26.4h, v27.4h",    26, 27)
GEN_TWOVEC_TEST(fabs_4h_27_28, "fabs v27.4h, v28.4h",    27, 28)
GEN_TWOVEC_TEST(fabs_4h_28_29, "fabs v28.4h, v29.4h",    28, 29)
GEN_TWOVEC_TEST(fabs_4h_29_30, "fabs v29.4h, v30.4h",    29, 30)
GEN_TWOVEC_TEST(fabs_4h_30_31, "fabs v30.4h, v31.4h",    30, 31)

// FNEG <Hd>, <Hn>

GEN_TWOVEC_TEST(fneg_h_00_01, "fneg h0, h1",    0, 1)
GEN_TWOVEC_TEST(fneg_h_01_02, "fneg h1, h2",    1, 2)
GEN_TWOVEC_TEST(fneg_h_02_03, "fneg h2, h3",    2, 3)
GEN_TWOVEC_TEST(fneg_h_03_04, "fneg h3, h4",    3, 4)
GEN_TWOVEC_TEST(fneg_h_04_05, "fneg h4, h5",    4, 5)
GEN_TWOVEC_TEST(fneg_h_05_06, "fneg h5, h6",    5, 6)
GEN_TWOVEC_TEST(fneg_h_06_07, "fneg h6, h7",    6, 7)
GEN_TWOVEC_TEST(fneg_h_07_08, "fneg h7, h8",    7, 8)
GEN_TWOVEC_TEST(fneg_h_08_09, "fneg h8, h9",    8, 9)
GEN_TWOVEC_TEST(fneg_h_09_10, "fneg h9, h10",    9, 10)
GEN_TWOVEC_TEST(fneg_h_10_11, "fneg h10, h11",    10, 11)
GEN_TWOVEC_TEST(fneg_h_11_12, "fneg h11, h12",    11, 12)
GEN_TWOVEC_TEST(fneg_h_12_13, "fneg h12, h13",    12, 13)
GEN_TWOVEC_TEST(fneg_h_13_14, "fneg h13, h14",    13, 14)
GEN_TWOVEC_TEST(fneg_h_14_15, "fneg h14, h15",    14, 15)
GEN_TWOVEC_TEST(fneg_h_15_16, "fneg h15, h16",    15, 16)
GEN_TWOVEC_TEST(fneg_h_16_17, "fneg h16, h17",    16, 17)
GEN_TWOVEC_TEST(fneg_h_17_18, "fneg h17, h18",    17, 18)
GEN_TWOVEC_TEST(fneg_h_18_19, "fneg h18, h19",    18, 19)
GEN_TWOVEC_TEST(fneg_h_19_20, "fneg h19, h20",    19, 20)
GEN_TWOVEC_TEST(fneg_h_20_21, "fneg h20, h21",    20, 21)
GEN_TWOVEC_TEST(fneg_h_21_22, "fneg h21, h22",    21, 22)
GEN_TWOVEC_TEST(fneg_h_22_23, "fneg h22, h23",    22, 23)
GEN_TWOVEC_TEST(fneg_h_23_24, "fneg h23, h24",    23, 24)
GEN_TWOVEC_TEST(fneg_h_24_25, "fneg h24, h25",    24, 25)
GEN_TWOVEC_TEST(fneg_h_25_26, "fneg h25, h26",    25, 26)
GEN_TWOVEC_TEST(fneg_h_26_27, "fneg h26, h27",    26, 27)
GEN_TWOVEC_TEST(fneg_h_27_28, "fneg h27, h28",    27, 28)
GEN_TWOVEC_TEST(fneg_h_28_29, "fneg h28, h29",    28, 29)
GEN_TWOVEC_TEST(fneg_h_29_30, "fneg h29, h30",    29, 30)
GEN_TWOVEC_TEST(fneg_h_30_31, "fneg h30, h31",    30, 31)

// FNEG <Vd>.<T>, <Vn>.<T>

GEN_TWOVEC_TEST(fneg_8h_00_01, "fneg v0.8h, v1.8h",    0, 1)
GEN_TWOVEC_TEST(fneg_8h_01_02, "fneg v1.8h, v2.8h",    1, 2)
GEN_TWOVEC_TEST(fneg_8h_02_03, "fneg v2.8h, v3.8h",    2, 3)
GEN_TWOVEC_TEST(fneg_8h_03_04, "fneg v3.8h, v4.8h",    3, 4)
GEN_TWOVEC_TEST(fneg_8h_04_05, "fneg v4.8h, v5.8h",    4, 5)
GEN_TWOVEC_TEST(fneg_8h_05_06, "fneg v5.8h, v6.8h",    5, 6)
GEN_TWOVEC_TEST(fneg_8h_06_07, "fneg v6.8h, v7.8h",    6, 7)
GEN_TWOVEC_TEST(fneg_8h_07_08, "fneg v7.8h, v8.8h",    7, 8)
GEN_TWOVEC_TEST(fneg_8h_08_09, "fneg v8.8h, v9.8h",    8, 9)
GEN_TWOVEC_TEST(fneg_8h_09_10, "fneg v9.8h, v10.8h",    9, 10)
GEN_TWOVEC_TEST(fneg_8h_10_11, "fneg v10.8h, v11.8h",    10, 11)
GEN_TWOVEC_TEST(fneg_8h_11_12, "fneg v11.8h, v12.8h",    11, 12)
GEN_TWOVEC_TEST(fneg_8h_12_13, "fneg v12.8h, v13.8h",    12, 13)
GEN_TWOVEC_TEST(fneg_8h_13_14, "fneg v13.8h, v14.8h",    13, 14)
GEN_TWOVEC_TEST(fneg_8h_14_15, "fneg v14.8h, v15.8h",    14, 15)
GEN_TWOVEC_TEST(fneg_8h_15_16, "fneg v15.8h, v16.8h",    15, 16)
GEN_TWOVEC_TEST(fneg_8h_16_17, "fneg v16.8h, v17.8h",    16, 17)
GEN_TWOVEC_TEST(fneg_8h_17_18, "fneg v17.8h, v18.8h",    17, 18)
GEN_TWOVEC_TEST(fneg_8h_18_19, "fneg v18.8h, v19.8h",    18, 19)
GEN_TWOVEC_TEST(fneg_8h_19_20, "fneg v19.8h, v20.8h",    19, 20)
GEN_TWOVEC_TEST(fneg_8h_20_21, "fneg v20.8h, v21.8h",    20, 21)
GEN_TWOVEC_TEST(fneg_8h_21_22, "fneg v21.8h, v22.8h",    21, 22)
GEN_TWOVEC_TEST(fneg_8h_22_23, "fneg v22.8h, v23.8h",    22, 23)
GEN_TWOVEC_TEST(fneg_8h_23_24, "fneg v23.8h, v24.8h",    23, 24)
GEN_TWOVEC_TEST(fneg_8h_24_25, "fneg v24.8h, v25.8h",    24, 25)
GEN_TWOVEC_TEST(fneg_8h_25_26, "fneg v25.8h, v26.8h",    25, 26)
GEN_TWOVEC_TEST(fneg_8h_26_27, "fneg v26.8h, v27.8h",    26, 27)
GEN_TWOVEC_TEST(fneg_8h_27_28, "fneg v27.8h, v28.8h",    27, 28)
GEN_TWOVEC_TEST(fneg_8h_28_29, "fneg v28.8h, v29.8h",    28, 29)
GEN_TWOVEC_TEST(fneg_8h_29_30, "fneg v29.8h, v30.8h",    29, 30)
GEN_TWOVEC_TEST(fneg_8h_30_31, "fneg v30.8h, v31.8h",    30, 31)

GEN_TWOVEC_TEST(fneg_4h_00_01, "fneg v0.4h, v1.4h",    0, 1)
GEN_TWOVEC_TEST(fneg_4h_01_02, "fneg v1.4h, v2.4h",    1, 2)
GEN_TWOVEC_TEST(fneg_4h_02_03, "fneg v2.4h, v3.4h",    2, 3)
GEN_TWOVEC_TEST(fneg_4h_03_04, "fneg v3.4h, v4.4h",    3, 4)
GEN_TWOVEC_TEST(fneg_4h_04_05, "fneg v4.4h, v5.4h",    4, 5)
GEN_TWOVEC_TEST(fneg_4h_05_06, "fneg v5.4h, v6.4h",    5, 6)
GEN_TWOVEC_TEST(fneg_4h_06_07, "fneg v6.4h, v7.4h",    6, 7)
GEN_TWOVEC_TEST(fneg_4h_07_08, "fneg v7.4h, v8.4h",    7, 8)
GEN_TWOVEC_TEST(fneg_4h_08_09, "fneg v8.4h, v9.4h",    8, 9)
GEN_TWOVEC_TEST(fneg_4h_09_10, "fneg v9.4h, v10.4h",    9, 10)
GEN_TWOVEC_TEST(fneg_4h_10_11, "fneg v10.4h, v11.4h",    10, 11)
GEN_TWOVEC_TEST(fneg_4h_11_12, "fneg v11.4h, v12.4h",    11, 12)
GEN_TWOVEC_TEST(fneg_4h_12_13, "fneg v12.4h, v13.4h",    12, 13)
GEN_TWOVEC_TEST(fneg_4h_13_14, "fneg v13.4h, v14.4h",    13, 14)
GEN_TWOVEC_TEST(fneg_4h_14_15, "fneg v14.4h, v15.4h",    14, 15)
GEN_TWOVEC_TEST(fneg_4h_15_16, "fneg v15.4h, v16.4h",    15, 16)
GEN_TWOVEC_TEST(fneg_4h_16_17, "fneg v16.4h, v17.4h",    16, 17)
GEN_TWOVEC_TEST(fneg_4h_17_18, "fneg v17.4h, v18.4h",    17, 18)
GEN_TWOVEC_TEST(fneg_4h_18_19, "fneg v18.4h, v19.4h",    18, 19)
GEN_TWOVEC_TEST(fneg_4h_19_20, "fneg v19.4h, v20.4h",    19, 20)
GEN_TWOVEC_TEST(fneg_4h_20_21, "fneg v20.4h, v21.4h",    20, 21)
GEN_TWOVEC_TEST(fneg_4h_21_22, "fneg v21.4h, v22.4h",    21, 22)
GEN_TWOVEC_TEST(fneg_4h_22_23, "fneg v22.4h, v23.4h",    22, 23)
GEN_TWOVEC_TEST(fneg_4h_23_24, "fneg v23.4h, v24.4h",    23, 24)
GEN_TWOVEC_TEST(fneg_4h_24_25, "fneg v24.4h, v25.4h",    24, 25)
GEN_TWOVEC_TEST(fneg_4h_25_26, "fneg v25.4h, v26.4h",    25, 26)
GEN_TWOVEC_TEST(fneg_4h_26_27, "fneg v26.4h, v27.4h",    26, 27)
GEN_TWOVEC_TEST(fneg_4h_27_28, "fneg v27.4h, v28.4h",    27, 28)
GEN_TWOVEC_TEST(fneg_4h_28_29, "fneg v28.4h, v29.4h",    28, 29)
GEN_TWOVEC_TEST(fneg_4h_29_30, "fneg v29.4h, v30.4h",    29, 30)
GEN_TWOVEC_TEST(fneg_4h_30_31, "fneg v30.4h, v31.4h",    30, 31)

// FSQRT <Hd>, <Hn>

GEN_TWOVEC_TEST(fsqrt_h_00_01, "fsqrt h0, h1",    0, 1)
GEN_TWOVEC_TEST(fsqrt_h_01_02, "fsqrt h1, h2",    1, 2)
GEN_TWOVEC_TEST(fsqrt_h_02_03, "fsqrt h2, h3",    2, 3)
GEN_TWOVEC_TEST(fsqrt_h_03_04, "fsqrt h3, h4",    3, 4)
GEN_TWOVEC_TEST(fsqrt_h_04_05, "fsqrt h4, h5",    4, 5)
GEN_TWOVEC_TEST(fsqrt_h_05_06, "fsqrt h5, h6",    5, 6)
GEN_TWOVEC_TEST(fsqrt_h_06_07, "fsqrt h6, h7",    6, 7)
GEN_TWOVEC_TEST(fsqrt_h_07_08, "fsqrt h7, h8",    7, 8)
GEN_TWOVEC_TEST(fsqrt_h_08_09, "fsqrt h8, h9",    8, 9)
GEN_TWOVEC_TEST(fsqrt_h_09_10, "fsqrt h9, h10",    9, 10)
GEN_TWOVEC_TEST(fsqrt_h_10_11, "fsqrt h10, h11",    10, 11)
GEN_TWOVEC_TEST(fsqrt_h_11_12, "fsqrt h11, h12",    11, 12)
GEN_TWOVEC_TEST(fsqrt_h_12_13, "fsqrt h12, h13",    12, 13)
GEN_TWOVEC_TEST(fsqrt_h_13_14, "fsqrt h13, h14",    13, 14)
GEN_TWOVEC_TEST(fsqrt_h_14_15, "fsqrt h14, h15",    14, 15)
GEN_TWOVEC_TEST(fsqrt_h_15_16, "fsqrt h15, h16",    15, 16)
GEN_TWOVEC_TEST(fsqrt_h_16_17, "fsqrt h16, h17",    16, 17)
GEN_TWOVEC_TEST(fsqrt_h_17_18, "fsqrt h17, h18",    17, 18)
GEN_TWOVEC_TEST(fsqrt_h_18_19, "fsqrt h18, h19",    18, 19)
GEN_TWOVEC_TEST(fsqrt_h_19_20, "fsqrt h19, h20",    19, 20)
GEN_TWOVEC_TEST(fsqrt_h_20_21, "fsqrt h20, h21",    20, 21)
GEN_TWOVEC_TEST(fsqrt_h_21_22, "fsqrt h21, h22",    21, 22)
GEN_TWOVEC_TEST(fsqrt_h_22_23, "fsqrt h22, h23",    22, 23)
GEN_TWOVEC_TEST(fsqrt_h_23_24, "fsqrt h23, h24",    23, 24)
GEN_TWOVEC_TEST(fsqrt_h_24_25, "fsqrt h24, h25",    24, 25)
GEN_TWOVEC_TEST(fsqrt_h_25_26, "fsqrt h25, h26",    25, 26)
GEN_TWOVEC_TEST(fsqrt_h_26_27, "fsqrt h26, h27",    26, 27)
GEN_TWOVEC_TEST(fsqrt_h_27_28, "fsqrt h27, h28",    27, 28)
GEN_TWOVEC_TEST(fsqrt_h_28_29, "fsqrt h28, h29",    28, 29)
GEN_TWOVEC_TEST(fsqrt_h_29_30, "fsqrt h29, h30",    29, 30)
GEN_TWOVEC_TEST(fsqrt_h_30_31, "fsqrt h30, h31",    30, 31)

// FSQRT <Vd>.<T>, <Vn>.<T>

GEN_TWOVEC_TEST(fsqrt_8h_00_01, "fsqrt v0.8h, v1.8h",    0, 1)
GEN_TWOVEC_TEST(fsqrt_8h_01_02, "fsqrt v1.8h, v2.8h",    1, 2)
GEN_TWOVEC_TEST(fsqrt_8h_02_03, "fsqrt v2.8h, v3.8h",    2, 3)
GEN_TWOVEC_TEST(fsqrt_8h_03_04, "fsqrt v3.8h, v4.8h",    3, 4)
GEN_TWOVEC_TEST(fsqrt_8h_04_05, "fsqrt v4.8h, v5.8h",    4, 5)
GEN_TWOVEC_TEST(fsqrt_8h_05_06, "fsqrt v5.8h, v6.8h",    5, 6)
GEN_TWOVEC_TEST(fsqrt_8h_06_07, "fsqrt v6.8h, v7.8h",    6, 7)
GEN_TWOVEC_TEST(fsqrt_8h_07_08, "fsqrt v7.8h, v8.8h",    7, 8)
GEN_TWOVEC_TEST(fsqrt_8h_08_09, "fsqrt v8.8h, v9.8h",    8, 9)
GEN_TWOVEC_TEST(fsqrt_8h_09_10, "fsqrt v9.8h, v10.8h",    9, 10)
GEN_TWOVEC_TEST(fsqrt_8h_10_11, "fsqrt v10.8h, v11.8h",    10, 11)
GEN_TWOVEC_TEST(fsqrt_8h_11_12, "fsqrt v11.8h, v12.8h",    11, 12)
GEN_TWOVEC_TEST(fsqrt_8h_12_13, "fsqrt v12.8h, v13.8h",    12, 13)
GEN_TWOVEC_TEST(fsqrt_8h_13_14, "fsqrt v13.8h, v14.8h",    13, 14)
GEN_TWOVEC_TEST(fsqrt_8h_14_15, "fsqrt v14.8h, v15.8h",    14, 15)
GEN_TWOVEC_TEST(fsqrt_8h_15_16, "fsqrt v15.8h, v16.8h",    15, 16)
GEN_TWOVEC_TEST(fsqrt_8h_16_17, "fsqrt v16.8h, v17.8h",    16, 17)
GEN_TWOVEC_TEST(fsqrt_8h_17_18, "fsqrt v17.8h, v18.8h",    17, 18)
GEN_TWOVEC_TEST(fsqrt_8h_18_19, "fsqrt v18.8h, v19.8h",    18, 19)
GEN_TWOVEC_TEST(fsqrt_8h_19_20, "fsqrt v19.8h, v20.8h",    19, 20)
GEN_TWOVEC_TEST(fsqrt_8h_20_21, "fsqrt v20.8h, v21.8h",    20, 21)
GEN_TWOVEC_TEST(fsqrt_8h_21_22, "fsqrt v21.8h, v22.8h",    21, 22)
GEN_TWOVEC_TEST(fsqrt_8h_22_23, "fsqrt v22.8h, v23.8h",    22, 23)
GEN_TWOVEC_TEST(fsqrt_8h_23_24, "fsqrt v23.8h, v24.8h",    23, 24)
GEN_TWOVEC_TEST(fsqrt_8h_24_25, "fsqrt v24.8h, v25.8h",    24, 25)
GEN_TWOVEC_TEST(fsqrt_8h_25_26, "fsqrt v25.8h, v26.8h",    25, 26)
GEN_TWOVEC_TEST(fsqrt_8h_26_27, "fsqrt v26.8h, v27.8h",    26, 27)
GEN_TWOVEC_TEST(fsqrt_8h_27_28, "fsqrt v27.8h, v28.8h",    27, 28)
GEN_TWOVEC_TEST(fsqrt_8h_28_29, "fsqrt v28.8h, v29.8h",    28, 29)
GEN_TWOVEC_TEST(fsqrt_8h_29_30, "fsqrt v29.8h, v30.8h",    29, 30)
GEN_TWOVEC_TEST(fsqrt_8h_30_31, "fsqrt v30.8h, v31.8h",    30, 31)

GEN_TWOVEC_TEST(fsqrt_4h_00_01, "fsqrt v0.4h, v1.4h",    0, 1)
GEN_TWOVEC_TEST(fsqrt_4h_01_02, "fsqrt v1.4h, v2.4h",    1, 2)
GEN_TWOVEC_TEST(fsqrt_4h_02_03, "fsqrt v2.4h, v3.4h",    2, 3)
GEN_TWOVEC_TEST(fsqrt_4h_03_04, "fsqrt v3.4h, v4.4h",    3, 4)
GEN_TWOVEC_TEST(fsqrt_4h_04_05, "fsqrt v4.4h, v5.4h",    4, 5)
GEN_TWOVEC_TEST(fsqrt_4h_05_06, "fsqrt v5.4h, v6.4h",    5, 6)
GEN_TWOVEC_TEST(fsqrt_4h_06_07, "fsqrt v6.4h, v7.4h",    6, 7)
GEN_TWOVEC_TEST(fsqrt_4h_07_08, "fsqrt v7.4h, v8.4h",    7, 8)
GEN_TWOVEC_TEST(fsqrt_4h_08_09, "fsqrt v8.4h, v9.4h",    8, 9)
GEN_TWOVEC_TEST(fsqrt_4h_09_10, "fsqrt v9.4h, v10.4h",    9, 10)
GEN_TWOVEC_TEST(fsqrt_4h_10_11, "fsqrt v10.4h, v11.4h",    10, 11)
GEN_TWOVEC_TEST(fsqrt_4h_11_12, "fsqrt v11.4h, v12.4h",    11, 12)
GEN_TWOVEC_TEST(fsqrt_4h_12_13, "fsqrt v12.4h, v13.4h",    12, 13)
GEN_TWOVEC_TEST(fsqrt_4h_13_14, "fsqrt v13.4h, v14.4h",    13, 14)
GEN_TWOVEC_TEST(fsqrt_4h_14_15, "fsqrt v14.4h, v15.4h",    14, 15)
GEN_TWOVEC_TEST(fsqrt_4h_15_16, "fsqrt v15.4h, v16.4h",    15, 16)
GEN_TWOVEC_TEST(fsqrt_4h_16_17, "fsqrt v16.4h, v17.4h",    16, 17)
GEN_TWOVEC_TEST(fsqrt_4h_17_18, "fsqrt v17.4h, v18.4h",    17, 18)
GEN_TWOVEC_TEST(fsqrt_4h_18_19, "fsqrt v18.4h, v19.4h",    18, 19)
GEN_TWOVEC_TEST(fsqrt_4h_19_20, "fsqrt v19.4h, v20.4h",    19, 20)
GEN_TWOVEC_TEST(fsqrt_4h_20_21, "fsqrt v20.4h, v21.4h",    20, 21)
GEN_TWOVEC_TEST(fsqrt_4h_21_22, "fsqrt v21.4h, v22.4h",    21, 22)
GEN_TWOVEC_TEST(fsqrt_4h_22_23, "fsqrt v22.4h, v23.4h",    22, 23)
GEN_TWOVEC_TEST(fsqrt_4h_23_24, "fsqrt v23.4h, v24.4h",    23, 24)
GEN_TWOVEC_TEST(fsqrt_4h_24_25, "fsqrt v24.4h, v25.4h",    24, 25)
GEN_TWOVEC_TEST(fsqrt_4h_25_26, "fsqrt v25.4h, v26.4h",    25, 26)
GEN_TWOVEC_TEST(fsqrt_4h_26_27, "fsqrt v26.4h, v27.4h",    26, 27)
GEN_TWOVEC_TEST(fsqrt_4h_27_28, "fsqrt v27.4h, v28.4h",    27, 28)
GEN_TWOVEC_TEST(fsqrt_4h_28_29, "fsqrt v28.4h, v29.4h",    28, 29)
GEN_TWOVEC_TEST(fsqrt_4h_29_30, "fsqrt v29.4h, v30.4h",    29, 30)
GEN_TWOVEC_TEST(fsqrt_4h_30_31, "fsqrt v30.4h, v31.4h",    30, 31)

// FABD <Hd>, <Hn>, <Hm>

GEN_THREEVEC_TEST(fabd_h_00_01_02, "fabd h0, h1, h2",    0, 1, 2)
GEN_THREEVEC_TEST(fabd_h_01_02_03, "fabd h1, h2, h3",    1, 2, 3)
GEN_THREEVEC_TEST(fabd_h_02_03_04, "fabd h2, h3, h4",    2, 3, 4)
GEN_THREEVEC_TEST(fabd_h_03_04_05, "fabd h3, h4, h5",    3, 4, 5)
GEN_THREEVEC_TEST(fabd_h_04_05_06, "fabd h4, h5, h6",    4, 5, 6)
GEN_THREEVEC_TEST(fabd_h_05_06_07, "fabd h5, h6, h7",    5, 6, 7)
GEN_THREEVEC_TEST(fabd_h_06_07_08, "fabd h6, h7, h8",    6, 7, 8)
GEN_THREEVEC_TEST(fabd_h_07_08_09, "fabd h7, h8, h9",    7, 8, 9)
GEN_THREEVEC_TEST(fabd_h_08_09_10, "fabd h8, h9, h10",    8, 9, 10)
GEN_THREEVEC_TEST(fabd_h_09_10_11, "fabd h9, h10, h11",    9, 10, 11)
GEN_THREEVEC_TEST(fabd_h_10_11_12, "fabd h10, h11, h12",    10, 11, 12)
GEN_THREEVEC_TEST(fabd_h_11_12_13, "fabd h11, h12, h13",    11, 12, 13)
GEN_THREEVEC_TEST(fabd_h_12_13_14, "fabd h12, h13, h14",    12, 13, 14)
GEN_THREEVEC_TEST(fabd_h_13_14_15, "fabd h13, h14, h15",    13, 14, 15)
GEN_THREEVEC_TEST(fabd_h_14_15_16, "fabd h14, h15, h16",    14, 15, 16)
GEN_THREEVEC_TEST(fabd_h_15_16_17, "fabd h15, h16, h17",    15, 16, 17)
GEN_THREEVEC_TEST(fabd_h_16_17_18, "fabd h16, h17, h18",    16, 17, 18)
GEN_THREEVEC_TEST(fabd_h_17_18_19, "fabd h17, h18, h19",    17, 18, 19)
GEN_THREEVEC_TEST(fabd_h_18_19_20, "fabd h18, h19, h20",    18, 19, 20)
GEN_THREEVEC_TEST(fabd_h_19_20_21, "fabd h19, h20, h21",    19, 20, 21)
GEN_THREEVEC_TEST(fabd_h_20_21_22, "fabd h20, h21, h22",    20, 21, 22)
GEN_THREEVEC_TEST(fabd_h_21_22_23, "fabd h21, h22, h23",    21, 22, 23)
GEN_THREEVEC_TEST(fabd_h_22_23_24, "fabd h22, h23, h24",    22, 23, 24)
GEN_THREEVEC_TEST(fabd_h_23_24_25, "fabd h23, h24, h25",    23, 24, 25)
GEN_THREEVEC_TEST(fabd_h_24_25_26, "fabd h24, h25, h26",    24, 25, 26)
GEN_THREEVEC_TEST(fabd_h_25_26_27, "fabd h25, h26, h27",    25, 26, 27)
GEN_THREEVEC_TEST(fabd_h_26_27_28, "fabd h26, h27, h28",    26, 27, 28)
GEN_THREEVEC_TEST(fabd_h_27_28_29, "fabd h27, h28, h29",    27, 28, 29)
GEN_THREEVEC_TEST(fabd_h_28_29_30, "fabd h28, h29, h30",    28, 29, 30)
GEN_THREEVEC_TEST(fabd_h_29_30_31, "fabd h29, h30, h31",    29, 30, 31)

// FABD <Vd>.<T>, <Vn>.<T>, <Vm>.<T>

GEN_THREEVEC_TEST(fabd_8h_00_01_02, "fabd v0.8h, v1.8h, v2.8h",    0, 1, 2)
GEN_THREEVEC_TEST(fabd_8h_01_02_03, "fabd v1.8h, v2.8h, v3.8h",    1, 2, 3)
GEN_THREEVEC_TEST(fabd_8h_02_03_04, "fabd v2.8h, v3.8h, v4.8h",    2, 3, 4)
GEN_THREEVEC_TEST(fabd_8h_03_04_05, "fabd v3.8h, v4.8h, v5.8h",    3, 4, 5)
GEN_THREEVEC_TEST(fabd_8h_04_05_06, "fabd v4.8h, v5.8h, v6.8h",    4, 5, 6)
GEN_THREEVEC_TEST(fabd_8h_05_06_07, "fabd v5.8h, v6.8h, v7.8h",    5, 6, 7)
GEN_THREEVEC_TEST(fabd_8h_06_07_08, "fabd v6.8h, v7.8h, v8.8h",    6, 7, 8)
GEN_THREEVEC_TEST(fabd_8h_07_08_09, "fabd v7.8h, v8.8h, v9.8h",    7, 8, 9)
GEN_THREEVEC_TEST(fabd_8h_08_09_10, "fabd v8.8h, v9.8h, v10.8h",    8, 9, 10)
GEN_THREEVEC_TEST(fabd_8h_09_10_11, "fabd v9.8h, v10.8h, v11.8h",    9, 10, 11)
GEN_THREEVEC_TEST(fabd_8h_10_11_12, "fabd v10.8h, v11.8h, v12.8h",    10, 11, 12)
GEN_THREEVEC_TEST(fabd_8h_11_12_13, "fabd v11.8h, v12.8h, v13.8h",    11, 12, 13)
GEN_THREEVEC_TEST(fabd_8h_12_13_14, "fabd v12.8h, v13.8h, v14.8h",    12, 13, 14)
GEN_THREEVEC_TEST(fabd_8h_13_14_15, "fabd v13.8h, v14.8h, v15.8h",    13, 14, 15)
GEN_THREEVEC_TEST(fabd_8h_14_15_16, "fabd v14.8h, v15.8h, v16.8h",    14, 15, 16)
GEN_THREEVEC_TEST(fabd_8h_15_16_17, "fabd v15.8h, v16.8h, v17.8h",    15, 16, 17)
GEN_THREEVEC_TEST(fabd_8h_16_17_18, "fabd v16.8h, v17.8h, v18.8h",    16, 17, 18)
GEN_THREEVEC_TEST(fabd_8h_17_18_19, "fabd v17.8h, v18.8h, v19.8h",    17, 18, 19)
GEN_THREEVEC_TEST(fabd_8h_18_19_20, "fabd v18.8h, v19.8h, v20.8h",    18, 19, 20)
GEN_THREEVEC_TEST(fabd_8h_19_20_21, "fabd v19.8h, v20.8h, v21.8h",    19, 20, 21)
GEN_THREEVEC_TEST(fabd_8h_20_21_22, "fabd v20.8h, v21.8h, v22.8h",    20, 21, 22)
GEN_THREEVEC_TEST(fabd_8h_21_22_23, "fabd v21.8h, v22.8h, v23.8h",    21, 22, 23)
GEN_THREEVEC_TEST(fabd_8h_22_23_24, "fabd v22.8h, v23.8h, v24.8h",    22, 23, 24)
GEN_THREEVEC_TEST(fabd_8h_23_24_25, "fabd v23.8h, v24.8h, v25.8h",    23, 24, 25)
GEN_THREEVEC_TEST(fabd_8h_24_25_26, "fabd v24.8h, v25.8h, v26.8h",    24, 25, 26)
GEN_THREEVEC_TEST(fabd_8h_25_26_27, "fabd v25.8h, v26.8h, v27.8h",    25, 26, 27)
GEN_THREEVEC_TEST(fabd_8h_26_27_28, "fabd v26.8h, v27.8h, v28.8h",    26, 27, 28)
GEN_THREEVEC_TEST(fabd_8h_27_28_29, "fabd v27.8h, v28.8h, v29.8h",    27, 28, 29)
GEN_THREEVEC_TEST(fabd_8h_28_29_30, "fabd v28.8h, v29.8h, v30.8h",    28, 29, 30)
GEN_THREEVEC_TEST(fabd_8h_29_30_31, "fabd v29.8h, v30.8h, v31.8h",    29, 30, 31)

GEN_THREEVEC_TEST(fabd_4h_00_01_02, "fabd v0.4h, v1.4h, v2.4h",    0, 1, 2)
GEN_THREEVEC_TEST(fabd_4h_01_02_03, "fabd v1.4h, v2.4h, v3.4h",    1, 2, 3)
GEN_THREEVEC_TEST(fabd_4h_02_03_04, "fabd v2.4h, v3.4h, v4.4h",    2, 3, 4)
GEN_THREEVEC_TEST(fabd_4h_03_04_05, "fabd v3.4h, v4.4h, v5.4h",    3, 4, 5)
GEN_THREEVEC_TEST(fabd_4h_04_05_06, "fabd v4.4h, v5.4h, v6.4h",    4, 5, 6)
GEN_THREEVEC_TEST(fabd_4h_05_06_07, "fabd v5.4h, v6.4h, v7.4h",    5, 6, 7)
GEN_THREEVEC_TEST(fabd_4h_06_07_08, "fabd v6.4h, v7.4h, v8.4h",    6, 7, 8)
GEN_THREEVEC_TEST(fabd_4h_07_08_09, "fabd v7.4h, v8.4h, v9.4h",    7, 8, 9)
GEN_THREEVEC_TEST(fabd_4h_08_09_10, "fabd v8.4h, v9.4h, v10.4h",    8, 9, 10)
GEN_THREEVEC_TEST(fabd_4h_09_10_11, "fabd v9.4h, v10.4h, v11.4h",    9, 10, 11)
GEN_THREEVEC_TEST(fabd_4h_10_11_12, "fabd v10.4h, v11.4h, v12.4h",    10, 11, 12)
GEN_THREEVEC_TEST(fabd_4h_11_12_13, "fabd v11.4h, v12.4h, v13.4h",    11, 12, 13)
GEN_THREEVEC_TEST(fabd_4h_12_13_14, "fabd v12.4h, v13.4h, v14.4h",    12, 13, 14)
GEN_THREEVEC_TEST(fabd_4h_13_14_15, "fabd v13.4h, v14.4h, v15.4h",    13, 14, 15)
GEN_THREEVEC_TEST(fabd_4h_14_15_16, "fabd v14.4h, v15.4h, v16.4h",    14, 15, 16)
GEN_THREEVEC_TEST(fabd_4h_15_16_17, "fabd v15.4h, v16.4h, v17.4h",    15, 16, 17)
GEN_THREEVEC_TEST(fabd_4h_16_17_18, "fabd v16.4h, v17.4h, v18.4h",    16, 17, 18)
GEN_THREEVEC_TEST(fabd_4h_17_18_19, "fabd v17.4h, v18.4h, v19.4h",    17, 18, 19)
GEN_THREEVEC_TEST(fabd_4h_18_19_20, "fabd v18.4h, v19.4h, v20.4h",    18, 19, 20)
GEN_THREEVEC_TEST(fabd_4h_19_20_21, "fabd v19.4h, v20.4h, v21.4h",    19, 20, 21)
GEN_THREEVEC_TEST(fabd_4h_20_21_22, "fabd v20.4h, v21.4h, v22.4h",    20, 21, 22)
GEN_THREEVEC_TEST(fabd_4h_21_22_23, "fabd v21.4h, v22.4h, v23.4h",    21, 22, 23)
GEN_THREEVEC_TEST(fabd_4h_22_23_24, "fabd v22.4h, v23.4h, v24.4h",    22, 23, 24)
GEN_THREEVEC_TEST(fabd_4h_23_24_25, "fabd v23.4h, v24.4h, v25.4h",    23, 24, 25)
GEN_THREEVEC_TEST(fabd_4h_24_25_26, "fabd v24.4h, v25.4h, v26.4h",    24, 25, 26)
GEN_THREEVEC_TEST(fabd_4h_25_26_27, "fabd v25.4h, v26.4h, v27.4h",    25, 26, 27)
GEN_THREEVEC_TEST(fabd_4h_26_27_28, "fabd v26.4h, v27.4h, v28.4h",    26, 27, 28)
GEN_THREEVEC_TEST(fabd_4h_27_28_29, "fabd v27.4h, v28.4h, v29.4h",    27, 28, 29)
GEN_THREEVEC_TEST(fabd_4h_28_29_30, "fabd v28.4h, v29.4h, v30.4h",    28, 29, 30)
GEN_THREEVEC_TEST(fabd_4h_29_30_31, "fabd v29.4h, v30.4h, v31.4h",    29, 30, 31)

// FACGT <Hd>, <Hn>, <Hm>

GEN_THREEVEC_TEST(facgt_h_00_01_02, "facgt h0, h1, h2",    0, 1, 2)
GEN_THREEVEC_TEST(facgt_h_01_02_03, "facgt h1, h2, h3",    1, 2, 3)
GEN_THREEVEC_TEST(facgt_h_02_03_04, "facgt h2, h3, h4",    2, 3, 4)
GEN_THREEVEC_TEST(facgt_h_03_04_05, "facgt h3, h4, h5",    3, 4, 5)
GEN_THREEVEC_TEST(facgt_h_04_05_06, "facgt h4, h5, h6",    4, 5, 6)
GEN_THREEVEC_TEST(facgt_h_05_06_07, "facgt h5, h6, h7",    5, 6, 7)
GEN_THREEVEC_TEST(facgt_h_06_07_08, "facgt h6, h7, h8",    6, 7, 8)
GEN_THREEVEC_TEST(facgt_h_07_08_09, "facgt h7, h8, h9",    7, 8, 9)
GEN_THREEVEC_TEST(facgt_h_08_09_10, "facgt h8, h9, h10",    8, 9, 10)
GEN_THREEVEC_TEST(facgt_h_09_10_11, "facgt h9, h10, h11",    9, 10, 11)
GEN_THREEVEC_TEST(facgt_h_10_11_12, "facgt h10, h11, h12",    10, 11, 12)
GEN_THREEVEC_TEST(facgt_h_11_12_13, "facgt h11, h12, h13",    11, 12, 13)
GEN_THREEVEC_TEST(facgt_h_12_13_14, "facgt h12, h13, h14",    12, 13, 14)
GEN_THREEVEC_TEST(facgt_h_13_14_15, "facgt h13, h14, h15",    13, 14, 15)
GEN_THREEVEC_TEST(facgt_h_14_15_16, "facgt h14, h15, h16",    14, 15, 16)
GEN_THREEVEC_TEST(facgt_h_15_16_17, "facgt h15, h16, h17",    15, 16, 17)
GEN_THREEVEC_TEST(facgt_h_16_17_18, "facgt h16, h17, h18",    16, 17, 18)
GEN_THREEVEC_TEST(facgt_h_17_18_19, "facgt h17, h18, h19",    17, 18, 19)
GEN_THREEVEC_TEST(facgt_h_18_19_20, "facgt h18, h19, h20",    18, 19, 20)
GEN_THREEVEC_TEST(facgt_h_19_20_21, "facgt h19, h20, h21",    19, 20, 21)
GEN_THREEVEC_TEST(facgt_h_20_21_22, "facgt h20, h21, h22",    20, 21, 22)
GEN_THREEVEC_TEST(facgt_h_21_22_23, "facgt h21, h22, h23",    21, 22, 23)
GEN_THREEVEC_TEST(facgt_h_22_23_24, "facgt h22, h23, h24",    22, 23, 24)
GEN_THREEVEC_TEST(facgt_h_23_24_25, "facgt h23, h24, h25",    23, 24, 25)
GEN_THREEVEC_TEST(facgt_h_24_25_26, "facgt h24, h25, h26",    24, 25, 26)
GEN_THREEVEC_TEST(facgt_h_25_26_27, "facgt h25, h26, h27",    25, 26, 27)
GEN_THREEVEC_TEST(facgt_h_26_27_28, "facgt h26, h27, h28",    26, 27, 28)
GEN_THREEVEC_TEST(facgt_h_27_28_29, "facgt h27, h28, h29",    27, 28, 29)
GEN_THREEVEC_TEST(facgt_h_28_29_30, "facgt h28, h29, h30",    28, 29, 30)
GEN_THREEVEC_TEST(facgt_h_29_30_31, "facgt h29, h30, h31",    29, 30, 31)

// FACGT <Vd>.<T>, <Vn>.<T>, <Vm>.<T>

GEN_THREEVEC_TEST(facgt_8h_00_01_02, "facgt v0.8h, v1.8h, v2.8h",    0, 1, 2)
GEN_THREEVEC_TEST(facgt_8h_01_02_03, "facgt v1.8h, v2.8h, v3.8h",    1, 2, 3)
GEN_THREEVEC_TEST(facgt_8h_02_03_04, "facgt v2.8h, v3.8h, v4.8h",    2, 3, 4)
GEN_THREEVEC_TEST(facgt_8h_03_04_05, "facgt v3.8h, v4.8h, v5.8h",    3, 4, 5)
GEN_THREEVEC_TEST(facgt_8h_04_05_06, "facgt v4.8h, v5.8h, v6.8h",    4, 5, 6)
GEN_THREEVEC_TEST(facgt_8h_05_06_07, "facgt v5.8h, v6.8h, v7.8h",    5, 6, 7)
GEN_THREEVEC_TEST(facgt_8h_06_07_08, "facgt v6.8h, v7.8h, v8.8h",    6, 7, 8)
GEN_THREEVEC_TEST(facgt_8h_07_08_09, "facgt v7.8h, v8.8h, v9.8h",    7, 8, 9)
GEN_THREEVEC_TEST(facgt_8h_08_09_10, "facgt v8.8h, v9.8h, v10.8h",    8, 9, 10)
GEN_THREEVEC_TEST(facgt_8h_09_10_11, "facgt v9.8h, v10.8h, v11.8h",    9, 10, 11)
GEN_THREEVEC_TEST(facgt_8h_10_11_12, "facgt v10.8h, v11.8h, v12.8h",    10, 11, 12)
GEN_THREEVEC_TEST(facgt_8h_11_12_13, "facgt v11.8h, v12.8h, v13.8h",    11, 12, 13)
GEN_THREEVEC_TEST(facgt_8h_12_13_14, "facgt v12.8h, v13.8h, v14.8h",    12, 13, 14)
GEN_THREEVEC_TEST(facgt_8h_13_14_15, "facgt v13.8h, v14.8h, v15.8h",    13, 14, 15)
GEN_THREEVEC_TEST(facgt_8h_14_15_16, "facgt v14.8h, v15.8h, v16.8h",    14, 15, 16)
GEN_THREEVEC_TEST(facgt_8h_15_16_17, "facgt v15.8h, v16.8h, v17.8h",    15, 16, 17)
GEN_THREEVEC_TEST(facgt_8h_16_17_18, "facgt v16.8h, v17.8h, v18.8h",    16, 17, 18)
GEN_THREEVEC_TEST(facgt_8h_17_18_19, "facgt v17.8h, v18.8h, v19.8h",    17, 18, 19)
GEN_THREEVEC_TEST(facgt_8h_18_19_20, "facgt v18.8h, v19.8h, v20.8h",    18, 19, 20)
GEN_THREEVEC_TEST(facgt_8h_19_20_21, "facgt v19.8h, v20.8h, v21.8h",    19, 20, 21)
GEN_THREEVEC_TEST(facgt_8h_20_21_22, "facgt v20.8h, v21.8h, v22.8h",    20, 21, 22)
GEN_THREEVEC_TEST(facgt_8h_21_22_23, "facgt v21.8h, v22.8h, v23.8h",    21, 22, 23)
GEN_THREEVEC_TEST(facgt_8h_22_23_24, "facgt v22.8h, v23.8h, v24.8h",    22, 23, 24)
GEN_THREEVEC_TEST(facgt_8h_23_24_25, "facgt v23.8h, v24.8h, v25.8h",    23, 24, 25)
GEN_THREEVEC_TEST(facgt_8h_24_25_26, "facgt v24.8h, v25.8h, v26.8h",    24, 25, 26)
GEN_THREEVEC_TEST(facgt_8h_25_26_27, "facgt v25.8h, v26.8h, v27.8h",    25, 26, 27)
GEN_THREEVEC_TEST(facgt_8h_26_27_28, "facgt v26.8h, v27.8h, v28.8h",    26, 27, 28)
GEN_THREEVEC_TEST(facgt_8h_27_28_29, "facgt v27.8h, v28.8h, v29.8h",    27, 28, 29)
GEN_THREEVEC_TEST(facgt_8h_28_29_30, "facgt v28.8h, v29.8h, v30.8h",    28, 29, 30)
GEN_THREEVEC_TEST(facgt_8h_29_30_31, "facgt v29.8h, v30.8h, v31.8h",    29, 30, 31)

GEN_THREEVEC_TEST(facgt_4h_00_01_02, "facgt v0.4h, v1.4h, v2.4h",    0, 1, 2)
GEN_THREEVEC_TEST(facgt_4h_01_02_03, "facgt v1.4h, v2.4h, v3.4h",    1, 2, 3)
GEN_THREEVEC_TEST(facgt_4h_02_03_04, "facgt v2.4h, v3.4h, v4.4h",    2, 3, 4)
GEN_THREEVEC_TEST(facgt_4h_03_04_05, "facgt v3.4h, v4.4h, v5.4h",    3, 4, 5)
GEN_THREEVEC_TEST(facgt_4h_04_05_06, "facgt v4.4h, v5.4h, v6.4h",    4, 5, 6)
GEN_THREEVEC_TEST(facgt_4h_05_06_07, "facgt v5.4h, v6.4h, v7.4h",    5, 6, 7)
GEN_THREEVEC_TEST(facgt_4h_06_07_08, "facgt v6.4h, v7.4h, v8.4h",    6, 7, 8)
GEN_THREEVEC_TEST(facgt_4h_07_08_09, "facgt v7.4h, v8.4h, v9.4h",    7, 8, 9)
GEN_THREEVEC_TEST(facgt_4h_08_09_10, "facgt v8.4h, v9.4h, v10.4h",    8, 9, 10)
GEN_THREEVEC_TEST(facgt_4h_09_10_11, "facgt v9.4h, v10.4h, v11.4h",    9, 10, 11)
GEN_THREEVEC_TEST(facgt_4h_10_11_12, "facgt v10.4h, v11.4h, v12.4h",    10, 11, 12)
GEN_THREEVEC_TEST(facgt_4h_11_12_13, "facgt v11.4h, v12.4h, v13.4h",    11, 12, 13)
GEN_THREEVEC_TEST(facgt_4h_12_13_14, "facgt v12.4h, v13.4h, v14.4h",    12, 13, 14)
GEN_THREEVEC_TEST(facgt_4h_13_14_15, "facgt v13.4h, v14.4h, v15.4h",    13, 14, 15)
GEN_THREEVEC_TEST(facgt_4h_14_15_16, "facgt v14.4h, v15.4h, v16.4h",    14, 15, 16)
GEN_THREEVEC_TEST(facgt_4h_15_16_17, "facgt v15.4h, v16.4h, v17.4h",    15, 16, 17)
GEN_THREEVEC_TEST(facgt_4h_16_17_18, "facgt v16.4h, v17.4h, v18.4h",    16, 17, 18)
GEN_THREEVEC_TEST(facgt_4h_17_18_19, "facgt v17.4h, v18.4h, v19.4h",    17, 18, 19)
GEN_THREEVEC_TEST(facgt_4h_18_19_20, "facgt v18.4h, v19.4h, v20.4h",    18, 19, 20)
GEN_THREEVEC_TEST(facgt_4h_19_20_21, "facgt v19.4h, v20.4h, v21.4h",    19, 20, 21)
GEN_THREEVEC_TEST(facgt_4h_20_21_22, "facgt v20.4h, v21.4h, v22.4h",    20, 21, 22)
GEN_THREEVEC_TEST(facgt_4h_21_22_23, "facgt v21.4h, v22.4h, v23.4h",    21, 22, 23)
GEN_THREEVEC_TEST(facgt_4h_22_23_24, "facgt v22.4h, v23.4h, v24.4h",    22, 23, 24)
GEN_THREEVEC_TEST(facgt_4h_23_24_25, "facgt v23.4h, v24.4h, v25.4h",    23, 24, 25)
GEN_THREEVEC_TEST(facgt_4h_24_25_26, "facgt v24.4h, v25.4h, v26.4h",    24, 25, 26)
GEN_THREEVEC_TEST(facgt_4h_25_26_27, "facgt v25.4h, v26.4h, v27.4h",    25, 26, 27)
GEN_THREEVEC_TEST(facgt_4h_26_27_28, "facgt v26.4h, v27.4h, v28.4h",    26, 27, 28)
GEN_THREEVEC_TEST(facgt_4h_27_28_29, "facgt v27.4h, v28.4h, v29.4h",    27, 28, 29)
GEN_THREEVEC_TEST(facgt_4h_28_29_30, "facgt v28.4h, v29.4h, v30.4h",    28, 29, 30)
GEN_THREEVEC_TEST(facgt_4h_29_30_31, "facgt v29.4h, v30.4h, v31.4h",    29, 30, 31)

// FACGE <Hd>, <Hn>, <Hm>

GEN_THREEVEC_TEST(facge_h_00_01_02, "facge h0, h1, h2",    0, 1, 2)
GEN_THREEVEC_TEST(facge_h_01_02_03, "facge h1, h2, h3",    1, 2, 3)
GEN_THREEVEC_TEST(facge_h_02_03_04, "facge h2, h3, h4",    2, 3, 4)
GEN_THREEVEC_TEST(facge_h_03_04_05, "facge h3, h4, h5",    3, 4, 5)
GEN_THREEVEC_TEST(facge_h_04_05_06, "facge h4, h5, h6",    4, 5, 6)
GEN_THREEVEC_TEST(facge_h_05_06_07, "facge h5, h6, h7",    5, 6, 7)
GEN_THREEVEC_TEST(facge_h_06_07_08, "facge h6, h7, h8",    6, 7, 8)
GEN_THREEVEC_TEST(facge_h_07_08_09, "facge h7, h8, h9",    7, 8, 9)
GEN_THREEVEC_TEST(facge_h_08_09_10, "facge h8, h9, h10",    8, 9, 10)
GEN_THREEVEC_TEST(facge_h_09_10_11, "facge h9, h10, h11",    9, 10, 11)
GEN_THREEVEC_TEST(facge_h_10_11_12, "facge h10, h11, h12",    10, 11, 12)
GEN_THREEVEC_TEST(facge_h_11_12_13, "facge h11, h12, h13",    11, 12, 13)
GEN_THREEVEC_TEST(facge_h_12_13_14, "facge h12, h13, h14",    12, 13, 14)
GEN_THREEVEC_TEST(facge_h_13_14_15, "facge h13, h14, h15",    13, 14, 15)
GEN_THREEVEC_TEST(facge_h_14_15_16, "facge h14, h15, h16",    14, 15, 16)
GEN_THREEVEC_TEST(facge_h_15_16_17, "facge h15, h16, h17",    15, 16, 17)
GEN_THREEVEC_TEST(facge_h_16_17_18, "facge h16, h17, h18",    16, 17, 18)
GEN_THREEVEC_TEST(facge_h_17_18_19, "facge h17, h18, h19",    17, 18, 19)
GEN_THREEVEC_TEST(facge_h_18_19_20, "facge h18, h19, h20",    18, 19, 20)
GEN_THREEVEC_TEST(facge_h_19_20_21, "facge h19, h20, h21",    19, 20, 21)
GEN_THREEVEC_TEST(facge_h_20_21_22, "facge h20, h21, h22",    20, 21, 22)
GEN_THREEVEC_TEST(facge_h_21_22_23, "facge h21, h22, h23",    21, 22, 23)
GEN_THREEVEC_TEST(facge_h_22_23_24, "facge h22, h23, h24",    22, 23, 24)
GEN_THREEVEC_TEST(facge_h_23_24_25, "facge h23, h24, h25",    23, 24, 25)
GEN_THREEVEC_TEST(facge_h_24_25_26, "facge h24, h25, h26",    24, 25, 26)
GEN_THREEVEC_TEST(facge_h_25_26_27, "facge h25, h26, h27",    25, 26, 27)
GEN_THREEVEC_TEST(facge_h_26_27_28, "facge h26, h27, h28",    26, 27, 28)
GEN_THREEVEC_TEST(facge_h_27_28_29, "facge h27, h28, h29",    27, 28, 29)
GEN_THREEVEC_TEST(facge_h_28_29_30, "facge h28, h29, h30",    28, 29, 30)
GEN_THREEVEC_TEST(facge_h_29_30_31, "facge h29, h30, h31",    29, 30, 31)

// FACGE <Vd>.<T>, <Vn>.<T>, <Vm>.<T>

GEN_THREEVEC_TEST(facge_8h_00_01_02, "facge v0.8h, v1.8h, v2.8h",    0, 1, 2)
GEN_THREEVEC_TEST(facge_8h_01_02_03, "facge v1.8h, v2.8h, v3.8h",    1, 2, 3)
GEN_THREEVEC_TEST(facge_8h_02_03_04, "facge v2.8h, v3.8h, v4.8h",    2, 3, 4)
GEN_THREEVEC_TEST(facge_8h_03_04_05, "facge v3.8h, v4.8h, v5.8h",    3, 4, 5)
GEN_THREEVEC_TEST(facge_8h_04_05_06, "facge v4.8h, v5.8h, v6.8h",    4, 5, 6)
GEN_THREEVEC_TEST(facge_8h_05_06_07, "facge v5.8h, v6.8h, v7.8h",    5, 6, 7)
GEN_THREEVEC_TEST(facge_8h_06_07_08, "facge v6.8h, v7.8h, v8.8h",    6, 7, 8)
GEN_THREEVEC_TEST(facge_8h_07_08_09, "facge v7.8h, v8.8h, v9.8h",    7, 8, 9)
GEN_THREEVEC_TEST(facge_8h_08_09_10, "facge v8.8h, v9.8h, v10.8h",    8, 9, 10)
GEN_THREEVEC_TEST(facge_8h_09_10_11, "facge v9.8h, v10.8h, v11.8h",    9, 10, 11)
GEN_THREEVEC_TEST(facge_8h_10_11_12, "facge v10.8h, v11.8h, v12.8h",    10, 11, 12)
GEN_THREEVEC_TEST(facge_8h_11_12_13, "facge v11.8h, v12.8h, v13.8h",    11, 12, 13)
GEN_THREEVEC_TEST(facge_8h_12_13_14, "facge v12.8h, v13.8h, v14.8h",    12, 13, 14)
GEN_THREEVEC_TEST(facge_8h_13_14_15, "facge v13.8h, v14.8h, v15.8h",    13, 14, 15)
GEN_THREEVEC_TEST(facge_8h_14_15_16, "facge v14.8h, v15.8h, v16.8h",    14, 15, 16)
GEN_THREEVEC_TEST(facge_8h_15_16_17, "facge v15.8h, v16.8h, v17.8h",    15, 16, 17)
GEN_THREEVEC_TEST(facge_8h_16_17_18, "facge v16.8h, v17.8h, v18.8h",    16, 17, 18)
GEN_THREEVEC_TEST(facge_8h_17_18_19, "facge v17.8h, v18.8h, v19.8h",    17, 18, 19)
GEN_THREEVEC_TEST(facge_8h_18_19_20, "facge v18.8h, v19.8h, v20.8h",    18, 19, 20)
GEN_THREEVEC_TEST(facge_8h_19_20_21, "facge v19.8h, v20.8h, v21.8h",    19, 20, 21)
GEN_THREEVEC_TEST(facge_8h_20_21_22, "facge v20.8h, v21.8h, v22.8h",    20, 21, 22)
GEN_THREEVEC_TEST(facge_8h_21_22_23, "facge v21.8h, v22.8h, v23.8h",    21, 22, 23)
GEN_THREEVEC_TEST(facge_8h_22_23_24, "facge v22.8h, v23.8h, v24.8h",    22, 23, 24)
GEN_THREEVEC_TEST(facge_8h_23_24_25, "facge v23.8h, v24.8h, v25.8h",    23, 24, 25)
GEN_THREEVEC_TEST(facge_8h_24_25_26, "facge v24.8h, v25.8h, v26.8h",    24, 25, 26)
GEN_THREEVEC_TEST(facge_8h_25_26_27, "facge v25.8h, v26.8h, v27.8h",    25, 26, 27)
GEN_THREEVEC_TEST(facge_8h_26_27_28, "facge v26.8h, v27.8h, v28.8h",    26, 27, 28)
GEN_THREEVEC_TEST(facge_8h_27_28_29, "facge v27.8h, v28.8h, v29.8h",    27, 28, 29)
GEN_THREEVEC_TEST(facge_8h_28_29_30, "facge v28.8h, v29.8h, v30.8h",    28, 29, 30)
GEN_THREEVEC_TEST(facge_8h_29_30_31, "facge v29.8h, v30.8h, v31.8h",    29, 30, 31)

GEN_THREEVEC_TEST(facge_4h_00_01_02, "facge v0.4h, v1.4h, v2.4h",    0, 1, 2)
GEN_THREEVEC_TEST(facge_4h_01_02_03, "facge v1.4h, v2.4h, v3.4h",    1, 2, 3)
GEN_THREEVEC_TEST(facge_4h_02_03_04, "facge v2.4h, v3.4h, v4.4h",    2, 3, 4)
GEN_THREEVEC_TEST(facge_4h_03_04_05, "facge v3.4h, v4.4h, v5.4h",    3, 4, 5)
GEN_THREEVEC_TEST(facge_4h_04_05_06, "facge v4.4h, v5.4h, v6.4h",    4, 5, 6)
GEN_THREEVEC_TEST(facge_4h_05_06_07, "facge v5.4h, v6.4h, v7.4h",    5, 6, 7)
GEN_THREEVEC_TEST(facge_4h_06_07_08, "facge v6.4h, v7.4h, v8.4h",    6, 7, 8)
GEN_THREEVEC_TEST(facge_4h_07_08_09, "facge v7.4h, v8.4h, v9.4h",    7, 8, 9)
GEN_THREEVEC_TEST(facge_4h_08_09_10, "facge v8.4h, v9.4h, v10.4h",    8, 9, 10)
GEN_THREEVEC_TEST(facge_4h_09_10_11, "facge v9.4h, v10.4h, v11.4h",    9, 10, 11)
GEN_THREEVEC_TEST(facge_4h_10_11_12, "facge v10.4h, v11.4h, v12.4h",    10, 11, 12)
GEN_THREEVEC_TEST(facge_4h_11_12_13, "facge v11.4h, v12.4h, v13.4h",    11, 12, 13)
GEN_THREEVEC_TEST(facge_4h_12_13_14, "facge v12.4h, v13.4h, v14.4h",    12, 13, 14)
GEN_THREEVEC_TEST(facge_4h_13_14_15, "facge v13.4h, v14.4h, v15.4h",    13, 14, 15)
GEN_THREEVEC_TEST(facge_4h_14_15_16, "facge v14.4h, v15.4h, v16.4h",    14, 15, 16)
GEN_THREEVEC_TEST(facge_4h_15_16_17, "facge v15.4h, v16.4h, v17.4h",    15, 16, 17)
GEN_THREEVEC_TEST(facge_4h_16_17_18, "facge v16.4h, v17.4h, v18.4h",    16, 17, 18)
GEN_THREEVEC_TEST(facge_4h_17_18_19, "facge v17.4h, v18.4h, v19.4h",    17, 18, 19)
GEN_THREEVEC_TEST(facge_4h_18_19_20, "facge v18.4h, v19.4h, v20.4h",    18, 19, 20)
GEN_THREEVEC_TEST(facge_4h_19_20_21, "facge v19.4h, v20.4h, v21.4h",    19, 20, 21)
GEN_THREEVEC_TEST(facge_4h_20_21_22, "facge v20.4h, v21.4h, v22.4h",    20, 21, 22)
GEN_THREEVEC_TEST(facge_4h_21_22_23, "facge v21.4h, v22.4h, v23.4h",    21, 22, 23)
GEN_THREEVEC_TEST(facge_4h_22_23_24, "facge v22.4h, v23.4h, v24.4h",    22, 23, 24)
GEN_THREEVEC_TEST(facge_4h_23_24_25, "facge v23.4h, v24.4h, v25.4h",    23, 24, 25)
GEN_THREEVEC_TEST(facge_4h_24_25_26, "facge v24.4h, v25.4h, v26.4h",    24, 25, 26)
GEN_THREEVEC_TEST(facge_4h_25_26_27, "facge v25.4h, v26.4h, v27.4h",    25, 26, 27)
GEN_THREEVEC_TEST(facge_4h_26_27_28, "facge v26.4h, v27.4h, v28.4h",    26, 27, 28)
GEN_THREEVEC_TEST(facge_4h_27_28_29, "facge v27.4h, v28.4h, v29.4h",    27, 28, 29)
GEN_THREEVEC_TEST(facge_4h_28_29_30, "facge v28.4h, v29.4h, v30.4h",    28, 29, 30)
GEN_THREEVEC_TEST(facge_4h_29_30_31, "facge v29.4h, v30.4h, v31.4h",    29, 30, 31)

/* ---------------------------------------------------------------- */
/* -- main()                                                     -- */
/* ---------------------------------------------------------------- */

int main ( void )
{
   assert(sizeof(V128) == 16);

   printf("\nFADD <Hd>, <Hn>, <Hm>\n\n");

   if (1) test_fadd_h_00_01_02(TyH);
   if (1) test_fadd_h_01_02_03(TyH);
   if (1) test_fadd_h_02_03_04(TyH);
   if (1) test_fadd_h_03_04_05(TyH);
   if (1) test_fadd_h_04_05_06(TyH);
   if (1) test_fadd_h_05_06_07(TyH);
   if (1) test_fadd_h_06_07_08(TyH);
   if (1) test_fadd_h_07_08_09(TyH);
   if (1) test_fadd_h_08_09_10(TyH);
   if (1) test_fadd_h_09_10_11(TyH);
   if (1) test_fadd_h_10_11_12(TyH);
   if (1) test_fadd_h_11_12_13(TyH);
   if (1) test_fadd_h_12_13_14(TyH);
   if (1) test_fadd_h_13_14_15(TyH);
   if (1) test_fadd_h_14_15_16(TyH);
   if (1) test_fadd_h_15_16_17(TyH);
   if (1) test_fadd_h_16_17_18(TyH);
   if (1) test_fadd_h_17_18_19(TyH);
   if (1) test_fadd_h_18_19_20(TyH);
   if (1) test_fadd_h_19_20_21(TyH);
   if (1) test_fadd_h_20_21_22(TyH);
   if (1) test_fadd_h_21_22_23(TyH);
   if (1) test_fadd_h_22_23_24(TyH);
   if (1) test_fadd_h_23_24_25(TyH);
   if (1) test_fadd_h_24_25_26(TyH);
   if (1) test_fadd_h_25_26_27(TyH);
   if (1) test_fadd_h_26_27_28(TyH);
   if (1) test_fadd_h_27_28_29(TyH);
   if (1) test_fadd_h_28_29_30(TyH);
   if (1) test_fadd_h_29_30_31(TyH);

   printf("\nFADD <Vd>.<T>, <Vn>.<T>, <Vm>.<T>\n\n");

   if (1) test_fadd_8h_00_01_02(TyH);
   if (1) test_fadd_8h_01_02_03(TyH);
   if (1) test_fadd_8h_02_03_04(TyH);
   if (1) test_fadd_8h_03_04_05(TyH);
   if (1) test_fadd_8h_04_05_06(TyH);
   if (1) test_fadd_8h_05_06_07(TyH);
   if (1) test_fadd_8h_06_07_08(TyH);
   if (1) test_fadd_8h_07_08_09(TyH);
   if (1) test_fadd_8h_08_09_10(TyH);
   if (1) test_fadd_8h_09_10_11(TyH);
   if (1) test_fadd_8h_10_11_12(TyH);
   if (1) test_fadd_8h_11_12_13(TyH);
   if (1) test_fadd_8h_12_13_14(TyH);
   if (1) test_fadd_8h_13_14_15(TyH);
   if (1) test_fadd_8h_14_15_16(TyH);
   if (1) test_fadd_8h_15_16_17(TyH);
   if (1) test_fadd_8h_16_17_18(TyH);
   if (1) test_fadd_8h_17_18_19(TyH);
   if (1) test_fadd_8h_18_19_20(TyH);
   if (1) test_fadd_8h_19_20_21(TyH);
   if (1) test_fadd_8h_20_21_22(TyH);
   if (1) test_fadd_8h_21_22_23(TyH);
   if (1) test_fadd_8h_22_23_24(TyH);
   if (1) test_fadd_8h_23_24_25(TyH);
   if (1) test_fadd_8h_24_25_26(TyH);
   if (1) test_fadd_8h_25_26_27(TyH);
   if (1) test_fadd_8h_26_27_28(TyH);
   if (1) test_fadd_8h_27_28_29(TyH);
   if (1) test_fadd_8h_28_29_30(TyH);
   if (1) test_fadd_8h_29_30_31(TyH);

   if (1) test_fadd_4h_00_01_02(TyH);
   if (1) test_fadd_4h_01_02_03(TyH);
   if (1) test_fadd_4h_02_03_04(TyH);
   if (1) test_fadd_4h_03_04_05(TyH);
   if (1) test_fadd_4h_04_05_06(TyH);
   if (1) test_fadd_4h_05_06_07(TyH);
   if (1) test_fadd_4h_06_07_08(TyH);
   if (1) test_fadd_4h_07_08_09(TyH);
   if (1) test_fadd_4h_08_09_10(TyH);
   if (1) test_fadd_4h_09_10_11(TyH);
   if (1) test_fadd_4h_10_11_12(TyH);
   if (1) test_fadd_4h_11_12_13(TyH);
   if (1) test_fadd_4h_12_13_14(TyH);
   if (1) test_fadd_4h_13_14_15(TyH);
   if (1) test_fadd_4h_14_15_16(TyH);
   if (1) test_fadd_4h_15_16_17(TyH);
   if (1) test_fadd_4h_16_17_18(TyH);
   if (1) test_fadd_4h_17_18_19(TyH);
   if (1) test_fadd_4h_18_19_20(TyH);
   if (1) test_fadd_4h_19_20_21(TyH);
   if (1) test_fadd_4h_20_21_22(TyH);
   if (1) test_fadd_4h_21_22_23(TyH);
   if (1) test_fadd_4h_22_23_24(TyH);
   if (1) test_fadd_4h_23_24_25(TyH);
   if (1) test_fadd_4h_24_25_26(TyH);
   if (1) test_fadd_4h_25_26_27(TyH);
   if (1) test_fadd_4h_26_27_28(TyH);
   if (1) test_fadd_4h_27_28_29(TyH);
   if (1) test_fadd_4h_28_29_30(TyH);
   if (1) test_fadd_4h_29_30_31(TyH);

   printf("\nFADDP <V><d>, <Vn>.<T>\n\n");

   if (1) test_faddp_h_2h_00_01(TyH);
   if (1) test_faddp_h_2h_01_02(TyH);
   if (1) test_faddp_h_2h_02_03(TyH);
   if (1) test_faddp_h_2h_03_04(TyH);
   if (1) test_faddp_h_2h_04_05(TyH);
   if (1) test_faddp_h_2h_05_06(TyH);
   if (1) test_faddp_h_2h_06_07(TyH);
   if (1) test_faddp_h_2h_07_08(TyH);
   if (1) test_faddp_h_2h_08_09(TyH);
   if (1) test_faddp_h_2h_09_10(TyH);
   if (1) test_faddp_h_2h_10_11(TyH);
   if (1) test_faddp_h_2h_11_12(TyH);
   if (1) test_faddp_h_2h_12_13(TyH);
   if (1) test_faddp_h_2h_13_14(TyH);
   if (1) test_faddp_h_2h_14_15(TyH);
   if (1) test_faddp_h_2h_15_16(TyH);
   if (1) test_faddp_h_2h_16_17(TyH);
   if (1) test_faddp_h_2h_17_18(TyH);
   if (1) test_faddp_h_2h_18_19(TyH);
   if (1) test_faddp_h_2h_19_20(TyH);
   if (1) test_faddp_h_2h_20_21(TyH);
   if (1) test_faddp_h_2h_21_22(TyH);
   if (1) test_faddp_h_2h_22_23(TyH);
   if (1) test_faddp_h_2h_23_24(TyH);
   if (1) test_faddp_h_2h_24_25(TyH);
   if (1) test_faddp_h_2h_25_26(TyH);
   if (1) test_faddp_h_2h_26_27(TyH);
   if (1) test_faddp_h_2h_27_28(TyH);
   if (1) test_faddp_h_2h_28_29(TyH);
   if (1) test_faddp_h_2h_29_30(TyH);
   if (1) test_faddp_h_2h_30_31(TyH);

   printf("\nFADDP <Vd>.<T>, <Vn>.<T>, <Vm>.<T>\n\n");

   if (1) test_faddp_8h_00_01_02(TyH);
   if (1) test_faddp_8h_01_02_03(TyH);
   if (1) test_faddp_8h_02_03_04(TyH);
   if (1) test_faddp_8h_03_04_05(TyH);
   if (1) test_faddp_8h_04_05_06(TyH);
   if (1) test_faddp_8h_05_06_07(TyH);
   if (1) test_faddp_8h_06_07_08(TyH);
   if (1) test_faddp_8h_07_08_09(TyH);
   if (1) test_faddp_8h_08_09_10(TyH);
   if (1) test_faddp_8h_09_10_11(TyH);
   if (1) test_faddp_8h_10_11_12(TyH);
   if (1) test_faddp_8h_11_12_13(TyH);
   if (1) test_faddp_8h_12_13_14(TyH);
   if (1) test_faddp_8h_13_14_15(TyH);
   if (1) test_faddp_8h_14_15_16(TyH);
   if (1) test_faddp_8h_15_16_17(TyH);
   if (1) test_faddp_8h_16_17_18(TyH);
   if (1) test_faddp_8h_17_18_19(TyH);
   if (1) test_faddp_8h_18_19_20(TyH);
   if (1) test_faddp_8h_19_20_21(TyH);
   if (1) test_faddp_8h_20_21_22(TyH);
   if (1) test_faddp_8h_21_22_23(TyH);
   if (1) test_faddp_8h_22_23_24(TyH);
   if (1) test_faddp_8h_23_24_25(TyH);
   if (1) test_faddp_8h_24_25_26(TyH);
   if (1) test_faddp_8h_25_26_27(TyH);
   if (1) test_faddp_8h_26_27_28(TyH);
   if (1) test_faddp_8h_27_28_29(TyH);
   if (1) test_faddp_8h_28_29_30(TyH);
   if (1) test_faddp_8h_29_30_31(TyH);

   if (1) test_faddp_4h_00_01_02(TyH);
   if (1) test_faddp_4h_01_02_03(TyH);
   if (1) test_faddp_4h_02_03_04(TyH);
   if (1) test_faddp_4h_03_04_05(TyH);
   if (1) test_faddp_4h_04_05_06(TyH);
   if (1) test_faddp_4h_05_06_07(TyH);
   if (1) test_faddp_4h_06_07_08(TyH);
   if (1) test_faddp_4h_07_08_09(TyH);
   if (1) test_faddp_4h_08_09_10(TyH);
   if (1) test_faddp_4h_09_10_11(TyH);
   if (1) test_faddp_4h_10_11_12(TyH);
   if (1) test_faddp_4h_11_12_13(TyH);
   if (1) test_faddp_4h_12_13_14(TyH);
   if (1) test_faddp_4h_13_14_15(TyH);
   if (1) test_faddp_4h_14_15_16(TyH);
   if (1) test_faddp_4h_15_16_17(TyH);
   if (1) test_faddp_4h_16_17_18(TyH);
   if (1) test_faddp_4h_17_18_19(TyH);
   if (1) test_faddp_4h_18_19_20(TyH);
   if (1) test_faddp_4h_19_20_21(TyH);
   if (1) test_faddp_4h_20_21_22(TyH);
   if (1) test_faddp_4h_21_22_23(TyH);
   if (1) test_faddp_4h_22_23_24(TyH);
   if (1) test_faddp_4h_23_24_25(TyH);
   if (1) test_faddp_4h_24_25_26(TyH);
   if (1) test_faddp_4h_25_26_27(TyH);
   if (1) test_faddp_4h_26_27_28(TyH);
   if (1) test_faddp_4h_27_28_29(TyH);
   if (1) test_faddp_4h_28_29_30(TyH);
   if (1) test_faddp_4h_29_30_31(TyH);

   printf("\nFABS <Hd>, <Hn>\n\n");

   if (1) test_fabs_h_00_01(TyH);
   if (1) test_fabs_h_01_02(TyH);
   if (1) test_fabs_h_02_03(TyH);
   if (1) test_fabs_h_03_04(TyH);
   if (1) test_fabs_h_04_05(TyH);
   if (1) test_fabs_h_05_06(TyH);
   if (1) test_fabs_h_06_07(TyH);
   if (1) test_fabs_h_07_08(TyH);
   if (1) test_fabs_h_08_09(TyH);
   if (1) test_fabs_h_09_10(TyH);
   if (1) test_fabs_h_10_11(TyH);
   if (1) test_fabs_h_11_12(TyH);
   if (1) test_fabs_h_12_13(TyH);
   if (1) test_fabs_h_13_14(TyH);
   if (1) test_fabs_h_14_15(TyH);
   if (1) test_fabs_h_15_16(TyH);
   if (1) test_fabs_h_16_17(TyH);
   if (1) test_fabs_h_17_18(TyH);
   if (1) test_fabs_h_18_19(TyH);
   if (1) test_fabs_h_19_20(TyH);
   if (1) test_fabs_h_20_21(TyH);
   if (1) test_fabs_h_21_22(TyH);
   if (1) test_fabs_h_22_23(TyH);
   if (1) test_fabs_h_23_24(TyH);
   if (1) test_fabs_h_24_25(TyH);
   if (1) test_fabs_h_25_26(TyH);
   if (1) test_fabs_h_26_27(TyH);
   if (1) test_fabs_h_27_28(TyH);
   if (1) test_fabs_h_28_29(TyH);
   if (1) test_fabs_h_29_30(TyH);
   if (1) test_fabs_h_30_31(TyH);

   printf("\nFABS <Vd>.<T>, <Vn>.<T>\n\n");

   if (1) test_fabs_8h_00_01(TyH);
   if (1) test_fabs_8h_01_02(TyH);
   if (1) test_fabs_8h_02_03(TyH);
   if (1) test_fabs_8h_03_04(TyH);
   if (1) test_fabs_8h_04_05(TyH);
   if (1) test_fabs_8h_05_06(TyH);
   if (1) test_fabs_8h_06_07(TyH);
   if (1) test_fabs_8h_07_08(TyH);
   if (1) test_fabs_8h_08_09(TyH);
   if (1) test_fabs_8h_09_10(TyH);
   if (1) test_fabs_8h_10_11(TyH);
   if (1) test_fabs_8h_11_12(TyH);
   if (1) test_fabs_8h_12_13(TyH);
   if (1) test_fabs_8h_13_14(TyH);
   if (1) test_fabs_8h_14_15(TyH);
   if (1) test_fabs_8h_15_16(TyH);
   if (1) test_fabs_8h_16_17(TyH);
   if (1) test_fabs_8h_17_18(TyH);
   if (1) test_fabs_8h_18_19(TyH);
   if (1) test_fabs_8h_19_20(TyH);
   if (1) test_fabs_8h_20_21(TyH);
   if (1) test_fabs_8h_21_22(TyH);
   if (1) test_fabs_8h_22_23(TyH);
   if (1) test_fabs_8h_23_24(TyH);
   if (1) test_fabs_8h_24_25(TyH);
   if (1) test_fabs_8h_25_26(TyH);
   if (1) test_fabs_8h_26_27(TyH);
   if (1) test_fabs_8h_27_28(TyH);
   if (1) test_fabs_8h_28_29(TyH);
   if (1) test_fabs_8h_29_30(TyH);
   if (1) test_fabs_8h_30_31(TyH);

   if (1) test_fabs_4h_00_01(TyH);
   if (1) test_fabs_4h_01_02(TyH);
   if (1) test_fabs_4h_02_03(TyH);
   if (1) test_fabs_4h_03_04(TyH);
   if (1) test_fabs_4h_04_05(TyH);
   if (1) test_fabs_4h_05_06(TyH);
   if (1) test_fabs_4h_06_07(TyH);
   if (1) test_fabs_4h_07_08(TyH);
   if (1) test_fabs_4h_08_09(TyH);
   if (1) test_fabs_4h_09_10(TyH);
   if (1) test_fabs_4h_10_11(TyH);
   if (1) test_fabs_4h_11_12(TyH);
   if (1) test_fabs_4h_12_13(TyH);
   if (1) test_fabs_4h_13_14(TyH);
   if (1) test_fabs_4h_14_15(TyH);
   if (1) test_fabs_4h_15_16(TyH);
   if (1) test_fabs_4h_16_17(TyH);
   if (1) test_fabs_4h_17_18(TyH);
   if (1) test_fabs_4h_18_19(TyH);
   if (1) test_fabs_4h_19_20(TyH);
   if (1) test_fabs_4h_20_21(TyH);
   if (1) test_fabs_4h_21_22(TyH);
   if (1) test_fabs_4h_22_23(TyH);
   if (1) test_fabs_4h_23_24(TyH);
   if (1) test_fabs_4h_24_25(TyH);
   if (1) test_fabs_4h_25_26(TyH);
   if (1) test_fabs_4h_26_27(TyH);
   if (1) test_fabs_4h_27_28(TyH);
   if (1) test_fabs_4h_28_29(TyH);
   if (1) test_fabs_4h_29_30(TyH);
   if (1) test_fabs_4h_30_31(TyH);

   printf("\nFNEG <Hd>, <Hn>\n\n");

   if (1) test_fneg_h_00_01(TyH);
   if (1) test_fneg_h_01_02(TyH);
   if (1) test_fneg_h_02_03(TyH);
   if (1) test_fneg_h_03_04(TyH);
   if (1) test_fneg_h_04_05(TyH);
   if (1) test_fneg_h_05_06(TyH);
   if (1) test_fneg_h_06_07(TyH);
   if (1) test_fneg_h_07_08(TyH);
   if (1) test_fneg_h_08_09(TyH);
   if (1) test_fneg_h_09_10(TyH);
   if (1) test_fneg_h_10_11(TyH);
   if (1) test_fneg_h_11_12(TyH);
   if (1) test_fneg_h_12_13(TyH);
   if (1) test_fneg_h_13_14(TyH);
   if (1) test_fneg_h_14_15(TyH);
   if (1) test_fneg_h_15_16(TyH);
   if (1) test_fneg_h_16_17(TyH);
   if (1) test_fneg_h_17_18(TyH);
   if (1) test_fneg_h_18_19(TyH);
   if (1) test_fneg_h_19_20(TyH);
   if (1) test_fneg_h_20_21(TyH);
   if (1) test_fneg_h_21_22(TyH);
   if (1) test_fneg_h_22_23(TyH);
   if (1) test_fneg_h_23_24(TyH);
   if (1) test_fneg_h_24_25(TyH);
   if (1) test_fneg_h_25_26(TyH);
   if (1) test_fneg_h_26_27(TyH);
   if (1) test_fneg_h_27_28(TyH);
   if (1) test_fneg_h_28_29(TyH);
   if (1) test_fneg_h_29_30(TyH);
   if (1) test_fneg_h_30_31(TyH);

   printf("\nFNEG <Vd>.<T>, <Vn>.<T>\n\n");

   if (1) test_fneg_8h_00_01(TyH);
   if (1) test_fneg_8h_01_02(TyH);
   if (1) test_fneg_8h_02_03(TyH);
   if (1) test_fneg_8h_03_04(TyH);
   if (1) test_fneg_8h_04_05(TyH);
   if (1) test_fneg_8h_05_06(TyH);
   if (1) test_fneg_8h_06_07(TyH);
   if (1) test_fneg_8h_07_08(TyH);
   if (1) test_fneg_8h_08_09(TyH);
   if (1) test_fneg_8h_09_10(TyH);
   if (1) test_fneg_8h_10_11(TyH);
   if (1) test_fneg_8h_11_12(TyH);
   if (1) test_fneg_8h_12_13(TyH);
   if (1) test_fneg_8h_13_14(TyH);
   if (1) test_fneg_8h_14_15(TyH);
   if (1) test_fneg_8h_15_16(TyH);
   if (1) test_fneg_8h_16_17(TyH);
   if (1) test_fneg_8h_17_18(TyH);
   if (1) test_fneg_8h_18_19(TyH);
   if (1) test_fneg_8h_19_20(TyH);
   if (1) test_fneg_8h_20_21(TyH);
   if (1) test_fneg_8h_21_22(TyH);
   if (1) test_fneg_8h_22_23(TyH);
   if (1) test_fneg_8h_23_24(TyH);
   if (1) test_fneg_8h_24_25(TyH);
   if (1) test_fneg_8h_25_26(TyH);
   if (1) test_fneg_8h_26_27(TyH);
   if (1) test_fneg_8h_27_28(TyH);
   if (1) test_fneg_8h_28_29(TyH);
   if (1) test_fneg_8h_29_30(TyH);
   if (1) test_fneg_8h_30_31(TyH);

   if (1) test_fneg_4h_00_01(TyH);
   if (1) test_fneg_4h_01_02(TyH);
   if (1) test_fneg_4h_02_03(TyH);
   if (1) test_fneg_4h_03_04(TyH);
   if (1) test_fneg_4h_04_05(TyH);
   if (1) test_fneg_4h_05_06(TyH);
   if (1) test_fneg_4h_06_07(TyH);
   if (1) test_fneg_4h_07_08(TyH);
   if (1) test_fneg_4h_08_09(TyH);
   if (1) test_fneg_4h_09_10(TyH);
   if (1) test_fneg_4h_10_11(TyH);
   if (1) test_fneg_4h_11_12(TyH);
   if (1) test_fneg_4h_12_13(TyH);
   if (1) test_fneg_4h_13_14(TyH);
   if (1) test_fneg_4h_14_15(TyH);
   if (1) test_fneg_4h_15_16(TyH);
   if (1) test_fneg_4h_16_17(TyH);
   if (1) test_fneg_4h_17_18(TyH);
   if (1) test_fneg_4h_18_19(TyH);
   if (1) test_fneg_4h_19_20(TyH);
   if (1) test_fneg_4h_20_21(TyH);
   if (1) test_fneg_4h_21_22(TyH);
   if (1) test_fneg_4h_22_23(TyH);
   if (1) test_fneg_4h_23_24(TyH);
   if (1) test_fneg_4h_24_25(TyH);
   if (1) test_fneg_4h_25_26(TyH);
   if (1) test_fneg_4h_26_27(TyH);
   if (1) test_fneg_4h_27_28(TyH);
   if (1) test_fneg_4h_28_29(TyH);
   if (1) test_fneg_4h_29_30(TyH);
   if (1) test_fneg_4h_30_31(TyH);

   printf("\nFSQRT <Hd>, <Hn>\n\n");

   if (1) test_fsqrt_h_00_01(TyH);
   if (1) test_fsqrt_h_01_02(TyH);
   if (1) test_fsqrt_h_02_03(TyH);
   if (1) test_fsqrt_h_03_04(TyH);
   if (1) test_fsqrt_h_04_05(TyH);
   if (1) test_fsqrt_h_05_06(TyH);
   if (1) test_fsqrt_h_06_07(TyH);
   if (1) test_fsqrt_h_07_08(TyH);
   if (1) test_fsqrt_h_08_09(TyH);
   if (1) test_fsqrt_h_09_10(TyH);
   if (1) test_fsqrt_h_10_11(TyH);
   if (1) test_fsqrt_h_11_12(TyH);
   if (1) test_fsqrt_h_12_13(TyH);
   if (1) test_fsqrt_h_13_14(TyH);
   if (1) test_fsqrt_h_14_15(TyH);
   if (1) test_fsqrt_h_15_16(TyH);
   if (1) test_fsqrt_h_16_17(TyH);
   if (1) test_fsqrt_h_17_18(TyH);
   if (1) test_fsqrt_h_18_19(TyH);
   if (1) test_fsqrt_h_19_20(TyH);
   if (1) test_fsqrt_h_20_21(TyH);
   if (1) test_fsqrt_h_21_22(TyH);
   if (1) test_fsqrt_h_22_23(TyH);
   if (1) test_fsqrt_h_23_24(TyH);
   if (1) test_fsqrt_h_24_25(TyH);
   if (1) test_fsqrt_h_25_26(TyH);
   if (1) test_fsqrt_h_26_27(TyH);
   if (1) test_fsqrt_h_27_28(TyH);
   if (1) test_fsqrt_h_28_29(TyH);
   if (1) test_fsqrt_h_29_30(TyH);
   if (1) test_fsqrt_h_30_31(TyH);

   printf("\nFSQRT <Vd>.<T>, <Vn>.<T>\n\n");

   if (1) test_fsqrt_8h_00_01(TyH);
   if (1) test_fsqrt_8h_01_02(TyH);
   if (1) test_fsqrt_8h_02_03(TyH);
   if (1) test_fsqrt_8h_03_04(TyH);
   if (1) test_fsqrt_8h_04_05(TyH);
   if (1) test_fsqrt_8h_05_06(TyH);
   if (1) test_fsqrt_8h_06_07(TyH);
   if (1) test_fsqrt_8h_07_08(TyH);
   if (1) test_fsqrt_8h_08_09(TyH);
   if (1) test_fsqrt_8h_09_10(TyH);
   if (1) test_fsqrt_8h_10_11(TyH);
   if (1) test_fsqrt_8h_11_12(TyH);
   if (1) test_fsqrt_8h_12_13(TyH);
   if (1) test_fsqrt_8h_13_14(TyH);
   if (1) test_fsqrt_8h_14_15(TyH);
   if (1) test_fsqrt_8h_15_16(TyH);
   if (1) test_fsqrt_8h_16_17(TyH);
   if (1) test_fsqrt_8h_17_18(TyH);
   if (1) test_fsqrt_8h_18_19(TyH);
   if (1) test_fsqrt_8h_19_20(TyH);
   if (1) test_fsqrt_8h_20_21(TyH);
   if (1) test_fsqrt_8h_21_22(TyH);
   if (1) test_fsqrt_8h_22_23(TyH);
   if (1) test_fsqrt_8h_23_24(TyH);
   if (1) test_fsqrt_8h_24_25(TyH);
   if (1) test_fsqrt_8h_25_26(TyH);
   if (1) test_fsqrt_8h_26_27(TyH);
   if (1) test_fsqrt_8h_27_28(TyH);
   if (1) test_fsqrt_8h_28_29(TyH);
   if (1) test_fsqrt_8h_29_30(TyH);
   if (1) test_fsqrt_8h_30_31(TyH);

   if (1) test_fsqrt_4h_00_01(TyH);
   if (1) test_fsqrt_4h_01_02(TyH);
   if (1) test_fsqrt_4h_02_03(TyH);
   if (1) test_fsqrt_4h_03_04(TyH);
   if (1) test_fsqrt_4h_04_05(TyH);
   if (1) test_fsqrt_4h_05_06(TyH);
   if (1) test_fsqrt_4h_06_07(TyH);
   if (1) test_fsqrt_4h_07_08(TyH);
   if (1) test_fsqrt_4h_08_09(TyH);
   if (1) test_fsqrt_4h_09_10(TyH);
   if (1) test_fsqrt_4h_10_11(TyH);
   if (1) test_fsqrt_4h_11_12(TyH);
   if (1) test_fsqrt_4h_12_13(TyH);
   if (1) test_fsqrt_4h_13_14(TyH);
   if (1) test_fsqrt_4h_14_15(TyH);
   if (1) test_fsqrt_4h_15_16(TyH);
   if (1) test_fsqrt_4h_16_17(TyH);
   if (1) test_fsqrt_4h_17_18(TyH);
   if (1) test_fsqrt_4h_18_19(TyH);
   if (1) test_fsqrt_4h_19_20(TyH);
   if (1) test_fsqrt_4h_20_21(TyH);
   if (1) test_fsqrt_4h_21_22(TyH);
   if (1) test_fsqrt_4h_22_23(TyH);
   if (1) test_fsqrt_4h_23_24(TyH);
   if (1) test_fsqrt_4h_24_25(TyH);
   if (1) test_fsqrt_4h_25_26(TyH);
   if (1) test_fsqrt_4h_26_27(TyH);
   if (1) test_fsqrt_4h_27_28(TyH);
   if (1) test_fsqrt_4h_28_29(TyH);
   if (1) test_fsqrt_4h_29_30(TyH);
   if (1) test_fsqrt_4h_30_31(TyH);

   printf("\nFABD <Hd>, <Hn>, <Hm>\n\n");

   if (1) test_fabd_h_00_01_02(TyH);
   if (1) test_fabd_h_01_02_03(TyH);
   if (1) test_fabd_h_02_03_04(TyH);
   if (1) test_fabd_h_03_04_05(TyH);
   if (1) test_fabd_h_04_05_06(TyH);
   if (1) test_fabd_h_05_06_07(TyH);
   if (1) test_fabd_h_06_07_08(TyH);
   if (1) test_fabd_h_07_08_09(TyH);
   if (1) test_fabd_h_08_09_10(TyH);
   if (1) test_fabd_h_09_10_11(TyH);
   if (1) test_fabd_h_10_11_12(TyH);
   if (1) test_fabd_h_11_12_13(TyH);
   if (1) test_fabd_h_12_13_14(TyH);
   if (1) test_fabd_h_13_14_15(TyH);
   if (1) test_fabd_h_14_15_16(TyH);
   if (1) test_fabd_h_15_16_17(TyH);
   if (1) test_fabd_h_16_17_18(TyH);
   if (1) test_fabd_h_17_18_19(TyH);
   if (1) test_fabd_h_18_19_20(TyH);
   if (1) test_fabd_h_19_20_21(TyH);
   if (1) test_fabd_h_20_21_22(TyH);
   if (1) test_fabd_h_21_22_23(TyH);
   if (1) test_fabd_h_22_23_24(TyH);
   if (1) test_fabd_h_23_24_25(TyH);
   if (1) test_fabd_h_24_25_26(TyH);
   if (1) test_fabd_h_25_26_27(TyH);
   if (1) test_fabd_h_26_27_28(TyH);
   if (1) test_fabd_h_27_28_29(TyH);
   if (1) test_fabd_h_28_29_30(TyH);
   if (1) test_fabd_h_29_30_31(TyH);

   printf("\nFABD <Vd>.<T>, <Vn>.<T>, <Vm>.<T>\n\n");

   if (1) test_fabd_8h_00_01_02(TyH);
   if (1) test_fabd_8h_01_02_03(TyH);
   if (1) test_fabd_8h_02_03_04(TyH);
   if (1) test_fabd_8h_03_04_05(TyH);
   if (1) test_fabd_8h_04_05_06(TyH);
   if (1) test_fabd_8h_05_06_07(TyH);
   if (1) test_fabd_8h_06_07_08(TyH);
   if (1) test_fabd_8h_07_08_09(TyH);
   if (1) test_fabd_8h_08_09_10(TyH);
   if (1) test_fabd_8h_09_10_11(TyH);
   if (1) test_fabd_8h_10_11_12(TyH);
   if (1) test_fabd_8h_11_12_13(TyH);
   if (1) test_fabd_8h_12_13_14(TyH);
   if (1) test_fabd_8h_13_14_15(TyH);
   if (1) test_fabd_8h_14_15_16(TyH);
   if (1) test_fabd_8h_15_16_17(TyH);
   if (1) test_fabd_8h_16_17_18(TyH);
   if (1) test_fabd_8h_17_18_19(TyH);
   if (1) test_fabd_8h_18_19_20(TyH);
   if (1) test_fabd_8h_19_20_21(TyH);
   if (1) test_fabd_8h_20_21_22(TyH);
   if (1) test_fabd_8h_21_22_23(TyH);
   if (1) test_fabd_8h_22_23_24(TyH);
   if (1) test_fabd_8h_23_24_25(TyH);
   if (1) test_fabd_8h_24_25_26(TyH);
   if (1) test_fabd_8h_25_26_27(TyH);
   if (1) test_fabd_8h_26_27_28(TyH);
   if (1) test_fabd_8h_27_28_29(TyH);
   if (1) test_fabd_8h_28_29_30(TyH);
   if (1) test_fabd_8h_29_30_31(TyH);

   if (1) test_fabd_4h_00_01_02(TyH);
   if (1) test_fabd_4h_01_02_03(TyH);
   if (1) test_fabd_4h_02_03_04(TyH);
   if (1) test_fabd_4h_03_04_05(TyH);
   if (1) test_fabd_4h_04_05_06(TyH);
   if (1) test_fabd_4h_05_06_07(TyH);
   if (1) test_fabd_4h_06_07_08(TyH);
   if (1) test_fabd_4h_07_08_09(TyH);
   if (1) test_fabd_4h_08_09_10(TyH);
   if (1) test_fabd_4h_09_10_11(TyH);
   if (1) test_fabd_4h_10_11_12(TyH);
   if (1) test_fabd_4h_11_12_13(TyH);
   if (1) test_fabd_4h_12_13_14(TyH);
   if (1) test_fabd_4h_13_14_15(TyH);
   if (1) test_fabd_4h_14_15_16(TyH);
   if (1) test_fabd_4h_15_16_17(TyH);
   if (1) test_fabd_4h_16_17_18(TyH);
   if (1) test_fabd_4h_17_18_19(TyH);
   if (1) test_fabd_4h_18_19_20(TyH);
   if (1) test_fabd_4h_19_20_21(TyH);
   if (1) test_fabd_4h_20_21_22(TyH);
   if (1) test_fabd_4h_21_22_23(TyH);
   if (1) test_fabd_4h_22_23_24(TyH);
   if (1) test_fabd_4h_23_24_25(TyH);
   if (1) test_fabd_4h_24_25_26(TyH);
   if (1) test_fabd_4h_25_26_27(TyH);
   if (1) test_fabd_4h_26_27_28(TyH);
   if (1) test_fabd_4h_27_28_29(TyH);
   if (1) test_fabd_4h_28_29_30(TyH);
   if (1) test_fabd_4h_29_30_31(TyH);

   printf("\nFACGT <Hd>, <Hn>, <Hm>\n\n");

   if (1) test_facgt_h_00_01_02(TyH);
   if (1) test_facgt_h_01_02_03(TyH);
   if (1) test_facgt_h_02_03_04(TyH);
   if (1) test_facgt_h_03_04_05(TyH);
   if (1) test_facgt_h_04_05_06(TyH);
   if (1) test_facgt_h_05_06_07(TyH);
   if (1) test_facgt_h_06_07_08(TyH);
   if (1) test_facgt_h_07_08_09(TyH);
   if (1) test_facgt_h_08_09_10(TyH);
   if (1) test_facgt_h_09_10_11(TyH);
   if (1) test_facgt_h_10_11_12(TyH);
   if (1) test_facgt_h_11_12_13(TyH);
   if (1) test_facgt_h_12_13_14(TyH);
   if (1) test_facgt_h_13_14_15(TyH);
   if (1) test_facgt_h_14_15_16(TyH);
   if (1) test_facgt_h_15_16_17(TyH);
   if (1) test_facgt_h_16_17_18(TyH);
   if (1) test_facgt_h_17_18_19(TyH);
   if (1) test_facgt_h_18_19_20(TyH);
   if (1) test_facgt_h_19_20_21(TyH);
   if (1) test_facgt_h_20_21_22(TyH);
   if (1) test_facgt_h_21_22_23(TyH);
   if (1) test_facgt_h_22_23_24(TyH);
   if (1) test_facgt_h_23_24_25(TyH);
   if (1) test_facgt_h_24_25_26(TyH);
   if (1) test_facgt_h_25_26_27(TyH);
   if (1) test_facgt_h_26_27_28(TyH);
   if (1) test_facgt_h_27_28_29(TyH);
   if (1) test_facgt_h_28_29_30(TyH);
   if (1) test_facgt_h_29_30_31(TyH);

   printf("\nFACGT <Vd>.<T>, <Vn>.<T>, <Vm>.<T>\n\n");

   if (1) test_facgt_8h_00_01_02(TyH);
   if (1) test_facgt_8h_01_02_03(TyH);
   if (1) test_facgt_8h_02_03_04(TyH);
   if (1) test_facgt_8h_03_04_05(TyH);
   if (1) test_facgt_8h_04_05_06(TyH);
   if (1) test_facgt_8h_05_06_07(TyH);
   if (1) test_facgt_8h_06_07_08(TyH);
   if (1) test_facgt_8h_07_08_09(TyH);
   if (1) test_facgt_8h_08_09_10(TyH);
   if (1) test_facgt_8h_09_10_11(TyH);
   if (1) test_facgt_8h_10_11_12(TyH);
   if (1) test_facgt_8h_11_12_13(TyH);
   if (1) test_facgt_8h_12_13_14(TyH);
   if (1) test_facgt_8h_13_14_15(TyH);
   if (1) test_facgt_8h_14_15_16(TyH);
   if (1) test_facgt_8h_15_16_17(TyH);
   if (1) test_facgt_8h_16_17_18(TyH);
   if (1) test_facgt_8h_17_18_19(TyH);
   if (1) test_facgt_8h_18_19_20(TyH);
   if (1) test_facgt_8h_19_20_21(TyH);
   if (1) test_facgt_8h_20_21_22(TyH);
   if (1) test_facgt_8h_21_22_23(TyH);
   if (1) test_facgt_8h_22_23_24(TyH);
   if (1) test_facgt_8h_23_24_25(TyH);
   if (1) test_facgt_8h_24_25_26(TyH);
   if (1) test_facgt_8h_25_26_27(TyH);
   if (1) test_facgt_8h_26_27_28(TyH);
   if (1) test_facgt_8h_27_28_29(TyH);
   if (1) test_facgt_8h_28_29_30(TyH);
   if (1) test_facgt_8h_29_30_31(TyH);

   if (1) test_facgt_4h_00_01_02(TyH);
   if (1) test_facgt_4h_01_02_03(TyH);
   if (1) test_facgt_4h_02_03_04(TyH);
   if (1) test_facgt_4h_03_04_05(TyH);
   if (1) test_facgt_4h_04_05_06(TyH);
   if (1) test_facgt_4h_05_06_07(TyH);
   if (1) test_facgt_4h_06_07_08(TyH);
   if (1) test_facgt_4h_07_08_09(TyH);
   if (1) test_facgt_4h_08_09_10(TyH);
   if (1) test_facgt_4h_09_10_11(TyH);
   if (1) test_facgt_4h_10_11_12(TyH);
   if (1) test_facgt_4h_11_12_13(TyH);
   if (1) test_facgt_4h_12_13_14(TyH);
   if (1) test_facgt_4h_13_14_15(TyH);
   if (1) test_facgt_4h_14_15_16(TyH);
   if (1) test_facgt_4h_15_16_17(TyH);
   if (1) test_facgt_4h_16_17_18(TyH);
   if (1) test_facgt_4h_17_18_19(TyH);
   if (1) test_facgt_4h_18_19_20(TyH);
   if (1) test_facgt_4h_19_20_21(TyH);
   if (1) test_facgt_4h_20_21_22(TyH);
   if (1) test_facgt_4h_21_22_23(TyH);
   if (1) test_facgt_4h_22_23_24(TyH);
   if (1) test_facgt_4h_23_24_25(TyH);
   if (1) test_facgt_4h_24_25_26(TyH);
   if (1) test_facgt_4h_25_26_27(TyH);
   if (1) test_facgt_4h_26_27_28(TyH);
   if (1) test_facgt_4h_27_28_29(TyH);
   if (1) test_facgt_4h_28_29_30(TyH);
   if (1) test_facgt_4h_29_30_31(TyH);

   printf("\nFACGE <Hd>, <Hn>, <Hm>\n\n");

   if (1) test_facge_h_00_01_02(TyH);
   if (1) test_facge_h_01_02_03(TyH);
   if (1) test_facge_h_02_03_04(TyH);
   if (1) test_facge_h_03_04_05(TyH);
   if (1) test_facge_h_04_05_06(TyH);
   if (1) test_facge_h_05_06_07(TyH);
   if (1) test_facge_h_06_07_08(TyH);
   if (1) test_facge_h_07_08_09(TyH);
   if (1) test_facge_h_08_09_10(TyH);
   if (1) test_facge_h_09_10_11(TyH);
   if (1) test_facge_h_10_11_12(TyH);
   if (1) test_facge_h_11_12_13(TyH);
   if (1) test_facge_h_12_13_14(TyH);
   if (1) test_facge_h_13_14_15(TyH);
   if (1) test_facge_h_14_15_16(TyH);
   if (1) test_facge_h_15_16_17(TyH);
   if (1) test_facge_h_16_17_18(TyH);
   if (1) test_facge_h_17_18_19(TyH);
   if (1) test_facge_h_18_19_20(TyH);
   if (1) test_facge_h_19_20_21(TyH);
   if (1) test_facge_h_20_21_22(TyH);
   if (1) test_facge_h_21_22_23(TyH);
   if (1) test_facge_h_22_23_24(TyH);
   if (1) test_facge_h_23_24_25(TyH);
   if (1) test_facge_h_24_25_26(TyH);
   if (1) test_facge_h_25_26_27(TyH);
   if (1) test_facge_h_26_27_28(TyH);
   if (1) test_facge_h_27_28_29(TyH);
   if (1) test_facge_h_28_29_30(TyH);
   if (1) test_facge_h_29_30_31(TyH);

   printf("\nFACGE <Vd>.<T>, <Vn>.<T>, <Vm>.<T>\n\n");

   if (1) test_facge_8h_00_01_02(TyH);
   if (1) test_facge_8h_01_02_03(TyH);
   if (1) test_facge_8h_02_03_04(TyH);
   if (1) test_facge_8h_03_04_05(TyH);
   if (1) test_facge_8h_04_05_06(TyH);
   if (1) test_facge_8h_05_06_07(TyH);
   if (1) test_facge_8h_06_07_08(TyH);
   if (1) test_facge_8h_07_08_09(TyH);
   if (1) test_facge_8h_08_09_10(TyH);
   if (1) test_facge_8h_09_10_11(TyH);
   if (1) test_facge_8h_10_11_12(TyH);
   if (1) test_facge_8h_11_12_13(TyH);
   if (1) test_facge_8h_12_13_14(TyH);
   if (1) test_facge_8h_13_14_15(TyH);
   if (1) test_facge_8h_14_15_16(TyH);
   if (1) test_facge_8h_15_16_17(TyH);
   if (1) test_facge_8h_16_17_18(TyH);
   if (1) test_facge_8h_17_18_19(TyH);
   if (1) test_facge_8h_18_19_20(TyH);
   if (1) test_facge_8h_19_20_21(TyH);
   if (1) test_facge_8h_20_21_22(TyH);
   if (1) test_facge_8h_21_22_23(TyH);
   if (1) test_facge_8h_22_23_24(TyH);
   if (1) test_facge_8h_23_24_25(TyH);
   if (1) test_facge_8h_24_25_26(TyH);
   if (1) test_facge_8h_25_26_27(TyH);
   if (1) test_facge_8h_26_27_28(TyH);
   if (1) test_facge_8h_27_28_29(TyH);
   if (1) test_facge_8h_28_29_30(TyH);
   if (1) test_facge_8h_29_30_31(TyH);

   if (1) test_facge_4h_00_01_02(TyH);
   if (1) test_facge_4h_01_02_03(TyH);
   if (1) test_facge_4h_02_03_04(TyH);
   if (1) test_facge_4h_03_04_05(TyH);
   if (1) test_facge_4h_04_05_06(TyH);
   if (1) test_facge_4h_05_06_07(TyH);
   if (1) test_facge_4h_06_07_08(TyH);
   if (1) test_facge_4h_07_08_09(TyH);
   if (1) test_facge_4h_08_09_10(TyH);
   if (1) test_facge_4h_09_10_11(TyH);
   if (1) test_facge_4h_10_11_12(TyH);
   if (1) test_facge_4h_11_12_13(TyH);
   if (1) test_facge_4h_12_13_14(TyH);
   if (1) test_facge_4h_13_14_15(TyH);
   if (1) test_facge_4h_14_15_16(TyH);
   if (1) test_facge_4h_15_16_17(TyH);
   if (1) test_facge_4h_16_17_18(TyH);
   if (1) test_facge_4h_17_18_19(TyH);
   if (1) test_facge_4h_18_19_20(TyH);
   if (1) test_facge_4h_19_20_21(TyH);
   if (1) test_facge_4h_20_21_22(TyH);
   if (1) test_facge_4h_21_22_23(TyH);
   if (1) test_facge_4h_22_23_24(TyH);
   if (1) test_facge_4h_23_24_25(TyH);
   if (1) test_facge_4h_24_25_26(TyH);
   if (1) test_facge_4h_25_26_27(TyH);
   if (1) test_facge_4h_26_27_28(TyH);
   if (1) test_facge_4h_27_28_29(TyH);
   if (1) test_facge_4h_28_29_30(TyH);
   if (1) test_facge_4h_29_30_31(TyH);

   return 0;
}

/* ---------------------------------------------------------------- */
/* -- List of instructions tested in order of execution.         -- */
/* -- Useful strings when searching for blocks of test cases.    -- */
/* ---------------------------------------------------------------- */
/*
   FADD <Hd>, <Hn>, <Hm> Floating-point Add (scalar).
   FADD <Vd>.<T>, <Vn>.<T>, <Vm>.<T> Floating-point Add (vector).
   FADDP <V><d>, <Vn>.<T> Floating-point Add Pair of elements (scalar).
   FADDP <Vd>.<T>, <Vn>.<T>, <Vm>.<T> Floating-point Add Pairwise (vector).
   FABS <Hd>, <Hn> Floating-point Absolute value (scalar).
   FABS <Vd>.<T>, <Vn>.<T> Floating-point Absolute value (vector).
   FNEG <Hd>, <Hn> Floating-point Negate (scalar).
   FNEG <Vd>.<T>, <Vn>.<T> Floating-point Negate (vector).
   FSQRT <Hd>, <Hn> Floating-point Square Root (scalar).
   FSQRT <Vd>.<T>, <Vn>.<T> Floating-point Square Root (vector).
   FABD <Hd>, <Hn>, <Hm> Floating-point Absolute Difference (scalar).
   FABD <Vd>.<T>, <Vn>.<T>, <Vm>.<T> Floating-point Absolute Difference (vector).
   FACGT <Hd>, <Hn>, <Hm> Floating-point Absolute Compare Greater than (scalar).
   FACGT <Vd>.<T>, <Vn>.<T>, <Vm>.<T> Floating-point Absolute Compare Greater than (vector).
   FACGE <Hd>, <Hn>, <Hm> Floating-point Absolute Compare Greater than or Equal (scalar).
   FACGE <Vd>.<T>, <Vn>.<T>, <Vm>.<T> Floating-point Absolute Compare Greater than or Equal (vector).
*/
