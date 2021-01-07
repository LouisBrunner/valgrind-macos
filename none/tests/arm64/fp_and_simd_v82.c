
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
/* ---------------------------------------------------------------- */

// ======================== FP ========================

GEN_TWOVEC_TEST(faddp_h_2h_00_00,    "faddp h0, v0.2h",    0, 0)
GEN_TWOVEC_TEST(faddp_h_2h_01_01,    "faddp h1, v1.2h",    1, 1)
GEN_TWOVEC_TEST(faddp_h_2h_02_02,    "faddp h2, v2.2h",    2, 2)
GEN_TWOVEC_TEST(faddp_h_2h_03_03,    "faddp h3, v3.2h",    3, 3)
GEN_TWOVEC_TEST(faddp_h_2h_04_04,    "faddp h4, v4.2h",    4, 4)
GEN_TWOVEC_TEST(faddp_h_2h_05_05,    "faddp h5, v5.2h",    5, 5)
GEN_TWOVEC_TEST(faddp_h_2h_06_06,    "faddp h6, v6.2h",    6, 6)
GEN_TWOVEC_TEST(faddp_h_2h_07_07,    "faddp h7, v7.2h",    7, 7)
GEN_TWOVEC_TEST(faddp_h_2h_08_08,    "faddp h8, v8.2h",    8, 8)
GEN_TWOVEC_TEST(faddp_h_2h_09_09,    "faddp h9, v9.2h",    9, 9)
GEN_TWOVEC_TEST(faddp_h_2h_10_10,    "faddp h10, v10.2h",    10, 10)
GEN_TWOVEC_TEST(faddp_h_2h_11_11,    "faddp h11, v11.2h",    11, 11)
GEN_TWOVEC_TEST(faddp_h_2h_12_12,    "faddp h12, v12.2h",    12, 12)
GEN_TWOVEC_TEST(faddp_h_2h_13_13,    "faddp h13, v13.2h",    13, 13)
GEN_TWOVEC_TEST(faddp_h_2h_14_14,    "faddp h14, v14.2h",    14, 14)
GEN_TWOVEC_TEST(faddp_h_2h_15_15,    "faddp h15, v15.2h",    15, 15)
GEN_TWOVEC_TEST(faddp_h_2h_16_16,    "faddp h16, v16.2h",    16, 16)
GEN_TWOVEC_TEST(faddp_h_2h_17_17,    "faddp h17, v17.2h",    17, 17)
GEN_TWOVEC_TEST(faddp_h_2h_18_18,    "faddp h18, v18.2h",    18, 18)
GEN_TWOVEC_TEST(faddp_h_2h_19_19,    "faddp h19, v19.2h",    19, 19)
GEN_TWOVEC_TEST(faddp_h_2h_20_20,    "faddp h20, v20.2h",    20, 20)
GEN_TWOVEC_TEST(faddp_h_2h_21_21,    "faddp h21, v21.2h",    21, 21)
GEN_TWOVEC_TEST(faddp_h_2h_22_22,    "faddp h22, v22.2h",    22, 22)
GEN_TWOVEC_TEST(faddp_h_2h_23_23,    "faddp h23, v23.2h",    23, 23)
GEN_TWOVEC_TEST(faddp_h_2h_24_24,    "faddp h24, v24.2h",    24, 24)
GEN_TWOVEC_TEST(faddp_h_2h_25_25,    "faddp h25, v25.2h",    25, 25)
GEN_TWOVEC_TEST(faddp_h_2h_26_26,    "faddp h26, v26.2h",    26, 26)
GEN_TWOVEC_TEST(faddp_h_2h_27_27,    "faddp h27, v27.2h",    27, 27)
GEN_TWOVEC_TEST(faddp_h_2h_28_28,    "faddp h28, v28.2h",    28, 28)
GEN_TWOVEC_TEST(faddp_h_2h_29_29,    "faddp h29, v29.2h",    29, 29)
GEN_TWOVEC_TEST(faddp_h_2h_30_30,    "faddp h30, v30.2h",    30, 30)
GEN_TWOVEC_TEST(faddp_h_2h_31_31,    "faddp h31, v31.2h",    31, 31)

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


/* ---------------------------------------------------------------- */
/* -- main()                                                     -- */
/* ---------------------------------------------------------------- */

int main ( void )
{
   assert(sizeof(V128) == 16);

   // ======================== FP ========================

   // faddp     h (half-precision floating add pair)
   // faddp     2h
   if (1) test_faddp_h_2h_00_00(TyH);
   if (1) test_faddp_h_2h_01_01(TyH);
   if (1) test_faddp_h_2h_02_02(TyH);
   if (1) test_faddp_h_2h_03_03(TyH);
   if (1) test_faddp_h_2h_04_04(TyH);
   if (1) test_faddp_h_2h_05_05(TyH);
   if (1) test_faddp_h_2h_06_06(TyH);
   if (1) test_faddp_h_2h_07_07(TyH);
   if (1) test_faddp_h_2h_08_08(TyH);
   if (1) test_faddp_h_2h_09_09(TyH);
   if (1) test_faddp_h_2h_10_10(TyH);
   if (1) test_faddp_h_2h_11_11(TyH);
   if (1) test_faddp_h_2h_12_12(TyH);
   if (1) test_faddp_h_2h_13_13(TyH);
   if (1) test_faddp_h_2h_14_14(TyH);
   if (1) test_faddp_h_2h_15_15(TyH);
   if (1) test_faddp_h_2h_16_16(TyH);
   if (1) test_faddp_h_2h_17_17(TyH);
   if (1) test_faddp_h_2h_18_18(TyH);
   if (1) test_faddp_h_2h_19_19(TyH);
   if (1) test_faddp_h_2h_20_20(TyH);
   if (1) test_faddp_h_2h_21_21(TyH);
   if (1) test_faddp_h_2h_22_22(TyH);
   if (1) test_faddp_h_2h_23_23(TyH);
   if (1) test_faddp_h_2h_24_24(TyH);
   if (1) test_faddp_h_2h_25_25(TyH);
   if (1) test_faddp_h_2h_26_26(TyH);
   if (1) test_faddp_h_2h_27_27(TyH);
   if (1) test_faddp_h_2h_28_28(TyH);
   if (1) test_faddp_h_2h_29_29(TyH);
   if (1) test_faddp_h_2h_30_30(TyH);
   if (1) test_faddp_h_2h_31_31(TyH);

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

   return 0;
}

/* ---------------------------------------------------------------- */
/* -- Alphabetical list of insns                                 -- */
/* ---------------------------------------------------------------- */
/*
   faddp     h (half-precision floating add pair)
   faddp     2h
*/
