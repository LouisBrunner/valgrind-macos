
#include <stdio.h>
#include <assert.h>
#include <malloc.h>  // memalign
#include <string.h>  // memset
#include <math.h>    // isnormal

typedef  unsigned char           UChar;
typedef  unsigned short int      UShort;
typedef  unsigned int            UInt;
typedef  signed int              Int;
typedef  unsigned char           UChar;
typedef  unsigned long long int  ULong;

typedef  unsigned char           Bool;
#define False ((Bool)0)
#define True  ((Bool)1)


#define ITERS 1

typedef
  enum { TySF=1234, TyDF, TyB, TyH, TyS, TyD, TyNONE }
  LaneTy;

union _V128 {
   UChar  u8[16];
   UShort u16[8];
   UInt   u32[4];
   ULong  u64[2];
   float  f32[4];
   double f64[2];
};
typedef  union _V128   V128;

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

/* Generates a random V128.  Ensures that that it contains normalised
   FP numbers when viewed as either F32x4 or F64x2, so that it is
   reasonable to use in FP test cases. */
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
          && isnormal(v->f32[3]) && isnormal(v->f64[0]) && isnormal(v->f64[1]))
        break;
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

__attribute__((unused))
static void* memalign16(size_t szB)
{
   void* x;
   x = memalign(16, szB);
   assert(x);
   assert(0 == ((16-1) & (unsigned long)x));
   return x;
}


/* ---------------------------------------------------------------- */
/* -- Test functions                                             -- */
/* ---------------------------------------------------------------- */

/* Note this also sets the destination register to a known value (0x55..55)
   since it can sometimes be an input to the instruction too. */
#define GEN_UNARY_TEST(INSN,SUFFIXD,SUFFIXN) \
  __attribute__((noinline)) \
  static void test_##INSN##_##SUFFIXD##_##SUFFIXN ( LaneTy ty ) { \
     Int i; \
     for (i = 0; i < ITERS; i++) { \
        V128 block[2]; \
        memset(block, 0x55, sizeof(block)); \
        randV128(&block[0], ty); \
        randV128(&block[1], ty); \
        __asm__ __volatile__( \
           "ldr   q7, [%0, #0]   ; " \
           "ldr   q8, [%0, #16]   ; " \
           #INSN " v8." #SUFFIXD ", v7." #SUFFIXN " ; " \
           "str   q8, [%0, #16] " \
           : : "r"(&block[0]) : "memory", "v7", "v8" \
        ); \
        printf(#INSN   " v8." #SUFFIXD ", v7." #SUFFIXN); \
        showV128(&block[0]); printf("  "); \
        showV128(&block[1]); printf("\n"); \
     } \
  }


/* Note this also sets the destination register to a known value (0x55..55)
   since it can sometimes be an input to the instruction too. */
#define GEN_BINARY_TEST(INSN,SUFFIXD,SUFFIXN,SUFFIXM)  \
  __attribute__((noinline)) \
  static void test_##INSN##_##SUFFIXD##_##SUFFIXN##_##SUFFIXM ( LaneTy ty ) { \
     Int i; \
     for (i = 0; i < ITERS; i++) { \
        V128 block[3]; \
        memset(block, 0x55, sizeof(block)); \
        randV128(&block[0], ty); \
        randV128(&block[1], ty); \
        randV128(&block[2], ty); \
        __asm__ __volatile__( \
           "ldr   q7, [%0, #0]   ; " \
           "ldr   q8, [%0, #16]   ; " \
           "ldr   q9, [%0, #32]   ; " \
           #INSN " v9." #SUFFIXD ", v7." #SUFFIXN ", v8." #SUFFIXM " ; " \
           "str   q9, [%0, #32] " \
           : : "r"(&block[0]) : "memory", "v7", "v8", "v9" \
        ); \
        printf(#INSN   " v9." #SUFFIXD \
               ", v7." #SUFFIXN ", v8." #SUFFIXM "  ");   \
        showV128(&block[0]); printf("  "); \
        showV128(&block[1]); printf("  "); \
        showV128(&block[2]); printf("\n"); \
     } \
  }


/* Note this also sets the destination register to a known value (0x55..55)
   since it can sometimes be an input to the instruction too. */
#define GEN_SHIFT_TEST(INSN,SUFFIXD,SUFFIXN,AMOUNT) \
  __attribute__((noinline)) \
  static void test_##INSN##_##SUFFIXD##_##SUFFIXN##_##AMOUNT ( LaneTy ty ) { \
     Int i; \
     for (i = 0; i < ITERS; i++) { \
        V128 block[2]; \
        memset(block, 0x55, sizeof(block)); \
        randV128(&block[0], ty); \
        randV128(&block[1], ty); \
        __asm__ __volatile__( \
           "ldr   q7, [%0, #0]   ; " \
           "ldr   q8, [%0, #16]   ; " \
           #INSN " v8." #SUFFIXD ", v7." #SUFFIXN ", #" #AMOUNT " ; " \
           "str   q8, [%0, #16] " \
           : : "r"(&block[0]) : "memory", "v7", "v8" \
        ); \
        printf(#INSN   " v8." #SUFFIXD ", v7." #SUFFIXN ", #" #AMOUNT "  "); \
        showV128(&block[0]); printf("  "); \
        showV128(&block[1]); printf("\n"); \
     } \
  }


/* Generate a test that involves one integer reg and one vector reg,
   with no bias as towards which is input or output. */
#define GEN_ONEINT_ONEVEC_TEST(TESTNAME,INSN,INTREGNO,VECREGNO) \
  __attribute__((noinline)) \
  static void test_##TESTNAME ( LaneTy ty ) { \
     Int i; \
     for (i = 0; i < ITERS; i++) { \
        V128 block[4]; \
        memset(block, 0x55, sizeof(block)); \
        randV128(&block[0], ty); \
        randV128(&block[1], ty); \
        randV128(&block[2], ty); \
        randV128(&block[3], ty); \
        __asm__ __volatile__( \
           "ldr   q"#VECREGNO", [%0, #0]  ; " \
           "ldr   x"#INTREGNO", [%0, #16] ; " \
           INSN " ; " \
           "str   q"#VECREGNO", [%0, #32] ; " \
           "str   x"#INTREGNO", [%0, #48] ; " \
           : : "r"(&block[0]) : "memory", "v"#VECREGNO, "x"#INTREGNO \
        ); \
        printf(INSN   "   "); \
        showV128(&block[0]); printf("  "); \
        showV128(&block[1]); printf("  "); \
        showV128(&block[2]); printf("  "); \
        showV128(&block[3]); printf("\n"); \
     } \
  }


/* Generate a test that involves two vector regs,
   with no bias as towards which is input or output. */
#define GEN_TWOVEC_TEST(TESTNAME,INSN,VECREG1NO,VECREG2NO) \
  __attribute__((noinline)) \
  static void test_##TESTNAME ( LaneTy ty ) { \
     Int i; \
     for (i = 0; i < ITERS; i++) { \
        V128 block[4]; \
        memset(block, 0x55, sizeof(block)); \
        randV128(&block[0], ty); \
        randV128(&block[1], ty); \
        randV128(&block[2], ty); \
        randV128(&block[3], ty); \
        __asm__ __volatile__( \
           "ldr   q"#VECREG1NO", [%0, #0]  ; " \
           "ldr   q"#VECREG2NO", [%0, #16] ; " \
           INSN " ; " \
           "str   q"#VECREG1NO", [%0, #32] ; " \
           "str   q"#VECREG2NO", [%0, #48] ; " \
           : : "r"(&block[0]) : "memory", "v"#VECREG1NO, "v"#VECREG2NO \
        ); \
        printf(INSN   "   "); \
        showV128(&block[0]); printf("  "); \
        showV128(&block[1]); printf("  "); \
        showV128(&block[2]); printf("  "); \
        showV128(&block[3]); printf("\n"); \
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
        V128 block[6]; \
        memset(block, 0x55, sizeof(block)); \
        randV128(&block[0], ty); \
        randV128(&block[1], ty); \
        randV128(&block[2], ty); \
        randV128(&block[3], ty); \
        randV128(&block[4], ty); \
        randV128(&block[5], ty); \
        __asm__ __volatile__( \
           "ldr   q"#VECREG1NO", [%0, #0]  ; " \
           "ldr   q"#VECREG2NO", [%0, #16] ; " \
           "ldr   q"#VECREG3NO", [%0, #32] ; " \
           INSN " ; " \
           "str   q"#VECREG1NO", [%0, #48] ; " \
           "str   q"#VECREG2NO", [%0, #64] ; " \
           "str   q"#VECREG3NO", [%0, #80] ; " \
           : : "r"(&block[0]) \
           : "memory", "v"#VECREG1NO, "v"#VECREG2NO, "v"#VECREG3NO, \
             "v16", "v17", "v18" \
        ); \
        printf(INSN   "   "); \
        showV128(&block[0]); printf("  "); \
        showV128(&block[1]); printf("  "); \
        showV128(&block[2]); printf("  "); \
        showV128(&block[3]); printf("  "); \
        showV128(&block[4]); printf("  "); \
        showV128(&block[5]); printf("\n"); \
     } \
  }


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


GEN_BINARY_TEST(umax, 4s, 4s, 4s)
GEN_BINARY_TEST(umax, 2s, 2s, 2s)
GEN_BINARY_TEST(umax, 8h, 8h, 8h)
GEN_BINARY_TEST(umax, 4h, 4h, 4h)
GEN_BINARY_TEST(umax, 16b, 16b, 16b)
GEN_BINARY_TEST(umax, 8b, 8b, 8b)

GEN_BINARY_TEST(umin, 4s, 4s, 4s)
GEN_BINARY_TEST(umin, 2s, 2s, 2s)
GEN_BINARY_TEST(umin, 8h, 8h, 8h)
GEN_BINARY_TEST(umin, 4h, 4h, 4h)
GEN_BINARY_TEST(umin, 16b, 16b, 16b)
GEN_BINARY_TEST(umin, 8b, 8b, 8b)

GEN_BINARY_TEST(smax, 4s, 4s, 4s)
GEN_BINARY_TEST(smax, 2s, 2s, 2s)
GEN_BINARY_TEST(smax, 8h, 8h, 8h)
GEN_BINARY_TEST(smax, 4h, 4h, 4h)
GEN_BINARY_TEST(smax, 16b, 16b, 16b)
GEN_BINARY_TEST(smax, 8b, 8b, 8b)

GEN_BINARY_TEST(smin, 4s, 4s, 4s)
GEN_BINARY_TEST(smin, 2s, 2s, 2s)
GEN_BINARY_TEST(smin, 8h, 8h, 8h)
GEN_BINARY_TEST(smin, 4h, 4h, 4h)
GEN_BINARY_TEST(smin, 16b, 16b, 16b)
GEN_BINARY_TEST(smin, 8b, 8b, 8b)

GEN_BINARY_TEST(add, 2d, 2d, 2d)
GEN_BINARY_TEST(add, 4s, 4s, 4s)
GEN_BINARY_TEST(add, 2s, 2s, 2s)
GEN_BINARY_TEST(add, 8h, 8h, 8h)
GEN_BINARY_TEST(add, 4h, 4h, 4h)
GEN_BINARY_TEST(add, 16b, 16b, 16b)
GEN_BINARY_TEST(add, 8b, 8b, 8b)

GEN_BINARY_TEST(sub, 2d, 2d, 2d)
GEN_BINARY_TEST(sub, 4s, 4s, 4s)
GEN_BINARY_TEST(sub, 2s, 2s, 2s)
GEN_BINARY_TEST(sub, 8h, 8h, 8h)
GEN_BINARY_TEST(sub, 4h, 4h, 4h)
GEN_BINARY_TEST(sub, 16b, 16b, 16b)
GEN_BINARY_TEST(sub, 8b, 8b, 8b)

GEN_BINARY_TEST(mul, 4s, 4s, 4s)
GEN_BINARY_TEST(mul, 2s, 2s, 2s)
GEN_BINARY_TEST(mul, 8h, 8h, 8h)
GEN_BINARY_TEST(mul, 4h, 4h, 4h)
GEN_BINARY_TEST(mul, 16b, 16b, 16b)
GEN_BINARY_TEST(mul, 8b, 8b, 8b)

GEN_BINARY_TEST(mla, 4s, 4s, 4s)
GEN_BINARY_TEST(mla, 2s, 2s, 2s)
GEN_BINARY_TEST(mla, 8h, 8h, 8h)
GEN_BINARY_TEST(mla, 4h, 4h, 4h)
GEN_BINARY_TEST(mla, 16b, 16b, 16b)
GEN_BINARY_TEST(mla, 8b, 8b, 8b)

GEN_BINARY_TEST(mls, 4s, 4s, 4s)
GEN_BINARY_TEST(mls, 2s, 2s, 2s)
GEN_BINARY_TEST(mls, 8h, 8h, 8h)
GEN_BINARY_TEST(mls, 4h, 4h, 4h)
GEN_BINARY_TEST(mls, 16b, 16b, 16b)
GEN_BINARY_TEST(mls, 8b, 8b, 8b)

GEN_BINARY_TEST(and, 16b, 16b, 16b)
GEN_BINARY_TEST(and, 8b, 8b, 8b)

GEN_BINARY_TEST(bic, 16b, 16b, 16b)
GEN_BINARY_TEST(bic, 8b, 8b, 8b)

GEN_BINARY_TEST(orr, 16b, 16b, 16b)
GEN_BINARY_TEST(orr, 8b, 8b, 8b)

GEN_BINARY_TEST(orn, 16b, 16b, 16b)
GEN_BINARY_TEST(orn, 8b, 8b, 8b)

GEN_BINARY_TEST(eor, 16b, 16b, 16b)
GEN_BINARY_TEST(eor, 8b, 8b, 8b)

GEN_BINARY_TEST(bsl, 16b, 16b, 16b)
GEN_BINARY_TEST(bsl, 8b, 8b, 8b)

GEN_BINARY_TEST(bit, 16b, 16b, 16b)
GEN_BINARY_TEST(bit, 8b, 8b, 8b)

GEN_BINARY_TEST(bif, 16b, 16b, 16b)
GEN_BINARY_TEST(bif, 8b, 8b, 8b)

GEN_BINARY_TEST(cmeq, 2d, 2d, 2d)
GEN_BINARY_TEST(cmeq, 4s, 4s, 4s)
GEN_BINARY_TEST(cmeq, 2s, 2s, 2s)
GEN_BINARY_TEST(cmeq, 8h, 8h, 8h)
GEN_BINARY_TEST(cmeq, 4h, 4h, 4h)
GEN_BINARY_TEST(cmeq, 16b, 16b, 16b)
GEN_BINARY_TEST(cmeq, 8b, 8b, 8b)

GEN_BINARY_TEST(cmtst, 2d, 2d, 2d)
GEN_BINARY_TEST(cmtst, 4s, 4s, 4s)
GEN_BINARY_TEST(cmtst, 2s, 2s, 2s)
GEN_BINARY_TEST(cmtst, 8h, 8h, 8h)
GEN_BINARY_TEST(cmtst, 4h, 4h, 4h)
GEN_BINARY_TEST(cmtst, 16b, 16b, 16b)
GEN_BINARY_TEST(cmtst, 8b, 8b, 8b)

GEN_BINARY_TEST(cmhi, 2d, 2d, 2d)
GEN_BINARY_TEST(cmhi, 4s, 4s, 4s)
GEN_BINARY_TEST(cmhi, 2s, 2s, 2s)
GEN_BINARY_TEST(cmhi, 8h, 8h, 8h)
GEN_BINARY_TEST(cmhi, 4h, 4h, 4h)
GEN_BINARY_TEST(cmhi, 16b, 16b, 16b)
GEN_BINARY_TEST(cmhi, 8b, 8b, 8b)

GEN_BINARY_TEST(cmgt, 2d, 2d, 2d)
GEN_BINARY_TEST(cmgt, 4s, 4s, 4s)
GEN_BINARY_TEST(cmgt, 2s, 2s, 2s)
GEN_BINARY_TEST(cmgt, 8h, 8h, 8h)
GEN_BINARY_TEST(cmgt, 4h, 4h, 4h)
GEN_BINARY_TEST(cmgt, 16b, 16b, 16b)
GEN_BINARY_TEST(cmgt, 8b, 8b, 8b)

GEN_BINARY_TEST(cmhs, 2d, 2d, 2d)
GEN_BINARY_TEST(cmhs, 4s, 4s, 4s)
GEN_BINARY_TEST(cmhs, 2s, 2s, 2s)
GEN_BINARY_TEST(cmhs, 8h, 8h, 8h)
GEN_BINARY_TEST(cmhs, 4h, 4h, 4h)
GEN_BINARY_TEST(cmhs, 16b, 16b, 16b)
GEN_BINARY_TEST(cmhs, 8b, 8b, 8b)

GEN_BINARY_TEST(cmge, 2d, 2d, 2d)
GEN_BINARY_TEST(cmge, 4s, 4s, 4s)
GEN_BINARY_TEST(cmge, 2s, 2s, 2s)
GEN_BINARY_TEST(cmge, 8h, 8h, 8h)
GEN_BINARY_TEST(cmge, 4h, 4h, 4h)
GEN_BINARY_TEST(cmge, 16b, 16b, 16b)
GEN_BINARY_TEST(cmge, 8b, 8b, 8b)

GEN_SHIFT_TEST(ushr, 2d, 2d, 1)
GEN_SHIFT_TEST(ushr, 2d, 2d, 13)
GEN_SHIFT_TEST(ushr, 2d, 2d, 63)
GEN_SHIFT_TEST(sshr, 2d, 2d, 1)
GEN_SHIFT_TEST(sshr, 2d, 2d, 13)
GEN_SHIFT_TEST(sshr, 2d, 2d, 63)
GEN_SHIFT_TEST(shl,  2d, 2d, 1)
GEN_SHIFT_TEST(shl,  2d, 2d, 13)
GEN_SHIFT_TEST(shl,  2d, 2d, 63)

GEN_SHIFT_TEST(ushr, 4s, 4s, 1)
GEN_SHIFT_TEST(ushr, 4s, 4s, 13)
GEN_SHIFT_TEST(ushr, 4s, 4s, 31)
GEN_SHIFT_TEST(sshr, 4s, 4s, 1)
GEN_SHIFT_TEST(sshr, 4s, 4s, 13)
GEN_SHIFT_TEST(sshr, 4s, 4s, 31)
GEN_SHIFT_TEST(shl,  4s, 4s, 1)
GEN_SHIFT_TEST(shl,  4s, 4s, 13)
GEN_SHIFT_TEST(shl,  4s, 4s, 31)

GEN_SHIFT_TEST(ushr, 2s, 2s, 1)
GEN_SHIFT_TEST(ushr, 2s, 2s, 13)
GEN_SHIFT_TEST(ushr, 2s, 2s, 31)
GEN_SHIFT_TEST(sshr, 2s, 2s, 1)
GEN_SHIFT_TEST(sshr, 2s, 2s, 13)
GEN_SHIFT_TEST(sshr, 2s, 2s, 31)
GEN_SHIFT_TEST(shl,  2s, 2s, 1)
GEN_SHIFT_TEST(shl,  2s, 2s, 13)
GEN_SHIFT_TEST(shl,  2s, 2s, 31)

GEN_SHIFT_TEST(ushr, 8h, 8h, 1)
GEN_SHIFT_TEST(ushr, 8h, 8h, 13)
GEN_SHIFT_TEST(ushr, 8h, 8h, 15)
GEN_SHIFT_TEST(sshr, 8h, 8h, 1)
GEN_SHIFT_TEST(sshr, 8h, 8h, 13)
GEN_SHIFT_TEST(sshr, 8h, 8h, 15)
GEN_SHIFT_TEST(shl,  8h, 8h, 1)
GEN_SHIFT_TEST(shl,  8h, 8h, 13)
GEN_SHIFT_TEST(shl,  8h, 8h, 15)

GEN_SHIFT_TEST(ushr, 4h, 4h, 1)
GEN_SHIFT_TEST(ushr, 4h, 4h, 13)
GEN_SHIFT_TEST(ushr, 4h, 4h, 15)
GEN_SHIFT_TEST(sshr, 4h, 4h, 1)
GEN_SHIFT_TEST(sshr, 4h, 4h, 13)
GEN_SHIFT_TEST(sshr, 4h, 4h, 15)
GEN_SHIFT_TEST(shl,  4h, 4h, 1)
GEN_SHIFT_TEST(shl,  4h, 4h, 13)
GEN_SHIFT_TEST(shl,  4h, 4h, 15)

GEN_SHIFT_TEST(ushr, 16b, 16b, 1)
GEN_SHIFT_TEST(ushr, 16b, 16b, 7)
GEN_SHIFT_TEST(sshr, 16b, 16b, 1)
GEN_SHIFT_TEST(sshr, 16b, 16b, 7)
GEN_SHIFT_TEST(shl,  16b, 16b, 1)
GEN_SHIFT_TEST(shl,  16b, 16b, 7)

GEN_SHIFT_TEST(ushr, 8b, 8b, 1)
GEN_SHIFT_TEST(ushr, 8b, 8b, 7)
GEN_SHIFT_TEST(sshr, 8b, 8b, 1)
GEN_SHIFT_TEST(sshr, 8b, 8b, 7)
GEN_SHIFT_TEST(shl,  8b, 8b, 1)
GEN_SHIFT_TEST(shl,  8b, 8b, 7)

GEN_SHIFT_TEST(ushll,  2d, 2s, 0)
GEN_SHIFT_TEST(ushll,  2d, 2s, 15)
GEN_SHIFT_TEST(ushll,  2d, 2s, 31)
GEN_SHIFT_TEST(ushll2, 2d, 4s, 0)
GEN_SHIFT_TEST(ushll2, 2d, 4s, 15)
GEN_SHIFT_TEST(ushll2, 2d, 4s, 31)

GEN_SHIFT_TEST(sshll,  2d, 2s, 0)
GEN_SHIFT_TEST(sshll,  2d, 2s, 15)
GEN_SHIFT_TEST(sshll,  2d, 2s, 31)
GEN_SHIFT_TEST(sshll2, 2d, 4s, 0)
GEN_SHIFT_TEST(sshll2, 2d, 4s, 15)
GEN_SHIFT_TEST(sshll2, 2d, 4s, 31)

GEN_UNARY_TEST(xtn,  2s, 2d)
GEN_UNARY_TEST(xtn2, 4s, 2d)
GEN_UNARY_TEST(xtn,  4h, 4s)
GEN_UNARY_TEST(xtn2, 8h, 4s)
GEN_UNARY_TEST(xtn,  8b, 8h)
GEN_UNARY_TEST(xtn2, 16b, 8h)

GEN_ONEINT_ONEVEC_TEST(umov_x_d0,  "umov x9, v10.d[0]", 9, 10)
GEN_ONEINT_ONEVEC_TEST(umov_x_d1,  "umov x9, v10.d[1]", 9, 10)
GEN_ONEINT_ONEVEC_TEST(umov_w_s0,  "umov w9, v10.s[0]", 9, 10)
GEN_ONEINT_ONEVEC_TEST(umov_w_s3,  "umov w9, v10.s[3]", 9, 10)
GEN_ONEINT_ONEVEC_TEST(umov_w_h0,  "umov w9, v10.h[0]", 9, 10)
GEN_ONEINT_ONEVEC_TEST(umov_w_h7,  "umov w9, v10.h[7]", 9, 10)
GEN_ONEINT_ONEVEC_TEST(umov_w_b0,  "umov w9, v10.b[0]", 9, 10)
GEN_ONEINT_ONEVEC_TEST(umov_w_b15, "umov w9, v10.b[15]", 9, 10)

GEN_ONEINT_ONEVEC_TEST(smov_x_s0,  "smov x9, v10.s[0]", 9, 10)
GEN_ONEINT_ONEVEC_TEST(smov_x_s3,  "smov x9, v10.s[3]", 9, 10)
GEN_ONEINT_ONEVEC_TEST(smov_x_h0,  "smov x9, v10.h[0]", 9, 10)
GEN_ONEINT_ONEVEC_TEST(smov_x_h7,  "smov x9, v10.h[7]", 9, 10)
GEN_ONEINT_ONEVEC_TEST(smov_w_h0,  "smov w9, v10.h[0]", 9, 10)
GEN_ONEINT_ONEVEC_TEST(smov_w_h7,  "smov w9, v10.h[7]", 9, 10)
GEN_ONEINT_ONEVEC_TEST(smov_x_b0,  "smov x9, v10.b[0]", 9, 10)
GEN_ONEINT_ONEVEC_TEST(smov_x_b15, "smov x9, v10.b[15]", 9, 10)
GEN_ONEINT_ONEVEC_TEST(smov_w_b0,  "smov w9, v10.b[0]", 9, 10)
GEN_ONEINT_ONEVEC_TEST(smov_w_b15, "smov w9, v10.b[15]", 9, 10)

GEN_TWOVEC_TEST(fcvtn_2s_2d, "fcvtn  v22.2s, v23.2d", 22, 23)
GEN_TWOVEC_TEST(fcvtn_4s_2d, "fcvtn2 v22.4s, v23.2d", 22, 23)

GEN_UNARY_TEST(neg, 2d, 2d)
GEN_UNARY_TEST(neg, 4s, 4s)
GEN_UNARY_TEST(neg, 2s, 2s)
GEN_UNARY_TEST(neg, 8h, 8h)
GEN_UNARY_TEST(neg, 4h, 4h)
GEN_UNARY_TEST(neg, 16b, 16b)
GEN_UNARY_TEST(neg, 8b,  8b)

GEN_BINARY_TEST(fadd, 2d, 2d, 2d)
GEN_BINARY_TEST(fadd, 4s, 4s, 4s)
GEN_BINARY_TEST(fadd, 2s, 2s, 2s)
GEN_BINARY_TEST(fsub, 2d, 2d, 2d)
GEN_BINARY_TEST(fsub, 4s, 4s, 4s)
GEN_BINARY_TEST(fsub, 2s, 2s, 2s)
GEN_BINARY_TEST(fmul, 2d, 2d, 2d)
GEN_BINARY_TEST(fmul, 4s, 4s, 4s)
GEN_BINARY_TEST(fmul, 2s, 2s, 2s)
GEN_BINARY_TEST(fdiv, 2d, 2d, 2d)
GEN_BINARY_TEST(fdiv, 4s, 4s, 4s)
GEN_BINARY_TEST(fdiv, 2s, 2s, 2s)
GEN_BINARY_TEST(fmla, 2d, 2d, 2d)
GEN_BINARY_TEST(fmla, 4s, 4s, 4s)
GEN_BINARY_TEST(fmla, 2s, 2s, 2s)
GEN_BINARY_TEST(fmls, 2d, 2d, 2d)
GEN_BINARY_TEST(fmls, 4s, 4s, 4s)
GEN_BINARY_TEST(fmls, 2s, 2s, 2s)
GEN_BINARY_TEST(fabd, 2d, 2d, 2d)
GEN_BINARY_TEST(fabd, 4s, 4s, 4s)
GEN_BINARY_TEST(fabd, 2s, 2s, 2s)

GEN_THREEVEC_TEST(add_d_d_d, "add d21, d22, d23", 21, 22, 23)
GEN_THREEVEC_TEST(sub_d_d_d, "sub d21, d22, d23", 21, 22, 23)

/* overkill -- don't need two vecs, only one */
GEN_TWOVEC_TEST(fmov_d_imm_01, "fmov d22, #0.125", 22, 23)
GEN_TWOVEC_TEST(fmov_d_imm_02, "fmov d22, #-4.0",  22, 23)
GEN_TWOVEC_TEST(fmov_d_imm_03, "fmov d22, #1.0",   22, 23)
GEN_TWOVEC_TEST(fmov_s_imm_01, "fmov s22, #0.125", 22, 23)
GEN_TWOVEC_TEST(fmov_s_imm_02, "fmov s22, #-4.0",  22, 23)
GEN_TWOVEC_TEST(fmov_s_imm_03, "fmov s22, #-1.0",   22, 23)

GEN_ONEINT_ONEVEC_TEST(fmov_s_w,  "fmov s7,      w15", 15, 7)
GEN_ONEINT_ONEVEC_TEST(fmov_d_x,  "fmov d7,      x15", 15, 7)
GEN_ONEINT_ONEVEC_TEST(fmov_d1_x, "fmov v7.d[1], x15", 15, 7)
GEN_ONEINT_ONEVEC_TEST(fmov_w_s,  "fmov w15,      s7", 15, 7)
GEN_ONEINT_ONEVEC_TEST(fmov_x_d,  "fmov x15,      d7", 15, 7)
GEN_ONEINT_ONEVEC_TEST(fmov_x_d1, "fmov x15, v7.d[1]", 15, 7)

GEN_TWOVEC_TEST(fmov_2d_imm_01, "fmov v22.2d, #0.125", 22, 23)
GEN_TWOVEC_TEST(fmov_2d_imm_02, "fmov v22.2d, #-4.0",  22, 23)
GEN_TWOVEC_TEST(fmov_2d_imm_03, "fmov v22.2d, #1.0",   22, 23)
GEN_TWOVEC_TEST(fmov_4s_imm_01, "fmov v22.4s, #0.125", 22, 23)
GEN_TWOVEC_TEST(fmov_4s_imm_02, "fmov v22.4s, #-4.0",  22, 23)
GEN_TWOVEC_TEST(fmov_4s_imm_03, "fmov v22.4s, #1.0",   22, 23)
GEN_TWOVEC_TEST(fmov_2s_imm_01, "fmov v22.2s, #0.125", 22, 23)
GEN_TWOVEC_TEST(fmov_2s_imm_02, "fmov v22.2s, #-4.0",  22, 23)
GEN_TWOVEC_TEST(fmov_2s_imm_03, "fmov v22.2s, #1.0",   22, 23)

GEN_ONEINT_ONEVEC_TEST(scvtf_s_w, "scvtf s7, w15", 15, 7)
GEN_ONEINT_ONEVEC_TEST(scvtf_d_w, "scvtf d7, w15", 15, 7)
GEN_ONEINT_ONEVEC_TEST(scvtf_s_x, "scvtf s7, x15", 15, 7)
GEN_ONEINT_ONEVEC_TEST(scvtf_d_x, "scvtf d7, x15", 15, 7)
GEN_ONEINT_ONEVEC_TEST(ucvtf_s_w, "ucvtf s7, w15", 15, 7)
GEN_ONEINT_ONEVEC_TEST(ucvtf_d_w, "ucvtf d7, w15", 15, 7)
GEN_ONEINT_ONEVEC_TEST(ucvtf_s_x, "ucvtf s7, x15", 15, 7)
GEN_ONEINT_ONEVEC_TEST(ucvtf_d_x, "ucvtf d7, x15", 15, 7)

GEN_THREEVEC_TEST(fadd_d_d_d,  "fadd d2, d11, d29", 2, 11, 29)
GEN_THREEVEC_TEST(fadd_s_s_s,  "fadd s2, s11, s29", 2, 11, 29)
GEN_THREEVEC_TEST(fsub_d_d_d,  "fsub d2, d11, d29", 2, 11, 29)
GEN_THREEVEC_TEST(fsub_s_s_s,  "fsub s2, s11, s29", 2, 11, 29)
GEN_THREEVEC_TEST(fmul_d_d_d,  "fmul d2, d11, d29", 2, 11, 29)
GEN_THREEVEC_TEST(fmul_s_s_s,  "fmul s2, s11, s29", 2, 11, 29)
GEN_THREEVEC_TEST(fdiv_d_d_d,  "fdiv d2, d11, d29", 2, 11, 29)
GEN_THREEVEC_TEST(fdiv_s_s_s,  "fdiv s2, s11, s29", 2, 11, 29)
GEN_THREEVEC_TEST(fnmul_d_d_d, "fnmul d2, d11, d29", 2, 11, 29)
GEN_THREEVEC_TEST(fnmul_s_s_s, "fnmul s2, s11, s29", 2, 11, 29)

GEN_THREEVEC_TEST(fabd_d_d_d,  "fabd d2, d11, d29", 2, 11, 29)
GEN_THREEVEC_TEST(fabd_s_s_s,  "fabd s2, s11, s29", 2, 11, 29)

GEN_TWOVEC_TEST(fmov_d_d,  "fmov d22, d23",   22, 23)
GEN_TWOVEC_TEST(fmov_s_s,  "fmov s22, s23",   22, 23)
GEN_TWOVEC_TEST(fabs_d_d,  "fabs d22, d23",   22, 23)
GEN_TWOVEC_TEST(fabs_s_s,  "fabs s22, s23",   22, 23)
GEN_TWOVEC_TEST(fneg_d_d,  "fneg d22, d23",   22, 23)
GEN_TWOVEC_TEST(fneg_s_s,  "fneg s22, s23",   22, 23)
GEN_TWOVEC_TEST(fsqrt_d_d, "fsqrt d22, d23",   22, 23)
GEN_TWOVEC_TEST(fsqrt_s_s, "fsqrt s22, s23",   22, 23)

GEN_UNARY_TEST(fneg, 2d, 2d)
GEN_UNARY_TEST(fneg, 4s, 4s)
GEN_UNARY_TEST(fneg, 2s, 2s)
GEN_UNARY_TEST(fabs, 2d, 2d)
GEN_UNARY_TEST(fabs, 4s, 4s)
GEN_UNARY_TEST(fabs, 2s, 2s)

GEN_BINARY_TEST(fcmeq, 2d, 2d, 2d)
GEN_BINARY_TEST(fcmeq, 4s, 4s, 4s)
GEN_BINARY_TEST(fcmeq, 2s, 2s, 2s)
GEN_BINARY_TEST(fcmge, 2d, 2d, 2d)
GEN_BINARY_TEST(fcmge, 4s, 4s, 4s)
GEN_BINARY_TEST(fcmge, 2s, 2s, 2s)
GEN_BINARY_TEST(fcmgt, 2d, 2d, 2d)
GEN_BINARY_TEST(fcmgt, 4s, 4s, 4s)
GEN_BINARY_TEST(fcmgt, 2s, 2s, 2s)
GEN_BINARY_TEST(facge, 2d, 2d, 2d)
GEN_BINARY_TEST(facge, 4s, 4s, 4s)
GEN_BINARY_TEST(facge, 2s, 2s, 2s)
GEN_BINARY_TEST(facgt, 2d, 2d, 2d)
GEN_BINARY_TEST(facgt, 4s, 4s, 4s)
GEN_BINARY_TEST(facgt, 2s, 2s, 2s)

// Uses v15 as the first table entry
GEN_THREEVEC_TEST(
   tbl_16b_1reg, "tbl v21.16b, {v15.16b}, v23.16b", 21, 15, 23)
// and v15 ^ v21 as the second table entry
GEN_THREEVEC_TEST(
   tbl_16b_2reg, "eor v16.16b, v15.16b, v21.16b ; "
                 "tbl v21.16b, {v15.16b, v16.16b}, v23.16b", 21, 15, 23)
// and v15 ^ v23 as the third table entry
GEN_THREEVEC_TEST(
   tbl_16b_3reg, "eor v16.16b, v15.16b, v21.16b ; "
                 "eor v17.16b, v15.16b, v23.16b ; "
                 "tbl v21.16b, {v15.16b, v16.16b, v17.16b}, v23.16b",
                 21, 15, 23)
// and v21 ^ v23 as the fourth table entry
GEN_THREEVEC_TEST(
   tbl_16b_4reg, "eor v16.16b, v15.16b, v21.16b ; "
                 "eor v17.16b, v15.16b, v23.16b ; "
                 "eor v18.16b, v21.16b, v23.16b ; "
                 "tbl v21.16b, {v15.16b, v16.16b, v17.16b, v18.16b}, v23.16b",
                 21, 15, 23)

// Same register scheme for tbl .8b, tbx .16b, tbx.8b
GEN_THREEVEC_TEST(
   tbl_8b_1reg, "tbl v21.8b, {v15.16b}, v23.8b", 21, 15, 23)
GEN_THREEVEC_TEST(
   tbl_8b_2reg, "eor v16.16b, v15.16b, v21.16b ; "
                "tbl v21.8b, {v15.16b, v16.16b}, v23.8b", 21, 15, 23)
GEN_THREEVEC_TEST(
   tbl_8b_3reg, "eor v16.16b, v15.16b, v21.16b ; "
                "eor v17.16b, v15.16b, v23.16b ; "
                "tbl v21.8b, {v15.16b, v16.16b, v17.16b}, v23.8b",
                21, 15, 23)
GEN_THREEVEC_TEST(
   tbl_8b_4reg, "eor v16.16b, v15.16b, v21.16b ; "
                "eor v17.16b, v15.16b, v23.16b ; "
                "eor v18.16b, v21.16b, v23.16b ; "
                "tbl v21.8b, {v15.16b, v16.16b, v17.16b, v18.16b}, v23.8b",
                21, 15, 23)

GEN_THREEVEC_TEST(
   tbx_16b_1reg, "tbx v21.16b, {v15.16b}, v23.16b", 21, 15, 23)
GEN_THREEVEC_TEST(
   tbx_16b_2reg, "eor v16.16b, v15.16b, v21.16b ; "
                 "tbx v21.16b, {v15.16b, v16.16b}, v23.16b", 21, 15, 23)
GEN_THREEVEC_TEST(
   tbx_16b_3reg, "eor v16.16b, v15.16b, v21.16b ; "
                 "eor v17.16b, v15.16b, v23.16b ; "
                 "tbx v21.16b, {v15.16b, v16.16b, v17.16b}, v23.16b",
                 21, 15, 23)
GEN_THREEVEC_TEST(
   tbx_16b_4reg, "eor v16.16b, v15.16b, v21.16b ; "
                 "eor v17.16b, v15.16b, v23.16b ; "
                 "eor v18.16b, v21.16b, v23.16b ; "
                 "tbx v21.16b, {v15.16b, v16.16b, v17.16b, v18.16b}, v23.16b",
                 21, 15, 23)

// Same register scheme for tbx .8b, tbx .16b, tbx.8b
GEN_THREEVEC_TEST(
   tbx_8b_1reg, "tbx v21.8b, {v15.16b}, v23.8b", 21, 15, 23)
GEN_THREEVEC_TEST(
   tbx_8b_2reg, "eor v16.16b, v15.16b, v21.16b ; "
                "tbx v21.8b, {v15.16b, v16.16b}, v23.8b", 21, 15, 23)
GEN_THREEVEC_TEST(
   tbx_8b_3reg, "eor v16.16b, v15.16b, v21.16b ; "
                "eor v17.16b, v15.16b, v23.16b ; "
                "tbx v21.8b, {v15.16b, v16.16b, v17.16b}, v23.8b",
                21, 15, 23)
GEN_THREEVEC_TEST(
   tbx_8b_4reg, "eor v16.16b, v15.16b, v21.16b ; "
                "eor v17.16b, v15.16b, v23.16b ; "
                "eor v18.16b, v21.16b, v23.16b ; "
                "tbx v21.8b, {v15.16b, v16.16b, v17.16b, v18.16b}, v23.8b",
                21, 15, 23)

GEN_TWOVEC_TEST(cmge_zero_2d_2d,   "cmge v5.2d,  v22.2d,  #0", 5, 22)
GEN_TWOVEC_TEST(cmge_zero_4s_4s,   "cmge v5.4s,  v22.4s,  #0", 5, 22)
GEN_TWOVEC_TEST(cmge_zero_2s_2s,   "cmge v5.2s,  v22.2s,  #0", 5, 22)
GEN_TWOVEC_TEST(cmge_zero_8h_8h,   "cmge v5.8h,  v22.8h,  #0", 5, 22)
GEN_TWOVEC_TEST(cmge_zero_4h_4h,   "cmge v5.4h,  v22.4h,  #0", 5, 22)
GEN_TWOVEC_TEST(cmge_zero_16b_16b, "cmge v5.16b, v22.16b, #0", 5, 22)
GEN_TWOVEC_TEST(cmge_zero_8b_8b,   "cmge v5.8b,  v22.8b,  #0", 5, 22)

GEN_TWOVEC_TEST(cmgt_zero_2d_2d,   "cmgt v5.2d,  v22.2d,  #0", 5, 22)
GEN_TWOVEC_TEST(cmgt_zero_4s_4s,   "cmgt v5.4s,  v22.4s,  #0", 5, 22)
GEN_TWOVEC_TEST(cmgt_zero_2s_2s,   "cmgt v5.2s,  v22.2s,  #0", 5, 22)
GEN_TWOVEC_TEST(cmgt_zero_8h_8h,   "cmgt v5.8h,  v22.8h,  #0", 5, 22)
GEN_TWOVEC_TEST(cmgt_zero_4h_4h,   "cmgt v5.4h,  v22.4h,  #0", 5, 22)
GEN_TWOVEC_TEST(cmgt_zero_16b_16b, "cmgt v5.16b, v22.16b, #0", 5, 22)
GEN_TWOVEC_TEST(cmgt_zero_8b_8b,   "cmgt v5.8b,  v22.8b,  #0", 5, 22)

GEN_TWOVEC_TEST(cmle_zero_2d_2d,   "cmle v5.2d,  v22.2d,  #0", 5, 22)
GEN_TWOVEC_TEST(cmle_zero_4s_4s,   "cmle v5.4s,  v22.4s,  #0", 5, 22)
GEN_TWOVEC_TEST(cmle_zero_2s_2s,   "cmle v5.2s,  v22.2s,  #0", 5, 22)
GEN_TWOVEC_TEST(cmle_zero_8h_8h,   "cmle v5.8h,  v22.8h,  #0", 5, 22)
GEN_TWOVEC_TEST(cmle_zero_4h_4h,   "cmle v5.4h,  v22.4h,  #0", 5, 22)
GEN_TWOVEC_TEST(cmle_zero_16b_16b, "cmle v5.16b, v22.16b, #0", 5, 22)
GEN_TWOVEC_TEST(cmle_zero_8b_8b,   "cmle v5.8b,  v22.8b,  #0", 5, 22)

GEN_TWOVEC_TEST(cmeq_zero_2d_2d,   "cmeq v5.2d,  v22.2d,  #0", 5, 22)
GEN_TWOVEC_TEST(cmeq_zero_4s_4s,   "cmeq v5.4s,  v22.4s,  #0", 5, 22)
GEN_TWOVEC_TEST(cmeq_zero_2s_2s,   "cmeq v5.2s,  v22.2s,  #0", 5, 22)
GEN_TWOVEC_TEST(cmeq_zero_8h_8h,   "cmeq v5.8h,  v22.8h,  #0", 5, 22)
GEN_TWOVEC_TEST(cmeq_zero_4h_4h,   "cmeq v5.4h,  v22.4h,  #0", 5, 22)
GEN_TWOVEC_TEST(cmeq_zero_16b_16b, "cmeq v5.16b, v22.16b, #0", 5, 22)
GEN_TWOVEC_TEST(cmeq_zero_8b_8b,   "cmeq v5.8b,  v22.8b,  #0", 5, 22)

GEN_TWOVEC_TEST(cmlt_zero_2d_2d,   "cmlt v5.2d,  v22.2d,  #0", 5, 22)
GEN_TWOVEC_TEST(cmlt_zero_4s_4s,   "cmlt v5.4s,  v22.4s,  #0", 5, 22)
GEN_TWOVEC_TEST(cmlt_zero_2s_2s,   "cmlt v5.2s,  v22.2s,  #0", 5, 22)
GEN_TWOVEC_TEST(cmlt_zero_8h_8h,   "cmlt v5.8h,  v22.8h,  #0", 5, 22)
GEN_TWOVEC_TEST(cmlt_zero_4h_4h,   "cmlt v5.4h,  v22.4h,  #0", 5, 22)
GEN_TWOVEC_TEST(cmlt_zero_16b_16b, "cmlt v5.16b, v22.16b, #0", 5, 22)
GEN_TWOVEC_TEST(cmlt_zero_8b_8b,   "cmlt v5.8b,  v22.8b,  #0", 5, 22)


/* ---------------------------------------------------------------- */
/* -- main()                                                     -- */
/* ---------------------------------------------------------------- */

int main ( void )
{
   assert(sizeof(V128) == 16);

   // ======================== FP ========================

   // fabs      d,s
   // fabs      2d,4s,2s
   test_fabs_d_d(TyDF);
   test_fabs_s_s(TySF);
   test_fabs_2d_2d(TyDF);
   test_fabs_4s_4s(TySF);
   test_fabs_2s_2s(TyDF);
   test_fneg_2d_2d(TySF);
   test_fneg_4s_4s(TyDF);
   test_fneg_2s_2s(TySF);

   // fneg      d,s
   // fneg      2d,4s,2s
   test_fneg_d_d(TyDF);
   test_fneg_s_s(TySF);

   // fsqrt     d,s
   // fsqrt     2d,4s,2s
   test_fsqrt_d_d(TyDF);
   test_fsqrt_s_s(TySF);

   // fadd      d,s
   // fsub      d,s
   test_fadd_d_d_d(TyDF);
   test_fadd_s_s_s(TySF);
   test_fsub_d_d_d(TyDF);
   test_fsub_s_s_s(TySF);

   // fadd      2d,4s,2s
   // fsub      2d,4s,2s
   test_fadd_2d_2d_2d(TyDF);
   test_fadd_4s_4s_4s(TySF);
   test_fadd_2s_2s_2s(TySF);
   test_fsub_2d_2d_2d(TyDF);
   test_fsub_4s_4s_4s(TySF);
   test_fsub_2s_2s_2s(TySF);

   // fabd      d,s
   // fabd      2d,4s,2s
   test_fabd_d_d_d(TyDF);
   test_fabd_s_s_s(TySF);
   test_fabd_2d_2d_2d(TyDF);
   test_fabd_4s_4s_4s(TySF);
   test_fabd_2s_2s_2s(TySF);

   // faddp     d,s (floating add pair)
   // faddp     2d,4s,2s

   // fccmp     d,s (floating point conditional quiet compare)
   // fccmpe    d,s (floating point conditional signaling compare)

   // fcmeq     d,s
   // fcmge     d,s
   // fcmgt     d,s
   // facgt     d,s  (floating abs compare GE)
   // facge     d,s  (floating abs compare GE)

   // fcmeq     2d,4s,2s
   // fcmge     2d,4s,2s
   // fcmgt     2d,4s,2s
   // facge     2d,4s,2s
   // facgt     2d,4s,2s
   test_fcmeq_2d_2d_2d(TyDF);
   test_fcmeq_4s_4s_4s(TySF);
   test_fcmeq_2s_2s_2s(TySF);
   test_fcmge_2d_2d_2d(TyDF);
   test_fcmge_4s_4s_4s(TySF);
   test_fcmge_2s_2s_2s(TySF);
   test_fcmgt_2d_2d_2d(TyDF);
   test_fcmgt_4s_4s_4s(TySF);
   test_fcmgt_2s_2s_2s(TySF);
   test_facge_2d_2d_2d(TyDF);
   test_facge_4s_4s_4s(TySF);
   test_facge_2s_2s_2s(TySF);
   test_facgt_2d_2d_2d(TyDF);
   test_facgt_4s_4s_4s(TySF);
   test_facgt_2s_2s_2s(TySF);

   // fcmeq_z   d,s
   // fcmge_z   d,s
   // fcmgt_z   d,s
   // fcmle_z   d,s
   // fcmlt_z   d,s

   // fcmeq_z   2d,4s,2s
   // fcmge_z   2d,4s,2s
   // fcmgt_z   2d,4s,2s
   // fcmle_z   2d,4s,2s
   // fcmlt_z   2d,4s,2s

   // fcmp_z    d,s
   // fcmpe_z   d,s
   // fcmp      d,s (floating point quiet, set flags)
   // fcmpe     d,s (floating point signaling, set flags)

   // fcsel     d,s (fp cond select)

   // fdiv      d,s
   // fdiv      2d,4s,2s
   test_fdiv_d_d_d(TyDF);
   test_fdiv_s_s_s(TySF);
   test_fdiv_2d_2d_2d(TyDF);
   test_fdiv_4s_4s_4s(TySF);
   test_fdiv_2s_2s_2s(TySF);

   // fmadd     d,s
   // fnmadd    d,s
   // fmsub     d,s
   // fnmsub    d,s

   // fnmul     d,s
   test_fnmul_d_d_d(TyDF);
   test_fnmul_s_s_s(TySF);

   // fmax      d,s
   // fmin      d,s
   // fmaxnm    d,s ("max number")
   // fminnm    d,s

   // fmax      2d,4s,2s
   // fmin      2d,4s,2s
   // fmaxnm    2d,4s,2s
   // fminnm    2d,4s,2s

   // fmaxnmp   d_2d,s_2s ("max number pairwise")
   // fminnmp   d_2d,s_2s

   // fmaxnmp   2d,4s,2s
   // fminnmp   2d,4s,2s

   // fmaxnmv   s_4s (maxnum across vector)
   // fminnmv   s_4s

   // fmaxp     d_2d,s_2s (max of a pair)
   // fminp     d_2d,s_2s (max of a pair)

   // fmaxp     2d,4s,2s  (max pairwise)
   // fminp     2d,4s,2s

   // fmaxv     s_4s (max across vector)
   // fminv     s_4s

   // fmla      2d,4s,2s
   // fmls      2d,4s,2s
   test_fmla_2d_2d_2d(TyDF);
   test_fmla_4s_4s_4s(TySF);
   test_fmla_2s_2s_2s(TySF);
   test_fmls_2d_2d_2d(TyDF);
   test_fmls_4s_4s_4s(TySF);
   test_fmls_2s_2s_2s(TySF);

   // fmla      d_d_d[],s_s_s[] (by element)
   // fmls      d_d_d[],s_s_s[] (by element)

   // fmla      2d_2d_d[],4s_4s_s[],2s_2s_s[]
   // fmls      2d_2d_d[],4s_4s_s[],2s_2s_s[]

   // fmov      2d,4s,2s #imm (part of the MOVI/MVNI/ORR/BIC imm group)
   // INCOMPLETE
   test_fmov_2d_imm_01(TyD);
   test_fmov_2d_imm_02(TyD);
   test_fmov_2d_imm_03(TyD);
   if (0) test_fmov_4s_imm_01(TyS);
   if (0) test_fmov_4s_imm_02(TyS);
   if (0) test_fmov_4s_imm_03(TyS);
   if (0) test_fmov_2s_imm_01(TyS);
   if (0) test_fmov_2s_imm_02(TyS);
   if (0) test_fmov_2s_imm_03(TyS);

   // fmov      d_d,s_s
   test_fmov_d_d(TyDF);
   test_fmov_s_s(TySF);

   // fmov      s_w,w_s,d_x,d[1]_x,x_d,x_d[1]
   test_fmov_s_w(TyS);
   test_fmov_d_x(TyD);
   test_fmov_d1_x(TyD);
   test_fmov_w_s(TyS);
   test_fmov_x_d(TyD);
   test_fmov_x_d1(TyD);

   // fmov      d,s #imm
   test_fmov_d_imm_01(TyNONE);
   test_fmov_d_imm_02(TyNONE);
   test_fmov_d_imm_03(TyNONE);
   test_fmov_s_imm_01(TyNONE);
   test_fmov_s_imm_02(TyNONE);
   test_fmov_s_imm_03(TyNONE);

   // fmul      d_d_d[],s_s_s[]
   // fmul      2d_2d_d[],4s_4s_s[],2s_2s_s[]

   // fmul      2d,4s,2s
   // fmul      d,s
   test_fmul_d_d_d(TyDF);
   test_fmul_s_s_s(TySF);
   test_fmul_2d_2d_2d(TyDF);
   test_fmul_4s_4s_4s(TySF);
   test_fmul_2s_2s_2s(TySF);

   // fmulx     d_d_d[],s_s_s[]
   // fmulx     2d_2d_d[],4s_4s_s[],2s_2s_s[]

   // fmulx     d,s
   // fmulx     2d,4s,2s

   // frecpe    d,s (recip estimate)
   // frecpe    2d,4s,2s

   // frecps    d,s (recip step)
   // frecps    2d,4s,2s

   // frecpx    d,s (recip exponent)

   // frinta    d,s
   // frinti    d,s
   // frintm    d,s
   // frintn    d,s
   // frintp    d,s
   // frintx    d,s
   // frintz    d,s

   // frinta    2d,4s,2s (round to integral, nearest away)
   // frinti    2d,4s,2s (round to integral, per FPCR)
   // frintm    2d,4s,2s (round to integral, minus inf)
   // frintn    2d,4s,2s (round to integral, nearest, to even)
   // frintp    2d,4s,2s (round to integral, plus inf)
   // frintx    2d,4s,2s (round to integral exact, per FPCR)
   // frintz    2d,4s,2s (round to integral, zero)

   // frsqrte   d,s (est)
   // frsqrte   2d,4s,2s

   // frsqrts   d,s (step)
   // frsqrts   2d,4s,2s

   // ======================== CONV ========================

   // fcvt      s_h,d_h,h_s,d_s,h_d,s_d (fp convert, scalar)

   // fcvtl{2}  4s/4h, 4s/8h, 2d/2s, 2d/4s (float convert to longer form)

   // fcvtn{2}  4h/4s, 8h/4s, 2s/2d, 4s/2d (float convert to narrower form)
   // INCOMPLETE
   test_fcvtn_2s_2d(TyDF);
   test_fcvtn_4s_2d(TyDF);

   // fcvtas    d,s  (fcvt to signed int,   nearest, ties away)
   // fcvtau    d,s  (fcvt to unsigned int, nearest, ties away)
   // fcvtas    2d,4s,2s
   // fcvtau    2d,4s,2s
   // fcvtas    w_s,x_s,w_d,x_d
   // fcvtau    w_s,x_s,w_d,x_d

   // fcvtms    d,s  (fcvt to signed int,   minus inf)
   // fcvtmu    d,s  (fcvt to unsigned int, minus inf)
   // fcvtms    2d,4s,2s
   // fcvtmu    2d,4s,2s
   // fcvtms    w_s,x_s,w_d,x_d
   // fcvtmu    w_s,x_s,w_d,x_d

   // fcvtns    d,s  (fcvt to signed int,   nearest)
   // fcvtnu    d,s  (fcvt to unsigned int, nearest)
   // fcvtns    2d,4s,2s
   // fcvtnu    2d,4s,2s
   // fcvtns    w_s,x_s,w_d,x_d
   // fcvtnu    w_s,x_s,w_d,x_d

   // fcvtps    d,s  (fcvt to signed int,   plus inf)
   // fcvtpu    d,s  (fcvt to unsigned int, plus inf)
   // fcvtps    2d,4s,2s
   // fcvtpu    2d,4s,2s
   // fcvtps    w_s,x_s,w_d,x_d
   // fcvtpu    w_s,x_s,w_d,x_d

   // fcvtzs    d,s (fcvt to signed integer,   to zero)
   // fcvtzu    d,s (fcvt to unsigned integer, to zero)
   // fcvtzs    2d,4s,2s
   // fcvtzu    2d,4s,2s
   // fcvtzs    w_s,x_s,w_d,x_d
   // fcvtzu    w_s,x_s,w_d,x_d

   // fcvtzs    d,s (fcvt to signed fixedpt,   to zero) (w/ #fbits)
   // fcvtzu    d,s (fcvt to unsigned fixedpt, to zero) (w/ #fbits)
   // fcvtzs    2d,4s,2s
   // fcvtzu    2d,4s,2s
   // fcvtzs    w_s,x_s,w_d,x_d (fcvt to signed fixedpt,   to zero) (w/ #fbits)
   // fcvtzu    w_s,x_s,w_d,x_d (fcvt to unsigned fixedpt, to zero) (w/ #fbits)

   // fcvtxn    s_d (fcvt to lower prec narrow, rounding to odd)
   // fcvtxn    2s_2d,4s_2d

   // scvtf     d,s        _#fbits
   // ucvtf     d,s        _#fbits

   // scvtf     2d,4s,2s   _#fbits
   // ucvtf     2d,4s,2s   _#fbits

   // scvtf     d,s
   // ucvtf     d,s

   // scvtf     2d,4s,2s
   // ucvtf     2d,4s,2s

   // scvtf     s_w, d_w, s_x, d_x,   _#fbits
   // ucvtf     s_w, d_w, s_x, d_x,   _#fbits

   // scvtf     s_w, d_w, s_x, d_x
   // ucvtf     s_w, d_w, s_x, d_x
   test_scvtf_s_w(TyS);
   test_scvtf_d_w(TyS);
   test_scvtf_s_x(TyD);
   test_scvtf_d_x(TyD);
   test_ucvtf_s_w(TyS);
   test_ucvtf_d_w(TyS);
   test_ucvtf_s_x(TyD);
   test_ucvtf_d_x(TyD);

   // ======================== INT ========================

   // abs       d
   // neg       d

   // abs       2d,4s,2s,8h,4h,16b,8b
   // neg       2d,4s,2s,8h,4h,16b,8b
   test_neg_2d_2d(TyD);
   test_neg_4s_4s(TyS);
   test_neg_2s_2s(TyS);
   test_neg_8h_8h(TyH);
   test_neg_4h_4h(TyH);
   test_neg_16b_16b(TyB);
   test_neg_8b_8b(TyB);

   // add       d
   // sub       d
   test_add_d_d_d(TyD);
   test_sub_d_d_d(TyD);

   // add       2d,4s,2s,8h,4h,16b,8b
   // sub       2d,4s,2s,8h,4h,16b,8b
   test_add_2d_2d_2d(TyD);
   test_add_4s_4s_4s(TyS);
   test_add_2s_2s_2s(TyS);
   test_add_8h_8h_8h(TyH);
   test_add_4h_4h_4h(TyH);
   test_add_16b_16b_16b(TyB);
   test_add_8b_8b_8b(TyB);
   test_sub_2d_2d_2d(TyD);
   test_sub_4s_4s_4s(TyS);
   test_sub_2s_2s_2s(TyS);
   test_sub_8h_8h_8h(TyH);
   test_sub_4h_4h_4h(TyH);
   test_sub_16b_16b_16b(TyB);
   test_sub_8b_8b_8b(TyB);

   // addhn{2}   2s/4s_2d_2d, 4h/8h_4s_4s, 8b/16b_8h_8h
   // subhn{2}   2s/4s_2d_2d, 4h/8h_4s_4s, 8b/16b_8h_8h
   // raddhn{2}  2s/4s_2d_2d, 4h/8h_4s_4s, 8b/16b_8h_8h
   // rsubhn{2}  2s/4s_2d_2d, 4h/8h_4s_4s, 8b/16b_8h_8h

   // addp     d (add pairs, across)
   // addp     2d,4s,2s,8h,4h,16b,8b
   // addv     4s,8h,4h,16b,18b (reduce across vector)

   // and      16b,8b
   // bic      16b,8b (vector,reg) (bit clear)
   // orn      16b,8b
   // orr      16b,8b
   test_and_16b_16b_16b(TyB);
   test_and_8b_8b_8b(TyB);
   test_bic_16b_16b_16b(TyB);
   test_bic_8b_8b_8b(TyB);
   test_orr_16b_16b_16b(TyB);
   test_orr_8b_8b_8b(TyB);
   test_orn_16b_16b_16b(TyB);
   test_orn_8b_8b_8b(TyB);

   // orr      8h,4h   #imm8, LSL #0 or 8
   // orr      4s,2s   #imm8, LSL #0, 8, 16 or 24
   // bic      8h,4h   #imm8, LSL #0 or 8
   // bic      4s,2s   #imm8, LSL #0, 8, 16 or 24
   // also movi, mvni

   // bif      16b,8b (vector) (bit insert if false)
   // bit      16b,8b (vector) (bit insert if true)
   // bsl      16b,8b (vector) (bit select)
   // eor      16b,8b (vector)
   test_bif_16b_16b_16b(TyB);
   test_bif_8b_8b_8b(TyB);
   test_bit_16b_16b_16b(TyB);
   test_bit_8b_8b_8b(TyB);
   test_bsl_16b_16b_16b(TyB);
   test_bsl_8b_8b_8b(TyB);
   test_eor_16b_16b_16b(TyB);
   test_eor_8b_8b_8b(TyB);

   // cls      4s,2s,8h,4h,16b,8b (count leading sign bits)
   // clz      4s,2s,8h,4h,16b,8b (count leading zero bits)

   // cmeq     d
   // cmge     d
   // cmgt     d
   // cmhi     d
   // cmhs     d
   // cmtst    d

   // cmeq     2d,4s,2s,8h,4h,16b,8b
   // cmge     2d,4s,2s,8h,4h,16b,8b
   // cmgt     2d,4s,2s,8h,4h,16b,8b
   // cmhi     2d,4s,2s,8h,4h,16b,8b
   // cmhs     2d,4s,2s,8h,4h,16b,8b
   // cmtst    2d,4s,2s,8h,4h,16b,8b
   test_cmeq_2d_2d_2d(TyD);
   test_cmeq_4s_4s_4s(TyS);
   test_cmeq_2s_2s_2s(TyS);
   test_cmeq_8h_8h_8h(TyH);
   test_cmeq_4h_4h_4h(TyH);
   test_cmeq_16b_16b_16b(TyB);
   test_cmeq_8b_8b_8b(TyB);
   test_cmge_2d_2d_2d(TyD);
   test_cmge_4s_4s_4s(TyS);
   test_cmge_2s_2s_2s(TyS);
   test_cmge_8h_8h_8h(TyH);
   test_cmge_4h_4h_4h(TyH);
   test_cmge_16b_16b_16b(TyB);
   test_cmge_8b_8b_8b(TyB);
   test_cmgt_2d_2d_2d(TyD);
   test_cmgt_4s_4s_4s(TyS);
   test_cmgt_2s_2s_2s(TyS);
   test_cmgt_8h_8h_8h(TyH);
   test_cmgt_4h_4h_4h(TyH);
   test_cmgt_16b_16b_16b(TyB);
   test_cmgt_8b_8b_8b(TyB);
   test_cmhi_2d_2d_2d(TyD);
   test_cmhi_4s_4s_4s(TyS);
   test_cmhi_2s_2s_2s(TyS);
   test_cmhi_8h_8h_8h(TyH);
   test_cmhi_4h_4h_4h(TyH);
   test_cmhi_16b_16b_16b(TyB);
   test_cmhi_8b_8b_8b(TyB);
   test_cmhs_2d_2d_2d(TyD);
   test_cmhs_4s_4s_4s(TyS);
   test_cmhs_2s_2s_2s(TyS);
   test_cmhs_8h_8h_8h(TyH);
   test_cmhs_4h_4h_4h(TyH);
   test_cmhs_16b_16b_16b(TyB);
   test_cmhs_8b_8b_8b(TyB);
   test_cmtst_2d_2d_2d(TyD);
   test_cmtst_4s_4s_4s(TyS);
   test_cmtst_2s_2s_2s(TyS);
   test_cmtst_8h_8h_8h(TyH);
   test_cmtst_4h_4h_4h(TyH);
   test_cmtst_16b_16b_16b(TyB);
   test_cmtst_8b_8b_8b(TyB);

   // cmeq_z   d
   // cmge_z   d
   // cmgt_z   d
   // cmle_z   d
   // cmlt_z   d

   // cmeq_z   2d,4s,2s,8h,4h,16b,8b
   // cmge_z   2d,4s,2s,8h,4h,16b,8b
   // cmgt_z   2d,4s,2s,8h,4h,16b,8b
   // cmle_z   2d,4s,2s,8h,4h,16b,8b
   // cmlt_z   2d,4s,2s,8h,4h,16b,8b
   test_cmeq_zero_2d_2d(TyD);
   test_cmeq_zero_4s_4s(TyS);
   test_cmeq_zero_2s_2s(TyS);
   test_cmeq_zero_8h_8h(TyH);
   test_cmeq_zero_4h_4h(TyH);
   test_cmeq_zero_16b_16b(TyB);
   test_cmeq_zero_8b_8b(TyB);
   test_cmge_zero_2d_2d(TyD);
   test_cmge_zero_4s_4s(TyS);
   test_cmge_zero_2s_2s(TyS);
   test_cmge_zero_8h_8h(TyH);
   test_cmge_zero_4h_4h(TyH);
   test_cmge_zero_16b_16b(TyB);
   test_cmge_zero_8b_8b(TyB);
   test_cmgt_zero_2d_2d(TyD);
   test_cmgt_zero_4s_4s(TyS);
   test_cmgt_zero_2s_2s(TyS);
   test_cmgt_zero_8h_8h(TyH);
   test_cmgt_zero_4h_4h(TyH);
   test_cmgt_zero_16b_16b(TyB);
   test_cmgt_zero_8b_8b(TyB);
   test_cmle_zero_2d_2d(TyD);
   test_cmle_zero_4s_4s(TyS);
   test_cmle_zero_2s_2s(TyS);
   test_cmle_zero_8h_8h(TyH);
   test_cmle_zero_4h_4h(TyH);
   test_cmle_zero_16b_16b(TyB);
   test_cmle_zero_8b_8b(TyB);
   test_cmlt_zero_2d_2d(TyD);
   test_cmlt_zero_4s_4s(TyS);
   test_cmlt_zero_2s_2s(TyS);
   test_cmlt_zero_8h_8h(TyH);
   test_cmlt_zero_4h_4h(TyH);
   test_cmlt_zero_16b_16b(TyB);
   test_cmlt_zero_8b_8b(TyB);

   // cnt      16b,8b (population count per byte)

   // dup      d,s,h,b (vec elem to scalar)
   // dup      2d,4s,2s,8h,4h,16b,8b (vec elem to vector)
   // dup      2d,4s,2s,8h,4h,16b,8b (general reg to vector)

   // ext      16b,8b,#imm4 (concat 2 vectors, then slice)

   // ins      d[]_d[],s[]_s[],h[]_h[],b[]_b[]

   // ins      d[]_x, s[]_w, h[]_w, b[]_w
   test_INS_general();

   // mla   4s_4s_s[],2s_2s_s[],8h_8h_h[],4h_4h_h[]
   // mls   4s_4s_s[],2s_2s_s[],8h_8h_h[],4h_4h_h[]

   // mla   4s,2s,8h,4h,16b,8b
   // mls   4s,2s,8h,4h,16b,8b
   test_mla_4s_4s_4s(TyS);
   test_mla_2s_2s_2s(TyS);
   test_mla_8h_8h_8h(TyH);
   test_mla_4h_4h_4h(TyH);
   test_mla_16b_16b_16b(TyB);
   test_mla_8b_8b_8b(TyB);
   test_mls_4s_4s_4s(TyS);
   test_mls_2s_2s_2s(TyS);
   test_mls_8h_8h_8h(TyH);
   test_mls_4h_4h_4h(TyH);
   test_mls_16b_16b_16b(TyB);
   test_mls_8b_8b_8b(TyB);

   // movi  16b,8b   #imm8, LSL #0
   // movi  8h,4h    #imm8, LSL #0 or 8
   // movi  4s,2s    #imm8, LSL #0, 8, 16, 24
   // movi  4s,2s    #imm8, MSL #8 or 16
   // movi  d,       #imm64
   // movi  2d,      #imm64

   // mul   4s_4s_s[],2s_2s_s[],8h_8h_h[],4h_4h_h[]

   // mul   4s,2s,8h,4h,16b,8b
   test_mul_4s_4s_4s(TyS);
   test_mul_2s_2s_2s(TyS);
   test_mul_8h_8h_8h(TyH);
   test_mul_4h_4h_4h(TyH);
   test_mul_16b_16b_16b(TyB);
   test_mul_8b_8b_8b(TyB);

   // mvni  8h,4h    #imm8, LSL #0 or 8
   // mvni  4s,2s    #imm8, LSL #0, 8, 16, 24
   // mvni  4s,2s    #imm8, MSL #8 or 16

   // not   16b,8b

   // pmul  16b,8b

   // pmull{2}  8h_8b_8b,8h_16b_16b,1q_1d_1d,1d_2d_2d

   // rbit    16b,8b
   // rev16   16b,8b
   // rev32   16b,8b,8h,4h
   // rev64   16b,8b,8h,4h,4s,2s

   // saba      16b,8b,8h,4h,4s,2s
   // uaba      16b,8b,8h,4h,4s,2s

   // sabal{2}  2d_(2s_2s)/(4s_4s), 4s_(4h_4h)/(8h_8h), 8h_(8b_8b)/(16b_16b)
   // uabal{2}  2d_(2s_2s)/(4s_4s), 4s_(4h_4h)/(8h_8h), 8h_(8b_8b)/(16b_16b)

   // sabd      16b,8b,8h,4h,4s,2s
   // uabd      16b,8b,8h,4h,4s,2s

   // sabdl{2}  2d_(2s_2s)/(4s_4s), 4s_(4h_4h)/(8h_8h), 8h_(8b_8b)/(16b_16b)
   // uabdl{2}  2d_(2s_2s)/(4s_4s), 4s_(4h_4h)/(8h_8h), 8h_(8b_8b)/(16b_16b)

   // sadalp    4h_8b,8h_16b,2s_4h,4s_8h,1d_2s,2d_4s
   // uadalp    4h_8b,8h_16b,2s_4h,4s_8h,1d_2s,2d_4s

   // saddl{2}  2d_(2s_2s)/(4s_4s), 4s_(4h_4h)/(8h_8h), 8h_(8b_8b)/(16b_16b)
   // uaddl{2}  2d_(2s_2s)/(4s_4s), 4s_(4h_4h)/(8h_8h), 8h_(8b_8b)/(16b_16b)
   // ssubl{2}  2d_(2s_2s)/(4s_4s), 4s_(4h_4h)/(8h_8h), 8h_(8b_8b)/(16b_16b)
   // usubl{2}  2d_(2s_2s)/(4s_4s), 4s_(4h_4h)/(8h_8h), 8h_(8b_8b)/(16b_16b)

   // saddlp    4h_8b,8h_16b,2s_4h,4s_8h,1d_2s,2d_4s
   // uaddlp    4h_8b,8h_16b,2s_4h,4s_8h,1d_2s,2d_4s

   // saddlv    h_16b/8b, s_8h/4h, d_4s
   // uaddlv    h_16b/8b, s_8h/4h, d_4s

   // saddw{2}  8h_8h_16b/8b, 4s_4s_8h/4h, 2d_2d_2s/4s
   // uaddw{2}  8h_8h_16b/8b, 4s_4s_8h/4h, 2d_2d_2s/4s
   // ssubw{2}  8h_8h_16b/8b, 4s_4s_8h/4h, 2d_2d_2s/4s
   // usubw{2}  8h_8h_16b/8b, 4s_4s_8h/4h, 2d_2d_2s/4s

   // shadd        16b,8b,8h,4h,4s,2s
   // uhadd        16b,8b,8h,4h,4s,2s
   // shsub        16b,8b,8h,4h,4s,2s
   // uhsub        16b,8b,8h,4h,4s,2s

   // shll{2}      8h_8b/16b_#8, 4s_4h/8h_#16, 2d_2s/4s_#32

   // shrn{2}      2s/4s_2d, 8h/4h_4s, 2s/4s_2d,   #imm in 1 .. elem_bits
   // rshrn{2}     2s/4s_2d, 8h/4h_4s, 2s/4s_2d,   #imm in 1 .. elem_bits

   // sli          d_#imm
   // sri          d_#imm

   // sli          2d,4s,2s,8h,4h,16b,8b  _#imm
   // sri          2d,4s,2s,8h,4h,16b,8b  _#imm

   // smax         4s,2s,8h,4h,16b,8b
   // umax         4s,2s,8h,4h,16b,8b
   // smin         4s,2s,8h,4h,16b,8b
   // umin         4s,2s,8h,4h,16b,8b
   test_smax_4s_4s_4s(TyS);
   test_smax_2s_2s_2s(TyS);
   test_smax_8h_8h_8h(TyH);
   test_smax_4h_4h_4h(TyH);
   test_smax_16b_16b_16b(TyB);
   test_smax_8b_8b_8b(TyB);
   test_umax_4s_4s_4s(TyS);
   test_umax_2s_2s_2s(TyS);
   test_umax_8h_8h_8h(TyH);
   test_umax_4h_4h_4h(TyH);
   test_umax_16b_16b_16b(TyB);
   test_umax_8b_8b_8b(TyB);
   test_smin_4s_4s_4s(TyS);
   test_smin_2s_2s_2s(TyS);
   test_smin_8h_8h_8h(TyH);
   test_smin_4h_4h_4h(TyH);
   test_smin_16b_16b_16b(TyB);
   test_smin_8b_8b_8b(TyB);
   test_umin_4s_4s_4s(TyS);
   test_umin_2s_2s_2s(TyS);
   test_umin_8h_8h_8h(TyH);
   test_umin_4h_4h_4h(TyH);
   test_umin_16b_16b_16b(TyB);
   test_umin_8b_8b_8b(TyB);

   // smaxp        4s,2s,8h,4h,16b,8b
   // umaxp        4s,2s,8h,4h,16b,8b
   // sminp        4s,2s,8h,4h,16b,8b
   // uminp        4s,2s,8h,4h,16b,8b

   // smaxv        s_4s,h_8h,h_4h,b_16b,b_8b
   // umaxv        s_4s,h_8h,h_4h,b_16b,b_8b
   // sminv        s_4s,h_8h,h_4h,b_16b,b_8b
   // uminv        s_4s,h_8h,h_4h,b_16b,b_8b
   test_SMAXV();
   test_UMAXV();
   test_SMINV();
   test_UMINV();

   // smlal{2}     2d_2s/4s_s[], 4s_4h/8h_h[]
   // umlal{2}     2d_2s/4s_s[], 4s_4h/8h_h[]
   // smlsl{2}     2d_2s/4s_s[], 4s_4h/8h_h[]
   // umlsl{2}     2d_2s/4s_s[], 4s_4h/8h_h[]
   // smull{2}     2d_2s/4s_s[]. 4s_4h/8h_h[]
   // umull{2}     2d_2s/4s_s[]. 4s_4h/8h_h[]

   // smlal{2}     2d_(2s_2s)/(4s_4s), 4s_(4h_4h)/(8h_8h), 8h_(8b_8b)/(16b_16b)
   // umlal{2}     2d_(2s_2s)/(4s_4s), 4s_(4h_4h)/(8h_8h), 8h_(8b_8b)/(16b_16b)
   // smlsl{2}     2d_(2s_2s)/(4s_4s), 4s_(4h_4h)/(8h_8h), 8h_(8b_8b)/(16b_16b)
   // umlsl{2}     2d_(2s_2s)/(4s_4s), 4s_(4h_4h)/(8h_8h), 8h_(8b_8b)/(16b_16b)
   // smull{2}     2d_(2s_2s)/(4s_4s), 4s_(4h_4h)/(8h_8h), 8h_(8b_8b)/(16b_16b)
   // umull{2}     2d_(2s_2s)/(4s_4s), 4s_(4h_4h)/(8h_8h), 8h_(8b_8b)/(16b_16b)

   // smov         w_b[], w_h[], x_b[], x_h[], x_s[]
   // umov         w_b[], w_h[], x_b[], x_h[], x_s[]
   // INCOMPLETE
   test_umov_x_d0(TyD);
   test_umov_x_d1(TyD);
   test_umov_w_s0(TyS);
   test_umov_w_s3(TyS);
   test_umov_w_h0(TyH);
   test_umov_w_h7(TyH);
   test_umov_w_b0(TyB);
   test_umov_w_b15(TyB);
   test_smov_x_s0(TyS);
   test_smov_x_s3(TyS);
   test_smov_x_h0(TyH);
   test_smov_x_h7(TyH);
   test_smov_w_h0(TyH);
   test_smov_w_h7(TyH);
   test_smov_x_b0(TyB);
   test_smov_x_b15(TyB);
   test_smov_w_b0(TyB);
   test_smov_w_b15(TyB);

   // sqabs        d,s,h,b
   // sqneg        d,s,h,b

   // sqabs        2d,4s,2s,8h,4h,16b,8b
   // sqneg        2d,4s,2s,8h,4h,16b,8b

   // sqadd        d,s,h,b
   // uqadd        d,s,h,b
   // sqsub        d,s,h,b
   // uqsub        d,s,h,b

   // sqadd        2d,4s,2s,8h,4h,16b,8b
   // uqadd        2d,4s,2s,8h,4h,16b,8b
   // sqsub        2d,4s,2s,8h,4h,16b,8b
   // uqsub        2d,4s,2s,8h,4h,16b,8b

   // sqdmlal      d_s_s[], s_h_h[]
   // sqdmlsl      d_s_s[], s_h_h[]
   // sqdmull      d_s_s[], s_h_h[]

   // sqdmlal{2}   2d_2s/4s_s[], 4s_4h/8h_h[]
   // sqdmlsl{2}   2d_2s/4s_s[], 4s_4h/8h_h[]
   // sqdmull{2}   2d_2s/4s_s[], 4s_4h/2h_h[]

   // sqdmlal      d_s_s, s_h_h
   // sqdmlsl      d_s_s, s_h_h
   // sqdmull      d_s_s, s_h_h

   // sqdmlal{2}   2d_(2s_2s)/(4s_4s), 4s_(4h_4h)/(8h_8h)
   // sqdmlsl{2}   2d_(2s_2s)/(4s_4s), 4s_(4h_4h)/(8h_8h)
   // sqdmull{2}   2d_(2s_2s)/(4s_4s), 4s_(4h_4h)/(8h_8h)

   // sqdmulh      s_s_s[], h_h_h[]
   // sqrdmulh     s_s_s[], h_h_h[]

   // sqdmulh      4s_4s_s[], 2s_2s_s[], 8h_8h_h[], 4h_4h_h[]
   // sqrdmulh     4s_4s_s[], 2s_2s_s[], 8h_8h_h[], 4h_4h_h[]

   // sqdmulh      h,s
   // sqrdmulh     h,s

   // sqdmulh      4s,2s,8h,4h
   // sqrdmulh     4s,2s,8h,4h

   // sqshl        d,s,h,b
   // uqshl        d,s,h,b
   // sqrshl       d,s,h,b
   // uqrshl       d,s,h,b

   // sqshl        2d,4s,2s,8h,4h,16b,8b
   // uqshl        2d,4s,2s,8h,4h,16b,8b
   // sqrshl       2d,4s,2s,8h,4h,16b,8b
   // uqrshl       2d,4s,2s,8h,4h,16b,8b

   // sqrshrn      s_d, h_s, b_h   #imm
   // uqrshrn      s_d, h_s, b_h   #imm
   // sqshrn       s_d, h_s, b_h   #imm
   // uqshrn       s_d, h_s, b_h   #imm

   // sqrshrun     s_d, h_s, b_h   #imm
   // sqshrun      s_d, h_s, b_h   #imm

   // sqrshrn{2}   2s/4s_2d, 4h/8h_4s, 8b/16b_8h,  #imm
   // uqrshrn{2}   2s/4s_2d, 4h/8h_4s, 8b/16b_8h,  #imm
   // sqshrn{2}    2s/4s_2d, 4h/8h_4s, 8b/16b_8h,  #imm
   // uqshrn{2}    2s/4s_2d, 4h/8h_4s, 8b/16b_8h,  #imm

   // sqrshrun{2}  2s/4s_2d, 4h/8h_4s, 8b/16b_8h,  #imm
   // sqshrun{2}   2s/4s_2d, 4h/8h_4s, 8b/16b_8h,  #imm

   // sqshl        d,s,h,b   _#imm
   // uqshl        d,s,h,b   _#imm
   // sqshlu       d,s,h,b   _#imm

   // sqshl        2d,4s,2s,8h,4h,16b,8b   _#imm
   // uqshl        2d,4s,2s,8h,4h,16b,8b   _#imm
   // sqshlu       2d,4s,2s,8h,4h,16b,8b   _#imm

   // sqxtn        s_d,h_s,b_h
   // uqxtn        s_d,h_s,b_h
   // sqxtun       s_d,h_s,b_h

   // sqxtn{2}     2s/4s_2d, 4h/8h_4s, 8b/16b_8h
   // uqxtn{2}     2s/4s_2d, 4h/8h_4s, 8b/16b_8h
   // sqxtun{2}    2s/4s_2d, 4h/8h_4s, 8b/16b_8h

   // srhadd       4s,2s,8h,4h,16b,8b
   // urhadd       4s,2s,8h,4h,16b,8b

   // sshl (reg)   d
   // ushl (reg)   d
   // sshl (reg)   2d,4s,2s,8h,4h,16b,8b
   // ushl (reg)   2d,4s,2s,8h,4h,16b,8b

   // shl  (imm)   d
   // sshr (imm)   d
   // ushr (imm)   d

   // shl  (imm)   16b,8b,8h,4h,4s,2s,2d
   // sshr (imm)   2d,4s,2s,8h,4h,16b,8b
   // ushr (imm)   2d,4s,2s,8h,4h,16b,8b
   test_shl_2d_2d_1(TyD);
   test_shl_2d_2d_13(TyD);
   test_shl_2d_2d_63(TyD);
   test_shl_4s_4s_1(TyS);
   test_shl_4s_4s_13(TyS);
   test_shl_4s_4s_31(TyS);
   test_shl_2s_2s_1(TyS);
   test_shl_2s_2s_13(TyS);
   test_shl_2s_2s_31(TyS);
   test_shl_8h_8h_1(TyH);
   test_shl_8h_8h_13(TyH);
   test_shl_8h_8h_15(TyH);
   test_shl_4h_4h_1(TyH);
   test_shl_4h_4h_13(TyH);
   test_shl_4h_4h_15(TyH);
   test_shl_16b_16b_1(TyB);
   test_shl_16b_16b_7(TyB);
   test_shl_8b_8b_1(TyB);
   test_shl_8b_8b_7(TyB);
   test_sshr_2d_2d_1(TyD);
   test_sshr_2d_2d_13(TyD);
   test_sshr_2d_2d_63(TyD);
   test_sshr_4s_4s_1(TyS);
   test_sshr_4s_4s_13(TyS);
   test_sshr_4s_4s_31(TyS);
   test_sshr_2s_2s_1(TyS);
   test_sshr_2s_2s_13(TyS);
   test_sshr_2s_2s_31(TyS);
   test_sshr_8h_8h_1(TyH);
   test_sshr_8h_8h_13(TyH);
   test_sshr_8h_8h_15(TyH);
   test_sshr_4h_4h_1(TyH);
   test_sshr_4h_4h_13(TyH);
   test_sshr_4h_4h_15(TyH);
   test_sshr_16b_16b_1(TyB);
   test_sshr_16b_16b_7(TyB);
   test_sshr_8b_8b_1(TyB);
   test_sshr_8b_8b_7(TyB);
   test_ushr_2d_2d_1(TyD);
   test_ushr_2d_2d_13(TyD);
   test_ushr_2d_2d_63(TyD);
   test_ushr_4s_4s_1(TyS);
   test_ushr_4s_4s_13(TyS);
   test_ushr_4s_4s_31(TyS);
   test_ushr_2s_2s_1(TyS);
   test_ushr_2s_2s_13(TyS);
   test_ushr_2s_2s_31(TyS);
   test_ushr_8h_8h_1(TyH);
   test_ushr_8h_8h_13(TyH);
   test_ushr_8h_8h_15(TyH);
   test_ushr_4h_4h_1(TyH);
   test_ushr_4h_4h_13(TyH);
   test_ushr_4h_4h_15(TyH);
   test_ushr_16b_16b_1(TyB);
   test_ushr_16b_16b_7(TyB);
   test_ushr_8b_8b_1(TyB);
   test_ushr_8b_8b_7(TyB);

   // ssra (imm)   d
   // usra (imm)   d
   // ssra (imm)   2d,4s,2s,8h,4h,16b,8b
   // usra (imm)   2d,4s,2s,8h,4h,16b,8b

   // srshl (reg)  d
   // urshl (reg)  d
   // srshl (reg)  2d,4s,2s,8h,4h,16b,8b
   // urshl (reg)  2d,4s,2s,8h,4h,16b,8b

   // srshr (imm)  d
   // urshr (imm)  d
   // srshr (imm)  2d,4s,2s,8h,4h,16b,8b
   // urshr (imm)  2d,4s,2s,8h,4h,16b,8b

   // srsra (imm)  d
   // ursra (imm)  d
   // srsra (imm)  2d,4s,2s,8h,4h,16b,8b
   // ursra (imm)  2d,4s,2s,8h,4h,16b,8b

   // sshll{2} (imm)  2d_2s/4s, 4s_4h/8h, 8h_8b/16b
   // ushll{2} (imm)  2d_2s/4s, 4s_4h/8h, 8h_8b/16b
   // INCOMPLETE
   test_sshll_2d_2s_0(TyS);
   test_sshll_2d_2s_15(TyS);
   test_sshll_2d_2s_31(TyS);
   test_sshll2_2d_4s_0(TyS);
   test_sshll2_2d_4s_15(TyS);
   test_sshll2_2d_4s_31(TyS);
   test_ushll_2d_2s_0(TyS);
   test_ushll_2d_2s_15(TyS);
   test_ushll_2d_2s_31(TyS);
   test_ushll2_2d_4s_0(TyS);
   test_ushll2_2d_4s_15(TyS);
   test_ushll2_2d_4s_31(TyS);

   // suqadd  d,s,h,b
   // suqadd  2d,4s,2s,8h,4h,16b,8b

   // tbl     8b_{16b}_8b, 16b_{16b}_16b
   // tbl     8b_{16b,16b}_8b, 16b_{16b,16b}_16b
   // tbl     8b_{16b,16b,16b}_8b, 16b_{16b,16b,16b}_16b
   // tbl     8b_{16b,16b,16b,16b}_8b, 16b_{16b,16b,16b,16b}_16b
   test_tbl_16b_1reg(TyB);
   test_tbl_16b_2reg(TyB);
   test_tbl_16b_3reg(TyB);
   test_tbl_16b_4reg(TyB);
   test_tbl_8b_1reg(TyB);
   test_tbl_8b_2reg(TyB);
   test_tbl_8b_3reg(TyB);
   test_tbl_8b_4reg(TyB);

   // tbx     8b_{16b}_8b, 16b_{16b}_16b
   // tbx     8b_{16b,16b}_8b, 16b_{16b,16b}_16b
   // tbx     8b_{16b,16b,16b}_8b, 16b_{16b,16b,16b}_16b
   // tbx     8b_{16b,16b,16b,16b}_8b, 16b_{16b,16b,16b,16b}_16b
   test_tbx_16b_1reg(TyB);
   test_tbx_16b_2reg(TyB);
   test_tbx_16b_3reg(TyB);
   test_tbx_16b_4reg(TyB);
   test_tbx_8b_1reg(TyB);
   test_tbx_8b_2reg(TyB);
   test_tbx_8b_3reg(TyB);
   test_tbx_8b_4reg(TyB);

   // trn1    2d,4s,2s,8h,4h,16b,8b
   // trn2    2d,4s,2s,8h,4h,16b,8b

   // urecpe      4s,2s

   // ursqrte     4s,2s

   // usqadd      d,s,h,b
   // usqadd      2d,4s,2s,8h,4h,16b,8b

   // uzp1      2d,4s,2s,8h,4h,16b,8b
   // uzp2      2d,4s,2s,8h,4h,16b,8b

   // xtn{2}    2s/4s_2d, 4h/8h_4s, 8b/16b_8h
   test_xtn_2s_2d(TyD);
   test_xtn2_4s_2d(TyD);
   test_xtn_4h_4s(TyS);
   test_xtn2_8h_4s(TyS);
   test_xtn_8b_8h(TyH);
   test_xtn2_16b_8h(TyH);

   // zip1      2d,4s,2s,8h,4h,16b,8b
   // zip2      2d,4s,2s,8h,4h,16b,8b

   // ======================== MEM ========================

   // ld1  (multiple 1-element structures to 1/2/3/4 regs)
   // ld1  (single 1-element structure to one lane of 1 reg)
   // ld1r (single 1-element structure and rep to all lanes of 1 reg)

   // ld2  (multiple 2-element structures to 2 regs)
   // ld2  (single 2-element structure to one lane of 2 regs)
   // ld2r (single 2-element structure and rep to all lanes of 2 regs)

   // ld3  (multiple 3-element structures to 3 regs)
   // ld3  (single 3-element structure to one lane of 3 regs)
   // ld3r (single 3-element structure and rep to all lanes of 3 regs)

   // ld4  (multiple 4-element structures to 4 regs)
   // ld4  (single 4-element structure to one lane of 4 regs)
   // ld4r (single 4-element structure and rep to all lanes of 4 regs)

   // ldnp  q_q_addr,d_d_addr,s_s_addr  (load pair w/ non-temporal hint)
   //       addr = reg + uimm7 * reg_size

   // ldp   q_q_addr,d_d_addr,s_s_addr  (load pair)
   //       addr = [Xn|SP],#imm   or [Xn|SP,#imm]!  or [Xn|SP,#imm]

   // ldr   q,d,s,h,b from addr
   //       addr = [Xn|SP],#imm   or [Xn|SP,#imm]!  or [Xn|SP,#imm]

   // ldr   q,d,s from  pc+#imm19

   // ldr   q,d,s,h,b from addr
   //       addr = [Xn|SP, R <extend> <shift]

   // ldur  q,d,s,h,b from addr
   //       addr = [Xn|SP,#imm] (unscaled offset)

   // st1 (multiple 1-element structures from 1/2/3/4 regs)
   // st1 (single 1-element structure for 1 lane of 1 reg)

   // st2 (multiple 2-element structures from 2 regs)
   // st2 (single 2-element structure from 1 lane of 2 regs)

   // st3 (multiple 3-element structures from 3 regs)
   // st3 (single 3-element structure from 1 lane of 3 regs)

   // st4 (multiple 4-element structures from 4 regs)
   // st4 (single 4-element structure from one lane of 4 regs)

   // stnp q_q_addr, d_d_addr, s_s_addr
   //      addr = [Xn|SP, #imm]

   // stp  q_q_addr, d_d_addr, s_s_addr
   //      addr = [Xn|SP], #imm  or [Xn|SP, #imm]!  or [Xn|SP, #imm]

   // str  q,d,s,h,b_addr
   //      addr = [Xn|SP], #simm  or [Xn|SP, #simm]!  or [Xn|SP, #pimm]

   // str   q,d,s,h,b_addr
   //       addr = [Xn|SP, R <extend> <shift]

   // stur  q,d,s,h,b_addr
   //       addr = [Xn|SP,#imm] (unscaled offset)

   // ======================== CRYPTO ========================

   // aesd       16b (aes single round decryption)
   // aese       16b (aes single round encryption)
   // aesimc     16b (aes inverse mix columns)
   // aesmc      16b (aes mix columns)

   // sha1c      q_s_4s
   // sha1h      s_s
   // sha1m      q_s_4s
   // sha1p      q_s_4s
   // sha1su0    4s_4s_4s
   // sha1su1    4s_4s

   // sha256h2   q_q_4s
   // sha256h    q_q_4s
   // sha256su0  4s_4s
   // sha256su1  4s_4s_4s

   return 0;
}


/* ---------------------------------------------------------------- */
/* -- Alphabetical list of insns                                 -- */
/* ---------------------------------------------------------------- */
/*
   abs      d
   abs      2d,4s,2s,8h,4h,16b,8b
   add      d
   add      2d,4s,2s,8h,4h,16b,8b
   addhn    2s.2d.2d, 4s.2d.2d, h_from_s and b_from_h (add and get high half)
   addp     d (add pairs, across)
   addp     2d,4s,2s,8h,4h,16b,8b
   addv     4s,8h,4h,16b,18b (reduce across vector)
   aesd     16b (aes single round decryption)
   aese     16b (aes single round encryption)
   aesimc   16b (aes inverse mix columns)
   aesmc    16b (aes mix columns)
   and      16b,8b

   bic      4s,2s,8h,4h (vector, imm)
   also movi, mvni, orr

   bic      16b,8b (vector,reg) (bit clear)
   bif      16b,8b (vector) (bit insert if false)
   bit      16b,8b (vector) (bit insert if true)
   bsl      16b,8b (vector) (bit select)

   cls      4s,2s,8h,4h,16b,8b (count leading sign bits)
   clz      4s,2s,8h,4h,16b,8b (count leading zero bits)

   cmeq     d
   cmeq     2d,4s,2s,8h,4h,16b,8b
   cmeq_z   d
   cmeq_z   2d,4s,2s,8h,4h,16b,8b

   cmge     d
   cmge     2d,4s,2s,8h,4h,16b,8b
   cmge_z   d
   cmge_z   2d,4s,2s,8h,4h,16b,8b

   cmgt     d
   cmgt     2d,4s,2s,8h,4h,16b,8b
   cmgt_z   d
   cmgt_z   2d,4s,2s,8h,4h,16b,8b

   cmhi     d
   cmhi     2d,4s,2s,8h,4h,16b,8b

   cmhs     d
   cmhs     2d,4s,2s,8h,4h,16b,8b

   cmle_z   d
   cmle_z   2d,4s,2s,8h,4h,16b,8b

   cmlt_z   d
   cmlt_z   2d,4s,2s,8h,4h,16b,8b

   cmtst    d
   cmtst    2d,4s,2s,8h,4h,16b,8b

   cnt      16b,8b (population count per byte)

   dup      d,s,h,b (vec elem to scalar)
   dup      2d,4s,2s,8h,4h,16b,8b (vec elem to vector)
   dup      2d,4s,2s,8h,4h,16b,8b (general reg to vector)

   eor      16b,8b (vector)
   ext      16b,8b,#imm4 (concat 2 vectors, then slice)

   fabd     d,s
   fabd     2d,4s,2s

   fabs     d,s
   fabs     2d,4s,2s

   facge    s,d  (floating abs compare GE)
   facge    2d,4s,2s

   facgt    s,d  (floating abs compare GE)
   facgt    2d,4s,2s

   fadd     d,s
   fadd     2d,4s,2s

   faddp    d,s (floating add pair)
   faddp    2d,4s,2s

   fccmp    d,s (floating point conditional quiet compare)
   fccmpe   d,s (floating point conditional signaling compare)

   fcmeq    d,s
   fcmeq    2d,4s,2s
   fcmeq_z  d,s
   fcmeq_z  2d,4s,2s

   fcmge    d,s
   fcmge    2d,4s,2s
   fcmge_z  d,s
   fcmge_z  2d,4s,2s

   fcmgt    d,s
   fcmgt    2d,4s,2s
   fcmgt_z  d,s
   fcmgt_z  2d,4s,2s

   fcmle_z  d,s
   fcmle_z  2d,4s,2s

   fcmlt_z  d,s
   fcmlt_z  2d,4s,2s

   fcmp     d,s (floating point quiet, set flags)
   fcmp_z   d,s
   fcmpe    d,s (floating point signaling, set flags)
   fcmpe_z  d,s

   fcsel    d,s (fp cond select)

   fcvt     s_h,d_h,h_s,d_s,h_d,s_d (fp convert, scalar)

   fcvtas   d,s  (fcvt to signed int, nearest, ties away)
   fcvtas   2d,4s,2s
   fcvtas   w_s,x_s,w_d,x_d

   fcvtau   d,s  (fcvt to unsigned int, nearest, ties away)
   fcvtau   2d,4s,2s
   fcvtau   w_s,x_s,w_d,x_d

   fcvtl{2} 4s/4h, 4s/8h, 2d/2s, 2d/4s (float convert to longer form)

   fcvtms   d,s  (fcvt to signed int, minus inf)
   fcvtms   2d,4s,2s
   fcvtms   w_s,x_s,w_d,x_d

   fcvtmu   d,s  (fcvt to unsigned int, minus inf)
   fcvtmu   2d,4s,2s
   fcvtmu   w_s,x_s,w_d,x_d

   fcvtn{2} 4h/4s, 8h/4s, 2s/2d, 4s/2d (float convert to narrower form)

   fcvtns   d,s  (fcvt to signed int, nearest)
   fcvtns   2d,4s,2s
   fcvtns   w_s,x_s,w_d,x_d

   fcvtnu   d,s  (fcvt to unsigned int, nearest)
   fcvtnu   2d,4s,2s
   fcvtnu   w_s,x_s,w_d,x_d

   fcvtps   d,s  (fcvt to signed int, plus inf)
   fcvtps   2d,4s,2s
   fcvtps   w_s,x_s,w_d,x_d

   fcvtpu   d,s  (fcvt to unsigned int, plus inf)
   fcvtpu   2d,4s,2s
   fcvtpu   w_s,x_s,w_d,x_d

   fcvtxn   s_d (fcvt to lower prec narrow, rounding to odd)
   fcvtxn   2s_2d,4s_2d

   fcvtzs   s,d (fcvt to signed fixedpt, to zero) (w/ #fbits)
   fcvtzs   2d,4s,2s

   fcvtzs   s,d (fcvt to signed integer, to zero)
   fcvtzs   2d,4s,2s

   fcvtzs   w_s,x_s,w_d,x_d (fcvt to signed fixedpt, to zero) (w/ #fbits)

   fcvtzs   w_s,x_s,w_d,x_d (fcvt to signed integer, to zero)

   fcvtzu   s,d (fcvt to unsigned fixedpt, to zero) (w/ #fbits)
   fcvtzu   2d,4s,2s

   fcvtzu   s,d (fcvt to unsigned integer, to zero)
   fcvtzu   2d,4s,2s

   fcvtzu   w_s,x_s,w_d,x_d (fcvt to unsigned fixedpt, to zero) (w/ #fbits)

   fcvtzu   w_s,x_s,w_d,x_d (fcvt to unsigned integer, to zero)

   fdiv     d,s
   fdiv     2d,4s,2s

   fmadd    d,s
   fnmadd   d,s
   fnmsub   d,s
   fnmul    d,s

   fmax     d,s
   fmin     d,s

   fmax     2d,4s,2s
   fmin     2d,4s,2s

   fmaxnm   d,s ("max number")
   fminnm   d,s

   fmaxnm   2d,4s,2s
   fminnm   2d,4s,2s

   fmaxnmp  d_2d,s_2s ("max number pairwise")
   fminnmp  d_2d,s_2s

   fmaxnmp  2d,4s,2s
   fminnmp  2d,4s,2s

   fmaxnmv  s_4s (maxnum across vector)
   fminnmv  s_4s

   fmaxp    d_2d,s_2s (max of a pair)
   fminp    d_2d,s_2s (max of a pair)

   fmaxp    2d,4s,2s  (max pairwise)
   fminp    2d,4s,2s

   fmaxv    s_4s (max across vector)
   fminv    s_4s

   fmla     d_d_d[],s_s_s[] (by element)
   fmla     2d_2d_d[],4s_4s_s[],2s_2s_s[]

   fmla     2d,4s,2s

   fmls     d_d_d[],s_s_s[] (by element)
   fmls     2d_2d_d[],4s_4s_s[],2s_2s_s[]

   fmls     2d,4s,2s

   fmov     2d,4s,2s #imm (part of the MOVI/MVNI/ORR/BIC imm group)

   fmov     d_d,s_s

   fmov     s_w,w_s,d_x,d[1]_x,x_d,x_d[1]

   fmov     d,s #imm

   fmsub    d,s

   fmul     d_d_d[],s_s_s[]
   fmul     2d_2d_d[],4s_4s_s[],2s_2s_s[]

   fmul     2d,4s,2s
   fmul     d,s

   fmulx    d_d_d[],s_s_s[]
   fmulx    2d_2d_d[],4s_4s_s[],2s_2s_s[]

   fmulx    d,s
   fmulx    2d,4s,2s

   fneg     d,s
   fneg     2d,4s,2s

   frecpe   d,s (recip estimate)
   frecpe   2d,4s,2s

   frecps   d,s (recip step)
   frecps   2d,4s,2s

   frecpx   d,s (recip exponent)

   frinta   2d,4s,2s (round to integral, nearest away)
   frinta   d,s

   frinti   2d,4s,2s (round to integral, per FPCR)
   frinti   d,s

   frintm   2d,4s,2s (round to integral, minus inf)
   frintm   d,s

   frintn   2d,4s,2s (round to integral, nearest, to even)
   frintn   d,s

   frintp   2d,4s,2s (round to integral, plus inf)
   frintp   d,s

   frintx   2d,4s,2s (round to integral exact, per FPCR)
   frintx   d,s

   frintz   2d,4s,2s (round to integral, zero)
   frintz   d,s

   frsqrte  d,s (est)
   frsqrte  2d,4s,2s

   frsqrts  d,s (step)
   frsqrts  2d,4s,2s

   fsqrt    d,s
   fsqrt    2d,4s,2s

   fsub     d,s
   fsub     2d,4s,2s

   ins      d[]_d[],s[]_s[],h[]_h[],b[]_b[]

   ins      d[]_x, s[]_w, h[]_w, b[]_w

   ld1  (multiple 1-element structures to 1/2/3/4 regs)
   ld1  (single 1-element structure to one lane of 1 reg)
   ld1r (single 1-element structure and rep to all lanes of 1 reg)

   ld2  (multiple 2-element structures to 2 regs)
   ld2  (single 2-element structure to one lane of 2 regs)
   ld2r (single 2-element structure and rep to all lanes of 2 regs)

   ld3  (multiple 3-element structures to 3 regs)
   ld3  (single 3-element structure to one lane of 3 regs)
   ld3r (single 3-element structure and rep to all lanes of 3 regs)

   ld4  (multiple 4-element structures to 4 regs)
   ld4  (single 4-element structure to one lane of 4 regs)
   ld4r (single 4-element structure and rep to all lanes of 4 regs)

   ldnp  q_q_addr,d_d_addr,s_s_addr  (load pair w/ non-temporal hint)
         addr = reg + uimm7 * reg_size

   ldp   q_q_addr,d_d_addr,s_s_addr  (load pair)
         addr = [Xn|SP],#imm   or [Xn|SP,#imm]!  or [Xn|SP,#imm]

   ldr   q,d,s,h,b from addr
         addr = [Xn|SP],#imm   or [Xn|SP,#imm]!  or [Xn|SP,#imm]

   ldr   q,d,s from  pc+#imm19

   ldr   q,d,s,h,b from addr
         addr = [Xn|SP, R <extend> <shift]

   ldur  q,d,s,h,b from addr
         addr = [Xn|SP,#imm] (unscaled offset)

   mla   4s_4s_s[],2s_2s_s[],8h_8h_h[],4h_4h_h[]
   mla   4s,2s,8h,4h,16b,8b

   mls   4s_4s_s[],2s_2s_s[],8h_8h_h[],4h_4h_h[]
   mls   4s,2s,8h,4h,16b,8b

   movi  16b,8b   #imm8, LSL #0
   movi  8h,4h    #imm8, LSL #0 or 8
   movi  4s,2s    #imm8, LSL #0, 8, 16, 24
   movi  4s,2s    #imm8, MSL #8 or 16
   movi  d,       #imm64
   movi  2d,      #imm64

   mul   4s_4s_s[],2s_2s_s[],8h_8h_h[],4h_4h_h[]
   mul   4s,2s,8h,4h,16b,8b

   mvni  8h,4h    #imm8, LSL #0 or 8
   mvni  4s,2s    #imm8, LSL #0, 8, 16, 24
   mvni  4s,2s    #imm8, MSL #8 or 16

   neg   d
   neg   2d,4s,2s,8h,4h,16b,8b

   not   16b,8b

   orn   16b,8b

   orr   8h,4h   #imm8, LSL #0 or 8
   orr   4s,2s   #imm8, LSL #0, 8, 16 or 24

   orr   16b,8b

   pmul  16b,8b

   pmull{2}  8h_8b_8b,8h_16b_16b,1q_1d_1d,1d_2d_2d

   raddhn{2}  2s/4s_2d_2d, 4h/8h_4s_4s, 8b/16b_8h_8h

   rbit    16b,8b
   rev16   16b,8b
   rev32   16b,8b,8h,4h
   rev64   16b,8b,8h,4h,4s,2s

   rshrn{2}  2s/4s_2d, 8h/4h_4s, 2s/4s_2d,   #imm in 1 .. elem_bits

   rsubhn{2}  2s/4s_2d_2d, 4h/8h_4s_4s, 8b/16b_8h_8h

   saba      16b,8b,8h,4h,4s,2s
   sabal{2}  2d_(2s_2s)/(4s_4s), 4s_(4h_4h)/(8h_8h), 8h_(8b_8b)/(16b_16b)

   sabd      16b,8b,8h,4h,4s,2s
   sabdl{2}  2d_(2s_2s)/(4s_4s), 4s_(4h_4h)/(8h_8h), 8h_(8b_8b)/(16b_16b)

   sadalp    4h_8b,8h_16b,2s_4h,4s_8h,1d_2s,2d_4s

   saddl{2}  2d_(2s_2s)/(4s_4s), 4s_(4h_4h)/(8h_8h), 8h_(8b_8b)/(16b_16b)

   saddlp    4h_8b,8h_16b,2s_4h,4s_8h,1d_2s,2d_4s

   saddlv    h_16b/8b, s_8h/4h, d_4s

   saddw{2}  8h_8h_16b/8b, 4s_4s_8h/4h, 2d_2d_2s/4s

   scvtf     d,s        _#fbits
   scvtf     2d,4s,2s   _#fbits

   scvtf     d,s
   scvtf     2d,4s,2s

   scvtf     s_w, d_w, s_x, d_x,   _#fbits
   scvtf     s_w, d_w, s_x, d_x

   sha1c       q_s_4s
   sha1h       s_s
   sha1m       q_s_4s
   sha1p       q_s_4s
   sha1su0     4s_4s_4s
   sha1su1     4s_4s
   sha256h2    q_q_4s
   sha256h     q_q_4s
   sha256su0   4s_4s
   sha256su1   4s_4s_4s

   shadd       16b,8b,8h,4h,4s,2s

   shl         d_#imm
   shl         16b,8b,8h,4h,4s,2s,2d  _#imm

   shll{2}   8h_8b/16b_#8, 4s_4h/8h_#16, 2d_2s/4s_#32

   shrn{2}  2s/4s_2d, 8h/4h_4s, 2s/4s_2d,   #imm in 1 .. elem_bits

   shsub       16b,8b,8h,4h,4s,2s

   sli         d_#imm
   sli         2d,4s,2s,8h,4h,16b,8b  _#imm

   smax        4s,2s,8h,4h,16b,8b

   smaxp       4s,2s,8h,4h,16b,8b

   smaxv       s_4s,h_8h,h_4h,b_16b,b_8b

   smin        4s,2s,8h,4h,16b,8b

   sminp       4s,2s,8h,4h,16b,8b

   sminv       s_4s,h_8h,h_4h,b_16b,b_8b

   smlal{2}    2d_2s/4s_s[], 4s_4h/8h_h[]
   smlal{2}    2d_(2s_2s)/(4s_4s), 4s_(4h_4h)/(8h_8h), 8h_(8b_8b)/(16b_16b)

   smlsl{2}    2d_2s/4s_s[], 4s_4h/8h_h[]
   smlsl{2}    2d_(2s_2s)/(4s_4s), 4s_(4h_4h)/(8h_8h), 8h_(8b_8b)/(16b_16b)

   smov        w_b[], w_h[], x_b[], x_h[], x_s[]

   smull{2}    2d_2s/4s_s[]. 4s_4h/8h_h[]
   smull{2}    2d_(2s_2s)/(4s_4s), 4s_(4h_4h)/(8h_8h), 8h_(8b_8b)/(16b_16b)

   sqabs       d,s,h,b
   sqabs       2d,4s,2s,8h,4h,16b,8b

   sqadd       d,s,h,b
   sqadd       2d,4s,2s,8h,4h,16b,8b

   sqdmlal     d_s_s[], s_h_h[]
   sqdmlal{2}  2d_2s/4s_s[], 4s_4h/8h_h[]

   sqdmlal     d_s_s, s_h_h
   sqdmlal{2}  2d_(2s_2s)/(4s_4s), 4s_(4h_4h)/(8h_8h)

   sqdmlsl     d_s_s[], s_h_h[]
   sqdmlsl{2}  2d_2s/4s_s[], 4s_4h/8h_h[]

   sqdmlsl     d_s_s, s_h_h
   sqdmlsl{2}  2d_(2s_2s)/(4s_4s), 4s_(4h_4h)/(8h_8h)

   sqdmulh     s_s_s[], h_h_h[]
   sqdmulh     4s_4s_s[], 2s_2s_s[], 8h_8h_h[], 4h_4h_h[]

   sqdmulh     h,s
   sqdmulh     4s,2s,8h,4h

   sqdmull     d_s_s[], s_h_h[]
   sqdmull{2}  2d_2s/4s_s[], 4s_4h/2h_h[]

   sqdmull     d_s_s,s_h_h
   sqdmull{2}  2d_(2s_2s)/(4s_4s), 4s_(4h_4h)/(8h_8h)

   sqneg       d,s,h,b
   sqneg       2d,4s,2s,8h,4h,16b,8b

   sqrdmulh    s_s_s[], h_h_h[]
   sqrdmulh    4s_4s_s[], 2s_2s_s[], 8h_8h_h[], 4h_4h_h[]

   sqrdmulh    h,s
   sqrdmulh    4s,2s,8h,4h

   sqrshl      d,s,h,b
   sqrshl      2d,4s,2s,8h,4h,16b,8b

   sqrshrn     s_d, h_s, b_h   #imm
   sqrshrn{2}  2s/4s_2d, 4h/8h_4s, 8b/16b_8h,  #imm

   sqrshrun     s_d, h_s, b_h   #imm
   sqrshrun{2}  2s/4s_2d, 4h/8h_4s, 8b/16b_8h,  #imm

   sqshl        d,s,h,b   _#imm
   sqshl        2d,4s,2s,8h,4h,16b,8b   _#imm

   sqshl        d,s,h,b
   sqshl        2d,4s,2s,8h,4h,16b,8b

   sqshlu       d,s,h,b  _#imm
   sqshlu       2d,4s,2s,8h,4h,16b,8b  _#imm

   sqshrn       s_d, h_s, b_h   #imm
   sqshrn{2}    2s/4s_2d, 4h/8h_4s, 8b/16b_8h,  #imm

   sqshrun      s_d, h_s, b_h   #imm
   sqshrun{2}   2s/4s_2d, 4h/8h_4s, 8b/16b_8h,  #imm

   sqsub       d,s,h,b
   sqsub       2d,4s,2s,8h,4h,16b,8b

   sqxtn       s_d,h_s,b_h
   sqxtn{2}    2s/4s_2d, 4h/8h_4s, 8b/16b_8h

   sqxtun      s_d,h_s,b_h
   sqxtun{2}   2s/4s_2d, 4h/8h_4s, 8b/16b_8h

   srhadd      4s,2s,8h,4h,16b,8b

   sri         d_#imm
   sri         2d,4s,2s,8h,4h,16b,8b  _#imm

   srshl (reg) d
   srshl       2d,4s,2s,8h,4h,16b,8b

   srshr (imm) d
   srshr       2d,4s,2s,8h,4h,16b,8b

   srsra (imm) d
   srsra       2d,4s,2s,8h,4h,16b,8b

   sshl (reg)  d
   sshl        2d,4s,2s,8h,4h,16b,8b

   sshll{2} (imm)  2d_2s/4s  4s_4h/8h, 8h_8b/16b

   sshr (imm)  d
   sshr        2d,4s,2s,8h,4h,16b,8b

   ssra (imm)  d
   ssra        2d,4s,2s,8h,4h,16b,8b

   ssubl{2}  2d_(2s_2s)/(4s_4s), 4s_(4h_4h)/(8h_8h), 8h_(8b_8b)/(16b_16b)

   ssubw{2}  8h_8h_16b/8b, 4s_4s_8h/4h, 2d_2d_2s/4s

   st1 (multiple 1-element structures from 1/2/3/4 regs)
   st1 (single 1-element structure for 1 lane of 1 reg)

   st2 (multiple 2-element structures from 2 regs)
   st2 (single 2-element structure from 1 lane of 2 regs)

   st3 (multiple 3-element structures from 3 regs)
   st3 (single 3-element structure from 1 lane of 3 regs)

   st4 (multiple 4-element structures from 4 regs)
   st4 (single 4-element structure from one lane of 4 regs)

   stnp q_q_addr, d_d_addr, s_s_addr
        addr = [Xn|SP, #imm]

   stp  q_q_addr, d_d_addr, s_s_addr
        addr = [Xn|SP], #imm  or [Xn|SP, #imm]!  or [Xn|SP, #imm]

   str  q,d,s,h,b_addr
        addr = [Xn|SP], #simm  or [Xn|SP, #simm]!  or [Xn|SP, #pimm]

   str   q,d,s,h,b_addr
         addr = [Xn|SP, R <extend> <shift]

   stur  q,d,s,h,b_addr
         addr = [Xn|SP,#imm] (unscaled offset)

   sub   d
   sub   2d,4s,2s,8h,4h,16b,8b

   subhn{2}  2s/4s_2d_2d, 4h/8h_4s_4s, 8b/16b_8h_8h

   suqadd  d,s,h,b
   suqadd  2d,4s,2s,8h,4h,16b,8b

   tbl     8b_{16b}_8b, 16b_{16b}_16b
   tbl     8b_{16b,16b}_8b, 16b_{16b,16b}_16b
   tbl     8b_{16b,16b,16b}_8b, 16b_{16b,16b,16b}_16b
   tbl     8b_{16b,16b,16b,16b}_8b, 16b_{16b,16b,16b,16b}_16b

   tbx     8b_{16b}_8b, 16b_{16b}_16b
   tbx     8b_{16b,16b}_8b, 16b_{16b,16b}_16b
   tbx     8b_{16b,16b,16b}_8b, 16b_{16b,16b,16b}_16b
   tbx     8b_{16b,16b,16b,16b}_8b, 16b_{16b,16b,16b,16b}_16b

   trn1    2d,4s,2s,8h,4h,16b,8b
   trn2    2d,4s,2s,8h,4h,16b,8b

   uaba      16b,8b,8h,4h,4s,2s
   uabal{2}  2d_(2s_2s)/(4s_4s), 4s_(4h_4h)/(8h_8h), 8h_(8b_8b)/(16b_16b)

   uabd      16b,8b,8h,4h,4s,2s
   uabdl{2}  2d_(2s_2s)/(4s_4s), 4s_(4h_4h)/(8h_8h), 8h_(8b_8b)/(16b_16b)

   uadalp    4h_8b,8h_16b,2s_4h,4s_8h,1d_2s,2d_4s

   uaddl{2}  2d_(2s_2s)/(4s_4s), 4s_(4h_4h)/(8h_8h), 8h_(8b_8b)/(16b_16b)

   uaddlp    4h_8b,8h_16b,2s_4h,4s_8h,1d_2s,2d_4s

   uaddlv    h_16b/8b, s_8h/4h, d_4s

   uaddw{2}  8h_8h_16b/8b, 4s_4s_8h/4h, 2d_2d_2s/4s

   ucvtf     d,s        _#fbits
   ucvtf     2d,4s,2s   _#fbits

   ucvtf     d,s
   ucvtf     2d,4s,2s

   ucvtf     s_w, d_w, s_x, d_x,   _#fbits
   ucvtf     s_w, d_w, s_x, d_x

   uhadd       16b,8b,8h,4h,4s,2s

   uhsub       16b,8b,8h,4h,4s,2s

   umax        4s,2s,8h,4h,16b,8b

   umaxp       4s,2s,8h,4h,16b,8b

   umaxv       s_4s,h_8h,h_4h,b_16b,b_8b

   umin        4s,2s,8h,4h,16b,8b

   uminp       4s,2s,8h,4h,16b,8b

   uminv       s_4s,h_8h,h_4h,b_16b,b_8b

   umlal{2}    2d_2s/4s_s[], 4s_4h/8h_h[]
   umlal{2}    2d_(2s_2s)/(4s_4s), 4s_(4h_4h)/(8h_8h), 8h_(8b_8b)/(16b_16b)

   umlsl{2}    2d_2s/4s_s[], 4s_4h/8h_h[]
   umlsl{2}    2d_(2s_2s)/(4s_4s), 4s_(4h_4h)/(8h_8h), 8h_(8b_8b)/(16b_16b)

   umov        w_b[], w_h[], x_b[], x_h[], x_s[]

   umull{2}    2d_2s/4s_s[]. 4s_4h/8h_h[]
   umull{2}    2d_(2s_2s)/(4s_4s), 4s_(4h_4h)/(8h_8h), 8h_(8b_8b)/(16b_16b)

   uqadd       d,s,h,b
   uqadd       2d,4s,2s,8h,4h,16b,8b

   uqrshl      d,s,h,b
   uqrshl      2d,4s,2s,8h,4h,16b,8b

   uqrshrn     s_d, h_s, b_h   #imm
   uqrshrn{2}  2s/4s_2d, 4h/8h_4s, 8b/16b_8h,  #imm

   uqshl        d,s,h,b   _#imm
   uqshl        2d,4s,2s,8h,4h,16b,8b   _#imm

   uqshl        d,s,h,b
   uqshl        2d,4s,2s,8h,4h,16b,8b

   uqshrn       s_d, h_s, b_h   #imm
   uqshrn{2}    2s/4s_2d, 4h/8h_4s, 8b/16b_8h,  #imm

   uqsub       d,s,h,b
   uqsub       2d,4s,2s,8h,4h,16b,8b

   uqxtn       s_d,h_s,b_h
   uqxtn{2}    2s/4s_2d, 4h/8h_4s, 8b/16b_8h

   urecpe      4s,2s

   urhadd      4s,2s,8h,4h,16b,8b

   urshl (reg) d
   urshl       2d,4s,2s,8h,4h,16b,8b

   urshr (imm) d
   urshr       2d,4s,2s,8h,4h,16b,8b

   ursqrte     4s,2s

   ursra (imm) d
   ursra       2d,4s,2s,8h,4h,16b,8b

   ushl (reg)  d
   ushl        2d,4s,2s,8h,4h,16b,8b

   ushll{2} (imm)  2d_2s/4s  4s_4h/8h, 8h_8b/16b

   ushr (imm)  d
   ushr        2d,4s,2s,8h,4h,16b,8b

   usqadd      d,s,h,b
   usqadd      2d,4s,2s,8h,4h,16b,8b

   usra (imm)  d
   usra        2d,4s,2s,8h,4h,16b,8b

   usubl{2}  2d_(2s_2s)/(4s_4s), 4s_(4h_4h)/(8h_8h), 8h_(8b_8b)/(16b_16b)

   usubw{2}  8h_8h_16b/8b, 4s_4s_8h/4h, 2d_2d_2s/4s

   uzp1      2d,4s,2s,8h,4h,16b,8b
   uzp2      2d,4s,2s,8h,4h,16b,8b

   xtn{2}    2s/4s_2d, 4h/8h_4s, 8b/16b_8h

   zip1      2d,4s,2s,8h,4h,16b,8b
   zip2      2d,4s,2s,8h,4h,16b,8b
*/


/* ---------------------------------------------------------------- */
/* -- List of insns, grouped somewhat by laneage configuration   -- */
/* ---------------------------------------------------------------- */
/*
   ======================== FP ========================

   fabs      d,s
   fabs      2d,4s,2s

   fneg      d,s
   fneg      2d,4s,2s

   fsqrt     d,s
   fsqrt     2d,4s,2s

   fadd      d,s
   fsub      d,s

   fadd      2d,4s,2s
   fsub      2d,4s,2s

   fabd      d,s
   fabd      2d,4s,2s

   faddp     d,s (floating add pair)
   faddp     2d,4s,2s

   fccmp     d,s (floating point conditional quiet compare)
   fccmpe    d,s (floating point conditional signaling compare)

   fcmeq     d,s
   fcmge     d,s
   fcmgt     d,s
   facgt     d,s  (floating abs compare GE)
   facge     d,s  (floating abs compare GE)

   fcmeq     2d,4s,2s
   fcmge     2d,4s,2s
   fcmgt     2d,4s,2s
   facge     2d,4s,2s
   facgt     2d,4s,2s

   fcmeq_z   d,s
   fcmge_z   d,s
   fcmgt_z   d,s
   fcmle_z   d,s
   fcmlt_z   d,s

   fcmeq_z   2d,4s,2s
   fcmge_z   2d,4s,2s
   fcmgt_z   2d,4s,2s
   fcmle_z   2d,4s,2s
   fcmlt_z   2d,4s,2s

   fcmp_z    d,s
   fcmpe_z   d,s
   fcmp      d,s (floating point quiet, set flags)
   fcmpe     d,s (floating point signaling, set flags)

   fcsel     d,s (fp cond select)

   fdiv      d,s
   fdiv      2d,4s,2s

   fmadd     d,s
   fnmadd    d,s
   fmsub     d,s
   fnmsub    d,s

   fnmul     d,s

   fmax      d,s
   fmin      d,s
   fmaxnm    d,s ("max number")
   fminnm    d,s

   fmax      2d,4s,2s
   fmin      2d,4s,2s
   fmaxnm    2d,4s,2s
   fminnm    2d,4s,2s

   fmaxnmp   d_2d,s_2s ("max number pairwise")
   fminnmp   d_2d,s_2s

   fmaxnmp   2d,4s,2s
   fminnmp   2d,4s,2s

   fmaxnmv   s_4s (maxnum across vector)
   fminnmv   s_4s

   fmaxp     d_2d,s_2s (max of a pair)
   fminp     d_2d,s_2s (max of a pair)

   fmaxp     2d,4s,2s  (max pairwise)
   fminp     2d,4s,2s

   fmaxv     s_4s (max across vector)
   fminv     s_4s

   fmla      2d,4s,2s
   fmls      2d,4s,2s

   fmla      d_d_d[],s_s_s[] (by element)
   fmls      d_d_d[],s_s_s[] (by element)

   fmla      2d_2d_d[],4s_4s_s[],2s_2s_s[]
   fmls      2d_2d_d[],4s_4s_s[],2s_2s_s[]

   fmov      2d,4s,2s #imm (part of the MOVI/MVNI/ORR/BIC imm group)

   fmov      d_d,s_s

   fmov      s_w,w_s,d_x,d[1]_x,x_d,x_d[1]

   fmov      d,s #imm

   fmul      d_d_d[],s_s_s[]
   fmul      2d_2d_d[],4s_4s_s[],2s_2s_s[]

   fmul      2d,4s,2s
   fmul      d,s

   fmulx     d_d_d[],s_s_s[]
   fmulx     2d_2d_d[],4s_4s_s[],2s_2s_s[]

   fmulx     d,s
   fmulx     2d,4s,2s

   frecpe    d,s (recip estimate)
   frecpe    2d,4s,2s

   frecps    d,s (recip step)
   frecps    2d,4s,2s

   frecpx    d,s (recip exponent)

   frinta    d,s
   frinti    d,s
   frintm    d,s
   frintn    d,s
   frintp    d,s
   frintx    d,s
   frintz    d,s

   frinta    2d,4s,2s (round to integral, nearest away)
   frinti    2d,4s,2s (round to integral, per FPCR)
   frintm    2d,4s,2s (round to integral, minus inf)
   frintn    2d,4s,2s (round to integral, nearest, to even)
   frintp    2d,4s,2s (round to integral, plus inf)
   frintx    2d,4s,2s (round to integral exact, per FPCR)
   frintz    2d,4s,2s (round to integral, zero)

   frsqrte   d,s (est)
   frsqrte   2d,4s,2s

   frsqrts   d,s (step)
   frsqrts   2d,4s,2s

   ======================== CONV ========================

   fcvt      s_h,d_h,h_s,d_s,h_d,s_d (fp convert, scalar)

   fcvtl{2}  4s/4h, 4s/8h, 2d/2s, 2d/4s (float convert to longer form)

   fcvtn{2}  4h/4s, 8h/4s, 2s/2d, 4s/2d (float convert to narrower form)

   fcvtas    d,s  (fcvt to signed int,   nearest, ties away)
   fcvtau    d,s  (fcvt to unsigned int, nearest, ties away)
   fcvtas    2d,4s,2s
   fcvtau    2d,4s,2s
   fcvtas    w_s,x_s,w_d,x_d
   fcvtau    w_s,x_s,w_d,x_d

   fcvtms    d,s  (fcvt to signed int,   minus inf)
   fcvtmu    d,s  (fcvt to unsigned int, minus inf)
   fcvtms    2d,4s,2s
   fcvtmu    2d,4s,2s
   fcvtms    w_s,x_s,w_d,x_d
   fcvtmu    w_s,x_s,w_d,x_d

   fcvtns    d,s  (fcvt to signed int,   nearest)
   fcvtnu    d,s  (fcvt to unsigned int, nearest)
   fcvtns    2d,4s,2s
   fcvtnu    2d,4s,2s
   fcvtns    w_s,x_s,w_d,x_d
   fcvtnu    w_s,x_s,w_d,x_d

   fcvtps    d,s  (fcvt to signed int,   plus inf)
   fcvtpu    d,s  (fcvt to unsigned int, plus inf)
   fcvtps    2d,4s,2s
   fcvtpu    2d,4s,2s
   fcvtps    w_s,x_s,w_d,x_d
   fcvtpu    w_s,x_s,w_d,x_d

   fcvtzs    d,s (fcvt to signed integer,   to zero)
   fcvtzu    d,s (fcvt to unsigned integer, to zero)
   fcvtzs    2d,4s,2s
   fcvtzu    2d,4s,2s
   fcvtzs    w_s,x_s,w_d,x_d
   fcvtzu    w_s,x_s,w_d,x_d

   fcvtzs    d,s (fcvt to signed fixedpt,   to zero) (w/ #fbits)
   fcvtzu    d,s (fcvt to unsigned fixedpt, to zero) (w/ #fbits)
   fcvtzs    2d,4s,2s
   fcvtzu    2d,4s,2s
   fcvtzs    w_s,x_s,w_d,x_d (fcvt to signed fixedpt,   to zero) (w/ #fbits)
   fcvtzu    w_s,x_s,w_d,x_d (fcvt to unsigned fixedpt, to zero) (w/ #fbits)

   fcvtxn    s_d (fcvt to lower prec narrow, rounding to odd)
   fcvtxn    2s_2d,4s_2d

   scvtf     d,s        _#fbits
   ucvtf     d,s        _#fbits

   scvtf     2d,4s,2s   _#fbits
   ucvtf     2d,4s,2s   _#fbits

   scvtf     d,s
   ucvtf     d,s

   scvtf     2d,4s,2s
   ucvtf     2d,4s,2s

   scvtf     s_w, d_w, s_x, d_x,   _#fbits
   ucvtf     s_w, d_w, s_x, d_x,   _#fbits

   scvtf     s_w, d_w, s_x, d_x
   ucvtf     s_w, d_w, s_x, d_x

   ======================== INT ========================

   abs       d
   neg       d

   abs       2d,4s,2s,8h,4h,16b,8b
   neg       2d,4s,2s,8h,4h,16b,8b

   add       d
   sub       d

   add       2d,4s,2s,8h,4h,16b,8b
   sub       2d,4s,2s,8h,4h,16b,8b

   addhn{2}   2s/4s_2d_2d, 4h/8h_4s_4s, 8b/16b_8h_8h
   subhn{2}   2s/4s_2d_2d, 4h/8h_4s_4s, 8b/16b_8h_8h
   raddhn{2}  2s/4s_2d_2d, 4h/8h_4s_4s, 8b/16b_8h_8h
   rsubhn{2}  2s/4s_2d_2d, 4h/8h_4s_4s, 8b/16b_8h_8h

   addp     d (add pairs, across)
   addp     2d,4s,2s,8h,4h,16b,8b
   addv     4s,8h,4h,16b,18b (reduce across vector)

   and      16b,8b

   orr      8h,4h   #imm8, LSL #0 or 8
   orr      4s,2s   #imm8, LSL #0, 8, 16 or 24
   bic      8h,4h   #imm8, LSL #0 or 8
   bic      4s,2s   #imm8, LSL #0, 8, 16 or 24
   also movi, mvni

   bic      16b,8b (vector,reg) (bit clear)
   bif      16b,8b (vector) (bit insert if false)
   bit      16b,8b (vector) (bit insert if true)
   bsl      16b,8b (vector) (bit select)

   cls      4s,2s,8h,4h,16b,8b (count leading sign bits)
   clz      4s,2s,8h,4h,16b,8b (count leading zero bits)

   cmeq     d
   cmge     d
   cmgt     d
   cmhi     d
   cmhs     d
   cmtst    d

   cmeq     2d,4s,2s,8h,4h,16b,8b
   cmge     2d,4s,2s,8h,4h,16b,8b
   cmgt     2d,4s,2s,8h,4h,16b,8b
   cmhi     2d,4s,2s,8h,4h,16b,8b
   cmhs     2d,4s,2s,8h,4h,16b,8b
   cmtst    2d,4s,2s,8h,4h,16b,8b

   cmeq_z   d
   cmge_z   d
   cmgt_z   d
   cmle_z   d
   cmlt_z   d

   cmeq_z   2d,4s,2s,8h,4h,16b,8b
   cmge_z   2d,4s,2s,8h,4h,16b,8b
   cmgt_z   2d,4s,2s,8h,4h,16b,8b
   cmle_z   2d,4s,2s,8h,4h,16b,8b
   cmlt_z   2d,4s,2s,8h,4h,16b,8b

   cnt      16b,8b (population count per byte)

   dup      d,s,h,b (vec elem to scalar)
   dup      2d,4s,2s,8h,4h,16b,8b (vec elem to vector)
   dup      2d,4s,2s,8h,4h,16b,8b (general reg to vector)

   eor      16b,8b (vector)
   ext      16b,8b,#imm4 (concat 2 vectors, then slice)

   ins      d[]_d[],s[]_s[],h[]_h[],b[]_b[]

   ins      d[]_x, s[]_w, h[]_w, b[]_w

   mla   4s_4s_s[],2s_2s_s[],8h_8h_h[],4h_4h_h[]
   mla   4s,2s,8h,4h,16b,8b

   mls   4s_4s_s[],2s_2s_s[],8h_8h_h[],4h_4h_h[]
   mls   4s,2s,8h,4h,16b,8b

   movi  16b,8b   #imm8, LSL #0
   movi  8h,4h    #imm8, LSL #0 or 8
   movi  4s,2s    #imm8, LSL #0, 8, 16, 24
   movi  4s,2s    #imm8, MSL #8 or 16
   movi  d,       #imm64
   movi  2d,      #imm64

   mul   4s_4s_s[],2s_2s_s[],8h_8h_h[],4h_4h_h[]
   mul   4s,2s,8h,4h,16b,8b

   mvni  8h,4h    #imm8, LSL #0 or 8
   mvni  4s,2s    #imm8, LSL #0, 8, 16, 24
   mvni  4s,2s    #imm8, MSL #8 or 16

   not   16b,8b

   orn   16b,8b
   orr   16b,8b

   pmul  16b,8b

   pmull{2}  8h_8b_8b,8h_16b_16b,1q_1d_1d,1d_2d_2d

   rbit    16b,8b
   rev16   16b,8b
   rev32   16b,8b,8h,4h
   rev64   16b,8b,8h,4h,4s,2s

   saba      16b,8b,8h,4h,4s,2s
   uaba      16b,8b,8h,4h,4s,2s

   sabal{2}  2d_(2s_2s)/(4s_4s), 4s_(4h_4h)/(8h_8h), 8h_(8b_8b)/(16b_16b)
   uabal{2}  2d_(2s_2s)/(4s_4s), 4s_(4h_4h)/(8h_8h), 8h_(8b_8b)/(16b_16b)

   sabd      16b,8b,8h,4h,4s,2s
   uabd      16b,8b,8h,4h,4s,2s

   sabdl{2}  2d_(2s_2s)/(4s_4s), 4s_(4h_4h)/(8h_8h), 8h_(8b_8b)/(16b_16b)
   uabdl{2}  2d_(2s_2s)/(4s_4s), 4s_(4h_4h)/(8h_8h), 8h_(8b_8b)/(16b_16b)

   sadalp    4h_8b,8h_16b,2s_4h,4s_8h,1d_2s,2d_4s
   uadalp    4h_8b,8h_16b,2s_4h,4s_8h,1d_2s,2d_4s

   saddl{2}  2d_(2s_2s)/(4s_4s), 4s_(4h_4h)/(8h_8h), 8h_(8b_8b)/(16b_16b)
   uaddl{2}  2d_(2s_2s)/(4s_4s), 4s_(4h_4h)/(8h_8h), 8h_(8b_8b)/(16b_16b)
   ssubl{2}  2d_(2s_2s)/(4s_4s), 4s_(4h_4h)/(8h_8h), 8h_(8b_8b)/(16b_16b)
   usubl{2}  2d_(2s_2s)/(4s_4s), 4s_(4h_4h)/(8h_8h), 8h_(8b_8b)/(16b_16b)

   saddlp    4h_8b,8h_16b,2s_4h,4s_8h,1d_2s,2d_4s
   uaddlp    4h_8b,8h_16b,2s_4h,4s_8h,1d_2s,2d_4s

   saddlv    h_16b/8b, s_8h/4h, d_4s
   uaddlv    h_16b/8b, s_8h/4h, d_4s

   saddw{2}  8h_8h_16b/8b, 4s_4s_8h/4h, 2d_2d_2s/4s
   uaddw{2}  8h_8h_16b/8b, 4s_4s_8h/4h, 2d_2d_2s/4s
   ssubw{2}  8h_8h_16b/8b, 4s_4s_8h/4h, 2d_2d_2s/4s
   usubw{2}  8h_8h_16b/8b, 4s_4s_8h/4h, 2d_2d_2s/4s

   shadd        16b,8b,8h,4h,4s,2s
   uhadd        16b,8b,8h,4h,4s,2s
   shsub        16b,8b,8h,4h,4s,2s
   uhsub        16b,8b,8h,4h,4s,2s

   shl          d_#imm
   shl          16b,8b,8h,4h,4s,2s,2d  _#imm

   shll{2}      8h_8b/16b_#8, 4s_4h/8h_#16, 2d_2s/4s_#32

   shrn{2}      2s/4s_2d, 8h/4h_4s, 2s/4s_2d,   #imm in 1 .. elem_bits
   rshrn{2}     2s/4s_2d, 8h/4h_4s, 2s/4s_2d,   #imm in 1 .. elem_bits

   sli          d_#imm
   sri          d_#imm

   sli          2d,4s,2s,8h,4h,16b,8b  _#imm
   sri          2d,4s,2s,8h,4h,16b,8b  _#imm

   smax         4s,2s,8h,4h,16b,8b
   umax         4s,2s,8h,4h,16b,8b
   smin         4s,2s,8h,4h,16b,8b
   umin         4s,2s,8h,4h,16b,8b

   smaxp        4s,2s,8h,4h,16b,8b
   umaxp        4s,2s,8h,4h,16b,8b
   sminp        4s,2s,8h,4h,16b,8b
   uminp        4s,2s,8h,4h,16b,8b

   smaxv        s_4s,h_8h,h_4h,b_16b,b_8b
   umaxv        s_4s,h_8h,h_4h,b_16b,b_8b
   sminv        s_4s,h_8h,h_4h,b_16b,b_8b
   uminv        s_4s,h_8h,h_4h,b_16b,b_8b

   smlal{2}     2d_2s/4s_s[], 4s_4h/8h_h[]
   umlal{2}     2d_2s/4s_s[], 4s_4h/8h_h[]
   smlsl{2}     2d_2s/4s_s[], 4s_4h/8h_h[]
   umlsl{2}     2d_2s/4s_s[], 4s_4h/8h_h[]
   smull{2}     2d_2s/4s_s[]. 4s_4h/8h_h[]
   umull{2}     2d_2s/4s_s[]. 4s_4h/8h_h[]

   smlal{2}     2d_(2s_2s)/(4s_4s), 4s_(4h_4h)/(8h_8h), 8h_(8b_8b)/(16b_16b)
   umlal{2}     2d_(2s_2s)/(4s_4s), 4s_(4h_4h)/(8h_8h), 8h_(8b_8b)/(16b_16b)
   smlsl{2}     2d_(2s_2s)/(4s_4s), 4s_(4h_4h)/(8h_8h), 8h_(8b_8b)/(16b_16b)
   umlsl{2}     2d_(2s_2s)/(4s_4s), 4s_(4h_4h)/(8h_8h), 8h_(8b_8b)/(16b_16b)
   smull{2}     2d_(2s_2s)/(4s_4s), 4s_(4h_4h)/(8h_8h), 8h_(8b_8b)/(16b_16b)
   umull{2}     2d_(2s_2s)/(4s_4s), 4s_(4h_4h)/(8h_8h), 8h_(8b_8b)/(16b_16b)

   smov         w_b[], w_h[], x_b[], x_h[], x_s[]
   umov         w_b[], w_h[], x_b[], x_h[], x_s[]

   sqabs        d,s,h,b
   sqneg        d,s,h,b

   sqabs        2d,4s,2s,8h,4h,16b,8b
   sqneg        2d,4s,2s,8h,4h,16b,8b

   sqadd        d,s,h,b
   uqadd        d,s,h,b
   sqsub        d,s,h,b
   uqsub        d,s,h,b

   sqadd        2d,4s,2s,8h,4h,16b,8b
   uqadd        2d,4s,2s,8h,4h,16b,8b
   sqsub        2d,4s,2s,8h,4h,16b,8b
   uqsub        2d,4s,2s,8h,4h,16b,8b

   sqdmlal      d_s_s[], s_h_h[]
   sqdmlsl      d_s_s[], s_h_h[]
   sqdmull      d_s_s[], s_h_h[]

   sqdmlal{2}   2d_2s/4s_s[], 4s_4h/8h_h[]
   sqdmlsl{2}   2d_2s/4s_s[], 4s_4h/8h_h[]
   sqdmull{2}   2d_2s/4s_s[], 4s_4h/2h_h[]

   sqdmlal      d_s_s, s_h_h
   sqdmlsl      d_s_s, s_h_h
   sqdmull      d_s_s, s_h_h

   sqdmlal{2}   2d_(2s_2s)/(4s_4s), 4s_(4h_4h)/(8h_8h)
   sqdmlsl{2}   2d_(2s_2s)/(4s_4s), 4s_(4h_4h)/(8h_8h)
   sqdmull{2}   2d_(2s_2s)/(4s_4s), 4s_(4h_4h)/(8h_8h)

   sqdmulh      s_s_s[], h_h_h[]
   sqrdmulh     s_s_s[], h_h_h[]

   sqdmulh      4s_4s_s[], 2s_2s_s[], 8h_8h_h[], 4h_4h_h[]
   sqrdmulh     4s_4s_s[], 2s_2s_s[], 8h_8h_h[], 4h_4h_h[]

   sqdmulh      h,s
   sqrdmulh     h,s

   sqdmulh      4s,2s,8h,4h
   sqrdmulh     4s,2s,8h,4h

   sqshl        d,s,h,b
   uqshl        d,s,h,b
   sqrshl       d,s,h,b
   uqrshl       d,s,h,b

   sqshl        2d,4s,2s,8h,4h,16b,8b
   uqshl        2d,4s,2s,8h,4h,16b,8b
   sqrshl       2d,4s,2s,8h,4h,16b,8b
   uqrshl       2d,4s,2s,8h,4h,16b,8b

   sqrshrn      s_d, h_s, b_h   #imm
   uqrshrn      s_d, h_s, b_h   #imm
   sqshrn       s_d, h_s, b_h   #imm
   uqshrn       s_d, h_s, b_h   #imm

   sqrshrun     s_d, h_s, b_h   #imm
   sqshrun      s_d, h_s, b_h   #imm

   sqrshrn{2}   2s/4s_2d, 4h/8h_4s, 8b/16b_8h,  #imm
   uqrshrn{2}   2s/4s_2d, 4h/8h_4s, 8b/16b_8h,  #imm
   sqshrn{2}    2s/4s_2d, 4h/8h_4s, 8b/16b_8h,  #imm
   uqshrn{2}    2s/4s_2d, 4h/8h_4s, 8b/16b_8h,  #imm

   sqrshrun{2}  2s/4s_2d, 4h/8h_4s, 8b/16b_8h,  #imm
   sqshrun{2}   2s/4s_2d, 4h/8h_4s, 8b/16b_8h,  #imm

   sqshl        d,s,h,b   _#imm
   uqshl        d,s,h,b   _#imm
   sqshlu       d,s,h,b   _#imm

   sqshl        2d,4s,2s,8h,4h,16b,8b   _#imm
   uqshl        2d,4s,2s,8h,4h,16b,8b   _#imm
   sqshlu       2d,4s,2s,8h,4h,16b,8b   _#imm

   sqxtn        s_d,h_s,b_h
   uqxtn        s_d,h_s,b_h
   sqxtun       s_d,h_s,b_h

   sqxtn{2}     2s/4s_2d, 4h/8h_4s, 8b/16b_8h
   uqxtn{2}     2s/4s_2d, 4h/8h_4s, 8b/16b_8h
   sqxtun{2}    2s/4s_2d, 4h/8h_4s, 8b/16b_8h

   srhadd       4s,2s,8h,4h,16b,8b
   urhadd       4s,2s,8h,4h,16b,8b

   sshl (reg)   d
   ushl (reg)   d
   sshr (imm)   d
   ushr (imm)   d
   ssra (imm)   d
   usra (imm)   d

   srshl (reg)  d
   urshl (reg)  d
   srshr (imm)  d
   urshr (imm)  d
   srsra (imm)  d
   ursra (imm)  d

   sshl         2d,4s,2s,8h,4h,16b,8b
   ushl         2d,4s,2s,8h,4h,16b,8b
   sshr         2d,4s,2s,8h,4h,16b,8b
   ushr         2d,4s,2s,8h,4h,16b,8b
   ssra         2d,4s,2s,8h,4h,16b,8b
   usra         2d,4s,2s,8h,4h,16b,8b

   srshl        2d,4s,2s,8h,4h,16b,8b
   urshl        2d,4s,2s,8h,4h,16b,8b
   srshr        2d,4s,2s,8h,4h,16b,8b
   urshr        2d,4s,2s,8h,4h,16b,8b
   srsra        2d,4s,2s,8h,4h,16b,8b
   ursra        2d,4s,2s,8h,4h,16b,8b

   sshll{2} (imm)  2d_2s/4s  4s_4h/8h, 8h_8b/16b
   ushll{2} (imm)  2d_2s/4s  4s_4h/8h, 8h_8b/16b

   suqadd  d,s,h,b
   suqadd  2d,4s,2s,8h,4h,16b,8b

   tbl     8b_{16b}_8b, 16b_{16b}_16b
   tbl     8b_{16b,16b}_8b, 16b_{16b,16b}_16b
   tbl     8b_{16b,16b,16b}_8b, 16b_{16b,16b,16b}_16b
   tbl     8b_{16b,16b,16b,16b}_8b, 16b_{16b,16b,16b,16b}_16b

   tbx     8b_{16b}_8b, 16b_{16b}_16b
   tbx     8b_{16b,16b}_8b, 16b_{16b,16b}_16b
   tbx     8b_{16b,16b,16b}_8b, 16b_{16b,16b,16b}_16b
   tbx     8b_{16b,16b,16b,16b}_8b, 16b_{16b,16b,16b,16b}_16b

   trn1    2d,4s,2s,8h,4h,16b,8b
   trn2    2d,4s,2s,8h,4h,16b,8b

   urecpe      4s,2s

   ursqrte     4s,2s

   usqadd      d,s,h,b
   usqadd      2d,4s,2s,8h,4h,16b,8b

   uzp1      2d,4s,2s,8h,4h,16b,8b
   uzp2      2d,4s,2s,8h,4h,16b,8b

   xtn{2}    2s/4s_2d, 4h/8h_4s, 8b/16b_8h

   zip1      2d,4s,2s,8h,4h,16b,8b
   zip2      2d,4s,2s,8h,4h,16b,8b

   ======================== MEM ========================

   ld1  (multiple 1-element structures to 1/2/3/4 regs)
   ld1  (single 1-element structure to one lane of 1 reg)
   ld1r (single 1-element structure and rep to all lanes of 1 reg)

   ld2  (multiple 2-element structures to 2 regs)
   ld2  (single 2-element structure to one lane of 2 regs)
   ld2r (single 2-element structure and rep to all lanes of 2 regs)

   ld3  (multiple 3-element structures to 3 regs)
   ld3  (single 3-element structure to one lane of 3 regs)
   ld3r (single 3-element structure and rep to all lanes of 3 regs)

   ld4  (multiple 4-element structures to 4 regs)
   ld4  (single 4-element structure to one lane of 4 regs)
   ld4r (single 4-element structure and rep to all lanes of 4 regs)

   ldnp  q_q_addr,d_d_addr,s_s_addr  (load pair w/ non-temporal hint)
         addr = reg + uimm7 * reg_size

   ldp   q_q_addr,d_d_addr,s_s_addr  (load pair)
         addr = [Xn|SP],#imm   or [Xn|SP,#imm]!  or [Xn|SP,#imm]

   ldr   q,d,s,h,b from addr
         addr = [Xn|SP],#imm   or [Xn|SP,#imm]!  or [Xn|SP,#imm]

   ldr   q,d,s from  pc+#imm19

   ldr   q,d,s,h,b from addr
         addr = [Xn|SP, R <extend> <shift]

   ldur  q,d,s,h,b from addr
         addr = [Xn|SP,#imm] (unscaled offset)

   st1 (multiple 1-element structures from 1/2/3/4 regs)
   st1 (single 1-element structure for 1 lane of 1 reg)

   st2 (multiple 2-element structures from 2 regs)
   st2 (single 2-element structure from 1 lane of 2 regs)

   st3 (multiple 3-element structures from 3 regs)
   st3 (single 3-element structure from 1 lane of 3 regs)

   st4 (multiple 4-element structures from 4 regs)
   st4 (single 4-element structure from one lane of 4 regs)

   stnp q_q_addr, d_d_addr, s_s_addr
        addr = [Xn|SP, #imm]

   stp  q_q_addr, d_d_addr, s_s_addr
        addr = [Xn|SP], #imm  or [Xn|SP, #imm]!  or [Xn|SP, #imm]

   str  q,d,s,h,b_addr
        addr = [Xn|SP], #simm  or [Xn|SP, #simm]!  or [Xn|SP, #pimm]

   str   q,d,s,h,b_addr
         addr = [Xn|SP, R <extend> <shift]

   stur  q,d,s,h,b_addr
         addr = [Xn|SP,#imm] (unscaled offset)

   ======================== CRYPTO ========================

   aesd       16b (aes single round decryption)
   aese       16b (aes single round encryption)
   aesimc     16b (aes inverse mix columns)
   aesmc      16b (aes mix columns)

   sha1c      q_s_4s
   sha1h      s_s
   sha1m      q_s_4s
   sha1p      q_s_4s
   sha1su0    4s_4s_4s
   sha1su1    4s_4s

   sha256h2   q_q_4s
   sha256h    q_q_4s
   sha256su0  4s_4s
   sha256su1  4s_4s_4s
*/
