
/*
gcc -o v8crypto v8crypto.c -march=armv8-a -mfpu=crypto-neon-fp-armv8
gcc -o v8crypto v8crypto.c -mfpu=crypto-neon-fp-armv8
*/

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

typedef  unsigned char           Bool;
#define False ((Bool)0)
#define True  ((Bool)1)


#define ITERS 1

typedef
  enum { TyHF=1234, TySF, TyDF, TyB, TyH, TyS, TyD, TyNONE }
  LaneTy;

union _V128 {
   UChar  u8[16];
   UShort u16[8];
   UInt   u32[4];
   ULong  u64[2];
   Float  f32[4];
   Double f64[2];
};
typedef  union _V128   V128;

static inline UChar randUChar ( void )
{
   static UInt seed = 80021;
   seed = 1103515245 * seed + 12345;
   return (seed >> 17) & 0xFF;
}

//static ULong randULong ( LaneTy ty )
//{
//   Int i;
//   ULong r = 0;
//   for (i = 0; i < 8; i++) {
//      r = (r << 8) | (ULong)(0xFF & randUChar());
//   }
//   return r;
//}

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

//static void showBlock ( const char* msg, V128* block, Int nBlock )
//{
//   Int i;
//   printf("%s\n", msg);
//   for (i = 0; i < nBlock; i++) {
//      printf("  ");
//      showV128(&block[i]);
//      printf("\n");
//   }
//}


/* ---------------------------------------------------------------- */
/* -- Parameterisable test macros                                -- */
/* ---------------------------------------------------------------- */

#define DO50(_action) \
   do { \
      Int _qq; for (_qq = 0; _qq < 50; _qq++) { _action ; } \
   } while (0)


/* Generate a test that involves two vector regs,
   with no bias as towards which is input or output. 
   It's OK to use r8 as scratch.*/
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
           "mov r9, #0 ; vmsr fpscr, r9 ; " \
           "add r9, %0, #0  ; vld1.8 { q"#VECREG1NO" }, [r9] ; " \
           "add r9, %0, #16 ; vld1.8 { q"#VECREG2NO" }, [r9] ; " \
           INSN " ; " \
           "add r9, %0, #32 ; vst1.8 { q"#VECREG1NO" }, [r9] ; " \
           "add r9, %0, #48 ; vst1.8 { q"#VECREG2NO" }, [r9] ; " \
           "vmrs r9, fpscr ; str r9, [%0, #64] " \
           : : "r"(&block[0]) \
             : "cc", "memory", "q"#VECREG1NO, "q"#VECREG2NO, "r8", "r9" \
        ); \
        printf(INSN   "   "); \
        UInt fpscr = 0xFFFFFFFF & block[4].u32[0]; \
        showV128(&block[0]); printf("  "); \
        showV128(&block[1]); printf("  "); \
        showV128(&block[2]); printf("  "); \
        showV128(&block[3]); printf(" fpscr=%08x\n", fpscr); \
     } \
  }


/* Generate a test that involves three vector regs,
   with no bias as towards which is input or output.  It's also OK
   to use r8 scratch. */
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
           "mov r9, #0 ; vmsr fpscr, r9 ; " \
           "add r9, %0, #0  ; vld1.8 { q"#VECREG1NO" }, [r9] ; " \
           "add r9, %0, #16 ; vld1.8 { q"#VECREG2NO" }, [r9] ; " \
           "add r9, %0, #32 ; vld1.8 { q"#VECREG3NO" }, [r9] ; " \
           INSN " ; " \
           "add r9, %0, #48 ; vst1.8 { q"#VECREG1NO" }, [r9] ; " \
           "add r9, %0, #64 ; vst1.8 { q"#VECREG2NO" }, [r9] ; " \
           "add r9, %0, #80 ; vst1.8 { q"#VECREG3NO" }, [r9] ; " \
           "vmrs r9, fpscr ; str r9, [%0, #96] " \
           : : "r"(&block[0]) \
           : "cc", "memory", "q"#VECREG1NO, "q"#VECREG2NO, "q"#VECREG3NO, \
             "r8", "r9" \
        ); \
        printf(INSN   "   "); \
        UInt fpscr = 0xFFFFFFFF & block[6].u32[0]; \
        showV128(&block[0]); printf("  "); \
        showV128(&block[1]); printf("  "); \
        showV128(&block[2]); printf("  "); \
        showV128(&block[3]); printf("  "); \
        showV128(&block[4]); printf("  "); \
        showV128(&block[5]); printf(" fpscr=%08x\n", fpscr); \
     } \
  }

// ======================== CRYPTO ========================

GEN_TWOVEC_TEST(aesd_q_q,   "aesd.8 q3, q4",     3,  4)
GEN_TWOVEC_TEST(aese_q_q,   "aese.8 q12, q13",  12, 13)
GEN_TWOVEC_TEST(aesimc_q_q, "aesimc.8 q15, q0", 15,  0)
GEN_TWOVEC_TEST(aesmc_q_q,  "aesmc.8 q1, q9",    1,  9)

GEN_THREEVEC_TEST(sha1c_q_q_q,   "sha1c.32 q11, q10, q2",   11, 10, 2)
GEN_TWOVEC_TEST(sha1h_q_q,       "sha1h.32 q6, q7",         6, 7)
GEN_THREEVEC_TEST(sha1m_q_q_q,   "sha1m.32 q2, q8, q13",    2, 8, 13)
GEN_THREEVEC_TEST(sha1p_q_q_q,   "sha1p.32 q3, q9, q14",    3, 9, 14)
GEN_THREEVEC_TEST(sha1su0_q_q_q, "sha1su0.32 q4, q10, q15", 4, 10, 15)
GEN_TWOVEC_TEST(sha1su1_q_q,     "sha1su1.32 q11, q2",      11, 2)

GEN_THREEVEC_TEST(sha256h2_q_q_q,  "sha256h2.32 q9, q8, q7",     9, 8, 7)
GEN_THREEVEC_TEST(sha256h_q_q_q,   "sha256h.32 q10, q9, q8",     10, 9, 8)
GEN_TWOVEC_TEST(sha256su0_q_q,     "sha256su0.32 q11, q10",      11, 10)
GEN_THREEVEC_TEST(sha256su1_q_q_q, "sha256su1.32 q12, q11, q10", 12, 11, 10)

// This is a bit complex.  This really mentions three registers, so it
// should really be a THREEVEC variant.  But the two source registers
// are D registers.  So we say it is just a TWOVEC insn, producing a Q
// and taking a single Q (q7); q7 is the d14-d15 register pair, which
// is why the insn itself is mentions d14 and d15 whereas the
// numbers that follow mention q7.  The result (q7) is 128 bits wide and
// so is unaffected by these shenanigans.
GEN_TWOVEC_TEST(pmull_q_d_d,  "vmull.p64 q13, d14, d15", 13, 7)

int main ( void )
{
   // ======================== CRYPTO ========================

   // aesd.8     q_q (aes single round decryption)
   // aese.8     q_q (aes single round encryption)
   // aesimc.8   q_q (aes inverse mix columns)
   // aesmc.8    q_q (aes mix columns)
   if (1) DO50( test_aesd_q_q(TyNONE) );
   if (1) DO50( test_aese_q_q(TyNONE) );
   if (1) DO50( test_aesimc_q_q(TyNONE) );
   if (1) DO50( test_aesmc_q_q(TyNONE) );

   // sha1c.32   q_q_q
   // sha1h.32   q_q
   // sha1m.32   q_q_q
   // sha1p.32   q_q_q
   // sha1su0.32 q_q_q
   // sha1su1.32 q_q
   if (1) DO50( test_sha1c_q_q_q(TyNONE) );
   if (1) DO50( test_sha1h_q_q(TyNONE) );
   if (1) DO50( test_sha1m_q_q_q(TyNONE) );
   if (1) DO50( test_sha1p_q_q_q(TyNONE) );
   if (1) DO50( test_sha1su0_q_q_q(TyNONE) );
   if (1) DO50( test_sha1su1_q_q(TyNONE) );

   // sha256h2.32  q_q_q
   // sha256h.32   q_q_q
   // sha256su0.32 q_q
   // sha256su1.32 q_q_q
   if (1) DO50( test_sha256h2_q_q_q(TyNONE) );
   if (1) DO50( test_sha256h_q_q_q(TyNONE) );
   if (1) DO50( test_sha256su0_q_q(TyNONE) );
   if (1) DO50( test_sha256su1_q_q_q(TyNONE) );

   // vmull.64  q_d_d
   if (1) DO50( test_pmull_q_d_d(TyD) );

   return 0;
}
