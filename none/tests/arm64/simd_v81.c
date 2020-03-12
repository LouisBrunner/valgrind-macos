#include <stdio.h>
#include <assert.h>
#include <string.h>
#include <math.h>

typedef  unsigned char           UChar;
typedef  unsigned short int      UShort;
typedef  unsigned int            UInt;
typedef  signed int              Int;
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

static void setV128( V128* v, int elements, LaneTy ty, ULong val )
{
   assert( (elements % 2) == 0 && elements >= 2 && elements <= 16 );
   memset(v, 0, sizeof(*v));
   switch (ty) {
      /* 4H or 8H */
      case TyH:
         assert( val < (1ULL << 16) );
         assert( elements == 4 || elements == 8 );
         for ( int i = 0; i < elements; i++ )
            v->u16[i] = val;
         break;
      /* 2S or 4S */
      case TyS:
         assert( val < (1ULL << 32) );
         assert( elements == 2 || elements == 4 );
         for ( int i = 0; i < elements; i++ )
            v->u32[i] = val;
         break;
      default:
         printf("8B, 2D and 16B not implemented\n");
   }
}

static void setV128_idx( V128* v, int elements, LaneTy ty, ULong val, int idx )
{
   assert( (elements % 2) == 0 && elements >= 2 && elements <= 16 );
   assert( idx >= 0 );
   memset(v, 0, sizeof(*v));
   switch (ty) {
      /* 4H or 8H */
      case TyH:
         assert( val < (1ULL << 16) );
         assert( elements == 4 || elements == 8 );
         assert( idx < elements );
         v->u16[idx] = val;
         break;
      /* 2S or 4S */
      case TyS:
         assert( val < (1ULL << 32) );
         assert( elements == 2 || elements == 4 );
         assert( idx < elements );
         v->u32[idx] = val;
         break;
      default:
         printf("8B, 2D and 16B not implemented\n");
   }
}

static inline UChar randUChar ( void )
{
   static UInt seed = 80021;
   seed = 1103515245 * seed + 12345;
   return (seed >> 17) & 0xFF;
}

/* Generates a random V128. Ensures that that it contains normalised FP numbers
 * when viewed as either F32x4 or F64x2, so that it is reasonable to use in FP
 * test cases.
 */
static void randV128 ( /*OUT*/V128* v )
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

/* Generate a test function that involves three vector regs with no bias as
 * towards which is input or output.
 */
#define GEN_THREEVEC_TEST_RND(TESTNAME,INSN,VECREG1NO,VECREG2NO,VECREG3NO)  \
  __attribute__((noinline)) \
  static void test_##TESTNAME ( LaneTy ty ) { \
     Int i; \
     for (i = 0; i < ITERS; i++) { \
        V128 block[6+1]; \
        memset(block, 0x55, sizeof(block)); \
        randV128(&block[0]); \
        randV128(&block[1]); \
        randV128(&block[2]); \
        randV128(&block[3]); \
        randV128(&block[4]); \
        randV128(&block[5]); \
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
           : "memory", "v"#VECREG1NO, "v"#VECREG2NO, "v"#VECREG3NO, "x30" \
        ); \
        printf("%-34s", INSN); \
        UInt fpsr = 0xFFFFFF60 & block[6].u32[0]; \
        printf("vout:"); showV128(&block[0]); printf("  "); \
        printf("vin0:"); showV128(&block[1]); printf("  "); \
        printf("vin1:"); showV128(&block[2]); \
        printf("\n                                  "); \
        printf("vout:"); showV128(&block[3]); printf("  "); \
        printf("vin0:"); showV128(&block[4]); printf("  "); \
        printf("vin1:"); showV128(&block[5]); printf(" fpsr=%08x\n", fpsr); \
     } \
  }

/* Generate a test function that involves three vector regs with no bias as
 * towards which is input or output. The three vector regs are loaded with user
 * defined values rather than generated using randV128().
 */
#define GEN_THREEVEC_TEST(TESTNAME,INSN,VECREGOUT,VECREGIN0,VECREGIN1) \
  __attribute__((noinline)) \
  static void test_##TESTNAME ( V128* vout, V128* vin0, V128* vin1, LaneTy ty ) { \
     V128 block[6+1]; \
     memset(block, 0, sizeof(block)); \
     block[0] = *vout; \
     block[1] = *vin0; \
     block[2] = *vin1; \
     __asm__ __volatile__( \
        "mov   x30, #0 ; msr fpsr, x30 ; " \
        "ldr   q"#VECREGOUT", [%0, #0]  ; " \
        "ldr   q"#VECREGIN0", [%0, #16] ; " \
        "ldr   q"#VECREGIN1", [%0, #32] ; " \
        INSN " ; " \
        "str   q"#VECREGOUT", [%0, #48] ; " \
        "str   q"#VECREGIN0", [%0, #64] ; " \
        "str   q"#VECREGIN1", [%0, #80] ; " \
        "mrs   x30, fpsr ; str x30, [%0, #96] " \
        : : "r"(&block[0]) \
        : "memory", "v"#VECREGOUT, "v"#VECREGIN0, "v"#VECREGIN1, "x30" \
     ); \
     printf("%-34s", INSN); \
     UInt fpsr = 0xFFFFFF60 & block[6].u32[0]; \
     printf("vout:"); showV128(&block[0]); printf("  "); \
     printf("vin0:"); showV128(&block[1]); printf("  "); \
     printf("vin1:"); showV128(&block[2]); \
     printf("\n                                  "); \
     printf("vout:"); showV128(&block[3]); printf("  "); \
     printf("vin0:"); showV128(&block[4]); printf("  "); \
     printf("vin1:"); showV128(&block[5]); printf(" fpsr=%08x\n", fpsr); \
  }

/* Generate calls to test functions generated by GEN_THREEVEC_TEST defined
 * above, which require user defined data.
 */
#define GEN_THREEVEC_TEST_CALLS(INSN,ECOUNT,ETYPE,EARRANGE,PATTERN) \
   setV128(&vout, ECOUNT, ETYPE, PATTERN); \
   setV128(&vin[0], ECOUNT, ETYPE, PATTERN); \
   setV128(&vin[1], ECOUNT, ETYPE, PATTERN); \
   test_##INSN##_##EARRANGE##_##EARRANGE##_##EARRANGE##_##PATTERN(&vout, &vin[0], &vin[1], ETYPE);

/* Indexed vector element variant of GEN_THREEVEC_TEST_CALLS above. */
#define GEN_THREEVEC_TEST_CALLSI(INSN,ECOUNT,ETYPE,EARRANGE,PATTERN,IDX) \
   setV128_idx(&vout, ECOUNT, ETYPE, PATTERN, IDX); \
   setV128_idx(&vin[0], ECOUNT, ETYPE, PATTERN, IDX); \
   setV128_idx(&vin[1], ECOUNT, ETYPE, PATTERN, IDX); \
   test_##INSN##_##EARRANGE##_##EARRANGE##_##EARRANGE##_##PATTERN(&vout, &vin[0], &vin[1], ETYPE);

/* Test patterns. */
#define ALL5s_32 0x55555555ULL
#define ALLas_32 0xAAAAAAAAULL
#define ALLfs_32 0xFFFFFFFFULL
#define UP_32    0x01234567ULL
#define DOWN_32  0xFEDCBA98ULL
#define PI_32    0x31415926ULL
#define E_32     0x27182818ULL

#define ALL5s_16 0x5555ULL
#define ALLas_16 0xAAAAULL
#define ALLfs_16 0xFFFFULL
#define UP_16    0x0123ULL
#define DOWN_16  0xFEDCULL
#define PI_16    0x3141ULL
#define E_16     0x2718ULL


/* ---------------------------------------------------------
 * -- Tests, in the same order that they appear in main() --
 * ---------------------------------------------------------
 *
 * There are 4 types of test sets for each instruction:
 * - vector
 * - indexed vector
 * - scalar
 * - indexed scalar
 */

/* sqrdmlah (vector version, no index) */
GEN_THREEVEC_TEST_RND(sqrdmlah_4h_4h_4h, "sqrdmlah v0.4h, v1.4h, v2.4h", 0, 1, 2)
GEN_THREEVEC_TEST_RND(sqrdmlah_8h_8h_8h, "sqrdmlah v0.8h, v1.8h, v2.8h", 0, 1, 2)
GEN_THREEVEC_TEST_RND(sqrdmlah_2s_2s_2s, "sqrdmlah v0.2s, v1.2s, v2.2s", 0, 1, 2)
GEN_THREEVEC_TEST_RND(sqrdmlah_4s_4s_4s, "sqrdmlah v0.4s, v1.4s, v2.4s", 0, 1, 2)

GEN_THREEVEC_TEST(sqrdmlah_4h_4h_4h_0, "sqrdmlah v0.4h, v1.4h, v2.4h", 0, 1, 2)
GEN_THREEVEC_TEST(sqrdmlah_4h_4h_4h_ALL5s_16, "sqrdmlah v0.4h, v1.4h, v2.4h", 0, 1, 2)
GEN_THREEVEC_TEST(sqrdmlah_4h_4h_4h_ALLas_16, "sqrdmlah v0.4h, v1.4h, v2.4h", 0, 1, 2)
GEN_THREEVEC_TEST(sqrdmlah_4h_4h_4h_ALLfs_16, "sqrdmlah v0.4h, v1.4h, v2.4h", 0, 1, 2)
GEN_THREEVEC_TEST(sqrdmlah_4h_4h_4h_UP_16, "sqrdmlah v0.4h, v1.4h, v2.4h", 0, 1, 2)
GEN_THREEVEC_TEST(sqrdmlah_4h_4h_4h_DOWN_16, "sqrdmlah v0.4h, v1.4h, v2.4h", 0, 1, 2)
GEN_THREEVEC_TEST(sqrdmlah_4h_4h_4h_PI_16, "sqrdmlah v0.4h, v1.4h, v2.4h", 0, 1, 2)
GEN_THREEVEC_TEST(sqrdmlah_4h_4h_4h_E_16, "sqrdmlah v0.4h, v1.4h, v2.4h", 0, 1, 2)

/* sqrdmlah (vector version, with index) */
GEN_THREEVEC_TEST_RND(sqrdmlah_i_4h_4h_4h, "sqrdmlah v0.4h, v1.4h, v2.4h[0]", 0, 1, 2)
GEN_THREEVEC_TEST_RND(sqrdmlah_i_8h_8h_8h, "sqrdmlah v0.8h, v1.8h, v2.8h[1]", 0, 1, 2)
GEN_THREEVEC_TEST_RND(sqrdmlah_i_2s_2s_2s, "sqrdmlah v0.2s, v1.2s, v2.2s[2]", 0, 1, 2)
GEN_THREEVEC_TEST_RND(sqrdmlah_i_4s_4s_4s, "sqrdmlah v0.4s, v1.4s, v2.4s[3]", 0, 1, 2)

GEN_THREEVEC_TEST(sqrdmlah_i_4h_4h_4h_0, "sqrdmlah v0.4h, v1.4h, v2.4h[0]", 0, 1, 2)
GEN_THREEVEC_TEST(sqrdmlah_i_4h_4h_4h_ALL5s_16, "sqrdmlah v0.4h, v1.4h, v2.4h[1]", 0, 1, 2)
GEN_THREEVEC_TEST(sqrdmlah_i_4h_4h_4h_ALLas_16, "sqrdmlah v0.4h, v1.4h, v2.4h[2]", 0, 1, 2)
GEN_THREEVEC_TEST(sqrdmlah_i_4h_4h_4h_ALLfs_16, "sqrdmlah v0.4h, v1.4h, v2.4h[3]", 0, 1, 2)
GEN_THREEVEC_TEST(sqrdmlah_i_4h_4h_4h_UP_16, "sqrdmlah v0.4h, v1.4h, v2.4h[0]", 0, 1, 2)
GEN_THREEVEC_TEST(sqrdmlah_i_4h_4h_4h_DOWN_16, "sqrdmlah v0.4h, v1.4h, v2.4h[1]", 0, 1, 2)
GEN_THREEVEC_TEST(sqrdmlah_i_4h_4h_4h_PI_16, "sqrdmlah v0.4h, v1.4h, v2.4h[2]", 0, 1, 2)
GEN_THREEVEC_TEST(sqrdmlah_i_4h_4h_4h_E_16, "sqrdmlah v0.4h, v1.4h, v2.4h[3]", 0, 1, 2)

/* sqrdmlah (scalar version) */
GEN_THREEVEC_TEST_RND(sqrdmlah_h0_h1_h2, "sqrdmlah h0, h1, h2", 0, 1, 2)
GEN_THREEVEC_TEST_RND(sqrdmlah_s0_s1_s2, "sqrdmlah s0, s1, s2", 0, 1, 2)
GEN_THREEVEC_TEST_RND(sqrdmlah_h3_h4_h5, "sqrdmlah h3, h4, h5", 3, 4, 5)
GEN_THREEVEC_TEST_RND(sqrdmlah_s3_s4_s5, "sqrdmlah s3, s4, s5", 3, 4, 5)
GEN_THREEVEC_TEST_RND(sqrdmlah_h6_h7_h8, "sqrdmlah h6, h7, h8", 6, 7, 8)
GEN_THREEVEC_TEST_RND(sqrdmlah_s6_s7_s8, "sqrdmlah s6, s7, s8", 6, 7, 8)
GEN_THREEVEC_TEST_RND(sqrdmlah_h9_h10_h11, "sqrdmlah h9, h10, h11", 9, 10, 11)
GEN_THREEVEC_TEST_RND(sqrdmlah_s9_s10_s11, "sqrdmlah s9, s10, s11", 9, 10, 11)
GEN_THREEVEC_TEST_RND(sqrdmlah_h12_h13_h14, "sqrdmlah h12, h13, h14", 12, 13, 14)
GEN_THREEVEC_TEST_RND(sqrdmlah_s12_s13_s14, "sqrdmlah s12, s13, s14", 12, 13, 14)
GEN_THREEVEC_TEST_RND(sqrdmlah_h15_h16_h17, "sqrdmlah h15, h16, h17", 15, 16, 17)
GEN_THREEVEC_TEST_RND(sqrdmlah_s15_s16_s17, "sqrdmlah s15, s16, s17", 15, 16, 17)
GEN_THREEVEC_TEST_RND(sqrdmlah_h18_h19_h20, "sqrdmlah h18, h19, h20", 18, 19, 20)
GEN_THREEVEC_TEST_RND(sqrdmlah_s18_s19_s20, "sqrdmlah s18, s19, s20", 18, 19, 20)
GEN_THREEVEC_TEST_RND(sqrdmlah_h21_h22_h23, "sqrdmlah h21, h22, h23", 21, 22, 23)
GEN_THREEVEC_TEST_RND(sqrdmlah_s21_s22_s23, "sqrdmlah s21, s22, s23", 21, 22, 23)
GEN_THREEVEC_TEST_RND(sqrdmlah_h24_h25_h26, "sqrdmlah h24, h25, h26", 24, 25, 26)
GEN_THREEVEC_TEST_RND(sqrdmlah_s24_s25_s26, "sqrdmlah s24, s25, s26", 24, 25, 26)
GEN_THREEVEC_TEST_RND(sqrdmlah_h27_h28_h29, "sqrdmlah h27, h28, h29", 27, 28, 29)
GEN_THREEVEC_TEST_RND(sqrdmlah_s27_s28_s29, "sqrdmlah s27, s28, s29", 27, 28, 29)
GEN_THREEVEC_TEST_RND(sqrdmlah_h30_h31_h0, "sqrdmlah h30, h31, h0", 30, 31, 0)
GEN_THREEVEC_TEST_RND(sqrdmlah_s30_s31_s0, "sqrdmlah s30, s31, s0", 30, 31, 0)

/* sqrdmlah (scalar version, with index) */
GEN_THREEVEC_TEST_RND(sqrdmlah_i_h0_h1_v2, "sqrdmlah h0, h1, v2.h[0]", 0, 1, 2)
GEN_THREEVEC_TEST_RND(sqrdmlah_i_s0_s1_v2, "sqrdmlah s0, s1, v2.s[0]", 0, 1, 2)
GEN_THREEVEC_TEST_RND(sqrdmlah_i_h3_h4_v5, "sqrdmlah h3, h4, v5.h[1]", 3, 4, 5)
GEN_THREEVEC_TEST_RND(sqrdmlah_i_s3_s4_v5, "sqrdmlah s3, s4, v5.s[1]", 3, 4, 5)

/* sqrdmlsh (vector version) */
GEN_THREEVEC_TEST_RND(sqrdmlsh_4h_4h_4h, "sqrdmlsh v0.4h, v1.4h, v2.4h", 0, 1, 2)
GEN_THREEVEC_TEST_RND(sqrdmlsh_8h_8h_8h, "sqrdmlsh v0.8h, v1.8h, v2.8h", 0, 1, 2)
GEN_THREEVEC_TEST_RND(sqrdmlsh_2s_2s_2s, "sqrdmlsh v0.2s, v1.2s, v2.2s", 0, 1, 2)
GEN_THREEVEC_TEST_RND(sqrdmlsh_4s_4s_4s, "sqrdmlsh v0.4s, v1.4s, v2.4s", 0, 1, 2)

GEN_THREEVEC_TEST(sqrdmlsh_4h_4h_4h_0, "sqrdmlsh v0.4h, v1.4h, v2.4h", 0, 1, 2)
GEN_THREEVEC_TEST(sqrdmlsh_4h_4h_4h_ALL5s_16, "sqrdmlsh v0.4h, v1.4h, v2.4h", 0, 1, 2)
GEN_THREEVEC_TEST(sqrdmlsh_4h_4h_4h_ALLas_16, "sqrdmlsh v0.4h, v1.4h, v2.4h", 0, 1, 2)
GEN_THREEVEC_TEST(sqrdmlsh_4h_4h_4h_ALLfs_16, "sqrdmlsh v0.4h, v1.4h, v2.4h", 0, 1, 2)
GEN_THREEVEC_TEST(sqrdmlsh_4h_4h_4h_UP_16, "sqrdmlsh v0.4h, v1.4h, v2.4h", 0, 1, 2)
GEN_THREEVEC_TEST(sqrdmlsh_4h_4h_4h_DOWN_16, "sqrdmlsh v0.4h, v1.4h, v2.4h", 0, 1, 2)
GEN_THREEVEC_TEST(sqrdmlsh_4h_4h_4h_PI_16, "sqrdmlsh v0.4h, v1.4h, v2.4h", 0, 1, 2)
GEN_THREEVEC_TEST(sqrdmlsh_4h_4h_4h_E_16, "sqrdmlsh v0.4h, v1.4h, v2.4h", 0, 1, 2)

/* sqrdmlsh (vector version, with index) */
GEN_THREEVEC_TEST_RND(sqrdmlsh_i_4h_4h_4h, "sqrdmlsh v0.4h, v1.4h, v2.4h[0]", 0, 1, 2)
GEN_THREEVEC_TEST_RND(sqrdmlsh_i_8h_8h_8h, "sqrdmlsh v0.8h, v1.8h, v2.8h[1]", 0, 1, 2)
GEN_THREEVEC_TEST_RND(sqrdmlsh_i_2s_2s_2s, "sqrdmlsh v0.2s, v1.2s, v2.2s[2]", 0, 1, 2)
GEN_THREEVEC_TEST_RND(sqrdmlsh_i_4s_4s_4s, "sqrdmlsh v0.4s, v1.4s, v2.4s[3]", 0, 1, 2)

GEN_THREEVEC_TEST(sqrdmlsh_i_4h_4h_4h_0, "sqrdmlsh v0.4h, v1.4h, v2.4h[0]", 0, 1, 2)
GEN_THREEVEC_TEST(sqrdmlsh_i_4h_4h_4h_ALL5s_16, "sqrdmlsh v0.4h, v1.4h, v2.4h[1]", 0, 1, 2)
GEN_THREEVEC_TEST(sqrdmlsh_i_4h_4h_4h_ALLas_16, "sqrdmlsh v0.4h, v1.4h, v2.4h[2]", 0, 1, 2)
GEN_THREEVEC_TEST(sqrdmlsh_i_4h_4h_4h_ALLfs_16, "sqrdmlsh v0.4h, v1.4h, v2.4h[3]", 0, 1, 2)
GEN_THREEVEC_TEST(sqrdmlsh_i_4h_4h_4h_UP_16, "sqrdmlsh v0.4h, v1.4h, v2.4h[0]", 0, 1, 2)
GEN_THREEVEC_TEST(sqrdmlsh_i_4h_4h_4h_DOWN_16, "sqrdmlsh v0.4h, v1.4h, v2.4h[1]", 0, 1, 2)
GEN_THREEVEC_TEST(sqrdmlsh_i_4h_4h_4h_PI_16, "sqrdmlsh v0.4h, v1.4h, v2.4h[2]", 0, 1, 2)
GEN_THREEVEC_TEST(sqrdmlsh_i_4h_4h_4h_E_16, "sqrdmlsh v0.4h, v1.4h, v2.4h[3]", 0, 1, 2)

/* sqrdmlsh (scalar version) */
GEN_THREEVEC_TEST_RND(sqrdmlsh_h0_h1_h2, "sqrdmlsh h0, h1, h2", 0, 1, 2)
GEN_THREEVEC_TEST_RND(sqrdmlsh_s0_s1_s2, "sqrdmlsh s0, s1, s2", 0, 1, 2)
GEN_THREEVEC_TEST_RND(sqrdmlsh_h3_h4_h5, "sqrdmlsh h3, h4, h5", 3, 4, 5)
GEN_THREEVEC_TEST_RND(sqrdmlsh_s3_s4_s5, "sqrdmlsh s3, s4, s5", 3, 4, 5)
GEN_THREEVEC_TEST_RND(sqrdmlsh_h6_h7_h8, "sqrdmlsh h6, h7, h8", 6, 7, 8)
GEN_THREEVEC_TEST_RND(sqrdmlsh_s6_s7_s8, "sqrdmlsh s6, s7, s8", 6, 7, 8)
GEN_THREEVEC_TEST_RND(sqrdmlsh_h9_h10_h11, "sqrdmlsh h9, h10, h11", 9, 10, 11)
GEN_THREEVEC_TEST_RND(sqrdmlsh_s9_s10_s11, "sqrdmlsh s9, s10, s11", 9, 10, 11)
GEN_THREEVEC_TEST_RND(sqrdmlsh_h12_h13_h14, "sqrdmlsh h12, h13, h14", 12, 13, 14)
GEN_THREEVEC_TEST_RND(sqrdmlsh_s12_s13_s14, "sqrdmlsh s12, s13, s14", 12, 13, 14)
GEN_THREEVEC_TEST_RND(sqrdmlsh_h15_h16_h17, "sqrdmlsh h15, h16, h17", 15, 16, 17)
GEN_THREEVEC_TEST_RND(sqrdmlsh_s15_s16_s17, "sqrdmlsh s15, s16, s17", 15, 16, 17)
GEN_THREEVEC_TEST_RND(sqrdmlsh_h18_h19_h20, "sqrdmlsh h18, h19, h20", 18, 19, 20)
GEN_THREEVEC_TEST_RND(sqrdmlsh_s18_s19_s20, "sqrdmlsh s18, s19, s20", 18, 19, 20)
GEN_THREEVEC_TEST_RND(sqrdmlsh_h21_h22_h23, "sqrdmlsh h21, h22, h23", 21, 22, 23)
GEN_THREEVEC_TEST_RND(sqrdmlsh_s21_s22_s23, "sqrdmlsh s21, s22, s23", 21, 22, 23)
GEN_THREEVEC_TEST_RND(sqrdmlsh_h24_h25_h26, "sqrdmlsh h24, h25, h26", 24, 25, 26)
GEN_THREEVEC_TEST_RND(sqrdmlsh_s24_s25_s26, "sqrdmlsh s24, s25, s26", 24, 25, 26)
GEN_THREEVEC_TEST_RND(sqrdmlsh_h27_h28_h29, "sqrdmlsh h27, h28, h29", 27, 28, 29)
GEN_THREEVEC_TEST_RND(sqrdmlsh_s27_s28_s29, "sqrdmlsh s27, s28, s29", 27, 28, 29)
GEN_THREEVEC_TEST_RND(sqrdmlsh_h30_h31_h0, "sqrdmlsh h30, h31, h0", 30, 31, 0)
GEN_THREEVEC_TEST_RND(sqrdmlsh_s30_s31_s0, "sqrdmlsh s30, s31, s0", 30, 31, 0)

/* sqrdmlsh (scalar version, with index) */
GEN_THREEVEC_TEST_RND(sqrdmlsh_i_h0_h1_v2, "sqrdmlsh h0, h1, v2.h[0]", 0, 1, 2)
GEN_THREEVEC_TEST_RND(sqrdmlsh_i_s0_s1_v2, "sqrdmlsh s0, s1, v2.s[0]", 0, 1, 2)
GEN_THREEVEC_TEST_RND(sqrdmlsh_i_h3_h4_v5, "sqrdmlsh h3, h4, v5.h[1]", 3, 4, 5)
GEN_THREEVEC_TEST_RND(sqrdmlsh_i_s3_s4_v5, "sqrdmlsh s3, s4, v5.s[1]", 3, 4, 5)

int main ( void )
{
   assert(sizeof(V128) == 16);

   /* sqrdmlah (vector version, no index) */
   if (1) test_sqrdmlah_4h_4h_4h(TyH);
   if (1) test_sqrdmlah_8h_8h_8h(TyH);
   if (1) test_sqrdmlah_2s_2s_2s(TyS);
   if (1) test_sqrdmlah_4s_4s_4s(TyS);

   V128 vout;
   V128 vin[2];

   if (1) {
      GEN_THREEVEC_TEST_CALLS(sqrdmlah, 4, TyH, 4h, 0);
      GEN_THREEVEC_TEST_CALLS(sqrdmlah, 4, TyH, 4h, ALL5s_16);
      GEN_THREEVEC_TEST_CALLS(sqrdmlah, 4, TyH, 4h, ALLas_16);
      GEN_THREEVEC_TEST_CALLS(sqrdmlah, 4, TyH, 4h, ALLfs_16);
      GEN_THREEVEC_TEST_CALLS(sqrdmlah, 4, TyH, 4h, UP_16);
      GEN_THREEVEC_TEST_CALLS(sqrdmlah, 4, TyH, 4h, DOWN_16);
      GEN_THREEVEC_TEST_CALLS(sqrdmlah, 4, TyH, 4h, PI_16);
      GEN_THREEVEC_TEST_CALLS(sqrdmlah, 4, TyH, 4h, E_16);
   }

   /* sqrdmlah (vector version, with index) */
   if (1) test_sqrdmlah_i_4h_4h_4h(TyH);
   if (1) test_sqrdmlah_i_8h_8h_8h(TyH);
   if (1) test_sqrdmlah_i_2s_2s_2s(TyS);
   if (1) test_sqrdmlah_i_4s_4s_4s(TyS);

   if (1) {
      GEN_THREEVEC_TEST_CALLSI(sqrdmlah_i, 4, TyH, 4h, 0, 0);
      GEN_THREEVEC_TEST_CALLSI(sqrdmlah_i, 4, TyH, 4h, ALL5s_16, 1);
      GEN_THREEVEC_TEST_CALLSI(sqrdmlah_i, 4, TyH, 4h, ALLas_16, 2);
      GEN_THREEVEC_TEST_CALLSI(sqrdmlah_i, 4, TyH, 4h, ALLfs_16, 3);
      GEN_THREEVEC_TEST_CALLSI(sqrdmlah_i, 4, TyH, 4h, UP_16, 0);
      GEN_THREEVEC_TEST_CALLSI(sqrdmlah_i, 4, TyH, 4h, DOWN_16, 1);
      GEN_THREEVEC_TEST_CALLSI(sqrdmlah_i, 4, TyH, 4h, PI_16, 2);
      GEN_THREEVEC_TEST_CALLSI(sqrdmlah_i, 4, TyH, 4h, E_16, 3);
   }

   /* sqrdmlah (scalar version) */
   if (1) test_sqrdmlah_h0_h1_h2(TyH);
   if (1) test_sqrdmlah_s0_s1_s2(TyS);
   if (1) test_sqrdmlah_h3_h4_h5(TyH);
   if (1) test_sqrdmlah_s3_s4_s5(TyS);
   if (1) test_sqrdmlah_h6_h7_h8(TyH);
   if (1) test_sqrdmlah_s6_s7_s8(TyS);
   if (1) test_sqrdmlah_h9_h10_h11(TyH);
   if (1) test_sqrdmlah_s9_s10_s11(TyS);
   if (1) test_sqrdmlah_h12_h13_h14(TyH);
   if (1) test_sqrdmlah_s12_s13_s14(TyS);
   if (1) test_sqrdmlah_h15_h16_h17(TyH);
   if (1) test_sqrdmlah_s15_s16_s17(TyS);
   if (1) test_sqrdmlah_h18_h19_h20(TyH);
   if (1) test_sqrdmlah_s18_s19_s20(TyS);
   if (1) test_sqrdmlah_h21_h22_h23(TyH);
   if (1) test_sqrdmlah_s21_s22_s23(TyS);
   if (1) test_sqrdmlah_h24_h25_h26(TyH);
   if (1) test_sqrdmlah_s24_s25_s26(TyS);
   if (1) test_sqrdmlah_h27_h28_h29(TyH);
   if (1) test_sqrdmlah_s27_s28_s29(TyS);
   if (1) test_sqrdmlah_h30_h31_h0(TyH);
   if (1) test_sqrdmlah_s30_s31_s0(TyS);

   /* sqrdmlah (scalar version, with index) */
   if (1) test_sqrdmlah_i_h0_h1_v2(TyH);
   if (1) test_sqrdmlah_i_s0_s1_v2(TyS);
   if (1) test_sqrdmlah_i_h3_h4_v5(TyH);
   if (1) test_sqrdmlah_i_s3_s4_v5(TyS);

   /* sqrdmlsh (vector version) */
   if (1) test_sqrdmlsh_4h_4h_4h(TyH);
   if (1) test_sqrdmlsh_8h_8h_8h(TyH);
   if (1) test_sqrdmlsh_2s_2s_2s(TyS);
   if (1) test_sqrdmlsh_4s_4s_4s(TyS);

   if (1) {
      GEN_THREEVEC_TEST_CALLS(sqrdmlsh, 4, TyH, 4h, 0);
      GEN_THREEVEC_TEST_CALLS(sqrdmlsh, 4, TyH, 4h, ALL5s_16);
      GEN_THREEVEC_TEST_CALLS(sqrdmlsh, 4, TyH, 4h, ALLas_16);
      GEN_THREEVEC_TEST_CALLS(sqrdmlsh, 4, TyH, 4h, ALLfs_16);
      GEN_THREEVEC_TEST_CALLS(sqrdmlsh, 4, TyH, 4h, UP_16);
      GEN_THREEVEC_TEST_CALLS(sqrdmlsh, 4, TyH, 4h, DOWN_16);
      GEN_THREEVEC_TEST_CALLS(sqrdmlsh, 4, TyH, 4h, PI_16);
      GEN_THREEVEC_TEST_CALLS(sqrdmlsh, 4, TyH, 4h, E_16);
   }

   /* sqrdmlsh (vector version, with index) */
   if (1) test_sqrdmlsh_i_4h_4h_4h(TyH);
   if (1) test_sqrdmlsh_i_8h_8h_8h(TyH);
   if (1) test_sqrdmlsh_i_2s_2s_2s(TyS);
   if (1) test_sqrdmlsh_i_4s_4s_4s(TyS);

   if (1) {
      GEN_THREEVEC_TEST_CALLSI(sqrdmlsh_i, 4, TyH, 4h, 0, 0);
      GEN_THREEVEC_TEST_CALLSI(sqrdmlsh_i, 4, TyH, 4h, ALL5s_16, 1);
      GEN_THREEVEC_TEST_CALLSI(sqrdmlsh_i, 4, TyH, 4h, ALLas_16, 2);
      GEN_THREEVEC_TEST_CALLSI(sqrdmlsh_i, 4, TyH, 4h, ALLfs_16, 3);
      GEN_THREEVEC_TEST_CALLSI(sqrdmlsh_i, 4, TyH, 4h, UP_16, 0);
      GEN_THREEVEC_TEST_CALLSI(sqrdmlsh_i, 4, TyH, 4h, DOWN_16, 1);
      GEN_THREEVEC_TEST_CALLSI(sqrdmlsh_i, 4, TyH, 4h, PI_16, 2);
      GEN_THREEVEC_TEST_CALLSI(sqrdmlsh_i, 4, TyH, 4h, E_16, 3);
   }

   /* sqrdmlsh (scalar version) */
   if (1) test_sqrdmlsh_h0_h1_h2(TyH);
   if (1) test_sqrdmlsh_s0_s1_s2(TyS);
   if (1) test_sqrdmlsh_h3_h4_h5(TyH);
   if (1) test_sqrdmlsh_s3_s4_s5(TyS);
   if (1) test_sqrdmlsh_h6_h7_h8(TyH);
   if (1) test_sqrdmlsh_s6_s7_s8(TyS);
   if (1) test_sqrdmlsh_h9_h10_h11(TyH);
   if (1) test_sqrdmlsh_s9_s10_s11(TyS);
   if (1) test_sqrdmlsh_h12_h13_h14(TyH);
   if (1) test_sqrdmlsh_s12_s13_s14(TyS);
   if (1) test_sqrdmlsh_h15_h16_h17(TyH);
   if (1) test_sqrdmlsh_s15_s16_s17(TyS);
   if (1) test_sqrdmlsh_h18_h19_h20(TyH);
   if (1) test_sqrdmlsh_s18_s19_s20(TyS);
   if (1) test_sqrdmlsh_h21_h22_h23(TyH);
   if (1) test_sqrdmlsh_s21_s22_s23(TyS);
   if (1) test_sqrdmlsh_h24_h25_h26(TyH);
   if (1) test_sqrdmlsh_s24_s25_s26(TyS);
   if (1) test_sqrdmlsh_h27_h28_h29(TyH);
   if (1) test_sqrdmlsh_s27_s28_s29(TyS);
   if (1) test_sqrdmlsh_h30_h31_h0(TyH);
   if (1) test_sqrdmlsh_s30_s31_s0(TyS);

   /* sqrdmlsh (scalar version, with index) */
   if (1) test_sqrdmlsh_i_h0_h1_v2(TyH);
   if (1) test_sqrdmlsh_i_s0_s1_v2(TyS);
   if (1) test_sqrdmlsh_i_h3_h4_v5(TyH);
   if (1) test_sqrdmlsh_i_s3_s4_v5(TyS);

   return 0;
}
