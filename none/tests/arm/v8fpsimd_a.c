
/*
gcc -o v8fpsimd_a v8fpsimd_a.c -march=armv8-a -mfpu=crypto-neon-fp-armv8 \
       -I../../.. -Wall -g -marm

gcc -o v8fpsimd_t v8fpsimd_a.c -march=armv8-a -mfpu=crypto-neon-fp-armv8 \
       -I../../.. -Wall -g
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
      if (randUChar() < 32) {
         /* once every 8 times, clone one of the lanes */
         switch (ty) {
            case TySF: case TyS: {
               UInt l1, l2;
               while (1) {
                  l1 = randUChar() & 3;
                  l2 = randUChar() & 3;
                  if (l1 != l2) break;
               }
               assert(l1 < 4 && l2 < 4);
               v->u32[l1] = v->u32[l2];
               printf("randV128: doing v->u32[%u] = v->u32[%u]\n", l1, l2);
               break;
            }
            case TyDF: case TyD: {
               UInt l1, l2;
               while (1) {
                  l1 = randUChar() & 1;
                  l2 = randUChar() & 1;
                  if (l1 != l2) break;
               }
               assert(l1 < 2 && l2 < 2);
               printf("randV128: doing v->u64[%u] = v->u64[%u]\n", l1, l2);
               v->u64[l1] = v->u64[l2];
               break;
            }
            default:
               break;
         }
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


/* Are we compiling for thumb or arm encodings?  This has a bearing
   on the inline assembly syntax needed below. */

#if defined(__thumb__) || defined(__thumb2__)
#  define IT_EQ "it eq ; "
#  define IT_NE "it ne ; "
#  define IT_AL /* */
#else
#  define IT_EQ /* */
#  define IT_NE /* */
#  define IT_AL /* */
#endif


/* Generate a test that involves two vector regs,
   with no bias as towards which is input or output. 
   It's OK to use r8 as scratch.

   Note that the insn doesn't *have* to use Q (128 bit) registers --
   it can instead mention D (64 bit) and S (32-bit) registers.
   However, in that case callers of this macro must be very careful to
   specify QVECREG1NO and QVECREG2NO in such a way as to cover all of
   the mentioned D and S registers, using the relations

     D<n> == S<2n+1> and S<2n>
     Q<n> == D<2n+1> and D<2n>

   Failing to do so correctly will make the test meaningless, because
   it will potentially load test data into the wrong registers before
   the test, and/or show the values of the wrong registers after the
   test.  The allowed register values are:
      S: 0 .. 31
      D: 0 .. 31
      Q: 0 .. 15
   Note that Q[15..0] == D[31..0] but S[31..0] only overlaps Q[0..7],
   so a Q value of 8 or above is definitely invalid for a S register.
   None of this is checked, though, so be careful when creating the
   Q numbers.

   It would be clearer and easier to write the Q numbers using integer
   division.  For example, in

      GEN_TWOVEC_QDS_TEST(vcvtn_s32_f64, "vcvtn.s32.f64 s27, d5",  6,2)

   instead of writing "6, 2" at the end, write "(27/4), (5/2)".  This
   would make clear the connection between the register numbers and
   the Q numbers.  Unfortunately those expressions need to expanded to
   single digits at C-preprocessing time, and cpp won't do that.  So
   we have to do it the hard and error-prone way.
*/
#define GEN_TWOVEC_QDS_TEST(TESTNAME,INSN_PRE,INSN, \
                            QVECREG1NO,QVECREG2NO) \
  __attribute__((noinline)) \
  static void test_##TESTNAME ( LaneTy ty ) { \
     Int i; \
     assert(QVECREG1NO >= 0 && QVECREG1NO <= 15); \
     assert(QVECREG2NO >= 0 && QVECREG2NO <= 15); \
     for (i = 0; i < ITERS; i++) { \
        V128 block[4+1]; \
        memset(block, 0x55, sizeof(block)); \
        randV128(&block[0], ty); \
        randV128(&block[1], ty); \
        randV128(&block[2], ty); \
        randV128(&block[3], ty); \
        __asm__ __volatile__( \
           "mov r9, #0 ; vmsr fpscr, r9 ; " \
           "msr apsr_nzcvq, r9 ; " \
           "add r9, %0, #0  ; vld1.8 { q"#QVECREG1NO" }, [r9] ; " \
           "add r9, %0, #16 ; vld1.8 { q"#QVECREG2NO" }, [r9] ; " \
           INSN_PRE INSN " ; " \
           "add r9, %0, #32 ; vst1.8 { q"#QVECREG1NO" }, [r9] ; " \
           "add r9, %0, #48 ; vst1.8 { q"#QVECREG2NO" }, [r9] ; " \
           "vmrs r9, fpscr ; str r9, [%0, #64] " \
           : : "r"(&block[0]) \
             : "cc", "memory", "q"#QVECREG1NO, "q"#QVECREG2NO, "r8", "r9" \
        ); \
        /* Don't use INSN_PRE in printing, since that differs */ \
        /* between ARM and Thumb and hence makes their outputs differ. */ \
        printf(INSN   "   "); \
        UInt fpscr = 0xFFFFFFE0 & block[4].u32[0]; \
        showV128(&block[0]); printf("  "); \
        showV128(&block[1]); printf("  "); \
        showV128(&block[2]); printf("  "); \
        showV128(&block[3]); printf(" fpscr=%08x\n", fpscr); \
     } \
  }


/* Generate a test that involves three vector regs,
   with no bias as towards which is input or output.  It's also OK
   to use r8 as scratch. */
#define GEN_THREEVEC_QDS_TEST(TESTNAME,INSN_PRE, \
                              INSN,QVECREG1NO,QVECREG2NO,QVECREG3NO) \
  __attribute__((noinline)) \
  static void test_##TESTNAME ( LaneTy ty ) { \
     Int i; \
     assert(QVECREG1NO >= 0 && QVECREG1NO <= 15); \
     assert(QVECREG2NO >= 0 && QVECREG2NO <= 15); \
     assert(QVECREG3NO >= 0 && QVECREG3NO <= 15); \
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
           "msr apsr_nzcvq, r9 ; " \
           "add r9, %0, #0  ; vld1.8 { q"#QVECREG1NO" }, [r9] ; " \
           "add r9, %0, #16 ; vld1.8 { q"#QVECREG2NO" }, [r9] ; " \
           "add r9, %0, #32 ; vld1.8 { q"#QVECREG3NO" }, [r9] ; " \
           INSN_PRE INSN " ; " \
           "add r9, %0, #48 ; vst1.8 { q"#QVECREG1NO" }, [r9] ; " \
           "add r9, %0, #64 ; vst1.8 { q"#QVECREG2NO" }, [r9] ; " \
           "add r9, %0, #80 ; vst1.8 { q"#QVECREG3NO" }, [r9] ; " \
           "vmrs r9, fpscr ; str r9, [%0, #96] " \
           : : "r"(&block[0]) \
           : "cc", "memory", "q"#QVECREG1NO, "q"#QVECREG2NO, "q"#QVECREG3NO, \
             "r8", "r9" \
        ); \
        /* Don't use INSN_PRE in printing, since that differs */ \
        /* between ARM and Thumb and hence makes their outputs differ. */ \
        printf(INSN   "   "); \
        UInt fpscr = 0xFFFFFFE0 & block[6].u32[0]; \
        showV128(&block[0]); printf("  "); \
        showV128(&block[1]); printf("  "); \
        showV128(&block[2]); printf("  "); \
        showV128(&block[3]); printf("  "); \
        showV128(&block[4]); printf("  "); \
        showV128(&block[5]); printf(" fpscr=%08x\n", fpscr); \
     } \
  }

GEN_THREEVEC_QDS_TEST(vselge_f32, IT_AL, "vselge.f32 s15,s16,s20", 3,4,5) 
GEN_THREEVEC_QDS_TEST(vselge_f64, IT_AL, "vselge.f64 d7, d8, d10", 3,4,5) 

GEN_THREEVEC_QDS_TEST(vselgt_f32, IT_AL, "vselgt.f32 s15,s16,s20", 3,4,5) 
GEN_THREEVEC_QDS_TEST(vselgt_f64, IT_AL, "vselgt.f64 d7, d8, d10", 3,4,5) 

GEN_THREEVEC_QDS_TEST(vseleq_f32, IT_AL, "vseleq.f32 s15,s16,s20", 3,4,5) 
GEN_THREEVEC_QDS_TEST(vseleq_f64, IT_AL, "vseleq.f64 d7, d8, d10", 3,4,5) 

GEN_THREEVEC_QDS_TEST(vselvs_f32, IT_AL, "vselvs.f32 s15,s16,s20", 3,4,5) 
GEN_THREEVEC_QDS_TEST(vselvs_f64, IT_AL, "vselvs.f64 d7, d8, d10", 3,4,5) 

GEN_THREEVEC_QDS_TEST(vmaxnm_f32, IT_AL, "vmaxnm.f32 s15,s16,s20", 3,4,5) 
GEN_THREEVEC_QDS_TEST(vmaxnm_f64, IT_AL, "vmaxnm.f64 d7, d8, d10", 3,4,5) 

GEN_THREEVEC_QDS_TEST(vminnm_f32, IT_AL, "vminnm.f32 s15,s16,s20", 3,4,5) 
GEN_THREEVEC_QDS_TEST(vminnm_f64, IT_AL, "vminnm.f64 d7, d8, d10", 3,4,5) 

GEN_TWOVEC_QDS_TEST(vcvtn_s32_f64, IT_AL, "vcvtn.s32.f64 s27, d5",  6,2)
GEN_TWOVEC_QDS_TEST(vcvta_s32_f64, IT_AL, "vcvta.s32.f64 s4,  d20", 1,10)
GEN_TWOVEC_QDS_TEST(vcvtp_s32_f64, IT_AL, "vcvtp.s32.f64 s7,  d31", 1,15)
GEN_TWOVEC_QDS_TEST(vcvtm_s32_f64, IT_AL, "vcvtm.s32.f64 s1,  d0",  0,0)

GEN_TWOVEC_QDS_TEST(vcvtn_s32_f32, IT_AL, "vcvtn.s32.f32 s27, s5",  6,1)
GEN_TWOVEC_QDS_TEST(vcvta_s32_f32, IT_AL, "vcvta.s32.f32 s4,  s20", 1,5)
GEN_TWOVEC_QDS_TEST(vcvtp_s32_f32, IT_AL, "vcvtp.s32.f32 s7,  s31", 1,7)
GEN_TWOVEC_QDS_TEST(vcvtm_s32_f32, IT_AL, "vcvtm.s32.f32 s1,  s0",  0,0)

GEN_TWOVEC_QDS_TEST(vcvtn_u32_f64, IT_AL, "vcvtn.u32.f64 s27, d5",  6,2)
GEN_TWOVEC_QDS_TEST(vcvta_u32_f64, IT_AL, "vcvta.u32.f64 s4,  d20", 1,10)
GEN_TWOVEC_QDS_TEST(vcvtp_u32_f64, IT_AL, "vcvtp.u32.f64 s7,  d31", 1,15)
GEN_TWOVEC_QDS_TEST(vcvtm_u32_f64, IT_AL, "vcvtm.u32.f64 s1,  d0",  0,0)

GEN_TWOVEC_QDS_TEST(vcvtn_u32_f32, IT_AL, "vcvtn.u32.f32 s27, s5",  6,1)
GEN_TWOVEC_QDS_TEST(vcvta_u32_f32, IT_AL, "vcvta.u32.f32 s4,  s20", 1,5)
GEN_TWOVEC_QDS_TEST(vcvtp_u32_f32, IT_AL, "vcvtp.u32.f32 s7,  s31", 1,7)
GEN_TWOVEC_QDS_TEST(vcvtm_u32_f32, IT_AL, "vcvtm.u32.f32 s1,  s0",  0,0)

GEN_TWOVEC_QDS_TEST(vcvtb_f64_f16, IT_AL, "vcvtb.f64.f16 d27, s18", 13, 4)
GEN_TWOVEC_QDS_TEST(vcvtt_f64_f16, IT_AL, "vcvtt.f64.f16 d28, s17", 14, 4)

GEN_TWOVEC_QDS_TEST(vcvtb_f16_f64, IT_AL, "vcvtb.f16.f64 s9, d17", 2, 8)
GEN_TWOVEC_QDS_TEST(vcvtt_f16_f64, IT_AL, "vcvtt.f16.f64 s8, d27", 2, 13)

GEN_TWOVEC_QDS_TEST(vrintzeq_f64_f64, IT_EQ, "vrintzeq.f64.f64 d0, d9",  0, 4)
GEN_TWOVEC_QDS_TEST(vrintzne_f64_f64, IT_NE, "vrintzne.f64.f64 d1, d10", 0, 5)
GEN_TWOVEC_QDS_TEST(vrintzal_f64_f64, IT_AL,   "vrintz.f64.f64 d2, d11", 1, 5)

GEN_TWOVEC_QDS_TEST(vrintreq_f64_f64, IT_EQ, "vrintreq.f64.f64 d3, d12", 1, 6)
GEN_TWOVEC_QDS_TEST(vrintrne_f64_f64, IT_NE, "vrintrne.f64.f64 d4, d13", 2, 6)
GEN_TWOVEC_QDS_TEST(vrintral_f64_f64, IT_AL,   "vrintr.f64.f64 d5, d14", 2, 7)

GEN_TWOVEC_QDS_TEST(vrintxeq_f64_f64, IT_EQ, "vrintxeq.f64.f64 d6, d15", 3, 7)
GEN_TWOVEC_QDS_TEST(vrintxne_f64_f64, IT_NE, "vrintxne.f64.f64 d7, d16", 3, 8)
GEN_TWOVEC_QDS_TEST(vrintxal_f64_f64, IT_AL,   "vrintx.f64.f64 d8, d8",  4, 4)

GEN_TWOVEC_QDS_TEST(vrintzeq_f32_f32, IT_EQ, "vrintzeq.f32.f32 s0, s9",  0, 2)
GEN_TWOVEC_QDS_TEST(vrintzne_f32_f32, IT_NE, "vrintzne.f32.f32 s1, s10", 0, 2)
GEN_TWOVEC_QDS_TEST(vrintzal_f32_f32, IT_AL,   "vrintz.f32.f32 s2, s11", 0, 2)

GEN_TWOVEC_QDS_TEST(vrintreq_f32_f32, IT_EQ, "vrintreq.f32.f32 s3, s12", 0, 3)
GEN_TWOVEC_QDS_TEST(vrintrne_f32_f32, IT_NE, "vrintrne.f32.f32 s4, s13", 1, 3)
GEN_TWOVEC_QDS_TEST(vrintral_f32_f32, IT_AL,   "vrintr.f32.f32 s5, s14", 1, 3)

GEN_TWOVEC_QDS_TEST(vrintxeq_f32_f32, IT_EQ, "vrintxeq.f32.f32 s6, s15", 1, 3)
GEN_TWOVEC_QDS_TEST(vrintxne_f32_f32, IT_NE, "vrintxne.f32.f32 s7, s16", 1, 4)
GEN_TWOVEC_QDS_TEST(vrintxal_f32_f32, IT_AL,   "vrintx.f32.f32 s8, s8",  2, 2)

GEN_TWOVEC_QDS_TEST(vrintn_f64_f64, IT_AL, "vrintn.f64.f64 d3,  d15",  1,  7)
GEN_TWOVEC_QDS_TEST(vrinta_f64_f64, IT_AL, "vrinta.f64.f64 d6,  d18",  3,  9)
GEN_TWOVEC_QDS_TEST(vrintp_f64_f64, IT_AL, "vrintp.f64.f64 d9,  d21",  4, 10)
GEN_TWOVEC_QDS_TEST(vrintm_f64_f64, IT_AL, "vrintm.f64.f64 d12, d12",  6,  6)

GEN_TWOVEC_QDS_TEST(vrintn_f32_f32, IT_AL, "vrintn.f32.f32 s3,  s15",  0,  3)
GEN_TWOVEC_QDS_TEST(vrinta_f32_f32, IT_AL, "vrinta.f32.f32 s6,  s18",  1,  4)
GEN_TWOVEC_QDS_TEST(vrintp_f32_f32, IT_AL, "vrintp.f32.f32 s9,  s21",  2,  5)
GEN_TWOVEC_QDS_TEST(vrintm_f32_f32, IT_AL, "vrintm.f32.f32 s12, s12",  3,  3)

GEN_THREEVEC_QDS_TEST(vmaxnm_f32_vec64,
                      IT_AL, "vmaxnm.f32 d15,d16,d20", 7,8,10)
GEN_THREEVEC_QDS_TEST(vmaxnm_f32_vec128,
                      IT_AL, "vmaxnm.f32 q7, q8, q10", 7,8,10)

GEN_THREEVEC_QDS_TEST(vminnm_f32_vec64,
                      IT_AL, "vminnm.f32 d15,d16,d20", 7,8,10)
GEN_THREEVEC_QDS_TEST(vminnm_f32_vec128,
                      IT_AL, "vminnm.f32 q7, q8, q10", 7,8,10)

GEN_TWOVEC_QDS_TEST(vcvtn_s32_f32_vec64,
                    IT_AL, "vcvtn.s32.f32 d0,  d20",  0, 10)
GEN_TWOVEC_QDS_TEST(vcvta_s32_f32_vec64,
                    IT_AL, "vcvta.s32.f32 d5,  d25",  2, 12)
GEN_TWOVEC_QDS_TEST(vcvtp_s32_f32_vec64,
                    IT_AL, "vcvtp.s32.f32 d10, d30",  5, 15)
GEN_TWOVEC_QDS_TEST(vcvtm_s32_f32_vec64,
                    IT_AL, "vcvtm.s32.f32 d15, d15",  7, 7)

GEN_TWOVEC_QDS_TEST(vcvtn_s32_f32_vec128,
                    IT_AL, "vcvtn.s32.f32 q15, q0",  15, 0)
GEN_TWOVEC_QDS_TEST(vcvta_s32_f32_vec128,
                    IT_AL, "vcvta.s32.f32 q14, q1",  14, 1)
GEN_TWOVEC_QDS_TEST(vcvtp_s32_f32_vec128,
                    IT_AL, "vcvtp.s32.f32 q13, q2",  13, 2)
GEN_TWOVEC_QDS_TEST(vcvtm_s32_f32_vec128,
                    IT_AL, "vcvtm.s32.f32 q12, q3",  12, 3)

GEN_TWOVEC_QDS_TEST(vcvtn_u32_f32_vec64,
                    IT_AL, "vcvtn.u32.f32 d0,  d20", 0, 10)
GEN_TWOVEC_QDS_TEST(vcvta_u32_f32_vec64,
                    IT_AL, "vcvta.u32.f32 d5,  d25", 2, 12)
GEN_TWOVEC_QDS_TEST(vcvtp_u32_f32_vec64,
                    IT_AL, "vcvtp.u32.f32 d10, d30", 5, 15)
GEN_TWOVEC_QDS_TEST(vcvtm_u32_f32_vec64,
                    IT_AL, "vcvtm.u32.f32 d15, d15", 7, 7)

GEN_TWOVEC_QDS_TEST(vcvtn_u32_f32_vec128,
                    IT_AL, "vcvtn.u32.f32 q15, q0",  15, 0)
GEN_TWOVEC_QDS_TEST(vcvta_u32_f32_vec128,
                    IT_AL, "vcvta.u32.f32 q14, q1",  14, 1)
GEN_TWOVEC_QDS_TEST(vcvtp_u32_f32_vec128,
                    IT_AL, "vcvtp.u32.f32 q13, q2",  13, 2)
GEN_TWOVEC_QDS_TEST(vcvtm_u32_f32_vec128,
                    IT_AL, "vcvtm.u32.f32 q12, q3",  12, 3)

GEN_TWOVEC_QDS_TEST(vrintn_f32_f32_vec64,
                    IT_AL, "vrintn.f32.f32 d0,  d18", 0, 9)
GEN_TWOVEC_QDS_TEST(vrinta_f32_f32_vec64,
                    IT_AL, "vrinta.f32.f32 d3,  d21", 1, 10)
GEN_TWOVEC_QDS_TEST(vrintp_f32_f32_vec64,
                    IT_AL, "vrintp.f32.f32 d6,  d24", 3, 12)
GEN_TWOVEC_QDS_TEST(vrintm_f32_f32_vec64,
                    IT_AL, "vrintm.f32.f32 d9,  d27", 4, 13)
GEN_TWOVEC_QDS_TEST(vrintz_f32_f32_vec64,
                    IT_AL, "vrintz.f32.f32 d12, d30", 6, 15)
GEN_TWOVEC_QDS_TEST(vrintx_f32_f32_vec64,
                    IT_AL, "vrintx.f32.f32 d15, d15", 7, 7)

GEN_TWOVEC_QDS_TEST(vrintn_f32_f32_vec128,
                    IT_AL, "vrintn.f32.f32 q0,  q2",   0, 2)
GEN_TWOVEC_QDS_TEST(vrinta_f32_f32_vec128,
                    IT_AL, "vrinta.f32.f32 q3,  q5",   3, 5)
GEN_TWOVEC_QDS_TEST(vrintp_f32_f32_vec128,
                    IT_AL, "vrintp.f32.f32 q6,  q8",   6, 8)
GEN_TWOVEC_QDS_TEST(vrintm_f32_f32_vec128,
                    IT_AL, "vrintm.f32.f32 q9,  q11",  9, 11)
GEN_TWOVEC_QDS_TEST(vrintz_f32_f32_vec128,
                    IT_AL, "vrintz.f32.f32 q12, q14",  12, 14)
GEN_TWOVEC_QDS_TEST(vrintx_f32_f32_vec128,
                    IT_AL, "vrintx.f32.f32 q15, q15",  15, 15)

int main ( void )
{
   if (1) DO50( test_vselge_f32(TySF) );
   if (1) DO50( test_vselge_f64(TyDF) );

   if (1) DO50( test_vselgt_f32(TySF) );
   if (1) DO50( test_vselgt_f64(TyDF) );

   if (1) DO50( test_vseleq_f32(TySF) );
   if (1) DO50( test_vseleq_f64(TyDF) );

   if (1) DO50( test_vselvs_f32(TySF) );
   if (1) DO50( test_vselvs_f64(TyDF) );

   if (1) DO50( test_vmaxnm_f32(TySF) );
   if (1) DO50( test_vmaxnm_f64(TyDF) );

   if (1) DO50( test_vminnm_f32(TySF) );
   if (1) DO50( test_vminnm_f64(TyDF) );

   if (1) DO50( test_vcvtn_s32_f64(TyDF) );
   if (1) DO50( test_vcvta_s32_f64(TyDF) );
   if (1) DO50( test_vcvtp_s32_f64(TyDF) );
   if (1) DO50( test_vcvtm_s32_f64(TyDF) );

   if (1) DO50( test_vcvtn_s32_f32(TySF) );
   if (1) DO50( test_vcvta_s32_f32(TySF) );
   if (1) DO50( test_vcvtp_s32_f32(TySF) );
   if (1) DO50( test_vcvtm_s32_f32(TySF) );

   if (1) DO50( test_vcvtn_u32_f64(TyDF) );
   if (1) DO50( test_vcvta_u32_f64(TyDF) );
   if (1) DO50( test_vcvtp_u32_f64(TyDF) );
   if (1) DO50( test_vcvtm_u32_f64(TyDF) );

   if (1) DO50( test_vcvtn_u32_f32(TySF) );
   if (1) DO50( test_vcvta_u32_f32(TySF) );
   if (1) DO50( test_vcvtp_u32_f32(TySF) );
   if (1) DO50( test_vcvtm_u32_f32(TySF) );

   if (0) DO50( test_vcvtb_f64_f16(TyDF) );
   if (0) DO50( test_vcvtt_f64_f16(TyDF) );

   if (0) DO50( test_vcvtb_f16_f64(TyHF) );
   if (0) DO50( test_vcvtt_f16_f64(TyHF) );

   if (1) DO50( test_vrintzeq_f64_f64(TyDF) );
   if (1) DO50( test_vrintzne_f64_f64(TyDF) );
   if (1) DO50( test_vrintzal_f64_f64(TyDF) );

   if (1) DO50( test_vrintreq_f64_f64(TyDF) );
   if (1) DO50( test_vrintrne_f64_f64(TyDF) );
   if (1) DO50( test_vrintral_f64_f64(TyDF) );

   if (1) DO50( test_vrintxeq_f64_f64(TyDF) );
   if (1) DO50( test_vrintxne_f64_f64(TyDF) );
   if (1) DO50( test_vrintxal_f64_f64(TyDF) );

   if (1) DO50( test_vrintzeq_f32_f32(TySF) );
   if (1) DO50( test_vrintzne_f32_f32(TySF) );
   if (1) DO50( test_vrintzal_f32_f32(TySF) );

   if (1) DO50( test_vrintreq_f32_f32(TySF) );
   if (1) DO50( test_vrintrne_f32_f32(TySF) );
   if (1) DO50( test_vrintral_f32_f32(TySF) );

   if (1) DO50( test_vrintxeq_f32_f32(TySF) );
   if (1) DO50( test_vrintxne_f32_f32(TySF) );
   if (1) DO50( test_vrintxal_f32_f32(TySF) );

   if (1) DO50( test_vrintn_f64_f64(TyDF) );
   if (1) DO50( test_vrinta_f64_f64(TyDF) );
   if (1) DO50( test_vrintp_f64_f64(TyDF) );
   if (1) DO50( test_vrintm_f64_f64(TyDF) );

   if (1) DO50( test_vrintn_f32_f32(TySF) );
   if (1) DO50( test_vrinta_f32_f32(TySF) );
   if (1) DO50( test_vrintp_f32_f32(TySF) );
   if (1) DO50( test_vrintm_f32_f32(TySF) );

   if (1) DO50( test_vmaxnm_f32_vec64(TySF) );
   if (1) DO50( test_vmaxnm_f32_vec128(TySF) );

   if (1) DO50( test_vminnm_f32_vec64(TySF) );
   if (1) DO50( test_vminnm_f32_vec128(TySF) );

   if (1) DO50( test_vcvtn_s32_f32_vec64(TySF) );
   if (1) DO50( test_vcvta_s32_f32_vec64(TySF) );
   if (1) DO50( test_vcvtp_s32_f32_vec64(TySF) );
   if (1) DO50( test_vcvtm_s32_f32_vec64(TySF) );

   if (1) DO50( test_vcvtn_s32_f32_vec128(TySF) );
   if (1) DO50( test_vcvta_s32_f32_vec128(TySF) );
   if (1) DO50( test_vcvtp_s32_f32_vec128(TySF) );
   if (1) DO50( test_vcvtm_s32_f32_vec128(TySF) );

   if (1) DO50( test_vcvtn_u32_f32_vec64(TySF) );
   if (1) DO50( test_vcvta_u32_f32_vec64(TySF) );
   if (1) DO50( test_vcvtp_u32_f32_vec64(TySF) );
   if (1) DO50( test_vcvtm_u32_f32_vec64(TySF) );

   if (1) DO50( test_vcvtn_u32_f32_vec128(TySF) );
   if (1) DO50( test_vcvta_u32_f32_vec128(TySF) );
   if (1) DO50( test_vcvtp_u32_f32_vec128(TySF) );
   if (1) DO50( test_vcvtm_u32_f32_vec128(TySF) );

   if (1) DO50( test_vrintn_f32_f32_vec64(TySF) );
   if (1) DO50( test_vrinta_f32_f32_vec64(TySF) );
   if (1) DO50( test_vrintp_f32_f32_vec64(TySF) );
   if (1) DO50( test_vrintm_f32_f32_vec64(TySF) );
   if (1) DO50( test_vrintz_f32_f32_vec64(TySF) );
   if (1) DO50( test_vrintx_f32_f32_vec64(TySF) );

   if (1) DO50( test_vrintn_f32_f32_vec128(TySF) );
   if (1) DO50( test_vrinta_f32_f32_vec128(TySF) );
   if (1) DO50( test_vrintp_f32_f32_vec128(TySF) );
   if (1) DO50( test_vrintm_f32_f32_vec128(TySF) );
   if (1) DO50( test_vrintz_f32_f32_vec128(TySF) );
   if (1) DO50( test_vrintx_f32_f32_vec128(TySF) );

   return 0;
}
