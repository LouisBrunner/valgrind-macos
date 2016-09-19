#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include <math.h>
#include "tests/malloc.h"

typedef  unsigned char           UChar;
typedef  unsigned int            UInt;
typedef  unsigned long int       UWord;
typedef  unsigned long long int  ULong;
typedef  double                  Double;
typedef  float                   Float;

#define IS_32_ALIGNED(_ptr) (0 == (0x1F & (UWord)(_ptr)))

typedef  union { UChar u8[16]; UInt u32[4]; Float f32[4]; Double f64[2]; } XMM;
typedef  union { UChar u8[32]; UInt u32[8]; XMM xmm[2]; }  YMM;
typedef  struct {  YMM r1; YMM r2; YMM r3; YMM r4; YMM m; }  Block;

void showFloat ( XMM* vec, int idx )
{
   Float f = vec->f32[idx];
   int neg = signbit (f);
   char sign = neg != 0 ? '-' : ' ';
   switch (fpclassify (f)) {
      case FP_NORMAL: {
         for (int i = idx * 4 + 3; i >= idx * 4; i--)
            printf("%02x", (UInt)vec->u8[i]);
         break;
      }
      case FP_INFINITE: {
         printf ("[ %cINF ]", sign);
         break;
      }
      case FP_ZERO: {
         printf ("[%cZERO ]", sign);
         break;
      }
      case FP_NAN: {
         printf ("[  NAN ]");
         break;
      }
      default: {
         printf ("[%cSUBNR]", sign);
         break;
      }
   }
}

void showDouble ( XMM* vec, int idx )
{
   Double d = vec->f64[idx];
   int neg = signbit (d);
   char sign = neg != 0 ? '-' : ' ';
   switch (fpclassify (d)) {
      case FP_NORMAL: {
         for (int i = idx * 8 + 7; i >= idx * 8; i--)
            printf("%02x", (UInt)vec->u8[i]);
         break;
      }
      case FP_INFINITE: {
         printf ("[     %cINF     ]", sign);
         break;
      }
      case FP_ZERO: {
         printf ("[    %cZERO     ]", sign);
         break;
      }
      case FP_NAN: {
         printf ("[      NAN     ]");
         break;
      }
      default: {
         printf ("[  %cSUBNORMAL  ]", sign);
         break;
      }
   }
}

void showXMM ( XMM* vec, int isDouble )
{
   if (isDouble) {
     showDouble ( vec, 1 );
     printf (".");
     showDouble ( vec, 0 );
   } else {
     showFloat ( vec, 3 );
     printf (".");
     showFloat ( vec, 2 );
     printf (".");
     showFloat ( vec, 1 );
     printf (".");
     showFloat ( vec, 0 );
   }
}

void showYMM ( YMM* vec, int isDouble )
{
   assert(IS_32_ALIGNED(vec));
   showXMM ( &vec->xmm[1], isDouble );
   printf(".");
   showXMM ( &vec->xmm[0], isDouble );
}

void showBlock ( char* msg, Block* block, int isDouble )
{
   printf("  %s\n", msg);
   printf("r1: "); showYMM(&block->r1, isDouble); printf("\n");
   printf("r2: "); showYMM(&block->r2, isDouble); printf("\n");
   printf("r3: "); showYMM(&block->r3, isDouble); printf("\n");
   printf("r4: "); showYMM(&block->r4, isDouble); printf("\n");
   printf(" m: "); showYMM(&block->m, isDouble); printf("\n");
}

static Double special_values[10];

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

static void init_special_values ( void )
{
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
}

void specialFBlock ( Block* b )
{
   int i;
   Float* p = (Float*)b;
   for (i = 0; i < sizeof(Block) / sizeof(Float); i++)
      p[i] = (Float) special_values[i % 10];
}

void specialDBlock ( Block* b )
{
   int i;
   Double* p = (Double*)b;
   for (i = 0; i < sizeof(Block) / sizeof(Double); i++)
      p[i] = special_values[i % 10];
}

UChar randUChar ( void )
{
   static UInt seed = 80021;
   seed = 1103515245 * seed + 12345;
   return (seed >> 17) & 0xFF;
}

void randBlock ( Block* b )
{
   int i;
   UChar* p = (UChar*)b;
   for (i = 0; i < sizeof(Block); i++)
      p[i] = randUChar();
}

void oneBlock ( Block* b )
{
   int i;
   UChar* p = (UChar*)b;
   for (i = 0; i < sizeof(Block); i++)
      p[i] = 1;
}

#define GEN_test(_name, _instr, _isD) \
   __attribute__ ((noinline)) void \
   test_##_name ( const char *n, Block* b) \
   { \
      printf("%s %s\n", #_name, n); \
      showBlock("before", b, _isD); \
      __asm__ __volatile__( \
          "vmovdqa   0(%0),%%ymm7"  "\n\t" \
          "vmovdqa  32(%0),%%ymm8"  "\n\t" \
          "vmovdqa  64(%0),%%ymm6"  "\n\t" \
          "vmovdqa  96(%0),%%ymm9"  "\n\t" \
          "leaq    128(%0),%%r14"   "\n\t" \
          _instr "\n\t" \
          "vmovdqa %%ymm7,  0(%0)"  "\n\t" \
          "vmovdqa %%ymm8, 32(%0)"  "\n\t" \
          "vmovdqa %%ymm6, 64(%0)"  "\n\t" \
          "vmovdqa %%ymm9, 96(%0)"  "\n\t" \
          : /*OUT*/  \
          : /*IN*/"r"(b) \
          : /*TRASH*/"xmm7","xmm8","xmm6","xmm9","r14","memory","cc" \
       ); \
       showBlock("after", b, _isD); \
       printf("\n"); \
    }

/* All these defines do the same thing (and someone with stronger
   preprocessor foo could probably express things much smaller).
   They generate 4 different functions to test 4 variants of an
   fma4 instruction. One with as input 4 registers, one where
   the output register is also one of the input registers and
   two versions where different inputs are a memory location.
   The xmm variants create 128 versions, the ymm variants 256. */

#define GEN_test_VFMADDPD_xmm(_name) \
   GEN_test(_name##_xmm, \
            "vfmaddpd %%xmm7,%%xmm8,%%xmm6,%%xmm9", 1); \
   GEN_test(_name##_xmm_src_dst, \
            "vfmaddpd %%xmm7,%%xmm8,%%xmm9,%%xmm9", 1); \
   GEN_test(_name##_xmm_mem1, \
            "vfmaddpd (%%r14),%%xmm8,%%xmm6,%%xmm9", 1); \
   GEN_test(_name##_xmm_mem2, \
            "vfmaddpd %%xmm8,(%%r14),%%xmm6,%%xmm9", 1);
GEN_test_VFMADDPD_xmm(VFMADDPD)

#define GEN_test_VFMADDPD_ymm(_name) \
   GEN_test(_name##_ymm, \
            "vfmaddpd %%ymm7,%%ymm8,%%ymm6,%%ymm9", 1); \
   GEN_test(_name##_ymm_src_dst, \
            "vfmaddpd %%ymm7,%%ymm8,%%ymm9,%%ymm9", 1); \
   GEN_test(_name##_ymm_mem1, \
            "vfmaddpd (%%r14),%%ymm8,%%ymm6,%%ymm9", 1); \
   GEN_test(_name##_ymm_mem2, \
            "vfmaddpd %%ymm8,(%%r14),%%ymm6,%%ymm9", 1);
GEN_test_VFMADDPD_ymm(VFMADDPD)

#define GEN_test_VFMADDPS_xmm(_name) \
   GEN_test(_name##_xmm, \
            "vfmaddps %%xmm7,%%xmm8,%%xmm6,%%xmm9", 0); \
   GEN_test(_name##_xmm_src_dst, \
            "vfmaddps %%xmm7,%%xmm8,%%xmm9,%%xmm9", 0); \
   GEN_test(_name##_xmm_mem1, \
            "vfmaddps (%%r14),%%xmm8,%%xmm6,%%xmm9", 0); \
   GEN_test(_name##_xmm_mem2, \
            "vfmaddps %%xmm8,(%%r14),%%xmm6,%%xmm9", 0);
GEN_test_VFMADDPS_xmm(VFMADDPS)

#define GEN_test_VFMADDPS_ymm(_name) \
   GEN_test(_name##_ymm, \
            "vfmaddps %%ymm7,%%ymm8,%%ymm6,%%ymm9", 0); \
   GEN_test(_name##_ymm_src_dst, \
            "vfmaddps %%ymm7,%%ymm8,%%ymm9,%%ymm9", 0); \
   GEN_test(_name##_ymm_mem1, \
            "vfmaddps (%%r14),%%ymm8,%%ymm6,%%ymm9", 0); \
   GEN_test(_name##_ymm_mem2, \
            "vfmaddps %%ymm8,(%%r14),%%ymm6,%%ymm9", 0);
GEN_test_VFMADDPS_ymm(VFMADDPS)

#define GEN_test_VFMADDSD_xmm(_name) \
   GEN_test(_name##_xmm, \
            "vfmaddsd %%xmm7,%%xmm8,%%xmm6,%%xmm9", 1); \
   GEN_test(_name##_xmm_src_dst, \
            "vfmaddsd %%xmm7,%%xmm8,%%xmm9,%%xmm9", 1); \
   GEN_test(_name##_xmm_mem1, \
            "vfmaddsd (%%r14),%%xmm8,%%xmm6,%%xmm9", 1); \
   GEN_test(_name##_xmm_mem2, \
            "vfmaddsd %%xmm8,(%%r14),%%xmm6,%%xmm9", 1);
GEN_test_VFMADDSD_xmm(VFMADDSD)

#define GEN_test_VFMADDSS_xmm(_name) \
   GEN_test(_name##_xmm, \
            "vfmaddss %%xmm7,%%xmm8,%%xmm6,%%xmm9", 0); \
   GEN_test(_name##_xmm_src_dst, \
            "vfmaddss %%xmm7,%%xmm8,%%xmm9,%%xmm9", 0); \
   GEN_test(_name##_xmm_mem1, \
            "vfmaddss (%%r14),%%xmm8,%%xmm6,%%xmm9", 0); \
   GEN_test(_name##_xmm_mem2, \
            "vfmaddss %%xmm8,(%%r14),%%xmm6,%%xmm9", 0);
GEN_test_VFMADDSS_xmm(VFMADDSS)

#define GEN_test_VFMADDSUBPD_xmm(_name) \
   GEN_test(_name##_xmm, \
            "vfmaddsubpd %%xmm7,%%xmm8,%%xmm6,%%xmm9", 1); \
   GEN_test(_name##_xmm_src_dst, \
            "vfmaddsubpd %%xmm7,%%xmm8,%%xmm9,%%xmm9", 1); \
   GEN_test(_name##_xmm_mem1, \
            "vfmaddsubpd (%%r14),%%xmm8,%%xmm6,%%xmm9", 1); \
   GEN_test(_name##_xmm_mem2, \
            "vfmaddsubpd %%xmm8,(%%r14),%%xmm6,%%xmm9", 1);
GEN_test_VFMADDSUBPD_xmm(VFMADDSUBPD)

#define GEN_test_VFMADDSUBPD_ymm(_name) \
   GEN_test(_name##_ymm, \
            "vfmaddsubpd %%ymm7,%%ymm8,%%ymm6,%%ymm9", 1); \
   GEN_test(_name##_ymm_src_dst, \
            "vfmaddsubpd %%ymm7,%%ymm8,%%ymm9,%%ymm9", 1); \
   GEN_test(_name##_ymm_mem1, \
            "vfmaddsubpd (%%r14),%%ymm8,%%ymm6,%%ymm9", 1); \
   GEN_test(_name##_ymm_mem2, \
            "vfmaddsubpd %%ymm8,(%%r14),%%ymm6,%%ymm9", 1);
GEN_test_VFMADDSUBPD_ymm(VFMADDSUBPD)

#define GEN_test_VFMADDSUBPS_xmm(_name) \
   GEN_test(_name##_xmm, \
            "vfmaddsubps %%xmm7,%%xmm8,%%xmm6,%%xmm9", 0); \
   GEN_test(_name##_xmm_src_dst, \
            "vfmaddsubps %%xmm7,%%xmm8,%%xmm9,%%xmm9", 0); \
   GEN_test(_name##_xmm_mem1, \
            "vfmaddsubps (%%r14),%%xmm8,%%xmm6,%%xmm9", 0); \
   GEN_test(_name##_xmm_mem2, \
            "vfmaddsubps %%xmm8,(%%r14),%%xmm6,%%xmm9", 0);
GEN_test_VFMADDSUBPS_xmm(VFMADDSUBPS)

#define GEN_test_VFMADDSUBPS_ymm(_name) \
   GEN_test(_name##_ymm, \
            "vfmaddsubps %%ymm7,%%ymm8,%%ymm6,%%ymm9", 0); \
   GEN_test(_name##_ymm_src_dst, \
            "vfmaddsubps %%ymm7,%%ymm8,%%ymm9,%%ymm9", 0); \
   GEN_test(_name##_ymm_mem1, \
            "vfmaddsubps (%%r14),%%ymm8,%%ymm6,%%ymm9", 0); \
   GEN_test(_name##_ymm_mem2, \
            "vfmaddsubps %%ymm8,(%%r14),%%ymm6,%%ymm9", 0);
GEN_test_VFMADDSUBPS_ymm(VFMADDSUBPS)

#define GEN_test_VFMSUBADDPD_xmm(_name) \
   GEN_test(_name##_xmm, \
            "vfmsubaddpd %%xmm7,%%xmm8,%%xmm6,%%xmm9", 1); \
   GEN_test(_name##_xmm_src_dst, \
            "vfmsubaddpd %%xmm7,%%xmm8,%%xmm9,%%xmm9", 1); \
   GEN_test(_name##_xmm_mem1, \
            "vfmsubaddpd (%%r14),%%xmm8,%%xmm6,%%xmm9", 1); \
   GEN_test(_name##_xmm_mem2, \
            "vfmsubaddpd %%xmm8,(%%r14),%%xmm6,%%xmm9", 1);
GEN_test_VFMSUBADDPD_xmm(VFMSUBADDPD)

#define GEN_test_VFMSUBADDPD_ymm(_name) \
   GEN_test(_name##_ymm, \
            "vfmsubaddpd %%ymm7,%%ymm8,%%ymm6,%%ymm9", 1); \
   GEN_test(_name##_ymm_src_dst, \
            "vfmsubaddpd %%ymm7,%%ymm8,%%ymm9,%%ymm9", 1); \
   GEN_test(_name##_ymm_mem1, \
            "vfmsubaddpd (%%r14),%%ymm8,%%ymm6,%%ymm9", 1); \
   GEN_test(_name##_ymm_mem2, \
            "vfmsubaddpd %%ymm8,(%%r14),%%ymm6,%%ymm9", 1);
GEN_test_VFMSUBADDPD_ymm(VFMSUBADDPD)

#define GEN_test_VFMSUBADDPS_xmm(_name) \
   GEN_test(_name##_xmm, \
            "vfmsubaddps %%xmm7,%%xmm8,%%xmm6,%%xmm9", 0); \
   GEN_test(_name##_xmm_src_dst, \
            "vfmsubaddps %%xmm7,%%xmm8,%%xmm9,%%xmm9", 0); \
   GEN_test(_name##_xmm_mem1, \
            "vfmsubaddps (%%r14),%%xmm8,%%xmm6,%%xmm9", 0); \
   GEN_test(_name##_xmm_mem2, \
            "vfmsubaddps %%xmm8,(%%r14),%%xmm6,%%xmm9", 0);
GEN_test_VFMSUBADDPS_xmm(VFMSUBADDPS)

#define GEN_test_VFMSUBADDPS_ymm(_name) \
   GEN_test(_name##_ymm, \
            "vfmsubaddps %%ymm7,%%ymm8,%%ymm6,%%ymm9", 0); \
   GEN_test(_name##_ymm_src_dst, \
            "vfmsubaddps %%ymm7,%%ymm8,%%ymm9,%%ymm9", 0); \
   GEN_test(_name##_ymm_mem1, \
            "vfmsubaddps (%%r14),%%ymm8,%%ymm6,%%ymm9", 0); \
   GEN_test(_name##_ymm_mem2, \
            "vfmsubaddps %%ymm8,(%%r14),%%ymm6,%%ymm9", 0);
GEN_test_VFMSUBADDPS_ymm(VFMSUBADDPS)

#define GEN_test_VFMSUBPD_xmm(_name) \
   GEN_test(_name##_xmm, \
            "vfmsubpd %%xmm7,%%xmm8,%%xmm6,%%xmm9", 1); \
   GEN_test(_name##_xmm_src_dst, \
            "vfmsubpd %%xmm7,%%xmm8,%%xmm9,%%xmm9", 1); \
   GEN_test(_name##_xmm_mem1, \
            "vfmsubpd (%%r14),%%xmm8,%%xmm6,%%xmm9", 1); \
   GEN_test(_name##_xmm_mem2, \
            "vfmsubpd %%xmm8,(%%r14),%%xmm6,%%xmm9", 1);
GEN_test_VFMSUBPD_xmm(VFMSUBPD)

#define GEN_test_VFMSUBPD_ymm(_name) \
   GEN_test(_name##_ymm, \
            "vfmsubpd %%ymm7,%%ymm8,%%ymm6,%%ymm9", 1); \
   GEN_test(_name##_ymm_src_dst, \
            "vfmsubpd %%ymm7,%%ymm8,%%ymm9,%%ymm9", 1); \
   GEN_test(_name##_ymm_mem1, \
            "vfmsubpd (%%r14),%%ymm8,%%ymm6,%%ymm9", 1); \
   GEN_test(_name##_ymm_mem2, \
            "vfmsubpd %%ymm8,(%%r14),%%ymm6,%%ymm9", 1);
GEN_test_VFMSUBPD_ymm(VFMSUBPD)

#define GEN_test_VFMSUBPS_xmm(_name) \
   GEN_test(_name##_xmm, \
            "vfmsubps %%xmm7,%%xmm8,%%xmm6,%%xmm9", 0); \
   GEN_test(_name##_xmm_src_dst, \
            "vfmsubps %%xmm7,%%xmm8,%%xmm9,%%xmm9", 0); \
   GEN_test(_name##_xmm_mem1, \
            "vfmsubps (%%r14),%%xmm8,%%xmm6,%%xmm9", 0); \
   GEN_test(_name##_xmm_mem2, \
            "vfmsubps %%xmm8,(%%r14),%%xmm6,%%xmm9", 0);
GEN_test_VFMSUBPS_xmm(VFMSUBPS)

#define GEN_test_VFMSUBPS_ymm(_name) \
   GEN_test(_name##_ymm, \
            "vfmsubps %%ymm7,%%ymm8,%%ymm6,%%ymm9", 0); \
   GEN_test(_name##_ymm_src_dst, \
            "vfmsubps %%ymm7,%%ymm8,%%ymm9,%%ymm9", 0); \
   GEN_test(_name##_ymm_mem1, \
            "vfmsubps (%%r14),%%ymm8,%%ymm6,%%ymm9", 0); \
   GEN_test(_name##_ymm_mem2, \
            "vfmsubps %%ymm8,(%%r14),%%ymm6,%%ymm9", 0);
GEN_test_VFMSUBPS_ymm(VFMSUBPS)

#define GEN_test_VFMSUBSD_xmm(_name) \
   GEN_test(_name##_xmm, \
            "vfmsubsd %%xmm7,%%xmm8,%%xmm6,%%xmm9", 1); \
   GEN_test(_name##_xmm_src_dst, \
            "vfmsubsd %%xmm7,%%xmm8,%%xmm9,%%xmm9", 1); \
   GEN_test(_name##_xmm_mem1, \
            "vfmsubsd (%%r14),%%xmm8,%%xmm6,%%xmm9", 1); \
   GEN_test(_name##_xmm_mem2, \
            "vfmsubsd %%xmm8,(%%r14),%%xmm6,%%xmm9", 1);
GEN_test_VFMSUBSD_xmm(VFMSUBSD)

#define GEN_test_VFMSUBSS_xmm(_name) \
   GEN_test(_name##_xmm, \
            "vfmsubss %%xmm7,%%xmm8,%%xmm6,%%xmm9", 0); \
   GEN_test(_name##_xmm_src_dst, \
            "vfmsubss %%xmm7,%%xmm8,%%xmm9,%%xmm9", 0); \
   GEN_test(_name##_xmm_mem1, \
            "vfmsubss (%%r14),%%xmm8,%%xmm6,%%xmm9", 0); \
   GEN_test(_name##_xmm_mem2, \
            "vfmsubss %%xmm8,(%%r14),%%xmm6,%%xmm9", 0);
GEN_test_VFMSUBSS_xmm(VFMSUBSS)

#define GEN_test_VFNMADDPD_xmm(_name) \
   GEN_test(_name##_xmm, \
            "vfnmaddpd %%xmm7,%%xmm8,%%xmm6,%%xmm9", 1); \
   GEN_test(_name##_xmm_src_dst, \
            "vfnmaddpd %%xmm7,%%xmm8,%%xmm9,%%xmm9", 1); \
   GEN_test(_name##_xmm_mem1, \
            "vfnmaddpd (%%r14),%%xmm8,%%xmm6,%%xmm9", 1); \
   GEN_test(_name##_xmm_mem2, \
            "vfnmaddpd %%xmm8,(%%r14),%%xmm6,%%xmm9", 1);
GEN_test_VFNMADDPD_xmm(VFNMADDPD)

#define GEN_test_VFNMADDPD_ymm(_name) \
   GEN_test(_name##_ymm, \
            "vfnmaddpd %%ymm7,%%ymm8,%%ymm6,%%ymm9", 1); \
   GEN_test(_name##_ymm_src_dst, \
            "vfnmaddpd %%ymm7,%%ymm8,%%ymm9,%%ymm9", 1); \
   GEN_test(_name##_ymm_mem1, \
            "vfnmaddpd (%%r14),%%ymm8,%%ymm6,%%ymm9", 1); \
   GEN_test(_name##_ymm_mem2, \
            "vfnmaddpd %%ymm8,(%%r14),%%ymm6,%%ymm9", 1);
GEN_test_VFNMADDPD_ymm(VFNMADDPD)

#define GEN_test_VFNMADDPS_xmm(_name) \
   GEN_test(_name##_xmm, \
            "vfnmaddps %%xmm7,%%xmm8,%%xmm6,%%xmm9", 0); \
   GEN_test(_name##_xmm_src_dst, \
            "vfnmaddps %%xmm7,%%xmm8,%%xmm9,%%xmm9", 0); \
   GEN_test(_name##_xmm_mem1, \
            "vfnmaddps (%%r14),%%xmm8,%%xmm6,%%xmm9", 0); \
   GEN_test(_name##_xmm_mem2, \
            "vfnmaddps %%xmm8,(%%r14),%%xmm6,%%xmm9", 0);
GEN_test_VFNMADDPS_xmm(VFNMADDPS)

#define GEN_test_VFNMADDPS_ymm(_name) \
   GEN_test(_name##_ymm, \
            "vfnmaddps %%ymm7,%%ymm8,%%ymm6,%%ymm9", 0); \
   GEN_test(_name##_ymm_src_dst, \
            "vfnmaddps %%ymm7,%%ymm8,%%ymm9,%%ymm9", 0); \
   GEN_test(_name##_ymm_mem1, \
            "vfnmaddps (%%r14),%%ymm8,%%ymm6,%%ymm9", 0); \
   GEN_test(_name##_ymm_mem2, \
            "vfnmaddps %%ymm8,(%%r14),%%ymm6,%%ymm9", 0);
GEN_test_VFNMADDPS_ymm(VFNMADDPS)

#define GEN_test_VFNMADDSD_xmm(_name) \
   GEN_test(_name##_xmm, \
            "vfnmaddsd %%xmm7,%%xmm8,%%xmm6,%%xmm9", 1); \
   GEN_test(_name##_xmm_src_dst, \
            "vfnmaddsd %%xmm7,%%xmm8,%%xmm9,%%xmm9", 1); \
   GEN_test(_name##_xmm_mem1, \
            "vfnmaddsd (%%r14),%%xmm8,%%xmm6,%%xmm9", 1); \
   GEN_test(_name##_xmm_mem2, \
            "vfnmaddsd %%xmm8,(%%r14),%%xmm6,%%xmm9", 1);
GEN_test_VFNMADDSD_xmm(VFNMADDSD)

#define GEN_test_VFNMADDSS_xmm(_name) \
   GEN_test(_name##_xmm, \
            "vfnmaddss %%xmm7,%%xmm8,%%xmm6,%%xmm9", 0); \
   GEN_test(_name##_xmm_src_dst, \
            "vfnmaddss %%xmm7,%%xmm8,%%xmm9,%%xmm9", 0); \
   GEN_test(_name##_xmm_mem1, \
            "vfnmaddss (%%r14),%%xmm8,%%xmm6,%%xmm9", 0); \
   GEN_test(_name##_xmm_mem2, \
            "vfnmaddss %%xmm8,(%%r14),%%xmm6,%%xmm9", 0);
GEN_test_VFNMADDSS_xmm(VFNMADDSS)

#define GEN_test_VFNMSUBPD_xmm(_name) \
   GEN_test(_name##_xmm, \
            "vfnmsubpd %%xmm7,%%xmm8,%%xmm6,%%xmm9", 1); \
   GEN_test(_name##_xmm_src_dst, \
            "vfnmsubpd %%xmm7,%%xmm8,%%xmm9,%%xmm9", 1); \
   GEN_test(_name##_xmm_mem1, \
            "vfnmsubpd (%%r14),%%xmm8,%%xmm6,%%xmm9", 1); \
   GEN_test(_name##_xmm_mem2, \
            "vfnmsubpd %%xmm8,(%%r14),%%xmm6,%%xmm9", 1);
GEN_test_VFNMSUBPD_xmm(VFNMSUBPD)

#define GEN_test_VFNMSUBPD_ymm(_name) \
   GEN_test(_name##_ymm, \
            "vfnmsubpd %%ymm7,%%ymm8,%%ymm6,%%ymm9", 1); \
   GEN_test(_name##_ymm_src_dst, \
            "vfnmsubpd %%ymm7,%%ymm8,%%ymm9,%%ymm9", 1); \
   GEN_test(_name##_ymm_mem1, \
            "vfnmsubpd (%%r14),%%ymm8,%%ymm6,%%ymm9", 1); \
   GEN_test(_name##_ymm_mem2, \
            "vfnmsubpd %%ymm8,(%%r14),%%ymm6,%%ymm9", 1);
GEN_test_VFNMSUBPD_ymm(VFNMSUBPD)

#define GEN_test_VFNMSUBPS_xmm(_name) \
   GEN_test(_name##_xmm, \
            "vfnmsubps %%xmm7,%%xmm8,%%xmm6,%%xmm9", 0); \
   GEN_test(_name##_xmm_src_dst, \
            "vfnmsubps %%xmm7,%%xmm8,%%xmm9,%%xmm9", 0); \
   GEN_test(_name##_xmm_mem1, \
            "vfnmsubps (%%r14),%%xmm8,%%xmm6,%%xmm9", 0); \
   GEN_test(_name##_xmm_mem2, \
            "vfnmsubps %%xmm8,(%%r14),%%xmm6,%%xmm9", 0);
GEN_test_VFNMSUBPS_xmm(VFNMSUBPS)

#define GEN_test_VFNMSUBPS_ymm(_name) \
   GEN_test(_name##_ymm, \
            "vfnmsubps %%ymm7,%%ymm8,%%ymm6,%%ymm9", 0); \
   GEN_test(_name##_ymm_src_dst, \
            "vfnmsubps %%ymm7,%%ymm8,%%ymm9,%%ymm9", 0); \
   GEN_test(_name##_ymm_mem1, \
            "vfnmsubps (%%r14),%%ymm8,%%ymm6,%%ymm9", 0); \
   GEN_test(_name##_ymm_mem2, \
            "vfnmsubps %%ymm8,(%%r14),%%ymm6,%%ymm9", 0);
GEN_test_VFNMSUBPS_ymm(VFNMSUBPS)

#define GEN_test_VFNMSUBSD_xmm(_name) \
   GEN_test(_name##_xmm, \
            "vfnmsubsd %%xmm7,%%xmm8,%%xmm6,%%xmm9", 1); \
   GEN_test(_name##_xmm_src_dst, \
            "vfnmsubsd %%xmm7,%%xmm8,%%xmm9,%%xmm9", 1); \
   GEN_test(_name##_xmm_mem1, \
            "vfnmsubsd (%%r14),%%xmm8,%%xmm6,%%xmm9", 1); \
   GEN_test(_name##_xmm_mem2, \
            "vfnmsubsd %%xmm8,(%%r14),%%xmm6,%%xmm9", 1);
GEN_test_VFNMSUBSD_xmm(VFNMSUBSD)

#define GEN_test_VFNMSUBSS_xmm(_name) \
   GEN_test(_name##_xmm, \
            "vfnmsubss %%xmm7,%%xmm8,%%xmm6,%%xmm9", 0); \
   GEN_test(_name##_xmm_src_dst, \
            "vfnmsubss %%xmm7,%%xmm8,%%xmm9,%%xmm9", 0); \
   GEN_test(_name##_xmm_mem1, \
            "vfnmsubss (%%r14),%%xmm8,%%xmm6,%%xmm9", 0); \
   GEN_test(_name##_xmm_mem2, \
            "vfnmsubss %%xmm8,(%%r14),%%xmm6,%%xmm9", 0);
GEN_test_VFNMSUBSS_xmm(VFNMSUBSS)

#define DO_test_block(_name, _sub, _bname, _block) \
   test_##_name##_##_sub(_bname,_block);

#define DO_test(_name, _sub, _isD) { \
   Block* b = memalign32(sizeof(Block)); \
   oneBlock(b); \
   DO_test_block(_name, _sub, "ones", b); \
   if (_isD) { \
      specialDBlock(b); \
      DO_test_block(_name, _sub, "specialD", b); \
   } else { \
      specialFBlock(b); \
      DO_test_block(_name, _sub, "specialF", b); \
   } \
   randBlock(b); \
   DO_test_block(_name, _sub, "rand", b); \
   free(b); \
}

#define DO_tests_xmm(_name,_isD) \
   DO_test(_name, xmm, _isD); \
   DO_test(_name, xmm_src_dst, _isD); \
   DO_test(_name, xmm_mem1, _isD); \
   DO_test(_name, xmm_mem2, _isD);

#define DO_tests_ymm(_name,_isD) \
   DO_test(_name, ymm, _isD); \
   DO_test(_name, ymm_src_dst, _isD); \
   DO_test(_name, ymm_mem1, _isD); \
   DO_test(_name, ymm_mem2, _isD);

int main ( void )
{
  init_special_values();

  // 128
  DO_tests_xmm(VFMADDPD, 1);
  DO_tests_xmm(VFMADDPS, 0);
  DO_tests_xmm(VFMADDSD, 1);
  DO_tests_xmm(VFMADDSS, 0);
  DO_tests_xmm(VFMADDSUBPD, 1);
  DO_tests_xmm(VFMADDSUBPS, 0);
  DO_tests_xmm(VFMSUBADDPD, 1);
  DO_tests_xmm(VFMSUBADDPS, 0);
  DO_tests_xmm(VFMSUBPD, 1);
  DO_tests_xmm(VFMSUBPS, 0);
  DO_tests_xmm(VFMSUBSD, 1);
  DO_tests_xmm(VFMSUBSS, 0);
  DO_tests_xmm(VFNMADDPD, 1);
  DO_tests_xmm(VFNMADDPS, 0);
  DO_tests_xmm(VFNMADDSD, 1);
  DO_tests_xmm(VFNMADDSS, 0);
  DO_tests_xmm(VFNMSUBPD, 1);
  DO_tests_xmm(VFNMSUBPS, 0);
  DO_tests_xmm(VFNMSUBSD, 1);
  DO_tests_xmm(VFNMSUBSS, 0);

  // 256
  /*
  DO_tests_ymm(VFMADDPD, 1);
  DO_tests_ymm(VFMADDPS, 0);
  DO_tests_ymm(VFMADDSUBPD, 1);
  DO_tests_ymm(VFMADDSUBPS, 0);
  DO_tests_ymm(VFMSUBADDPD, 1);
  DO_tests_ymm(VFMSUBADDPS, 0);
  DO_tests_ymm(VFMSUBPD, 1);
  DO_tests_ymm(VFMSUBPS, 0);
  DO_tests_ymm(VFNMADDPD, 1);
  DO_tests_ymm(VFNMADDPS, 0);
  DO_tests_ymm(VFNMSUBPD, 1);
  DO_tests_ymm(VFNMSUBPS, 0);
  */

  return 0;
}
