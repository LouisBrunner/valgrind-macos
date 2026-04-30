/* Common infrastructure for SSE4 tests (both x86 and amd64) */

#ifndef __SSE4_COMMON_H
#define __SSE4_COMMON_H

#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include "tests/malloc.h"
#include <string.h>

typedef  unsigned char           V128[16];
typedef  unsigned int            UInt;
typedef  signed int              Int;
typedef  unsigned char           UChar;
typedef  unsigned long long int  ULong;

typedef  unsigned char           Bool;
#define False ((Bool)0)
#define True  ((Bool)1)


typedef
   struct {
      V128 arg1;
      V128 arg2;
      V128 res;
   }
   RRArgs;

typedef
   struct {
      V128 arg1;
      V128 res;
   }
   RMArgs;

static UChar randUChar ( void )
{
   static UInt seed = 80021;
   seed = 1103515245 * seed + 12345;
   return (seed >> 17) & 0xFF;
}


static ULong randULong ( void )
{
   Int i;
   ULong r = 0;
   for (i = 0; i < 8; i++) {
      r = (r << 8) | (ULong)(0xFF & randUChar());
   }
   return r;
}

static void randV128 ( V128* v )
{
   Int i;
   for (i = 0; i < 16; i++)
      (*v)[i] = randUChar();
}

static void showV128 ( V128* v )
{
   Int i;
   for (i = 15; i >= 0; i--)
      printf("%02x", (Int)(*v)[i]);
}

static void showMaskedV128 ( V128* v, V128* mask )
{
   Int i;
   for (i = 15; i >= 0; i--)
      printf("%02x", (Int)( ((*v)[i]) & ((*mask)[i]) ));
}

static void do64HLtoV128 ( /*OUT*/V128* res, ULong wHi, ULong wLo )
{
   // try to sidestep strict-aliasing snafus by memcpying explicitly
   UChar* p = (UChar*)res;
   memcpy(&p[8], (UChar*)&wHi, 8);
   memcpy(&p[0], (UChar*)&wLo, 8);
}

static void showIGVV( char* rOrM, char* op, Int imm,
                      ULong src64, V128* dst, V128* res )
{
   printf("%s %10s $%d ", rOrM, op, imm);
   printf("%016llx", src64);
   printf(" ");
   showV128(dst);
   printf(" ");
   showV128(res);
   printf("\n");
}

static void showIAG ( char* rOrM, char* op, Int imm,
                      V128* argL, ULong argR, ULong res )
{
   printf("%s %10s $%d ", rOrM, op, imm);
   showV128(argL);
   printf(" ");
   printf("%016llx", argR);
   printf(" ");
   printf("%016llx", res);
   printf("\n");
}

static void showIAA ( char* rOrM, char* op, Int imm, RRArgs* rra, V128* rmask )
{
   printf("%s %10s $%d ", rOrM, op, imm);
   showV128(&rra->arg1);
   printf(" ");
   showV128(&rra->arg2);
   printf(" ");
   showMaskedV128(&rra->res, rmask);
   printf("\n");
}

static void showAA ( char* rOrM, char* op, RRArgs* rra, V128* rmask )
{
   printf("%s %10s ", rOrM, op);
   showV128(&rra->arg1);
   printf(" ");
   showV128(&rra->arg2);
   printf(" ");
   showMaskedV128(&rra->res, rmask);
   printf("\n");
}

/* Note: these are little endian.  Hence first byte is the least
   significant byte of lane zero. */

/* Mask for insns where all result bits are non-approximated. */
static V128 AllMask  = { 0xFF,0xFF,0xFF,0xFF, 0xFF,0xFF,0xFF,0xFF,
                         0xFF,0xFF,0xFF,0xFF, 0xFF,0xFF,0xFF,0xFF };

/* Mark for insns which produce approximated vector short results. */
__attribute__((unused))
static V128 ApproxPS = { 0x00,0x00,0x80,0xFF, 0x00,0x00,0x80,0xFF,
                         0x00,0x00,0x80,0xFF, 0x00,0x00,0x80,0xFF };

/* Mark for insns which produce approximated scalar short results. */
__attribute__((unused))
static V128 ApproxSS = { 0x00,0x00,0x80,0xFF, 0xFF,0xFF,0xFF,0xFF,
                         0xFF,0xFF,0xFF,0xFF, 0xFF,0xFF,0xFF,0xFF };

static V128 fives    = { 0x55,0x55,0x55,0x55, 0x55,0x55,0x55,0x55,
                         0x55,0x55,0x55,0x55, 0x55,0x55,0x55,0x55 };

static V128 zeroes   = { 0x00,0x00,0x00,0x00, 0x00,0x00,0x00,0x00,
                         0x00,0x00,0x00,0x00, 0x00,0x00,0x00,0x00 };

/* Helper functions for creating special float values */
static inline double mkPosInf ( void ) { return 1.0 / 0.0; }
static inline double mkNegInf ( void ) { return -mkPosInf(); }
static inline double mkPosNan ( void ) { return 0.0 / 0.0; }
static inline double mkNegNan ( void ) { return -mkPosNan(); }

/* Macros for testing XMM register to register and memory to register operations */

/* Use xmm7 for both 32-bit x86 and amd64 (xmm8-15 don't exist in 32-bit mode) */
#ifdef __x86_64__
#define XMMREG_DST "xmm7"
#else
#define XMMREG_DST "xmm7"
#endif

#define DO_imm_r_r(_opname, _imm, _src, _dst)  \
   {  \
      V128 _tmp;  \
      __asm__ __volatile__(  \
         "movupd (%0), %%xmm2"    "\n\t"  \
         "movupd (%1), %%" XMMREG_DST   "\n\t"  \
         _opname " $" #_imm ", %%xmm2, %%" XMMREG_DST  "\n\t"  \
         "movupd %%" XMMREG_DST ", (%2)" "\n"  \
         : /*out*/ : /*in*/ "r"(&(_src)), "r"(&(_dst)), "r"(&(_tmp))  \
         : "cc", "memory", "xmm2", XMMREG_DST                            \
      );  \
      RRArgs rra;  \
      memcpy(&rra.arg1, &(_src), sizeof(V128));  \
      memcpy(&rra.arg2, &(_dst), sizeof(V128));  \
      memcpy(&rra.res,  &(_tmp), sizeof(V128));  \
      showIAA("r", (_opname), (_imm), &rra, &AllMask);  \
   }

#define DO_imm_m_r(_opname, _imm, _src, _dst)  \
   {  \
      V128 _tmp;  \
      V128* _srcM = memalign16(sizeof(V128));  \
      memcpy(_srcM, &(_src), sizeof(V128));  \
      __asm__ __volatile__(  \
         "movupd (%1), %%" XMMREG_DST   "\n\t"  \
         _opname " $" #_imm ", (%0), %%" XMMREG_DST  "\n\t"  \
         "movupd %%" XMMREG_DST ", (%2)" "\n"  \
         : /*out*/ : /*in*/ "r"(_srcM), "r"(&(_dst)), "r"(&(_tmp))  \
         : "cc", "memory", XMMREG_DST  \
      );  \
      RRArgs rra;  \
      memcpy(&rra.arg1, &(_src), sizeof(V128));  \
      memcpy(&rra.arg2, &(_dst), sizeof(V128));  \
      memcpy(&rra.res,  &(_tmp), sizeof(V128));  \
      showIAA("m", (_opname), (_imm), &rra, &AllMask);  \
      free(_srcM);  \
   }

#define DO_imm_mandr_r(_opname, _imm, _src, _dst)  \
      DO_imm_r_r( _opname, _imm, _src, _dst ) \
      DO_imm_m_r( _opname, _imm, _src, _dst )

#define DO_r_r(_opname, _src, _dst)  \
   {  \
      V128 _tmp;  \
      __asm__ __volatile__(  \
         "movupd (%0), %%xmm2"    "\n\t"  \
         "movupd (%1), %%" XMMREG_DST   "\n\t"  \
         _opname " %%xmm2, %%" XMMREG_DST  "\n\t"  \
         "movupd %%" XMMREG_DST ", (%2)" "\n"  \
         : /*out*/ : /*in*/ "r"(&(_src)), "r"(&(_dst)), "r"(&(_tmp))  \
         : "cc", "memory", "xmm2", XMMREG_DST  \
      );  \
      RRArgs rra;  \
      memcpy(&rra.arg1, &(_src), sizeof(V128));  \
      memcpy(&rra.arg2, &(_dst), sizeof(V128));  \
      memcpy(&rra.res,  &(_tmp), sizeof(V128));  \
      showAA("r", (_opname), &rra, &AllMask);  \
   }

#define DO_m_r(_opname, _src, _dst)  \
   {  \
      V128 _tmp;  \
      V128* _srcM = memalign16(sizeof(V128));  \
      memcpy(_srcM, &(_src), sizeof(V128));  \
      __asm__ __volatile__(  \
         "movupd (%1), %%" XMMREG_DST   "\n\t"  \
         _opname " (%0), %%" XMMREG_DST  "\n\t"  \
         "movupd %%" XMMREG_DST ", (%2)" "\n"  \
         : /*out*/ : /*in*/ "r"(_srcM), "r"(&(_dst)), "r"(&(_tmp))  \
         : "cc", "memory", XMMREG_DST  \
      );  \
      RRArgs rra;  \
      memcpy(&rra.arg1, &(_src), sizeof(V128));  \
      memcpy(&rra.arg2, &(_dst), sizeof(V128));  \
      memcpy(&rra.res,  &(_tmp), sizeof(V128));  \
      showAA("m", (_opname), &rra, &AllMask);  \
      free(_srcM);  \
   }

#define DO_mandr_r(_opname, _src, _dst)  \
      DO_r_r(_opname, _src, _dst) \
      DO_m_r(_opname, _src, _dst)

/* Common test functions */

static inline void test_PMAXSD ( void )
{
   V128 src, dst;
   Int i;
   for (i = 0; i < 10; i++) {
      randV128(&src);
      randV128(&dst);
      DO_mandr_r("pmaxsd", src, dst);
   }
}

static inline void test_PMINSD ( void )
{
   V128 src, dst;
   Int i;
   for (i = 0; i < 10; i++) {
      randV128(&src);
      randV128(&dst);
      DO_mandr_r("pminsd", src, dst);
   }
}

static inline void test_PMAXSB ( void )
{
   V128 src, dst;
   Int i;
   for (i = 0; i < 10; i++) {
      randV128(&src);
      randV128(&dst);
      DO_mandr_r("pmaxsb", src, dst);
   }
}

static inline void test_PMAXUD ( void )
{
   V128 src, dst;
   Int i;
   for (i = 0; i < 10; i++) {
      randV128(&src);
      randV128(&dst);
      DO_mandr_r("pmaxud", src, dst);
   }
}

static inline void test_PMAXUW ( void )
{
   V128 src, dst;
   Int i;
   for (i = 0; i < 10; i++) {
      randV128(&src);
      randV128(&dst);
      DO_mandr_r("pmaxuw", src, dst);
   }
}

static inline void test_PMINSB ( void )
{
   V128 src, dst;
   Int i;
   for (i = 0; i < 10; i++) {
      randV128(&src);
      randV128(&dst);
      DO_mandr_r("pminsb", src, dst);
   }
}

static inline void test_PMINUD ( void )
{
   V128 src, dst;
   Int i;
   for (i = 0; i < 10; i++) {
      randV128(&src);
      randV128(&dst);
      DO_mandr_r("pminud", src, dst);
   }
}

static inline void test_PMINUW ( void )
{
   V128 src, dst;
   Int i;
   for (i = 0; i < 10; i++) {
      randV128(&src);
      randV128(&dst);
      DO_mandr_r("pminuw", src, dst);
   }
}

static inline void test_PMULLD ( void )
{
   V128 src, dst;
   Int i;
   for (i = 0; i < 10; i++) {
      randV128(&src);
      randV128(&dst);
      DO_mandr_r("pmulld", src, dst);
   }
}

static inline void test_BLENDPD ( void )
{
   V128 src, dst;
   Int i;
   for (i = 0; i < 10; i++) {
      randV128(&src);
      randV128(&dst);
      DO_imm_mandr_r("blendpd", 0, src, dst);
      DO_imm_mandr_r("blendpd", 1, src, dst);
      DO_imm_mandr_r("blendpd", 2, src, dst);
      DO_imm_mandr_r("blendpd", 3, src, dst);
   }
}

static inline void test_BLENDPS ( void )
{
   V128 src, dst;
   Int i;
   for (i = 0; i < 10; i++) {
      randV128(&src);
      randV128(&dst);
      DO_imm_mandr_r("blendps", 0, src, dst);
      DO_imm_mandr_r("blendps", 1, src, dst);
      DO_imm_mandr_r("blendps", 2, src, dst);
      DO_imm_mandr_r("blendps", 3, src, dst);
      DO_imm_mandr_r("blendps", 4, src, dst);
      DO_imm_mandr_r("blendps", 5, src, dst);
      DO_imm_mandr_r("blendps", 6, src, dst);
      DO_imm_mandr_r("blendps", 7, src, dst);
      DO_imm_mandr_r("blendps", 8, src, dst);
      DO_imm_mandr_r("blendps", 9, src, dst);
      DO_imm_mandr_r("blendps", 10, src, dst);
      DO_imm_mandr_r("blendps", 11, src, dst);
      DO_imm_mandr_r("blendps", 12, src, dst);
      DO_imm_mandr_r("blendps", 13, src, dst);
      DO_imm_mandr_r("blendps", 14, src, dst);
      DO_imm_mandr_r("blendps", 15, src, dst);
   }
}

static inline void test_PBLENDW ( void )
{
   V128 src, dst;
   randV128(&src);
   randV128(&dst);
   {
      DO_imm_mandr_r("pblendw", 0, src, dst);
      DO_imm_mandr_r("pblendw", 1, src, dst);
      DO_imm_mandr_r("pblendw", 2, src, dst);
      DO_imm_mandr_r("pblendw", 3, src, dst);
      DO_imm_mandr_r("pblendw", 4, src, dst);
      DO_imm_mandr_r("pblendw", 5, src, dst);
      DO_imm_mandr_r("pblendw", 6, src, dst);
      DO_imm_mandr_r("pblendw", 7, src, dst);
      DO_imm_mandr_r("pblendw", 8, src, dst);
      DO_imm_mandr_r("pblendw", 9, src, dst);
      DO_imm_mandr_r("pblendw", 10, src, dst);
      DO_imm_mandr_r("pblendw", 11, src, dst);
      DO_imm_mandr_r("pblendw", 12, src, dst);
      DO_imm_mandr_r("pblendw", 13, src, dst);
      DO_imm_mandr_r("pblendw", 14, src, dst);
      DO_imm_mandr_r("pblendw", 15, src, dst);
      DO_imm_mandr_r("pblendw", 16, src, dst);
      DO_imm_mandr_r("pblendw", 17, src, dst);
      DO_imm_mandr_r("pblendw", 18, src, dst);
      DO_imm_mandr_r("pblendw", 19, src, dst);
      DO_imm_mandr_r("pblendw", 20, src, dst);
      DO_imm_mandr_r("pblendw", 21, src, dst);
      DO_imm_mandr_r("pblendw", 22, src, dst);
      DO_imm_mandr_r("pblendw", 23, src, dst);
      DO_imm_mandr_r("pblendw", 24, src, dst);
      DO_imm_mandr_r("pblendw", 25, src, dst);
      DO_imm_mandr_r("pblendw", 26, src, dst);
      DO_imm_mandr_r("pblendw", 27, src, dst);
      DO_imm_mandr_r("pblendw", 28, src, dst);
      DO_imm_mandr_r("pblendw", 29, src, dst);
      DO_imm_mandr_r("pblendw", 30, src, dst);
      DO_imm_mandr_r("pblendw", 31, src, dst);
      DO_imm_mandr_r("pblendw", 32, src, dst);
      DO_imm_mandr_r("pblendw", 33, src, dst);
      DO_imm_mandr_r("pblendw", 34, src, dst);
      DO_imm_mandr_r("pblendw", 35, src, dst);
      DO_imm_mandr_r("pblendw", 36, src, dst);
      DO_imm_mandr_r("pblendw", 37, src, dst);
      DO_imm_mandr_r("pblendw", 38, src, dst);
      DO_imm_mandr_r("pblendw", 39, src, dst);
      DO_imm_mandr_r("pblendw", 40, src, dst);
      DO_imm_mandr_r("pblendw", 41, src, dst);
      DO_imm_mandr_r("pblendw", 42, src, dst);
      DO_imm_mandr_r("pblendw", 43, src, dst);
      DO_imm_mandr_r("pblendw", 44, src, dst);
      DO_imm_mandr_r("pblendw", 45, src, dst);
      DO_imm_mandr_r("pblendw", 46, src, dst);
      DO_imm_mandr_r("pblendw", 47, src, dst);
      DO_imm_mandr_r("pblendw", 48, src, dst);
      DO_imm_mandr_r("pblendw", 49, src, dst);
      DO_imm_mandr_r("pblendw", 50, src, dst);
      DO_imm_mandr_r("pblendw", 51, src, dst);
      DO_imm_mandr_r("pblendw", 52, src, dst);
      DO_imm_mandr_r("pblendw", 53, src, dst);
      DO_imm_mandr_r("pblendw", 54, src, dst);
      DO_imm_mandr_r("pblendw", 55, src, dst);
      DO_imm_mandr_r("pblendw", 56, src, dst);
      DO_imm_mandr_r("pblendw", 57, src, dst);
      DO_imm_mandr_r("pblendw", 58, src, dst);
      DO_imm_mandr_r("pblendw", 59, src, dst);
      DO_imm_mandr_r("pblendw", 60, src, dst);
      DO_imm_mandr_r("pblendw", 61, src, dst);
      DO_imm_mandr_r("pblendw", 62, src, dst);
      DO_imm_mandr_r("pblendw", 63, src, dst);
      DO_imm_mandr_r("pblendw", 64, src, dst);
      DO_imm_mandr_r("pblendw", 65, src, dst);
      DO_imm_mandr_r("pblendw", 66, src, dst);
      DO_imm_mandr_r("pblendw", 67, src, dst);
      DO_imm_mandr_r("pblendw", 68, src, dst);
      DO_imm_mandr_r("pblendw", 69, src, dst);
      DO_imm_mandr_r("pblendw", 70, src, dst);
      DO_imm_mandr_r("pblendw", 71, src, dst);
      DO_imm_mandr_r("pblendw", 72, src, dst);
      DO_imm_mandr_r("pblendw", 73, src, dst);
      DO_imm_mandr_r("pblendw", 74, src, dst);
      DO_imm_mandr_r("pblendw", 75, src, dst);
      DO_imm_mandr_r("pblendw", 76, src, dst);
      DO_imm_mandr_r("pblendw", 77, src, dst);
      DO_imm_mandr_r("pblendw", 78, src, dst);
      DO_imm_mandr_r("pblendw", 79, src, dst);
      DO_imm_mandr_r("pblendw", 80, src, dst);
      DO_imm_mandr_r("pblendw", 81, src, dst);
      DO_imm_mandr_r("pblendw", 82, src, dst);
      DO_imm_mandr_r("pblendw", 83, src, dst);
      DO_imm_mandr_r("pblendw", 84, src, dst);
      DO_imm_mandr_r("pblendw", 85, src, dst);
      DO_imm_mandr_r("pblendw", 86, src, dst);
      DO_imm_mandr_r("pblendw", 87, src, dst);
      DO_imm_mandr_r("pblendw", 88, src, dst);
      DO_imm_mandr_r("pblendw", 89, src, dst);
      DO_imm_mandr_r("pblendw", 90, src, dst);
      DO_imm_mandr_r("pblendw", 91, src, dst);
      DO_imm_mandr_r("pblendw", 92, src, dst);
      DO_imm_mandr_r("pblendw", 93, src, dst);
      DO_imm_mandr_r("pblendw", 94, src, dst);
      DO_imm_mandr_r("pblendw", 95, src, dst);
      DO_imm_mandr_r("pblendw", 96, src, dst);
      DO_imm_mandr_r("pblendw", 97, src, dst);
      DO_imm_mandr_r("pblendw", 98, src, dst);
      DO_imm_mandr_r("pblendw", 99, src, dst);
      DO_imm_mandr_r("pblendw", 100, src, dst);
      DO_imm_mandr_r("pblendw", 101, src, dst);
      DO_imm_mandr_r("pblendw", 102, src, dst);
      DO_imm_mandr_r("pblendw", 103, src, dst);
      DO_imm_mandr_r("pblendw", 104, src, dst);
      DO_imm_mandr_r("pblendw", 105, src, dst);
      DO_imm_mandr_r("pblendw", 106, src, dst);
      DO_imm_mandr_r("pblendw", 107, src, dst);
      DO_imm_mandr_r("pblendw", 108, src, dst);
      DO_imm_mandr_r("pblendw", 109, src, dst);
      DO_imm_mandr_r("pblendw", 110, src, dst);
      DO_imm_mandr_r("pblendw", 111, src, dst);
      DO_imm_mandr_r("pblendw", 112, src, dst);
      DO_imm_mandr_r("pblendw", 113, src, dst);
      DO_imm_mandr_r("pblendw", 114, src, dst);
      DO_imm_mandr_r("pblendw", 115, src, dst);
      DO_imm_mandr_r("pblendw", 116, src, dst);
      DO_imm_mandr_r("pblendw", 117, src, dst);
      DO_imm_mandr_r("pblendw", 118, src, dst);
      DO_imm_mandr_r("pblendw", 119, src, dst);
      DO_imm_mandr_r("pblendw", 120, src, dst);
      DO_imm_mandr_r("pblendw", 121, src, dst);
      DO_imm_mandr_r("pblendw", 122, src, dst);
      DO_imm_mandr_r("pblendw", 123, src, dst);
      DO_imm_mandr_r("pblendw", 124, src, dst);
      DO_imm_mandr_r("pblendw", 125, src, dst);
      DO_imm_mandr_r("pblendw", 126, src, dst);
      DO_imm_mandr_r("pblendw", 127, src, dst);
      DO_imm_mandr_r("pblendw", 128, src, dst);
      DO_imm_mandr_r("pblendw", 129, src, dst);
      DO_imm_mandr_r("pblendw", 130, src, dst);
      DO_imm_mandr_r("pblendw", 131, src, dst);
      DO_imm_mandr_r("pblendw", 132, src, dst);
      DO_imm_mandr_r("pblendw", 133, src, dst);
      DO_imm_mandr_r("pblendw", 134, src, dst);
      DO_imm_mandr_r("pblendw", 135, src, dst);
      DO_imm_mandr_r("pblendw", 136, src, dst);
      DO_imm_mandr_r("pblendw", 137, src, dst);
      DO_imm_mandr_r("pblendw", 138, src, dst);
      DO_imm_mandr_r("pblendw", 139, src, dst);
      DO_imm_mandr_r("pblendw", 140, src, dst);
      DO_imm_mandr_r("pblendw", 141, src, dst);
      DO_imm_mandr_r("pblendw", 142, src, dst);
      DO_imm_mandr_r("pblendw", 143, src, dst);
      DO_imm_mandr_r("pblendw", 144, src, dst);
      DO_imm_mandr_r("pblendw", 145, src, dst);
      DO_imm_mandr_r("pblendw", 146, src, dst);
      DO_imm_mandr_r("pblendw", 147, src, dst);
      DO_imm_mandr_r("pblendw", 148, src, dst);
      DO_imm_mandr_r("pblendw", 149, src, dst);
      DO_imm_mandr_r("pblendw", 150, src, dst);
      DO_imm_mandr_r("pblendw", 151, src, dst);
      DO_imm_mandr_r("pblendw", 152, src, dst);
      DO_imm_mandr_r("pblendw", 153, src, dst);
      DO_imm_mandr_r("pblendw", 154, src, dst);
      DO_imm_mandr_r("pblendw", 155, src, dst);
      DO_imm_mandr_r("pblendw", 156, src, dst);
      DO_imm_mandr_r("pblendw", 157, src, dst);
      DO_imm_mandr_r("pblendw", 158, src, dst);
      DO_imm_mandr_r("pblendw", 159, src, dst);
      DO_imm_mandr_r("pblendw", 160, src, dst);
      DO_imm_mandr_r("pblendw", 161, src, dst);
      DO_imm_mandr_r("pblendw", 162, src, dst);
      DO_imm_mandr_r("pblendw", 163, src, dst);
      DO_imm_mandr_r("pblendw", 164, src, dst);
      DO_imm_mandr_r("pblendw", 165, src, dst);
      DO_imm_mandr_r("pblendw", 166, src, dst);
      DO_imm_mandr_r("pblendw", 167, src, dst);
      DO_imm_mandr_r("pblendw", 168, src, dst);
      DO_imm_mandr_r("pblendw", 169, src, dst);
      DO_imm_mandr_r("pblendw", 170, src, dst);
      DO_imm_mandr_r("pblendw", 171, src, dst);
      DO_imm_mandr_r("pblendw", 172, src, dst);
      DO_imm_mandr_r("pblendw", 173, src, dst);
      DO_imm_mandr_r("pblendw", 174, src, dst);
      DO_imm_mandr_r("pblendw", 175, src, dst);
      DO_imm_mandr_r("pblendw", 176, src, dst);
      DO_imm_mandr_r("pblendw", 177, src, dst);
      DO_imm_mandr_r("pblendw", 178, src, dst);
      DO_imm_mandr_r("pblendw", 179, src, dst);
      DO_imm_mandr_r("pblendw", 180, src, dst);
      DO_imm_mandr_r("pblendw", 181, src, dst);
      DO_imm_mandr_r("pblendw", 182, src, dst);
      DO_imm_mandr_r("pblendw", 183, src, dst);
      DO_imm_mandr_r("pblendw", 184, src, dst);
      DO_imm_mandr_r("pblendw", 185, src, dst);
      DO_imm_mandr_r("pblendw", 186, src, dst);
      DO_imm_mandr_r("pblendw", 187, src, dst);
      DO_imm_mandr_r("pblendw", 188, src, dst);
      DO_imm_mandr_r("pblendw", 189, src, dst);
      DO_imm_mandr_r("pblendw", 190, src, dst);
      DO_imm_mandr_r("pblendw", 191, src, dst);
      DO_imm_mandr_r("pblendw", 192, src, dst);
      DO_imm_mandr_r("pblendw", 193, src, dst);
      DO_imm_mandr_r("pblendw", 194, src, dst);
      DO_imm_mandr_r("pblendw", 195, src, dst);
      DO_imm_mandr_r("pblendw", 196, src, dst);
      DO_imm_mandr_r("pblendw", 197, src, dst);
      DO_imm_mandr_r("pblendw", 198, src, dst);
      DO_imm_mandr_r("pblendw", 199, src, dst);
      DO_imm_mandr_r("pblendw", 200, src, dst);
      DO_imm_mandr_r("pblendw", 201, src, dst);
      DO_imm_mandr_r("pblendw", 202, src, dst);
      DO_imm_mandr_r("pblendw", 203, src, dst);
      DO_imm_mandr_r("pblendw", 204, src, dst);
      DO_imm_mandr_r("pblendw", 205, src, dst);
      DO_imm_mandr_r("pblendw", 206, src, dst);
      DO_imm_mandr_r("pblendw", 207, src, dst);
      DO_imm_mandr_r("pblendw", 208, src, dst);
      DO_imm_mandr_r("pblendw", 209, src, dst);
      DO_imm_mandr_r("pblendw", 210, src, dst);
      DO_imm_mandr_r("pblendw", 211, src, dst);
      DO_imm_mandr_r("pblendw", 212, src, dst);
      DO_imm_mandr_r("pblendw", 213, src, dst);
      DO_imm_mandr_r("pblendw", 214, src, dst);
      DO_imm_mandr_r("pblendw", 215, src, dst);
      DO_imm_mandr_r("pblendw", 216, src, dst);
      DO_imm_mandr_r("pblendw", 217, src, dst);
      DO_imm_mandr_r("pblendw", 218, src, dst);
      DO_imm_mandr_r("pblendw", 219, src, dst);
      DO_imm_mandr_r("pblendw", 220, src, dst);
      DO_imm_mandr_r("pblendw", 221, src, dst);
      DO_imm_mandr_r("pblendw", 222, src, dst);
      DO_imm_mandr_r("pblendw", 223, src, dst);
      DO_imm_mandr_r("pblendw", 224, src, dst);
      DO_imm_mandr_r("pblendw", 225, src, dst);
      DO_imm_mandr_r("pblendw", 226, src, dst);
      DO_imm_mandr_r("pblendw", 227, src, dst);
      DO_imm_mandr_r("pblendw", 228, src, dst);
      DO_imm_mandr_r("pblendw", 229, src, dst);
      DO_imm_mandr_r("pblendw", 230, src, dst);
      DO_imm_mandr_r("pblendw", 231, src, dst);
      DO_imm_mandr_r("pblendw", 232, src, dst);
      DO_imm_mandr_r("pblendw", 233, src, dst);
      DO_imm_mandr_r("pblendw", 234, src, dst);
      DO_imm_mandr_r("pblendw", 235, src, dst);
      DO_imm_mandr_r("pblendw", 236, src, dst);
      DO_imm_mandr_r("pblendw", 237, src, dst);
      DO_imm_mandr_r("pblendw", 238, src, dst);
      DO_imm_mandr_r("pblendw", 239, src, dst);
      DO_imm_mandr_r("pblendw", 240, src, dst);
      DO_imm_mandr_r("pblendw", 241, src, dst);
      DO_imm_mandr_r("pblendw", 242, src, dst);
      DO_imm_mandr_r("pblendw", 243, src, dst);
      DO_imm_mandr_r("pblendw", 244, src, dst);
      DO_imm_mandr_r("pblendw", 245, src, dst);
      DO_imm_mandr_r("pblendw", 246, src, dst);
      DO_imm_mandr_r("pblendw", 247, src, dst);
      DO_imm_mandr_r("pblendw", 248, src, dst);
      DO_imm_mandr_r("pblendw", 249, src, dst);
      DO_imm_mandr_r("pblendw", 250, src, dst);
      DO_imm_mandr_r("pblendw", 251, src, dst);
      DO_imm_mandr_r("pblendw", 252, src, dst);
      DO_imm_mandr_r("pblendw", 253, src, dst);
      DO_imm_mandr_r("pblendw", 254, src, dst);
      DO_imm_mandr_r("pblendw", 255, src, dst);
   }
}

/* ------------ PBLENDVB ------------ */

static inline void do_PBLENDVB ( Bool mem, V128* xmm0, V128* src, /*MOD*/V128* dst )
{
   if (mem) {
      __asm__ __volatile__(
         "movupd   (%2), %%xmm0"         "\n\t"
         "movupd   (%1), %%xmm7"        "\n\t"
         "pblendvb (%0), %%xmm7"        "\n\t"
         "movupd   %%xmm7, (%1)"        "\n"
         : /*OUT*/
         : /*IN*/ "r"(src), "r"(dst), "r"(xmm0)
         : /*TRASH*/ "xmm7","xmm0"
      );
   } else {
      __asm__ __volatile__(
         "movupd   (%2), %%xmm0"         "\n\t"
         "movupd   (%1), %%xmm7"        "\n\t"
         "movupd   (%0), %%xmm2"         "\n\t"
         "pblendvb %%xmm2, %%xmm7"      "\n\t"
         "movupd   %%xmm7, (%1)"        "\n"
         : /*OUT*/
         : /*IN*/ "r"(src), "r"(dst), "r"(xmm0)
         : /*TRASH*/ "xmm7","xmm2","xmm0"
      );
   }
}

static inline void test_PBLENDVB ( void )
{
   __attribute__ ( (aligned (16))) V128 xmm0, src, dst, t_xmm0, t_src, t_dst;
   Int i;
   for (i = 0; i < 10; i++) {
      randV128(&t_xmm0);
      randV128(&t_src);
      randV128(&t_dst);

      memcpy(&xmm0, &t_xmm0, 16);
      memcpy(&src, &t_src, 16);
      memcpy(&dst, &t_dst, 16);
      do_PBLENDVB(False/*reg*/, &xmm0, &src, &dst);
      printf("r pblendvb  ");
      showV128(&t_xmm0);
      printf(" ");
      showV128(&t_src);
      printf(" ");
      showV128(&t_dst);
      printf(" -> ");
      showV128(&dst);
      printf("\n");

      memcpy(&xmm0, &t_xmm0, 16);
      memcpy(&src, &t_src, 16);
      memcpy(&dst, &t_dst, 16);
      do_PBLENDVB(True/*mem*/, &xmm0, &src, &dst);
      printf("m pblendvb  ");
      showV128(&t_xmm0);
      printf(" ");
      showV128(&t_src);
      printf(" ");
      showV128(&t_dst);
      printf(" -> ");
      showV128(&dst);
      printf("\n");
   }
}

/* ------------ BLENDVPD ------------ */

static inline void do_BLENDVPD ( Bool mem, V128* xmm0, V128* src, /*MOD*/V128* dst )
{
   if (mem) {
      __asm__ __volatile__(
         "movupd   (%2), %%xmm0"         "\n\t"
         "movupd   (%1), %%xmm7"        "\n\t"
         "blendvpd (%0), %%xmm7"        "\n\t"
         "movupd   %%xmm7, (%1)"        "\n"
         : /*OUT*/
         : /*IN*/ "r"(src), "r"(dst), "r"(xmm0)
         : /*TRASH*/ "xmm7","xmm0"
      );
   } else {
      __asm__ __volatile__(
         "movupd   (%2), %%xmm0"         "\n\t"
         "movupd   (%1), %%xmm7"        "\n\t"
         "movupd   (%0), %%xmm2"         "\n\t"
         "blendvpd %%xmm2, %%xmm7"      "\n\t"
         "movupd   %%xmm7, (%1)"        "\n"
         : /*OUT*/
         : /*IN*/ "r"(src), "r"(dst), "r"(xmm0)
         : /*TRASH*/ "xmm7","xmm2","xmm0"
      );
   }
}

static inline void test_BLENDVPD ( void )
{
   __attribute__ ( (aligned (16))) V128 xmm0, src, dst, t_xmm0, t_src, t_dst;
   Int i;
   for (i = 0; i < 10; i++) {
      randV128(&t_xmm0);
      randV128(&t_src);
      randV128(&t_dst);

      memcpy(&xmm0, &t_xmm0, 16);
      memcpy(&src, &t_src, 16);
      memcpy(&dst, &t_dst, 16);
      do_BLENDVPD(False/*reg*/, &xmm0, &src, &dst);
      printf("r blendvpd  ");
      showV128(&t_xmm0);
      printf(" ");
      showV128(&t_src);
      printf(" ");
      showV128(&t_dst);
      printf(" -> ");
      showV128(&dst);
      printf("\n");

      memcpy(&xmm0, &t_xmm0, 16);
      memcpy(&src, &t_src, 16);
      memcpy(&dst, &t_dst, 16);
      do_BLENDVPD(True/*mem*/, &xmm0, &src, &dst);
      printf("m blendvpd  ");
      showV128(&t_xmm0);
      printf(" ");
      showV128(&t_src);
      printf(" ");
      showV128(&t_dst);
      printf(" -> ");
      showV128(&dst);
      printf("\n");
   }
}

/* ------------ BLENDVPS ------------ */

static inline void do_BLENDVPS ( Bool mem, V128* xmm0, V128* src, /*MOD*/V128* dst )
{
   if (mem) {
      __asm__ __volatile__(
         "movupd   (%2), %%xmm0"         "\n\t"
         "movupd   (%1), %%xmm7"        "\n\t"
         "blendvps (%0), %%xmm7"        "\n\t"
         "movupd   %%xmm7, (%1)"        "\n"
         : /*OUT*/
         : /*IN*/ "r"(src), "r"(dst), "r"(xmm0)
         : /*TRASH*/ "xmm7","xmm0"
      );
   } else {
      __asm__ __volatile__(
         "movupd   (%2), %%xmm0"         "\n\t"
         "movupd   (%1), %%xmm7"        "\n\t"
         "movupd   (%0), %%xmm2"         "\n\t"
         "blendvps %%xmm2, %%xmm7"      "\n\t"
         "movupd   %%xmm7, (%1)"        "\n"
         : /*OUT*/
         : /*IN*/ "r"(src), "r"(dst), "r"(xmm0)
         : /*TRASH*/ "xmm7","xmm2","xmm0"
      );
   }
}

static inline void test_BLENDVPS ( void )
{
   __attribute__ ( (aligned (16))) V128 xmm0, src, dst, t_xmm0, t_src, t_dst;
   Int i;
   for (i = 0; i < 10; i++) {
      randV128(&t_xmm0);
      randV128(&t_src);
      randV128(&t_dst);

      memcpy(&xmm0, &t_xmm0, 16);
      memcpy(&src, &t_src, 16);
      memcpy(&dst, &t_dst, 16);
      do_BLENDVPS(False/*reg*/, &xmm0, &src, &dst);
      printf("r blendvps  ");
      showV128(&t_xmm0);
      printf(" ");
      showV128(&t_src);
      printf(" ");
      showV128(&t_dst);
      printf(" -> ");
      showV128(&dst);
      printf("\n");

      memcpy(&xmm0, &t_xmm0, 16);
      memcpy(&src, &t_src, 16);
      memcpy(&dst, &t_dst, 16);
      do_BLENDVPS(True/*mem*/, &xmm0, &src, &dst);
      printf("m blendvps  ");
      showV128(&t_xmm0);
      printf(" ");
      showV128(&t_src);
      printf(" ");
      showV128(&t_dst);
      printf(" -> ");
      showV128(&dst);
      printf("\n");
   }
}

static inline void test_PCMPEQQ ( void )
{
   V128 src, dst;
   Int i;
   for (i = 0; i < 10; i++) {
      randV128(&src);
      randV128(&dst);
      switch (i - 6) {
         case 0: memset(&src[0], 0x55, 8);
                 memset(&dst[0], 0x55, 8); break;
         case 1: memset(&src[8], 0x55, 8);
                 memset(&dst[8], 0x55, 8); break;
         default:
            break;
      }
      DO_mandr_r("pcmpeqq", src, dst);
   }
}

/* ------------ MPSADBW ------------ */
static inline void test_MPSADBW ( void )
{
   V128 src, dst;
   Int i;
   for (i = 0; i < 50; i++) {
      randV128(&src);
      randV128(&dst);
      DO_imm_mandr_r("mpsadbw", 0, src, dst);
      DO_imm_mandr_r("mpsadbw", 1, src, dst);
      DO_imm_mandr_r("mpsadbw", 2, src, dst);
      DO_imm_mandr_r("mpsadbw", 3, src, dst);
      DO_imm_mandr_r("mpsadbw", 4, src, dst);
      DO_imm_mandr_r("mpsadbw", 5, src, dst);
      DO_imm_mandr_r("mpsadbw", 6, src, dst);
      DO_imm_mandr_r("mpsadbw", 7, src, dst);
   }
}

static inline void test_MOVNTDQA ( void )
{
   V128 src, dst;
   Int i;
   for (i = 0; i < 10; i++) {
      randV128(&src);
      /* make sure the load actually happens */
      randV128(&dst);
      DO_m_r("movntdqa", src, dst);
   }
}

#endif /* __SSE4_COMMON_H */
