
/* A program to test SSE4.1/SSE4.2 instructions. 
   Revisions:  Nov.208     - wrote this file
               Apr.10.2010 - added PEXTR* tests
               Apr.16.2010 - added PINS*  tests
*/

/* HOW TO COMPILE:
   gcc -m64 -g -O -Wall -o sse4-64 sse4-64.c
*/

#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
//#include "tests/malloc.h" // reenable when reintegrated
#include <string.h>



// rmme when reintegrated
// Allocates a 16-aligned block.  Asserts if the allocation fails.
#ifdef VGO_darwin
#include <stdlib.h>
#else
#include <malloc.h>
#endif
__attribute__((unused))
static void* memalign16(size_t szB)
{
   void* x;
#if defined(VGO_darwin)
   // Darwin lacks memalign, but its malloc is always 16-aligned anyway.
   x = malloc(szB);
#else
   x = memalign(16, szB);
#endif
   assert(x);
   assert(0 == ((16-1) & (unsigned long)x));
   return x;
}



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

static void do64HLtoV128 ( /*OUT*/V128* res, ULong wHi, ULong wLo )
{
   // try to sidestep strict-aliasing snafus by memcpying explicitly
   UChar* p = (UChar*)res;
   memcpy(&p[8], (UChar*)&wHi, 8);
   memcpy(&p[0], (UChar*)&wLo, 8);
}

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

double mkPosInf ( void ) { return 1.0 / 0.0; }
double mkNegInf ( void ) { return -mkPosInf(); }
double mkPosNan ( void ) { return 0.0 / 0.0; }
double mkNegNan ( void ) { return -mkPosNan(); }

__attribute__((noinline))
UInt get_mxcsr ( void )
{
   ULong w64;
   __asm__ __volatile__(
      "subq    $8, %%rsp"    "\n\t"
      "stmxcsr (%%rsp)"      "\n\t"
      "movq    (%%rsp), %0"  "\n"
      "addq    $8, %%rsp"
      : /*OUT*/"=r"(w64) : /*IN*/ : "memory","cc"
   );
   if (0) printf("get %08x\n", (UInt)w64);
   return (UInt)w64;
}

__attribute__((noinline))
void set_mxcsr ( UInt w32 )
{
   if (0) printf("set %08x\n", w32);
   ULong w64 = (ULong)w32;
   __asm__ __volatile__(
      "subq    $8, %%rsp"    "\n\t"
      "movq    %0, (%%rsp)"  "\n\t"
      "ldmxcsr (%%rsp)"      "\n\t"
      "addq    $8, %%rsp"
      : /*OUT*/ : /*IN*/"r"(w64) : "memory",/*"mxcsr",*/"cc"
   );
}

UInt get_sse_roundingmode ( void )
{
   UInt w = get_mxcsr();
   return (w >> 13) & 3;
}

void set_sse_roundingmode ( UInt m )
{
   UInt w;
   assert(0 == (m & ~3));
   w = get_mxcsr();
   w &= ~(3 << 13);
   w |= (m << 13);
   set_mxcsr(w);
}


#define DO_imm_r_r(_opname, _imm, _src, _dst)  \
   {  \
      V128 _tmp;  \
      __asm__ __volatile__(  \
         "movupd (%0), %%xmm2"    "\n\t"  \
         "movupd (%1), %%xmm11"   "\n\t"  \
         _opname " $" #_imm ", %%xmm2, %%xmm11"  "\n\t"  \
         "movupd %%xmm11, (%2)" "\n"  \
         : /*out*/ : /*in*/ "r"(&(_src)), "r"(&(_dst)), "r"(&(_tmp))  \
         : "cc", "memory", "xmm2", "xmm11"                            \
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
         "movupd (%1), %%xmm11"   "\n\t"  \
         _opname " $" #_imm ", (%0), %%xmm11"  "\n\t"  \
         "movupd %%xmm11, (%2)" "\n"  \
         : /*out*/ : /*in*/ "r"(_srcM), "r"(&(_dst)), "r"(&(_tmp))  \
         : "cc", "memory", "xmm11"  \
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
         "movupd (%1), %%xmm11"   "\n\t"  \
         _opname " %%xmm2, %%xmm11"  "\n\t"  \
         "movupd %%xmm11, (%2)" "\n"  \
         : /*out*/ : /*in*/ "r"(&(_src)), "r"(&(_dst)), "r"(&(_tmp))  \
         : "cc", "memory", "xmm2", "xmm11"  \
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
         "movupd (%1), %%xmm11"   "\n\t"  \
         _opname " (%0), %%xmm11"  "\n\t"  \
         "movupd %%xmm11, (%2)" "\n"  \
         : /*out*/ : /*in*/ "r"(_srcM), "r"(&(_dst)), "r"(&(_tmp))  \
         : "cc", "memory", "xmm11"  \
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




#define DO_imm_r_to_rscalar(_opname, _imm, _src, _dstsuffix)       \
   {  \
      ULong _scbefore = 0x5555555555555555ULL;  \
      ULong _scafter  = 0xAAAAAAAAAAAAAAAAULL; \
      /* This assumes that gcc won't make any of %0, %1, %2 */ \
      /* be r11.  That should be ensured (cough, cough) */ \
      /* by declaring r11 to be clobbered. */ \
      __asm__ __volatile__(  \
         "movupd (%0), %%xmm2"    "\n\t"  \
         "movq   (%1), %%r11"   "\n\t"  \
         _opname " $" #_imm ", %%xmm2, %%r11" _dstsuffix  "\n\t"  \
         "movq   %%r11, (%2)" "\n"  \
         : /*out*/ \
         : /*in*/ "r"(&(_src)), "r"(&(_scbefore)), "r"(&(_scafter))  \
         : "cc", "memory", "xmm2", "r11"  \
      );  \
      showIAG("r", (_opname), (_imm), &(_src), (_scbefore), (_scafter));  \
   }

#define DO_imm_r_to_mscalar(_opname, _imm, _src)   \
   {  \
      ULong _scbefore = 0x5555555555555555ULL;  \
      ULong _scafter = _scbefore; \
      __asm__ __volatile__(  \
         "movupd (%0), %%xmm2"    "\n\t"  \
         _opname " $" #_imm ", %%xmm2, (%1)"  "\n\t"  \
         : /*out*/ \
         : /*in*/ "r"(&(_src)), "r"(&(_scafter))  \
         : "cc", "memory", "xmm2"  \
      );  \
      showIAG("m", (_opname), (_imm), &(_src), (_scbefore), (_scafter));  \
   }

#define DO_imm_r_to_mandrscalar(_opname, _imm, _src, _dstsuffix)   \
      DO_imm_r_to_rscalar( _opname, _imm, _src, _dstsuffix )       \
      DO_imm_r_to_mscalar( _opname, _imm, _src )








#define DO_imm_rscalar_to_r(_opname, _imm, _src, _srcsuffix)       \
   {  \
      V128  dstv;         \
      V128  res;          \
      ULong src64 = (ULong)(_src); \
      memcpy(dstv, fives, sizeof(dstv)); \
      memcpy(res,  zeroes, sizeof(res)); \
      /* This assumes that gcc won't make any of %0, %1, %2 */ \
      /* be r11.  That should be ensured (cough, cough) */ \
      /* by declaring r11 to be clobbered. */ \
      __asm__ __volatile__(  \
         "movupd (%0), %%xmm2"    "\n\t"   /*dstv*/   \
         "movq   (%1), %%r11"     "\n\t"   /*src64*/  \
         _opname " $" #_imm ", %%r11" _srcsuffix ", %%xmm2"   "\n\t"  \
         "movupd  %%xmm2, (%2)" "\n" /*res*/                          \
         : /*out*/ \
         : /*in*/ "r"(&dstv), "r"(&src64), "r"(&res)  \
         : "cc", "memory", "xmm2", "r11"  \
      );  \
      showIGVV("r", (_opname), (_imm), src64, &dstv, &res); \
   }
#define DO_imm_mscalar_to_r(_opname, _imm, _src)       \
   {  \
      V128  dstv;         \
      V128  res;          \
      ULong src64 = (ULong)(_src); \
      memcpy(dstv, fives, sizeof(dstv)); \
      memcpy(res,  zeroes, sizeof(res)); \
      __asm__ __volatile__(  \
         "movupd (%0), %%xmm2"    "\n\t"   /*dstv*/   \
         _opname " $" #_imm ", (%1), %%xmm2"   "\n\t"  \
         "movupd  %%xmm2, (%2)" "\n" /*res*/                          \
         : /*out*/ \
         : /*in*/ "r"(&dstv), "r"(&src64), "r"(&res)  \
         : "cc", "memory", "xmm2"  \
      );  \
      showIGVV("m", (_opname), (_imm), src64, &dstv, &res); \
   }

#define DO_imm_mandrscalar_to_r(_opname, _imm, _src, _dstsuffix)   \
      DO_imm_rscalar_to_r( _opname, _imm, _src, _dstsuffix )       \
      DO_imm_mscalar_to_r( _opname, _imm, _src )





void test_BLENDPD ( void )
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

void test_BLENDPS ( void )
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

void test_DPPD ( void )
{
   V128 src, dst;
   {
      *(double*)(&src[0]) =  1.2345;
      *(double*)(&src[8]) = -6.78910;
      *(double*)(&dst[0]) = -11.121314;
      *(double*)(&dst[8]) =  15.161718;
      DO_imm_mandr_r("dppd", 0, src, dst);
      DO_imm_mandr_r("dppd", 1, src, dst);
      DO_imm_mandr_r("dppd", 2, src, dst);
      DO_imm_mandr_r("dppd", 3, src, dst);
      DO_imm_mandr_r("dppd", 4, src, dst);
      DO_imm_mandr_r("dppd", 5, src, dst);
      DO_imm_mandr_r("dppd", 6, src, dst);
      DO_imm_mandr_r("dppd", 7, src, dst);
      DO_imm_mandr_r("dppd", 8, src, dst);
      DO_imm_mandr_r("dppd", 9, src, dst);
      DO_imm_mandr_r("dppd", 10, src, dst);
      DO_imm_mandr_r("dppd", 11, src, dst);
      DO_imm_mandr_r("dppd", 12, src, dst);
      DO_imm_mandr_r("dppd", 13, src, dst);
      DO_imm_mandr_r("dppd", 14, src, dst);
      DO_imm_mandr_r("dppd", 15, src, dst);
      DO_imm_mandr_r("dppd", 16, src, dst);
      DO_imm_mandr_r("dppd", 17, src, dst);
      DO_imm_mandr_r("dppd", 18, src, dst);
      DO_imm_mandr_r("dppd", 19, src, dst);
      DO_imm_mandr_r("dppd", 20, src, dst);
      DO_imm_mandr_r("dppd", 21, src, dst);
      DO_imm_mandr_r("dppd", 22, src, dst);
      DO_imm_mandr_r("dppd", 23, src, dst);
      DO_imm_mandr_r("dppd", 24, src, dst);
      DO_imm_mandr_r("dppd", 25, src, dst);
      DO_imm_mandr_r("dppd", 26, src, dst);
      DO_imm_mandr_r("dppd", 27, src, dst);
      DO_imm_mandr_r("dppd", 28, src, dst);
      DO_imm_mandr_r("dppd", 29, src, dst);
      DO_imm_mandr_r("dppd", 30, src, dst);
      DO_imm_mandr_r("dppd", 31, src, dst);
      DO_imm_mandr_r("dppd", 32, src, dst);
      DO_imm_mandr_r("dppd", 33, src, dst);
      DO_imm_mandr_r("dppd", 34, src, dst);
      DO_imm_mandr_r("dppd", 35, src, dst);
      DO_imm_mandr_r("dppd", 36, src, dst);
      DO_imm_mandr_r("dppd", 37, src, dst);
      DO_imm_mandr_r("dppd", 38, src, dst);
      DO_imm_mandr_r("dppd", 39, src, dst);
      DO_imm_mandr_r("dppd", 40, src, dst);
      DO_imm_mandr_r("dppd", 41, src, dst);
      DO_imm_mandr_r("dppd", 42, src, dst);
      DO_imm_mandr_r("dppd", 43, src, dst);
      DO_imm_mandr_r("dppd", 44, src, dst);
      DO_imm_mandr_r("dppd", 45, src, dst);
      DO_imm_mandr_r("dppd", 46, src, dst);
      DO_imm_mandr_r("dppd", 47, src, dst);
      DO_imm_mandr_r("dppd", 48, src, dst);
      DO_imm_mandr_r("dppd", 49, src, dst);
      DO_imm_mandr_r("dppd", 50, src, dst);
      DO_imm_mandr_r("dppd", 51, src, dst);
      DO_imm_mandr_r("dppd", 52, src, dst);
      DO_imm_mandr_r("dppd", 53, src, dst);
      DO_imm_mandr_r("dppd", 54, src, dst);
      DO_imm_mandr_r("dppd", 55, src, dst);
      DO_imm_mandr_r("dppd", 56, src, dst);
      DO_imm_mandr_r("dppd", 57, src, dst);
      DO_imm_mandr_r("dppd", 58, src, dst);
      DO_imm_mandr_r("dppd", 59, src, dst);
      DO_imm_mandr_r("dppd", 60, src, dst);
      DO_imm_mandr_r("dppd", 61, src, dst);
      DO_imm_mandr_r("dppd", 62, src, dst);
      DO_imm_mandr_r("dppd", 63, src, dst);
      DO_imm_mandr_r("dppd", 64, src, dst);
      DO_imm_mandr_r("dppd", 65, src, dst);
      DO_imm_mandr_r("dppd", 66, src, dst);
      DO_imm_mandr_r("dppd", 67, src, dst);
      DO_imm_mandr_r("dppd", 68, src, dst);
      DO_imm_mandr_r("dppd", 69, src, dst);
      DO_imm_mandr_r("dppd", 70, src, dst);
      DO_imm_mandr_r("dppd", 71, src, dst);
      DO_imm_mandr_r("dppd", 72, src, dst);
      DO_imm_mandr_r("dppd", 73, src, dst);
      DO_imm_mandr_r("dppd", 74, src, dst);
      DO_imm_mandr_r("dppd", 75, src, dst);
      DO_imm_mandr_r("dppd", 76, src, dst);
      DO_imm_mandr_r("dppd", 77, src, dst);
      DO_imm_mandr_r("dppd", 78, src, dst);
      DO_imm_mandr_r("dppd", 79, src, dst);
      DO_imm_mandr_r("dppd", 80, src, dst);
      DO_imm_mandr_r("dppd", 81, src, dst);
      DO_imm_mandr_r("dppd", 82, src, dst);
      DO_imm_mandr_r("dppd", 83, src, dst);
      DO_imm_mandr_r("dppd", 84, src, dst);
      DO_imm_mandr_r("dppd", 85, src, dst);
      DO_imm_mandr_r("dppd", 86, src, dst);
      DO_imm_mandr_r("dppd", 87, src, dst);
      DO_imm_mandr_r("dppd", 88, src, dst);
      DO_imm_mandr_r("dppd", 89, src, dst);
      DO_imm_mandr_r("dppd", 90, src, dst);
      DO_imm_mandr_r("dppd", 91, src, dst);
      DO_imm_mandr_r("dppd", 92, src, dst);
      DO_imm_mandr_r("dppd", 93, src, dst);
      DO_imm_mandr_r("dppd", 94, src, dst);
      DO_imm_mandr_r("dppd", 95, src, dst);
      DO_imm_mandr_r("dppd", 96, src, dst);
      DO_imm_mandr_r("dppd", 97, src, dst);
      DO_imm_mandr_r("dppd", 98, src, dst);
      DO_imm_mandr_r("dppd", 99, src, dst);
      DO_imm_mandr_r("dppd", 100, src, dst);
      DO_imm_mandr_r("dppd", 101, src, dst);
      DO_imm_mandr_r("dppd", 102, src, dst);
      DO_imm_mandr_r("dppd", 103, src, dst);
      DO_imm_mandr_r("dppd", 104, src, dst);
      DO_imm_mandr_r("dppd", 105, src, dst);
      DO_imm_mandr_r("dppd", 106, src, dst);
      DO_imm_mandr_r("dppd", 107, src, dst);
      DO_imm_mandr_r("dppd", 108, src, dst);
      DO_imm_mandr_r("dppd", 109, src, dst);
      DO_imm_mandr_r("dppd", 110, src, dst);
      DO_imm_mandr_r("dppd", 111, src, dst);
      DO_imm_mandr_r("dppd", 112, src, dst);
      DO_imm_mandr_r("dppd", 113, src, dst);
      DO_imm_mandr_r("dppd", 114, src, dst);
      DO_imm_mandr_r("dppd", 115, src, dst);
      DO_imm_mandr_r("dppd", 116, src, dst);
      DO_imm_mandr_r("dppd", 117, src, dst);
      DO_imm_mandr_r("dppd", 118, src, dst);
      DO_imm_mandr_r("dppd", 119, src, dst);
      DO_imm_mandr_r("dppd", 120, src, dst);
      DO_imm_mandr_r("dppd", 121, src, dst);
      DO_imm_mandr_r("dppd", 122, src, dst);
      DO_imm_mandr_r("dppd", 123, src, dst);
      DO_imm_mandr_r("dppd", 124, src, dst);
      DO_imm_mandr_r("dppd", 125, src, dst);
      DO_imm_mandr_r("dppd", 126, src, dst);
      DO_imm_mandr_r("dppd", 127, src, dst);
      DO_imm_mandr_r("dppd", 128, src, dst);
      DO_imm_mandr_r("dppd", 129, src, dst);
      DO_imm_mandr_r("dppd", 130, src, dst);
      DO_imm_mandr_r("dppd", 131, src, dst);
      DO_imm_mandr_r("dppd", 132, src, dst);
      DO_imm_mandr_r("dppd", 133, src, dst);
      DO_imm_mandr_r("dppd", 134, src, dst);
      DO_imm_mandr_r("dppd", 135, src, dst);
      DO_imm_mandr_r("dppd", 136, src, dst);
      DO_imm_mandr_r("dppd", 137, src, dst);
      DO_imm_mandr_r("dppd", 138, src, dst);
      DO_imm_mandr_r("dppd", 139, src, dst);
      DO_imm_mandr_r("dppd", 140, src, dst);
      DO_imm_mandr_r("dppd", 141, src, dst);
      DO_imm_mandr_r("dppd", 142, src, dst);
      DO_imm_mandr_r("dppd", 143, src, dst);
      DO_imm_mandr_r("dppd", 144, src, dst);
      DO_imm_mandr_r("dppd", 145, src, dst);
      DO_imm_mandr_r("dppd", 146, src, dst);
      DO_imm_mandr_r("dppd", 147, src, dst);
      DO_imm_mandr_r("dppd", 148, src, dst);
      DO_imm_mandr_r("dppd", 149, src, dst);
      DO_imm_mandr_r("dppd", 150, src, dst);
      DO_imm_mandr_r("dppd", 151, src, dst);
      DO_imm_mandr_r("dppd", 152, src, dst);
      DO_imm_mandr_r("dppd", 153, src, dst);
      DO_imm_mandr_r("dppd", 154, src, dst);
      DO_imm_mandr_r("dppd", 155, src, dst);
      DO_imm_mandr_r("dppd", 156, src, dst);
      DO_imm_mandr_r("dppd", 157, src, dst);
      DO_imm_mandr_r("dppd", 158, src, dst);
      DO_imm_mandr_r("dppd", 159, src, dst);
      DO_imm_mandr_r("dppd", 160, src, dst);
      DO_imm_mandr_r("dppd", 161, src, dst);
      DO_imm_mandr_r("dppd", 162, src, dst);
      DO_imm_mandr_r("dppd", 163, src, dst);
      DO_imm_mandr_r("dppd", 164, src, dst);
      DO_imm_mandr_r("dppd", 165, src, dst);
      DO_imm_mandr_r("dppd", 166, src, dst);
      DO_imm_mandr_r("dppd", 167, src, dst);
      DO_imm_mandr_r("dppd", 168, src, dst);
      DO_imm_mandr_r("dppd", 169, src, dst);
      DO_imm_mandr_r("dppd", 170, src, dst);
      DO_imm_mandr_r("dppd", 171, src, dst);
      DO_imm_mandr_r("dppd", 172, src, dst);
      DO_imm_mandr_r("dppd", 173, src, dst);
      DO_imm_mandr_r("dppd", 174, src, dst);
      DO_imm_mandr_r("dppd", 175, src, dst);
      DO_imm_mandr_r("dppd", 176, src, dst);
      DO_imm_mandr_r("dppd", 177, src, dst);
      DO_imm_mandr_r("dppd", 178, src, dst);
      DO_imm_mandr_r("dppd", 179, src, dst);
      DO_imm_mandr_r("dppd", 180, src, dst);
      DO_imm_mandr_r("dppd", 181, src, dst);
      DO_imm_mandr_r("dppd", 182, src, dst);
      DO_imm_mandr_r("dppd", 183, src, dst);
      DO_imm_mandr_r("dppd", 184, src, dst);
      DO_imm_mandr_r("dppd", 185, src, dst);
      DO_imm_mandr_r("dppd", 186, src, dst);
      DO_imm_mandr_r("dppd", 187, src, dst);
      DO_imm_mandr_r("dppd", 188, src, dst);
      DO_imm_mandr_r("dppd", 189, src, dst);
      DO_imm_mandr_r("dppd", 190, src, dst);
      DO_imm_mandr_r("dppd", 191, src, dst);
      DO_imm_mandr_r("dppd", 192, src, dst);
      DO_imm_mandr_r("dppd", 193, src, dst);
      DO_imm_mandr_r("dppd", 194, src, dst);
      DO_imm_mandr_r("dppd", 195, src, dst);
      DO_imm_mandr_r("dppd", 196, src, dst);
      DO_imm_mandr_r("dppd", 197, src, dst);
      DO_imm_mandr_r("dppd", 198, src, dst);
      DO_imm_mandr_r("dppd", 199, src, dst);
      DO_imm_mandr_r("dppd", 200, src, dst);
      DO_imm_mandr_r("dppd", 201, src, dst);
      DO_imm_mandr_r("dppd", 202, src, dst);
      DO_imm_mandr_r("dppd", 203, src, dst);
      DO_imm_mandr_r("dppd", 204, src, dst);
      DO_imm_mandr_r("dppd", 205, src, dst);
      DO_imm_mandr_r("dppd", 206, src, dst);
      DO_imm_mandr_r("dppd", 207, src, dst);
      DO_imm_mandr_r("dppd", 208, src, dst);
      DO_imm_mandr_r("dppd", 209, src, dst);
      DO_imm_mandr_r("dppd", 210, src, dst);
      DO_imm_mandr_r("dppd", 211, src, dst);
      DO_imm_mandr_r("dppd", 212, src, dst);
      DO_imm_mandr_r("dppd", 213, src, dst);
      DO_imm_mandr_r("dppd", 214, src, dst);
      DO_imm_mandr_r("dppd", 215, src, dst);
      DO_imm_mandr_r("dppd", 216, src, dst);
      DO_imm_mandr_r("dppd", 217, src, dst);
      DO_imm_mandr_r("dppd", 218, src, dst);
      DO_imm_mandr_r("dppd", 219, src, dst);
      DO_imm_mandr_r("dppd", 220, src, dst);
      DO_imm_mandr_r("dppd", 221, src, dst);
      DO_imm_mandr_r("dppd", 222, src, dst);
      DO_imm_mandr_r("dppd", 223, src, dst);
      DO_imm_mandr_r("dppd", 224, src, dst);
      DO_imm_mandr_r("dppd", 225, src, dst);
      DO_imm_mandr_r("dppd", 226, src, dst);
      DO_imm_mandr_r("dppd", 227, src, dst);
      DO_imm_mandr_r("dppd", 228, src, dst);
      DO_imm_mandr_r("dppd", 229, src, dst);
      DO_imm_mandr_r("dppd", 230, src, dst);
      DO_imm_mandr_r("dppd", 231, src, dst);
      DO_imm_mandr_r("dppd", 232, src, dst);
      DO_imm_mandr_r("dppd", 233, src, dst);
      DO_imm_mandr_r("dppd", 234, src, dst);
      DO_imm_mandr_r("dppd", 235, src, dst);
      DO_imm_mandr_r("dppd", 236, src, dst);
      DO_imm_mandr_r("dppd", 237, src, dst);
      DO_imm_mandr_r("dppd", 238, src, dst);
      DO_imm_mandr_r("dppd", 239, src, dst);
      DO_imm_mandr_r("dppd", 240, src, dst);
      DO_imm_mandr_r("dppd", 241, src, dst);
      DO_imm_mandr_r("dppd", 242, src, dst);
      DO_imm_mandr_r("dppd", 243, src, dst);
      DO_imm_mandr_r("dppd", 244, src, dst);
      DO_imm_mandr_r("dppd", 245, src, dst);
      DO_imm_mandr_r("dppd", 246, src, dst);
      DO_imm_mandr_r("dppd", 247, src, dst);
      DO_imm_mandr_r("dppd", 248, src, dst);
      DO_imm_mandr_r("dppd", 249, src, dst);
      DO_imm_mandr_r("dppd", 250, src, dst);
      DO_imm_mandr_r("dppd", 251, src, dst);
      DO_imm_mandr_r("dppd", 252, src, dst);
      DO_imm_mandr_r("dppd", 253, src, dst);
      DO_imm_mandr_r("dppd", 254, src, dst);
      DO_imm_mandr_r("dppd", 255, src, dst);
   }
}

void test_DPPS ( void )
{
   V128 src, dst;
   {
      *(float*)(&src[0])  =   1.2;
      *(float*)(&src[4])  =  -3.4;
      *(float*)(&src[8])  =  -6.7;
      *(float*)(&src[12]) =   8.9;
      *(float*)(&dst[0])  = -10.11;
      *(float*)(&dst[4])  =  12.13;
      *(float*)(&dst[8])  =  14.15;
      *(float*)(&dst[12]) = -16.17;
      DO_imm_mandr_r("dpps", 0, src, dst);
      DO_imm_mandr_r("dpps", 1, src, dst);
      DO_imm_mandr_r("dpps", 2, src, dst);
      DO_imm_mandr_r("dpps", 3, src, dst);
      DO_imm_mandr_r("dpps", 4, src, dst);
      DO_imm_mandr_r("dpps", 5, src, dst);
      DO_imm_mandr_r("dpps", 6, src, dst);
      DO_imm_mandr_r("dpps", 7, src, dst);
      DO_imm_mandr_r("dpps", 8, src, dst);
      DO_imm_mandr_r("dpps", 9, src, dst);
      DO_imm_mandr_r("dpps", 10, src, dst);
      DO_imm_mandr_r("dpps", 11, src, dst);
      DO_imm_mandr_r("dpps", 12, src, dst);
      DO_imm_mandr_r("dpps", 13, src, dst);
      DO_imm_mandr_r("dpps", 14, src, dst);
      DO_imm_mandr_r("dpps", 15, src, dst);
      DO_imm_mandr_r("dpps", 16, src, dst);
      DO_imm_mandr_r("dpps", 17, src, dst);
      DO_imm_mandr_r("dpps", 18, src, dst);
      DO_imm_mandr_r("dpps", 19, src, dst);
      DO_imm_mandr_r("dpps", 20, src, dst);
      DO_imm_mandr_r("dpps", 21, src, dst);
      DO_imm_mandr_r("dpps", 22, src, dst);
      DO_imm_mandr_r("dpps", 23, src, dst);
      DO_imm_mandr_r("dpps", 24, src, dst);
      DO_imm_mandr_r("dpps", 25, src, dst);
      DO_imm_mandr_r("dpps", 26, src, dst);
      DO_imm_mandr_r("dpps", 27, src, dst);
      DO_imm_mandr_r("dpps", 28, src, dst);
      DO_imm_mandr_r("dpps", 29, src, dst);
      DO_imm_mandr_r("dpps", 30, src, dst);
      DO_imm_mandr_r("dpps", 31, src, dst);
      DO_imm_mandr_r("dpps", 32, src, dst);
      DO_imm_mandr_r("dpps", 33, src, dst);
      DO_imm_mandr_r("dpps", 34, src, dst);
      DO_imm_mandr_r("dpps", 35, src, dst);
      DO_imm_mandr_r("dpps", 36, src, dst);
      DO_imm_mandr_r("dpps", 37, src, dst);
      DO_imm_mandr_r("dpps", 38, src, dst);
      DO_imm_mandr_r("dpps", 39, src, dst);
      DO_imm_mandr_r("dpps", 40, src, dst);
      DO_imm_mandr_r("dpps", 41, src, dst);
      DO_imm_mandr_r("dpps", 42, src, dst);
      DO_imm_mandr_r("dpps", 43, src, dst);
      DO_imm_mandr_r("dpps", 44, src, dst);
      DO_imm_mandr_r("dpps", 45, src, dst);
      DO_imm_mandr_r("dpps", 46, src, dst);
      DO_imm_mandr_r("dpps", 47, src, dst);
      DO_imm_mandr_r("dpps", 48, src, dst);
      DO_imm_mandr_r("dpps", 49, src, dst);
      DO_imm_mandr_r("dpps", 50, src, dst);
      DO_imm_mandr_r("dpps", 51, src, dst);
      DO_imm_mandr_r("dpps", 52, src, dst);
      DO_imm_mandr_r("dpps", 53, src, dst);
      DO_imm_mandr_r("dpps", 54, src, dst);
      DO_imm_mandr_r("dpps", 55, src, dst);
      DO_imm_mandr_r("dpps", 56, src, dst);
      DO_imm_mandr_r("dpps", 57, src, dst);
      DO_imm_mandr_r("dpps", 58, src, dst);
      DO_imm_mandr_r("dpps", 59, src, dst);
      DO_imm_mandr_r("dpps", 60, src, dst);
      DO_imm_mandr_r("dpps", 61, src, dst);
      DO_imm_mandr_r("dpps", 62, src, dst);
      DO_imm_mandr_r("dpps", 63, src, dst);
      DO_imm_mandr_r("dpps", 64, src, dst);
      DO_imm_mandr_r("dpps", 65, src, dst);
      DO_imm_mandr_r("dpps", 66, src, dst);
      DO_imm_mandr_r("dpps", 67, src, dst);
      DO_imm_mandr_r("dpps", 68, src, dst);
      DO_imm_mandr_r("dpps", 69, src, dst);
      DO_imm_mandr_r("dpps", 70, src, dst);
      DO_imm_mandr_r("dpps", 71, src, dst);
      DO_imm_mandr_r("dpps", 72, src, dst);
      DO_imm_mandr_r("dpps", 73, src, dst);
      DO_imm_mandr_r("dpps", 74, src, dst);
      DO_imm_mandr_r("dpps", 75, src, dst);
      DO_imm_mandr_r("dpps", 76, src, dst);
      DO_imm_mandr_r("dpps", 77, src, dst);
      DO_imm_mandr_r("dpps", 78, src, dst);
      DO_imm_mandr_r("dpps", 79, src, dst);
      DO_imm_mandr_r("dpps", 80, src, dst);
      DO_imm_mandr_r("dpps", 81, src, dst);
      DO_imm_mandr_r("dpps", 82, src, dst);
      DO_imm_mandr_r("dpps", 83, src, dst);
      DO_imm_mandr_r("dpps", 84, src, dst);
      DO_imm_mandr_r("dpps", 85, src, dst);
      DO_imm_mandr_r("dpps", 86, src, dst);
      DO_imm_mandr_r("dpps", 87, src, dst);
      DO_imm_mandr_r("dpps", 88, src, dst);
      DO_imm_mandr_r("dpps", 89, src, dst);
      DO_imm_mandr_r("dpps", 90, src, dst);
      DO_imm_mandr_r("dpps", 91, src, dst);
      DO_imm_mandr_r("dpps", 92, src, dst);
      DO_imm_mandr_r("dpps", 93, src, dst);
      DO_imm_mandr_r("dpps", 94, src, dst);
      DO_imm_mandr_r("dpps", 95, src, dst);
      DO_imm_mandr_r("dpps", 96, src, dst);
      DO_imm_mandr_r("dpps", 97, src, dst);
      DO_imm_mandr_r("dpps", 98, src, dst);
      DO_imm_mandr_r("dpps", 99, src, dst);
      DO_imm_mandr_r("dpps", 100, src, dst);
      DO_imm_mandr_r("dpps", 101, src, dst);
      DO_imm_mandr_r("dpps", 102, src, dst);
      DO_imm_mandr_r("dpps", 103, src, dst);
      DO_imm_mandr_r("dpps", 104, src, dst);
      DO_imm_mandr_r("dpps", 105, src, dst);
      DO_imm_mandr_r("dpps", 106, src, dst);
      DO_imm_mandr_r("dpps", 107, src, dst);
      DO_imm_mandr_r("dpps", 108, src, dst);
      DO_imm_mandr_r("dpps", 109, src, dst);
      DO_imm_mandr_r("dpps", 110, src, dst);
      DO_imm_mandr_r("dpps", 111, src, dst);
      DO_imm_mandr_r("dpps", 112, src, dst);
      DO_imm_mandr_r("dpps", 113, src, dst);
      DO_imm_mandr_r("dpps", 114, src, dst);
      DO_imm_mandr_r("dpps", 115, src, dst);
      DO_imm_mandr_r("dpps", 116, src, dst);
      DO_imm_mandr_r("dpps", 117, src, dst);
      DO_imm_mandr_r("dpps", 118, src, dst);
      DO_imm_mandr_r("dpps", 119, src, dst);
      DO_imm_mandr_r("dpps", 120, src, dst);
      DO_imm_mandr_r("dpps", 121, src, dst);
      DO_imm_mandr_r("dpps", 122, src, dst);
      DO_imm_mandr_r("dpps", 123, src, dst);
      DO_imm_mandr_r("dpps", 124, src, dst);
      DO_imm_mandr_r("dpps", 125, src, dst);
      DO_imm_mandr_r("dpps", 126, src, dst);
      DO_imm_mandr_r("dpps", 127, src, dst);
      DO_imm_mandr_r("dpps", 128, src, dst);
      DO_imm_mandr_r("dpps", 129, src, dst);
      DO_imm_mandr_r("dpps", 130, src, dst);
      DO_imm_mandr_r("dpps", 131, src, dst);
      DO_imm_mandr_r("dpps", 132, src, dst);
      DO_imm_mandr_r("dpps", 133, src, dst);
      DO_imm_mandr_r("dpps", 134, src, dst);
      DO_imm_mandr_r("dpps", 135, src, dst);
      DO_imm_mandr_r("dpps", 136, src, dst);
      DO_imm_mandr_r("dpps", 137, src, dst);
      DO_imm_mandr_r("dpps", 138, src, dst);
      DO_imm_mandr_r("dpps", 139, src, dst);
      DO_imm_mandr_r("dpps", 140, src, dst);
      DO_imm_mandr_r("dpps", 141, src, dst);
      DO_imm_mandr_r("dpps", 142, src, dst);
      DO_imm_mandr_r("dpps", 143, src, dst);
      DO_imm_mandr_r("dpps", 144, src, dst);
      DO_imm_mandr_r("dpps", 145, src, dst);
      DO_imm_mandr_r("dpps", 146, src, dst);
      DO_imm_mandr_r("dpps", 147, src, dst);
      DO_imm_mandr_r("dpps", 148, src, dst);
      DO_imm_mandr_r("dpps", 149, src, dst);
      DO_imm_mandr_r("dpps", 150, src, dst);
      DO_imm_mandr_r("dpps", 151, src, dst);
      DO_imm_mandr_r("dpps", 152, src, dst);
      DO_imm_mandr_r("dpps", 153, src, dst);
      DO_imm_mandr_r("dpps", 154, src, dst);
      DO_imm_mandr_r("dpps", 155, src, dst);
      DO_imm_mandr_r("dpps", 156, src, dst);
      DO_imm_mandr_r("dpps", 157, src, dst);
      DO_imm_mandr_r("dpps", 158, src, dst);
      DO_imm_mandr_r("dpps", 159, src, dst);
      DO_imm_mandr_r("dpps", 160, src, dst);
      DO_imm_mandr_r("dpps", 161, src, dst);
      DO_imm_mandr_r("dpps", 162, src, dst);
      DO_imm_mandr_r("dpps", 163, src, dst);
      DO_imm_mandr_r("dpps", 164, src, dst);
      DO_imm_mandr_r("dpps", 165, src, dst);
      DO_imm_mandr_r("dpps", 166, src, dst);
      DO_imm_mandr_r("dpps", 167, src, dst);
      DO_imm_mandr_r("dpps", 168, src, dst);
      DO_imm_mandr_r("dpps", 169, src, dst);
      DO_imm_mandr_r("dpps", 170, src, dst);
      DO_imm_mandr_r("dpps", 171, src, dst);
      DO_imm_mandr_r("dpps", 172, src, dst);
      DO_imm_mandr_r("dpps", 173, src, dst);
      DO_imm_mandr_r("dpps", 174, src, dst);
      DO_imm_mandr_r("dpps", 175, src, dst);
      DO_imm_mandr_r("dpps", 176, src, dst);
      DO_imm_mandr_r("dpps", 177, src, dst);
      DO_imm_mandr_r("dpps", 178, src, dst);
      DO_imm_mandr_r("dpps", 179, src, dst);
      DO_imm_mandr_r("dpps", 180, src, dst);
      DO_imm_mandr_r("dpps", 181, src, dst);
      DO_imm_mandr_r("dpps", 182, src, dst);
      DO_imm_mandr_r("dpps", 183, src, dst);
      DO_imm_mandr_r("dpps", 184, src, dst);
      DO_imm_mandr_r("dpps", 185, src, dst);
      DO_imm_mandr_r("dpps", 186, src, dst);
      DO_imm_mandr_r("dpps", 187, src, dst);
      DO_imm_mandr_r("dpps", 188, src, dst);
      DO_imm_mandr_r("dpps", 189, src, dst);
      DO_imm_mandr_r("dpps", 190, src, dst);
      DO_imm_mandr_r("dpps", 191, src, dst);
      DO_imm_mandr_r("dpps", 192, src, dst);
      DO_imm_mandr_r("dpps", 193, src, dst);
      DO_imm_mandr_r("dpps", 194, src, dst);
      DO_imm_mandr_r("dpps", 195, src, dst);
      DO_imm_mandr_r("dpps", 196, src, dst);
      DO_imm_mandr_r("dpps", 197, src, dst);
      DO_imm_mandr_r("dpps", 198, src, dst);
      DO_imm_mandr_r("dpps", 199, src, dst);
      DO_imm_mandr_r("dpps", 200, src, dst);
      DO_imm_mandr_r("dpps", 201, src, dst);
      DO_imm_mandr_r("dpps", 202, src, dst);
      DO_imm_mandr_r("dpps", 203, src, dst);
      DO_imm_mandr_r("dpps", 204, src, dst);
      DO_imm_mandr_r("dpps", 205, src, dst);
      DO_imm_mandr_r("dpps", 206, src, dst);
      DO_imm_mandr_r("dpps", 207, src, dst);
      DO_imm_mandr_r("dpps", 208, src, dst);
      DO_imm_mandr_r("dpps", 209, src, dst);
      DO_imm_mandr_r("dpps", 210, src, dst);
      DO_imm_mandr_r("dpps", 211, src, dst);
      DO_imm_mandr_r("dpps", 212, src, dst);
      DO_imm_mandr_r("dpps", 213, src, dst);
      DO_imm_mandr_r("dpps", 214, src, dst);
      DO_imm_mandr_r("dpps", 215, src, dst);
      DO_imm_mandr_r("dpps", 216, src, dst);
      DO_imm_mandr_r("dpps", 217, src, dst);
      DO_imm_mandr_r("dpps", 218, src, dst);
      DO_imm_mandr_r("dpps", 219, src, dst);
      DO_imm_mandr_r("dpps", 220, src, dst);
      DO_imm_mandr_r("dpps", 221, src, dst);
      DO_imm_mandr_r("dpps", 222, src, dst);
      DO_imm_mandr_r("dpps", 223, src, dst);
      DO_imm_mandr_r("dpps", 224, src, dst);
      DO_imm_mandr_r("dpps", 225, src, dst);
      DO_imm_mandr_r("dpps", 226, src, dst);
      DO_imm_mandr_r("dpps", 227, src, dst);
      DO_imm_mandr_r("dpps", 228, src, dst);
      DO_imm_mandr_r("dpps", 229, src, dst);
      DO_imm_mandr_r("dpps", 230, src, dst);
      DO_imm_mandr_r("dpps", 231, src, dst);
      DO_imm_mandr_r("dpps", 232, src, dst);
      DO_imm_mandr_r("dpps", 233, src, dst);
      DO_imm_mandr_r("dpps", 234, src, dst);
      DO_imm_mandr_r("dpps", 235, src, dst);
      DO_imm_mandr_r("dpps", 236, src, dst);
      DO_imm_mandr_r("dpps", 237, src, dst);
      DO_imm_mandr_r("dpps", 238, src, dst);
      DO_imm_mandr_r("dpps", 239, src, dst);
      DO_imm_mandr_r("dpps", 240, src, dst);
      DO_imm_mandr_r("dpps", 241, src, dst);
      DO_imm_mandr_r("dpps", 242, src, dst);
      DO_imm_mandr_r("dpps", 243, src, dst);
      DO_imm_mandr_r("dpps", 244, src, dst);
      DO_imm_mandr_r("dpps", 245, src, dst);
      DO_imm_mandr_r("dpps", 246, src, dst);
      DO_imm_mandr_r("dpps", 247, src, dst);
      DO_imm_mandr_r("dpps", 248, src, dst);
      DO_imm_mandr_r("dpps", 249, src, dst);
      DO_imm_mandr_r("dpps", 250, src, dst);
      DO_imm_mandr_r("dpps", 251, src, dst);
      DO_imm_mandr_r("dpps", 252, src, dst);
      DO_imm_mandr_r("dpps", 253, src, dst);
      DO_imm_mandr_r("dpps", 254, src, dst);
      DO_imm_mandr_r("dpps", 255, src, dst);
   }
}

void test_INSERTPS ( void )
{
   V128 src, dst;
   {
      *(float*)(&src[0])  =   1.2;
      *(float*)(&src[4])  =  -3.4;
      *(float*)(&src[8])  =  -6.7;
      *(float*)(&src[12]) =   8.9;
      *(float*)(&dst[0])  = -10.11;
      *(float*)(&dst[4])  =  12.13;
      *(float*)(&dst[8])  =  14.15;
      *(float*)(&dst[12]) = -16.17;
      DO_imm_mandr_r("insertps", 0, src, dst);
      DO_imm_mandr_r("insertps", 1, src, dst);
      DO_imm_mandr_r("insertps", 2, src, dst);
      DO_imm_mandr_r("insertps", 3, src, dst);
      DO_imm_mandr_r("insertps", 4, src, dst);
      DO_imm_mandr_r("insertps", 5, src, dst);
      DO_imm_mandr_r("insertps", 6, src, dst);
      DO_imm_mandr_r("insertps", 7, src, dst);
      DO_imm_mandr_r("insertps", 8, src, dst);
      DO_imm_mandr_r("insertps", 9, src, dst);
      DO_imm_mandr_r("insertps", 10, src, dst);
      DO_imm_mandr_r("insertps", 11, src, dst);
      DO_imm_mandr_r("insertps", 12, src, dst);
      DO_imm_mandr_r("insertps", 13, src, dst);
      DO_imm_mandr_r("insertps", 14, src, dst);
      DO_imm_mandr_r("insertps", 15, src, dst);
      DO_imm_mandr_r("insertps", 16, src, dst);
      DO_imm_mandr_r("insertps", 17, src, dst);
      DO_imm_mandr_r("insertps", 18, src, dst);
      DO_imm_mandr_r("insertps", 19, src, dst);
      DO_imm_mandr_r("insertps", 20, src, dst);
      DO_imm_mandr_r("insertps", 21, src, dst);
      DO_imm_mandr_r("insertps", 22, src, dst);
      DO_imm_mandr_r("insertps", 23, src, dst);
      DO_imm_mandr_r("insertps", 24, src, dst);
      DO_imm_mandr_r("insertps", 25, src, dst);
      DO_imm_mandr_r("insertps", 26, src, dst);
      DO_imm_mandr_r("insertps", 27, src, dst);
      DO_imm_mandr_r("insertps", 28, src, dst);
      DO_imm_mandr_r("insertps", 29, src, dst);
      DO_imm_mandr_r("insertps", 30, src, dst);
      DO_imm_mandr_r("insertps", 31, src, dst);
      DO_imm_mandr_r("insertps", 32, src, dst);
      DO_imm_mandr_r("insertps", 33, src, dst);
      DO_imm_mandr_r("insertps", 34, src, dst);
      DO_imm_mandr_r("insertps", 35, src, dst);
      DO_imm_mandr_r("insertps", 36, src, dst);
      DO_imm_mandr_r("insertps", 37, src, dst);
      DO_imm_mandr_r("insertps", 38, src, dst);
      DO_imm_mandr_r("insertps", 39, src, dst);
      DO_imm_mandr_r("insertps", 40, src, dst);
      DO_imm_mandr_r("insertps", 41, src, dst);
      DO_imm_mandr_r("insertps", 42, src, dst);
      DO_imm_mandr_r("insertps", 43, src, dst);
      DO_imm_mandr_r("insertps", 44, src, dst);
      DO_imm_mandr_r("insertps", 45, src, dst);
      DO_imm_mandr_r("insertps", 46, src, dst);
      DO_imm_mandr_r("insertps", 47, src, dst);
      DO_imm_mandr_r("insertps", 48, src, dst);
      DO_imm_mandr_r("insertps", 49, src, dst);
      DO_imm_mandr_r("insertps", 50, src, dst);
      DO_imm_mandr_r("insertps", 51, src, dst);
      DO_imm_mandr_r("insertps", 52, src, dst);
      DO_imm_mandr_r("insertps", 53, src, dst);
      DO_imm_mandr_r("insertps", 54, src, dst);
      DO_imm_mandr_r("insertps", 55, src, dst);
      DO_imm_mandr_r("insertps", 56, src, dst);
      DO_imm_mandr_r("insertps", 57, src, dst);
      DO_imm_mandr_r("insertps", 58, src, dst);
      DO_imm_mandr_r("insertps", 59, src, dst);
      DO_imm_mandr_r("insertps", 60, src, dst);
      DO_imm_mandr_r("insertps", 61, src, dst);
      DO_imm_mandr_r("insertps", 62, src, dst);
      DO_imm_mandr_r("insertps", 63, src, dst);
      DO_imm_mandr_r("insertps", 64, src, dst);
      DO_imm_mandr_r("insertps", 65, src, dst);
      DO_imm_mandr_r("insertps", 66, src, dst);
      DO_imm_mandr_r("insertps", 67, src, dst);
      DO_imm_mandr_r("insertps", 68, src, dst);
      DO_imm_mandr_r("insertps", 69, src, dst);
      DO_imm_mandr_r("insertps", 70, src, dst);
      DO_imm_mandr_r("insertps", 71, src, dst);
      DO_imm_mandr_r("insertps", 72, src, dst);
      DO_imm_mandr_r("insertps", 73, src, dst);
      DO_imm_mandr_r("insertps", 74, src, dst);
      DO_imm_mandr_r("insertps", 75, src, dst);
      DO_imm_mandr_r("insertps", 76, src, dst);
      DO_imm_mandr_r("insertps", 77, src, dst);
      DO_imm_mandr_r("insertps", 78, src, dst);
      DO_imm_mandr_r("insertps", 79, src, dst);
      DO_imm_mandr_r("insertps", 80, src, dst);
      DO_imm_mandr_r("insertps", 81, src, dst);
      DO_imm_mandr_r("insertps", 82, src, dst);
      DO_imm_mandr_r("insertps", 83, src, dst);
      DO_imm_mandr_r("insertps", 84, src, dst);
      DO_imm_mandr_r("insertps", 85, src, dst);
      DO_imm_mandr_r("insertps", 86, src, dst);
      DO_imm_mandr_r("insertps", 87, src, dst);
      DO_imm_mandr_r("insertps", 88, src, dst);
      DO_imm_mandr_r("insertps", 89, src, dst);
      DO_imm_mandr_r("insertps", 90, src, dst);
      DO_imm_mandr_r("insertps", 91, src, dst);
      DO_imm_mandr_r("insertps", 92, src, dst);
      DO_imm_mandr_r("insertps", 93, src, dst);
      DO_imm_mandr_r("insertps", 94, src, dst);
      DO_imm_mandr_r("insertps", 95, src, dst);
      DO_imm_mandr_r("insertps", 96, src, dst);
      DO_imm_mandr_r("insertps", 97, src, dst);
      DO_imm_mandr_r("insertps", 98, src, dst);
      DO_imm_mandr_r("insertps", 99, src, dst);
      DO_imm_mandr_r("insertps", 100, src, dst);
      DO_imm_mandr_r("insertps", 101, src, dst);
      DO_imm_mandr_r("insertps", 102, src, dst);
      DO_imm_mandr_r("insertps", 103, src, dst);
      DO_imm_mandr_r("insertps", 104, src, dst);
      DO_imm_mandr_r("insertps", 105, src, dst);
      DO_imm_mandr_r("insertps", 106, src, dst);
      DO_imm_mandr_r("insertps", 107, src, dst);
      DO_imm_mandr_r("insertps", 108, src, dst);
      DO_imm_mandr_r("insertps", 109, src, dst);
      DO_imm_mandr_r("insertps", 110, src, dst);
      DO_imm_mandr_r("insertps", 111, src, dst);
      DO_imm_mandr_r("insertps", 112, src, dst);
      DO_imm_mandr_r("insertps", 113, src, dst);
      DO_imm_mandr_r("insertps", 114, src, dst);
      DO_imm_mandr_r("insertps", 115, src, dst);
      DO_imm_mandr_r("insertps", 116, src, dst);
      DO_imm_mandr_r("insertps", 117, src, dst);
      DO_imm_mandr_r("insertps", 118, src, dst);
      DO_imm_mandr_r("insertps", 119, src, dst);
      DO_imm_mandr_r("insertps", 120, src, dst);
      DO_imm_mandr_r("insertps", 121, src, dst);
      DO_imm_mandr_r("insertps", 122, src, dst);
      DO_imm_mandr_r("insertps", 123, src, dst);
      DO_imm_mandr_r("insertps", 124, src, dst);
      DO_imm_mandr_r("insertps", 125, src, dst);
      DO_imm_mandr_r("insertps", 126, src, dst);
      DO_imm_mandr_r("insertps", 127, src, dst);
      DO_imm_mandr_r("insertps", 128, src, dst);
      DO_imm_mandr_r("insertps", 129, src, dst);
      DO_imm_mandr_r("insertps", 130, src, dst);
      DO_imm_mandr_r("insertps", 131, src, dst);
      DO_imm_mandr_r("insertps", 132, src, dst);
      DO_imm_mandr_r("insertps", 133, src, dst);
      DO_imm_mandr_r("insertps", 134, src, dst);
      DO_imm_mandr_r("insertps", 135, src, dst);
      DO_imm_mandr_r("insertps", 136, src, dst);
      DO_imm_mandr_r("insertps", 137, src, dst);
      DO_imm_mandr_r("insertps", 138, src, dst);
      DO_imm_mandr_r("insertps", 139, src, dst);
      DO_imm_mandr_r("insertps", 140, src, dst);
      DO_imm_mandr_r("insertps", 141, src, dst);
      DO_imm_mandr_r("insertps", 142, src, dst);
      DO_imm_mandr_r("insertps", 143, src, dst);
      DO_imm_mandr_r("insertps", 144, src, dst);
      DO_imm_mandr_r("insertps", 145, src, dst);
      DO_imm_mandr_r("insertps", 146, src, dst);
      DO_imm_mandr_r("insertps", 147, src, dst);
      DO_imm_mandr_r("insertps", 148, src, dst);
      DO_imm_mandr_r("insertps", 149, src, dst);
      DO_imm_mandr_r("insertps", 150, src, dst);
      DO_imm_mandr_r("insertps", 151, src, dst);
      DO_imm_mandr_r("insertps", 152, src, dst);
      DO_imm_mandr_r("insertps", 153, src, dst);
      DO_imm_mandr_r("insertps", 154, src, dst);
      DO_imm_mandr_r("insertps", 155, src, dst);
      DO_imm_mandr_r("insertps", 156, src, dst);
      DO_imm_mandr_r("insertps", 157, src, dst);
      DO_imm_mandr_r("insertps", 158, src, dst);
      DO_imm_mandr_r("insertps", 159, src, dst);
      DO_imm_mandr_r("insertps", 160, src, dst);
      DO_imm_mandr_r("insertps", 161, src, dst);
      DO_imm_mandr_r("insertps", 162, src, dst);
      DO_imm_mandr_r("insertps", 163, src, dst);
      DO_imm_mandr_r("insertps", 164, src, dst);
      DO_imm_mandr_r("insertps", 165, src, dst);
      DO_imm_mandr_r("insertps", 166, src, dst);
      DO_imm_mandr_r("insertps", 167, src, dst);
      DO_imm_mandr_r("insertps", 168, src, dst);
      DO_imm_mandr_r("insertps", 169, src, dst);
      DO_imm_mandr_r("insertps", 170, src, dst);
      DO_imm_mandr_r("insertps", 171, src, dst);
      DO_imm_mandr_r("insertps", 172, src, dst);
      DO_imm_mandr_r("insertps", 173, src, dst);
      DO_imm_mandr_r("insertps", 174, src, dst);
      DO_imm_mandr_r("insertps", 175, src, dst);
      DO_imm_mandr_r("insertps", 176, src, dst);
      DO_imm_mandr_r("insertps", 177, src, dst);
      DO_imm_mandr_r("insertps", 178, src, dst);
      DO_imm_mandr_r("insertps", 179, src, dst);
      DO_imm_mandr_r("insertps", 180, src, dst);
      DO_imm_mandr_r("insertps", 181, src, dst);
      DO_imm_mandr_r("insertps", 182, src, dst);
      DO_imm_mandr_r("insertps", 183, src, dst);
      DO_imm_mandr_r("insertps", 184, src, dst);
      DO_imm_mandr_r("insertps", 185, src, dst);
      DO_imm_mandr_r("insertps", 186, src, dst);
      DO_imm_mandr_r("insertps", 187, src, dst);
      DO_imm_mandr_r("insertps", 188, src, dst);
      DO_imm_mandr_r("insertps", 189, src, dst);
      DO_imm_mandr_r("insertps", 190, src, dst);
      DO_imm_mandr_r("insertps", 191, src, dst);
      DO_imm_mandr_r("insertps", 192, src, dst);
      DO_imm_mandr_r("insertps", 193, src, dst);
      DO_imm_mandr_r("insertps", 194, src, dst);
      DO_imm_mandr_r("insertps", 195, src, dst);
      DO_imm_mandr_r("insertps", 196, src, dst);
      DO_imm_mandr_r("insertps", 197, src, dst);
      DO_imm_mandr_r("insertps", 198, src, dst);
      DO_imm_mandr_r("insertps", 199, src, dst);
      DO_imm_mandr_r("insertps", 200, src, dst);
      DO_imm_mandr_r("insertps", 201, src, dst);
      DO_imm_mandr_r("insertps", 202, src, dst);
      DO_imm_mandr_r("insertps", 203, src, dst);
      DO_imm_mandr_r("insertps", 204, src, dst);
      DO_imm_mandr_r("insertps", 205, src, dst);
      DO_imm_mandr_r("insertps", 206, src, dst);
      DO_imm_mandr_r("insertps", 207, src, dst);
      DO_imm_mandr_r("insertps", 208, src, dst);
      DO_imm_mandr_r("insertps", 209, src, dst);
      DO_imm_mandr_r("insertps", 210, src, dst);
      DO_imm_mandr_r("insertps", 211, src, dst);
      DO_imm_mandr_r("insertps", 212, src, dst);
      DO_imm_mandr_r("insertps", 213, src, dst);
      DO_imm_mandr_r("insertps", 214, src, dst);
      DO_imm_mandr_r("insertps", 215, src, dst);
      DO_imm_mandr_r("insertps", 216, src, dst);
      DO_imm_mandr_r("insertps", 217, src, dst);
      DO_imm_mandr_r("insertps", 218, src, dst);
      DO_imm_mandr_r("insertps", 219, src, dst);
      DO_imm_mandr_r("insertps", 220, src, dst);
      DO_imm_mandr_r("insertps", 221, src, dst);
      DO_imm_mandr_r("insertps", 222, src, dst);
      DO_imm_mandr_r("insertps", 223, src, dst);
      DO_imm_mandr_r("insertps", 224, src, dst);
      DO_imm_mandr_r("insertps", 225, src, dst);
      DO_imm_mandr_r("insertps", 226, src, dst);
      DO_imm_mandr_r("insertps", 227, src, dst);
      DO_imm_mandr_r("insertps", 228, src, dst);
      DO_imm_mandr_r("insertps", 229, src, dst);
      DO_imm_mandr_r("insertps", 230, src, dst);
      DO_imm_mandr_r("insertps", 231, src, dst);
      DO_imm_mandr_r("insertps", 232, src, dst);
      DO_imm_mandr_r("insertps", 233, src, dst);
      DO_imm_mandr_r("insertps", 234, src, dst);
      DO_imm_mandr_r("insertps", 235, src, dst);
      DO_imm_mandr_r("insertps", 236, src, dst);
      DO_imm_mandr_r("insertps", 237, src, dst);
      DO_imm_mandr_r("insertps", 238, src, dst);
      DO_imm_mandr_r("insertps", 239, src, dst);
      DO_imm_mandr_r("insertps", 240, src, dst);
      DO_imm_mandr_r("insertps", 241, src, dst);
      DO_imm_mandr_r("insertps", 242, src, dst);
      DO_imm_mandr_r("insertps", 243, src, dst);
      DO_imm_mandr_r("insertps", 244, src, dst);
      DO_imm_mandr_r("insertps", 245, src, dst);
      DO_imm_mandr_r("insertps", 246, src, dst);
      DO_imm_mandr_r("insertps", 247, src, dst);
      DO_imm_mandr_r("insertps", 248, src, dst);
      DO_imm_mandr_r("insertps", 249, src, dst);
      DO_imm_mandr_r("insertps", 250, src, dst);
      DO_imm_mandr_r("insertps", 251, src, dst);
      DO_imm_mandr_r("insertps", 252, src, dst);
      DO_imm_mandr_r("insertps", 253, src, dst);
      DO_imm_mandr_r("insertps", 254, src, dst);
      DO_imm_mandr_r("insertps", 255, src, dst);
   }
}

void test_MPSADBW ( void )
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

void test_PACKUSDW ( void )
{
   V128 src, dst;
   Int i;
   for (i = 0; i < 10; i++) {
      if (i < 9) {
         randV128(&src);
         randV128(&dst);
      } else {
         memset(&src, 0, sizeof(src));
         memset(&dst, 0, sizeof(src));
         src[0] = 0x11; src[1] = 0x22;
         src[4] = 0x33; src[5] = 0x44;
         src[8] = 0x55; src[9] = 0x66;
         src[12] = 0x77; src[13] = 0x88;
         dst[0] = 0xaa; dst[1] = 0xbb;
         dst[4] = 0xcc; dst[5] = 0xdd;
         dst[8] = 0xee; dst[9] = 0xff;
         dst[12] = 0xa1; dst[13] = 0xb2;
      }
      DO_mandr_r("packusdw", src, dst);
   }
}

void test_PBLENDW ( void )
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


void test_PCMPEQQ ( void )
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


void test_PEXTRB ( void )
{
   V128 src;
   randV128(&src);
   DO_imm_r_to_mandrscalar("pextrb", 0, src, "d");
   DO_imm_r_to_mandrscalar("pextrb", 1, src, "d");
   DO_imm_r_to_mandrscalar("pextrb", 2, src, "d");
   DO_imm_r_to_mandrscalar("pextrb", 3, src, "d");
   DO_imm_r_to_mandrscalar("pextrb", 4, src, "d");
   DO_imm_r_to_mandrscalar("pextrb", 5, src, "d");
   DO_imm_r_to_mandrscalar("pextrb", 6, src, "d");
   DO_imm_r_to_mandrscalar("pextrb", 7, src, "d");
   DO_imm_r_to_mandrscalar("pextrb", 8, src, "d");
   DO_imm_r_to_mandrscalar("pextrb", 9, src, "d");
   DO_imm_r_to_mandrscalar("pextrb", 10, src, "d");
   DO_imm_r_to_mandrscalar("pextrb", 11, src, "d");
   DO_imm_r_to_mandrscalar("pextrb", 12, src, "d");
   DO_imm_r_to_mandrscalar("pextrb", 13, src, "d");
   DO_imm_r_to_mandrscalar("pextrb", 14, src, "d");
   DO_imm_r_to_mandrscalar("pextrb", 15, src, "d");
}

void test_PINSRB ( void )
{
   ULong src;
   src = randULong();
   DO_imm_mandrscalar_to_r("pinsrb", 0, src, "d");
   src = randULong();
   DO_imm_mandrscalar_to_r("pinsrb", 1, src, "d");
   src = randULong();
   DO_imm_mandrscalar_to_r("pinsrb", 2, src, "d");
   src = randULong();
   DO_imm_mandrscalar_to_r("pinsrb", 3, src, "d");
   src = randULong();
   DO_imm_mandrscalar_to_r("pinsrb", 4, src, "d");
   src = randULong();
   DO_imm_mandrscalar_to_r("pinsrb", 5, src, "d");
   src = randULong();
   DO_imm_mandrscalar_to_r("pinsrb", 6, src, "d");
   src = randULong();
   DO_imm_mandrscalar_to_r("pinsrb", 7, src, "d");
   src = randULong();
   DO_imm_mandrscalar_to_r("pinsrb", 8, src, "d");
   src = randULong();
   DO_imm_mandrscalar_to_r("pinsrb", 9, src, "d");
   src = randULong();
   DO_imm_mandrscalar_to_r("pinsrb", 10, src, "d");
   src = randULong();
   DO_imm_mandrscalar_to_r("pinsrb", 11, src, "d");
   src = randULong();
   DO_imm_mandrscalar_to_r("pinsrb", 12, src, "d");
   src = randULong();
   DO_imm_mandrscalar_to_r("pinsrb", 13, src, "d");
   src = randULong();
   DO_imm_mandrscalar_to_r("pinsrb", 14, src, "d");
   src = randULong();
   DO_imm_mandrscalar_to_r("pinsrb", 15, src, "d");
}


void test_PEXTRW ( void )
{
   V128 src;
   randV128(&src);
   DO_imm_r_to_mandrscalar("pextrw", 0, src, "d");
   DO_imm_r_to_mandrscalar("pextrw", 1, src, "d");
   DO_imm_r_to_mandrscalar("pextrw", 2, src, "d");
   DO_imm_r_to_mandrscalar("pextrw", 3, src, "d");
   DO_imm_r_to_mandrscalar("pextrw", 4, src, "d");
   DO_imm_r_to_mandrscalar("pextrw", 5, src, "d");
   DO_imm_r_to_mandrscalar("pextrw", 6, src, "d");
   DO_imm_r_to_mandrscalar("pextrw", 7, src, "d");
}

void test_PINSRW ( void )
{
   ULong src;
   src = randULong();
   DO_imm_mandrscalar_to_r("pinsrw", 0, src, "d");
   src = randULong();
   DO_imm_mandrscalar_to_r("pinsrw", 1, src, "d");
   src = randULong();
   DO_imm_mandrscalar_to_r("pinsrw", 2, src, "d");
   src = randULong();
   DO_imm_mandrscalar_to_r("pinsrw", 3, src, "d");
   src = randULong();
   DO_imm_mandrscalar_to_r("pinsrw", 4, src, "d");
   src = randULong();
   DO_imm_mandrscalar_to_r("pinsrw", 5, src, "d");
   src = randULong();
   DO_imm_mandrscalar_to_r("pinsrw", 6, src, "d");
   src = randULong();
   DO_imm_mandrscalar_to_r("pinsrw", 7, src, "d");
}


void test_PEXTRD ( void )
{
   V128 src;
   randV128(&src);
   DO_imm_r_to_mandrscalar("pextrd", 0, src, "d");
   DO_imm_r_to_mandrscalar("pextrd", 1, src, "d");
   DO_imm_r_to_mandrscalar("pextrd", 2, src, "d");
   DO_imm_r_to_mandrscalar("pextrd", 3, src, "d");
}

void test_PINSRD ( void )
{
   ULong src;
   src = randULong();
   DO_imm_mandrscalar_to_r("pinsrd", 0, src, "d");
   src = randULong();
   DO_imm_mandrscalar_to_r("pinsrd", 1, src, "d");
   src = randULong();
   DO_imm_mandrscalar_to_r("pinsrd", 2, src, "d");
   src = randULong();
   DO_imm_mandrscalar_to_r("pinsrd", 3, src, "d");
}


void test_PEXTRQ ( void )
{
   V128 src;
   randV128(&src);
   DO_imm_r_to_mandrscalar("pextrq", 0, src, "");
   DO_imm_r_to_mandrscalar("pextrq", 1, src, "");
}

void test_PINSRQ ( void )
{
   ULong src;
   src = randULong();
   DO_imm_mandrscalar_to_r("pinsrq", 0, src, "");
   src = randULong();
   DO_imm_mandrscalar_to_r("pinsrq", 1, src, "");
}


void test_EXTRACTPS ( void )
{
   V128 src;
   randV128(&src);
   DO_imm_r_to_mandrscalar("extractps", 0, src, "d");
   DO_imm_r_to_mandrscalar("extractps", 1, src, "d");
   DO_imm_r_to_mandrscalar("extractps", 2, src, "d");
   DO_imm_r_to_mandrscalar("extractps", 3, src, "d");
}


void test_PHMINPOSUW ( void )
{
   V128 src, dst;
   Int i;
   for (i = 0; i < 20; i++) {
      randV128(&src);
      randV128(&dst);
      DO_mandr_r("phminposuw", src, dst);
   }
   memset(src, 0x55, sizeof(src));
   memset(dst, 0xAA, sizeof(dst));
   DO_mandr_r("phminposuw", src, dst);
}

void test_PMAXSB ( void )
{
   V128 src, dst;
   Int i;
   for (i = 0; i < 10; i++) {
      randV128(&src);
      randV128(&dst);
      DO_mandr_r("pmaxsb", src, dst);
   }
}

void test_PMAXSD ( void )
{
   V128 src, dst;
   Int i;
   for (i = 0; i < 10; i++) {
      randV128(&src);
      randV128(&dst);
      DO_mandr_r("pmaxsd", src, dst);
   }
}

void test_PMAXUD ( void )
{
   V128 src, dst;
   Int i;
   for (i = 0; i < 10; i++) {
      randV128(&src);
      randV128(&dst);
      DO_mandr_r("pmaxud", src, dst);
   }
}

void test_PMAXUW ( void )
{
   V128 src, dst;
   Int i;
   for (i = 0; i < 10; i++) {
      randV128(&src);
      randV128(&dst);
      DO_mandr_r("pmaxuw", src, dst);
   }
}

void test_PMINSB ( void )
{
   V128 src, dst;
   Int i;
   for (i = 0; i < 10; i++) {
      randV128(&src);
      randV128(&dst);
      DO_mandr_r("pminsb", src, dst);
   }
}

void test_PMINSD ( void )
{
   V128 src, dst;
   Int i;
   for (i = 0; i < 10; i++) {
      randV128(&src);
      randV128(&dst);
      DO_mandr_r("pminsd", src, dst);
   }
}

void test_PMINUD ( void )
{
   V128 src, dst;
   Int i;
   for (i = 0; i < 10; i++) {
      randV128(&src);
      randV128(&dst);
      DO_mandr_r("pminud", src, dst);
   }
}

void test_PMINUW ( void )
{
   V128 src, dst;
   Int i;
   for (i = 0; i < 10; i++) {
      randV128(&src);
      randV128(&dst);
      DO_mandr_r("pminuw", src, dst);
   }
}

void test_PMOVSXBW ( void )
{
   V128 src, dst;
   Int i;
   for (i = 0; i < 10; i++) {
      randV128(&src);
      randV128(&dst);
      DO_mandr_r("pmovsxbw", src, dst);
   }
}

void test_PMOVSXBD ( void )
{
   V128 src, dst;
   Int i;
   for (i = 0; i < 10; i++) {
      randV128(&src);
      randV128(&dst);
      DO_mandr_r("pmovsxbd", src, dst);
   }
}

void test_PMOVSXBQ ( void )
{
   V128 src, dst;
   Int i;
   for (i = 0; i < 10; i++) {
      randV128(&src);
      randV128(&dst);
      DO_mandr_r("pmovsxbq", src, dst);
   }
}

void test_PMOVSXWD ( void )
{
   V128 src, dst;
   Int i;
   for (i = 0; i < 10; i++) {
      randV128(&src);
      randV128(&dst);
      DO_mandr_r("pmovsxwd", src, dst);
   }
}

void test_PMOVSXWQ ( void )
{
   V128 src, dst;
   Int i;
   for (i = 0; i < 10; i++) {
      randV128(&src);
      randV128(&dst);
      DO_mandr_r("pmovsxwq", src, dst);
   }
}

void test_PMOVSXDQ ( void )
{
   V128 src, dst;
   Int i;
   for (i = 0; i < 10; i++) {
      randV128(&src);
      randV128(&dst);
      DO_mandr_r("pmovsxdq", src, dst);
   }
}

void test_PMOVZXBW ( void )
{
   V128 src, dst;
   Int i;
   for (i = 0; i < 10; i++) {
      randV128(&src);
      randV128(&dst);
      DO_mandr_r("pmovzxbw", src, dst);
   }
}

void test_PMOVZXBD ( void )
{
   V128 src, dst;
   Int i;
   for (i = 0; i < 10; i++) {
      randV128(&src);
      randV128(&dst);
      DO_mandr_r("pmovzxbd", src, dst);
   }
}

void test_PMOVZXBQ ( void )
{
   V128 src, dst;
   Int i;
   for (i = 0; i < 10; i++) {
      randV128(&src);
      randV128(&dst);
      DO_mandr_r("pmovzxbq", src, dst);
   }
}

void test_PMOVZXWD ( void )
{
   V128 src, dst;
   Int i;
   for (i = 0; i < 10; i++) {
      randV128(&src);
      randV128(&dst);
      DO_mandr_r("pmovzxwd", src, dst);
   }
}

void test_PMOVZXWQ ( void )
{
   V128 src, dst;
   Int i;
   for (i = 0; i < 10; i++) {
      randV128(&src);
      randV128(&dst);
      DO_mandr_r("pmovzxwq", src, dst);
   }
}

void test_PMOVZXDQ ( void )
{
   V128 src, dst;
   Int i;
   for (i = 0; i < 10; i++) {
      randV128(&src);
      randV128(&dst);
      DO_mandr_r("pmovzxdq", src, dst);
   }
}

void test_PMULDQ ( void )
{
   V128 src, dst;
   Int i;
   for (i = 0; i < 10; i++) {
      randV128(&src);
      randV128(&dst);
      DO_mandr_r("pmuldq", src, dst);
   }
}


void test_PMULLD ( void )
{
   V128 src, dst;
   Int i;
   for (i = 0; i < 10; i++) {
      randV128(&src);
      randV128(&dst);
      DO_mandr_r("pmulld", src, dst);
   }
}


void test_POPCNTQ ( void )
{
   ULong block[4];
   Int i;
   ULong oszacp_mask = 0x8D5;
   for (i = 0; i < 10; i++) {
      block[0] = i == 0 ? 0 : randULong();
      block[1] = randULong();
      block[2] = randULong();
      block[3] = randULong();
      __asm__ __volatile__(
         "movq %0,       %%rax"  "\n\t"
         "movq 0(%%rax), %%rdi"  "\n\t"
         "movq 8(%%rax), %%r11"  "\n\t"
#ifndef VGP_amd64_darwin
         "popcntq %%rdi, %%r11"  "\n\t"
#else
         "popcnt  %%rdi, %%r11"  "\n\t"
#endif
         "movq %%r11, 16(%%rax)"  "\n\t"
         "pushfq"                 "\n\t"
         "popq %%r12"             "\n\t"
         "movq %%r12, 24(%%rax)"  "\n"
         : /*out*/
         : /*in*/"r"(&block[0])
         : /*trash*/ "cc", "memory", "rdi", "r11", "r12"
      );
      printf("r popcntq  %016llx %016llx  %016llx %016llx\n",
             block[0], block[1], block[2], block[3] & oszacp_mask);

      block[0] = i == 0 ? 0 : randULong();
      block[1] = randULong();
      block[2] = randULong();
      block[3] = randULong();
      __asm__ __volatile__(
         "movq %0,       %%rax"  "\n\t"
         "movq 8(%%rax), %%r11"  "\n\t"
#ifndef VGP_amd64_darwin
         "popcntq 0(%%rax), %%r11"  "\n\t"
#else
         "popcnt  0(%%rax), %%r11"  "\n\t"
#endif
         "movq %%r11, 16(%%rax)"  "\n\t"
         "pushfq"                 "\n\t"
         "popq %%r12"             "\n\t"
         "movq %%r12, 24(%%rax)"  "\n"
         : /*out*/
         : /*in*/"r"(&block[0])
         : /*trash*/ "cc", "memory", "r11", "r12"
      );
      printf("m popcntq  %016llx %016llx  %016llx %016llx\n",
             block[0], block[1], block[2], block[3] & oszacp_mask);
   }
}


void test_POPCNTL ( void )
{
   ULong block[4];
   Int i;
   ULong oszacp_mask = 0x8D5;
   for (i = 0; i < 10; i++) {
      block[0] = i == 0 ? 0 : randULong();
      block[1] = randULong();
      block[2] = randULong();
      block[3] = randULong();
      __asm__ __volatile__(
         "movq %0,       %%rax"  "\n\t"
         "movq 0(%%rax), %%rdi"  "\n\t"
         "movq 8(%%rax), %%r11"  "\n\t"
#ifndef VGP_amd64_darwin
         "popcntl %%edi, %%r11d"  "\n\t"
#else
         "popcnt  %%edi, %%r11d"  "\n\t"
#endif
         "movq %%r11, 16(%%rax)"  "\n\t"
         "pushfq"                 "\n\t"
         "popq %%r12"             "\n\t"
         "movq %%r12, 24(%%rax)"  "\n"
         : /*out*/
         : /*in*/"r"(&block[0])
         : /*trash*/ "cc", "memory", "rdi", "r11", "r12"
      );
      printf("r popcntl  %016llx %016llx  %016llx %016llx\n",
             block[0], block[1], block[2], block[3] & oszacp_mask);

      block[0] = i == 0 ? 0 : randULong();
      block[1] = randULong();
      block[2] = randULong();
      block[3] = randULong();
      __asm__ __volatile__(
         "movq %0,       %%rax"  "\n\t"
         "movq 8(%%rax), %%r11"  "\n\t"
#ifndef VGP_amd64_darwin
         "popcntl 0(%%rax), %%r11d"  "\n\t"
#else
         "popcnt  0(%%rax), %%r11d"  "\n\t"
#endif
         "movq %%r11, 16(%%rax)"  "\n\t"
         "pushfq"                 "\n\t"
         "popq %%r12"             "\n\t"
         "movq %%r12, 24(%%rax)"  "\n"
         : /*out*/
         : /*in*/"r"(&block[0])
         : /*trash*/ "cc", "memory", "r11", "r12"
      );
      printf("m popcntl  %016llx %016llx  %016llx %016llx\n",
             block[0], block[1], block[2], block[3] & oszacp_mask);
   }
}


void test_POPCNTW ( void )
{
   ULong block[4];
   Int i;
   ULong oszacp_mask = 0x8D5;
   for (i = 0; i < 10; i++) {
      block[0] = i == 0 ? 0 : randULong();
      block[1] = randULong();
      block[2] = randULong();
      block[3] = randULong();
      __asm__ __volatile__(
         "movq %0,       %%rax"  "\n\t"
         "movq 0(%%rax), %%rdi"  "\n\t"
         "movq 8(%%rax), %%r11"  "\n\t"
#ifndef VGP_amd64_darwin
         "popcntw %%di,  %%r11w"  "\n\t"
#else
         "popcnt  %%di,  %%r11w"  "\n\t"
#endif
         "movq %%r11, 16(%%rax)"  "\n\t"
         "pushfq"                 "\n\t"
         "popq %%r12"             "\n\t"
         "movq %%r12, 24(%%rax)"  "\n"
         : /*out*/
         : /*in*/"r"(&block[0])
         : /*trash*/ "cc", "memory", "rdi", "r11", "r12"
      );
      printf("r popcntw  %016llx %016llx  %016llx %016llx\n",
             block[0], block[1], block[2], block[3] & oszacp_mask);

      block[0] = i == 0 ? 0 : randULong();
      block[1] = randULong();
      block[2] = randULong();
      block[3] = randULong();
      __asm__ __volatile__(
         "movq %0,       %%rax"  "\n\t"
         "movq 8(%%rax), %%r11"  "\n\t"
#ifndef VGP_amd64_darwin
         "popcntw 0(%%rax), %%r11w"  "\n\t"
#else
         "popcnt  0(%%rax), %%r11w"  "\n\t"
#endif
         "movq %%r11, 16(%%rax)"  "\n\t"
         "pushfq"                 "\n\t"
         "popq %%r12"             "\n\t"
         "movq %%r12, 24(%%rax)"  "\n"
         : /*out*/
         : /*in*/"r"(&block[0])
         : /*trash*/ "cc", "memory", "r11", "r12"
      );
      printf("m popcntw  %016llx %016llx  %016llx %016llx\n",
             block[0], block[1], block[2], block[3] & oszacp_mask);
   }
}


void test_PCMPGTQ ( void )
{
   V128 spec[7];
   do64HLtoV128( &spec[0], 0x0000000000000000ULL, 0xffffffffffffffffULL );
   do64HLtoV128( &spec[1], 0x0000000000000001ULL, 0xfffffffffffffffeULL );
   do64HLtoV128( &spec[2], 0x7fffffffffffffffULL, 0x8000000000000001ULL );
   do64HLtoV128( &spec[3], 0x8000000000000000ULL, 0x8000000000000000ULL );
   do64HLtoV128( &spec[4], 0x8000000000000001ULL, 0x7fffffffffffffffULL );
   do64HLtoV128( &spec[5], 0xfffffffffffffffeULL, 0x0000000000000001ULL );
   do64HLtoV128( &spec[6], 0xffffffffffffffffULL, 0x0000000000000000ULL );

   V128 src, dst;
   Int i, j;
   for (i = 0; i < 10; i++) {
      randV128(&src);
      randV128(&dst);
      DO_mandr_r("pcmpgtq", src, dst);
   }
   for (i = 0; i < 7; i++) {
      for (j = 0; j < 7; j++) {
         memcpy(&src, &spec[i], 16);
         memcpy(&dst, &spec[j], 16);
         DO_mandr_r("pcmpgtq", src, dst);
      }
   }
}

/* ------------ ROUNDSD ------------ */

void do_ROUNDSD_000 ( Bool mem, V128* src, /*OUT*/V128* dst )
{
   if (mem) {
      __asm__ __volatile__(
         "movupd  (%1), %%xmm11"       "\n\t"
         "roundsd $0, (%0), %%xmm11"   "\n\t"
         "movupd  %%xmm11, (%1)"       "\n"
         : /*OUT*/
         : /*IN*/ "r"(src), "r"(dst)
         : /*TRASH*/ "xmm11"
      );
   } else {
      __asm__ __volatile__(
         "movupd  (%1), %%xmm11"         "\n\t"
         "movupd  (%0), %%xmm2"          "\n\t"
         "roundsd $0, %%xmm2, %%xmm11"   "\n\t"
         "movupd  %%xmm11, (%1)"         "\n"
         : /*OUT*/
         : /*IN*/ "r"(src), "r"(dst)
         : /*TRASH*/ "xmm11","xmm2"
      );
   }
}

void do_ROUNDSD_001 ( Bool mem, V128* src, /*OUT*/V128* dst )
{
   if (mem) {
      __asm__ __volatile__(
         "movupd  (%1), %%xmm11"       "\n\t"
         "roundsd $1, (%0), %%xmm11"   "\n\t"
         "movupd  %%xmm11, (%1)"       "\n"
         : /*OUT*/
         : /*IN*/ "r"(src), "r"(dst)
         : /*TRASH*/ "xmm11"
      );
   } else {
      __asm__ __volatile__(
         "movupd  (%1), %%xmm11"         "\n\t"
         "movupd  (%0), %%xmm2"          "\n\t"
         "roundsd $1, %%xmm2, %%xmm11"   "\n\t"
         "movupd  %%xmm11, (%1)"         "\n"
         : /*OUT*/
         : /*IN*/ "r"(src), "r"(dst)
         : /*TRASH*/ "xmm11","xmm2"
      );
   }
}

void do_ROUNDSD_010 ( Bool mem, V128* src, /*OUT*/V128* dst )
{
   if (mem) {
      __asm__ __volatile__(
         "movupd  (%1), %%xmm11"       "\n\t"
         "roundsd $2, (%0), %%xmm11"   "\n\t"
         "movupd  %%xmm11, (%1)"       "\n"
         : /*OUT*/
         : /*IN*/ "r"(src), "r"(dst)
         : /*TRASH*/ "xmm11"
      );
   } else {
      __asm__ __volatile__(
         "movupd  (%1), %%xmm11"         "\n\t"
         "movupd  (%0), %%xmm2"          "\n\t"
         "roundsd $2, %%xmm2, %%xmm11"   "\n\t"
         "movupd  %%xmm11, (%1)"         "\n"
         : /*OUT*/
         : /*IN*/ "r"(src), "r"(dst)
         : /*TRASH*/ "xmm11","xmm2"
      );
   }
}

void do_ROUNDSD_011 ( Bool mem, V128* src, /*OUT*/V128* dst )
{
   if (mem) {
      __asm__ __volatile__(
         "movupd  (%1), %%xmm11"       "\n\t"
         "roundsd $3, (%0), %%xmm11"   "\n\t"
         "movupd  %%xmm11, (%1)"       "\n"
         : /*OUT*/
         : /*IN*/ "r"(src), "r"(dst)
         : /*TRASH*/ "xmm11"
      );
   } else {
      __asm__ __volatile__(
         "movupd  (%1), %%xmm11"         "\n\t"
         "movupd  (%0), %%xmm2"          "\n\t"
         "roundsd $3, %%xmm2, %%xmm11"   "\n\t"
         "movupd  %%xmm11, (%1)"         "\n"
         : /*OUT*/
         : /*IN*/ "r"(src), "r"(dst)
         : /*TRASH*/ "xmm11","xmm2"
      );
   }
}

void do_ROUNDSD_1XX ( Bool mem, V128* src, /*OUT*/V128* dst )
{
   if (mem) {
      __asm__ __volatile__(
         "movupd  (%1), %%xmm11"       "\n\t"
         "roundsd $4, (%0), %%xmm11"   "\n\t"
         "movupd  %%xmm11, (%1)"       "\n"
         : /*OUT*/
         : /*IN*/ "r"(src), "r"(dst)
         : /*TRASH*/ "xmm11"
      );
   } else {
      __asm__ __volatile__(
         "movupd  (%1), %%xmm11"         "\n\t"
         "movupd  (%0), %%xmm2"          "\n\t"
         "roundsd $4, %%xmm2, %%xmm11"   "\n\t"
         "movupd  %%xmm11, (%1)"         "\n"
         : /*OUT*/
         : /*IN*/ "r"(src), "r"(dst)
         : /*TRASH*/ "xmm11","xmm2"
      );
   }
}

void test_ROUNDSD_w_immediate_rounding ( void )
{
   double vals[22];
   Int i = 0;
   vals[i++] = 0.0;
   vals[i++] = -0.0;
   vals[i++] = mkPosInf();
   vals[i++] = mkNegInf();
   vals[i++] = mkPosNan();
   vals[i++] = mkNegNan();
   vals[i++] = -1.3;
   vals[i++] = -1.1;
   vals[i++] = -0.9;
   vals[i++] = -0.7;
   vals[i++] = -0.50001;
   vals[i++] = -0.49999;
   vals[i++] = -0.3;
   vals[i++] = -0.1;
   vals[i++] = 0.1;
   vals[i++] = 0.3;
   vals[i++] = 0.49999;
   vals[i++] = 0.50001;
   vals[i++] = 0.7;
   vals[i++] = 0.9;
   vals[i++] = 1.1;
   vals[i++] = 1.3;
   assert(i == 22);

   for (i = 0; i < sizeof(vals)/sizeof(vals[0]); i++) {
      V128 src, dst;

      randV128(&src);
      randV128(&dst);
      memcpy(&src[0], &vals[i], 8);
      do_ROUNDSD_000(False/*reg*/, &src, &dst);
      printf("r roundsd_000  ");
      showV128(&src);
      printf(" ");
      showV128(&dst);
      printf("  %10f %10f", vals[i], *(double*)(&dst[0]));
      printf("\n");

      randV128(&src);
      randV128(&dst);
      memcpy(&src[0], &vals[i], 8);
      do_ROUNDSD_000(True/*mem*/, &src, &dst);
      printf("m roundsd_000  ");
      showV128(&src);
      printf(" ");
      showV128(&dst);
      printf("  %10f %10f", vals[i], *(double*)(&dst[0]));
      printf("\n");


      randV128(&src);
      randV128(&dst);
      memcpy(&src[0], &vals[i], 8);
      do_ROUNDSD_001(False/*reg*/, &src, &dst);
      printf("r roundsd_001  ");
      showV128(&src);
      printf(" ");
      showV128(&dst);
      printf("  %10f %10f", vals[i], *(double*)(&dst[0]));
      printf("\n");

      randV128(&src);
      randV128(&dst);
      memcpy(&src[0], &vals[i], 8);
      do_ROUNDSD_001(True/*mem*/, &src, &dst);
      printf("m roundsd_001  ");
      showV128(&src);
      printf(" ");
      showV128(&dst);
      printf("  %10f %10f", vals[i], *(double*)(&dst[0]));
      printf("\n");


      randV128(&src);
      randV128(&dst);
      memcpy(&src[0], &vals[i], 8);
      do_ROUNDSD_010(False/*reg*/, &src, &dst);
      printf("r roundsd_010  ");
      showV128(&src);
      printf(" ");
      showV128(&dst);
      printf("  %10f %10f", vals[i], *(double*)(&dst[0]));
      printf("\n");

      randV128(&src);
      randV128(&dst);
      memcpy(&src[0], &vals[i], 8);
      do_ROUNDSD_010(True/*mem*/, &src, &dst);
      printf("m roundsd_010  ");
      showV128(&src);
      printf(" ");
      showV128(&dst);
      printf("  %10f %10f", vals[i], *(double*)(&dst[0]));
      printf("\n");


      randV128(&src);
      randV128(&dst);
      memcpy(&src[0], &vals[i], 8);
      do_ROUNDSD_011(False/*reg*/, &src, &dst);
      printf("r roundsd_011  ");
      showV128(&src);
      printf(" ");
      showV128(&dst);
      printf("  %10f %10f", vals[i], *(double*)(&dst[0]));
      printf("\n");

      randV128(&src);
      randV128(&dst);
      memcpy(&src[0], &vals[i], 8);
      do_ROUNDSD_011(True/*mem*/, &src, &dst);
      printf("m roundsd_011  ");
      showV128(&src);
      printf(" ");
      showV128(&dst);
      printf("  %10f %10f", vals[i], *(double*)(&dst[0]));
      printf("\n");
   }
}

void test_ROUNDSD_w_mxcsr_rounding ( void )
{
   UInt rm;
   double vals[22];
   Int i = 0;
   vals[i++] = 0.0;
   vals[i++] = -0.0;
   vals[i++] = mkPosInf();
   vals[i++] = mkNegInf();
   vals[i++] = mkPosNan();
   vals[i++] = mkNegNan();
   vals[i++] = -1.3;
   vals[i++] = -1.1;
   vals[i++] = -0.9;
   vals[i++] = -0.7;
   vals[i++] = -0.50001;
   vals[i++] = -0.49999;
   vals[i++] = -0.3;
   vals[i++] = -0.1;
   vals[i++] = 0.1;
   vals[i++] = 0.3;
   vals[i++] = 0.49999;
   vals[i++] = 0.50001;
   vals[i++] = 0.7;
   vals[i++] = 0.9;
   vals[i++] = 1.1;
   vals[i++] = 1.3;
   assert(i == 22);

   rm = get_sse_roundingmode();
   assert(rm == 0); // 0 == RN == default

   for (i = 0; i < sizeof(vals)/sizeof(vals[0]); i++) {
      V128 src, dst;

      for (rm = 0; rm <= 3; rm++) {
         set_sse_roundingmode(rm);

         randV128(&src);
         randV128(&dst);
         memcpy(&src[0], &vals[i], 8);
         do_ROUNDSD_1XX(False/*reg*/, &src, &dst);
         printf("r (rm=%u) roundsd_1XX  ", rm);
         showV128(&src);
         printf(" ");
         showV128(&dst);
         printf("  %10f %10f", vals[i], *(double*)(&dst[0]));
         printf("\n");

         randV128(&src);
         randV128(&dst);
         memcpy(&src[0], &vals[i], 8);
         do_ROUNDSD_1XX(True/*mem*/, &src, &dst);
         printf("m (rm=%u) roundsd_1XX  ", rm);
         showV128(&src);
         printf(" ");
         showV128(&dst);
         printf("  %10f %10f", vals[i], *(double*)(&dst[0]));
         printf("\n");
      }
   }

   rm = get_sse_roundingmode();
   assert(rm == 3);
   set_sse_roundingmode(0);
   rm = get_sse_roundingmode();
   assert(rm == 0); // 0 == RN == default
}


/* ------------ ROUNDSS ------------ */

void do_ROUNDSS_000 ( Bool mem, V128* src, /*OUT*/V128* dst )
{
   if (mem) {
      __asm__ __volatile__(
         "movupd  (%1), %%xmm11"       "\n\t"
         "roundss $0, (%0), %%xmm11"   "\n\t"
         "movupd  %%xmm11, (%1)"       "\n"
         : /*OUT*/
         : /*IN*/ "r"(src), "r"(dst)
         : /*TRASH*/ "xmm11"
      );
   } else {
      __asm__ __volatile__(
         "movupd  (%1), %%xmm11"         "\n\t"
         "movupd  (%0), %%xmm2"          "\n\t"
         "roundss $0, %%xmm2, %%xmm11"   "\n\t"
         "movupd  %%xmm11, (%1)"         "\n"
         : /*OUT*/
         : /*IN*/ "r"(src), "r"(dst)
         : /*TRASH*/ "xmm11","xmm2"
      );
   }
}

void do_ROUNDSS_001 ( Bool mem, V128* src, /*OUT*/V128* dst )
{
   if (mem) {
      __asm__ __volatile__(
         "movupd  (%1), %%xmm11"       "\n\t"
         "roundss $1, (%0), %%xmm11"   "\n\t"
         "movupd  %%xmm11, (%1)"       "\n"
         : /*OUT*/
         : /*IN*/ "r"(src), "r"(dst)
         : /*TRASH*/ "xmm11"
      );
   } else {
      __asm__ __volatile__(
         "movupd  (%1), %%xmm11"         "\n\t"
         "movupd  (%0), %%xmm2"          "\n\t"
         "roundss $1, %%xmm2, %%xmm11"   "\n\t"
         "movupd  %%xmm11, (%1)"         "\n"
         : /*OUT*/
         : /*IN*/ "r"(src), "r"(dst)
         : /*TRASH*/ "xmm11","xmm2"
      );
   }
}

void do_ROUNDSS_010 ( Bool mem, V128* src, /*OUT*/V128* dst )
{
   if (mem) {
      __asm__ __volatile__(
         "movupd  (%1), %%xmm11"       "\n\t"
         "roundss $2, (%0), %%xmm11"   "\n\t"
         "movupd  %%xmm11, (%1)"       "\n"
         : /*OUT*/
         : /*IN*/ "r"(src), "r"(dst)
         : /*TRASH*/ "xmm11"
      );
   } else {
      __asm__ __volatile__(
         "movupd  (%1), %%xmm11"         "\n\t"
         "movupd  (%0), %%xmm2"          "\n\t"
         "roundss $2, %%xmm2, %%xmm11"   "\n\t"
         "movupd  %%xmm11, (%1)"         "\n"
         : /*OUT*/
         : /*IN*/ "r"(src), "r"(dst)
         : /*TRASH*/ "xmm11","xmm2"
      );
   }
}

void do_ROUNDSS_011 ( Bool mem, V128* src, /*OUT*/V128* dst )
{
   if (mem) {
      __asm__ __volatile__(
         "movupd  (%1), %%xmm11"       "\n\t"
         "roundss $3, (%0), %%xmm11"   "\n\t"
         "movupd  %%xmm11, (%1)"       "\n"
         : /*OUT*/
         : /*IN*/ "r"(src), "r"(dst)
         : /*TRASH*/ "xmm11"
      );
   } else {
      __asm__ __volatile__(
         "movupd  (%1), %%xmm11"         "\n\t"
         "movupd  (%0), %%xmm2"          "\n\t"
         "roundss $3, %%xmm2, %%xmm11"   "\n\t"
         "movupd  %%xmm11, (%1)"         "\n"
         : /*OUT*/
         : /*IN*/ "r"(src), "r"(dst)
         : /*TRASH*/ "xmm11","xmm2"
      );
   }
}

void do_ROUNDSS_1XX ( Bool mem, V128* src, /*OUT*/V128* dst )
{
   if (mem) {
      __asm__ __volatile__(
         "movupd  (%1), %%xmm11"       "\n\t"
         "roundss $4, (%0), %%xmm11"   "\n\t"
         "movupd  %%xmm11, (%1)"       "\n"
         : /*OUT*/
         : /*IN*/ "r"(src), "r"(dst)
         : /*TRASH*/ "xmm11"
      );
   } else {
      __asm__ __volatile__(
         "movupd  (%1), %%xmm11"         "\n\t"
         "movupd  (%0), %%xmm2"          "\n\t"
         "roundss $4, %%xmm2, %%xmm11"   "\n\t"
         "movupd  %%xmm11, (%1)"         "\n"
         : /*OUT*/
         : /*IN*/ "r"(src), "r"(dst)
         : /*TRASH*/ "xmm11","xmm2"
      );
   }
}

void test_ROUNDSS_w_immediate_rounding ( void )
{
   float vals[22];
   Int i = 0;
   vals[i++] = 0.0;
   vals[i++] = -0.0;
   vals[i++] = mkPosInf();
   vals[i++] = mkNegInf();
   vals[i++] = mkPosNan();
   vals[i++] = mkNegNan();
   vals[i++] = -1.3;
   vals[i++] = -1.1;
   vals[i++] = -0.9;
   vals[i++] = -0.7;
   vals[i++] = -0.50001;
   vals[i++] = -0.49999;
   vals[i++] = -0.3;
   vals[i++] = -0.1;
   vals[i++] = 0.1;
   vals[i++] = 0.3;
   vals[i++] = 0.49999;
   vals[i++] = 0.50001;
   vals[i++] = 0.7;
   vals[i++] = 0.9;
   vals[i++] = 1.1;
   vals[i++] = 1.3;
   assert(i == 22);

   for (i = 0; i < sizeof(vals)/sizeof(vals[0]); i++) {
      V128 src, dst;

      randV128(&src);
      randV128(&dst);
      memcpy(&src[0], &vals[i], 4);
      do_ROUNDSS_000(False/*reg*/, &src, &dst);
      printf("r roundss_000  ");
      showV128(&src);
      printf(" ");
      showV128(&dst);
      printf("  %10f %10f", (double)vals[i], (double)*(float*)(&dst[0]));
      printf("\n");

      randV128(&src);
      randV128(&dst);
      memcpy(&src[0], &vals[i], 4);
      do_ROUNDSS_000(True/*mem*/, &src, &dst);
      printf("m roundss_000  ");
      showV128(&src);
      printf(" ");
      showV128(&dst);
      printf("  %10f %10f", (double)vals[i], (double)*(float*)(&dst[0]));
      printf("\n");


      randV128(&src);
      randV128(&dst);
      memcpy(&src[0], &vals[i], 4);
      do_ROUNDSS_001(False/*reg*/, &src, &dst);
      printf("r roundss_001  ");
      showV128(&src);
      printf(" ");
      showV128(&dst);
      printf("  %10f %10f", (double)vals[i], (double)*(float*)(&dst[0]));
      printf("\n");

      randV128(&src);
      randV128(&dst);
      memcpy(&src[0], &vals[i], 4);
      do_ROUNDSS_001(True/*mem*/, &src, &dst);
      printf("m roundss_001  ");
      showV128(&src);
      printf(" ");
      showV128(&dst);
      printf("  %10f %10f", (double)vals[i], (double)*(float*)(&dst[0]));
      printf("\n");


      randV128(&src);
      randV128(&dst);
      memcpy(&src[0], &vals[i], 4);
      do_ROUNDSS_010(False/*reg*/, &src, &dst);
      printf("r roundss_010  ");
      showV128(&src);
      printf(" ");
      showV128(&dst);
      printf("  %10f %10f", (double)vals[i], (double)*(float*)(&dst[0]));
      printf("\n");

      randV128(&src);
      randV128(&dst);
      memcpy(&src[0], &vals[i], 4);
      do_ROUNDSS_010(True/*mem*/, &src, &dst);
      printf("m roundss_010  ");
      showV128(&src);
      printf(" ");
      showV128(&dst);
      printf("  %10f %10f", (double)vals[i], (double)*(float*)(&dst[0]));
      printf("\n");


      randV128(&src);
      randV128(&dst);
      memcpy(&src[0], &vals[i], 4);
      do_ROUNDSS_011(False/*reg*/, &src, &dst);
      printf("r roundss_011  ");
      showV128(&src);
      printf(" ");
      showV128(&dst);
      printf("  %10f %10f", (double)vals[i], (double)*(float*)(&dst[0]));
      printf("\n");

      randV128(&src);
      randV128(&dst);
      memcpy(&src[0], &vals[i], 4);
      do_ROUNDSS_011(True/*mem*/, &src, &dst);
      printf("m roundss_011  ");
      showV128(&src);
      printf(" ");
      showV128(&dst);
      printf("  %10f %10f", (double)vals[i], (double)*(float*)(&dst[0]));
      printf("\n");
   }
}

void test_ROUNDSS_w_mxcsr_rounding ( void )
{
   UInt rm;
   float vals[22];
   Int i = 0;
   vals[i++] = 0.0;
   vals[i++] = -0.0;
   vals[i++] = mkPosInf();
   vals[i++] = mkNegInf();
   vals[i++] = mkPosNan();
   vals[i++] = mkNegNan();
   vals[i++] = -1.3;
   vals[i++] = -1.1;
   vals[i++] = -0.9;
   vals[i++] = -0.7;
   vals[i++] = -0.50001;
   vals[i++] = -0.49999;
   vals[i++] = -0.3;
   vals[i++] = -0.1;
   vals[i++] = 0.1;
   vals[i++] = 0.3;
   vals[i++] = 0.49999;
   vals[i++] = 0.50001;
   vals[i++] = 0.7;
   vals[i++] = 0.9;
   vals[i++] = 1.1;
   vals[i++] = 1.3;
   assert(i == 22);

   rm = get_sse_roundingmode();
   assert(rm == 0); // 0 == RN == default

   for (i = 0; i < sizeof(vals)/sizeof(vals[0]); i++) {
      V128 src, dst;

      for (rm = 0; rm <= 3; rm++) {
         set_sse_roundingmode(rm);

         randV128(&src);
         randV128(&dst);
         memcpy(&src[0], &vals[i], 4);
         do_ROUNDSS_1XX(False/*reg*/, &src, &dst);
         printf("r (rm=%u) roundss_1XX  ", rm);
         showV128(&src);
         printf(" ");
         showV128(&dst);
         printf("  %10f %10f", (double)vals[i], (double)*(float*)(&dst[0]));
         printf("\n");

         randV128(&src);
         randV128(&dst);
         memcpy(&src[0], &vals[i], 4);
         do_ROUNDSS_1XX(True/*mem*/, &src, &dst);
         printf("m (rm=%u) roundss_1XX  ", rm);
         showV128(&src);
         printf(" ");
         showV128(&dst);
         printf("  %10f %10f", (double)vals[i], (double)*(float*)(&dst[0]));
         printf("\n");
      }
   }

   rm = get_sse_roundingmode();
   assert(rm == 3);
   set_sse_roundingmode(0);
   rm = get_sse_roundingmode();
   assert(rm == 0); // 0 == RN == default
}

/* ------------ ROUNDPD ------------ */

void do_ROUNDPD_000 ( Bool mem, V128* src, /*OUT*/V128* dst )
{
   if (mem) {
      __asm__ __volatile__(
         "movupd  (%1), %%xmm11"       "\n\t"
         "roundpd $0, (%0), %%xmm11"   "\n\t"
         "movupd  %%xmm11, (%1)"       "\n"
         : /*OUT*/
         : /*IN*/ "r"(src), "r"(dst)
         : /*TRASH*/ "xmm11"
      );
   } else {
      __asm__ __volatile__(
         "movupd  (%1), %%xmm11"         "\n\t"
         "movupd  (%0), %%xmm2"          "\n\t"
         "roundpd $0, %%xmm2, %%xmm11"   "\n\t"
         "movupd  %%xmm11, (%1)"         "\n"
         : /*OUT*/
         : /*IN*/ "r"(src), "r"(dst)
         : /*TRASH*/ "xmm11","xmm2"
      );
   }
}

void do_ROUNDPD_001 ( Bool mem, V128* src, /*OUT*/V128* dst )
{
   if (mem) {
      __asm__ __volatile__(
         "movupd  (%1), %%xmm11"       "\n\t"
         "roundpd $1, (%0), %%xmm11"   "\n\t"
         "movupd  %%xmm11, (%1)"       "\n"
         : /*OUT*/
         : /*IN*/ "r"(src), "r"(dst)
         : /*TRASH*/ "xmm11"
      );
   } else {
      __asm__ __volatile__(
         "movupd  (%1), %%xmm11"         "\n\t"
         "movupd  (%0), %%xmm2"          "\n\t"
         "roundpd $1, %%xmm2, %%xmm11"   "\n\t"
         "movupd  %%xmm11, (%1)"         "\n"
         : /*OUT*/
         : /*IN*/ "r"(src), "r"(dst)
         : /*TRASH*/ "xmm11","xmm2"
      );
   }
}

void do_ROUNDPD_010 ( Bool mem, V128* src, /*OUT*/V128* dst )
{
   if (mem) {
      __asm__ __volatile__(
         "movupd  (%1), %%xmm11"       "\n\t"
         "roundpd $2, (%0), %%xmm11"   "\n\t"
         "movupd  %%xmm11, (%1)"       "\n"
         : /*OUT*/
         : /*IN*/ "r"(src), "r"(dst)
         : /*TRASH*/ "xmm11"
      );
   } else {
      __asm__ __volatile__(
         "movupd  (%1), %%xmm11"         "\n\t"
         "movupd  (%0), %%xmm2"          "\n\t"
         "roundpd $2, %%xmm2, %%xmm11"   "\n\t"
         "movupd  %%xmm11, (%1)"         "\n"
         : /*OUT*/
         : /*IN*/ "r"(src), "r"(dst)
         : /*TRASH*/ "xmm11","xmm2"
      );
   }
}

void do_ROUNDPD_011 ( Bool mem, V128* src, /*OUT*/V128* dst )
{
   if (mem) {
      __asm__ __volatile__(
         "movupd  (%1), %%xmm11"       "\n\t"
         "roundpd $3, (%0), %%xmm11"   "\n\t"
         "movupd  %%xmm11, (%1)"       "\n"
         : /*OUT*/
         : /*IN*/ "r"(src), "r"(dst)
         : /*TRASH*/ "xmm11"
      );
   } else {
      __asm__ __volatile__(
         "movupd  (%1), %%xmm11"         "\n\t"
         "movupd  (%0), %%xmm2"          "\n\t"
         "roundpd $3, %%xmm2, %%xmm11"   "\n\t"
         "movupd  %%xmm11, (%1)"         "\n"
         : /*OUT*/
         : /*IN*/ "r"(src), "r"(dst)
         : /*TRASH*/ "xmm11","xmm2"
      );
   }
}

void do_ROUNDPD_1XX ( Bool mem, V128* src, /*OUT*/V128* dst )
{
   if (mem) {
      __asm__ __volatile__(
         "movupd  (%1), %%xmm11"       "\n\t"
         "roundpd $4, (%0), %%xmm11"   "\n\t"
         "movupd  %%xmm11, (%1)"       "\n"
         : /*OUT*/
         : /*IN*/ "r"(src), "r"(dst)
         : /*TRASH*/ "xmm11"
      );
   } else {
      __asm__ __volatile__(
         "movupd  (%1), %%xmm11"         "\n\t"
         "movupd  (%0), %%xmm2"          "\n\t"
         "roundpd $4, %%xmm2, %%xmm11"   "\n\t"
         "movupd  %%xmm11, (%1)"         "\n"
         : /*OUT*/
         : /*IN*/ "r"(src), "r"(dst)
         : /*TRASH*/ "xmm11","xmm2"
      );
   }
}

void test_ROUNDPD_w_immediate_rounding ( void )
{
   double vals[22];
   Int i = 0;
   vals[i++] = 0.0;
   vals[i++] = -0.0;
   vals[i++] = mkPosInf();
   vals[i++] = mkNegInf();
   vals[i++] = mkPosNan();
   vals[i++] = mkNegNan();
   vals[i++] = -1.3;
   vals[i++] = -1.1;
   vals[i++] = -0.9;
   vals[i++] = -0.7;
   vals[i++] = -0.50001;
   vals[i++] = -0.49999;
   vals[i++] = -0.3;
   vals[i++] = -0.1;
   vals[i++] = 0.1;
   vals[i++] = 0.3;
   vals[i++] = 0.49999;
   vals[i++] = 0.50001;
   vals[i++] = 0.7;
   vals[i++] = 0.9;
   vals[i++] = 1.1;
   vals[i++] = 1.3;
   assert(i == 22);

   for (i = 0; i < sizeof(vals)/sizeof(vals[0]); i++) {
      V128 src, dst;

      randV128(&src);
      randV128(&dst);
      memcpy(&src[0], &vals[i], 8);
      memcpy(&src[8], &vals[(i+11)%22], 8);
      do_ROUNDPD_000(False/*reg*/, &src, &dst);
      printf("r roundpd_000  ");
      showV128(&src);
      printf(" ");
      showV128(&dst);
      printf("  %10f -> %10f", vals[i], *(double*)(&dst[0]));
      printf("   %10f -> %10f", vals[(i+11)%22], *(double*)(&dst[8]));
      printf("\n");

      randV128(&src);
      randV128(&dst);
      memcpy(&src[0], &vals[i], 8);
      memcpy(&src[8], &vals[(i+11)%22], 8);
      do_ROUNDPD_000(True/*mem*/, &src, &dst);
      printf("m roundpd_000  ");
      showV128(&src);
      printf(" ");
      showV128(&dst);
      printf("  %10f -> %10f", vals[i], *(double*)(&dst[0]));
      printf("   %10f -> %10f", vals[(i+11)%22], *(double*)(&dst[8]));
      printf("\n");


      randV128(&src);
      randV128(&dst);
      memcpy(&src[0], &vals[i], 8);
      memcpy(&src[8], &vals[(i+11)%22], 8);
      do_ROUNDPD_001(False/*reg*/, &src, &dst);
      printf("r roundpd_001  ");
      showV128(&src);
      printf(" ");
      showV128(&dst);
      printf("  %10f -> %10f", vals[i], *(double*)(&dst[0]));
      printf("   %10f -> %10f", vals[(i+11)%22], *(double*)(&dst[8]));
      printf("\n");

      randV128(&src);
      randV128(&dst);
      memcpy(&src[0], &vals[i], 8);
      memcpy(&src[8], &vals[(i+11)%22], 8);
      do_ROUNDPD_001(True/*mem*/, &src, &dst);
      printf("m roundpd_001  ");
      showV128(&src);
      printf(" ");
      showV128(&dst);
      printf("  %10f -> %10f", vals[i], *(double*)(&dst[0]));
      printf("   %10f -> %10f", vals[(i+11)%22], *(double*)(&dst[8]));
      printf("\n");


      randV128(&src);
      randV128(&dst);
      memcpy(&src[0], &vals[i], 8);
      memcpy(&src[8], &vals[(i+11)%22], 8);
      do_ROUNDPD_010(False/*reg*/, &src, &dst);
      printf("r roundpd_010  ");
      showV128(&src);
      printf(" ");
      showV128(&dst);
      printf("  %10f -> %10f", vals[i], *(double*)(&dst[0]));
      printf("   %10f -> %10f", vals[(i+11)%22], *(double*)(&dst[8]));
      printf("\n");

      randV128(&src);
      randV128(&dst);
      memcpy(&src[0], &vals[i], 8);
      memcpy(&src[8], &vals[(i+11)%22], 8);
      do_ROUNDPD_010(True/*mem*/, &src, &dst);
      printf("m roundpd_010  ");
      showV128(&src);
      printf(" ");
      showV128(&dst);
      printf("  %10f -> %10f", vals[i], *(double*)(&dst[0]));
      printf("   %10f -> %10f", vals[(i+11)%22], *(double*)(&dst[8]));
      printf("\n");


      randV128(&src);
      randV128(&dst);
      memcpy(&src[0], &vals[i], 8);
      memcpy(&src[8], &vals[(i+11)%22], 8);
      do_ROUNDPD_011(False/*reg*/, &src, &dst);
      printf("r roundpd_011  ");
      showV128(&src);
      printf(" ");
      showV128(&dst);
      printf("  %10f -> %10f", vals[i], *(double*)(&dst[0]));
      printf("   %10f -> %10f", vals[(i+11)%22], *(double*)(&dst[8]));
      printf("\n");

      randV128(&src);
      randV128(&dst);
      memcpy(&src[0], &vals[i], 8);
      memcpy(&src[8], &vals[(i+11)%22], 8);
      do_ROUNDPD_011(True/*mem*/, &src, &dst);
      printf("m roundpd_011  ");
      showV128(&src);
      printf(" ");
      showV128(&dst);
      printf("  %10f -> %10f", vals[i], *(double*)(&dst[0]));
      printf("   %10f -> %10f", vals[(i+11)%22], *(double*)(&dst[8]));
      printf("\n");
   }
}

void test_ROUNDPD_w_mxcsr_rounding ( void )
{
   UInt rm;
   double vals[22];
   Int i = 0;
   vals[i++] = 0.0;
   vals[i++] = -0.0;
   vals[i++] = mkPosInf();
   vals[i++] = mkNegInf();
   vals[i++] = mkPosNan();
   vals[i++] = mkNegNan();
   vals[i++] = -1.3;
   vals[i++] = -1.1;
   vals[i++] = -0.9;
   vals[i++] = -0.7;
   vals[i++] = -0.50001;
   vals[i++] = -0.49999;
   vals[i++] = -0.3;
   vals[i++] = -0.1;
   vals[i++] = 0.1;
   vals[i++] = 0.3;
   vals[i++] = 0.49999;
   vals[i++] = 0.50001;
   vals[i++] = 0.7;
   vals[i++] = 0.9;
   vals[i++] = 1.1;
   vals[i++] = 1.3;
   assert(i == 22);

   rm = get_sse_roundingmode();
   assert(rm == 0); // 0 == RN == default

   for (i = 0; i < sizeof(vals)/sizeof(vals[0]); i++) {
      V128 src, dst;

      for (rm = 0; rm <= 3; rm++) {
         set_sse_roundingmode(rm);

         randV128(&src);
         randV128(&dst);
         memcpy(&src[0], &vals[i], 8);
         memcpy(&src[8], &vals[(i+11)%22], 8);
         do_ROUNDPD_1XX(False/*reg*/, &src, &dst);
         printf("r (rm=%u) roundpd_1XX  ", rm);
         showV128(&src);
         printf(" ");
         showV128(&dst);
         printf("  %10f -> %10f", vals[i], *(double*)(&dst[0]));
         printf("   %10f -> %10f", vals[(i+11)%22], *(double*)(&dst[8]));
         printf("\n");

         randV128(&src);
         randV128(&dst);
         memcpy(&src[0], &vals[i], 8);
         memcpy(&src[8], &vals[(i+11)%22], 8);
         do_ROUNDPD_1XX(True/*mem*/, &src, &dst);
         printf("m (rm=%u) roundpd_1XX  ", rm);
         showV128(&src);
         printf(" ");
         showV128(&dst);
         printf("  %10f -> %10f", vals[i], *(double*)(&dst[0]));
         printf("   %10f -> %10f", vals[(i+11)%22], *(double*)(&dst[8]));
         printf("\n");
      }
   }

   rm = get_sse_roundingmode();
   assert(rm == 3);
   set_sse_roundingmode(0);
   rm = get_sse_roundingmode();
   assert(rm == 0); // 0 == RN == default
}

/* ------------ ROUNDPS ------------ */

void do_ROUNDPS_000 ( Bool mem, V128* src, /*OUT*/V128* dst )
{
   if (mem) {
      __asm__ __volatile__(
         "movupd  (%1), %%xmm11"       "\n\t"
         "roundps $0, (%0), %%xmm11"   "\n\t"
         "movupd  %%xmm11, (%1)"       "\n"
         : /*OUT*/
         : /*IN*/ "r"(src), "r"(dst)
         : /*TRASH*/ "xmm11"
      );
   } else {
      __asm__ __volatile__(
         "movupd  (%1), %%xmm11"         "\n\t"
         "movupd  (%0), %%xmm2"          "\n\t"
         "roundps $0, %%xmm2, %%xmm11"   "\n\t"
         "movupd  %%xmm11, (%1)"         "\n"
         : /*OUT*/
         : /*IN*/ "r"(src), "r"(dst)
         : /*TRASH*/ "xmm11","xmm2"
      );
   }
}

void do_ROUNDPS_001 ( Bool mem, V128* src, /*OUT*/V128* dst )
{
   if (mem) {
      __asm__ __volatile__(
         "movupd  (%1), %%xmm11"       "\n\t"
         "roundps $1, (%0), %%xmm11"   "\n\t"
         "movupd  %%xmm11, (%1)"       "\n"
         : /*OUT*/
         : /*IN*/ "r"(src), "r"(dst)
         : /*TRASH*/ "xmm11"
      );
   } else {
      __asm__ __volatile__(
         "movupd  (%1), %%xmm11"         "\n\t"
         "movupd  (%0), %%xmm2"          "\n\t"
         "roundps $1, %%xmm2, %%xmm11"   "\n\t"
         "movupd  %%xmm11, (%1)"         "\n"
         : /*OUT*/
         : /*IN*/ "r"(src), "r"(dst)
         : /*TRASH*/ "xmm11","xmm2"
      );
   }
}

void do_ROUNDPS_010 ( Bool mem, V128* src, /*OUT*/V128* dst )
{
   if (mem) {
      __asm__ __volatile__(
         "movupd  (%1), %%xmm11"       "\n\t"
         "roundps $2, (%0), %%xmm11"   "\n\t"
         "movupd  %%xmm11, (%1)"       "\n"
         : /*OUT*/
         : /*IN*/ "r"(src), "r"(dst)
         : /*TRASH*/ "xmm11"
      );
   } else {
      __asm__ __volatile__(
         "movupd  (%1), %%xmm11"         "\n\t"
         "movupd  (%0), %%xmm2"          "\n\t"
         "roundps $2, %%xmm2, %%xmm11"   "\n\t"
         "movupd  %%xmm11, (%1)"         "\n"
         : /*OUT*/
         : /*IN*/ "r"(src), "r"(dst)
         : /*TRASH*/ "xmm11","xmm2"
      );
   }
}

void do_ROUNDPS_011 ( Bool mem, V128* src, /*OUT*/V128* dst )
{
   if (mem) {
      __asm__ __volatile__(
         "movupd  (%1), %%xmm11"       "\n\t"
         "roundps $3, (%0), %%xmm11"   "\n\t"
         "movupd  %%xmm11, (%1)"       "\n"
         : /*OUT*/
         : /*IN*/ "r"(src), "r"(dst)
         : /*TRASH*/ "xmm11"
      );
   } else {
      __asm__ __volatile__(
         "movupd  (%1), %%xmm11"         "\n\t"
         "movupd  (%0), %%xmm2"          "\n\t"
         "roundps $3, %%xmm2, %%xmm11"   "\n\t"
         "movupd  %%xmm11, (%1)"         "\n"
         : /*OUT*/
         : /*IN*/ "r"(src), "r"(dst)
         : /*TRASH*/ "xmm11","xmm2"
      );
   }
}

void do_ROUNDPS_1XX ( Bool mem, V128* src, /*OUT*/V128* dst )
{
   if (mem) {
      __asm__ __volatile__(
         "movupd  (%1), %%xmm11"       "\n\t"
         "roundps $4, (%0), %%xmm11"   "\n\t"
         "movupd  %%xmm11, (%1)"       "\n"
         : /*OUT*/
         : /*IN*/ "r"(src), "r"(dst)
         : /*TRASH*/ "xmm11"
      );
   } else {
      __asm__ __volatile__(
         "movupd  (%1), %%xmm11"         "\n\t"
         "movupd  (%0), %%xmm2"          "\n\t"
         "roundps $4, %%xmm2, %%xmm11"   "\n\t"
         "movupd  %%xmm11, (%1)"         "\n"
         : /*OUT*/
         : /*IN*/ "r"(src), "r"(dst)
         : /*TRASH*/ "xmm11","xmm2"
      );
   }
}

void test_ROUNDPS_w_immediate_rounding ( void )
{
   float vals[22];
   Int i = 0;
   vals[i++] = 0.0;
   vals[i++] = -0.0;
   vals[i++] = mkPosInf();
   vals[i++] = mkNegInf();
   vals[i++] = mkPosNan();
   vals[i++] = mkNegNan();
   vals[i++] = -1.3;
   vals[i++] = -1.1;
   vals[i++] = -0.9;
   vals[i++] = -0.7;
   vals[i++] = -0.50001;
   vals[i++] = -0.49999;
   vals[i++] = -0.3;
   vals[i++] = -0.1;
   vals[i++] = 0.1;
   vals[i++] = 0.3;
   vals[i++] = 0.49999;
   vals[i++] = 0.50001;
   vals[i++] = 0.7;
   vals[i++] = 0.9;
   vals[i++] = 1.1;
   vals[i++] = 1.3;
   assert(i == 22);

   for (i = 0; i < sizeof(vals)/sizeof(vals[0]); i++) {
      V128 src, dst;

      randV128(&src);
      randV128(&dst);
      memcpy(&src[0], &vals[i], 4);
      memcpy(&src[4], &vals[(i+5)%22], 4);
      memcpy(&src[8], &vals[(i+11)%22], 4);
      memcpy(&src[12], &vals[(i+17)%22], 4);
      do_ROUNDPS_000(False/*reg*/, &src, &dst);
      printf("r roundps_000  ");
      showV128(&src);
      printf(" ");
      showV128(&dst);
      printf("  %9f:%9f", vals[i], (double)*(float*)(&dst[0]));
      printf("  %9f:%9f", vals[(i+5)%22], (double)*(float*)(&dst[4]));
      printf("  %9f:%9f", vals[(i+11)%22], (double)*(float*)(&dst[8]));
      printf("  %9f:%9f", vals[(i+17)%22], (double)*(float*)(&dst[12]));
      printf("\n");

      randV128(&src);
      randV128(&dst);
      memcpy(&src[0], &vals[i], 4);
      memcpy(&src[4], &vals[(i+5)%22], 4);
      memcpy(&src[8], &vals[(i+11)%22], 4);
      memcpy(&src[12], &vals[(i+17)%22], 4);
      do_ROUNDPS_000(True/*mem*/, &src, &dst);
      printf("m roundps_000  ");
      showV128(&src);
      printf(" ");
      showV128(&dst);
      printf("  %9f:%9f", vals[i], (double)*(float*)(&dst[0]));
      printf("  %9f:%9f", vals[(i+5)%22], (double)*(float*)(&dst[4]));
      printf("  %9f:%9f", vals[(i+11)%22], (double)*(float*)(&dst[8]));
      printf("  %9f:%9f", vals[(i+17)%22], (double)*(float*)(&dst[12]));
      printf("\n");


      randV128(&src);
      randV128(&dst);
      memcpy(&src[0], &vals[i], 4);
      memcpy(&src[4], &vals[(i+5)%22], 4);
      memcpy(&src[8], &vals[(i+11)%22], 4);
      memcpy(&src[12], &vals[(i+17)%22], 4);
      do_ROUNDPS_001(False/*reg*/, &src, &dst);
      printf("r roundps_001  ");
      showV128(&src);
      printf(" ");
      showV128(&dst);
      printf("  %9f:%9f", vals[i], (double)*(float*)(&dst[0]));
      printf("  %9f:%9f", vals[(i+5)%22], (double)*(float*)(&dst[4]));
      printf("  %9f:%9f", vals[(i+11)%22], (double)*(float*)(&dst[8]));
      printf("  %9f:%9f", vals[(i+17)%22], (double)*(float*)(&dst[12]));
      printf("\n");

      randV128(&src);
      randV128(&dst);
      memcpy(&src[0], &vals[i], 4);
      memcpy(&src[4], &vals[(i+5)%22], 4);
      memcpy(&src[8], &vals[(i+11)%22], 4);
      memcpy(&src[12], &vals[(i+17)%22], 4);
      do_ROUNDPS_001(True/*mem*/, &src, &dst);
      printf("m roundps_001  ");
      showV128(&src);
      printf(" ");
      showV128(&dst);
      printf("  %9f:%9f", vals[i], (double)*(float*)(&dst[0]));
      printf("  %9f:%9f", vals[(i+5)%22], (double)*(float*)(&dst[4]));
      printf("  %9f:%9f", vals[(i+11)%22], (double)*(float*)(&dst[8]));
      printf("  %9f:%9f", vals[(i+17)%22], (double)*(float*)(&dst[12]));
      printf("\n");


      randV128(&src);
      randV128(&dst);
      memcpy(&src[0], &vals[i], 4);
      memcpy(&src[4], &vals[(i+5)%22], 4);
      memcpy(&src[8], &vals[(i+11)%22], 4);
      memcpy(&src[12], &vals[(i+17)%22], 4);
      do_ROUNDPS_010(False/*reg*/, &src, &dst);
      printf("r roundps_010  ");
      showV128(&src);
      printf(" ");
      showV128(&dst);
      printf("  %9f:%9f", vals[i], (double)*(float*)(&dst[0]));
      printf("  %9f:%9f", vals[(i+5)%22], (double)*(float*)(&dst[4]));
      printf("  %9f:%9f", vals[(i+11)%22], (double)*(float*)(&dst[8]));
      printf("  %9f:%9f", vals[(i+17)%22], (double)*(float*)(&dst[12]));
      printf("\n");

      randV128(&src);
      randV128(&dst);
      memcpy(&src[0], &vals[i], 4);
      memcpy(&src[4], &vals[(i+5)%22], 4);
      memcpy(&src[8], &vals[(i+11)%22], 4);
      memcpy(&src[12], &vals[(i+17)%22], 4);
      do_ROUNDPS_010(True/*mem*/, &src, &dst);
      printf("m roundps_010  ");
      showV128(&src);
      printf(" ");
      showV128(&dst);
      printf("  %9f:%9f", vals[i], (double)*(float*)(&dst[0]));
      printf("  %9f:%9f", vals[(i+5)%22], (double)*(float*)(&dst[4]));
      printf("  %9f:%9f", vals[(i+11)%22], (double)*(float*)(&dst[8]));
      printf("  %9f:%9f", vals[(i+17)%22], (double)*(float*)(&dst[12]));
      printf("\n");


      randV128(&src);
      randV128(&dst);
      memcpy(&src[0], &vals[i], 4);
      memcpy(&src[4], &vals[(i+5)%22], 4);
      memcpy(&src[8], &vals[(i+11)%22], 4);
      memcpy(&src[12], &vals[(i+17)%22], 4);
      do_ROUNDPS_011(False/*reg*/, &src, &dst);
      printf("r roundps_011  ");
      showV128(&src);
      printf(" ");
      showV128(&dst);
      printf("  %9f:%9f", vals[i], (double)*(float*)(&dst[0]));
      printf("  %9f:%9f", vals[(i+5)%22], (double)*(float*)(&dst[4]));
      printf("  %9f:%9f", vals[(i+11)%22], (double)*(float*)(&dst[8]));
      printf("  %9f:%9f", vals[(i+17)%22], (double)*(float*)(&dst[12]));
      printf("\n");

      randV128(&src);
      randV128(&dst);
      memcpy(&src[0], &vals[i], 4);
      memcpy(&src[4], &vals[(i+5)%22], 4);
      memcpy(&src[8], &vals[(i+11)%22], 4);
      memcpy(&src[12], &vals[(i+17)%22], 4);
      do_ROUNDPS_011(True/*mem*/, &src, &dst);
      printf("m roundps_011  ");
      showV128(&src);
      printf(" ");
      showV128(&dst);
      printf("  %9f:%9f", vals[i], (double)*(float*)(&dst[0]));
      printf("  %9f:%9f", vals[(i+5)%22], (double)*(float*)(&dst[4]));
      printf("  %9f:%9f", vals[(i+11)%22], (double)*(float*)(&dst[8]));
      printf("  %9f:%9f", vals[(i+17)%22], (double)*(float*)(&dst[12]));
      printf("\n");
   }
}

void test_ROUNDPS_w_mxcsr_rounding ( void )
{
   UInt rm;
   float vals[22];
   Int i = 0;
   vals[i++] = 0.0;
   vals[i++] = -0.0;
   vals[i++] = mkPosInf();
   vals[i++] = mkNegInf();
   vals[i++] = mkPosNan();
   vals[i++] = mkNegNan();
   vals[i++] = -1.3;
   vals[i++] = -1.1;
   vals[i++] = -0.9;
   vals[i++] = -0.7;
   vals[i++] = -0.50001;
   vals[i++] = -0.49999;
   vals[i++] = -0.3;
   vals[i++] = -0.1;
   vals[i++] = 0.1;
   vals[i++] = 0.3;
   vals[i++] = 0.49999;
   vals[i++] = 0.50001;
   vals[i++] = 0.7;
   vals[i++] = 0.9;
   vals[i++] = 1.1;
   vals[i++] = 1.3;
   assert(i == 22);

   rm = get_sse_roundingmode();
   assert(rm == 0); // 0 == RN == default

   for (i = 0; i < sizeof(vals)/sizeof(vals[0]); i++) {
      V128 src, dst;

      for (rm = 0; rm <= 3; rm++) {
         set_sse_roundingmode(rm);

         randV128(&src);
         randV128(&dst);
         memcpy(&src[0], &vals[i], 4);
         memcpy(&src[4], &vals[(i+5)%22], 4);
         memcpy(&src[8], &vals[(i+11)%22], 4);
         memcpy(&src[12], &vals[(i+17)%22], 4);
         do_ROUNDPS_1XX(False/*reg*/, &src, &dst);
         printf("r (rm=%u) roundps_1XX  ", rm);
         showV128(&src);
         printf(" ");
         showV128(&dst);
         printf("  %9f:%9f", vals[i], (double)*(float*)(&dst[0]));
         printf("  %9f:%9f", vals[(i+5)%22], (double)*(float*)(&dst[4]));
         printf("  %9f:%9f", vals[(i+11)%22], (double)*(float*)(&dst[8]));
         printf("  %9f:%9f", vals[(i+17)%22], (double)*(float*)(&dst[12]));
         printf("\n");

         randV128(&src);
         randV128(&dst);
         memcpy(&src[0], &vals[i], 4);
         memcpy(&src[4], &vals[(i+5)%22], 4);
         memcpy(&src[8], &vals[(i+11)%22], 4);
         memcpy(&src[12], &vals[(i+17)%22], 4);
         do_ROUNDPS_1XX(True/*mem*/, &src, &dst);
         printf("m (rm=%u) roundps_1XX  ", rm);
         showV128(&src);
         printf(" ");
         showV128(&dst);
         printf("  %9f:%9f", vals[i], (double)*(float*)(&dst[0]));
         printf("  %9f:%9f", vals[(i+5)%22], (double)*(float*)(&dst[4]));
         printf("  %9f:%9f", vals[(i+11)%22], (double)*(float*)(&dst[8]));
         printf("  %9f:%9f", vals[(i+17)%22], (double)*(float*)(&dst[12]));
         printf("\n");
      }
   }

   rm = get_sse_roundingmode();
   assert(rm == 3);
   set_sse_roundingmode(0);
   rm = get_sse_roundingmode();
   assert(rm == 0); // 0 == RN == default
}

/* ------------ PTEST ------------ */

void test_PTEST ( void )
{
   const Int ntests = 8;
   V128 spec[ntests];
   do64HLtoV128( &spec[0], 0x0000000000000000ULL, 0x0000000000000000ULL );
   do64HLtoV128( &spec[1], 0x0000000000000000ULL, 0x0000000000000001ULL );
   do64HLtoV128( &spec[2], 0x0000000000000001ULL, 0x0000000000000000ULL );
   do64HLtoV128( &spec[3], 0x0000000000000001ULL, 0x0000000000000001ULL );
   do64HLtoV128( &spec[4], 0xffffffffffffffffULL, 0xffffffffffffffffULL );
   do64HLtoV128( &spec[5], 0xffffffffffffffffULL, 0xfffffffffffffffeULL );
   do64HLtoV128( &spec[6], 0xfffffffffffffffeULL, 0xffffffffffffffffULL );
   do64HLtoV128( &spec[7], 0xfffffffffffffffeULL, 0xfffffffffffffffeULL );
   V128 block[2];
   Int i, j;
   ULong flags;
   for (i = 0; i < ntests; i++) {
      for (j = 0; j < ntests; j++) {
         memcpy(&block[0], &spec[i], 16);
         memcpy(&block[1], &spec[j], 16);
         __asm__ __volatile__(
            "subq $256, %%rsp"        "\n\t"
            "movupd 0(%1), %%xmm2"    "\n\t"
            "ptest 16(%1), %%xmm2"    "\n\t"
            "pushfq"                  "\n\t"
            "popq %0"                 "\n\t"
            "addq $256, %%rsp"        "\n\t"
            : /*out*/"=r"(flags) : /*in*/ "r"(&block[0]) :
            "xmm2", "memory", "cc"
         );
         printf("r   ptest ");
         showV128(&block[0]);
         printf(" ");
         showV128(&block[1]);
         printf(" -> eflags %04x\n", (UInt)flags & 0x8D5);
      }
   }
}

/* ------------ PBLENDVB ------------ */

void do_PBLENDVB ( Bool mem, V128* xmm0, V128* src, /*MOD*/V128* dst )
{
   if (mem) {
      __asm__ __volatile__(
         "movupd   (%2), %%xmm0"         "\n\t"
         "movupd   (%1), %%xmm11"        "\n\t"
         "pblendvb (%0), %%xmm11"        "\n\t"
         "movupd   %%xmm11, (%1)"        "\n"
         : /*OUT*/
         : /*IN*/ "r"(src), "r"(dst), "r"(xmm0)
         : /*TRASH*/ "xmm11","xmm0"
      );
   } else {
      __asm__ __volatile__(
         "movupd   (%2), %%xmm0"         "\n\t"
         "movupd   (%1), %%xmm11"        "\n\t"
         "movupd   (%0), %%xmm2"         "\n\t"
         "pblendvb %%xmm2, %%xmm11"      "\n\t"
         "movupd   %%xmm11, (%1)"        "\n"
         : /*OUT*/
         : /*IN*/ "r"(src), "r"(dst), "r"(xmm0)
         : /*TRASH*/ "xmm11","xmm2","xmm0"
      );
   }
}

void test_PBLENDVB ( void )
{
   V128 xmm0, src, dst, t_xmm0, t_src, t_dst;
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

void do_BLENDVPD ( Bool mem, V128* xmm0, V128* src, /*MOD*/V128* dst )
{
   if (mem) {
      __asm__ __volatile__(
         "movupd   (%2), %%xmm0"         "\n\t"
         "movupd   (%1), %%xmm11"        "\n\t"
         "blendvpd (%0), %%xmm11"        "\n\t"
         "movupd   %%xmm11, (%1)"        "\n"
         : /*OUT*/
         : /*IN*/ "r"(src), "r"(dst), "r"(xmm0)
         : /*TRASH*/ "xmm11","xmm0"
      );
   } else {
      __asm__ __volatile__(
         "movupd   (%2), %%xmm0"         "\n\t"
         "movupd   (%1), %%xmm11"        "\n\t"
         "movupd   (%0), %%xmm2"         "\n\t"
         "blendvpd %%xmm2, %%xmm11"      "\n\t"
         "movupd   %%xmm11, (%1)"        "\n"
         : /*OUT*/
         : /*IN*/ "r"(src), "r"(dst), "r"(xmm0)
         : /*TRASH*/ "xmm11","xmm2","xmm0"
      );
   }
}

void test_BLENDVPD ( void )
{
   V128 xmm0, src, dst, t_xmm0, t_src, t_dst;
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

void do_BLENDVPS ( Bool mem, V128* xmm0, V128* src, /*MOD*/V128* dst )
{
   if (mem) {
      __asm__ __volatile__(
         "movupd   (%2), %%xmm0"         "\n\t"
         "movupd   (%1), %%xmm11"        "\n\t"
         "blendvps (%0), %%xmm11"        "\n\t"
         "movupd   %%xmm11, (%1)"        "\n"
         : /*OUT*/
         : /*IN*/ "r"(src), "r"(dst), "r"(xmm0)
         : /*TRASH*/ "xmm11","xmm0"
      );
   } else {
      __asm__ __volatile__(
         "movupd   (%2), %%xmm0"         "\n\t"
         "movupd   (%1), %%xmm11"        "\n\t"
         "movupd   (%0), %%xmm2"         "\n\t"
         "blendvps %%xmm2, %%xmm11"      "\n\t"
         "movupd   %%xmm11, (%1)"        "\n"
         : /*OUT*/
         : /*IN*/ "r"(src), "r"(dst), "r"(xmm0)
         : /*TRASH*/ "xmm11","xmm2","xmm0"
      );
   }
}

void test_BLENDVPS ( void )
{
   V128 xmm0, src, dst, t_xmm0, t_src, t_dst;
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

void test_MOVNTDQA ( void )
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

/* ------------ main ------------ */

int main ( int argc, char** argv )
{
#if 1
   // ------ SSE 4.1 ------
   test_BLENDPD();        // done Apr.01.2010
   test_BLENDPS();        // done Apr.02.2010
   test_PBLENDW();
   test_PBLENDVB();
   test_BLENDVPD();
   test_BLENDVPS();
   test_DPPD();           // done Apr.08.2010
   test_DPPS();           // done Apr.09.2010
   test_EXTRACTPS();
   test_INSERTPS();       // done Apr.01.2010
   test_PCMPEQQ();
   test_PEXTRB();         // done Apr.15.2010
   test_PEXTRD();         // done Apr.14.2010
   test_PEXTRQ();         // done Apr.14.2010
   test_PEXTRW();         // done Apr.14.2010
   test_PINSRQ();         // done Apr.16.2010
   test_PINSRD();         // todo
   test_PINSRW(); /* Umm, this is SSE2, not SSE4.  Right? */
   test_PINSRB();         // todo
   test_PMAXSB();
   test_PMAXSD();         // done Apr.09.2010
   test_PMAXUD();         // done Apr.16.2010
   test_PMAXUW();
   test_PMINSB();
   test_PMINSD();         // done Apr.09.2010
   test_PMINUD();
   test_PMINUW();
   test_PMOVSXBW();       // done Apr.02.2010
   test_PMOVSXBD();       // done Mar.30.2010
   test_PMOVSXBQ();       // done Mar.30.2010
   test_PMOVSXWD();       // done Mar.31.2010
   test_PMOVSXWQ();       // done Mar.31.2010
   test_PMOVSXDQ();       // done Mar.31.2010
   test_PMOVZXBW();       // done Mar.28.2010
   test_PMOVZXBD();       // done Mar.29.2010
   test_PMOVZXBQ();       // done Mar.29.2010
   test_PMOVZXWD();       // done Mar.28.2010
   test_PMOVZXWQ();       // done Mar.29.2010
   test_PMOVZXDQ();       // done Mar.29.2010
   test_POPCNTW();
   test_POPCNTL();
   test_POPCNTQ();
   test_PMULDQ();
   test_PMULLD();
   test_PTEST();
   test_ROUNDSD_w_immediate_rounding();
   test_ROUNDSS_w_immediate_rounding();
   test_ROUNDPD_w_immediate_rounding();
   test_ROUNDPS_w_immediate_rounding();
   test_ROUNDSD_w_mxcsr_rounding();
   test_ROUNDSS_w_mxcsr_rounding();
   test_ROUNDPD_w_mxcsr_rounding();
   test_ROUNDPS_w_mxcsr_rounding();
   // ------ SSE 4.2 ------
   test_PCMPGTQ();
   // CRC32B,Q
   test_PACKUSDW();
   test_PHMINPOSUW();
   test_MPSADBW();
   test_MOVNTDQA(); /* not sure whether this is 4.1 or 4.2 */
#else
   test_MPSADBW();
#endif

   return 0;
}

