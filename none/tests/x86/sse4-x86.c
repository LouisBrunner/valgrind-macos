
/* A program to test SSE4.1/SSE4.2 instructions. 
   Copied from amd64 version.
*/

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


static void showV128 ( V128* v )
{
   Int i;
   for (i = 15; i >= 0; i--)
      printf("%02x", (Int)(*v)[i]);
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

static V128 fives    = { 0x55,0x55,0x55,0x55, 0x55,0x55,0x55,0x55,
                         0x55,0x55,0x55,0x55, 0x55,0x55,0x55,0x55 };

static V128 zeroes   = { 0x00,0x00,0x00,0x00, 0x00,0x00,0x00,0x00,
                         0x00,0x00,0x00,0x00, 0x00,0x00,0x00,0x00 };

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

#define DO_imm_r_to_rscalar(_opname, _imm, _src)       \
   {  \
      ULong _scbefore = 0x5555555555555555ULL;  \
      ULong _scafter  = 0xAAAAAAAAAAAAAAAAULL; \
      /* This assumes that gcc won't make any of %0, %1, %2 */ \
      /* be r11.  That should be ensured (cough, cough) */ \
      /* by declaring r11 to be clobbered. */ \
      __asm__ __volatile__(  \
         "movupd (%0), %%xmm2"    "\n\t"  \
         "movq   (%1), %%r11"   "\n\t"  \
         _opname " $" #_imm ", %%xmm2, %%r11"  "\n\t"  \
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

#define DO_imm_r_to_mandrscalar(_opname, _imm, _src )   \
      DO_imm_r_to_rscalar( _opname, _imm, _src )       \
      DO_imm_r_to_mscalar( _opname, _imm, _src )


#define DO_imm_rscalar_to_r(_opname, _imm, _src)       \
   {  \
      V128  dstv;         \
      V128  res;          \
      ULong src64 = (ULong)(_src); \
      memcpy(dstv, fives, sizeof(dstv)); \
      memcpy(res,  zeroes, sizeof(res)); \
      __asm__ __volatile__(  \
         "movupd (%0), %%xmm2"    "\n\t"   /*dstv*/   \
         "mov   (%1), %%eax"     "\n\t"   /*src64*/  \
         _opname " $" #_imm ", %%eax" ", %%xmm2"   "\n\t"  \
         "movupd  %%xmm2, (%2)" "\n" /*res*/                          \
         : /*out*/ \
         : /*in*/ "r"(&dstv), "r"(&src64), "r"(&res)  \
         : "cc", "memory", "xmm2", "eax"  \
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

#define DO_imm_mandrscalar_to_r(_opname, _imm, _src )   \
      DO_imm_rscalar_to_r( _opname, _imm, _src )       \
      DO_imm_mscalar_to_r( _opname, _imm, _src )


void test_PINSRD ( void )
{
   ULong src;
   src = randULong();
   DO_imm_mandrscalar_to_r("pinsrd", 0, src);
   src = randULong();
   DO_imm_mandrscalar_to_r("pinsrd", 1, src);
   src = randULong();
   DO_imm_mandrscalar_to_r("pinsrd", 2, src);
   src = randULong();
   DO_imm_mandrscalar_to_r("pinsrd", 3, src);
}

/* ------------ main ------------ */

int main(void)
{
   // ------ SSE 4.1 ------
   test_PINSRD();

   return 0;
}

