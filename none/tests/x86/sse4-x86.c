
/* A program to test SSE4.1/SSE4.2 instructions.
   Copied from amd64 version.
*/

#include "../sse4-common.h"

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

