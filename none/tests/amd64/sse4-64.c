
/* A program to test SSE4.1/SSE4.2 instructions. 
   Revisions:  Nov.208     - wrote this file
               Apr.10.2010 - added PEXTR* tests
               Apr.16.2010 - added PINS*  tests
*/

/* HOW TO COMPILE:
   gcc -m64 -g -O -Wall -o sse4-64 sse4-64.c
*/

/* Architecture-specific macros for amd64 (use %r11) */

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

#include "../sse4-common.h"

/* Architecture-specific macros for amd64 (use %r11) */




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

