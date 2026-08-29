
/* A program to test SSE4.1/SSE4.2 instructions.
   Copied from amd64 version.
*/

#define DO_imm_r_to_rscalar(_opname, _imm, _src)       \
   {  \
      ULong _scbefore = 0x5555555555555555ULL;  \
      ULong _scafter  = 0ULL; \
      __asm__ __volatile__(  \
         "movupd (%0), %%xmm2"    "\n\t"  \
         _opname " $" #_imm ", %%xmm2, %%eax"  "\n\t"  \
         "movl   %%eax, (%1)" "\n"  \
         : /*out*/ \
         : /*in*/ "r"(&(_src)), "r"(&(_scafter))  \
         : "cc", "memory", "xmm2", "eax"  \
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

#define DO_imm_r_to_mandrscalar(_opname, _imm, _src, ...)   \
      DO_imm_r_to_rscalar( _opname, _imm, _src )            \
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

#include "../sse4-common.h"


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

/* ------------ PTEST ------------ */

/* Same test vectors as amd64 variant. Same flag results.  */
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
   __attribute__ ( (aligned (16))) V128 block[2];
   Int i, j;
   UInt flags;
   for (i = 0; i < ntests; i++) {
      for (j = 0; j < ntests; j++) {
         memcpy(&block[0], &spec[i], 16);
         memcpy(&block[1], &spec[j], 16);

         __asm__ __volatile__(            \
            "sub $256, %%esp"      "\n\t" \
            "movaps 0(%1), %%xmm2" "\n\t" \
            "ptest 16(%1), %%xmm2" "\n\t" \
            "pushf"                "\n\t" \
            "pop %0"               "\n\t" \
            "add $256, %%esp"      "\n\t" \
            : /*out*/ "=r"(flags)
            : /*in*/ "r"(&block[0])
            : "xmm2", "memory", "cc");

         printf("r   ptest ");
         showV128(&block[0]);
         printf(" ");
         showV128(&block[1]);
         printf(" -> flags %04x\n", flags & 0x8D5);
      }
   }
}

void test_POPCNTL_x86 ( void )
{
   UInt block[4];
   Int i;
   UInt oszacp_mask = 0x8D5;
   for (i = 0; i < 10; i++) {
      block[0] = i == 0 ? 0 : (UInt)randUInt();
      block[1] = (UInt)randUInt();
      block[2] = (UInt)randUInt();
      block[3] = (UInt)randUInt();
      __asm__ __volatile__(
         "movl %0,       %%eax"  "\n\t"
         "movl 0(%%eax), %%edi"  "\n\t"
         "movl 4(%%eax), %%ecx"  "\n\t"
         "popcntl %%edi,  %%ecx"  "\n\t"
         "movl %%ecx, 8(%%eax)"  "\n\t"
         "pushf"                 "\n\t"
         "popl %%edx"             "\n\t"
         "movl %%edx, 12(%%eax)"  "\n"
         : /*out*/
         : /*in*/"r"(&block[0])
         : /*trash*/ "cc", "memory", "edi", "ecx", "edx"
      );
      printf("r popcntl  %08x %08x  %08x %08x\n",
             block[0], block[1], block[2], block[3] & oszacp_mask);

      block[0] = i == 0 ? 0 : (UInt)randUInt();
      block[1] = (UInt)randUInt();
      block[2] = (UInt)randUInt();
      block[3] = (UInt)randUInt();
      __asm__ __volatile__(
         "movl %0,       %%eax"  "\n\t"
         "movl 4(%%eax), %%ecx"  "\n\t"
         "popcntl 0(%%eax), %%ecx"  "\n\t"
         "movl %%ecx, 8(%%eax)"  "\n\t"
         "pushf"                 "\n\t"
         "popl %%edx"             "\n\t"
         "movl %%edx, 12(%%eax)"  "\n"
         : /*out*/
         : /*in*/"r"(&block[0])
         : /*trash*/ "cc", "memory", "ecx", "edx"
      );
      printf("m popcntl  %08x %08x  %08x %08x\n",
             block[0], block[1], block[2], block[3] & oszacp_mask);
   }
}

void test_POPCNTW_x86 ( void )
{
   UInt block[4];
   Int i;
   UInt oszacp_mask = 0x8D5;
   for (i = 0; i < 10; i++) {
      block[0] = i == 0 ? 0 : (UInt)randUInt();
      block[1] = (UInt)randUInt();
      block[2] = (UInt)randUInt();
      block[3] = (UInt)randUInt();
      __asm__ __volatile__(
         "movl %0,       %%eax"  "\n\t"
         "movl 0(%%eax), %%edi"  "\n\t"
         "movl 4(%%eax), %%ecx"  "\n\t"
         "popcntw %%di,  %%cx"  "\n\t"
         "movl %%ecx, 8(%%eax)"  "\n\t"
         "pushf"                 "\n\t"
         "popl %%edx"             "\n\t"
         "movl %%edx, 12(%%eax)"  "\n"
         : /*out*/
         : /*in*/"r"(&block[0])
         : /*trash*/ "cc", "memory", "edi", "ecx", "edx"
      );
      printf("r popcntw  %08x %08x  %08x %08x\n",
             block[0], block[1], block[2], block[3] & oszacp_mask);

      block[0] = i == 0 ? 0 : (UInt)randUInt();
      block[1] = (UInt)randUInt();
      block[2] = (UInt)randUInt();
      block[3] = (UInt)randUInt();
      __asm__ __volatile__(
         "movl %0,       %%eax"  "\n\t"
         "movl 4(%%eax), %%ecx"  "\n\t"
         "popcntw 0(%%eax), %%cx"  "\n\t"
         "movl %%ecx, 8(%%eax)"  "\n\t"
         "pushf"                 "\n\t"
         "popl %%edx"             "\n\t"
         "movl %%edx, 12(%%eax)"  "\n"
         : /*out*/
         : /*in*/"r"(&block[0])
         : /*trash*/ "cc", "memory", "ecx", "edx"
      );
      printf("m popcntw  %08x %08x  %08x %08x\n",
             block[0], block[1], block[2], block[3] & oszacp_mask);
   }
}

/* ------------ main ------------ */

int main(void)
{
   // ------ SSE 4.1 ------
   test_PINSRD();
   test_PMAXSB();
   test_PMAXSD();
   test_PMAXUD();
   test_PMAXUW();
   test_PMINSB();
   test_PMINSD();
   test_PMINUD();
   test_PMINUW();
   test_PMULLD();
   test_BLENDPD();
   test_BLENDPS();
   test_PBLENDW();
   test_PBLENDVB();
   test_BLENDVPD();
   test_BLENDVPS();
   test_PTEST();
   test_PCMPEQQ();
   test_MPSADBW();
   test_MOVNTDQA();
   test_ROUNDSD_w_immediate_rounding();
   test_ROUNDSS_w_immediate_rounding();
   test_ROUNDSD_w_mxcsr_rounding();
   test_ROUNDSS_w_mxcsr_rounding();
   test_PEXTRD();
   test_PACKUSDW();
   test_POPCNTL_x86();
   test_POPCNTW_x86();
   test_PHMINPOSUW();
   test_ROUNDPD_w_immediate_rounding();
   test_ROUNDPS_w_immediate_rounding();
   test_ROUNDPD_w_mxcsr_rounding();
   test_ROUNDPS_w_mxcsr_rounding();
   test_PMULDQ();
   test_PCMPGTQ();
   test_PMOVSXBW();

   return 0;
}

