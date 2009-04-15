
/* A program to test that SSE/SSE2 insns do not read memory they
   should not.  Covers insns of the form OP %xmm, %xmm and OP memory,
   %xmm only. */

#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include "tests/malloc.h"
#include <string.h>

typedef  unsigned char  V128[16];
typedef  unsigned int   UInt;
typedef  signed int     Int;
typedef  unsigned char  UChar;

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

static void randomise ( UChar* p, Int n )
{
   Int i;
   for (i = 0; i < n; i++)
      p[i] = randUChar();
}

static void randV128 ( V128* v )
{
   Int i;
   for (i = 0; i < 16; i++)
      (*v)[i] = randUChar();
}

static void randRRArgs ( RRArgs* rra )
{
   randV128(&rra->arg1);
   randV128(&rra->arg2);
   randV128(&rra->res);
}

static void randRMArgs ( RMArgs* rra )
{
   randV128(&rra->arg1);
   randV128(&rra->res);
}

static void showV128 ( V128* v )
{
   Int i;
   for (i = 0; i < 16; i++)
      printf("%02x", (Int)(*v)[i]);
}

static void showMaskedV128 ( V128* v, V128* mask )
{
   Int i;
   for (i = 0; i < 16; i++)
      printf("%02x", (Int)( ((*v)[i]) & ((*mask)[i]) ));
}

static void showRR ( char* op, RRArgs* rra, V128* rmask )
{
   printf("r %10s ", op);
   showV128(&rra->arg1);
   printf(" ");
   showV128(&rra->arg2);
   printf(" ");
   showMaskedV128(&rra->res, rmask);
   printf("\n");
}

static void showRM ( char* op, RMArgs* rra, UChar* mem, Int nMem, V128* rmask )
{
   Int i;
   assert(nMem == 4 || nMem == 8 || nMem == 16 || nMem==0);
   printf("m %10s ", op);
   for (i = 0; i < nMem; i++)
      printf("%02x", (Int)mem[i]);
   printf(" ");
   showV128(&rra->arg1);
   printf(" ");
   showMaskedV128(&rra->res, rmask );
   printf("\n");
}

#define Wrapper_RegReg(OP)                 \
   void r_r_##OP ( RRArgs* p )             \
   {                                       \
      __asm__ __volatile__("\n"            \
         "\tmovups 0(%0), %%xmm6\n"        \
         "\tmovups 16(%0), %%xmm7\n"       \
         "\t" #OP " %%xmm6, %%xmm7\n"      \
         "\tmovups %%xmm7, 32(%0)\n"       \
         :                                 \
         : "r" (p)                         \
         : "memory", "xmm6", "xmm7", "cc"  \
      );                                   \
   }

#define Wrapper_RegMem(OP)                 \
   void r_m_##OP ( RMArgs* p, void* mem )  \
   {                                       \
      __asm__ __volatile__("\n"            \
         "\tmovups 0(%0), %%xmm7\n"        \
         "\t" #OP " 0(%1), %%xmm7\n"       \
         "\tmovups %%xmm7, 16(%0)\n"       \
         :                                 \
         : "r" (p), "r" (mem)              \
         : "memory", "xmm7", "cc"          \
      );                                   \
   }


#define TEST_INSN(res_mask,mem_size,insn)  \
                                           \
Wrapper_RegReg(insn)                       \
Wrapper_RegMem(insn)                       \
                                           \
void do_##insn ( void )                    \
{                                          \
   Int    i;                               \
   UChar* buf;                             \
   RRArgs rargs __attribute__((aligned(16))); \
   RMArgs margs __attribute__((aligned(16))); \
   for (i = 0; i < 5; i++) {               \
      randRRArgs(&rargs);                  \
      r_r_##insn(&rargs);                  \
      showRR(#insn, &rargs, res_mask);     \
   }                                       \
   for (i = 0; i < 5; i++) {               \
      randRMArgs(&margs);                  \
      buf = memalign16(mem_size);          \
      randomise(buf,mem_size);             \
      r_m_##insn(&margs,buf);              \
      showRM(#insn, &margs, buf, mem_size, res_mask);\
      free(buf);                           \
   }                                       \
}

/* Note: these are little endian.  Hence first byte is the least
   significant byte of lane zero. */

/* Mask for insns where all result bits are non-approximated. */
static V128 AllMask  = { 0xFF,0xFF,0xFF,0xFF, 0xFF,0xFF,0xFF,0xFF, 
                         0xFF,0xFF,0xFF,0xFF, 0xFF,0xFF,0xFF,0xFF };

/* Mark for insns which produce approximated vector short results. */
static V128 ApproxPS = { 0x00,0x00,0x80,0xFF, 0x00,0x00,0x80,0xFF, 
                         0x00,0x00,0x80,0xFF, 0x00,0x00,0x80,0xFF };

/* Mark for insns which produce approximated scalar short results. */
static V128 ApproxSS = { 0x00,0x00,0x80,0xFF, 0xFF,0xFF,0xFF,0xFF,
                         0xFF,0xFF,0xFF,0xFF, 0xFF,0xFF,0xFF,0xFF };

#define PD 16
#define SD 8
#define PS 16
#define SS 4

/* ------------------------ SSE1 ------------------------ */
TEST_INSN( &AllMask, PS,addps)
TEST_INSN( &AllMask, SS,addss)
TEST_INSN( &AllMask, PS,andnps)
TEST_INSN( &AllMask, PS,andps)
TEST_INSN( &AllMask, PS,cmpeqps)
TEST_INSN( &AllMask, SS,cmpeqss)
TEST_INSN( &AllMask, PS,cmpleps)
TEST_INSN( &AllMask, SS,cmpless)
TEST_INSN( &AllMask, PS,cmpltps)
TEST_INSN( &AllMask, SS,cmpltss)
TEST_INSN( &AllMask, PS,cmpneqps)
TEST_INSN( &AllMask, SS,cmpneqss)
TEST_INSN( &AllMask, PS,cmpnleps)
TEST_INSN( &AllMask, SS,cmpnless)
TEST_INSN( &AllMask, PS,cmpnltps)
TEST_INSN( &AllMask, SS,cmpnltss)
TEST_INSN( &AllMask, PS,cmpordps)
TEST_INSN( &AllMask, SS,cmpordss)
TEST_INSN( &AllMask, PS,cmpunordps)
TEST_INSN( &AllMask, SS,cmpunordss)
TEST_INSN( &AllMask, SS,comiss)
//TEST_INSN( &AllMask, 0,cvtpi2ps)
//TEST_INSN( &AllMask, 0,cvtps2pi)
//TEST_INSN( &AllMask, 0,cvtsi2ss)
//TEST_INSN( &AllMask, 0,cvtss2si)
//TEST_INSN( &AllMask, 0,cvttps2pi)
//TEST_INSN( &AllMask, 0,cvttss2si)
TEST_INSN( &AllMask, PS,divps)
TEST_INSN( &AllMask, SS,divss)
TEST_INSN( &AllMask, PS,maxps)
TEST_INSN( &AllMask, SS,maxss)
TEST_INSN( &AllMask, PS,minps)
TEST_INSN( &AllMask, SS,minss)
TEST_INSN( &AllMask, 16,movaps)
//TEST_INSN( &AllMask, 0,movhlps)
//TEST_INSN( &AllMask, 0,movhps)
//TEST_INSN( &AllMask, 0,movlhps)
//TEST_INSN( &AllMask, 0,movlps)
//TEST_INSN( &AllMask, 0,movmskps)
//TEST_INSN( &AllMask, 0,movntps)
//TEST_INSN( &AllMask, 0,movntq)
TEST_INSN( &AllMask, 4,movss)
TEST_INSN( &AllMask, 16,movups)
TEST_INSN( &AllMask, PS,mulps)
TEST_INSN( &AllMask, SS,mulss)
TEST_INSN( &AllMask, PS,orps)
//TEST_INSN( &AllMask, 0,pavgb) -- dup with sse2?
//TEST_INSN( &AllMask, 0,pavgw) -- dup with sse2?
//TEST_INSN( &AllMask, 0,pextrw)
//TEST_INSN( &AllMask, 0,pinsrw)
//TEST_INSN( &AllMask, 0,pmaxsw) -- dup with sse2?
//TEST_INSN( &AllMask, 0,pmaxub) -- dup with sse2?
//TEST_INSN( &AllMask, 0,pminsw) -- dup with sse2?
//TEST_INSN( &AllMask, 0,pminub) -- dup with sse2?
//TEST_INSN( &AllMask, 0,pmovmskb)
//TEST_INSN( &AllMask, 0,pmulhuw) -- dup with sse2?
TEST_INSN( &AllMask, 16,psadbw) // -- XXXXXXXXXXXXXXXX sse2 (xmm variant) not implemented!
//TEST_INSN( &AllMask, 0,pshufw)
TEST_INSN(&ApproxPS, PS,rcpps)
TEST_INSN(&ApproxSS, SS,rcpss)
TEST_INSN(&ApproxPS, PS,rsqrtps)
TEST_INSN(&ApproxSS, SS,rsqrtss)
//TEST_INSN( &AllMask, PS,shufps)
TEST_INSN( &AllMask, PS,sqrtps)
TEST_INSN( &AllMask, SS,sqrtss)
TEST_INSN( &AllMask, PS,subps)
TEST_INSN( &AllMask, SS,subss)
TEST_INSN( &AllMask, SS,ucomiss)
TEST_INSN( &AllMask, PS,unpckhps)
TEST_INSN( &AllMask, PS,unpcklps)
TEST_INSN( &AllMask, PS,xorps)


/* ------------------------ SSE2 ------------------------ */
TEST_INSN( &AllMask, PD,addpd)
TEST_INSN( &AllMask, SD,addsd)
TEST_INSN( &AllMask, PD,andnpd)
TEST_INSN( &AllMask, PD,andpd)
TEST_INSN( &AllMask, PD,cmpeqpd)
TEST_INSN( &AllMask, SD,cmpeqsd)
TEST_INSN( &AllMask, PD,cmplepd)
TEST_INSN( &AllMask, SD,cmplesd)
TEST_INSN( &AllMask, PD,cmpltpd)
TEST_INSN( &AllMask, SD,cmpltsd)
TEST_INSN( &AllMask, PD,cmpneqpd)
TEST_INSN( &AllMask, SD,cmpneqsd)
TEST_INSN( &AllMask, PD,cmpnlepd)
TEST_INSN( &AllMask, SD,cmpnlesd)
TEST_INSN( &AllMask, PD,cmpnltpd)
TEST_INSN( &AllMask, SD,cmpnltsd)
TEST_INSN( &AllMask, PD,cmpordpd)
TEST_INSN( &AllMask, SD,cmpordsd)
TEST_INSN( &AllMask, PD,cmpunordpd)
TEST_INSN( &AllMask, SD,cmpunordsd)
TEST_INSN( &AllMask, SD,comisd)
TEST_INSN( &AllMask, 8,cvtdq2pd)
TEST_INSN( &AllMask, 16,cvtdq2ps)
TEST_INSN( &AllMask, 16,cvtpd2dq)
//TEST_INSN( &AllMask, 0,cvtpd2pi)
TEST_INSN( &AllMask, 16,cvtpd2ps)   /* reads 16 */
//TEST_INSN( &AllMask, 0,cvtpi2pd)
TEST_INSN( &AllMask, 16,cvtps2dq)  /* reads 16 */
TEST_INSN( &AllMask, 8,cvtps2pd)   /* reads 8 */
//TEST_INSN( &AllMask, 0,cvtsd2si)
TEST_INSN( &AllMask, SD,cvtsd2ss)   /* reads SD */
//TEST_INSN( &AllMask, 0,cvtsi2sd)
TEST_INSN( &AllMask, SS,cvtss2sd)   /* reads SS */
TEST_INSN( &AllMask, 16,cvttpd2dq)
//TEST_INSN( &AllMask, 0,cvttpd2pi)
TEST_INSN( &AllMask, 16,cvttps2dq)
//TEST_INSN( &AllMask, 0,cvttsd2si)
TEST_INSN( &AllMask, PD,divpd)
TEST_INSN( &AllMask, SD,divsd)
TEST_INSN( &AllMask, PD,maxpd)
TEST_INSN( &AllMask, SD,maxsd)
TEST_INSN( &AllMask, PD,minpd)
TEST_INSN( &AllMask, SD,minsd)
TEST_INSN( &AllMask, PD,movapd)
//TEST_INSN( &AllMask, 8,movd)
//TEST_INSN( &AllMask, 0,movdq2q)
TEST_INSN( &AllMask, 16,movdqa)
TEST_INSN( &AllMask, 16,movdqu)
//TEST_INSN( &AllMask, 16,movhpd)
//TEST_INSN( &AllMask, 16,movlpd)
//TEST_INSN( &AllMask, 0,movmskpd)
//TEST_INSN( &AllMask, 0,movntdq)
//TEST_INSN( &AllMask, 0,movnti)
//TEST_INSN( &AllMask, 0,movntpd)
TEST_INSN( &AllMask, 8,movq)
//TEST_INSN( &AllMask, 0,movq2dq)
TEST_INSN( &AllMask, 8,movsd)
TEST_INSN( &AllMask, 16,movupd)
TEST_INSN( &AllMask, PD,mulpd)
TEST_INSN( &AllMask, SD,mulsd)
TEST_INSN( &AllMask, PD,orpd)
TEST_INSN( &AllMask, 16,packssdw)
TEST_INSN( &AllMask, 16,packsswb)
TEST_INSN( &AllMask, 16,packuswb)
TEST_INSN( &AllMask, 16,paddb)
TEST_INSN( &AllMask, 16,paddd)
TEST_INSN( &AllMask, 16,paddq)
TEST_INSN( &AllMask, 16,paddsb)
TEST_INSN( &AllMask, 16,paddsw)
TEST_INSN( &AllMask, 16,paddusb)
TEST_INSN( &AllMask, 16,paddusw)
TEST_INSN( &AllMask, 16,paddw)
TEST_INSN( &AllMask, 16,pand)
TEST_INSN( &AllMask, 16,pandn)
TEST_INSN( &AllMask, 16,pavgb)
TEST_INSN( &AllMask, 16,pavgw)
TEST_INSN( &AllMask, 16,pcmpeqb)
TEST_INSN( &AllMask, 16,pcmpeqd)
TEST_INSN( &AllMask, 16,pcmpeqw)
TEST_INSN( &AllMask, 16,pcmpgtb)
TEST_INSN( &AllMask, 16,pcmpgtd)
TEST_INSN( &AllMask, 16,pcmpgtw)
//TEST_INSN( &AllMask, 16,pextrw)
//TEST_INSN( &AllMask, 16,pinsrw)
TEST_INSN( &AllMask, 16,pmaxsw)
TEST_INSN( &AllMask, 16,pmaxub)
TEST_INSN( &AllMask, 16,pminsw)
TEST_INSN( &AllMask, 16,pminub)
//TEST_INSN( &AllMask, 0,pmovmskb)
TEST_INSN( &AllMask, 16,pmulhuw)
TEST_INSN( &AllMask, 16,pmulhw)
TEST_INSN( &AllMask, 16,pmullw)
TEST_INSN( &AllMask, 16,pmuludq)
TEST_INSN( &AllMask, 16,por)
//TEST_INSN( &AllMask, 16,pshufd)
//TEST_INSN( &AllMask, 16,pshufhw)
//TEST_INSN( &AllMask, 16,pshuflw)
TEST_INSN( &AllMask, 16,pslld)
//TEST_INSN( &AllMask, 16,pslldq)
TEST_INSN( &AllMask, 16,psllq)
TEST_INSN( &AllMask, 16,psllw)
TEST_INSN( &AllMask, 16,psrad)
TEST_INSN( &AllMask, 16,psraw)
TEST_INSN( &AllMask, 16,psrld)
//TEST_INSN( &AllMask, 16,psrldq)
TEST_INSN( &AllMask, 16,psrlq)
TEST_INSN( &AllMask, 16,psrlw)
TEST_INSN( &AllMask, 16,psubb)
TEST_INSN( &AllMask, 16,psubd)
TEST_INSN( &AllMask, 16,psubq)
TEST_INSN( &AllMask, 16,psubsb)
TEST_INSN( &AllMask, 16,psubsw)
TEST_INSN( &AllMask, 16,psubusb)
TEST_INSN( &AllMask, 16,psubusw)
TEST_INSN( &AllMask, 16,psubw)
TEST_INSN( &AllMask, 16,punpckhbw)
TEST_INSN( &AllMask, 16,punpckhdq)
TEST_INSN( &AllMask, 16,punpckhqdq)
TEST_INSN( &AllMask, 16,punpckhwd)
TEST_INSN( &AllMask, 16,punpcklbw)
TEST_INSN( &AllMask, 16,punpckldq)
TEST_INSN( &AllMask, 16,punpcklqdq)
TEST_INSN( &AllMask, 16,punpcklwd)
TEST_INSN( &AllMask, 16,pxor)
//TEST_INSN( &AllMask, PD,shufpd)
TEST_INSN( &AllMask, PD,sqrtpd)
TEST_INSN( &AllMask, SD,sqrtsd)
TEST_INSN( &AllMask, PD,subpd)
TEST_INSN( &AllMask, SD,subsd)
TEST_INSN( &AllMask, SD,ucomisd)
TEST_INSN( &AllMask, PD,unpckhpd)
TEST_INSN( &AllMask, PD,unpcklpd)
TEST_INSN( &AllMask, PD,xorpd)


int main ( int argc, char** argv )
{
   Int sse1 = 0, sse2 = 0;

   if (argc == 2 && 0==strcmp(argv[1], "sse1")) {
      sse1 = 1;
   }
   else
   if (argc == 2 && 0==strcmp(argv[1], "sse2")) {
      sse2 = 1;
   }
   else
   if (argc == 2 && 0==strcmp(argv[1], "all")) {
      sse1 = sse2 = 1;
   }
   else {
      fprintf(stderr, "usage: sse_memory [sse1|sse2|all]\n");
      return 0;
   }

   /* ------------------------ SSE1 ------------------------ */
   if (sse1) {
      do_addps();
      do_addss();
      do_andnps();
      do_andps();
      do_cmpeqps();
      do_cmpeqss();
      do_cmpleps();
      do_cmpless();
      do_cmpltps();
      do_cmpltss();
      do_cmpneqps();
      do_cmpneqss();
      do_cmpnleps();
      do_cmpnless();
      do_cmpnltps();
      do_cmpnltss();
      do_cmpordps();
      do_cmpordss();
      do_cmpunordps();
      do_cmpunordss();
      do_comiss();
      //TEST_INSN( &AllMask, 0,cvtpi2ps)
      //TEST_INSN( &AllMask, 0,cvtps2pi)
      //TEST_INSN( &AllMask, 0,cvtsi2ss)
      //TEST_INSN( &AllMask, 0,cvtss2si)
      //TEST_INSN( &AllMask, 0,cvttps2pi)
      //TEST_INSN( &AllMask, 0,cvttss2si)
      do_divps();
      do_divss();
      do_maxps();
      do_maxss();
      do_minps();
      do_minss();
      do_movaps();
      //TEST_INSN( &AllMask, 0,movhlps)
      //TEST_INSN( &AllMask, 0,movhps)
      //TEST_INSN( &AllMask, 0,movlhps)
      //TEST_INSN( &AllMask, 0,movlps)
      //TEST_INSN( &AllMask, 0,movmskps)
      //TEST_INSN( &AllMask, 0,movntps)
      //TEST_INSN( &AllMask, 0,movntq)
      do_movss();
      do_movups();
      do_mulps();
      do_mulss();
      do_orps();
      //TEST_INSN( &AllMask, 0,pavgb) -- dup with sse2?
      //TEST_INSN( &AllMask, 0,pavgw) -- dup with sse2?
      //TEST_INSN( &AllMask, 0,pextrw)
      //TEST_INSN( &AllMask, 0,pinsrw)
      //TEST_INSN( &AllMask, 0,pmaxsw) -- dup with sse2?
      //TEST_INSN( &AllMask, 0,pmaxub) -- dup with sse2?
      //TEST_INSN( &AllMask, 0,pminsw) -- dup with sse2?
      //TEST_INSN( &AllMask, 0,pminub) -- dup with sse2?
      //TEST_INSN( &AllMask, 0,pmovmskb)
      //TEST_INSN( &AllMask, 0,pmulhuw) -- dup with sse2?
      //do_psadbw();  -- XXXXXXXXXXXXXXXX sse2 (xmm variant) not implemented!
      //TEST_INSN( &AllMask, 0,pshufw)
      do_rcpps();
      do_rcpss();
      do_rsqrtps();
      do_rsqrtss();
      //TEST_INSN( &AllMask, PS,shufps)
      do_sqrtps();
      do_sqrtss();
      do_subps();
      do_subss();
      do_ucomiss();
      do_unpckhps();
      do_unpcklps();
      do_xorps();
   }

   /* ------------------------ SSE2 ------------------------ */
   if (sse2) {
      do_addpd();
      do_addsd();
      do_andnpd();
      do_andpd();
      do_cmpeqpd();
      do_cmpeqsd();
      do_cmplepd();
      do_cmplesd();
      do_cmpltpd();
      do_cmpltsd();
      do_cmpneqpd();
      do_cmpneqsd();
      do_cmpnlepd();
      do_cmpnlesd();
      do_cmpnltpd();
      do_cmpnltsd();
      do_cmpordpd();
      do_cmpordsd();
      do_cmpunordpd();
      do_cmpunordsd();
      do_comisd();
      do_cvtdq2pd();
      do_cvtdq2ps();
      do_cvtpd2dq();
      //TEST_INSN( &AllMask, 0,cvtpd2pi)
      do_cvtpd2ps();
      //TEST_INSN( &AllMask, 0,cvtpi2pd)
      do_cvtps2dq();
      do_cvtps2pd();
      //TEST_INSN( &AllMask, 0,cvtsd2si)
      do_cvtsd2ss();
      //TEST_INSN( &AllMask, 0,cvtsi2sd)
      do_cvtss2sd();
      do_cvttpd2dq();
      //TEST_INSN( &AllMask, 0,cvttpd2pi)
      do_cvttps2dq();
      //TEST_INSN( &AllMask, 0,cvttsd2si)
      do_divpd();
      do_divsd();
      do_maxpd();
      do_maxsd();
      do_minpd();
      do_minsd();
      do_movapd();
      //TEST_INSN( &AllMask, 8,movd)
      //TEST_INSN( &AllMask, 0,movdq2q)
      do_movdqa();
      do_movdqu();
      //TEST_INSN( &AllMask, 16,movhpd)
      //TEST_INSN( &AllMask, 16,movlpd)
      //TEST_INSN( &AllMask, 0,movmskpd)
      //TEST_INSN( &AllMask, 0,movntdq)
      //TEST_INSN( &AllMask, 0,movnti)
      //TEST_INSN( &AllMask, 0,movntpd)
      do_movq();
      //TEST_INSN( &AllMask, 0,movq2dq)
      do_movsd();
      do_movupd();
      do_mulpd();
      do_mulsd();
      do_orpd();
      do_packssdw();
      do_packsswb();
      do_packuswb();
      do_paddb();
      do_paddd();
      do_paddq();
      do_paddsb();
      do_paddsw();
      do_paddusb();
      do_paddusw();
      do_paddw();
      do_pand();
      do_pandn();
      do_pavgb();
      do_pavgw();
      do_pcmpeqb();
      do_pcmpeqd();
      do_pcmpeqw();
      do_pcmpgtb();
      do_pcmpgtd();
      do_pcmpgtw();
      //TEST_INSN( &AllMask, 16,pextrw)
      //TEST_INSN( &AllMask, 16,pinsrw)
      do_pmaxsw();
      do_pmaxub();
      do_pminsw();
      do_pminub();
      //TEST_INSN( &AllMask, 0,pmovmskb)
      do_pmulhuw();
      do_pmulhw();
      do_pmullw();
      do_pmuludq();
      do_por();
      //TEST_INSN( &AllMask, 16,pshufd)
      //TEST_INSN( &AllMask, 16,pshufhw)
      //TEST_INSN( &AllMask, 16,pshuflw)
      do_pslld();
      //TEST_INSN( &AllMask, 16,pslldq)
      do_psllq();
      do_psllw();
      do_psrad();
      do_psraw();
      do_psrld();
      //TEST_INSN( &AllMask, 16,psrldq)
      do_psrlq();
      do_psrlw();
      do_psubb();
      do_psubd();
      do_psubq();
      do_psubsb();
      do_psubsw();
      do_psubusb();
      do_psubusw();
      do_psubw();
      do_punpckhbw();
      do_punpckhdq();
      do_punpckhqdq();
      do_punpckhwd();
      do_punpcklbw();
      do_punpckldq();
      do_punpcklqdq();
      do_punpcklwd();
      do_pxor();
      //TEST_INSN( &AllMask, PD,shufpd)
      do_sqrtpd();
      do_sqrtsd();
      do_subpd();
      do_subsd();
      do_ucomisd();
      do_unpckhpd();
      do_unpcklpd();
      do_xorpd();
   }

   return 0;   
}

