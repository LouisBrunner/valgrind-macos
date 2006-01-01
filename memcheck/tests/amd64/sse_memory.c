
/* A program to test that SSE/SSE2 insns do not read memory they
   should not.  Covers insns of the form OP %xmm, %xmm and OP memory,
   %xmm only. */

#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include <malloc.h>
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

static void showRR ( char* op, RRArgs* rra )
{
   printf("r %10s ", op);
   showV128(&rra->arg1);
   printf(" ");
   showV128(&rra->arg2);
   printf(" ");
   showV128(&rra->res);
   printf("\n");
}

static void showRM ( char* op, RMArgs* rra, UChar* mem, Int nMem )
{
   Int i;
   assert(nMem == 4 || nMem == 8 || nMem == 16 || nMem==0);
   printf("m %10s ", op);
   for (i = 0; i < nMem; i++)
      printf("%02x", (Int)mem[i]);
   printf(" ");
   showV128(&rra->arg1);
   printf(" ");
   showV128(&rra->res);
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


#define TEST_INSN(mem_size,insn)           \
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
      showRR(#insn, &rargs);               \
   }                                       \
   for (i = 0; i < 5; i++) {               \
      randRMArgs(&margs);                  \
      buf = memalign(16,mem_size);         \
      randomise(buf,mem_size);             \
      r_m_##insn(&margs,buf);              \
      showRM(#insn, &margs, buf, mem_size);\
      free(buf);                           \
   }                                       \
}

#define PD 16
#define SD 8
#define PS 16
#define SS 4

/* ------------------------ SSE1 ------------------------ */
TEST_INSN(PS,addps)
TEST_INSN(SS,addss)
TEST_INSN(PS,andnps)
TEST_INSN(PS,andps)
TEST_INSN(PS,cmpeqps)
TEST_INSN(SS,cmpeqss)
TEST_INSN(PS,cmpleps)
TEST_INSN(SS,cmpless)
TEST_INSN(PS,cmpltps)
TEST_INSN(SS,cmpltss)
TEST_INSN(PS,cmpneqps)
TEST_INSN(SS,cmpneqss)
TEST_INSN(PS,cmpnleps)
TEST_INSN(SS,cmpnless)
TEST_INSN(PS,cmpnltps)
TEST_INSN(SS,cmpnltss)
TEST_INSN(PS,cmpordps)
TEST_INSN(SS,cmpordss)
TEST_INSN(PS,cmpunordps)
TEST_INSN(SS,cmpunordss)
TEST_INSN(SS,comiss)
//TEST_INSN(0,cvtpi2ps)
//TEST_INSN(0,cvtps2pi)
//TEST_INSN(0,cvtsi2ss)
//TEST_INSN(0,cvtss2si)
//TEST_INSN(0,cvttps2pi)
//TEST_INSN(0,cvttss2si)
TEST_INSN(PS,divps)
TEST_INSN(SS,divss)
TEST_INSN(PS,maxps)
TEST_INSN(SS,maxss)
TEST_INSN(PS,minps)
TEST_INSN(SS,minss)
TEST_INSN(16,movaps)
//TEST_INSN(0,movhlps)
//TEST_INSN(0,movhps)
//TEST_INSN(0,movlhps)
//TEST_INSN(0,movlps)
//TEST_INSN(0,movmskps)
//TEST_INSN(0,movntps)
//TEST_INSN(0,movntq)
TEST_INSN(4,movss)
TEST_INSN(16,movups)
TEST_INSN(PS,mulps)
TEST_INSN(SS,mulss)
TEST_INSN(PS,orps)
//TEST_INSN(0,pavgb) -- dup with sse2?
//TEST_INSN(0,pavgw) -- dup with sse2?
//TEST_INSN(0,pextrw)
//TEST_INSN(0,pinsrw)
//TEST_INSN(0,pmaxsw) -- dup with sse2?
//TEST_INSN(0,pmaxub) -- dup with sse2?
//TEST_INSN(0,pminsw) -- dup with sse2?
//TEST_INSN(0,pminub) -- dup with sse2?
//TEST_INSN(0,pmovmskb)
//TEST_INSN(0,pmulhuw) -- dup with sse2?
TEST_INSN(16,psadbw) // -- XXXXXXXXXXXXXXXX sse2 (xmm variant) not implemented!
//TEST_INSN(0,pshufw)
TEST_INSN(PS,rcpps)
TEST_INSN(SS,rcpss)
TEST_INSN(PS,rsqrtps)
TEST_INSN(SS,rsqrtss)
//TEST_INSN(PS,shufps)
TEST_INSN(PS,sqrtps)
TEST_INSN(SS,sqrtss)
TEST_INSN(PS,subps)
TEST_INSN(SS,subss)
TEST_INSN(SS,ucomiss)
TEST_INSN(PS,unpckhps)
TEST_INSN(PS,unpcklps)
TEST_INSN(PS,xorps)


/* ------------------------ SSE2 ------------------------ */
TEST_INSN(PD,addpd)
TEST_INSN(SD,addsd)
TEST_INSN(PD,andnpd)
TEST_INSN(PD,andpd)
TEST_INSN(PD,cmpeqpd)
TEST_INSN(SD,cmpeqsd)
TEST_INSN(PD,cmplepd)
TEST_INSN(SD,cmplesd)
TEST_INSN(PD,cmpltpd)
TEST_INSN(SD,cmpltsd)
TEST_INSN(PD,cmpneqpd)
TEST_INSN(SD,cmpneqsd)
TEST_INSN(PD,cmpnlepd)
TEST_INSN(SD,cmpnlesd)
TEST_INSN(PD,cmpnltpd)
TEST_INSN(SD,cmpnltsd)
TEST_INSN(PD,cmpordpd)
TEST_INSN(SD,cmpordsd)
TEST_INSN(PD,cmpunordpd)
TEST_INSN(SD,cmpunordsd)
TEST_INSN(SD,comisd)
TEST_INSN(8,cvtdq2pd)
TEST_INSN(16,cvtdq2ps)
TEST_INSN(16,cvtpd2dq)
//TEST_INSN(0,cvtpd2pi)
TEST_INSN(16,cvtpd2ps)   /* reads 16 */
//TEST_INSN(0,cvtpi2pd)
TEST_INSN(16,cvtps2dq)  /* reads 16 */
TEST_INSN(8,cvtps2pd)   /* reads 8 */
//TEST_INSN(0,cvtsd2si)
TEST_INSN(SD,cvtsd2ss)   /* reads SD */
//TEST_INSN(0,cvtsi2sd)
TEST_INSN(SS,cvtss2sd)   /* reads SS */
TEST_INSN(16,cvttpd2dq)
//TEST_INSN(0,cvttpd2pi)
TEST_INSN(16,cvttps2dq)
//TEST_INSN(0,cvttsd2si)
TEST_INSN(PD,divpd)
TEST_INSN(SD,divsd)
TEST_INSN(PD,maxpd)
TEST_INSN(SD,maxsd)
TEST_INSN(PD,minpd)
TEST_INSN(SD,minsd)
TEST_INSN(PD,movapd)
//TEST_INSN(8,movd)
//TEST_INSN(0,movdq2q)
TEST_INSN(16,movdqa)
TEST_INSN(16,movdqu)
//TEST_INSN(16,movhpd)
//TEST_INSN(16,movlpd)
//TEST_INSN(0,movmskpd)
//TEST_INSN(0,movntdq)
//TEST_INSN(0,movnti)
//TEST_INSN(0,movntpd)
TEST_INSN(8,movq)
//TEST_INSN(0,movq2dq)
TEST_INSN(8,movsd)
TEST_INSN(16,movupd)
TEST_INSN(PD,mulpd)
TEST_INSN(SD,mulsd)
TEST_INSN(PD,orpd)
TEST_INSN(16,packssdw)
TEST_INSN(16,packsswb)
TEST_INSN(16,packuswb)
TEST_INSN(16,paddb)
TEST_INSN(16,paddd)
TEST_INSN(16,paddq)
TEST_INSN(16,paddsb)
TEST_INSN(16,paddsw)
TEST_INSN(16,paddusb)
TEST_INSN(16,paddusw)
TEST_INSN(16,paddw)
TEST_INSN(16,pand)
TEST_INSN(16,pandn)
TEST_INSN(16,pavgb)
TEST_INSN(16,pavgw)
TEST_INSN(16,pcmpeqb)
TEST_INSN(16,pcmpeqd)
TEST_INSN(16,pcmpeqw)
TEST_INSN(16,pcmpgtb)
TEST_INSN(16,pcmpgtd)
TEST_INSN(16,pcmpgtw)
//TEST_INSN(16,pextrw)
//TEST_INSN(16,pinsrw)
TEST_INSN(16,pmaxsw)
TEST_INSN(16,pmaxub)
TEST_INSN(16,pminsw)
TEST_INSN(16,pminub)
//TEST_INSN(0,pmovmskb)
TEST_INSN(16,pmulhuw)
TEST_INSN(16,pmulhw)
TEST_INSN(16,pmullw)
TEST_INSN(16,pmuludq)
TEST_INSN(16,por)
//TEST_INSN(16,pshufd)
//TEST_INSN(16,pshufhw)
//TEST_INSN(16,pshuflw)
TEST_INSN(16,pslld)
//TEST_INSN(16,pslldq)
TEST_INSN(16,psllq)
TEST_INSN(16,psllw)
TEST_INSN(16,psrad)
TEST_INSN(16,psraw)
TEST_INSN(16,psrld)
//TEST_INSN(16,psrldq)
TEST_INSN(16,psrlq)
TEST_INSN(16,psrlw)
TEST_INSN(16,psubb)
TEST_INSN(16,psubd)
TEST_INSN(16,psubq)
TEST_INSN(16,psubsb)
TEST_INSN(16,psubsw)
TEST_INSN(16,psubusb)
TEST_INSN(16,psubusw)
TEST_INSN(16,psubw)
TEST_INSN(16,punpckhbw)
TEST_INSN(16,punpckhdq)
TEST_INSN(16,punpckhqdq)
TEST_INSN(16,punpckhwd)
TEST_INSN(16,punpcklbw)
TEST_INSN(16,punpckldq)
TEST_INSN(16,punpcklqdq)
TEST_INSN(16,punpcklwd)
TEST_INSN(16,pxor)
//TEST_INSN(PD,shufpd)
TEST_INSN(PD,sqrtpd)
TEST_INSN(SD,sqrtsd)
TEST_INSN(PD,subpd)
TEST_INSN(SD,subsd)
TEST_INSN(SD,ucomisd)
TEST_INSN(PD,unpckhpd)
TEST_INSN(PD,unpcklpd)
TEST_INSN(PD,xorpd)


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
      //TEST_INSN(0,cvtpi2ps)
      //TEST_INSN(0,cvtps2pi)
      //TEST_INSN(0,cvtsi2ss)
      //TEST_INSN(0,cvtss2si)
      //TEST_INSN(0,cvttps2pi)
      //TEST_INSN(0,cvttss2si)
      do_divps();
      do_divss();
      do_maxps();
      do_maxss();
      do_minps();
      do_minss();
      do_movaps();
      //TEST_INSN(0,movhlps)
      //TEST_INSN(0,movhps)
      //TEST_INSN(0,movlhps)
      //TEST_INSN(0,movlps)
      //TEST_INSN(0,movmskps)
      //TEST_INSN(0,movntps)
      //TEST_INSN(0,movntq)
      do_movss();
      do_movups();
      do_mulps();
      do_mulss();
      do_orps();
      //TEST_INSN(0,pavgb) -- dup with sse2?
      //TEST_INSN(0,pavgw) -- dup with sse2?
      //TEST_INSN(0,pextrw)
      //TEST_INSN(0,pinsrw)
      //TEST_INSN(0,pmaxsw) -- dup with sse2?
      //TEST_INSN(0,pmaxub) -- dup with sse2?
      //TEST_INSN(0,pminsw) -- dup with sse2?
      //TEST_INSN(0,pminub) -- dup with sse2?
      //TEST_INSN(0,pmovmskb)
      //TEST_INSN(0,pmulhuw) -- dup with sse2?
      //do_psadbw();  -- XXXXXXXXXXXXXXXX sse2 (xmm variant) not implemented!
      //TEST_INSN(0,pshufw)
      do_rcpps();
      do_rcpss();
      do_rsqrtps();
      do_rsqrtss();
      //TEST_INSN(PS,shufps)
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
      //TEST_INSN(0,cvtpd2pi)
      do_cvtpd2ps();
      //TEST_INSN(0,cvtpi2pd)
      do_cvtps2dq();
      do_cvtps2pd();
      //TEST_INSN(0,cvtsd2si)
      do_cvtsd2ss();
      //TEST_INSN(0,cvtsi2sd)
      do_cvtss2sd();
      do_cvttpd2dq();
      //TEST_INSN(0,cvttpd2pi)
      do_cvttps2dq();
      //TEST_INSN(0,cvttsd2si)
      do_divpd();
      do_divsd();
      do_maxpd();
      do_maxsd();
      do_minpd();
      do_minsd();
      do_movapd();
      //TEST_INSN(8,movd)
      //TEST_INSN(0,movdq2q)
      do_movdqa();
      do_movdqu();
      //TEST_INSN(16,movhpd)
      //TEST_INSN(16,movlpd)
      //TEST_INSN(0,movmskpd)
      //TEST_INSN(0,movntdq)
      //TEST_INSN(0,movnti)
      //TEST_INSN(0,movntpd)
      do_movq();
      //TEST_INSN(0,movq2dq)
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
      //TEST_INSN(16,pextrw)
      //TEST_INSN(16,pinsrw)
      do_pmaxsw();
      do_pmaxub();
      do_pminsw();
      do_pminub();
      //TEST_INSN(0,pmovmskb)
      do_pmulhuw();
      do_pmulhw();
      do_pmullw();
      do_pmuludq();
      do_por();
      //TEST_INSN(16,pshufd)
      //TEST_INSN(16,pshufhw)
      //TEST_INSN(16,pshuflw)
      do_pslld();
      //TEST_INSN(16,pslldq)
      do_psllq();
      do_psllw();
      do_psrad();
      do_psraw();
      do_psrld();
      //TEST_INSN(16,psrldq)
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
      //TEST_INSN(PD,shufpd)
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

