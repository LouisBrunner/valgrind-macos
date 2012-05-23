      /* VMOVSD m64, xmm1 = VEX.LIG.F2.0F.WIG 10 /r */
      /* VMOVSS m32, xmm1 = VEX.LIG.F3.0F.WIG 10 /r */
      /* VMOVSD xmm1, m64 = VEX.LIG.F2.0F.WIG 11 /r */
      /* VMOVSS xmm1, m64 = VEX.LIG.F3.0F.WIG 11 /r */
      /* VMOVUPD xmm1, xmm2/m128 = VEX.128.66.0F.WIG 11 /r */
      /* VMOVAPD xmm2/m128, xmm1 = VEX.128.66.0F.WIG 28 /r */
      /* VMOVAPD ymm2/m256, ymm1 = VEX.256.66.0F.WIG 28 /r */
      /* VMOVAPS xmm2/m128, xmm1 = VEX.128.0F.WIG 28 /r */
      /* VMOVAPS xmm1, xmm2/m128 = VEX.128.0F.WIG 29 /r */
      /* VMOVAPD xmm1, xmm2/m128 = VEX.128.66.0F.WIG 29 /r */

/* . VCVTSI2SD r/m32, xmm2, xmm1 = VEX.NDS.LIG.F2.0F.W0 2A /r */
/* . VCVTSI2SD r/m64, xmm2, xmm1 = VEX.NDS.LIG.F2.0F.W1 2A /r */
/* . VCVTSI2SS r/m64, xmm2, xmm1 = VEX.NDS.LIG.F3.0F.W1 2A /r */
/* . VCVTTSD2SI xmm1/m64, r32 = VEX.LIG.F2.0F.W0 2C /r */
/* VCVTTSD2SI xmm1/m64, r64 = VEX.LIG.F2.0F.W1 2C /r */
/* VUCOMISD xmm2/m64, xmm1 = VEX.LIG.66.0F.WIG 2E /r */
/* VUCOMISS xmm2/m32, xmm1 = VEX.LIG.0F.WIG 2E /r */
/* . VSQRTSD xmm3/m64(E), xmm2(V), xmm1(G) = VEX.NDS.LIG.F2.0F.WIG 51 /r */
/* VANDPD r/m, rV, r ::: r = rV & r/m (MVR format) */
/* VANDNPD r/m, rV, r ::: r = (not rV) & r/m (MVR format) */
/* VORPD r/m, rV, r ::: r = rV ^ r/m (MVR format) */
/* VXORPD r/m, rV, r ::: r = rV ^ r/m (MVR format) */
/* VXORPS r/m, rV, r ::: r = rV ^ r/m (MVR format) */
/* VADDSD xmm3/m64, xmm2, xmm1 = VEX.NDS.LIG.F0.0F.WIG 58 /r */
/* VMULSD xmm3/m64, xmm2, xmm1 = VEX.NDS.LIG.F0.0F.WIG 59 /r */
/* VCVTPS2PD xmm2/m64, xmm1 = VEX.128.0F.WIG 5A /r */
/* VSUBSD xmm3/m64, xmm2, xmm1 = VEX.NDS.LIG.F2.0F.WIG 5C /r */
/* VMINSD xmm3/m64, xmm2, xmm1 = VEX.NDS.LIG.F2.0F.WIG 5D /r */
/* VDIVSD xmm3/m64, xmm2, xmm1 = VEX.NDS.LIG.F2.0F.WIG 5E /r */
/* VMAXSD xmm3/m64, xmm2, xmm1 = VEX.NDS.LIG.F2.0F.WIG 5F /r */

      /* VMOVD r32/m32, xmm1 = VEX.128.66.0F.W0 6E */
      /* VMOVDQA ymm2/m256, ymm1 = VEX.256.66.0F.WIG 6F */
      /* VMOVDQA xmm2/m128, xmm1 = VEX.128.66.0F.WIG 6F */
      /* VMOVDQU xmm2/m128, xmm1 = VEX.128.F3.0F.WIG 6F */

/* VPSHUFD imm8, xmm2/m128, xmm1 = VEX.128.66.0F.WIG 70 /r ib */
/* VPSLLD imm8, xmm2, xmm1 = VEX.128.66.0F.WIG 72 /6 ib */
/* VPSRLDQ VEX.NDD.128.66.0F.WIG 73 /3 ib */
/* VPCMPEQD r/m, rV, r ::: r = rV `eq-by-32s` r/m (MVR format) */

      /* VMOVDQA ymm1, ymm2/m256 = VEX.256.66.0F.WIG 7F */
      /* VMOVDQA xmm1, xmm2/m128 = VEX.128.66.0F.WIG 7F */
      /* VMOVDQU xmm1, xmm2/m128 = VEX.128.F3.0F.WIG 7F */

/* . VCMPSD xmm3/m64(E=argL), xmm2(V=argR), xmm1(G) */
/* . VPOR = VEX.NDS.128.66.0F.WIG EB /r */
/* . VPXOR = VEX.NDS.128.66.0F.WIG EF /r */
/* . VPSUBB = VEX.NDS.128.66.0F.WIG EF /r */
/* . VPSUBD = VEX.NDS.128.66.0F.WIG FE /r */
/* . VPADDD = VEX.NDS.128.66.0F.WIG FE /r */
/* . VPSHUFB r/m, rV, r ::: r = shuf(rV, r/m) (MVR format) */
/* . VPMOVZXBW = VEX.128.66.0F38.WIG 30 /r */
/* . VPMOVZXWD = VEX.128.66.0F38.WIG 33 /r */
/* . VPMINSD = VEX.NDS.128.66.0F38.WIG 39 /r */
/* . VPMAXSD = VEX.NDS.128.66.0F38.WIG 3D /r */
      /* VPEXTRD imm8, r32/m32, xmm2 */
      /* VINSERTF128 r/m, rV, rD */
      /* VEXTRACTF128 rS, r/m */

/* . VPBLENDVB xmmG, xmmE/memE, xmmV, xmmIS4 */
      /* VEX.128.F2.0F.WIG /12 r = MOVDDUP xmm2/m64, xmm1 */
     /* VCVTPD2PS xmm2/m128, xmm1 = VEX.128.66.0F.WIG 5A /r */
/* . VMULSS xmm3/m32, xmm2, xmm1 = VEX.NDS.LIG.F3.0F.WIG 59 /r */
/* . VSUBSS xmm3/m32, xmm2, xmm1 = VEX.NDS.LIG.F3.0F.WIG 5C /r */
/* . VADDSS xmm3/m32, xmm2, xmm1 = VEX.NDS.LIG.F3.0F.WIG 58 /r */
/* . VDIVSS xmm3/m32, xmm2, xmm1 = VEX.NDS.LIG.F3.0F.WIG 5E /r */
/* . VUNPCKLPS xmm3/m128, xmm2, xmm1 = VEX.NDS.128.0F.WIG 14 /r */
/* . VCVTSI2SS r/m32, xmm2, xmm1 = VEX.NDS.LIG.F3.0F.W0 2A /r */
/* . VANDPS = VEX.NDS.128.0F.WIG 54 /r */
/* . VMINSS xmm3/m32, xmm2, xmm1 = VEX.NDS.LIG.F3.0F.WIG 5D /r */
/* . VMAXSS xmm3/m32, xmm2, xmm1 = VEX.NDS.LIG.F3.0F.WIG 5F /r */

/* really needs testing -- Intel docs don't make sense */
/* VMOVQ xmm2/m64, xmm1 = VEX.128.F3.0F.W0 */

/* really needs testing -- Intel docs don't make sense */
/* of the form vmovq  %xmm0,-0x8(%rsp) */

/* VCMPSS xmm3/m32(E=argL), xmm2(V=argR), xmm1(G) */
/* . VANDNPS = VEX.NDS.128.0F.WIG 55 /r */
/* . VORPS = VEX.NDS.128.0F.WIG 56 /r */

#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include <malloc.h>

typedef  unsigned char           UChar;
typedef  unsigned int            UInt;
typedef  unsigned long int       UWord;
typedef  unsigned long long int  ULong;

#define IS_32_ALIGNED(_ptr) (0 == (0x1F & (UWord)(_ptr)))

typedef  union { UChar u8[32];  UInt u32[8];  }  YMM;

typedef  struct {  YMM a1; YMM a2; YMM a3; YMM a4; ULong u64; }  Block;

void showYMM ( YMM* vec )
{
   int i;
   assert(IS_32_ALIGNED(vec));
   for (i = 31; i >= 0; i--) {
      printf("%02x", (UInt)vec->u8[i]);
      if (i > 0 && 0 == ((i+0) & 7)) printf(".");
   }
}

void showBlock ( char* msg, Block* block )
{
   printf("  %s\n", msg);
   printf("    "); showYMM(&block->a1); printf("\n");
   printf("    "); showYMM(&block->a2); printf("\n");
   printf("    "); showYMM(&block->a3); printf("\n");
   printf("    "); showYMM(&block->a4); printf("\n");
   printf("    %016llx\n", block->u64);
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


/* Generate a function test_NAME, that tests the given insn, in both
   its mem and reg forms.  The reg form of the insn may mention, as
   operands only %ymm6, %ymm7, %ymm8, %ymm9 and %r14.  The mem form of
   the insn may mention as operands only (%rax), %ymm7, %ymm8, %ymm9
   and %r14. */

#define GEN_test_RandM(_name, _reg_form, _mem_form)   \
    \
    __attribute__ ((noinline)) static void test_##_name ( void )   \
    { \
       Block* b = memalign(32, sizeof(Block)); \
       randBlock(b); \
       printf("%s(reg)\n", #_name); \
       showBlock("before", b); \
       __asm__ __volatile__( \
          "vmovdqa   0(%0),%%ymm7"  "\n\t" \
          "vmovdqa  32(%0),%%ymm8"  "\n\t" \
          "vmovdqa  64(%0),%%ymm6"  "\n\t" \
          "vmovdqa  96(%0),%%ymm9"  "\n\t" \
          "movq    128(%0),%%r14"   "\n\t" \
          _reg_form   "\n\t" \
          "vmovdqa %%ymm7,  0(%0)"  "\n\t" \
          "vmovdqa %%ymm8, 32(%0)"  "\n\t" \
          "vmovdqa %%ymm6, 64(%0)"  "\n\t" \
          "vmovdqa %%ymm9, 96(%0)"  "\n\t" \
          "movq    %%r14, 128(%0)"  "\n\t" \
          : /*OUT*/  \
          : /*IN*/"r"(b) \
          : /*TRASH*/"xmm7","xmm8","xmm6","xmm9","r14","memory","cc" \
       ); \
       showBlock("after", b); \
       randBlock(b); \
       printf("%s(mem)\n", #_name); \
       showBlock("before", b); \
       __asm__ __volatile__( \
          "leaq      0(%0),%%rax"  "\n\t" \
          "vmovdqa  32(%0),%%ymm8"  "\n\t" \
          "vmovdqa  64(%0),%%ymm7"  "\n\t" \
          "vmovdqa  96(%0),%%ymm9"  "\n\t" \
          "movq    128(%0),%%r14"   "\n\t" \
          _mem_form   "\n\t" \
          "vmovdqa %%ymm8, 32(%0)"  "\n\t" \
          "vmovdqa %%ymm7, 64(%0)"  "\n\t" \
          "vmovdqa %%ymm9, 96(%0)"  "\n\t" \
          "movq    %%r14, 128(%0)"  "\n\t" \
          : /*OUT*/  \
          : /*IN*/"r"(b) \
          : /*TRASH*/"xmm8","xmm7","xmm9","r14","rax","memory","cc" \
       ); \
       showBlock("after", b); \
       printf("\n"); \
       free(b); \
    }

#define GEN_test_Ronly(_name, _reg_form) \
   GEN_test_RandM(_name, _reg_form, "")
#define GEN_test_Monly(_name, _mem_form) \
   GEN_test_RandM(_name, "", _mem_form)


GEN_test_RandM(VPOR_128,
               "vpor %%xmm6,  %%xmm8, %%xmm7",
               "vpor (%%rax), %%xmm8, %%xmm7")

GEN_test_RandM(VPXOR_128,
               "vpxor %%xmm6,  %%xmm8, %%xmm7",
               "vpxor (%%rax), %%xmm8, %%xmm7")

GEN_test_RandM(VPSUBB_128,
               "vpsubb %%xmm6,  %%xmm8, %%xmm7",
               "vpsubb (%%rax), %%xmm8, %%xmm7")

GEN_test_RandM(VPSUBD_128,
               "vpsubd %%xmm6,  %%xmm8, %%xmm7",
               "vpsubd (%%rax), %%xmm8, %%xmm7")

GEN_test_RandM(VPADDD_128,
               "vpaddd %%xmm6,  %%xmm8, %%xmm7",
               "vpaddd (%%rax), %%xmm8, %%xmm7")

GEN_test_RandM(VPMOVZXWD_128,
               "vpmovzxwd %%xmm6,  %%xmm8",
               "vpmovzxwd (%%rax), %%xmm8")

GEN_test_RandM(VPMOVZXBW_128,
               "vpmovzxbw %%xmm6,  %%xmm8",
               "vpmovzxbw (%%rax), %%xmm8")

GEN_test_RandM(VPBLENDVB_128,
               "vpblendvb %%xmm9, %%xmm6,  %%xmm8, %%xmm7",
               "vpblendvb %%xmm9, (%%rax), %%xmm8, %%xmm7")

GEN_test_RandM(VPMINSD_128,
               "vpminsd %%xmm6,  %%xmm8, %%xmm7",
               "vpminsd (%%rax), %%xmm8, %%xmm7")

GEN_test_RandM(VPMAXSD_128,
               "vpmaxsd %%xmm6,  %%xmm8, %%xmm7",
               "vpmaxsd (%%rax), %%xmm8, %%xmm7")

GEN_test_RandM(VANDPD_128,
               "vandpd %%xmm6,  %%xmm8, %%xmm7",
               "vandpd (%%rax), %%xmm8, %%xmm7")

GEN_test_RandM(VCVTSI2SD_32,
               "vcvtsi2sdl %%r14d,  %%xmm8, %%xmm7",
               "vcvtsi2sdl (%%rax), %%xmm8, %%xmm7")

GEN_test_RandM(VCVTSI2SD_64,
               "vcvtsi2sdq %%r14,   %%xmm8, %%xmm7",
               "vcvtsi2sdq (%%rax), %%xmm8, %%xmm7")

GEN_test_RandM(VCVTSI2SS_64,
               "vcvtsi2ssq %%r14,   %%xmm8, %%xmm7",
               "vcvtsi2ssq (%%rax), %%xmm8, %%xmm7")

GEN_test_RandM(VCVTTSD2SI_32,
               "vcvttsd2si %%xmm8,  %%r14d",
               "vcvttsd2si (%%rax), %%r14d")

GEN_test_RandM(VPSHUFB_128,
               "vpshufb %%xmm6,  %%xmm8, %%xmm7",
               "vpshufb (%%rax), %%xmm8, %%xmm7")

GEN_test_RandM(VCMPSD_128_0x0,
               "vcmpsd $0, %%xmm6,  %%xmm8, %%xmm7",
               "vcmpsd $0, (%%rax), %%xmm8, %%xmm7")

GEN_test_RandM(VCMPSD_128_0xD,
               "vcmpsd $0xd, %%xmm6,  %%xmm8, %%xmm7",
               "vcmpsd $0xd, (%%rax), %%xmm8, %%xmm7")

GEN_test_RandM(VSQRTSD_128,
               "vsqrtsd %%xmm6,  %%xmm8, %%xmm7",
               "vsqrtsd (%%rax), %%xmm8, %%xmm7")

GEN_test_RandM(VORPS_128,
               "vorps %%xmm6,  %%xmm8, %%xmm7",
               "vorps (%%rax), %%xmm8, %%xmm7")

GEN_test_RandM(VANDNPS_128,
               "vandnps %%xmm6,  %%xmm8, %%xmm7",
               "vandnps (%%rax), %%xmm8, %%xmm7")

GEN_test_RandM(VMAXSS_128,
               "vmaxss %%xmm6,  %%xmm8, %%xmm7",
               "vmaxss (%%rax), %%xmm8, %%xmm7")

GEN_test_RandM(VMINSS_128,
               "vminss %%xmm6,  %%xmm8, %%xmm7",
               "vminss (%%rax), %%xmm8, %%xmm7")

GEN_test_RandM(VANDPS_128,
               "vandps %%xmm6,  %%xmm8, %%xmm7",
               "vandps (%%rax), %%xmm8, %%xmm7")

GEN_test_RandM(VCVTSI2SS_128,
               "vcvtsi2ssl %%r14d,  %%xmm8, %%xmm7",
               "vcvtsi2ssl (%%rax), %%xmm8, %%xmm7")

GEN_test_RandM(VUNPCKLPS_128,
               "vunpcklps %%xmm6,  %%xmm8, %%xmm7",
               "vunpcklps (%%rax), %%xmm8, %%xmm7")

GEN_test_RandM(VDIVSS_128,
               "vdivss %%xmm6,  %%xmm8, %%xmm7",
               "vdivss (%%rax), %%xmm8, %%xmm7")

GEN_test_RandM(VADDSS_128,
               "vaddss %%xmm6,  %%xmm8, %%xmm7",
               "vaddss (%%rax), %%xmm8, %%xmm7")

GEN_test_RandM(VSUBSS_128,
               "vsubss %%xmm6,  %%xmm8, %%xmm7",
               "vsubss (%%rax), %%xmm8, %%xmm7")

GEN_test_RandM(VMULSS_128,
               "vmulss %%xmm6,  %%xmm8, %%xmm7",
               "vmulss (%%rax), %%xmm8, %%xmm7")

GEN_test_RandM(VPUNPCKLBW_128,
               "vpunpcklbw %%xmm6,  %%xmm8, %%xmm7",
               "vpunpcklbw (%%rax), %%xmm8, %%xmm7")

GEN_test_RandM(VPUNPCKHBW_128,
               "vpunpckhbw %%xmm6,  %%xmm8, %%xmm7",
               "vpunpckhbw (%%rax), %%xmm8, %%xmm7")

GEN_test_RandM(VCVTTSS2SI_32,
               "vcvttss2si %%xmm8,  %%r14d",
               "vcvttss2si (%%rax), %%r14d")

GEN_test_RandM(VMOVQ_XMMorMEM64_to_XMM,
               "vmovq %%xmm7,  %%xmm8",
               "vmovq (%%rax), %%xmm8")

/* NB tests the reg form only */
GEN_test_Ronly(VMOVQ_XMM_to_IREG64,
               "vmovq %%xmm7, %%r14")

/* This insn only exists in the reg-reg-reg form. */
GEN_test_Ronly(VMOVHLPS_128,
               "vmovhlps %%xmm6, %%xmm8, %%xmm7")

GEN_test_RandM(VPABSD_128,
               "vpabsd %%xmm6,  %%xmm8",
               "vpabsd (%%rax), %%xmm8")

/* This insn only exists in the reg-reg-reg form. */
GEN_test_Ronly(VMOVLHPS_128,
               "vmovlhps %%xmm6, %%xmm8, %%xmm7")

GEN_test_Monly(VMOVNTDQ_128,
               "vmovntdq %%xmm8, (%%rax)")

GEN_test_RandM(VMOVUPS_XMM_to_XMMorMEM,
               "vmovups %%xmm8, %%xmm7",
               "vmovups %%xmm9, (%%rax)")

GEN_test_RandM(VMOVQ_IREGorMEM64_to_XMM,
               "vmovq %%r14, %%xmm7",
               "vmovq (%%rax), %%xmm9")

/* Comment duplicated above, for convenient reference:
   Allowed operands in test insns:
     Reg form:  %ymm6,  %ymm7, %ymm8, %ymm9 and %r14.
     Mem form:  (%rax), %ymm7, %ymm8, %ymm9 and %r14.
   Imm8 etc fields are also allowed, where they make sense.
*/

int main ( void )
{
   test_VMOVQ_IREGorMEM64_to_XMM();
   test_VMOVUPS_XMM_to_XMMorMEM();
   test_VMOVNTDQ_128();
   test_VMOVLHPS_128();
   test_VPABSD_128();
   test_VMOVHLPS_128();
   test_VMOVQ_XMM_to_IREG64();
   test_VMOVQ_XMMorMEM64_to_XMM();
   test_VCVTTSS2SI_32();
   test_VPUNPCKLBW_128();
   test_VPUNPCKHBW_128();
   test_VMULSS_128();
   test_VSUBSS_128();
   test_VADDSS_128();
   test_VDIVSS_128();
   test_VUNPCKLPS_128();
   test_VCVTSI2SS_128();
   test_VANDPS_128();
   test_VMINSS_128();
   test_VMAXSS_128();
   test_VANDNPS_128();
   test_VORPS_128();
   test_VSQRTSD_128();
   // test_VCMPSD_128_0xD(); BORKED
   test_VCMPSD_128_0x0();
   test_VPSHUFB_128();
   test_VCVTTSD2SI_32();
   test_VCVTSI2SS_64();
   test_VCVTSI2SD_64();
   test_VCVTSI2SD_32();
   test_VPOR_128();
   test_VPXOR_128();
   test_VPSUBB_128();
   test_VPSUBD_128();
   test_VPADDD_128();
   test_VPMOVZXBW_128();
   test_VPMOVZXWD_128();
   test_VPBLENDVB_128();
   test_VPMINSD_128();
   test_VPMAXSD_128();
   test_VANDPD_128();
   return 0;
}
