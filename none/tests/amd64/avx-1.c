
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
   and %r14.  It's OK for the insn to clobber ymm0, as this is needed
   for testing PCMPxSTRx. */

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
          : /*TRASH*/"xmm0","xmm7","xmm8","xmm6","xmm9","r14","memory","cc" \
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
          : /*TRASH*/"xmm0","xmm8","xmm7","xmm9","r14","rax","memory","cc" \
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

GEN_test_RandM(VCVTTSD2SI_64,
               "vcvttsd2si %%xmm8,  %%r14",
               "vcvttsd2si (%%rax), %%r14")

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

GEN_test_RandM(VPCMPESTRM_0x45_128,
               "vpcmpestrm $0x45, %%xmm7, %%xmm8;  movapd %%xmm0, %%xmm9",
               "vpcmpestrm $0x45, (%%rax), %%xmm8; movapd %%xmm0, %%xmm9")

/* NB tests the reg form only */
GEN_test_Ronly(VMOVD_XMM_to_IREG32,
               "vmovd %%xmm7, %%r14d")

GEN_test_RandM(VCVTSD2SS_128,
               "vcvtsd2ss %%xmm9,  %%xmm8, %%xmm7",
               "vcvtsd2ss (%%rax), %%xmm8, %%xmm7")

GEN_test_RandM(VCVTSS2SD_128,
               "vcvtss2sd %%xmm9,  %%xmm8, %%xmm7",
               "vcvtss2sd (%%rax), %%xmm8, %%xmm7")

GEN_test_RandM(VPACKUSWB_128,
               "vpackuswb %%xmm9,  %%xmm8, %%xmm7",
               "vpackuswb (%%rax), %%xmm8, %%xmm7")

GEN_test_RandM(VCVTTSS2SI_64,
               "vcvttss2si %%xmm8,  %%r14",
               "vcvttss2si (%%rax), %%r14")

GEN_test_Ronly(VPMOVMSKB_128,
               "vpmovmskb %%xmm8, %%r14")

GEN_test_RandM(VPAND_128,
               "vpand %%xmm9,  %%xmm8, %%xmm7",
               "vpand (%%rax), %%xmm8, %%xmm7")

GEN_test_Monly(VMOVHPD_128_StoreForm,
               "vmovhpd %%xmm8, (%%rax)")

GEN_test_RandM(VPCMPEQB_128,
               "vpcmpeqb %%xmm9,  %%xmm8, %%xmm7",
               "vpcmpeqb (%%rax), %%xmm8, %%xmm7")

GEN_test_RandM(VSHUFPS_0x39_128,
               "vshufps $0x39, %%xmm9,  %%xmm8, %%xmm7",
               "vshufps $0xC6, (%%rax), %%xmm8, %%xmm7")

GEN_test_RandM(VMULPS_128,
               "vmulps %%xmm9,  %%xmm8, %%xmm7",
               "vmulps (%%rax), %%xmm8, %%xmm7")

GEN_test_RandM(VSUBPS_128,
               "vsubps %%xmm9,  %%xmm8, %%xmm7",
               "vsubps (%%rax), %%xmm8, %%xmm7")

GEN_test_RandM(VADDPS_128,
               "vaddps %%xmm9,  %%xmm8, %%xmm7",
               "vaddps (%%rax), %%xmm8, %%xmm7")

GEN_test_RandM(VMAXPS_128,
               "vmaxps %%xmm9,  %%xmm8, %%xmm7",
               "vmaxps (%%rax), %%xmm8, %%xmm7")

GEN_test_RandM(VMINPS_128,
               "vminps %%xmm9,  %%xmm8, %%xmm7",
               "vminps (%%rax), %%xmm8, %%xmm7")

GEN_test_RandM(VCVTPS2DQ_128,
               "vcvtps2dq %%xmm8, %%xmm7",
               "vcvtps2dq (%%rax), %%xmm8")

GEN_test_RandM(VPSHUFLW_0x39_128,
               "vpshuflw $0x39, %%xmm9,  %%xmm7",
               "vpshuflw $0xC6, (%%rax), %%xmm8")

GEN_test_RandM(VPSHUFHW_0x39_128,
               "vpshufhw $0x39, %%xmm9,  %%xmm7",
               "vpshufhw $0xC6, (%%rax), %%xmm8")

GEN_test_RandM(VPMULLW_128,
               "vpmullw %%xmm9,  %%xmm8, %%xmm7",
               "vpmullw (%%rax), %%xmm8, %%xmm7")

GEN_test_RandM(VPADDUSW_128,
               "vpaddusw %%xmm9,  %%xmm8, %%xmm7",
               "vpaddusw (%%rax), %%xmm8, %%xmm7")

GEN_test_RandM(VPMULHUW_128,
               "vpmulhuw %%xmm9,  %%xmm8, %%xmm7",
               "vpmulhuw (%%rax), %%xmm8, %%xmm7")

GEN_test_RandM(VPADDUSB_128,
               "vpaddusb %%xmm9,  %%xmm8, %%xmm7",
               "vpaddusb (%%rax), %%xmm8, %%xmm7")

GEN_test_RandM(VPUNPCKLWD_128,
               "vpunpcklwd %%xmm6,  %%xmm8, %%xmm7",
               "vpunpcklwd (%%rax), %%xmm8, %%xmm7")

GEN_test_RandM(VPUNPCKHWD_128,
               "vpunpckhwd %%xmm6,  %%xmm8, %%xmm7",
               "vpunpckhwd (%%rax), %%xmm8, %%xmm7")

GEN_test_Ronly(VPSLLD_0x05_128,
               "vpslld $0x5, %%xmm9,  %%xmm7")

GEN_test_Ronly(VPSRLD_0x05_128,
               "vpsrld $0x5, %%xmm9,  %%xmm7")

GEN_test_RandM(VPSUBUSB_128,
               "vpsubusb %%xmm9,  %%xmm8, %%xmm7",
               "vpsubusb (%%rax), %%xmm8, %%xmm7")

GEN_test_Ronly(VPSRLDQ_0x05_128,
               "vpsrldq $0x5, %%xmm9,  %%xmm7")

GEN_test_Ronly(VPSLLDQ_0x05_128,
               "vpslldq $0x5, %%xmm9,  %%xmm7")

GEN_test_RandM(VPANDN_128,
               "vpandn %%xmm9,  %%xmm8, %%xmm7",
               "vpandn (%%rax), %%xmm8, %%xmm7")

/* NB tests the mem form only */
GEN_test_Monly(VMOVD_XMM_to_MEM32,
               "vmovd %%xmm7, (%%rax)")

GEN_test_RandM(VPINSRD_128,
               "vpinsrd $0, %%r14d,  %%xmm8, %%xmm7",
               "vpinsrd $3, (%%rax), %%xmm8, %%xmm7")

GEN_test_RandM(VPUNPCKLQDQ_128,
               "vpunpcklqdq %%xmm6,  %%xmm8, %%xmm7",
               "vpunpcklqdq (%%rax), %%xmm8, %%xmm7")

GEN_test_Ronly(VPSRLW_0x05_128,
               "vpsrlw $0x5, %%xmm9,  %%xmm7")

GEN_test_RandM(VPADDW_128,
               "vpaddw %%xmm6,  %%xmm8, %%xmm7",
               "vpaddw (%%rax), %%xmm8, %%xmm7")

GEN_test_RandM(VPACKSSDW_128,
               "vpackssdw %%xmm9,  %%xmm8, %%xmm7",
               "vpackssdw (%%rax), %%xmm8, %%xmm7")

GEN_test_RandM(VPUNPCKLDQ_128,
               "vpunpckldq %%xmm6,  %%xmm8, %%xmm7",
               "vpunpckldq (%%rax), %%xmm8, %%xmm7")

GEN_test_RandM(VINSERTPS_0x39_128,
               "vinsertps $0x39, %%xmm6,  %%xmm8, %%xmm7",
               "vinsertps $0xC6, (%%rax), %%xmm8, %%xmm7")

GEN_test_Monly(VMOVSD_M64_XMM, "vmovsd (%%rax), %%xmm8")

GEN_test_Monly(VMOVSS_M64_XMM, "vmovss (%%rax), %%xmm8")

GEN_test_Monly(VMOVSD_XMM_M64, "vmovsd %%xmm8, (%%rax)")

GEN_test_Monly(VMOVSS_XMM_M32, "vmovss %%xmm8, (%%rax)")

GEN_test_RandM(VMOVUPD_GtoE_128,
               "vmovupd %%xmm9,  %%xmm6",
               "vmovupd %%xmm7, (%%rax)")

GEN_test_RandM(VMOVAPD_EtoG_128,
               "vmovapd %%xmm6,  %%xmm8",
               "vmovapd (%%rax), %%xmm9")

GEN_test_RandM(VMOVAPD_EtoG_256,
               "vmovapd %%ymm6,  %%ymm8",
               "vmovapd (%%rax), %%ymm9")

GEN_test_RandM(VMOVAPS_EtoG_128,
               "vmovaps %%xmm6,  %%xmm8",
               "vmovaps (%%rax), %%xmm9")

GEN_test_RandM(VMOVAPS_GtoE_128,
               "vmovaps %%xmm9,  %%xmm6",
               "vmovaps %%xmm7, (%%rax)")

GEN_test_RandM(VMOVAPS_GtoE_256,
               "vmovaps %%ymm9,  %%ymm6",
               "vmovaps %%ymm7, (%%rax)")

GEN_test_RandM(VMOVAPD_GtoE_128,
               "vmovapd %%xmm9,  %%xmm6",
               "vmovapd %%xmm7, (%%rax)")

GEN_test_RandM(VMOVAPD_GtoE_256,
               "vmovapd %%ymm9,  %%ymm6",
               "vmovapd %%ymm7, (%%rax)")

GEN_test_RandM(VMOVDQU_EtoG_128,
               "vmovdqu %%xmm6,  %%xmm8",
               "vmovdqu (%%rax), %%xmm9")

GEN_test_RandM(VMOVDQA_EtoG_128,
               "vmovdqa %%xmm6,  %%xmm8",
               "vmovdqa (%%rax), %%xmm9")

GEN_test_RandM(VMOVDQA_EtoG_256,
               "vmovdqa %%ymm6,  %%ymm8",
               "vmovdqa (%%rax), %%ymm9")

GEN_test_RandM(VMOVDQU_GtoE_128,
               "vmovdqu %%xmm9,  %%xmm6",
               "vmovdqu %%xmm7, (%%rax)")

GEN_test_RandM(VMOVDQA_GtoE_128,
               "vmovdqa %%xmm9,  %%xmm6",
               "vmovdqa %%xmm7, (%%rax)")

GEN_test_RandM(VMOVDQA_GtoE_256,
               "vmovdqa %%ymm9,  %%ymm6",
               "vmovdqa %%ymm7, (%%rax)")

GEN_test_Monly(VMOVQ_XMM_MEM64, "vmovq %%xmm8, (%%rax)")

GEN_test_RandM(VMOVD_IREGorMEM32_to_XMM,
               "vmovd %%r14d, %%xmm7",
               "vmovd (%%rax), %%xmm9")

GEN_test_RandM(VMOVDDUP_XMMorMEM64_to_XMM,
               "vmovddup %%xmm8,  %%xmm7",
               "vmovddup (%%rax), %%xmm9")

GEN_test_RandM(VCMPSS_128_0x0,
               "vcmpss $0, %%xmm6,  %%xmm8, %%xmm7",
               "vcmpss $0, (%%rax), %%xmm8, %%xmm7")

GEN_test_RandM(VCMPSS_128_0x1,
               "vcmpss $1, %%xmm6,  %%xmm8, %%xmm7",
               "vcmpss $1, (%%rax), %%xmm8, %%xmm7")

GEN_test_RandM(VCMPSS_128_0x2,
               "vcmpss $2, %%xmm6,  %%xmm8, %%xmm7",
               "vcmpss $2, (%%rax), %%xmm8, %%xmm7")

GEN_test_RandM(VCMPSS_128_0x3,
               "vcmpss $3, %%xmm6,  %%xmm8, %%xmm7",
               "vcmpss $3, (%%rax), %%xmm8, %%xmm7")

GEN_test_RandM(VCMPSS_128_0x4,
               "vcmpss $4, %%xmm6,  %%xmm8, %%xmm7",
               "vcmpss $4, (%%rax), %%xmm8, %%xmm7")

GEN_test_RandM(VCMPSS_128_0x5,
               "vcmpss $5, %%xmm6,  %%xmm8, %%xmm7",
               "vcmpss $5, (%%rax), %%xmm8, %%xmm7")

GEN_test_RandM(VCMPSS_128_0x6,
               "vcmpss $6, %%xmm6,  %%xmm8, %%xmm7",
               "vcmpss $6, (%%rax), %%xmm8, %%xmm7")

GEN_test_RandM(VCMPSS_128_0x7,
               "vcmpss $7, %%xmm6,  %%xmm8, %%xmm7",
               "vcmpss $7, (%%rax), %%xmm8, %%xmm7")

GEN_test_RandM(VCMPSS_128_0xA,
               "vcmpss $0xA, %%xmm6,  %%xmm8, %%xmm7",
               "vcmpss $0xA, (%%rax), %%xmm8, %%xmm7")

GEN_test_RandM(VCMPSS_128_0xC,
               "vcmpss $0xC, %%xmm6,  %%xmm8, %%xmm7",
               "vcmpss $0xC, (%%rax), %%xmm8, %%xmm7")

GEN_test_RandM(VCMPSS_128_0xD,
               "vcmpss $0xD, %%xmm6,  %%xmm8, %%xmm7",
               "vcmpss $0xD, (%%rax), %%xmm8, %%xmm7")

GEN_test_RandM(VCMPSS_128_0xE,
               "vcmpss $0xE, %%xmm6,  %%xmm8, %%xmm7",
               "vcmpss $0xE, (%%rax), %%xmm8, %%xmm7")

// The x suffix denotes a 128 -> 64 operation
GEN_test_RandM(VCVTPD2PS_128,
               "vcvtpd2psx %%xmm8,  %%xmm7",
               "vcvtpd2psx (%%rax), %%xmm9")

GEN_test_RandM(VEXTRACTF128_0x0,
               "vextractf128 $0x0, %%ymm7, %%xmm9",
               "vextractf128 $0x0, %%ymm7, (%%rax)")

GEN_test_RandM(VEXTRACTF128_0x1,
               "vextractf128 $0x1, %%ymm7, %%xmm9",
               "vextractf128 $0x1, %%ymm7, (%%rax)")

GEN_test_RandM(VINSERTF128_0x0,
               "vinsertf128 $0x0, %%xmm9,  %%ymm7, %%ymm8",
               "vinsertf128 $0x0, (%%rax), %%ymm7, %%ymm8")

GEN_test_RandM(VINSERTF128_0x1,
               "vinsertf128 $0x1, %%xmm9,  %%ymm7, %%ymm8",
               "vinsertf128 $0x1, (%%rax), %%ymm7, %%ymm8")

GEN_test_RandM(VPEXTRD_128_0x0,
               "vpextrd $0x0, %%xmm7, %%r14d",
               "vpextrd $0x0, %%xmm7, (%%rax)")

GEN_test_RandM(VPEXTRD_128_0x3,
               "vpextrd $0x3, %%xmm7, %%r14d",
               "vpextrd $0x3, %%xmm7, (%%rax)")

GEN_test_RandM(VPCMPEQD_128,
               "vpcmpeqd %%xmm6,  %%xmm8, %%xmm7",
               "vpcmpeqd (%%rax), %%xmm8, %%xmm7")

GEN_test_RandM(VPSHUFD_0x39_128,
               "vpshufd $0x39, %%xmm9,  %%xmm8",
               "vpshufd $0xC6, (%%rax), %%xmm7")

GEN_test_RandM(VMAXSD_128,
               "vmaxsd %%xmm6,  %%xmm8, %%xmm7",
               "vmaxsd (%%rax), %%xmm8, %%xmm7")

GEN_test_RandM(VDIVSD_128,
               "vdivsd %%xmm6,  %%xmm8, %%xmm7",
               "vdivsd (%%rax), %%xmm8, %%xmm7")

GEN_test_RandM(VMINSD_128,
               "vminsd %%xmm6,  %%xmm8, %%xmm7",
               "vminsd (%%rax), %%xmm8, %%xmm7")

GEN_test_RandM(VSUBSD_128,
               "vsubsd %%xmm6,  %%xmm8, %%xmm7",
               "vsubsd (%%rax), %%xmm8, %%xmm7")

GEN_test_RandM(VADDSD_128,
               "vaddsd %%xmm6,  %%xmm8, %%xmm7",
               "vaddsd (%%rax), %%xmm8, %%xmm7")

GEN_test_RandM(VMULSD_128,
               "vmulsd %%xmm6,  %%xmm8, %%xmm7",
               "vmulsd (%%rax), %%xmm8, %%xmm7")

GEN_test_RandM(VXORPS_128,
               "vxorps %%xmm6,  %%xmm8, %%xmm7",
               "vxorps (%%rax), %%xmm8, %%xmm7")

GEN_test_RandM(VXORPD_128,
               "vxorpd %%xmm6,  %%xmm8, %%xmm7",
               "vxorpd (%%rax), %%xmm8, %%xmm7")

GEN_test_RandM(VORPD_128,
               "vorpd %%xmm6,  %%xmm8, %%xmm7",
               "vorpd (%%rax), %%xmm8, %%xmm7")

GEN_test_RandM(VANDNPD_128,
               "vandnpd %%xmm6,  %%xmm8, %%xmm7",
               "vandnpd (%%rax), %%xmm8, %%xmm7")

GEN_test_RandM(VCVTPS2PD_128,
               "vcvtps2pd %%xmm6,  %%xmm8",
               "vcvtps2pd (%%rax), %%xmm8")

GEN_test_RandM(VUCOMISD_128,
   "vucomisd %%xmm6,  %%xmm8; pushfq; popq %%r14; andq $0x8D5, %%r14",
   "vucomisd (%%rax), %%xmm8; pushfq; popq %%r14; andq $0x8D5, %%r14")

GEN_test_RandM(VUCOMISS_128,
   "vucomiss %%xmm6,  %%xmm8; pushfq; popq %%r14; andq $0x8D5, %%r14",
   "vucomiss (%%rax), %%xmm8; pushfq; popq %%r14; andq $0x8D5, %%r14")

GEN_test_RandM(VPINSRQ_128,
               "vpinsrq $0, %%r14,   %%xmm8, %%xmm7",
               "vpinsrq $1, (%%rax), %%xmm8, %%xmm7")

GEN_test_RandM(VPADDQ_128,
               "vpaddq %%xmm6,  %%xmm8, %%xmm7",
               "vpaddq (%%rax), %%xmm8, %%xmm7")

GEN_test_RandM(VPSUBQ_128,
               "vpsubq %%xmm6,  %%xmm8, %%xmm7",
               "vpsubq (%%rax), %%xmm8, %%xmm7")

GEN_test_RandM(VPSUBW_128,
               "vpsubw %%xmm6,  %%xmm8, %%xmm7",
               "vpsubw (%%rax), %%xmm8, %%xmm7")

GEN_test_RandM(VMOVUPD_GtoE_256,
               "vmovupd %%ymm9,  %%ymm6",
               "vmovupd %%ymm7, (%%rax)")

GEN_test_RandM(VMOVUPD_EtoG_256,
               "vmovupd %%ymm6,  %%ymm9",
               "vmovupd (%%rax), %%ymm7")

GEN_test_RandM(VMULPD_256,
               "vmulpd %%ymm6,  %%ymm8, %%ymm7",
               "vmulpd (%%rax), %%ymm8, %%ymm7")

GEN_test_RandM(VMOVUPD_EtoG_128,
               "vmovupd %%xmm6,  %%xmm9",
               "vmovupd (%%rax), %%xmm7")

GEN_test_RandM(VADDPD_256,
               "vaddpd %%ymm6,  %%ymm8, %%ymm7",
               "vaddpd (%%rax), %%ymm8, %%ymm7")

GEN_test_RandM(VSUBPD_256,
               "vsubpd %%ymm6,  %%ymm8, %%ymm7",
               "vsubpd (%%rax), %%ymm8, %%ymm7")

GEN_test_RandM(VDIVPD_256,
               "vdivpd %%ymm6,  %%ymm8, %%ymm7",
               "vdivpd (%%rax), %%ymm8, %%ymm7")

GEN_test_RandM(VPCMPEQQ_128,
               "vpcmpeqq %%xmm6,  %%xmm8, %%xmm7",
               "vpcmpeqq (%%rax), %%xmm8, %%xmm7")

GEN_test_RandM(VSUBPD_128,
               "vsubpd %%xmm6,  %%xmm8, %%xmm7",
               "vsubpd (%%rax), %%xmm8, %%xmm7")

GEN_test_RandM(VADDPD_128,
               "vaddpd %%xmm6,  %%xmm8, %%xmm7",
               "vaddpd (%%rax), %%xmm8, %%xmm7")

GEN_test_RandM(VUNPCKLPD_128,
               "vunpcklpd %%xmm6,  %%xmm8, %%xmm7",
               "vunpcklpd (%%rax), %%xmm8, %%xmm7")

GEN_test_RandM(VUNPCKHPD_128,
               "vunpckhpd %%xmm6,  %%xmm8, %%xmm7",
               "vunpckhpd (%%rax), %%xmm8, %%xmm7")

GEN_test_RandM(VUNPCKHPS_128,
               "vunpckhps %%xmm6,  %%xmm8, %%xmm7",
               "vunpckhps (%%rax), %%xmm8, %%xmm7")

GEN_test_RandM(VMOVUPS_EtoG_128,
               "vmovups %%xmm6,  %%xmm8",
               "vmovups (%%rax), %%xmm9")

GEN_test_RandM(VADDPS_256,
               "vaddps %%ymm6,  %%ymm8, %%ymm7",
               "vaddps (%%rax), %%ymm8, %%ymm7")

GEN_test_RandM(VSUBPS_256,
               "vsubps %%ymm6,  %%ymm8, %%ymm7",
               "vsubps (%%rax), %%ymm8, %%ymm7")

GEN_test_RandM(VMULPS_256,
               "vmulps %%ymm6,  %%ymm8, %%ymm7",
               "vmulps (%%rax), %%ymm8, %%ymm7")

GEN_test_RandM(VDIVPS_256,
               "vdivps %%ymm6,  %%ymm8, %%ymm7",
               "vdivps (%%rax), %%ymm8, %%ymm7")

GEN_test_RandM(VPCMPGTQ_128,
               "vpcmpgtq %%xmm6,  %%xmm8, %%xmm7",
               "vpcmpgtq (%%rax), %%xmm8, %%xmm7")

GEN_test_RandM(VPEXTRQ_128_0x0,
               "vpextrq $0x0, %%xmm7, %%r14",
               "vpextrq $0x0, %%xmm7, (%%rax)")

GEN_test_RandM(VPEXTRQ_128_0x1,
               "vpextrq $0x1, %%xmm7, %%r14",
               "vpextrq $0x1, %%xmm7, (%%rax)")

GEN_test_Ronly(VPSRLQ_0x05_128,
               "vpsrlq $0x5, %%xmm9,  %%xmm7")

GEN_test_RandM(VPMULUDQ_128,
               "vpmuludq %%xmm6,  %%xmm8, %%xmm7",
               "vpmuludq (%%rax), %%xmm8, %%xmm7")

GEN_test_Ronly(VPSLLQ_0x05_128,
               "vpsllq $0x5, %%xmm9,  %%xmm7")

GEN_test_RandM(VPMAXUD_128,
               "vpmaxud %%xmm6,  %%xmm8, %%xmm7",
               "vpmaxud (%%rax), %%xmm8, %%xmm7")

GEN_test_RandM(VPMINUD_128,
               "vpminud %%xmm6,  %%xmm8, %%xmm7",
               "vpminud (%%rax), %%xmm8, %%xmm7")

GEN_test_RandM(VPMULLD_128,
               "vpmulld %%xmm6,  %%xmm8, %%xmm7",
               "vpmulld (%%rax), %%xmm8, %%xmm7")

GEN_test_RandM(VPMAXUW_128,
               "vpmaxuw %%xmm6,  %%xmm8, %%xmm7",
               "vpmaxuw (%%rax), %%xmm8, %%xmm7")

GEN_test_Ronly(VPEXTRW_128_EregOnly_toG_0x0,
               "vpextrw $0x0, %%xmm7, %%r14d")

GEN_test_Ronly(VPEXTRW_128_EregOnly_toG_0x7,
               "vpextrw $0x7, %%xmm7, %%r14d")

GEN_test_RandM(VPMINUW_128,
               "vpminuw %%xmm6,  %%xmm8, %%xmm7",
               "vpminuw (%%rax), %%xmm8, %%xmm7")

GEN_test_RandM(VPHMINPOSUW_128,
               "vphminposuw %%xmm6,  %%xmm8",
               "vphminposuw (%%rax), %%xmm7")

GEN_test_RandM(VPMAXSW_128,
               "vpmaxsw %%xmm6,  %%xmm8, %%xmm7",
               "vpmaxsw (%%rax), %%xmm8, %%xmm7")

GEN_test_RandM(VPMINSW_128,
               "vpminsw %%xmm6,  %%xmm8, %%xmm7",
               "vpminsw (%%rax), %%xmm8, %%xmm7")

GEN_test_RandM(VPMAXUB_128,
               "vpmaxub %%xmm6,  %%xmm8, %%xmm7",
               "vpmaxub (%%rax), %%xmm8, %%xmm7")

GEN_test_RandM(VPEXTRB_GtoE_128_0x0,
               "vpextrb $0x0, %%xmm8, %%r14",
               "vpextrb $0x0, %%xmm8, (%%rax)")

GEN_test_RandM(VPEXTRB_GtoE_128_0x1,
               "vpextrb $0x1, %%xmm8, %%r14",
               "vpextrb $0x1, %%xmm8, (%%rax)")

GEN_test_RandM(VPEXTRB_GtoE_128_0x2,
               "vpextrb $0x2, %%xmm8, %%r14",
               "vpextrb $0x2, %%xmm8, (%%rax)")

GEN_test_RandM(VPEXTRB_GtoE_128_0x3,
               "vpextrb $0x3, %%xmm8, %%r14",
               "vpextrb $0x3, %%xmm8, (%%rax)")

GEN_test_RandM(VPEXTRB_GtoE_128_0x4,
               "vpextrb $0x4, %%xmm8, %%r14",
               "vpextrb $0x4, %%xmm8, (%%rax)")

GEN_test_RandM(VPEXTRB_GtoE_128_0x9,
               "vpextrb $0x9, %%xmm8, %%r14",
               "vpextrb $0x9, %%xmm8, (%%rax)")

GEN_test_RandM(VPEXTRB_GtoE_128_0xE,
               "vpextrb $0xE, %%xmm8, %%r14",
               "vpextrb $0xE, %%xmm8, (%%rax)")

GEN_test_RandM(VPEXTRB_GtoE_128_0xF,
               "vpextrb $0xF, %%xmm8, %%r14",
               "vpextrb $0xF, %%xmm8, (%%rax)")

GEN_test_RandM(VPMINUB_128,
               "vpminub %%xmm6,  %%xmm8, %%xmm7",
               "vpminub (%%rax), %%xmm8, %%xmm7")

GEN_test_RandM(VPMAXSB_128,
               "vpmaxsb %%xmm6,  %%xmm8, %%xmm7",
               "vpmaxsb (%%rax), %%xmm8, %%xmm7")

GEN_test_RandM(VPMINSB_128,
               "vpminsb %%xmm6,  %%xmm8, %%xmm7",
               "vpminsb (%%rax), %%xmm8, %%xmm7")

GEN_test_RandM(VPERM2F128_0x00,
               "vperm2f128 $0x00, %%ymm6,  %%ymm8, %%ymm7",
               "vperm2f128 $0x00, (%%rax), %%ymm8, %%ymm7")
GEN_test_RandM(VPERM2F128_0xFF,
               "vperm2f128 $0xFF, %%ymm6,  %%ymm8, %%ymm7",
               "vperm2f128 $0xFF, (%%rax), %%ymm8, %%ymm7")
GEN_test_RandM(VPERM2F128_0x30,
               "vperm2f128 $0x30, %%ymm6,  %%ymm8, %%ymm7",
               "vperm2f128 $0x30, (%%rax), %%ymm8, %%ymm7")
GEN_test_RandM(VPERM2F128_0x21,
               "vperm2f128 $0x21, %%ymm6,  %%ymm8, %%ymm7",
               "vperm2f128 $0x21, (%%rax), %%ymm8, %%ymm7")
GEN_test_RandM(VPERM2F128_0x12,
               "vperm2f128 $0x12, %%ymm6,  %%ymm8, %%ymm7",
               "vperm2f128 $0x12, (%%rax), %%ymm8, %%ymm7")
GEN_test_RandM(VPERM2F128_0x03,
               "vperm2f128 $0x03, %%ymm6,  %%ymm8, %%ymm7",
               "vperm2f128 $0x03, (%%rax), %%ymm8, %%ymm7")
GEN_test_RandM(VPERM2F128_0x85,
               "vperm2f128 $0x85, %%ymm6,  %%ymm8, %%ymm7",
               "vperm2f128 $0x85, (%%rax), %%ymm8, %%ymm7")
GEN_test_RandM(VPERM2F128_0x5A,
               "vperm2f128 $0x5A, %%ymm6,  %%ymm8, %%ymm7",
               "vperm2f128 $0x5A, (%%rax), %%ymm8, %%ymm7")

GEN_test_RandM(VPERMILPD_256_0x0,
               "vpermilpd $0x0, %%ymm6,  %%ymm8",
               "vpermilpd $0x1, (%%rax), %%ymm8")
GEN_test_RandM(VPERMILPD_256_0xF,
               "vpermilpd $0xF, %%ymm6,  %%ymm8",
               "vpermilpd $0xE, (%%rax), %%ymm8")
GEN_test_RandM(VPERMILPD_256_0xA,
               "vpermilpd $0xA, %%ymm6,  %%ymm8",
               "vpermilpd $0xB, (%%rax), %%ymm8")
GEN_test_RandM(VPERMILPD_256_0x5,
               "vpermilpd $0x5, %%ymm6,  %%ymm8",
               "vpermilpd $0x4, (%%rax), %%ymm8")

GEN_test_RandM(VPERMILPD_128_0x0,
               "vpermilpd $0x0, %%xmm6,  %%xmm8",
               "vpermilpd $0x1, (%%rax), %%xmm8")
GEN_test_RandM(VPERMILPD_128_0x3,
               "vpermilpd $0x3, %%xmm6,  %%xmm8",
               "vpermilpd $0x2, (%%rax), %%xmm8")

GEN_test_RandM(VUNPCKLPD_256,
               "vunpcklpd %%ymm6,  %%ymm8, %%ymm7",
               "vunpcklpd (%%rax), %%ymm8, %%ymm7")

GEN_test_RandM(VUNPCKHPD_256,
               "vunpckhpd %%ymm6,  %%ymm8, %%ymm7",
               "vunpckhpd (%%rax), %%ymm8, %%ymm7")

GEN_test_RandM(VSHUFPS_0x39_256,
               "vshufps $0x39, %%ymm9,  %%ymm8, %%ymm7",
               "vshufps $0xC6, (%%rax), %%ymm8, %%ymm7")

GEN_test_RandM(VUNPCKLPS_256,
               "vunpcklps %%ymm6,  %%ymm8, %%ymm7",
               "vunpcklps (%%rax), %%ymm8, %%ymm7")

GEN_test_RandM(VUNPCKHPS_256,
               "vunpckhps %%ymm6,  %%ymm8, %%ymm7",
               "vunpckhps (%%rax), %%ymm8, %%ymm7")

GEN_test_RandM(VXORPD_256,
               "vxorpd %%ymm6,  %%ymm8, %%ymm7",
               "vxorpd (%%rax), %%ymm8, %%ymm7")

GEN_test_Monly(VBROADCASTSD_256,
               "vbroadcastsd (%%rax), %%ymm8")

GEN_test_RandM(VCMPPD_128_0x4,
               "vcmppd $4, %%xmm6,  %%xmm8, %%xmm7",
               "vcmppd $4, (%%rax), %%xmm8, %%xmm7")

GEN_test_RandM(VCVTDQ2PD_128,
               "vcvtdq2pd %%xmm6,  %%xmm8",
               "vcvtdq2pd (%%rax), %%xmm8")

GEN_test_RandM(VDIVPD_128,
               "vdivpd %%xmm6,  %%xmm8, %%xmm7",
               "vdivpd (%%rax), %%xmm8, %%xmm7")

GEN_test_RandM(VANDPD_256,
               "vandpd %%ymm6,  %%ymm8, %%ymm7",
               "vandpd (%%rax), %%ymm8, %%ymm7")

GEN_test_RandM(VPMOVSXBW_128,
               "vpmovsxbw %%xmm6,  %%xmm8",
               "vpmovsxbw (%%rax), %%xmm8")

GEN_test_RandM(VPSUBUSW_128,
               "vpsubusw %%xmm9,  %%xmm8, %%xmm7",
               "vpsubusw (%%rax), %%xmm8, %%xmm7")

GEN_test_RandM(VPCMPEQW_128,
               "vpcmpeqw %%xmm6,  %%xmm8, %%xmm7",
               "vpcmpeqw (%%rax), %%xmm8, %%xmm7")

GEN_test_RandM(VPADDB_128,
               "vpaddb %%xmm6,  %%xmm8, %%xmm7",
               "vpaddb (%%rax), %%xmm8, %%xmm7")

GEN_test_RandM(VMOVAPS_EtoG_256,
               "vmovaps %%ymm6,  %%ymm8",
               "vmovaps (%%rax), %%ymm9")

GEN_test_RandM(VCVTDQ2PD_256,
               "vcvtdq2pd %%xmm6,  %%ymm8",
               "vcvtdq2pd (%%rax), %%ymm8")

GEN_test_Monly(VMOVHPD_128_LoadForm,
               "vmovhpd (%%rax), %%xmm8, %%xmm7")

// The y suffix denotes a 256 -> 128 operation
GEN_test_RandM(VCVTPD2PS_256,
               "vcvtpd2psy %%ymm8,  %%xmm7",
               "vcvtpd2psy (%%rax), %%xmm9")

GEN_test_RandM(VPUNPCKHDQ_128,
               "vpunpckhdq %%xmm6,  %%xmm8, %%xmm7",
               "vpunpckhdq (%%rax), %%xmm8, %%xmm7")

GEN_test_Monly(VBROADCASTSS_128,
               "vbroadcastss (%%rax), %%xmm8")

GEN_test_RandM(VPMOVSXDQ_128,
               "vpmovsxdq %%xmm6,  %%xmm8",
               "vpmovsxdq (%%rax), %%xmm8")

GEN_test_RandM(VPMOVSXWD_128,
               "vpmovsxwd %%xmm6,  %%xmm8",
               "vpmovsxwd (%%rax), %%xmm8")

GEN_test_RandM(VDIVPS_128,
               "vdivps %%xmm9,  %%xmm8, %%xmm7",
               "vdivps (%%rax), %%xmm8, %%xmm7")

GEN_test_RandM(VANDPS_256,
               "vandps %%ymm6,  %%ymm8, %%ymm7",
               "vandps (%%rax), %%ymm8, %%ymm7")

GEN_test_RandM(VXORPS_256,
               "vxorps %%ymm6,  %%ymm8, %%ymm7",
               "vxorps (%%rax), %%ymm8, %%ymm7")

GEN_test_RandM(VORPS_256,
               "vorps %%ymm6,  %%ymm8, %%ymm7",
               "vorps (%%rax), %%ymm8, %%ymm7")

GEN_test_RandM(VANDNPD_256,
               "vandnpd %%ymm6,  %%ymm8, %%ymm7",
               "vandnpd (%%rax), %%ymm8, %%ymm7")

GEN_test_RandM(VANDNPS_256,
               "vandnps %%ymm6,  %%ymm8, %%ymm7",
               "vandnps (%%rax), %%ymm8, %%ymm7")

GEN_test_RandM(VORPD_256,
               "vorpd %%ymm6,  %%ymm8, %%ymm7",
               "vorpd (%%rax), %%ymm8, %%ymm7")

GEN_test_RandM(VPERMILPS_256_0x0F,
               "vpermilps $0x0F, %%ymm6,  %%ymm8",
               "vpermilps $0x1E, (%%rax), %%ymm8")
GEN_test_RandM(VPERMILPS_256_0xFA,
               "vpermilps $0xFA, %%ymm6,  %%ymm8",
               "vpermilps $0xE5, (%%rax), %%ymm8")
GEN_test_RandM(VPERMILPS_256_0xA3,
               "vpermilps $0xA3, %%ymm6,  %%ymm8",
               "vpermilps $0xB4, (%%rax), %%ymm8")
GEN_test_RandM(VPERMILPS_256_0x5A,
               "vpermilps $0x5A, %%ymm6,  %%ymm8",
               "vpermilps $0x45, (%%rax), %%ymm8")

GEN_test_RandM(VPMULHW_128,
               "vpmulhw %%xmm9,  %%xmm8, %%xmm7",
               "vpmulhw (%%rax), %%xmm8, %%xmm7")

GEN_test_RandM(VPUNPCKHQDQ_128,
               "vpunpckhqdq %%xmm6,  %%xmm8, %%xmm7",
               "vpunpckhqdq (%%rax), %%xmm8, %%xmm7")

GEN_test_Ronly(VPSRAW_0x05_128,
               "vpsraw $0x5, %%xmm9,  %%xmm7")

GEN_test_RandM(VPCMPGTD_128,
               "vpcmpgtd %%xmm6,  %%xmm8, %%xmm7",
               "vpcmpgtd (%%rax), %%xmm8, %%xmm7")

GEN_test_RandM(VPMOVZXBD_128,
               "vpmovzxbd %%xmm6,  %%xmm8",
               "vpmovzxbd (%%rax), %%xmm8")

GEN_test_RandM(VPMOVSXBD_128,
               "vpmovsxbd %%xmm6,  %%xmm8",
               "vpmovsxbd (%%rax), %%xmm8")

GEN_test_RandM(VPINSRB_128_1of3,
               "vpinsrb $0, %%r14d,  %%xmm8, %%xmm7",
               "vpinsrb $3, (%%rax), %%xmm8, %%xmm7")
GEN_test_RandM(VPINSRB_128_2of3,
               "vpinsrb $6, %%r14d,  %%xmm8, %%xmm7",
               "vpinsrb $9, (%%rax), %%xmm8, %%xmm7")
GEN_test_RandM(VPINSRB_128_3of3,
               "vpinsrb $12, %%r14d,  %%xmm8, %%xmm7",
               "vpinsrb $15, (%%rax), %%xmm8, %%xmm7")

GEN_test_RandM(VPINSRW_128_1of4,
               "vpinsrw $0, %%r14d,  %%xmm8, %%xmm7",
               "vpinsrw $3, (%%rax), %%xmm8, %%xmm7")
GEN_test_RandM(VPINSRW_128_2of4,
               "vpinsrw $2, %%r14d,  %%xmm8, %%xmm7",
               "vpinsrw $3, (%%rax), %%xmm8, %%xmm7")
GEN_test_RandM(VPINSRW_128_3of4,
               "vpinsrw $4, %%r14d,  %%xmm8, %%xmm7",
               "vpinsrw $5, (%%rax), %%xmm8, %%xmm7")
GEN_test_RandM(VPINSRW_128_4of4,
               "vpinsrw $6, %%r14d,  %%xmm8, %%xmm7",
               "vpinsrw $7, (%%rax), %%xmm8, %%xmm7")

GEN_test_RandM(VCOMISD_128,
   "vcomisd %%xmm6,  %%xmm8; pushfq; popq %%r14; andq $0x8D5, %%r14",
   "vcomisd (%%rax), %%xmm8; pushfq; popq %%r14; andq $0x8D5, %%r14")

GEN_test_RandM(VCOMISS_128,
   "vcomiss %%xmm6,  %%xmm8; pushfq; popq %%r14; andq $0x8D5, %%r14",
   "vcomiss (%%rax), %%xmm8; pushfq; popq %%r14; andq $0x8D5, %%r14")

GEN_test_RandM(VMOVUPS_YMM_to_YMMorMEM,
               "vmovups %%ymm8, %%ymm7",
               "vmovups %%ymm9, (%%rax)")

GEN_test_RandM(VDPPD_128_1of4,
               "vdppd $0x00, %%xmm6,  %%xmm8, %%xmm7",
               "vdppd $0xA5, (%%rax), %%xmm9, %%xmm6")
GEN_test_RandM(VDPPD_128_2of4,
               "vdppd $0x5A, %%xmm6,  %%xmm8, %%xmm7",
               "vdppd $0xFF, (%%rax), %%xmm9, %%xmm6")
GEN_test_RandM(VDPPD_128_3of4,
               "vdppd $0x0F, %%xmm6,  %%xmm8, %%xmm7",
               "vdppd $0x37, (%%rax), %%xmm9, %%xmm6")
GEN_test_RandM(VDPPD_128_4of4,
               "vdppd $0xF0, %%xmm6,  %%xmm8, %%xmm7",
               "vdppd $0x73, (%%rax), %%xmm9, %%xmm6")

GEN_test_Monly(VBROADCASTSS_256,
               "vbroadcastss (%%rax), %%ymm8")

GEN_test_RandM(VPALIGNR_128_1of3,
               "vpalignr $0, %%xmm6,  %%xmm8, %%xmm7",
               "vpalignr $3, (%%rax), %%xmm8, %%xmm7")
GEN_test_RandM(VPALIGNR_128_2of3,
               "vpalignr $6, %%xmm6,  %%xmm8, %%xmm7",
               "vpalignr $9, (%%rax), %%xmm8, %%xmm7")
GEN_test_RandM(VPALIGNR_128_3of3,
               "vpalignr $12, %%xmm6,  %%xmm8, %%xmm7",
               "vpalignr $15, (%%rax), %%xmm8, %%xmm7")

GEN_test_Ronly(VMOVSD_REG_XMM, "vmovsd %%xmm9, %%xmm7, %%xmm8")

GEN_test_Monly(VMOVLPD_128_M64_XMM_XMM, "vmovlpd (%%rax), %%xmm8, %%xmm7")

GEN_test_Monly(VMOVLPD_128_XMM_M64, "vmovlpd %%xmm7, (%%rax)")

GEN_test_RandM(VSHUFPD_128_1of2,
               "vshufpd $0, %%xmm9,  %%xmm8, %%xmm7",
               "vshufpd $1, (%%rax), %%xmm8, %%xmm7")
GEN_test_RandM(VSHUFPD_128_2of2,
               "vshufpd $2, %%xmm9,  %%xmm8, %%xmm7",
               "vshufpd $3, (%%rax), %%xmm8, %%xmm7")

GEN_test_RandM(VSHUFPD_256_1of2,
               "vshufpd $0x00, %%ymm9,  %%ymm8, %%ymm7",
               "vshufpd $0xFF, (%%rax), %%ymm8, %%ymm7")
GEN_test_RandM(VSHUFPD_256_2of2,
               "vshufpd $0x5A, %%ymm9,  %%ymm8, %%ymm7",
               "vshufpd $0xA5, (%%rax), %%ymm8, %%ymm7")

GEN_test_RandM(VPERMILPS_128_0x00,
               "vpermilps $0x00, %%xmm6,  %%xmm8",
               "vpermilps $0x01, (%%rax), %%xmm8")
GEN_test_RandM(VPERMILPS_128_0xFE,
               "vpermilps $0xFE, %%xmm6,  %%xmm8",
               "vpermilps $0xFF, (%%rax), %%xmm8")
GEN_test_RandM(VPERMILPS_128_0x30,
               "vpermilps $0x30, %%xmm6,  %%xmm8",
               "vpermilps $0x03, (%%rax), %%xmm8")
GEN_test_RandM(VPERMILPS_128_0x21,
               "vpermilps $0x21, %%xmm6,  %%xmm8",
               "vpermilps $0x12, (%%rax), %%xmm8")
GEN_test_RandM(VPERMILPS_128_0xD7,
               "vpermilps $0xD7, %%xmm6,  %%xmm8",
               "vpermilps $0x6C, (%%rax), %%xmm8")
GEN_test_RandM(VPERMILPS_128_0xB5,
               "vpermilps $0xB5, %%xmm6,  %%xmm8",
               "vpermilps $0x4A, (%%rax), %%xmm8")
GEN_test_RandM(VPERMILPS_128_0x85,
               "vpermilps $0x85, %%xmm6,  %%xmm8",
               "vpermilps $0xDC, (%%rax), %%xmm8")
GEN_test_RandM(VPERMILPS_128_0x29,
               "vpermilps $0x29, %%xmm6,  %%xmm8",
               "vpermilps $0x92, (%%rax), %%xmm8")

GEN_test_RandM(VBLENDPS_128_1of3,
               "vblendps $0, %%xmm6,  %%xmm8, %%xmm7",
               "vblendps $3, (%%rax), %%xmm8, %%xmm7")
GEN_test_RandM(VBLENDPS_128_2of3,
               "vblendps $6, %%xmm6,  %%xmm8, %%xmm7",
               "vblendps $9, (%%rax), %%xmm8, %%xmm7")
GEN_test_RandM(VBLENDPS_128_3of3,
               "vblendps $12, %%xmm6,  %%xmm8, %%xmm7",
               "vblendps $15, (%%rax), %%xmm8, %%xmm7")

GEN_test_RandM(VBLENDPD_128_1of2,
               "vblendpd $0, %%xmm6,  %%xmm8, %%xmm7",
               "vblendpd $1, (%%rax), %%xmm8, %%xmm7")
GEN_test_RandM(VBLENDPD_128_2of2,
               "vblendpd $2, %%xmm6,  %%xmm8, %%xmm7",
               "vblendpd $3, (%%rax), %%xmm8, %%xmm7")

GEN_test_RandM(VBLENDPD_256_1of3,
               "vblendpd $0, %%ymm6,  %%ymm8, %%ymm7",
               "vblendpd $3, (%%rax), %%ymm8, %%ymm7")
GEN_test_RandM(VBLENDPD_256_2of3,
               "vblendpd $6, %%ymm6,  %%ymm8, %%ymm7",
               "vblendpd $9, (%%rax), %%ymm8, %%ymm7")
GEN_test_RandM(VBLENDPD_256_3of3,
               "vblendpd $12, %%ymm6,  %%ymm8, %%ymm7",
               "vblendpd $15, (%%rax), %%ymm8, %%ymm7")

GEN_test_RandM(VPBLENDW_128_0x00,
               "vpblendw $0x00, %%xmm6,  %%xmm8, %%xmm7",
               "vpblendw $0x01, (%%rax), %%xmm8, %%xmm7")
GEN_test_RandM(VPBLENDW_128_0xFE,
               "vpblendw $0xFE, %%xmm6,  %%xmm8, %%xmm7",
               "vpblendw $0xFF, (%%rax), %%xmm8, %%xmm7")
GEN_test_RandM(VPBLENDW_128_0x30,
               "vpblendw $0x30, %%xmm6,  %%xmm8, %%xmm7",
               "vpblendw $0x03, (%%rax), %%xmm8, %%xmm7")
GEN_test_RandM(VPBLENDW_128_0x21,
               "vpblendw $0x21, %%xmm6,  %%xmm8, %%xmm7",
               "vpblendw $0x12, (%%rax), %%xmm8, %%xmm7")
GEN_test_RandM(VPBLENDW_128_0xD7,
               "vpblendw $0xD7, %%xmm6,  %%xmm8, %%xmm7",
               "vpblendw $0x6C, (%%rax), %%xmm8, %%xmm7")
GEN_test_RandM(VPBLENDW_128_0xB5,
               "vpblendw $0xB5, %%xmm6,  %%xmm8, %%xmm7",
               "vpblendw $0x4A, (%%rax), %%xmm8, %%xmm7")
GEN_test_RandM(VPBLENDW_128_0x85,
               "vpblendw $0x85, %%xmm6,  %%xmm8, %%xmm7",
               "vpblendw $0xDC, (%%rax), %%xmm8, %%xmm7")
GEN_test_RandM(VPBLENDW_128_0x29,
               "vpblendw $0x29, %%xmm6,  %%xmm8, %%xmm7",
               "vpblendw $0x92, (%%rax), %%xmm8, %%xmm7")


/* Comment duplicated above, for convenient reference:
   Allowed operands in test insns:
     Reg form:  %ymm6,  %ymm7, %ymm8, %ymm9 and %r14.
     Mem form:  (%rax), %ymm7, %ymm8, %ymm9 and %r14.
   Imm8 etc fields are also allowed, where they make sense.
*/

int main ( void )
{
   test_VMOVUPD_EtoG_256();
   test_VMOVUPD_GtoE_256();
   test_VPSUBW_128();
   test_VPSUBQ_128();
   test_VPADDQ_128();
   test_VPINSRQ_128();
   test_VUCOMISS_128();
   test_VUCOMISD_128();
   test_VCVTPS2PD_128();
   test_VANDNPD_128();
   test_VORPD_128();
   test_VXORPD_128();
   test_VXORPS_128();
   test_VMULSD_128();
   test_VADDSD_128();
   test_VMINSD_128();
   test_VSUBSD_128();
   test_VDIVSD_128();
   test_VMAXSD_128();
   test_VPSHUFD_0x39_128();
   test_VPCMPEQD_128();
   test_VPEXTRD_128_0x3();
   test_VPEXTRD_128_0x0();
   test_VINSERTF128_0x0();
   test_VINSERTF128_0x1();
   test_VEXTRACTF128_0x0();
   test_VEXTRACTF128_0x1();
   test_VCVTPD2PS_128(); // see comment on the test
   /* Test all CMPSS variants; this code is tricky. */
   test_VCMPSS_128_0x0();
   test_VCMPSS_128_0x1();
   test_VCMPSS_128_0x2();
   test_VCMPSS_128_0x3();
   test_VCMPSS_128_0x4();
   test_VCMPSS_128_0x5();
   test_VCMPSS_128_0x6();
   test_VCMPSS_128_0x7();
   test_VCMPSS_128_0xA();
   /* no 0xB case yet observed */
   test_VCMPSS_128_0xC();
   test_VCMPSS_128_0xD();
   test_VCMPSS_128_0xE();
   test_VMOVDDUP_XMMorMEM64_to_XMM();
   test_VMOVD_IREGorMEM32_to_XMM();
   test_VMOVQ_XMM_MEM64();
   test_VMOVDQA_GtoE_256();
   test_VMOVDQA_GtoE_128();
   test_VMOVDQU_GtoE_128();
   test_VMOVDQA_EtoG_256();
   test_VMOVDQA_EtoG_128();
   test_VMOVDQU_EtoG_128();
   test_VMOVAPD_GtoE_128();
   test_VMOVAPD_GtoE_256();
   test_VMOVAPS_GtoE_128();
   test_VMOVAPS_GtoE_256();
   test_VMOVAPS_EtoG_128();
   test_VMOVAPD_EtoG_256();
   test_VMOVAPD_EtoG_128();
   test_VMOVUPD_GtoE_128();
   test_VMOVSS_XMM_M32();
   test_VMOVSD_XMM_M64();
   test_VMOVSS_M64_XMM();
   test_VMOVSD_M64_XMM();
   test_VINSERTPS_0x39_128();
   test_VPUNPCKLDQ_128();
   test_VPACKSSDW_128();
   test_VPADDW_128();
   test_VPSRLW_0x05_128();
   test_VPUNPCKLQDQ_128();
   test_VPINSRD_128();
   test_VMOVD_XMM_to_MEM32();
   test_VPANDN_128();
   test_VPSLLDQ_0x05_128();
   test_VPSRLDQ_0x05_128();
   test_VPSUBUSB_128();
   test_VPSLLD_0x05_128();
   test_VPSRLD_0x05_128();
   test_VPUNPCKLWD_128();
   test_VPUNPCKHWD_128();
   test_VPADDUSB_128();
   test_VPMULHUW_128();
   test_VPADDUSW_128();
   test_VPMULLW_128();
   test_VPSHUFHW_0x39_128();
   test_VPSHUFLW_0x39_128();
   test_VCVTPS2DQ_128();
   test_VSUBPS_128();
   test_VADDPS_128();
   test_VMULPS_128();
   test_VMAXPS_128();
   test_VMINPS_128();
   test_VSHUFPS_0x39_128();
   test_VPCMPEQB_128();
   test_VMOVHPD_128_StoreForm();
   test_VPAND_128();
   test_VPMOVMSKB_128();
   test_VCVTTSS2SI_64();
   test_VPACKUSWB_128();
   test_VCVTSS2SD_128();
   test_VCVTSD2SS_128();
   test_VMOVD_XMM_to_IREG32();
   test_VPCMPESTRM_0x45_128();
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
   test_VCMPSD_128_0xD();
   test_VCMPSD_128_0x0();
   test_VPSHUFB_128();
   test_VCVTTSD2SI_32();
   test_VCVTTSD2SI_64();
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
   test_VMULPD_256();
   test_VMOVUPD_EtoG_128();
   test_VADDPD_256();
   test_VSUBPD_256();
   test_VDIVPD_256();
   test_VPCMPEQQ_128();
   test_VSUBPD_128();
   test_VADDPD_128();
   test_VUNPCKLPD_128();
   test_VUNPCKHPD_128();
   test_VUNPCKHPS_128();
   test_VMOVUPS_EtoG_128();
   test_VADDPS_256();
   test_VSUBPS_256();
   test_VMULPS_256();
   test_VDIVPS_256();
   test_VPCMPGTQ_128();
   test_VPEXTRQ_128_0x0();
   test_VPEXTRQ_128_0x1();
   test_VPSRLQ_0x05_128();
   test_VPMULUDQ_128();
   test_VPSLLQ_0x05_128();
   test_VPMAXUD_128();
   test_VPMINUD_128();
   test_VPMULLD_128();
   test_VPMAXUW_128();
   test_VPEXTRW_128_EregOnly_toG_0x0();
   test_VPEXTRW_128_EregOnly_toG_0x7();
   test_VPMINUW_128();
   test_VPHMINPOSUW_128();
   test_VPMAXSW_128();
   test_VPMINSW_128();
   test_VPMAXUB_128();
   test_VPEXTRB_GtoE_128_0x0();
   test_VPEXTRB_GtoE_128_0x1();
   test_VPEXTRB_GtoE_128_0x2();
   test_VPEXTRB_GtoE_128_0x3();
   test_VPEXTRB_GtoE_128_0x4();
   test_VPEXTRB_GtoE_128_0x9();
   test_VPEXTRB_GtoE_128_0xE();
   test_VPEXTRB_GtoE_128_0xF();
   test_VPMINUB_128();
   test_VPMAXSB_128();
   test_VPMINSB_128();
   test_VPERM2F128_0x00();
   test_VPERM2F128_0xFF();
   test_VPERM2F128_0x30();
   test_VPERM2F128_0x21();
   test_VPERM2F128_0x12();
   test_VPERM2F128_0x03();
   test_VPERM2F128_0x85();
   test_VPERM2F128_0x5A();
   test_VPERMILPD_256_0x0();
   test_VPERMILPD_256_0xF();
   test_VPERMILPD_256_0xA();
   test_VPERMILPD_256_0x5();
   test_VPERMILPD_128_0x0();
   test_VPERMILPD_128_0x3();
   test_VUNPCKLPD_256();
   test_VUNPCKHPD_256();
   test_VSHUFPS_0x39_256();
   test_VUNPCKLPS_256();
   test_VUNPCKHPS_256();
   test_VXORPD_256();
   test_VBROADCASTSD_256();
   test_VCMPPD_128_0x4();
   test_VCVTDQ2PD_128();
   test_VDIVPD_128();
   test_VANDPD_256();
   test_VPMOVSXBW_128();
   test_VPSUBUSW_128();
   test_VPCMPEQW_128();
   test_VPADDB_128();
   test_VMOVAPS_EtoG_256();
   test_VCVTDQ2PD_256();
   test_VMOVHPD_128_LoadForm();
   test_VCVTPD2PS_256();
   test_VPUNPCKHDQ_128();
   test_VBROADCASTSS_128();
   test_VPMOVSXDQ_128();
   test_VPMOVSXWD_128();
   test_VDIVPS_128();
   test_VANDPS_256();
   test_VXORPS_256();
   test_VORPS_256();
   test_VANDNPD_256();
   test_VANDNPS_256();
   test_VORPD_256();
   test_VPERMILPS_256_0x0F();
   test_VPERMILPS_256_0xFA();
   test_VPERMILPS_256_0xA3();
   test_VPERMILPS_256_0x5A();
   test_VPMULHW_128();
   test_VPUNPCKHQDQ_128();
   test_VPSRAW_0x05_128();
   test_VPCMPGTD_128();
   test_VPMOVZXBD_128();
   test_VPMOVSXBD_128();
   test_VPINSRB_128_1of3();
   test_VPINSRB_128_2of3();
   test_VPINSRB_128_3of3();
   test_VCOMISD_128();
   test_VCOMISS_128();
   test_VMOVUPS_YMM_to_YMMorMEM();
   test_VDPPD_128_1of4();
   test_VDPPD_128_2of4();
   test_VDPPD_128_3of4();
   test_VDPPD_128_4of4();
   test_VPINSRW_128_1of4();
   test_VPINSRW_128_2of4();
   test_VPINSRW_128_3of4();
   test_VPINSRW_128_4of4();
   test_VBROADCASTSS_256();
   test_VPALIGNR_128_1of3();
   test_VPALIGNR_128_2of3();
   test_VPALIGNR_128_3of3();
   test_VMOVSD_REG_XMM();
   test_VMOVLPD_128_M64_XMM_XMM();
   test_VMOVLPD_128_XMM_M64();
   test_VSHUFPD_128_1of2();
   test_VSHUFPD_128_2of2();
   test_VSHUFPD_256_1of2();
   test_VSHUFPD_256_2of2();
   test_VPERMILPS_128_0x00();
   test_VPERMILPS_128_0xFE();
   test_VPERMILPS_128_0x30();
   test_VPERMILPS_128_0x21();
   test_VPERMILPS_128_0xD7();
   test_VPERMILPS_128_0xB5();
   test_VPERMILPS_128_0x85();
   test_VPERMILPS_128_0x29();
   test_VBLENDPS_128_1of3();
   test_VBLENDPS_128_2of3();
   test_VBLENDPS_128_3of3();
   test_VBLENDPD_128_1of2();
   test_VBLENDPD_128_2of2();
   test_VBLENDPD_256_1of3();
   test_VBLENDPD_256_2of3();
   test_VBLENDPD_256_3of3();
   test_VPBLENDW_128_0x00();
   test_VPBLENDW_128_0xFE();
   test_VPBLENDW_128_0x30();
   test_VPBLENDW_128_0x21();
   test_VPBLENDW_128_0xD7();
   test_VPBLENDW_128_0xB5();
   test_VPBLENDW_128_0x85();
   test_VPBLENDW_128_0x29();
   return 0;
}
