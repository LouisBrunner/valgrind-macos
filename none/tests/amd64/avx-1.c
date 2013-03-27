
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

GEN_test_RandM(VCVTSD2SI_32,
               "vcvtsd2si %%xmm8,  %%r14d",
               "vcvtsd2si (%%rax), %%r14d")

GEN_test_RandM(VCVTSD2SI_64,
               "vcvtsd2si %%xmm8,  %%r14",
               "vcvtsd2si (%%rax), %%r14")

GEN_test_RandM(VPSHUFB_128,
               "vpshufb %%xmm6,  %%xmm8, %%xmm7",
               "vpshufb (%%rax), %%xmm8, %%xmm7")

GEN_test_RandM(VCMPSD_128_0x0,
               "vcmpsd $0, %%xmm6,  %%xmm8, %%xmm7",
               "vcmpsd $0, (%%rax), %%xmm8, %%xmm7")
GEN_test_RandM(VCMPSD_128_0x1,
               "vcmpsd $1, %%xmm6,  %%xmm8, %%xmm7",
               "vcmpsd $1, (%%rax), %%xmm8, %%xmm7")
GEN_test_RandM(VCMPSD_128_0x2,
               "vcmpsd $2, %%xmm6,  %%xmm8, %%xmm7",
               "vcmpsd $2, (%%rax), %%xmm8, %%xmm7")
GEN_test_RandM(VCMPSD_128_0x3,
               "vcmpsd $3, %%xmm6,  %%xmm8, %%xmm7",
               "vcmpsd $3, (%%rax), %%xmm8, %%xmm7")
GEN_test_RandM(VCMPSD_128_0x4,
               "vcmpsd $4, %%xmm6,  %%xmm8, %%xmm7",
               "vcmpsd $4, (%%rax), %%xmm8, %%xmm7")
GEN_test_RandM(VCMPSD_128_0x5,
               "vcmpsd $5, %%xmm6,  %%xmm8, %%xmm7",
               "vcmpsd $5, (%%rax), %%xmm8, %%xmm7")
GEN_test_RandM(VCMPSD_128_0x6,
               "vcmpsd $6, %%xmm6,  %%xmm8, %%xmm7",
               "vcmpsd $6, (%%rax), %%xmm8, %%xmm7")
GEN_test_RandM(VCMPSD_128_0x7,
               "vcmpsd $7, %%xmm6,  %%xmm8, %%xmm7",
               "vcmpsd $7, (%%rax), %%xmm8, %%xmm7")
GEN_test_RandM(VCMPSD_128_0x8,
               "vcmpsd $8, %%xmm6,  %%xmm8, %%xmm7",
               "vcmpsd $8, (%%rax), %%xmm8, %%xmm7")
GEN_test_RandM(VCMPSD_128_0xA,
               "vcmpsd $0xA, %%xmm6,  %%xmm8, %%xmm7",
               "vcmpsd $0xA, (%%rax), %%xmm8, %%xmm7")
GEN_test_RandM(VCMPSD_128_0xC,
               "vcmpsd $0xC, %%xmm6,  %%xmm8, %%xmm7",
               "vcmpsd $0xC, (%%rax), %%xmm8, %%xmm7")
GEN_test_RandM(VCMPSD_128_0xD,
               "vcmpsd $0xD, %%xmm6,  %%xmm8, %%xmm7",
               "vcmpsd $0xD, (%%rax), %%xmm8, %%xmm7")
GEN_test_RandM(VCMPSD_128_0xE,
               "vcmpsd $0xE, %%xmm6,  %%xmm8, %%xmm7",
               "vcmpsd $0xE, (%%rax), %%xmm8, %%xmm7")
GEN_test_RandM(VCMPSD_128_0x11,
               "vcmpsd $0x11, %%xmm6,  %%xmm8, %%xmm7",
               "vcmpsd $0x11, (%%rax), %%xmm8, %%xmm7")
GEN_test_RandM(VCMPSD_128_0x12,
               "vcmpsd $0x12, %%xmm6,  %%xmm8, %%xmm7",
               "vcmpsd $0x12, (%%rax), %%xmm8, %%xmm7")
GEN_test_RandM(VCMPSD_128_0x16,
               "vcmpsd $0x16, %%xmm6,  %%xmm8, %%xmm7",
               "vcmpsd $0x16, (%%rax), %%xmm8, %%xmm7")
GEN_test_RandM(VCMPSD_128_0x1E,
               "vcmpsd $0x1E, %%xmm6,  %%xmm8, %%xmm7",
               "vcmpsd $0x1E, (%%rax), %%xmm8, %%xmm7")

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

GEN_test_RandM(VCVTSS2SI_32,
               "vcvtss2si %%xmm8,  %%r14d",
               "vcvtss2si (%%rax), %%r14d")

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

GEN_test_Monly(VMOVNTDQ_256,
               "vmovntdq %%ymm8, (%%rax)")

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

GEN_test_RandM(VCVTSS2SI_64,
               "vcvtss2si %%xmm8,  %%r14",
               "vcvtss2si (%%rax), %%r14")

GEN_test_Ronly(VPMOVMSKB_128,
               "vpmovmskb %%xmm8, %%r14")

GEN_test_RandM(VPAND_128,
               "vpand %%xmm9,  %%xmm8, %%xmm7",
               "vpand (%%rax), %%xmm8, %%xmm7")

GEN_test_Monly(VMOVHPD_128_StoreForm,
               "vmovhpd %%xmm8, (%%rax)")

GEN_test_Monly(VMOVHPS_128_StoreForm,
               "vmovhps %%xmm8, (%%rax)")

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

GEN_test_RandM(VMAXPS_256,
               "vmaxps %%ymm9,  %%ymm8, %%ymm7",
               "vmaxps (%%rax), %%ymm8, %%ymm7")

GEN_test_RandM(VMAXPD_128,
               "vmaxpd %%xmm9,  %%xmm8, %%xmm7",
               "vmaxpd (%%rax), %%xmm8, %%xmm7")

GEN_test_RandM(VMAXPD_256,
               "vmaxpd %%ymm9,  %%ymm8, %%ymm7",
               "vmaxpd (%%rax), %%ymm8, %%ymm7")

GEN_test_RandM(VMINPS_128,
               "vminps %%xmm9,  %%xmm8, %%xmm7",
               "vminps (%%rax), %%xmm8, %%xmm7")

GEN_test_RandM(VMINPS_256,
               "vminps %%ymm9,  %%ymm8, %%ymm7",
               "vminps (%%rax), %%ymm8, %%ymm7")

GEN_test_RandM(VMINPD_128,
               "vminpd %%xmm9,  %%xmm8, %%xmm7",
               "vminpd (%%rax), %%xmm8, %%xmm7")

GEN_test_RandM(VMINPD_256,
               "vminpd %%ymm9,  %%ymm8, %%ymm7",
               "vminpd (%%rax), %%ymm8, %%ymm7")

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

GEN_test_Ronly(VPSRAD_0x05_128,
               "vpsrad $0x5, %%xmm9,  %%xmm7")

GEN_test_RandM(VPSUBUSB_128,
               "vpsubusb %%xmm9,  %%xmm8, %%xmm7",
               "vpsubusb (%%rax), %%xmm8, %%xmm7")

GEN_test_RandM(VPSUBSB_128,
               "vpsubsb %%xmm9,  %%xmm8, %%xmm7",
               "vpsubsb (%%rax), %%xmm8, %%xmm7")

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

GEN_test_Ronly(VPSLLW_0x05_128,
               "vpsllw $0x5, %%xmm9,  %%xmm7")

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
GEN_test_RandM(VCMPSS_128_0x8,
               "vcmpss $8, %%xmm6,  %%xmm8, %%xmm7",
               "vcmpss $8, (%%rax), %%xmm8, %%xmm7")
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
GEN_test_RandM(VCMPSS_128_0x11,
               "vcmpss $0x11, %%xmm6,  %%xmm8, %%xmm7",
               "vcmpss $0x11, (%%rax), %%xmm8, %%xmm7")
GEN_test_RandM(VCMPSS_128_0x12,
               "vcmpss $0x12, %%xmm6,  %%xmm8, %%xmm7",
               "vcmpss $0x12, (%%rax), %%xmm8, %%xmm7")
GEN_test_RandM(VCMPSS_128_0x16,
               "vcmpss $0x16, %%xmm6,  %%xmm8, %%xmm7",
               "vcmpss $0x16, (%%rax), %%xmm8, %%xmm7")
GEN_test_RandM(VCMPSS_128_0x1E,
               "vcmpss $0x1E, %%xmm6,  %%xmm8, %%xmm7",
               "vcmpss $0x1E, (%%rax), %%xmm8, %%xmm7")

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

GEN_test_RandM(VPMULDQ_128,
               "vpmuldq %%xmm6,  %%xmm8, %%xmm7",
               "vpmuldq (%%rax), %%xmm8, %%xmm7")

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

GEN_test_RandM(VCMPPD_256_0x4,
               "vcmppd $4, %%ymm6,  %%ymm8, %%ymm7",
               "vcmppd $4, (%%rax), %%ymm8, %%ymm7")

GEN_test_RandM(VCMPPS_128_0x4,
               "vcmpps $4, %%xmm6,  %%xmm8, %%xmm7",
               "vcmpps $4, (%%rax), %%xmm8, %%xmm7")

GEN_test_RandM(VCMPPS_256_0x4,
               "vcmpps $4, %%ymm6,  %%ymm8, %%ymm7",
               "vcmpps $4, (%%rax), %%ymm8, %%ymm7")

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

GEN_test_RandM(VPSUBSW_128,
               "vpsubsw %%xmm9,  %%xmm8, %%xmm7",
               "vpsubsw (%%rax), %%xmm8, %%xmm7")

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

GEN_test_Monly(VMOVHPS_128_LoadForm,
               "vmovhps (%%rax), %%xmm8, %%xmm7")

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

GEN_test_RandM(VPCMPGTB_128,
               "vpcmpgtb %%xmm6,  %%xmm8, %%xmm7",
               "vpcmpgtb (%%rax), %%xmm8, %%xmm7")

GEN_test_RandM(VPCMPGTW_128,
               "vpcmpgtw %%xmm6,  %%xmm8, %%xmm7",
               "vpcmpgtw (%%rax), %%xmm8, %%xmm7")

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

GEN_test_RandM(VDPPS_128_1of4,
               "vdpps $0x00, %%xmm6,  %%xmm8, %%xmm7",
               "vdpps $0xA5, (%%rax), %%xmm9, %%xmm6")
GEN_test_RandM(VDPPS_128_2of4,
               "vdpps $0x5A, %%xmm6,  %%xmm8, %%xmm7",
               "vdpps $0xFF, (%%rax), %%xmm9, %%xmm6")
GEN_test_RandM(VDPPS_128_3of4,
               "vdpps $0x0F, %%xmm6,  %%xmm8, %%xmm7",
               "vdpps $0x37, (%%rax), %%xmm9, %%xmm6")
GEN_test_RandM(VDPPS_128_4of4,
               "vdpps $0xF0, %%xmm6,  %%xmm8, %%xmm7",
               "vdpps $0x73, (%%rax), %%xmm9, %%xmm6")

GEN_test_RandM(VDPPS_256_1of4,
               "vdpps $0x00, %%ymm6,  %%ymm8, %%ymm7",
               "vdpps $0xA5, (%%rax), %%ymm9, %%ymm6")
GEN_test_RandM(VDPPS_256_2of4,
               "vdpps $0x5A, %%ymm6,  %%ymm8, %%ymm7",
               "vdpps $0xFF, (%%rax), %%ymm9, %%ymm6")
GEN_test_RandM(VDPPS_256_3of4,
               "vdpps $0x0F, %%ymm6,  %%ymm8, %%ymm7",
               "vdpps $0x37, (%%rax), %%ymm9, %%ymm6")
GEN_test_RandM(VDPPS_256_4of4,
               "vdpps $0xF0, %%ymm6,  %%ymm8, %%ymm7",
               "vdpps $0x73, (%%rax), %%ymm9, %%ymm6")

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

GEN_test_Ronly(VMOVSS_REG_XMM, "vmovss %%xmm9, %%xmm7, %%xmm8")

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

GEN_test_RandM(VMOVUPS_EtoG_256,
               "vmovups %%ymm6,  %%ymm9",
               "vmovups (%%rax), %%ymm7")

GEN_test_RandM(VSQRTSS_128,
               "vsqrtss %%xmm6,  %%xmm8, %%xmm7",
               "vsqrtss (%%rax), %%xmm8, %%xmm7")

GEN_test_RandM(VSQRTPS_128,
               "vsqrtps %%xmm6,  %%xmm8",
               "vsqrtps (%%rax), %%xmm8")

GEN_test_RandM(VSQRTPS_256,
               "vsqrtps %%ymm6,  %%ymm8",
               "vsqrtps (%%rax), %%ymm8")

GEN_test_RandM(VSQRTPD_128,
               "vsqrtpd %%xmm6,  %%xmm8",
               "vsqrtpd (%%rax), %%xmm8")

GEN_test_RandM(VSQRTPD_256,
               "vsqrtpd %%ymm6,  %%ymm8",
               "vsqrtpd (%%rax), %%ymm8")

GEN_test_RandM(VRSQRTSS_128,
               "vrsqrtss %%xmm6,  %%xmm8, %%xmm7",
               "vrsqrtss (%%rax), %%xmm8, %%xmm7")

GEN_test_RandM(VRSQRTPS_128,
               "vrsqrtps %%xmm6,  %%xmm8",
               "vrsqrtps (%%rax), %%xmm8")

GEN_test_RandM(VRSQRTPS_256,
               "vrsqrtps %%ymm6,  %%ymm8",
               "vrsqrtps (%%rax), %%ymm8")

GEN_test_RandM(VMOVDQU_GtoE_256,
               "vmovdqu %%ymm9,  %%ymm6",
               "vmovdqu %%ymm7, (%%rax)")

GEN_test_RandM(VCVTPS2PD_256,
               "vcvtps2pd %%xmm9,  %%ymm6",
               "vcvtps2pd (%%rax), %%ymm7")

GEN_test_RandM(VCVTTPS2DQ_128,
               "vcvttps2dq %%xmm9,  %%xmm6",
               "vcvttps2dq (%%rax), %%xmm7")

GEN_test_RandM(VCVTTPS2DQ_256,
               "vcvttps2dq %%ymm9,  %%ymm6",
               "vcvttps2dq (%%rax), %%ymm7")

GEN_test_RandM(VCVTDQ2PS_128,
               "vcvtdq2ps %%xmm9,  %%xmm6",
               "vcvtdq2ps (%%rax), %%xmm7")

GEN_test_RandM(VCVTDQ2PS_256,
               "vcvtdq2ps %%ymm9,  %%ymm6",
               "vcvtdq2ps (%%rax), %%ymm7")

GEN_test_RandM(VCVTTPD2DQ_128,
               "vcvttpd2dqx %%xmm9,  %%xmm6",
               "vcvttpd2dqx (%%rax), %%xmm7")

GEN_test_RandM(VCVTTPD2DQ_256,
               "vcvttpd2dqy %%ymm9,  %%xmm6",
               "vcvttpd2dqy (%%rax), %%xmm7")

GEN_test_RandM(VCVTPD2DQ_128,
               "vcvtpd2dqx %%xmm9,  %%xmm6",
               "vcvtpd2dqx (%%rax), %%xmm7")

GEN_test_RandM(VCVTPD2DQ_256,
               "vcvtpd2dqy %%ymm9,  %%xmm6",
               "vcvtpd2dqy (%%rax), %%xmm7")

GEN_test_RandM(VMOVSLDUP_128,
               "vmovsldup %%xmm9,  %%xmm6",
               "vmovsldup (%%rax), %%xmm7")

GEN_test_RandM(VMOVSLDUP_256,
               "vmovsldup %%ymm9,  %%ymm6",
               "vmovsldup (%%rax), %%ymm7")

GEN_test_RandM(VMOVSHDUP_128,
               "vmovshdup %%xmm9,  %%xmm6",
               "vmovshdup (%%rax), %%xmm7")

GEN_test_RandM(VMOVSHDUP_256,
               "vmovshdup %%ymm9,  %%ymm6",
               "vmovshdup (%%rax), %%ymm7")

GEN_test_RandM(VPERMILPS_VAR_128,
               "vpermilps %%xmm6,  %%xmm8, %%xmm7",
               "vpermilps (%%rax), %%xmm8, %%xmm7")

GEN_test_RandM(VPERMILPD_VAR_128,
               "vpermilpd %%xmm6,  %%xmm8, %%xmm7",
               "vpermilpd (%%rax), %%xmm8, %%xmm7")

GEN_test_RandM(VPERMILPS_VAR_256,
               "vpermilps %%ymm6,  %%ymm8, %%ymm7",
               "vpermilps (%%rax), %%ymm8, %%ymm7")

GEN_test_RandM(VPERMILPD_VAR_256,
               "vpermilpd %%ymm6,  %%ymm8, %%ymm7",
               "vpermilpd (%%rax), %%ymm8, %%ymm7")

GEN_test_RandM(VPSLLW_128,
               "andl $15, %%r14d;"
               "vmovd %%r14d, %%xmm6;"
               "vpsllw %%xmm6,     %%xmm8, %%xmm9",
               "andq $15, 128(%%rax);"
               "vpsllw 128(%%rax), %%xmm8, %%xmm9")

GEN_test_RandM(VPSRLW_128,
               "andl $15, %%r14d;"
               "vmovd %%r14d, %%xmm6;"
               "vpsrlw %%xmm6,     %%xmm8, %%xmm9",
               "andq $15, 128(%%rax);"
               "vpsrlw 128(%%rax), %%xmm8, %%xmm9")

GEN_test_RandM(VPSRAW_128,
               "andl $31, %%r14d;"
               "vmovd %%r14d, %%xmm6;"
               "vpsraw %%xmm6,     %%xmm8, %%xmm9",
               "andq $15, 128(%%rax);"
               "vpsraw 128(%%rax), %%xmm8, %%xmm9")

GEN_test_RandM(VPSLLD_128,
               "andl $31, %%r14d;"
               "vmovd %%r14d, %%xmm6;"
               "vpslld %%xmm6,     %%xmm8, %%xmm9",
               "andq $31, 128(%%rax);"
               "vpslld 128(%%rax), %%xmm8, %%xmm9")

GEN_test_RandM(VPSRLD_128,
               "andl $31, %%r14d;"
               "vmovd %%r14d, %%xmm6;"
               "vpsrld %%xmm6,     %%xmm8, %%xmm9",
               "andq $31, 128(%%rax);"
               "vpsrld 128(%%rax), %%xmm8, %%xmm9")

GEN_test_RandM(VPSRAD_128,
               "andl $31, %%r14d;"
               "vmovd %%r14d, %%xmm6;"
               "vpsrad %%xmm6,     %%xmm8, %%xmm9",
               "andq $31, 128(%%rax);"
               "vpsrad 128(%%rax), %%xmm8, %%xmm9")

GEN_test_RandM(VPSLLQ_128,
               "andl $63, %%r14d;"
               "vmovd %%r14d, %%xmm6;"
               "vpsllq %%xmm6,     %%xmm8, %%xmm9",
               "andq $63, 128(%%rax);"
               "vpsllq 128(%%rax), %%xmm8, %%xmm9")

GEN_test_RandM(VPSRLQ_128,
               "andl $63, %%r14d;"
               "vmovd %%r14d, %%xmm6;"
               "vpsrlq %%xmm6,     %%xmm8, %%xmm9",
               "andq $63, 128(%%rax);"
               "vpsrlq 128(%%rax), %%xmm8, %%xmm9")

GEN_test_RandM(VROUNDPS_128_0x0,
               "vroundps $0x0, %%xmm8,  %%xmm9",
               "vroundps $0x0, (%%rax), %%xmm9")
GEN_test_RandM(VROUNDPS_128_0x1,
               "vroundps $0x1, %%xmm8,  %%xmm9",
               "vroundps $0x1, (%%rax), %%xmm9")
GEN_test_RandM(VROUNDPS_128_0x2,
               "vroundps $0x2, %%xmm8,  %%xmm9",
               "vroundps $0x2, (%%rax), %%xmm9")
GEN_test_RandM(VROUNDPS_128_0x3,
               "vroundps $0x3, %%xmm8,  %%xmm9",
               "vroundps $0x3, (%%rax), %%xmm9")
GEN_test_RandM(VROUNDPS_128_0x4,
               "vroundps $0x4, %%xmm8,  %%xmm9",
               "vroundps $0x4, (%%rax), %%xmm9")

GEN_test_RandM(VROUNDPS_256_0x0,
               "vroundps $0x0, %%ymm8,  %%ymm9",
               "vroundps $0x0, (%%rax), %%ymm9")
GEN_test_RandM(VROUNDPS_256_0x1,
               "vroundps $0x1, %%ymm8,  %%ymm9",
               "vroundps $0x1, (%%rax), %%ymm9")
GEN_test_RandM(VROUNDPS_256_0x2,
               "vroundps $0x2, %%ymm8,  %%ymm9",
               "vroundps $0x2, (%%rax), %%ymm9")
GEN_test_RandM(VROUNDPS_256_0x3,
               "vroundps $0x3, %%ymm8,  %%ymm9",
               "vroundps $0x3, (%%rax), %%ymm9")
GEN_test_RandM(VROUNDPS_256_0x4,
               "vroundps $0x4, %%ymm8,  %%ymm9",
               "vroundps $0x4, (%%rax), %%ymm9")

GEN_test_RandM(VROUNDPD_128_0x0,
               "vroundpd $0x0, %%xmm8,  %%xmm9",
               "vroundpd $0x0, (%%rax), %%xmm9")
GEN_test_RandM(VROUNDPD_128_0x1,
               "vroundpd $0x1, %%xmm8,  %%xmm9",
               "vroundpd $0x1, (%%rax), %%xmm9")
GEN_test_RandM(VROUNDPD_128_0x2,
               "vroundpd $0x2, %%xmm8,  %%xmm9",
               "vroundpd $0x2, (%%rax), %%xmm9")
GEN_test_RandM(VROUNDPD_128_0x3,
               "vroundpd $0x3, %%xmm8,  %%xmm9",
               "vroundpd $0x3, (%%rax), %%xmm9")
GEN_test_RandM(VROUNDPD_128_0x4,
               "vroundpd $0x4, %%xmm8,  %%xmm9",
               "vroundpd $0x4, (%%rax), %%xmm9")

GEN_test_RandM(VROUNDPD_256_0x0,
               "vroundpd $0x0, %%ymm8,  %%ymm9",
               "vroundpd $0x0, (%%rax), %%ymm9")
GEN_test_RandM(VROUNDPD_256_0x1,
               "vroundpd $0x1, %%ymm8,  %%ymm9",
               "vroundpd $0x1, (%%rax), %%ymm9")
GEN_test_RandM(VROUNDPD_256_0x2,
               "vroundpd $0x2, %%ymm8,  %%ymm9",
               "vroundpd $0x2, (%%rax), %%ymm9")
GEN_test_RandM(VROUNDPD_256_0x3,
               "vroundpd $0x3, %%ymm8,  %%ymm9",
               "vroundpd $0x3, (%%rax), %%ymm9")
GEN_test_RandM(VROUNDPD_256_0x4,
               "vroundpd $0x4, %%ymm8,  %%ymm9",
               "vroundpd $0x4, (%%rax), %%ymm9")

GEN_test_RandM(VPMADDWD_128,
               "vpmaddwd %%xmm6,  %%xmm8, %%xmm7",
               "vpmaddwd (%%rax), %%xmm8, %%xmm7")

GEN_test_RandM(VADDSUBPS_128,
               "vaddsubps %%xmm6,  %%xmm8, %%xmm7",
               "vaddsubps (%%rax), %%xmm8, %%xmm7")

GEN_test_RandM(VADDSUBPS_256,
               "vaddsubps %%ymm6,  %%ymm8, %%ymm7",
               "vaddsubps (%%rax), %%ymm8, %%ymm7")

GEN_test_RandM(VADDSUBPD_128,
               "vaddsubpd %%xmm6,  %%xmm8, %%xmm7",
               "vaddsubpd (%%rax), %%xmm8, %%xmm7")

GEN_test_RandM(VADDSUBPD_256,
               "vaddsubpd %%ymm6,  %%ymm8, %%ymm7",
               "vaddsubpd (%%rax), %%ymm8, %%ymm7")

GEN_test_RandM(VROUNDSS_0x0,
               "vroundss $0x0, %%xmm8,  %%xmm6, %%xmm9",
               "vroundss $0x0, (%%rax), %%xmm6, %%xmm9")
GEN_test_RandM(VROUNDSS_0x1,
               "vroundss $0x1, %%xmm8,  %%xmm6, %%xmm9",
               "vroundss $0x1, (%%rax), %%xmm6, %%xmm9")
GEN_test_RandM(VROUNDSS_0x2,
               "vroundss $0x2, %%xmm8,  %%xmm6, %%xmm9",
               "vroundss $0x2, (%%rax), %%xmm6, %%xmm9")
GEN_test_RandM(VROUNDSS_0x3,
               "vroundss $0x3, %%xmm8,  %%xmm6, %%xmm9",
               "vroundss $0x3, (%%rax), %%xmm6, %%xmm9")
GEN_test_RandM(VROUNDSS_0x4,
               "vroundss $0x4, %%xmm8,  %%xmm6, %%xmm9",
               "vroundss $0x4, (%%rax), %%xmm6, %%xmm9")
GEN_test_RandM(VROUNDSS_0x5,
               "vroundss $0x5, %%xmm8,  %%xmm6, %%xmm9",
               "vroundss $0x5, (%%rax), %%xmm6, %%xmm9")

GEN_test_RandM(VROUNDSD_0x0,
               "vroundsd $0x0, %%xmm8,  %%xmm6, %%xmm9",
               "vroundsd $0x0, (%%rax), %%xmm6, %%xmm9")
GEN_test_RandM(VROUNDSD_0x1,
               "vroundsd $0x1, %%xmm8,  %%xmm6, %%xmm9",
               "vroundsd $0x1, (%%rax), %%xmm6, %%xmm9")
GEN_test_RandM(VROUNDSD_0x2,
               "vroundsd $0x2, %%xmm8,  %%xmm6, %%xmm9",
               "vroundsd $0x2, (%%rax), %%xmm6, %%xmm9")
GEN_test_RandM(VROUNDSD_0x3,
               "vroundsd $0x3, %%xmm8,  %%xmm6, %%xmm9",
               "vroundsd $0x3, (%%rax), %%xmm6, %%xmm9")
GEN_test_RandM(VROUNDSD_0x4,
               "vroundsd $0x4, %%xmm8,  %%xmm6, %%xmm9",
               "vroundsd $0x4, (%%rax), %%xmm6, %%xmm9")
GEN_test_RandM(VROUNDSD_0x5,
               "vroundsd $0x5, %%xmm8,  %%xmm6, %%xmm9",
               "vroundsd $0x5, (%%rax), %%xmm6, %%xmm9")

GEN_test_RandM(VPTEST_128_1,
   "vptest %%xmm6,  %%xmm8; "
      "pushfq; popq %%r14; andq $0x8D5, %%r14",
   "vptest (%%rax), %%xmm8; "
      "pushfq; popq %%r14; andq $0x8D5, %%r14")

/* Here we ignore the boilerplate-supplied data and try to do
   x AND x   and   x AND NOT x.  Not a great test but better
   than nothing. */
GEN_test_RandM(VPTEST_128_2,
   "vmovups %%xmm6, %%xmm8;"
   "vptest %%xmm6,  %%xmm8; "
      "pushfq; popq %%r14; andq $0x8D5, %%r14",
   "vmovups (%%rax), %%xmm8;"
   "vcmpeqpd %%xmm8,%%xmm8,%%xmm7;"
   "vxorpd %%xmm8,%%xmm7,%%xmm8;"
   "vptest (%%rax), %%xmm8; "
      "pushfq; popq %%r14; andq $0x8D5, %%r14")

GEN_test_RandM(VPTEST_256_1,
   "vptest %%ymm6,  %%ymm8; "
      "pushfq; popq %%r14; andq $0x8D5, %%r14",
   "vptest (%%rax), %%ymm8; "
      "pushfq; popq %%r14; andq $0x8D5, %%r14")

/* Here we ignore the boilerplate-supplied data and try to do
   x AND x   and   x AND NOT x.  Not a great test but better
   than nothing. */
GEN_test_RandM(VPTEST_256_2,
   "vmovups %%ymm6, %%ymm8;"
   "vptest %%ymm6,  %%ymm8; "
      "pushfq; popq %%r14; andq $0x8D5, %%r14",
   "vmovups (%%rax), %%ymm8;"
   "vcmpeqpd %%xmm8,%%xmm8,%%xmm7;"
   "subq $1024, %%rsp;"
   "vmovups %%xmm7,512(%%rsp);"
   "vmovups %%xmm7,528(%%rsp);"
   "vmovups 512(%%rsp), %%ymm7;"
   "addq $1024, %%rsp;"
   "vxorpd %%ymm8,%%ymm7,%%ymm8;"
   "vptest (%%rax), %%ymm8; "
      "pushfq; popq %%r14; andq $0x8D5, %%r14")


/* VTESTPS/VTESTPD: test once with all-0 operands, once with
   one all-0s and one all 1s, and once with random data. */

GEN_test_RandM(VTESTPS_128_1,
   "vtestps %%xmm6,  %%xmm8; "
      "pushfq; popq %%r14; andq $0x8D5, %%r14",
   "vtestps (%%rax), %%xmm8; "
      "pushfq; popq %%r14; andq $0x8D5, %%r14")

/* Here we ignore the boilerplate-supplied data and try to do
   x AND x   and   x AND NOT x.  Not a great test but better
   than nothing. */
GEN_test_RandM(VTESTPS_128_2,
   "vmovups %%xmm6, %%xmm8;"
   "vtestps %%xmm6,  %%xmm8; "
      "pushfq; popq %%r14; andq $0x8D5, %%r14",
   "vmovups (%%rax), %%xmm8;"
   "vcmpeqpd %%xmm8,%%xmm8,%%xmm7;"
   "vxorpd %%xmm8,%%xmm7,%%xmm8;"
   "vtestps (%%rax), %%xmm8; "
      "pushfq; popq %%r14; andq $0x8D5, %%r14")

GEN_test_RandM(VTESTPS_128_3,
               "vtestps %%xmm8,  %%xmm9; "
                  "pushfq; popq %%r14; andq $0x8D5, %%r14",
               "vtestps (%%rax), %%xmm9; "
                  "pushfq; popq %%r14; andq $0x8D5, %%r14")




GEN_test_RandM(VTESTPS_256_1,
   "vtestps %%ymm6,  %%ymm8; "
      "pushfq; popq %%r14; andq $0x8D5, %%r14",
   "vtestps (%%rax), %%ymm8; "
      "pushfq; popq %%r14; andq $0x8D5, %%r14")

/* Here we ignore the boilerplate-supplied data and try to do
   x AND x   and   x AND NOT x.  Not a great test but better
   than nothing. */
GEN_test_RandM(VTESTPS_256_2,
   "vmovups %%ymm6, %%ymm8;"
   "vtestps %%ymm6,  %%ymm8; "
      "pushfq; popq %%r14; andq $0x8D5, %%r14",
   "vmovups (%%rax), %%ymm8;"
   "vcmpeqpd %%xmm8,%%xmm8,%%xmm7;"
   "subq $1024, %%rsp;"
   "vmovups %%xmm7,512(%%rsp);"
   "vmovups %%xmm7,528(%%rsp);"
   "vmovups 512(%%rsp), %%ymm7;"
   "addq $1024, %%rsp;"
   "vxorpd %%ymm8,%%ymm7,%%ymm8;"
   "vtestps (%%rax), %%ymm8; "
      "pushfq; popq %%r14; andq $0x8D5, %%r14")

GEN_test_RandM(VTESTPS_256_3,
               "vtestps %%ymm8,  %%ymm9; "
                  "pushfq; popq %%r14; andq $0x8D5, %%r14",
               "vtestps (%%rax), %%ymm9; "
                  "pushfq; popq %%r14; andq $0x8D5, %%r14")



GEN_test_RandM(VTESTPD_128_1,
   "vtestpd %%xmm6,  %%xmm8; "
      "pushfq; popq %%r14; andq $0x8D5, %%r14",
   "vtestpd (%%rax), %%xmm8; "
      "pushfq; popq %%r14; andq $0x8D5, %%r14")

/* Here we ignore the boilerplate-supplied data and try to do
   x AND x   and   x AND NOT x.  Not a great test but better
   than nothing. */
GEN_test_RandM(VTESTPD_128_2,
   "vmovups %%xmm6, %%xmm8;"
   "vtestpd %%xmm6,  %%xmm8; "
      "pushfq; popq %%r14; andq $0x8D5, %%r14",
   "vmovups (%%rax), %%xmm8;"
   "vcmpeqpd %%xmm8,%%xmm8,%%xmm7;"
   "vxorpd %%xmm8,%%xmm7,%%xmm8;"
   "vtestpd (%%rax), %%xmm8; "
      "pushfq; popq %%r14; andq $0x8D5, %%r14")

GEN_test_RandM(VTESTPD_128_3,
               "vtestpd %%xmm8,  %%xmm9; "
                  "pushfq; popq %%r14; andq $0x8D5, %%r14",
               "vtestpd (%%rax), %%xmm9; "
                  "pushfq; popq %%r14; andq $0x8D5, %%r14")




GEN_test_RandM(VTESTPD_256_1,
   "vtestpd %%ymm6,  %%ymm8; "
      "pushfq; popq %%r14; andq $0x8D5, %%r14",
   "vtestpd (%%rax), %%ymm8; "
      "pushfq; popq %%r14; andq $0x8D5, %%r14")

/* Here we ignore the boilerplate-supplied data and try to do
   x AND x   and   x AND NOT x.  Not a great test but better
   than nothing. */
GEN_test_RandM(VTESTPD_256_2,
   "vmovups %%ymm6, %%ymm8;"
   "vtestpd %%ymm6,  %%ymm8; "
      "pushfq; popq %%r14; andq $0x8D5, %%r14",
   "vmovups (%%rax), %%ymm8;"
   "vcmpeqpd %%xmm8,%%xmm8,%%xmm7;"
   "subq $1024, %%rsp;"
   "vmovups %%xmm7,512(%%rsp);"
   "vmovups %%xmm7,528(%%rsp);"
   "vmovups 512(%%rsp), %%ymm7;"
   "addq $1024, %%rsp;"
   "vxorpd %%ymm8,%%ymm7,%%ymm8;"
   "vtestpd (%%rax), %%ymm8; "
      "pushfq; popq %%r14; andq $0x8D5, %%r14")

GEN_test_RandM(VTESTPD_256_3,
               "vtestpd %%ymm8,  %%ymm9; "
                  "pushfq; popq %%r14; andq $0x8D5, %%r14",
               "vtestpd (%%rax), %%ymm9; "
                  "pushfq; popq %%r14; andq $0x8D5, %%r14")

GEN_test_RandM(VBLENDVPS_128,
               "vblendvps %%xmm9, %%xmm6,  %%xmm8, %%xmm7",
               "vblendvps %%xmm9, (%%rax), %%xmm8, %%xmm7")

GEN_test_RandM(VBLENDVPS_256,
               "vblendvps %%ymm9, %%ymm6,  %%ymm8, %%ymm7",
               "vblendvps %%ymm9, (%%rax), %%ymm8, %%ymm7")

GEN_test_RandM(VBLENDVPD_128,
               "vblendvpd %%xmm9, %%xmm6,  %%xmm8, %%xmm7",
               "vblendvpd %%xmm9, (%%rax), %%xmm8, %%xmm7")

GEN_test_RandM(VBLENDVPD_256,
               "vblendvpd %%ymm9, %%ymm6,  %%ymm8, %%ymm7",
               "vblendvpd %%ymm9, (%%rax), %%ymm8, %%ymm7")


GEN_test_RandM(VHADDPS_128,
               "vhaddps %%xmm6,  %%xmm8, %%xmm7",
               "vhaddps (%%rax), %%xmm8, %%xmm7")

GEN_test_RandM(VHADDPS_256,
               "vhaddps %%ymm6,  %%ymm8, %%ymm7",
               "vhaddps (%%rax), %%ymm8, %%ymm7")

GEN_test_RandM(VHADDPD_128,
               "vhaddpd %%xmm6,  %%xmm8, %%xmm7",
               "vhaddpd (%%rax), %%xmm8, %%xmm7")

GEN_test_RandM(VHADDPD_256,
               "vhaddpd %%ymm6,  %%ymm8, %%ymm7",
               "vhaddpd (%%rax), %%ymm8, %%ymm7")

GEN_test_RandM(VHSUBPS_128,
               "vhsubps %%xmm6,  %%xmm8, %%xmm7",
               "vhsubps (%%rax), %%xmm8, %%xmm7")

GEN_test_RandM(VHSUBPS_256,
               "vhsubps %%ymm6,  %%ymm8, %%ymm7",
               "vhsubps (%%rax), %%ymm8, %%ymm7")

GEN_test_RandM(VHSUBPD_128,
               "vhsubpd %%xmm6,  %%xmm8, %%xmm7",
               "vhsubpd (%%rax), %%xmm8, %%xmm7")

GEN_test_RandM(VHSUBPD_256,
               "vhsubpd %%ymm6,  %%ymm8, %%ymm7",
               "vhsubpd (%%rax), %%ymm8, %%ymm7")

GEN_test_RandM(VEXTRACTPS_0x0,
               "vextractps $0, %%xmm8, %%r14d",
               "vextractps $0, %%xmm8, (%%rax)")

GEN_test_RandM(VEXTRACTPS_0x1,
               "vextractps $1, %%xmm8, %%r14d",
               "vextractps $1, %%xmm8, (%%rax)")

GEN_test_RandM(VEXTRACTPS_0x2,
               "vextractps $2, %%xmm8, %%r14d",
               "vextractps $2, %%xmm8, (%%rax)")

GEN_test_RandM(VEXTRACTPS_0x3,
               "vextractps $3, %%xmm8, %%r14d",
               "vextractps $3, %%xmm8, (%%rax)")

GEN_test_Monly(VLDDQU_128,
               "vlddqu 1(%%rax), %%xmm8")

GEN_test_Monly(VLDDQU_256,
               "vlddqu 1(%%rax), %%ymm8")

GEN_test_Monly(VMOVNTDQA_128,
               "vmovntdqa (%%rax), %%xmm9")

GEN_test_Monly(VMASKMOVDQU_128,
               "xchgq %%rax, %%rdi;"
               "vmaskmovdqu %%xmm8, %%xmm9;"
               "xchgq %%rax, %%rdi")

GEN_test_Ronly(VMOVMSKPD_128,
               "vmovmskpd %%xmm9, %%r14d")

GEN_test_Ronly(VMOVMSKPD_256,
               "vmovmskpd %%ymm9, %%r14d")

GEN_test_Ronly(VMOVMSKPS_128,
               "vmovmskps %%xmm9, %%r14d")

GEN_test_Ronly(VMOVMSKPS_256,
               "vmovmskps %%ymm9, %%r14d")

GEN_test_Monly(VMOVNTPD_128,
               "vmovntpd %%xmm9, (%%rax)")

GEN_test_Monly(VMOVNTPD_256,
               "vmovntpd %%ymm9, (%%rax)")

GEN_test_Monly(VMOVNTPS_128,
               "vmovntps %%xmm9, (%%rax)")

GEN_test_Monly(VMOVNTPS_256,
               "vmovntps %%ymm9, (%%rax)")

GEN_test_RandM(VPACKSSWB_128,
               "vpacksswb %%xmm6,  %%xmm8, %%xmm7",
               "vpacksswb (%%rax), %%xmm8, %%xmm7")

GEN_test_RandM(VPAVGB_128,
               "vpavgb %%xmm6,  %%xmm8, %%xmm7",
               "vpavgb (%%rax), %%xmm8, %%xmm7")

GEN_test_RandM(VPAVGW_128,
               "vpavgw %%xmm6,  %%xmm8, %%xmm7",
               "vpavgw (%%rax), %%xmm8, %%xmm7")

GEN_test_RandM(VPADDSB_128,
               "vpaddsb %%xmm6,  %%xmm8, %%xmm7",
               "vpaddsb (%%rax), %%xmm8, %%xmm7")

GEN_test_RandM(VPADDSW_128,
               "vpaddsw %%xmm6,  %%xmm8, %%xmm7",
               "vpaddsw (%%rax), %%xmm8, %%xmm7")

GEN_test_RandM(VPHADDW_128,
               "vphaddw %%xmm6,  %%xmm8, %%xmm7",
               "vphaddw (%%rax), %%xmm8, %%xmm7")

GEN_test_RandM(VPHADDD_128,
               "vphaddd %%xmm6,  %%xmm8, %%xmm7",
               "vphaddd (%%rax), %%xmm8, %%xmm7")

GEN_test_RandM(VPHADDSW_128,
               "vphaddsw %%xmm6,  %%xmm8, %%xmm7",
               "vphaddsw (%%rax), %%xmm8, %%xmm7")

GEN_test_RandM(VPMADDUBSW_128,
               "vpmaddubsw %%xmm6,  %%xmm8, %%xmm7",
               "vpmaddubsw (%%rax), %%xmm8, %%xmm7")

GEN_test_RandM(VPHSUBW_128,
               "vphsubw %%xmm6,  %%xmm8, %%xmm7",
               "vphsubw (%%rax), %%xmm8, %%xmm7")

GEN_test_RandM(VPHSUBD_128,
               "vphsubd %%xmm6,  %%xmm8, %%xmm7",
               "vphsubd (%%rax), %%xmm8, %%xmm7")

GEN_test_RandM(VPHSUBSW_128,
               "vphsubsw %%xmm6,  %%xmm8, %%xmm7",
               "vphsubsw (%%rax), %%xmm8, %%xmm7")

GEN_test_RandM(VPABSB_128,
               "vpabsb %%xmm6,  %%xmm7",
               "vpabsb (%%rax), %%xmm7")

GEN_test_RandM(VPABSW_128,
               "vpabsw %%xmm6,  %%xmm7",
               "vpabsw (%%rax), %%xmm7")

GEN_test_RandM(VPMOVSXBQ_128,
               "vpmovsxbq %%xmm6,  %%xmm8",
               "vpmovsxbq (%%rax), %%xmm8")

GEN_test_RandM(VPMOVSXWQ_128,
               "vpmovsxwq %%xmm6,  %%xmm8",
               "vpmovsxwq (%%rax), %%xmm8")

GEN_test_RandM(VPACKUSDW_128,
               "vpackusdw %%xmm6,  %%xmm8, %%xmm7",
               "vpackusdw (%%rax), %%xmm8, %%xmm7")

GEN_test_RandM(VPMOVZXBQ_128,
               "vpmovzxbq %%xmm6,  %%xmm8",
               "vpmovzxbq (%%rax), %%xmm8")

GEN_test_RandM(VPMOVZXWQ_128,
               "vpmovzxwq %%xmm6,  %%xmm8",
               "vpmovzxwq (%%rax), %%xmm8")

GEN_test_RandM(VPMOVZXDQ_128,
               "vpmovzxdq %%xmm6,  %%xmm8",
               "vpmovzxdq (%%rax), %%xmm8")

GEN_test_RandM(VMPSADBW_128_0x0,
               "vmpsadbw $0, %%xmm6,  %%xmm8, %%xmm7",
               "vmpsadbw $0, (%%rax), %%xmm8, %%xmm7")
GEN_test_RandM(VMPSADBW_128_0x1,
               "vmpsadbw $1, %%xmm6,  %%xmm8, %%xmm7",
               "vmpsadbw $1, (%%rax), %%xmm8, %%xmm7")
GEN_test_RandM(VMPSADBW_128_0x2,
               "vmpsadbw $2, %%xmm6,  %%xmm8, %%xmm7",
               "vmpsadbw $2, (%%rax), %%xmm8, %%xmm7")
GEN_test_RandM(VMPSADBW_128_0x3,
               "vmpsadbw $3, %%xmm6,  %%xmm8, %%xmm7",
               "vmpsadbw $3, (%%rax), %%xmm8, %%xmm7")
GEN_test_RandM(VMPSADBW_128_0x4,
               "vmpsadbw $4, %%xmm6,  %%xmm8, %%xmm7",
               "vmpsadbw $4, (%%rax), %%xmm8, %%xmm7")
GEN_test_RandM(VMPSADBW_128_0x5,
               "vmpsadbw $5, %%xmm6,  %%xmm8, %%xmm7",
               "vmpsadbw $5, (%%rax), %%xmm8, %%xmm7")
GEN_test_RandM(VMPSADBW_128_0x6,
               "vmpsadbw $6, %%xmm6,  %%xmm8, %%xmm7",
               "vmpsadbw $6, (%%rax), %%xmm8, %%xmm7")
GEN_test_RandM(VMPSADBW_128_0x7,
               "vmpsadbw $7, %%xmm6,  %%xmm8, %%xmm7",
               "vmpsadbw $7, (%%rax), %%xmm8, %%xmm7")

GEN_test_RandM(VMOVDDUP_YMMorMEM256_to_YMM,
               "vmovddup %%ymm8,  %%ymm7",
               "vmovddup (%%rax), %%ymm9")

GEN_test_Monly(VMOVLPS_128_M64_XMM_XMM, "vmovlps (%%rax), %%xmm8, %%xmm7")

GEN_test_Monly(VMOVLPS_128_XMM_M64, "vmovlps %%xmm7, (%%rax)")

GEN_test_RandM(VRCPSS_128,
               "vrcpss %%xmm6,  %%xmm8, %%xmm7",
               "vrcpss (%%rax), %%xmm8, %%xmm7")

GEN_test_RandM(VRCPPS_128,
               "vrcpps %%xmm6,  %%xmm8",
               "vrcpps (%%rax), %%xmm8")

GEN_test_RandM(VRCPPS_256,
               "vrcpps %%ymm6,  %%ymm8",
               "vrcpps (%%rax), %%ymm8")

GEN_test_RandM(VPSADBW_128,
               "vpsadbw %%xmm6,  %%xmm8, %%xmm7",
               "vpsadbw (%%rax), %%xmm8, %%xmm7")

GEN_test_RandM(VPSIGNB_128,
               "vpsignb %%xmm6,  %%xmm8, %%xmm7",
               "vpsignb (%%rax), %%xmm8, %%xmm7")

GEN_test_RandM(VPSIGNW_128,
               "vpsignw %%xmm6,  %%xmm8, %%xmm7",
               "vpsignw (%%rax), %%xmm8, %%xmm7")

GEN_test_RandM(VPSIGND_128,
               "vpsignd %%xmm6,  %%xmm8, %%xmm7",
               "vpsignd (%%rax), %%xmm8, %%xmm7")

GEN_test_RandM(VPMULHRSW_128,
               "vpmulhrsw %%xmm6,  %%xmm8, %%xmm7",
               "vpmulhrsw (%%rax), %%xmm8, %%xmm7")

GEN_test_Monly(VBROADCASTF128,
               "vbroadcastf128 (%%rax), %%ymm9")

GEN_test_RandM(VPEXTRW_128_0x0,
               "vpextrw $0x0, %%xmm7, %%r14d",
               "vpextrw $0x0, %%xmm7, (%%rax)")
GEN_test_RandM(VPEXTRW_128_0x1,
               "vpextrw $0x1, %%xmm7, %%r14d",
               "vpextrw $0x1, %%xmm7, (%%rax)")
GEN_test_RandM(VPEXTRW_128_0x2,
               "vpextrw $0x2, %%xmm7, %%r14d",
               "vpextrw $0x2, %%xmm7, (%%rax)")
GEN_test_RandM(VPEXTRW_128_0x3,
               "vpextrw $0x3, %%xmm7, %%r14d",
               "vpextrw $0x3, %%xmm7, (%%rax)")
GEN_test_RandM(VPEXTRW_128_0x4,
               "vpextrw $0x4, %%xmm7, %%r14d",
               "vpextrw $0x4, %%xmm7, (%%rax)")
GEN_test_RandM(VPEXTRW_128_0x5,
               "vpextrw $0x5, %%xmm7, %%r14d",
               "vpextrw $0x5, %%xmm7, (%%rax)")
GEN_test_RandM(VPEXTRW_128_0x6,
               "vpextrw $0x6, %%xmm7, %%r14d",
               "vpextrw $0x6, %%xmm7, (%%rax)")
GEN_test_RandM(VPEXTRW_128_0x7,
               "vpextrw $0x7, %%xmm7, %%r14d",
               "vpextrw $0x7, %%xmm7, (%%rax)")

GEN_test_RandM(VAESENC,
               "vaesenc %%xmm6,  %%xmm8, %%xmm7",
               "vaesenc (%%rax), %%xmm8, %%xmm7")

GEN_test_RandM(VAESENCLAST,
               "vaesenclast %%xmm6,  %%xmm8, %%xmm7",
               "vaesenclast (%%rax), %%xmm8, %%xmm7")

GEN_test_RandM(VAESDEC,
               "vaesdec %%xmm6,  %%xmm8, %%xmm7",
               "vaesdec (%%rax), %%xmm8, %%xmm7")

GEN_test_RandM(VAESDECLAST,
               "vaesdeclast %%xmm6,  %%xmm8, %%xmm7",
               "vaesdeclast (%%rax), %%xmm8, %%xmm7")

GEN_test_RandM(VAESIMC,
               "vaesimc %%xmm6,  %%xmm7",
               "vaesimc (%%rax), %%xmm7")

GEN_test_RandM(VAESKEYGENASSIST_0x00,
               "vaeskeygenassist $0x00, %%xmm6,  %%xmm7",
               "vaeskeygenassist $0x00, (%%rax), %%xmm7")
GEN_test_RandM(VAESKEYGENASSIST_0x31,
               "vaeskeygenassist $0x31, %%xmm6,  %%xmm7",
               "vaeskeygenassist $0x31, (%%rax), %%xmm7")
GEN_test_RandM(VAESKEYGENASSIST_0xB2,
               "vaeskeygenassist $0xb2, %%xmm6,  %%xmm7",
               "vaeskeygenassist $0xb2, (%%rax), %%xmm7")
GEN_test_RandM(VAESKEYGENASSIST_0xFF,
               "vaeskeygenassist $0xFF, %%xmm6,  %%xmm7",
               "vaeskeygenassist $0xFF, (%%rax), %%xmm7")

GEN_test_RandM(VPCLMULQDQ_0x00,
               "vpclmulqdq $0x00, %%xmm6,  %%xmm8, %%xmm7",
               "vpclmulqdq $0x00, (%%rax), %%xmm8, %%xmm7")
GEN_test_RandM(VPCLMULQDQ_0x01,
               "vpclmulqdq $0x01, %%xmm6,  %%xmm8, %%xmm7",
               "vpclmulqdq $0x01, (%%rax), %%xmm8, %%xmm7")
GEN_test_RandM(VPCLMULQDQ_0x10,
               "vpclmulqdq $0x10, %%xmm6,  %%xmm8, %%xmm7",
               "vpclmulqdq $0x10, (%%rax), %%xmm8, %%xmm7")
GEN_test_RandM(VPCLMULQDQ_0x11,
               "vpclmulqdq $0x11, %%xmm6,  %%xmm8, %%xmm7",
               "vpclmulqdq $0x11, (%%rax), %%xmm8, %%xmm7")
GEN_test_RandM(VPCLMULQDQ_0xFF,
               "vpclmulqdq $0xFF, %%xmm6,  %%xmm8, %%xmm7",
               "vpclmulqdq $0xFF, (%%rax), %%xmm8, %%xmm7")

GEN_test_RandM(VCMPSS_128_0x9,
               "vcmpss $0x9, %%xmm6,  %%xmm8, %%xmm7",
               "vcmpss $0x9, (%%rax), %%xmm8, %%xmm7")

GEN_test_Monly(VMASKMOVPS_128_LoadForm,
               "vmaskmovps (%%rax), %%xmm8, %%xmm7;"
               "vxorps %%xmm6, %%xmm6, %%xmm6;"
               "vmaskmovps (%%rax,%%rax,4), %%xmm6, %%xmm9")

GEN_test_Monly(VMASKMOVPS_256_LoadForm,
               "vmaskmovps (%%rax), %%ymm8, %%ymm7;"
               "vxorps %%ymm6, %%ymm6, %%ymm6;"
               "vmaskmovps (%%rax,%%rax,4), %%ymm6, %%ymm9")

GEN_test_Monly(VMASKMOVPD_128_LoadForm,
               "vmaskmovpd (%%rax), %%xmm8, %%xmm7;"
               "vxorpd %%xmm6, %%xmm6, %%xmm6;"
               "vmaskmovpd (%%rax,%%rax,4), %%xmm6, %%xmm9")

GEN_test_Monly(VMASKMOVPD_256_LoadForm,
               "vmaskmovpd (%%rax), %%ymm8, %%ymm7;"
               "vxorpd %%ymm6, %%ymm6, %%ymm6;"
               "vmaskmovpd (%%rax,%%rax,4), %%ymm6, %%ymm9")

/* Comment duplicated above, for convenient reference:
   Allowed operands in test insns:
     Reg form:  %ymm6,  %ymm7, %ymm8, %ymm9 and %r14.
     Mem form:  (%rax), %ymm7, %ymm8, %ymm9 and %r14.
   Imm8 etc fields are also allowed, where they make sense.
*/

#define N_DEFAULT_ITERS 3

// Do the specified test some number of times
#define DO_N(_iters, _testfn) \
   do { int i; for (i = 0; i < (_iters); i++) { test_##_testfn(); } } while (0)

// Do the specified test the default number of times
#define DO_D(_testfn) DO_N(N_DEFAULT_ITERS, _testfn)


int main ( void )
{
   DO_D( VMOVUPD_EtoG_256 );
   DO_D( VMOVUPD_GtoE_256 );
   DO_D( VPSUBW_128 );
   DO_D( VPSUBQ_128 );
   DO_D( VPADDQ_128 );
   DO_D( VPINSRQ_128 );
   DO_D( VUCOMISS_128 );
   DO_D( VUCOMISD_128 );
   DO_D( VCVTPS2PD_128 );
   DO_D( VANDNPD_128 );
   DO_D( VORPD_128 );
   DO_D( VXORPD_128 );
   DO_D( VXORPS_128 );
   DO_D( VMULSD_128 );
   DO_D( VADDSD_128 );
   DO_D( VMINSD_128 );
   DO_D( VSUBSD_128 );
   DO_D( VDIVSD_128 );
   DO_D( VMAXSD_128 );
   DO_D( VPSHUFD_0x39_128 );
   DO_D( VPCMPEQD_128 );
   DO_D( VPEXTRD_128_0x3 );
   DO_D( VPEXTRD_128_0x0 );
   DO_D( VINSERTF128_0x0 );
   DO_D( VINSERTF128_0x1 );
   DO_D( VEXTRACTF128_0x0 );
   DO_D( VEXTRACTF128_0x1 );
   DO_D( VCVTPD2PS_128 );
   /* Test all CMPSS variants; this code is tricky. */
   DO_D( VCMPSS_128_0x0 );
   DO_D( VCMPSS_128_0x1 );
   DO_D( VCMPSS_128_0x2 );
   DO_D( VCMPSS_128_0x3 );
   DO_D( VCMPSS_128_0x4 );
   DO_D( VCMPSS_128_0x5 );
   DO_D( VCMPSS_128_0x6 );
   DO_D( VCMPSS_128_0x7 );
   DO_D( VCMPSS_128_0x8 );
   DO_D( VCMPSS_128_0xA );
   DO_D( VCMPSS_128_0xC );
   DO_D( VCMPSS_128_0xC );
   DO_D( VCMPSS_128_0xD );
   DO_D( VCMPSS_128_0xE );
   DO_D( VCMPSS_128_0x11 );
   DO_D( VCMPSS_128_0x12);
   DO_D( VCMPSS_128_0x16 );
   DO_D( VCMPSS_128_0x1E );
   DO_D( VMOVDDUP_XMMorMEM64_to_XMM );
   DO_D( VMOVD_IREGorMEM32_to_XMM );
   DO_D( VMOVQ_XMM_MEM64 );
   DO_D( VMOVDQA_GtoE_256 );
   DO_D( VMOVDQA_GtoE_128 );
   DO_D( VMOVDQU_GtoE_128 );
   DO_D( VMOVDQA_EtoG_256 );
   DO_D( VMOVDQA_EtoG_128 );
   DO_D( VMOVDQU_EtoG_128 );
   DO_D( VMOVAPD_GtoE_128 );
   DO_D( VMOVAPD_GtoE_256 );
   DO_D( VMOVAPS_GtoE_128 );
   DO_D( VMOVAPS_GtoE_256 );
   DO_D( VMOVAPS_EtoG_128 );
   DO_D( VMOVAPD_EtoG_256 );
   DO_D( VMOVAPD_EtoG_128 );
   DO_D( VMOVUPD_GtoE_128 );
   DO_D( VMOVSS_XMM_M32 );
   DO_D( VMOVSD_XMM_M64 );
   DO_D( VMOVSS_M64_XMM );
   DO_D( VMOVSD_M64_XMM );
   DO_D( VINSERTPS_0x39_128 );
   DO_D( VPUNPCKLDQ_128 );
   DO_D( VPACKSSDW_128 );
   DO_D( VPADDW_128 );
   DO_D( VPSRLW_0x05_128 );
   DO_D( VPSLLW_0x05_128 );
   DO_D( VPUNPCKLQDQ_128 );
   DO_D( VPINSRD_128 );
   DO_D( VMOVD_XMM_to_MEM32 );
   DO_D( VPANDN_128 );
   DO_D( VPSLLDQ_0x05_128 );
   DO_D( VPSRLDQ_0x05_128 );
   DO_D( VPSUBUSB_128 );
   DO_D( VPSUBSB_128 );
   DO_D( VPSLLD_0x05_128 );
   DO_D( VPSRLD_0x05_128 );
   DO_D( VPSRAD_0x05_128 );
   DO_D( VPUNPCKLWD_128 );
   DO_D( VPUNPCKHWD_128 );
   DO_D( VPADDUSB_128 );
   DO_D( VPMULHUW_128 );
   DO_D( VPADDUSW_128 );
   DO_D( VPMULLW_128 );
   DO_D( VPSHUFHW_0x39_128 );
   DO_D( VPSHUFLW_0x39_128 );
   DO_D( VCVTPS2DQ_128 );
   DO_D( VSUBPS_128 );
   DO_D( VADDPS_128 );
   DO_D( VMULPS_128 );
   DO_D( VMAXPS_128 );
   DO_D( VMINPS_128 );
   DO_D( VSHUFPS_0x39_128 );
   DO_D( VPCMPEQB_128 );
   DO_D( VMOVHPD_128_StoreForm );
   DO_D( VPAND_128 );
   DO_D( VPMOVMSKB_128 );
   DO_D( VCVTTSS2SI_64 );
   DO_D( VPACKUSWB_128 );
   DO_D( VCVTSS2SD_128 );
   DO_D( VCVTSD2SS_128 );
   DO_D( VMOVD_XMM_to_IREG32 );
   DO_D( VPCMPESTRM_0x45_128 );
   DO_D( VMOVQ_IREGorMEM64_to_XMM );
   DO_D( VMOVUPS_XMM_to_XMMorMEM );
   DO_D( VMOVNTDQ_128 );
   DO_D( VMOVLHPS_128 );
   DO_D( VPABSD_128 );
   DO_D( VMOVHLPS_128 );
   DO_D( VMOVQ_XMM_to_IREG64 );
   DO_D( VMOVQ_XMMorMEM64_to_XMM );
   DO_D( VCVTTSS2SI_32 );
   DO_D( VPUNPCKLBW_128 );
   DO_D( VPUNPCKHBW_128 );
   DO_D( VMULSS_128 );
   DO_D( VSUBSS_128 );
   DO_D( VADDSS_128 );
   DO_D( VDIVSS_128 );
   DO_D( VUNPCKLPS_128 );
   DO_D( VCVTSI2SS_128 );
   DO_D( VANDPS_128 );
   DO_D( VMINSS_128 );
   DO_D( VMAXSS_128 );
   DO_D( VANDNPS_128 );
   DO_D( VORPS_128 );
   DO_D( VSQRTSD_128 );
   /* Test all CMPSS variants; this code is tricky. */
   DO_D( VCMPSD_128_0x0 );
   DO_D( VCMPSD_128_0x1 );
   DO_D( VCMPSD_128_0x2 );
   DO_D( VCMPSD_128_0x3 );
   DO_D( VCMPSD_128_0x4 );
   DO_D( VCMPSD_128_0x5 );
   DO_D( VCMPSD_128_0x6 );
   DO_D( VCMPSD_128_0x7 );
   DO_D( VCMPSD_128_0x8 );
   DO_D( VCMPSD_128_0xA );
   DO_D( VCMPSD_128_0xC );
   DO_D( VCMPSD_128_0xD );
   DO_D( VCMPSD_128_0xE );
   DO_D( VCMPSD_128_0x11 );
   DO_D( VCMPSD_128_0x12 );
   DO_D( VCMPSD_128_0x16 );
   DO_D( VCMPSD_128_0x1E );
   DO_D( VPSHUFB_128 );
   DO_D( VCVTTSD2SI_32 );
   DO_D( VCVTTSD2SI_64 );
   DO_D( VCVTSI2SS_64 );
   DO_D( VCVTSI2SD_64 );
   DO_D( VCVTSI2SD_32 );
   DO_D( VPOR_128 );
   DO_D( VPXOR_128 );
   DO_D( VPSUBB_128 );
   DO_D( VPSUBD_128 );
   DO_D( VPADDD_128 );
   DO_D( VPMOVZXBW_128 );
   DO_D( VPMOVZXWD_128 );
   DO_D( VPBLENDVB_128 );
   DO_D( VPMINSD_128 );
   DO_D( VPMAXSD_128 );
   DO_D( VANDPD_128 );
   DO_D( VMULPD_256 );
   DO_D( VMOVUPD_EtoG_128 );
   DO_D( VADDPD_256 );
   DO_D( VSUBPD_256 );
   DO_D( VDIVPD_256 );
   DO_D( VPCMPEQQ_128 );
   DO_D( VSUBPD_128 );
   DO_D( VADDPD_128 );
   DO_D( VUNPCKLPD_128 );
   DO_D( VUNPCKHPD_128 );
   DO_D( VUNPCKHPS_128 );
   DO_D( VMOVUPS_EtoG_128 );
   DO_D( VADDPS_256 );
   DO_D( VSUBPS_256 );
   DO_D( VMULPS_256 );
   DO_D( VDIVPS_256 );
   DO_D( VPCMPGTQ_128 );
   DO_D( VPEXTRQ_128_0x0 );
   DO_D( VPEXTRQ_128_0x1 );
   DO_D( VPSRLQ_0x05_128 );
   DO_D( VPMULUDQ_128 );
   DO_D( VPSLLQ_0x05_128 );
   DO_D( VPMAXUD_128 );
   DO_D( VPMINUD_128 );
   DO_D( VPMULLD_128 );
   DO_D( VPMAXUW_128 );
   DO_D( VPEXTRW_128_EregOnly_toG_0x0 );
   DO_D( VPEXTRW_128_EregOnly_toG_0x7 );
   DO_D( VPMINUW_128 );
   DO_D( VPHMINPOSUW_128 );
   DO_D( VPMAXSW_128 );
   DO_D( VPMINSW_128 );
   DO_D( VPMAXUB_128 );
   DO_D( VPEXTRB_GtoE_128_0x0 );
   DO_D( VPEXTRB_GtoE_128_0x1 );
   DO_D( VPEXTRB_GtoE_128_0x2 );
   DO_D( VPEXTRB_GtoE_128_0x3 );
   DO_D( VPEXTRB_GtoE_128_0x4 );
   DO_D( VPEXTRB_GtoE_128_0x9 );
   DO_D( VPEXTRB_GtoE_128_0xE );
   DO_D( VPEXTRB_GtoE_128_0xF );
   DO_D( VPMINUB_128 );
   DO_D( VPMAXSB_128 );
   DO_D( VPMINSB_128 );
   DO_D( VPERM2F128_0x00 );
   DO_D( VPERM2F128_0xFF );
   DO_D( VPERM2F128_0x30 );
   DO_D( VPERM2F128_0x21 );
   DO_D( VPERM2F128_0x12 );
   DO_D( VPERM2F128_0x03 );
   DO_D( VPERM2F128_0x85 );
   DO_D( VPERM2F128_0x5A );
   DO_D( VPERMILPD_256_0x0 );
   DO_D( VPERMILPD_256_0xF );
   DO_D( VPERMILPD_256_0xA );
   DO_D( VPERMILPD_256_0x5 );
   DO_D( VPERMILPD_128_0x0 );
   DO_D( VPERMILPD_128_0x3 );
   DO_D( VUNPCKLPD_256 );
   DO_D( VUNPCKHPD_256 );
   DO_D( VSHUFPS_0x39_256 );
   DO_D( VUNPCKLPS_256 );
   DO_D( VUNPCKHPS_256 );
   DO_D( VXORPD_256 );
   DO_D( VBROADCASTSD_256 );
   DO_D( VCMPPD_128_0x4 );
   DO_D( VCVTDQ2PD_128 );
   DO_D( VDIVPD_128 );
   DO_D( VANDPD_256 );
   DO_D( VPMOVSXBW_128 );
   DO_D( VPSUBUSW_128 );
   DO_D( VPSUBSW_128 );
   DO_D( VPCMPEQW_128 );
   DO_D( VPADDB_128 );
   DO_D( VMOVAPS_EtoG_256 );
   DO_D( VCVTDQ2PD_256 );
   DO_D( VMOVHPD_128_LoadForm );
   DO_D( VCVTPD2PS_256 );
   DO_D( VPUNPCKHDQ_128 );
   DO_D( VBROADCASTSS_128 );
   DO_D( VPMOVSXDQ_128 );
   DO_D( VPMOVSXWD_128 );
   DO_D( VDIVPS_128 );
   DO_D( VANDPS_256 );
   DO_D( VXORPS_256 );
   DO_D( VORPS_256 );
   DO_D( VANDNPD_256 );
   DO_D( VANDNPS_256 );
   DO_D( VORPD_256 );
   DO_D( VPERMILPS_256_0x0F );
   DO_D( VPERMILPS_256_0xFA );
   DO_D( VPERMILPS_256_0xA3 );
   DO_D( VPERMILPS_256_0x5A );
   DO_D( VPMULHW_128 );
   DO_D( VPUNPCKHQDQ_128 );
   DO_D( VPSRAW_0x05_128 );
   DO_D( VPCMPGTD_128 );
   DO_D( VPMOVZXBD_128 );
   DO_D( VPMOVSXBD_128 );
   DO_D( VPINSRB_128_1of3 );
   DO_D( VPINSRB_128_2of3 );
   DO_D( VPINSRB_128_3of3 );
   DO_D( VCOMISD_128 );
   DO_D( VCOMISS_128 );
   DO_D( VMOVUPS_YMM_to_YMMorMEM );
   DO_D( VDPPD_128_1of4 );
   DO_D( VDPPD_128_2of4 );
   DO_D( VDPPD_128_3of4 );
   DO_D( VDPPD_128_4of4 );
   DO_D( VPINSRW_128_1of4 );
   DO_D( VPINSRW_128_2of4 );
   DO_D( VPINSRW_128_3of4 );
   DO_D( VPINSRW_128_4of4 );
   DO_D( VBROADCASTSS_256 );
   DO_D( VPALIGNR_128_1of3 );
   DO_D( VPALIGNR_128_2of3 );
   DO_D( VPALIGNR_128_3of3 );
   DO_D( VMOVSD_REG_XMM );
   DO_D( VMOVSS_REG_XMM );
   DO_D( VMOVLPD_128_M64_XMM_XMM );
   DO_D( VMOVLPD_128_XMM_M64 );
   DO_D( VSHUFPD_128_1of2 );
   DO_D( VSHUFPD_128_2of2 );
   DO_D( VSHUFPD_256_1of2 );
   DO_D( VSHUFPD_256_2of2 );
   DO_D( VPERMILPS_128_0x00 );
   DO_D( VPERMILPS_128_0xFE );
   DO_D( VPERMILPS_128_0x30 );
   DO_D( VPERMILPS_128_0x21 );
   DO_D( VPERMILPS_128_0xD7 );
   DO_D( VPERMILPS_128_0xB5 );
   DO_D( VPERMILPS_128_0x85 );
   DO_D( VPERMILPS_128_0x29 );
   DO_D( VBLENDPS_128_1of3 );
   DO_D( VBLENDPS_128_2of3 );
   DO_D( VBLENDPS_128_3of3 );
   DO_D( VBLENDPD_128_1of2 );
   DO_D( VBLENDPD_128_2of2 );
   DO_D( VBLENDPD_256_1of3 );
   DO_D( VBLENDPD_256_2of3 );
   DO_D( VBLENDPD_256_3of3 );
   DO_D( VPBLENDW_128_0x00 );
   DO_D( VPBLENDW_128_0xFE );
   DO_D( VPBLENDW_128_0x30 );
   DO_D( VPBLENDW_128_0x21 );
   DO_D( VPBLENDW_128_0xD7 );
   DO_D( VPBLENDW_128_0xB5 );
   DO_D( VPBLENDW_128_0x85 );
   DO_D( VPBLENDW_128_0x29 );
   DO_D( VMOVUPS_EtoG_256 );
   DO_D( VSQRTSS_128 );
   DO_D( VSQRTPS_128 );
   DO_D( VSQRTPS_256 );
   DO_D( VSQRTPD_128 );
   DO_D( VSQRTPD_256 );
   DO_D( VRSQRTSS_128 );
   DO_D( VRSQRTPS_128 );
   DO_D( VRSQRTPS_256 );
   DO_D( VMOVDQU_GtoE_256 );
   DO_D( VCVTPS2PD_256 );
   DO_D( VCVTTPS2DQ_128 );
   DO_D( VCVTTPS2DQ_256 );
   DO_D( VCVTDQ2PS_128 );
   DO_D( VCVTDQ2PS_256 );
   DO_D( VCVTTPD2DQ_128 );
   DO_D( VCVTTPD2DQ_256 );
   DO_D( VCVTPD2DQ_128 );
   DO_D( VCVTPD2DQ_256 );
   DO_D( VMOVSLDUP_128 );
   DO_D( VMOVSLDUP_256 );
   DO_D( VMOVSHDUP_128 );
   DO_D( VMOVSHDUP_256 );
   DO_D( VPERMILPS_VAR_128 );
   DO_D( VPERMILPD_VAR_128 );
   DO_D( VPERMILPS_VAR_256 );
   DO_D( VPERMILPD_VAR_256 );
   DO_D( VPSLLW_128 );
   DO_D( VPSRLW_128 );
   DO_D( VPSRAW_128 );
   DO_D( VPSLLD_128 );
   DO_D( VPSRLD_128 );
   DO_D( VPSRAD_128 );
   DO_D( VPSLLQ_128 );
   DO_D( VPSRLQ_128 );
   DO_D( VROUNDPS_128_0x0 );
   DO_D( VROUNDPS_128_0x1 );
   DO_D( VROUNDPS_128_0x2 );
   DO_D( VROUNDPS_128_0x3 );
   DO_D( VROUNDPS_128_0x4 );
   DO_D( VROUNDPS_256_0x0 );
   DO_D( VROUNDPS_256_0x1 );
   DO_D( VROUNDPS_256_0x2 );
   DO_D( VROUNDPS_256_0x3 );
   DO_D( VROUNDPS_256_0x4 );
   DO_D( VROUNDPD_128_0x0 );
   DO_D( VROUNDPD_128_0x1 );
   DO_D( VROUNDPD_128_0x2 );
   DO_D( VROUNDPD_128_0x3 );
   DO_D( VROUNDPD_128_0x4 );
   DO_D( VROUNDPD_256_0x0 );
   DO_D( VROUNDPD_256_0x1 );
   DO_D( VROUNDPD_256_0x2 );
   DO_D( VROUNDPD_256_0x3 );
   DO_D( VROUNDPD_256_0x4 );
   DO_D( VROUNDSS_0x0 );
   DO_D( VROUNDSS_0x1 );
   DO_D( VROUNDSS_0x2 );
   DO_D( VROUNDSS_0x3 );
   DO_D( VROUNDSS_0x4 );
   DO_D( VROUNDSS_0x5 );
   DO_D( VROUNDSD_0x0 );
   DO_D( VROUNDSD_0x1 );
   DO_D( VROUNDSD_0x2 );
   DO_D( VROUNDSD_0x3 );
   DO_D( VROUNDSD_0x4 );
   DO_D( VROUNDSD_0x5 );
   DO_D( VPTEST_128_1 );
   DO_D( VPTEST_128_2 );
   DO_D( VPTEST_256_1 );
   DO_D( VPTEST_256_2 );
   DO_D( VTESTPS_128_1 );
   DO_D( VTESTPS_128_2 );
   DO_N( 10, VTESTPS_128_3 );
   DO_D( VTESTPS_256_1 );
   DO_D( VTESTPS_256_2 );
   DO_N( 10, VTESTPS_256_3 );
   DO_D( VTESTPD_128_1 );
   DO_D( VTESTPD_128_2 );
   DO_N( 10, VTESTPD_128_3 );
   DO_D( VTESTPD_256_1 );
   DO_D( VTESTPD_256_2 );
   DO_N( 10, VTESTPD_256_3 );
   DO_D( VBLENDVPS_128 );
   DO_D( VBLENDVPS_256 );
   DO_D( VBLENDVPD_128 );
   DO_D( VBLENDVPD_256 );
   DO_D( VPMULDQ_128 );
   DO_D( VCMPPD_256_0x4 );
   DO_D( VCMPPS_128_0x4 );
   DO_D( VCMPPS_256_0x4 );
   DO_D( VPCMPGTB_128 );
   DO_D( VPCMPGTW_128 );
   DO_D( VPMADDWD_128 );
   DO_D( VADDSUBPS_128 );
   DO_D( VADDSUBPS_256 );
   DO_D( VADDSUBPD_128 );
   DO_D( VADDSUBPD_256 );
   DO_D( VCVTSS2SI_64 );
   DO_D( VCVTSS2SI_32 );
   DO_D( VCVTSD2SI_32 );
   DO_D( VCVTSD2SI_64 );
   DO_D( VDPPS_128_1of4 );
   DO_D( VDPPS_128_2of4 );
   DO_D( VDPPS_128_3of4 );
   DO_D( VDPPS_128_4of4 );
   DO_D( VDPPS_256_1of4 );
   DO_D( VDPPS_256_2of4 );
   DO_D( VDPPS_256_3of4 );
   DO_D( VDPPS_256_4of4 );
   DO_D( VHADDPS_128 );
   DO_D( VHADDPS_256 );
   DO_D( VHADDPD_128 );
   DO_D( VHADDPD_256 );
   DO_D( VHSUBPS_128 );
   DO_D( VHSUBPS_256 );
   DO_D( VHSUBPD_128 );
   DO_D( VHSUBPD_256 );
   DO_D( VEXTRACTPS_0x0 );
   DO_D( VEXTRACTPS_0x1 );
   DO_D( VEXTRACTPS_0x2 );
   DO_D( VEXTRACTPS_0x3 );
   DO_D( VLDDQU_128 );
   DO_D( VLDDQU_256 );
   DO_D( VMAXPS_256 );
   DO_D( VMAXPD_128 );
   DO_D( VMAXPD_256 );
   DO_D( VMINPS_256 );
   DO_D( VMINPD_128 );
   DO_D( VMINPD_256 );
   DO_D( VMOVHPS_128_StoreForm );
   DO_D( VMOVNTDQ_256 );
   DO_D( VMOVHPS_128_LoadForm );
   DO_D( VMOVNTDQA_128 );
   DO_D( VMASKMOVDQU_128 );
   DO_D( VMOVMSKPD_128 );
   DO_D( VMOVMSKPD_256 );
   DO_D( VMOVMSKPS_128 );
   DO_D( VMOVMSKPS_256 );
   DO_D( VMOVNTPD_128 );
   DO_D( VMOVNTPD_256 );
   DO_D( VMOVNTPS_128 );
   DO_D( VMOVNTPS_256 );
   DO_D( VPACKSSWB_128 );
   DO_D( VPAVGB_128 );
   DO_D( VPAVGW_128 );
   DO_D( VPADDSB_128 );
   DO_D( VPADDSW_128 );
   DO_D( VPHADDW_128 );
   DO_D( VPHADDD_128 );
   DO_D( VPHADDSW_128 );
   DO_D( VPMADDUBSW_128 );
   DO_D( VPHSUBW_128 );
   DO_D( VPHSUBD_128 );
   DO_D( VPHSUBSW_128 );
   DO_D( VPABSB_128 );
   DO_D( VPABSW_128 );
   DO_D( VPMOVSXBQ_128 );
   DO_D( VPMOVSXWQ_128 );
   DO_D( VPACKUSDW_128 );
   DO_D( VPMOVZXBQ_128 );
   DO_D( VPMOVZXWQ_128 );
   DO_D( VPMOVZXDQ_128 );
   DO_D( VMPSADBW_128_0x0 );
   DO_D( VMPSADBW_128_0x1 );
   DO_D( VMPSADBW_128_0x2 );
   DO_D( VMPSADBW_128_0x3 );
   DO_D( VMPSADBW_128_0x4 );
   DO_D( VMPSADBW_128_0x5 );
   DO_D( VMPSADBW_128_0x6 );
   DO_D( VMPSADBW_128_0x7 );
   DO_D( VMOVDDUP_YMMorMEM256_to_YMM );
   DO_D( VMOVLPS_128_M64_XMM_XMM );
   DO_D( VMOVLPS_128_XMM_M64 );
   DO_D( VRCPSS_128 );
   DO_D( VRCPPS_128 );
   DO_D( VRCPPS_256 );
   DO_D( VPSADBW_128 );
   DO_D( VPSIGNB_128 );
   DO_D( VPSIGNW_128 );
   DO_D( VPSIGND_128 );
   DO_D( VPMULHRSW_128 );
   DO_D( VBROADCASTF128 );
   DO_D( VPEXTRW_128_0x0 );
   DO_D( VPEXTRW_128_0x1 );
   DO_D( VPEXTRW_128_0x2 );
   DO_D( VPEXTRW_128_0x3 );
   DO_D( VPEXTRW_128_0x4 );
   DO_D( VPEXTRW_128_0x5 );
   DO_D( VPEXTRW_128_0x6 );
   DO_D( VPEXTRW_128_0x7 );
   DO_D( VAESENC );
   DO_D( VAESENCLAST );
   DO_D( VAESDEC );
   DO_D( VAESDECLAST );
   DO_D( VAESIMC );
   DO_D( VAESKEYGENASSIST_0x00 );
   DO_D( VAESKEYGENASSIST_0x31 );
   DO_D( VAESKEYGENASSIST_0xB2 );
   DO_D( VAESKEYGENASSIST_0xFF );
   DO_D( VPCLMULQDQ_0x00 );
   DO_D( VPCLMULQDQ_0x01 );
   DO_D( VPCLMULQDQ_0x10 );
   DO_D( VPCLMULQDQ_0x11 );
   DO_D( VPCLMULQDQ_0xFF );
   DO_D( VCMPSS_128_0x9 );
   DO_D( VMASKMOVPS_128_LoadForm );
   DO_D( VMASKMOVPS_256_LoadForm );
   DO_D( VMASKMOVPD_128_LoadForm );
   DO_D( VMASKMOVPD_256_LoadForm );
   return 0;
}

