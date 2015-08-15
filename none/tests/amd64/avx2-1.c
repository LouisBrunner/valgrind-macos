
#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include "tests/malloc.h"

typedef  unsigned char           UChar;
typedef  unsigned int            UInt;
typedef  unsigned long int       UWord;
typedef  unsigned long long int  ULong;

#if defined(VGO_darwin)
UChar randArray[1027] __attribute__((used));
#else
UChar _randArray[1027] __attribute__((used));
#endif

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
   for testing PCMPxSTRx, and ymm6, as this is needed for testing
   MOVMASK variants. */

#define GEN_test_RandM(_name, _reg_form, _mem_form)   \
    \
    __attribute__ ((noinline)) static void test_##_name ( void )   \
    { \
       Block* b = memalign32(sizeof(Block)); \
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
          : /*TRASH*/"xmm6", \
                     "xmm0","xmm8","xmm7","xmm9","r14","rax","memory","cc" \
       ); \
       showBlock("after", b); \
       printf("\n"); \
       free(b); \
    }

#define GEN_test_Ronly(_name, _reg_form) \
   GEN_test_RandM(_name, _reg_form, "")
#define GEN_test_Monly(_name, _mem_form) \
   GEN_test_RandM(_name, "", _mem_form)

/* Vector integers promoved from 128-bit in AVX to 256-bit in AVX2.  */

GEN_test_RandM(VPOR_256,
               "vpor %%ymm6,  %%ymm8, %%ymm7",
               "vpor (%%rax), %%ymm8, %%ymm7")

GEN_test_RandM(VPXOR_256,
               "vpxor %%ymm6,  %%ymm8, %%ymm7",
               "vpxor (%%rax), %%ymm8, %%ymm7")

GEN_test_RandM(VPSUBB_256,
               "vpsubb %%ymm6,  %%ymm8, %%ymm7",
               "vpsubb (%%rax), %%ymm8, %%ymm7")

GEN_test_RandM(VPSUBD_256,
               "vpsubd %%ymm6,  %%ymm8, %%ymm7",
               "vpsubd (%%rax), %%ymm8, %%ymm7")

GEN_test_RandM(VPADDD_256,
               "vpaddd %%ymm6,  %%ymm8, %%ymm7",
               "vpaddd (%%rax), %%ymm8, %%ymm7")

GEN_test_RandM(VPMOVZXWD_256,
               "vpmovzxwd %%xmm6,  %%ymm8",
               "vpmovzxwd (%%rax), %%ymm8")

GEN_test_RandM(VPMOVZXBW_256,
               "vpmovzxbw %%xmm6,  %%ymm8",
               "vpmovzxbw (%%rax), %%ymm8")

GEN_test_RandM(VPBLENDVB_256,
               "vpblendvb %%ymm9, %%ymm6,  %%ymm8, %%ymm7",
               "vpblendvb %%ymm9, (%%rax), %%ymm8, %%ymm7")

GEN_test_RandM(VPMINSD_256,
               "vpminsd %%ymm6,  %%ymm8, %%ymm7",
               "vpminsd (%%rax), %%ymm8, %%ymm7")

GEN_test_RandM(VPMAXSD_256,
               "vpmaxsd %%ymm6,  %%ymm8, %%ymm7",
               "vpmaxsd (%%rax), %%ymm8, %%ymm7")

GEN_test_RandM(VPSHUFB_256,
               "vpshufb %%ymm6,  %%ymm8, %%ymm7",
               "vpshufb (%%rax), %%ymm8, %%ymm7")

GEN_test_RandM(VPUNPCKLBW_256,
               "vpunpcklbw %%ymm6,  %%ymm8, %%ymm7",
               "vpunpcklbw (%%rax), %%ymm8, %%ymm7")

GEN_test_RandM(VPUNPCKHBW_256,
               "vpunpckhbw %%ymm6,  %%ymm8, %%ymm7",
               "vpunpckhbw (%%rax), %%ymm8, %%ymm7")

GEN_test_RandM(VPABSD_256,
               "vpabsd %%ymm6,  %%ymm8",
               "vpabsd (%%rax), %%ymm8")

GEN_test_RandM(VPACKUSWB_256,
               "vpackuswb %%ymm9,  %%ymm8, %%ymm7",
               "vpackuswb (%%rax), %%ymm8, %%ymm7")

GEN_test_Ronly(VPMOVMSKB_256,
               "vpmovmskb %%ymm8, %%r14")

GEN_test_RandM(VPAND_256,
               "vpand %%ymm9,  %%ymm8, %%ymm7",
               "vpand (%%rax), %%ymm8, %%ymm7")

GEN_test_RandM(VPCMPEQB_256,
               "vpcmpeqb %%ymm9,  %%ymm8, %%ymm7",
               "vpcmpeqb (%%rax), %%ymm8, %%ymm7")

GEN_test_RandM(VPSHUFLW_0x39_256,
               "vpshuflw $0x39, %%ymm9,  %%ymm7",
               "vpshuflw $0xC6, (%%rax), %%ymm8")

GEN_test_RandM(VPSHUFHW_0x39_256,
               "vpshufhw $0x39, %%ymm9,  %%ymm7",
               "vpshufhw $0xC6, (%%rax), %%ymm8")

GEN_test_RandM(VPMULLW_256,
               "vpmullw %%ymm9,  %%ymm8, %%ymm7",
               "vpmullw (%%rax), %%ymm8, %%ymm7")

GEN_test_RandM(VPADDUSW_256,
               "vpaddusw %%ymm9,  %%ymm8, %%ymm7",
               "vpaddusw (%%rax), %%ymm8, %%ymm7")

GEN_test_RandM(VPMULHUW_256,
               "vpmulhuw %%ymm9,  %%ymm8, %%ymm7",
               "vpmulhuw (%%rax), %%ymm8, %%ymm7")

GEN_test_RandM(VPADDUSB_256,
               "vpaddusb %%ymm9,  %%ymm8, %%ymm7",
               "vpaddusb (%%rax), %%ymm8, %%ymm7")

GEN_test_RandM(VPUNPCKLWD_256,
               "vpunpcklwd %%ymm6,  %%ymm8, %%ymm7",
               "vpunpcklwd (%%rax), %%ymm8, %%ymm7")

GEN_test_RandM(VPUNPCKHWD_256,
               "vpunpckhwd %%ymm6,  %%ymm8, %%ymm7",
               "vpunpckhwd (%%rax), %%ymm8, %%ymm7")

GEN_test_Ronly(VPSLLD_0x05_256,
               "vpslld $0x5, %%ymm9,  %%ymm7")

GEN_test_Ronly(VPSRLD_0x05_256,
               "vpsrld $0x5, %%ymm9,  %%ymm7")

GEN_test_Ronly(VPSRAD_0x05_256,
               "vpsrad $0x5, %%ymm9,  %%ymm7")

GEN_test_RandM(VPSUBUSB_256,
               "vpsubusb %%ymm9,  %%ymm8, %%ymm7",
               "vpsubusb (%%rax), %%ymm8, %%ymm7")

GEN_test_RandM(VPSUBSB_256,
               "vpsubsb %%ymm9,  %%ymm8, %%ymm7",
               "vpsubsb (%%rax), %%ymm8, %%ymm7")

GEN_test_Ronly(VPSRLDQ_0x05_256,
               "vpsrldq $0x5, %%ymm9,  %%ymm7")

GEN_test_Ronly(VPSLLDQ_0x05_256,
               "vpslldq $0x5, %%ymm9,  %%ymm7")

GEN_test_RandM(VPANDN_256,
               "vpandn %%ymm9,  %%ymm8, %%ymm7",
               "vpandn (%%rax), %%ymm8, %%ymm7")

GEN_test_RandM(VPUNPCKLQDQ_256,
               "vpunpcklqdq %%ymm6,  %%ymm8, %%ymm7",
               "vpunpcklqdq (%%rax), %%ymm8, %%ymm7")

GEN_test_Ronly(VPSRLW_0x05_256,
               "vpsrlw $0x5, %%ymm9,  %%ymm7")

GEN_test_Ronly(VPSLLW_0x05_256,
               "vpsllw $0x5, %%ymm9,  %%ymm7")

GEN_test_RandM(VPADDW_256,
               "vpaddw %%ymm6,  %%ymm8, %%ymm7",
               "vpaddw (%%rax), %%ymm8, %%ymm7")

GEN_test_RandM(VPACKSSDW_256,
               "vpackssdw %%ymm9,  %%ymm8, %%ymm7",
               "vpackssdw (%%rax), %%ymm8, %%ymm7")

GEN_test_RandM(VPUNPCKLDQ_256,
               "vpunpckldq %%ymm6,  %%ymm8, %%ymm7",
               "vpunpckldq (%%rax), %%ymm8, %%ymm7")

GEN_test_RandM(VPCMPEQD_256,
               "vpcmpeqd %%ymm6,  %%ymm8, %%ymm7",
               "vpcmpeqd (%%rax), %%ymm8, %%ymm7")

GEN_test_RandM(VPSHUFD_0x39_256,
               "vpshufd $0x39, %%ymm9,  %%ymm8",
               "vpshufd $0xC6, (%%rax), %%ymm7")

GEN_test_RandM(VPADDQ_256,
               "vpaddq %%ymm6,  %%ymm8, %%ymm7",
               "vpaddq (%%rax), %%ymm8, %%ymm7")

GEN_test_RandM(VPSUBQ_256,
               "vpsubq %%ymm6,  %%ymm8, %%ymm7",
               "vpsubq (%%rax), %%ymm8, %%ymm7")

GEN_test_RandM(VPSUBW_256,
               "vpsubw %%ymm6,  %%ymm8, %%ymm7",
               "vpsubw (%%rax), %%ymm8, %%ymm7")

GEN_test_RandM(VPCMPEQQ_256,
               "vpcmpeqq %%ymm6,  %%ymm8, %%ymm7",
               "vpcmpeqq (%%rax), %%ymm8, %%ymm7")

GEN_test_RandM(VPCMPGTQ_256,
               "vpcmpgtq %%ymm6,  %%ymm8, %%ymm7",
               "vpcmpgtq (%%rax), %%ymm8, %%ymm7")

GEN_test_Ronly(VPSRLQ_0x05_256,
               "vpsrlq $0x5, %%ymm9,  %%ymm7")

GEN_test_RandM(VPMULUDQ_256,
               "vpmuludq %%ymm6,  %%ymm8, %%ymm7",
               "vpmuludq (%%rax), %%ymm8, %%ymm7")

GEN_test_RandM(VPMULDQ_256,
               "vpmuldq %%ymm6,  %%ymm8, %%ymm7",
               "vpmuldq (%%rax), %%ymm8, %%ymm7")

GEN_test_Ronly(VPSLLQ_0x05_256,
               "vpsllq $0x5, %%ymm9,  %%ymm7")

GEN_test_RandM(VPMAXUD_256,
               "vpmaxud %%ymm6,  %%ymm8, %%ymm7",
               "vpmaxud (%%rax), %%ymm8, %%ymm7")

GEN_test_RandM(VPMINUD_256,
               "vpminud %%ymm6,  %%ymm8, %%ymm7",
               "vpminud (%%rax), %%ymm8, %%ymm7")

GEN_test_RandM(VPMULLD_256,
               "vpmulld %%ymm6,  %%ymm8, %%ymm7",
               "vpmulld (%%rax), %%ymm8, %%ymm7")

GEN_test_RandM(VPMAXUW_256,
               "vpmaxuw %%ymm6,  %%ymm8, %%ymm7",
               "vpmaxuw (%%rax), %%ymm8, %%ymm7")

GEN_test_RandM(VPMINUW_256,
               "vpminuw %%ymm6,  %%ymm8, %%ymm7",
               "vpminuw (%%rax), %%ymm8, %%ymm7")

GEN_test_RandM(VPMAXSW_256,
               "vpmaxsw %%ymm6,  %%ymm8, %%ymm7",
               "vpmaxsw (%%rax), %%ymm8, %%ymm7")

GEN_test_RandM(VPMINSW_256,
               "vpminsw %%ymm6,  %%ymm8, %%ymm7",
               "vpminsw (%%rax), %%ymm8, %%ymm7")

GEN_test_RandM(VPMAXUB_256,
               "vpmaxub %%ymm6,  %%ymm8, %%ymm7",
               "vpmaxub (%%rax), %%ymm8, %%ymm7")

GEN_test_RandM(VPMINUB_256,
               "vpminub %%ymm6,  %%ymm8, %%ymm7",
               "vpminub (%%rax), %%ymm8, %%ymm7")

GEN_test_RandM(VPMAXSB_256,
               "vpmaxsb %%ymm6,  %%ymm8, %%ymm7",
               "vpmaxsb (%%rax), %%ymm8, %%ymm7")

GEN_test_RandM(VPMINSB_256,
               "vpminsb %%ymm6,  %%ymm8, %%ymm7",
               "vpminsb (%%rax), %%ymm8, %%ymm7")

GEN_test_RandM(VPMOVSXBW_256,
               "vpmovsxbw %%xmm6,  %%ymm8",
               "vpmovsxbw (%%rax), %%ymm8")

GEN_test_RandM(VPSUBUSW_256,
               "vpsubusw %%ymm9,  %%ymm8, %%ymm7",
               "vpsubusw (%%rax), %%ymm8, %%ymm7")

GEN_test_RandM(VPSUBSW_256,
               "vpsubsw %%ymm9,  %%ymm8, %%ymm7",
               "vpsubsw (%%rax), %%ymm8, %%ymm7")

GEN_test_RandM(VPCMPEQW_256,
               "vpcmpeqw %%ymm6,  %%ymm8, %%ymm7",
               "vpcmpeqw (%%rax), %%ymm8, %%ymm7")

GEN_test_RandM(VPADDB_256,
               "vpaddb %%ymm6,  %%ymm8, %%ymm7",
               "vpaddb (%%rax), %%ymm8, %%ymm7")

GEN_test_RandM(VPUNPCKHDQ_256,
               "vpunpckhdq %%ymm6,  %%ymm8, %%ymm7",
               "vpunpckhdq (%%rax), %%ymm8, %%ymm7")

GEN_test_RandM(VPMOVSXDQ_256,
               "vpmovsxdq %%xmm6,  %%ymm8",
               "vpmovsxdq (%%rax), %%ymm8")

GEN_test_RandM(VPMOVSXWD_256,
               "vpmovsxwd %%xmm6,  %%ymm8",
               "vpmovsxwd (%%rax), %%ymm8")

GEN_test_RandM(VPMULHW_256,
               "vpmulhw %%ymm9,  %%ymm8, %%ymm7",
               "vpmulhw (%%rax), %%ymm8, %%ymm7")

GEN_test_RandM(VPUNPCKHQDQ_256,
               "vpunpckhqdq %%ymm6,  %%ymm8, %%ymm7",
               "vpunpckhqdq (%%rax), %%ymm8, %%ymm7")

GEN_test_Ronly(VPSRAW_0x05_256,
               "vpsraw $0x5, %%ymm9,  %%ymm7")

GEN_test_RandM(VPCMPGTB_256,
               "vpcmpgtb %%ymm6,  %%ymm8, %%ymm7",
               "vpcmpgtb (%%rax), %%ymm8, %%ymm7")

GEN_test_RandM(VPCMPGTW_256,
               "vpcmpgtw %%ymm6,  %%ymm8, %%ymm7",
               "vpcmpgtw (%%rax), %%ymm8, %%ymm7")

GEN_test_RandM(VPCMPGTD_256,
               "vpcmpgtd %%ymm6,  %%ymm8, %%ymm7",
               "vpcmpgtd (%%rax), %%ymm8, %%ymm7")

GEN_test_RandM(VPMOVZXBD_256,
               "vpmovzxbd %%xmm6,  %%ymm8",
               "vpmovzxbd (%%rax), %%ymm8")

GEN_test_RandM(VPMOVSXBD_256,
               "vpmovsxbd %%xmm6,  %%ymm8",
               "vpmovsxbd (%%rax), %%ymm8")

GEN_test_RandM(VPALIGNR_256_1of3,
               "vpalignr $0, %%ymm6,  %%ymm8, %%ymm7",
               "vpalignr $3, (%%rax), %%ymm8, %%ymm7")
GEN_test_RandM(VPALIGNR_256_2of3,
               "vpalignr $6, %%ymm6,  %%ymm8, %%ymm7",
               "vpalignr $9, (%%rax), %%ymm8, %%ymm7")
GEN_test_RandM(VPALIGNR_256_3of3,
               "vpalignr $12, %%ymm6,  %%ymm8, %%ymm7",
               "vpalignr $15, (%%rax), %%ymm8, %%ymm7")

GEN_test_RandM(VPBLENDW_256_0x00,
               "vpblendw $0x00, %%ymm6,  %%ymm8, %%ymm7",
               "vpblendw $0x01, (%%rax), %%ymm8, %%ymm7")
GEN_test_RandM(VPBLENDW_256_0xFE,
               "vpblendw $0xFE, %%ymm6,  %%ymm8, %%ymm7",
               "vpblendw $0xFF, (%%rax), %%ymm8, %%ymm7")
GEN_test_RandM(VPBLENDW_256_0x30,
               "vpblendw $0x30, %%ymm6,  %%ymm8, %%ymm7",
               "vpblendw $0x03, (%%rax), %%ymm8, %%ymm7")
GEN_test_RandM(VPBLENDW_256_0x21,
               "vpblendw $0x21, %%ymm6,  %%ymm8, %%ymm7",
               "vpblendw $0x12, (%%rax), %%ymm8, %%ymm7")
GEN_test_RandM(VPBLENDW_256_0xD7,
               "vpblendw $0xD7, %%ymm6,  %%ymm8, %%ymm7",
               "vpblendw $0x6C, (%%rax), %%ymm8, %%ymm7")
GEN_test_RandM(VPBLENDW_256_0xB5,
               "vpblendw $0xB5, %%ymm6,  %%ymm8, %%ymm7",
               "vpblendw $0x4A, (%%rax), %%ymm8, %%ymm7")
GEN_test_RandM(VPBLENDW_256_0x85,
               "vpblendw $0x85, %%ymm6,  %%ymm8, %%ymm7",
               "vpblendw $0xDC, (%%rax), %%ymm8, %%ymm7")
GEN_test_RandM(VPBLENDW_256_0x29,
               "vpblendw $0x29, %%ymm6,  %%ymm8, %%ymm7",
               "vpblendw $0x92, (%%rax), %%ymm8, %%ymm7")

GEN_test_RandM(VPSLLW_256,
               "andl $15, %%r14d;"
               "vmovd %%r14d, %%xmm6;"
               "vpsllw %%xmm6,     %%ymm8, %%ymm9",
               "andq $15, 128(%%rax);"
               "vpsllw 128(%%rax), %%ymm8, %%ymm9")

GEN_test_RandM(VPSRLW_256,
               "andl $15, %%r14d;"
               "vmovd %%r14d, %%xmm6;"
               "vpsrlw %%xmm6,     %%ymm8, %%ymm9",
               "andq $15, 128(%%rax);"
               "vpsrlw 128(%%rax), %%ymm8, %%ymm9")

GEN_test_RandM(VPSRAW_256,
               "andl $31, %%r14d;"
               "vmovd %%r14d, %%xmm6;"
               "vpsraw %%xmm6,     %%ymm8, %%ymm9",
               "andq $15, 128(%%rax);"
               "vpsraw 128(%%rax), %%ymm8, %%ymm9")

GEN_test_RandM(VPSLLD_256,
               "andl $31, %%r14d;"
               "vmovd %%r14d, %%xmm6;"
               "vpslld %%xmm6,     %%ymm8, %%ymm9",
               "andq $31, 128(%%rax);"
               "vpslld 128(%%rax), %%ymm8, %%ymm9")

GEN_test_RandM(VPSRLD_256,
               "andl $31, %%r14d;"
               "vmovd %%r14d, %%xmm6;"
               "vpsrld %%xmm6,     %%ymm8, %%ymm9",
               "andq $31, 128(%%rax);"
               "vpsrld 128(%%rax), %%ymm8, %%ymm9")

GEN_test_RandM(VPSRAD_256,
               "andl $31, %%r14d;"
               "vmovd %%r14d, %%xmm6;"
               "vpsrad %%xmm6,     %%ymm8, %%ymm9",
               "andq $31, 128(%%rax);"
               "vpsrad 128(%%rax), %%ymm8, %%ymm9")

GEN_test_RandM(VPSLLQ_256,
               "andl $63, %%r14d;"
               "vmovd %%r14d, %%xmm6;"
               "vpsllq %%xmm6,     %%ymm8, %%ymm9",
               "andq $63, 128(%%rax);"
               "vpsllq 128(%%rax), %%ymm8, %%ymm9")

GEN_test_RandM(VPSRLQ_256,
               "andl $63, %%r14d;"
               "vmovd %%r14d, %%xmm6;"
               "vpsrlq %%xmm6,     %%ymm8, %%ymm9",
               "andq $63, 128(%%rax);"
               "vpsrlq 128(%%rax), %%ymm8, %%ymm9")

GEN_test_RandM(VPMADDWD_256,
               "vpmaddwd %%ymm6,  %%ymm8, %%ymm7",
               "vpmaddwd (%%rax), %%ymm8, %%ymm7")

GEN_test_Monly(VMOVNTDQA_256,
               "vmovntdqa (%%rax), %%ymm9")

GEN_test_RandM(VPACKSSWB_256,
               "vpacksswb %%ymm6,  %%ymm8, %%ymm7",
               "vpacksswb (%%rax), %%ymm8, %%ymm7")

GEN_test_RandM(VPAVGB_256,
               "vpavgb %%ymm6,  %%ymm8, %%ymm7",
               "vpavgb (%%rax), %%ymm8, %%ymm7")

GEN_test_RandM(VPAVGW_256,
               "vpavgw %%ymm6,  %%ymm8, %%ymm7",
               "vpavgw (%%rax), %%ymm8, %%ymm7")

GEN_test_RandM(VPADDSB_256,
               "vpaddsb %%ymm6,  %%ymm8, %%ymm7",
               "vpaddsb (%%rax), %%ymm8, %%ymm7")

GEN_test_RandM(VPADDSW_256,
               "vpaddsw %%ymm6,  %%ymm8, %%ymm7",
               "vpaddsw (%%rax), %%ymm8, %%ymm7")

GEN_test_RandM(VPHADDW_256,
               "vphaddw %%ymm6,  %%ymm8, %%ymm7",
               "vphaddw (%%rax), %%ymm8, %%ymm7")

GEN_test_RandM(VPHADDD_256,
               "vphaddd %%ymm6,  %%ymm8, %%ymm7",
               "vphaddd (%%rax), %%ymm8, %%ymm7")

GEN_test_RandM(VPHADDSW_256,
               "vphaddsw %%ymm6,  %%ymm8, %%ymm7",
               "vphaddsw (%%rax), %%ymm8, %%ymm7")

GEN_test_RandM(VPMADDUBSW_256,
               "vpmaddubsw %%ymm6,  %%ymm8, %%ymm7",
               "vpmaddubsw (%%rax), %%ymm8, %%ymm7")

GEN_test_RandM(VPHSUBW_256,
               "vphsubw %%ymm6,  %%ymm8, %%ymm7",
               "vphsubw (%%rax), %%ymm8, %%ymm7")

GEN_test_RandM(VPHSUBD_256,
               "vphsubd %%ymm6,  %%ymm8, %%ymm7",
               "vphsubd (%%rax), %%ymm8, %%ymm7")

GEN_test_RandM(VPHSUBSW_256,
               "vphsubsw %%ymm6,  %%ymm8, %%ymm7",
               "vphsubsw (%%rax), %%ymm8, %%ymm7")

GEN_test_RandM(VPABSB_256,
               "vpabsb %%ymm6,  %%ymm7",
               "vpabsb (%%rax), %%ymm7")

GEN_test_RandM(VPABSW_256,
               "vpabsw %%ymm6,  %%ymm7",
               "vpabsw (%%rax), %%ymm7")

GEN_test_RandM(VPMOVSXBQ_256,
               "vpmovsxbq %%xmm6,  %%ymm8",
               "vpmovsxbq (%%rax), %%ymm8")

GEN_test_RandM(VPMOVSXWQ_256,
               "vpmovsxwq %%xmm6,  %%ymm8",
               "vpmovsxwq (%%rax), %%ymm8")

GEN_test_RandM(VPACKUSDW_256,
               "vpackusdw %%ymm6,  %%ymm8, %%ymm7",
               "vpackusdw (%%rax), %%ymm8, %%ymm7")

GEN_test_RandM(VPMOVZXBQ_256,
               "vpmovzxbq %%xmm6,  %%ymm8",
               "vpmovzxbq (%%rax), %%ymm8")

GEN_test_RandM(VPMOVZXWQ_256,
               "vpmovzxwq %%xmm6,  %%ymm8",
               "vpmovzxwq (%%rax), %%ymm8")

GEN_test_RandM(VPMOVZXDQ_256,
               "vpmovzxdq %%xmm6,  %%ymm8",
               "vpmovzxdq (%%rax), %%ymm8")

GEN_test_RandM(VMPSADBW_256_0x0,
               "vmpsadbw $0, %%ymm6,  %%ymm8, %%ymm7",
               "vmpsadbw $0, (%%rax), %%ymm8, %%ymm7")
GEN_test_RandM(VMPSADBW_256_0x39,
               "vmpsadbw $0x39, %%ymm6,  %%ymm8, %%ymm7",
               "vmpsadbw $0x39, (%%rax), %%ymm8, %%ymm7")
GEN_test_RandM(VMPSADBW_256_0x32,
               "vmpsadbw $0x32, %%ymm6,  %%ymm8, %%ymm7",
               "vmpsadbw $0x32, (%%rax), %%ymm8, %%ymm7")
GEN_test_RandM(VMPSADBW_256_0x2b,
               "vmpsadbw $0x2b, %%ymm6,  %%ymm8, %%ymm7",
               "vmpsadbw $0x2b, (%%rax), %%ymm8, %%ymm7")
GEN_test_RandM(VMPSADBW_256_0x24,
               "vmpsadbw $0x24, %%ymm6,  %%ymm8, %%ymm7",
               "vmpsadbw $0x24, (%%rax), %%ymm8, %%ymm7")
GEN_test_RandM(VMPSADBW_256_0x1d,
               "vmpsadbw $0x1d, %%ymm6,  %%ymm8, %%ymm7",
               "vmpsadbw $0x1d, (%%rax), %%ymm8, %%ymm7")
GEN_test_RandM(VMPSADBW_256_0x16,
               "vmpsadbw $0x16, %%ymm6,  %%ymm8, %%ymm7",
               "vmpsadbw $0x16, (%%rax), %%ymm8, %%ymm7")
GEN_test_RandM(VMPSADBW_256_0x0f,
               "vmpsadbw $0x0f, %%ymm6,  %%ymm8, %%ymm7",
               "vmpsadbw $0x0f, (%%rax), %%ymm8, %%ymm7")

GEN_test_RandM(VPSADBW_256,
               "vpsadbw %%ymm6,  %%ymm8, %%ymm7",
               "vpsadbw (%%rax), %%ymm8, %%ymm7")

GEN_test_RandM(VPSIGNB_256,
               "vpsignb %%ymm6,  %%ymm8, %%ymm7",
               "vpsignb (%%rax), %%ymm8, %%ymm7")

GEN_test_RandM(VPSIGNW_256,
               "vpsignw %%ymm6,  %%ymm8, %%ymm7",
               "vpsignw (%%rax), %%ymm8, %%ymm7")

GEN_test_RandM(VPSIGND_256,
               "vpsignd %%ymm6,  %%ymm8, %%ymm7",
               "vpsignd (%%rax), %%ymm8, %%ymm7")

GEN_test_RandM(VPMULHRSW_256,
               "vpmulhrsw %%ymm6,  %%ymm8, %%ymm7",
               "vpmulhrsw (%%rax), %%ymm8, %%ymm7")

/* Instructions new in AVX2.  */

GEN_test_Monly(VBROADCASTI128,
               "vbroadcasti128 (%%rax), %%ymm9")

GEN_test_RandM(VEXTRACTI128_0x0,
               "vextracti128 $0x0, %%ymm7, %%xmm9",
               "vextracti128 $0x0, %%ymm7, (%%rax)")

GEN_test_RandM(VEXTRACTI128_0x1,
               "vextracti128 $0x1, %%ymm7, %%xmm9",
               "vextracti128 $0x1, %%ymm7, (%%rax)")

GEN_test_RandM(VINSERTI128_0x0,
               "vinserti128 $0x0, %%xmm9,  %%ymm7, %%ymm8",
               "vinserti128 $0x0, (%%rax), %%ymm7, %%ymm8")

GEN_test_RandM(VINSERTI128_0x1,
               "vinserti128 $0x1, %%xmm9,  %%ymm7, %%ymm8",
               "vinserti128 $0x1, (%%rax), %%ymm7, %%ymm8")

GEN_test_RandM(VPERM2I128_0x00,
               "vperm2i128 $0x00, %%ymm6,  %%ymm8, %%ymm7",
               "vperm2i128 $0x00, (%%rax), %%ymm8, %%ymm7")
GEN_test_RandM(VPERM2I128_0xFF,
               "vperm2i128 $0xFF, %%ymm6,  %%ymm8, %%ymm7",
               "vperm2i128 $0xFF, (%%rax), %%ymm8, %%ymm7")
GEN_test_RandM(VPERM2I128_0x30,
               "vperm2i128 $0x30, %%ymm6,  %%ymm8, %%ymm7",
               "vperm2i128 $0x30, (%%rax), %%ymm8, %%ymm7")
GEN_test_RandM(VPERM2I128_0x21,
               "vperm2i128 $0x21, %%ymm6,  %%ymm8, %%ymm7",
               "vperm2i128 $0x21, (%%rax), %%ymm8, %%ymm7")
GEN_test_RandM(VPERM2I128_0x12,
               "vperm2i128 $0x12, %%ymm6,  %%ymm8, %%ymm7",
               "vperm2i128 $0x12, (%%rax), %%ymm8, %%ymm7")
GEN_test_RandM(VPERM2I128_0x03,
               "vperm2i128 $0x03, %%ymm6,  %%ymm8, %%ymm7",
               "vperm2i128 $0x03, (%%rax), %%ymm8, %%ymm7")
GEN_test_RandM(VPERM2I128_0x85,
               "vperm2i128 $0x85, %%ymm6,  %%ymm8, %%ymm7",
               "vperm2i128 $0x85, (%%rax), %%ymm8, %%ymm7")
GEN_test_RandM(VPERM2I128_0x5A,
               "vperm2i128 $0x5A, %%ymm6,  %%ymm8, %%ymm7",
               "vperm2i128 $0x5A, (%%rax), %%ymm8, %%ymm7")

GEN_test_Ronly(VBROADCASTSS_128,
               "vbroadcastss %%xmm9, %%xmm7")

GEN_test_Ronly(VBROADCASTSS_256,
               "vbroadcastss %%xmm9, %%ymm7")

GEN_test_Ronly(VBROADCASTSD_256,
               "vbroadcastsd %%xmm9, %%ymm7")

GEN_test_RandM(VPERMD,
               "vpermd %%ymm6, %%ymm7, %%ymm9",
               "vpermd (%%rax), %%ymm7, %%ymm9")

GEN_test_RandM(VPERMQ_0x00,
               "vpermq $0x00, %%ymm6,  %%ymm7",
               "vpermq $0x01, (%%rax), %%ymm7")
GEN_test_RandM(VPERMQ_0xFE,
               "vpermq $0xFE, %%ymm6,  %%ymm7",
               "vpermq $0xFF, (%%rax), %%ymm7")
GEN_test_RandM(VPERMQ_0x30,
               "vpermq $0x30, %%ymm6,  %%ymm7",
               "vpermq $0x03, (%%rax), %%ymm7")
GEN_test_RandM(VPERMQ_0x21,
               "vpermq $0x21, %%ymm6,  %%ymm7",
               "vpermq $0x12, (%%rax), %%ymm7")
GEN_test_RandM(VPERMQ_0xD7,
               "vpermq $0xD7, %%ymm6,  %%ymm7",
               "vpermq $0x6C, (%%rax), %%ymm7")
GEN_test_RandM(VPERMQ_0xB5,
               "vpermq $0xB5, %%ymm6,  %%ymm7",
               "vpermq $0x4A, (%%rax), %%ymm7")
GEN_test_RandM(VPERMQ_0x85,
               "vpermq $0x85, %%ymm6,  %%ymm7",
               "vpermq $0xDC, (%%rax), %%ymm7")
GEN_test_RandM(VPERMQ_0x29,
               "vpermq $0x29, %%ymm6,  %%ymm7",
               "vpermq $0x92, (%%rax), %%ymm7")

GEN_test_RandM(VPERMPS,
               "vpermps %%ymm6, %%ymm7, %%ymm9",
               "vpermps (%%rax), %%ymm7, %%ymm9")

GEN_test_RandM(VPERMPD_0x00,
               "vpermpd $0x00, %%ymm6,  %%ymm7",
               "vpermpd $0x01, (%%rax), %%ymm7")
GEN_test_RandM(VPERMPD_0xFE,
               "vpermpd $0xFE, %%ymm6,  %%ymm7",
               "vpermpd $0xFF, (%%rax), %%ymm7")
GEN_test_RandM(VPERMPD_0x30,
               "vpermpd $0x30, %%ymm6,  %%ymm7",
               "vpermpd $0x03, (%%rax), %%ymm7")
GEN_test_RandM(VPERMPD_0x21,
               "vpermpd $0x21, %%ymm6,  %%ymm7",
               "vpermpd $0x12, (%%rax), %%ymm7")
GEN_test_RandM(VPERMPD_0xD7,
               "vpermpd $0xD7, %%ymm6,  %%ymm7",
               "vpermpd $0x6C, (%%rax), %%ymm7")
GEN_test_RandM(VPERMPD_0xB5,
               "vpermpd $0xB5, %%ymm6,  %%ymm7",
               "vpermpd $0x4A, (%%rax), %%ymm7")
GEN_test_RandM(VPERMPD_0x85,
               "vpermpd $0x85, %%ymm6,  %%ymm7",
               "vpermpd $0xDC, (%%rax), %%ymm7")
GEN_test_RandM(VPERMPD_0x29,
               "vpermpd $0x29, %%ymm6,  %%ymm7",
               "vpermpd $0x92, (%%rax), %%ymm7")

GEN_test_RandM(VPBLENDD_128_0x00,
               "vpblendd $0x00, %%xmm6,  %%xmm8, %%xmm7",
               "vpblendd $0x01, (%%rax), %%xmm8, %%xmm7")
GEN_test_RandM(VPBLENDD_128_0x02,
               "vpblendd $0x02, %%xmm6,  %%xmm8, %%xmm7",
               "vpblendd $0x03, (%%rax), %%xmm8, %%xmm7")
GEN_test_RandM(VPBLENDD_128_0x04,
               "vpblendd $0x04, %%xmm6,  %%xmm8, %%xmm7",
               "vpblendd $0x05, (%%rax), %%xmm8, %%xmm7")
GEN_test_RandM(VPBLENDD_128_0x06,
               "vpblendd $0x06, %%xmm6,  %%xmm8, %%xmm7",
               "vpblendd $0x07, (%%rax), %%xmm8, %%xmm7")
GEN_test_RandM(VPBLENDD_128_0x08,
               "vpblendd $0x08, %%xmm6,  %%xmm8, %%xmm7",
               "vpblendd $0x09, (%%rax), %%xmm8, %%xmm7")
GEN_test_RandM(VPBLENDD_128_0x0A,
               "vpblendd $0x0A, %%xmm6,  %%xmm8, %%xmm7",
               "vpblendd $0x0B, (%%rax), %%xmm8, %%xmm7")
GEN_test_RandM(VPBLENDD_128_0x0C,
               "vpblendd $0x0C, %%xmm6,  %%xmm8, %%xmm7",
               "vpblendd $0x0D, (%%rax), %%xmm8, %%xmm7")
GEN_test_RandM(VPBLENDD_128_0x0E,
               "vpblendd $0x0E, %%xmm6,  %%xmm8, %%xmm7",
               "vpblendd $0x0F, (%%rax), %%xmm8, %%xmm7")

GEN_test_RandM(VPBLENDD_256_0x00,
               "vpblendd $0x00, %%ymm6,  %%ymm8, %%ymm7",
               "vpblendd $0x01, (%%rax), %%ymm8, %%ymm7")
GEN_test_RandM(VPBLENDD_256_0xFE,
               "vpblendd $0xFE, %%ymm6,  %%ymm8, %%ymm7",
               "vpblendd $0xFF, (%%rax), %%ymm8, %%ymm7")
GEN_test_RandM(VPBLENDD_256_0x30,
               "vpblendd $0x30, %%ymm6,  %%ymm8, %%ymm7",
               "vpblendd $0x03, (%%rax), %%ymm8, %%ymm7")
GEN_test_RandM(VPBLENDD_256_0x21,
               "vpblendd $0x21, %%ymm6,  %%ymm8, %%ymm7",
               "vpblendd $0x12, (%%rax), %%ymm8, %%ymm7")
GEN_test_RandM(VPBLENDD_256_0xD7,
               "vpblendd $0xD7, %%ymm6,  %%ymm8, %%ymm7",
               "vpblendd $0x6C, (%%rax), %%ymm8, %%ymm7")
GEN_test_RandM(VPBLENDD_256_0xB5,
               "vpblendd $0xB5, %%ymm6,  %%ymm8, %%ymm7",
               "vpblendd $0x4A, (%%rax), %%ymm8, %%ymm7")
GEN_test_RandM(VPBLENDD_256_0x85,
               "vpblendd $0x85, %%ymm6,  %%ymm8, %%ymm7",
               "vpblendd $0xDC, (%%rax), %%ymm8, %%ymm7")
GEN_test_RandM(VPBLENDD_256_0x29,
               "vpblendd $0x29, %%ymm6,  %%ymm8, %%ymm7",
               "vpblendd $0x92, (%%rax), %%ymm8, %%ymm7")

GEN_test_RandM(VPSLLVD_128,
               "vpslld $27, %%xmm6, %%xmm6;"
               "vpsrld $27, %%xmm6, %%xmm6;"
               "vpsllvd %%xmm6, %%xmm8, %%xmm7",
               "andl $31, (%%rax);"
               "andl $31, 4(%%rax);"
               "andl $31, 8(%%rax);"
               "vpsllvd (%%rax), %%xmm8, %%xmm7")

GEN_test_RandM(VPSLLVD_256,
               "vpslld $27, %%ymm6, %%ymm6;"
               "vpsrld $27, %%ymm6, %%ymm6;"
               "vpsllvd %%ymm6, %%ymm8, %%ymm7",
               "andl $31, (%%rax);"
               "andl $31, 4(%%rax);"
               "andl $31, 8(%%rax);"
               "andl $31, 16(%%rax);"
               "andl $31, 20(%%rax);"
               "andl $31, 24(%%rax);"
               "vpsllvd (%%rax), %%ymm8, %%ymm7")

GEN_test_RandM(VPSLLVQ_128,
               "vpsllq $58, %%xmm6, %%xmm6;"
               "vpsrlq $58, %%xmm6, %%xmm6;"
               "vpsllvq %%xmm6, %%xmm8, %%xmm7",
               "andl $63, (%%rax);"
               "vpsllvq (%%rax), %%xmm8, %%xmm7")

GEN_test_RandM(VPSLLVQ_256,
               "vpsllq $58, %%ymm6, %%ymm6;"
               "vpsrlq $58, %%ymm6, %%ymm6;"
               "vpsllvq %%ymm6, %%ymm8, %%ymm7",
               "andl $63, (%%rax);"
               "andl $63, 8(%%rax);"
               "andl $63, 16(%%rax);"
               "vpsllvq (%%rax), %%ymm8, %%ymm7")

GEN_test_RandM(VPSRLVD_128,
               "vpslld $27, %%xmm6, %%xmm6;"
               "vpsrld $27, %%xmm6, %%xmm6;"
               "vpsrlvd %%xmm6, %%xmm8, %%xmm7",
               "andl $31, (%%rax);"
               "andl $31, 4(%%rax);"
               "andl $31, 8(%%rax);"
               "vpsrlvd (%%rax), %%xmm8, %%xmm7")

GEN_test_RandM(VPSRLVD_256,
               "vpslld $27, %%ymm6, %%ymm6;"
               "vpsrld $27, %%ymm6, %%ymm6;"
               "vpsrlvd %%ymm6, %%ymm8, %%ymm7",
               "andl $31, (%%rax);"
               "andl $31, 4(%%rax);"
               "andl $31, 8(%%rax);"
               "andl $31, 16(%%rax);"
               "andl $31, 20(%%rax);"
               "andl $31, 24(%%rax);"
               "vpsrlvd (%%rax), %%ymm8, %%ymm7")

GEN_test_RandM(VPSRLVQ_128,
               "vpsllq $58, %%xmm6, %%xmm6;"
               "vpsrlq $58, %%xmm6, %%xmm6;"
               "vpsrlvq %%xmm6, %%xmm8, %%xmm7",
               "andl $63, (%%rax);"
               "vpsrlvq (%%rax), %%xmm8, %%xmm7")

GEN_test_RandM(VPSRLVQ_256,
               "vpsllq $58, %%ymm6, %%ymm6;"
               "vpsrlq $58, %%ymm6, %%ymm6;"
               "vpsrlvq %%ymm6, %%ymm8, %%ymm7",
               "andl $63, (%%rax);"
               "andl $63, 8(%%rax);"
               "andl $63, 16(%%rax);"
               "vpsrlvq (%%rax), %%ymm8, %%ymm7")

GEN_test_RandM(VPSRAVD_128,
               "vpslld $27, %%xmm6, %%xmm6;"
               "vpsrld $27, %%xmm6, %%xmm6;"
               "vpsravd %%xmm6, %%xmm8, %%xmm7",
               "andl $31, (%%rax);"
               "andl $31, 4(%%rax);"
               "andl $31, 8(%%rax);"
               "vpsravd (%%rax), %%xmm8, %%xmm7")

GEN_test_RandM(VPSRAVD_256,
               "vpslld $27, %%ymm6, %%ymm6;"
               "vpsrld $27, %%ymm6, %%ymm6;"
               "vpsravd %%ymm6, %%ymm8, %%ymm7",
               "andl $31, (%%rax);"
               "andl $31, 4(%%rax);"
               "andl $31, 8(%%rax);"
               "andl $31, 16(%%rax);"
               "andl $31, 20(%%rax);"
               "andl $31, 24(%%rax);"
               "vpsravd (%%rax), %%ymm8, %%ymm7")

GEN_test_RandM(VPBROADCASTB_128,
               "vpbroadcastb %%xmm9, %%xmm7",
               "vpbroadcastb (%%rax), %%xmm7")

GEN_test_RandM(VPBROADCASTB_256,
               "vpbroadcastb %%xmm9, %%ymm7",
               "vpbroadcastb (%%rax), %%ymm7")

GEN_test_RandM(VPBROADCASTW_128,
               "vpbroadcastw %%xmm9, %%xmm7",
               "vpbroadcastw (%%rax), %%xmm7")

GEN_test_RandM(VPBROADCASTW_256,
               "vpbroadcastw %%xmm9, %%ymm7",
               "vpbroadcastw (%%rax), %%ymm7")

GEN_test_RandM(VPBROADCASTD_128,
               "vpbroadcastd %%xmm9, %%xmm7",
               "vpbroadcastd (%%rax), %%xmm7")

GEN_test_RandM(VPBROADCASTD_256,
               "vpbroadcastd %%xmm9, %%ymm7",
               "vpbroadcastd (%%rax), %%ymm7")

GEN_test_RandM(VPBROADCASTQ_128,
               "vpbroadcastq %%xmm9, %%xmm7",
               "vpbroadcastq (%%rax), %%xmm7")

GEN_test_RandM(VPBROADCASTQ_256,
               "vpbroadcastq %%xmm9, %%ymm7",
               "vpbroadcastq (%%rax), %%ymm7")

GEN_test_Monly(VPMASKMOVD_128_LoadForm,
               "vpmaskmovd (%%rax), %%xmm8, %%xmm7;"
               "vxorps %%xmm6, %%xmm6, %%xmm6;"
               "vpmaskmovd (%%rax,%%rax,4), %%xmm6, %%xmm9")

GEN_test_Monly(VPMASKMOVD_256_LoadForm,
               "vpmaskmovd (%%rax), %%ymm8, %%ymm7;"
               "vxorps %%ymm6, %%ymm6, %%ymm6;"
               "vpmaskmovd (%%rax,%%rax,4), %%ymm6, %%ymm9")

GEN_test_Monly(VPMASKMOVQ_128_LoadForm,
               "vpmaskmovq (%%rax), %%xmm8, %%xmm7;"
               "vxorpd %%xmm6, %%xmm6, %%xmm6;"
               "vpmaskmovq (%%rax,%%rax,4), %%xmm6, %%xmm9")

GEN_test_Monly(VPMASKMOVQ_256_LoadForm,
               "vpmaskmovq (%%rax), %%ymm8, %%ymm7;"
               "vxorpd %%ymm6, %%ymm6, %%ymm6;"
               "vpmaskmovq (%%rax,%%rax,4), %%ymm6, %%ymm9")

GEN_test_Monly(VPMASKMOVD_128_StoreForm,
               "vpmaskmovd %%xmm8, %%xmm7, (%%rax);"
               "vxorps %%xmm6, %%xmm6, %%xmm6;"
               "vpmaskmovd %%xmm9, %%xmm6, (%%rax,%%rax,4)")

GEN_test_Monly(VPMASKMOVD_256_StoreForm,
               "vpmaskmovd %%ymm8, %%ymm7, (%%rax);"
               "vxorps %%ymm6, %%ymm6, %%ymm6;"
               "vpmaskmovd %%ymm9, %%ymm6, (%%rax,%%rax,4)")

GEN_test_Monly(VPMASKMOVQ_128_StoreForm,
               "vpmaskmovq %%xmm8, %%xmm7, (%%rax);"
               "vxorpd %%xmm6, %%xmm6, %%xmm6;"
               "vpmaskmovq %%xmm9, %%xmm6, (%%rax,%%rax,4)")

GEN_test_Monly(VPMASKMOVQ_256_StoreForm,
               "vpmaskmovq %%ymm8, %%ymm7, (%%rax);"
               "vxorpd %%ymm6, %%ymm6, %%ymm6;"
               "vpmaskmovq %%ymm9, %%ymm6, (%%rax,%%rax,4)")

GEN_test_Ronly(VGATHERDPS_128,
               "vpslld $25, %%xmm7, %%xmm8;"
               "vpsrld $25, %%xmm8, %%xmm8;"
               "vblendvps %%xmm6, %%xmm8, %%xmm7, %%xmm8;"
               "leaq _randArray(%%rip), %%r14;"
               "vgatherdps %%xmm6, 3(%%r14,%%xmm8,4), %%xmm9;"
               "xorl %%r14d, %%r14d")

GEN_test_Ronly(VGATHERDPS_256,
               "vpslld $25, %%ymm7, %%ymm8;"
               "vpsrld $25, %%ymm8, %%ymm8;"
               "vblendvps %%ymm6, %%ymm8, %%ymm7, %%ymm8;"
               "leaq _randArray(%%rip), %%r14;"
               "vgatherdps %%ymm6, 3(%%r14,%%ymm8,4), %%ymm9;"
               "xorl %%r14d, %%r14d")

GEN_test_Ronly(VGATHERQPS_128_1,
               "vpsllq $57, %%xmm7, %%xmm8;"
               "vpsrlq $57, %%xmm8, %%xmm8;"
               "vpmovsxdq %%xmm6, %%xmm9;"
               "vblendvpd %%xmm9, %%xmm8, %%xmm7, %%xmm8;"
               "vmovdqa 96(%0), %%ymm9;"
               "leaq _randArray(%%rip), %%r14;"
               "vgatherqps %%xmm6, 3(%%r14,%%xmm8,4), %%xmm9;"
               "xorl %%r14d, %%r14d")

GEN_test_Ronly(VGATHERQPS_256_1,
               "vpsllq $57, %%ymm7, %%ymm8;"
               "vpsrlq $57, %%ymm8, %%ymm8;"
               "vpmovsxdq %%xmm6, %%ymm9;"
               "vblendvpd %%ymm9, %%ymm8, %%ymm7, %%ymm8;"
               "vmovdqa 96(%0), %%ymm9;"
               "leaq _randArray(%%rip), %%r14;"
               "vgatherqps %%xmm6, 3(%%r14,%%ymm8,4), %%xmm9;"
               "xorl %%r14d, %%r14d")

GEN_test_Ronly(VGATHERQPS_128_2,
               "vpsllq $57, %%xmm7, %%xmm8;"
               "vpsrlq $57, %%xmm8, %%xmm8;"
               "vpmovsxdq %%xmm6, %%xmm9;"
               "vblendvpd %%xmm9, %%xmm8, %%xmm7, %%xmm8;"
               "vmovdqa 96(%0), %%ymm9;"
               "leaq _randArray(%%rip), %%r14;"
               "vmovq %%r14, %%xmm7;"
               "vpsllq $2, %%xmm8, %%xmm8;"
               "vpbroadcastq %%xmm7, %%xmm7;"
               "vpaddq %%xmm7, %%xmm8, %%xmm8;"
               "vgatherqps %%xmm6, 1(,%%xmm8,1), %%xmm9;"
               "vpsubq %%xmm7, %%xmm8, %%xmm8;"
               "vmovdqa 0(%0), %%ymm7;"
               "xorl %%r14d, %%r14d")

GEN_test_Ronly(VGATHERQPS_256_2,
               "vpsllq $57, %%ymm7, %%ymm8;"
               "vpsrlq $57, %%ymm8, %%ymm8;"
               "vpmovsxdq %%xmm6, %%ymm9;"
               "vblendvpd %%ymm9, %%ymm8, %%ymm7, %%ymm8;"
               "vmovdqa 96(%0), %%ymm9;"
               "leaq _randArray(%%rip), %%r14;"
               "vmovq %%r14, %%xmm7;"
               "vpsllq $2, %%ymm8, %%ymm8;"
               "vpbroadcastq %%xmm7, %%ymm7;"
               "vpaddq %%ymm7, %%ymm8, %%ymm8;"
               "vgatherqps %%xmm6, 1(,%%ymm8,1), %%xmm9;"
               "vpsubq %%ymm7, %%ymm8, %%ymm8;"
               "vmovdqa 0(%0), %%ymm7;"
               "xorl %%r14d, %%r14d")

GEN_test_Ronly(VGATHERDPD_128,
               "vpslld $26, %%xmm7, %%xmm8;"
               "vpsrld $26, %%xmm8, %%xmm8;"
               "vshufps $13, %%xmm6, %%xmm6, %%xmm9;"
               "vblendvps %%xmm9, %%xmm8, %%xmm7, %%xmm8;"
               "vmovdqa 96(%0), %%ymm9;"
               "leaq _randArray(%%rip), %%r14;"
               "vgatherdpd %%xmm6, 3(%%r14,%%xmm8,8), %%xmm9;"
               "xorl %%r14d, %%r14d")

GEN_test_Ronly(VGATHERDPD_256,
               "vpslld $26, %%ymm7, %%ymm8;"
               "vpsrld $26, %%ymm8, %%ymm8;"
               "vextracti128 $1, %%ymm6, %%xmm9;"
               "vshufps $221, %%ymm9, %%ymm6, %%ymm9;"
               "vblendvps %%ymm9, %%ymm8, %%ymm7, %%ymm8;"
               "vmovdqa 96(%0), %%ymm9;"
               "leaq _randArray(%%rip), %%r14;"
               "vgatherdpd %%ymm6, 3(%%r14,%%xmm8,8), %%ymm9;"
               "xorl %%r14d, %%r14d")

GEN_test_Ronly(VGATHERQPD_128_1,
               "vpsllq $58, %%xmm7, %%xmm8;"
               "vpsrlq $58, %%xmm8, %%xmm8;"
               "vblendvpd %%xmm6, %%xmm8, %%xmm7, %%xmm8;"
               "leaq _randArray(%%rip), %%r14;"
               "vgatherqpd %%xmm6, 3(%%r14,%%xmm8,8), %%xmm9;"
               "xorl %%r14d, %%r14d")

GEN_test_Ronly(VGATHERQPD_256_1,
               "vpsllq $58, %%ymm7, %%ymm8;"
               "vpsrlq $58, %%ymm8, %%ymm8;"
               "vblendvpd %%ymm6, %%ymm8, %%ymm7, %%ymm8;"
               "leaq _randArray(%%rip), %%r14;"
               "vgatherqpd %%ymm6, 3(%%r14,%%ymm8,8), %%ymm9;"
               "xorl %%r14d, %%r14d")

GEN_test_Ronly(VGATHERQPD_128_2,
               "vpsllq $58, %%xmm7, %%xmm8;"
               "vpsrlq $58, %%xmm8, %%xmm8;"
               "vblendvpd %%xmm6, %%xmm8, %%xmm7, %%xmm8;"
               "leaq _randArray(%%rip), %%r14;"
               "vmovq %%r14, %%xmm7;"
               "vpsllq $2, %%xmm8, %%xmm8;"
               "vpbroadcastq %%xmm7, %%xmm7;"
               "vpaddq %%xmm7, %%xmm8, %%xmm8;"
               "vgatherqpd %%xmm6, 1(,%%xmm8,1), %%xmm9;"
               "vpsubq %%xmm7, %%xmm8, %%xmm8;"
               "vmovdqa 0(%0), %%ymm7;"
               "xorl %%r14d, %%r14d")

GEN_test_Ronly(VGATHERQPD_256_2,
               "vpsllq $58, %%ymm7, %%ymm8;"
               "vpsrlq $58, %%ymm8, %%ymm8;"
               "vblendvpd %%ymm6, %%ymm8, %%ymm7, %%ymm8;"
               "leaq _randArray(%%rip), %%r14;"
               "vmovq %%r14, %%xmm7;"
               "vpsllq $2, %%ymm8, %%ymm8;"
               "vpbroadcastq %%xmm7, %%ymm7;"
               "vpaddq %%ymm7, %%ymm8, %%ymm8;"
               "vgatherqpd %%ymm6, 1(,%%ymm8,1), %%ymm9;"
               "vpsubq %%ymm7, %%ymm8, %%ymm8;"
               "vmovdqa 0(%0), %%ymm7;"
               "xorl %%r14d, %%r14d")

GEN_test_Ronly(VPGATHERDD_128,
               "vpslld $25, %%xmm7, %%xmm8;"
               "vpsrld $25, %%xmm8, %%xmm8;"
               "vblendvps %%xmm6, %%xmm8, %%xmm7, %%xmm8;"
               "leaq _randArray(%%rip), %%r14;"
               "vpgatherdd %%xmm6, 3(%%r14,%%xmm8,4), %%xmm9;"
               "xorl %%r14d, %%r14d")

GEN_test_Ronly(VPGATHERDD_256,
               "vpslld $25, %%ymm7, %%ymm8;"
               "vpsrld $25, %%ymm8, %%ymm8;"
               "vblendvps %%ymm6, %%ymm8, %%ymm7, %%ymm8;"
               "leaq _randArray(%%rip), %%r14;"
               "vpgatherdd %%ymm6, 3(%%r14,%%ymm8,4), %%ymm9;"
               "xorl %%r14d, %%r14d")

GEN_test_Ronly(VPGATHERQD_128_1,
               "vpsllq $57, %%xmm7, %%xmm8;"
               "vpsrlq $57, %%xmm8, %%xmm8;"
               "vpmovsxdq %%xmm6, %%xmm9;"
               "vblendvpd %%xmm9, %%xmm8, %%xmm7, %%xmm8;"
               "vmovdqa 96(%0), %%ymm9;"
               "leaq _randArray(%%rip), %%r14;"
               "vpgatherqd %%xmm6, 3(%%r14,%%xmm8,4), %%xmm9;"
               "xorl %%r14d, %%r14d")

GEN_test_Ronly(VPGATHERQD_256_1,
               "vpsllq $57, %%ymm7, %%ymm8;"
               "vpsrlq $57, %%ymm8, %%ymm8;"
               "vpmovsxdq %%xmm6, %%ymm9;"
               "vblendvpd %%ymm9, %%ymm8, %%ymm7, %%ymm8;"
               "vmovdqa 96(%0), %%ymm9;"
               "leaq _randArray(%%rip), %%r14;"
               "vpgatherqd %%xmm6, 3(%%r14,%%ymm8,4), %%xmm9;"
               "xorl %%r14d, %%r14d")

GEN_test_Ronly(VPGATHERQD_128_2,
               "vpsllq $57, %%xmm7, %%xmm8;"
               "vpsrlq $57, %%xmm8, %%xmm8;"
               "vpmovsxdq %%xmm6, %%xmm9;"
               "vblendvpd %%xmm9, %%xmm8, %%xmm7, %%xmm8;"
               "vmovdqa 96(%0), %%ymm9;"
               "leaq _randArray(%%rip), %%r14;"
               "vmovq %%r14, %%xmm7;"
               "vpsllq $2, %%xmm8, %%xmm8;"
               "vpbroadcastq %%xmm7, %%xmm7;"
               "vpaddq %%xmm7, %%xmm8, %%xmm8;"
               "vpgatherqd %%xmm6, 1(,%%xmm8,1), %%xmm9;"
               "vpsubq %%xmm7, %%xmm8, %%xmm8;"
               "vmovdqa 0(%0), %%ymm7;"
               "xorl %%r14d, %%r14d")

GEN_test_Ronly(VPGATHERQD_256_2,
               "vpsllq $57, %%ymm7, %%ymm8;"
               "vpsrlq $57, %%ymm8, %%ymm8;"
               "vpmovsxdq %%xmm6, %%ymm9;"
               "vblendvpd %%ymm9, %%ymm8, %%ymm7, %%ymm8;"
               "vmovdqa 96(%0), %%ymm9;"
               "leaq _randArray(%%rip), %%r14;"
               "vmovq %%r14, %%xmm7;"
               "vpsllq $2, %%ymm8, %%ymm8;"
               "vpbroadcastq %%xmm7, %%ymm7;"
               "vpaddq %%ymm7, %%ymm8, %%ymm8;"
               "vpgatherqd %%xmm6, 1(,%%ymm8,1), %%xmm9;"
               "vpsubq %%ymm7, %%ymm8, %%ymm8;"
               "vmovdqa 0(%0), %%ymm7;"
               "xorl %%r14d, %%r14d")

GEN_test_Ronly(VPGATHERDQ_128,
               "vpslld $26, %%xmm7, %%xmm8;"
               "vpsrld $26, %%xmm8, %%xmm8;"
               "vshufps $13, %%xmm6, %%xmm6, %%xmm9;"
               "vblendvps %%xmm9, %%xmm8, %%xmm7, %%xmm8;"
               "vmovdqa 96(%0), %%ymm9;"
               "leaq _randArray(%%rip), %%r14;"
               "vpgatherdq %%xmm6, 3(%%r14,%%xmm8,8), %%xmm9;"
               "xorl %%r14d, %%r14d")

GEN_test_Ronly(VPGATHERDQ_256,
               "vpslld $26, %%ymm7, %%ymm8;"
               "vpsrld $26, %%ymm8, %%ymm8;"
               "vextracti128 $1, %%ymm6, %%xmm9;"
               "vshufps $221, %%ymm9, %%ymm6, %%ymm9;"
               "vblendvps %%ymm9, %%ymm8, %%ymm7, %%ymm8;"
               "vmovdqa 96(%0), %%ymm9;"
               "leaq _randArray(%%rip), %%r14;"
               "vpgatherdq %%ymm6, 3(%%r14,%%xmm8,8), %%ymm9;"
               "xorl %%r14d, %%r14d")

GEN_test_Ronly(VPGATHERQQ_128_1,
               "vpsllq $58, %%xmm7, %%xmm8;"
               "vpsrlq $58, %%xmm8, %%xmm8;"
               "vblendvpd %%xmm6, %%xmm8, %%xmm7, %%xmm8;"
               "leaq _randArray(%%rip), %%r14;"
               "vpgatherqq %%xmm6, 3(%%r14,%%xmm8,8), %%xmm9;"
               "xorl %%r14d, %%r14d")

GEN_test_Ronly(VPGATHERQQ_256_1,
               "vpsllq $58, %%ymm7, %%ymm8;"
               "vpsrlq $58, %%ymm8, %%ymm8;"
               "vblendvpd %%ymm6, %%ymm8, %%ymm7, %%ymm8;"
               "leaq _randArray(%%rip), %%r14;"
               "vpgatherqq %%ymm6, 3(%%r14,%%ymm8,8), %%ymm9;"
               "xorl %%r14d, %%r14d")

GEN_test_Ronly(VPGATHERQQ_128_2,
               "vpsllq $58, %%xmm7, %%xmm8;"
               "vpsrlq $58, %%xmm8, %%xmm8;"
               "vblendvpd %%xmm6, %%xmm8, %%xmm7, %%xmm8;"
               "leaq _randArray(%%rip), %%r14;"
               "vmovq %%r14, %%xmm7;"
               "vpsllq $2, %%xmm8, %%xmm8;"
               "vpbroadcastq %%xmm7, %%xmm7;"
               "vpaddq %%xmm7, %%xmm8, %%xmm8;"
               "vpgatherqq %%xmm6, 1(,%%xmm8,1), %%xmm9;"
               "vpsubq %%xmm7, %%xmm8, %%xmm8;"
               "vmovdqa 0(%0), %%ymm7;"
               "xorl %%r14d, %%r14d")

GEN_test_Ronly(VPGATHERQQ_256_2,
               "vpsllq $58, %%ymm7, %%ymm8;"
               "vpsrlq $58, %%ymm8, %%ymm8;"
               "vblendvpd %%ymm6, %%ymm8, %%ymm7, %%ymm8;"
               "leaq _randArray(%%rip), %%r14;"
               "vmovq %%r14, %%xmm7;"
               "vpsllq $2, %%ymm8, %%ymm8;"
               "vpbroadcastq %%xmm7, %%ymm7;"
               "vpaddq %%ymm7, %%ymm8, %%ymm8;"
               "vpgatherqq %%ymm6, 1(,%%ymm8,1), %%ymm9;"
               "vpsubq %%ymm7, %%ymm8, %%ymm8;"
               "vmovdqa 0(%0), %%ymm7;"
               "xorl %%r14d, %%r14d")

/* Comment duplicated above, for convenient reference:
   Allowed operands in test insns:
     Reg form:  %ymm6,  %ymm7, %ymm8, %ymm9 and %r14.
     Mem form:  (%rax), %ymm7, %ymm8, %ymm9 and %r14.
   Imm8 etc fields are also allowed, where they make sense.
   Both forms may use ymm0 as scratch.  Mem form may also use
   ymm6 as scratch.
*/

#define N_DEFAULT_ITERS 3

// Do the specified test some number of times
#define DO_N(_iters, _testfn) \
   do { int i; for (i = 0; i < (_iters); i++) { test_##_testfn(); } } while (0)

// Do the specified test the default number of times
#define DO_D(_testfn) DO_N(N_DEFAULT_ITERS, _testfn)


int main ( void )
{
   DO_D( VPOR_256 );
   DO_D( VPXOR_256 );
   DO_D( VPSUBB_256 );
   DO_D( VPSUBD_256 );
   DO_D( VPADDD_256 );
   DO_D( VPMOVZXWD_256 );
   DO_D( VPMOVZXBW_256 );
   DO_D( VPBLENDVB_256 );
   DO_D( VPMINSD_256 );
   DO_D( VPMAXSD_256 );
   DO_D( VPSHUFB_256 );
   DO_D( VPUNPCKLBW_256 );
   DO_D( VPUNPCKHBW_256 );
   DO_D( VPABSD_256 );
   DO_D( VPACKUSWB_256 );
   DO_D( VPMOVMSKB_256 );
   DO_D( VPAND_256 );
   DO_D( VPCMPEQB_256 );
   DO_D( VPSHUFLW_0x39_256 );
   DO_D( VPSHUFHW_0x39_256 );
   DO_D( VPMULLW_256 );
   DO_D( VPADDUSW_256 );
   DO_D( VPMULHUW_256 );
   DO_D( VPADDUSB_256 );
   DO_D( VPUNPCKLWD_256 );
   DO_D( VPUNPCKHWD_256 );
   DO_D( VPSLLD_0x05_256 );
   DO_D( VPSRLD_0x05_256 );
   DO_D( VPSRAD_0x05_256 );
   DO_D( VPSUBUSB_256 );
   DO_D( VPSUBSB_256 );
   DO_D( VPSRLDQ_0x05_256 );
   DO_D( VPSLLDQ_0x05_256 );
   DO_D( VPANDN_256 );
   DO_D( VPUNPCKLQDQ_256 );
   DO_D( VPSRLW_0x05_256 );
   DO_D( VPSLLW_0x05_256 );
   DO_D( VPADDW_256 );
   DO_D( VPACKSSDW_256 );
   DO_D( VPUNPCKLDQ_256 );
   DO_D( VPCMPEQD_256 );
   DO_D( VPSHUFD_0x39_256 );
   DO_D( VPADDQ_256 );
   DO_D( VPSUBQ_256 );
   DO_D( VPSUBW_256 );
   DO_D( VPCMPEQQ_256 );
   DO_D( VPCMPGTQ_256 );
   DO_D( VPSRLQ_0x05_256 );
   DO_D( VPMULUDQ_256 );
   DO_D( VPMULDQ_256 );
   DO_D( VPSLLQ_0x05_256 );
   DO_D( VPMAXUD_256 );
   DO_D( VPMINUD_256 );
   DO_D( VPMULLD_256 );
   DO_D( VPMAXUW_256 );
   DO_D( VPMINUW_256 );
   DO_D( VPMAXSW_256 );
   DO_D( VPMINSW_256 );
   DO_D( VPMAXUB_256 );
   DO_D( VPMINUB_256 );
   DO_D( VPMAXSB_256 );
   DO_D( VPMINSB_256 );
   DO_D( VPMOVSXBW_256 );
   DO_D( VPSUBUSW_256 );
   DO_D( VPSUBSW_256 );
   DO_D( VPCMPEQW_256 );
   DO_D( VPADDB_256 );
   DO_D( VPUNPCKHDQ_256 );
   DO_D( VPMOVSXDQ_256 );
   DO_D( VPMOVSXWD_256 );
   DO_D( VPMULHW_256 );
   DO_D( VPUNPCKHQDQ_256 );
   DO_D( VPSRAW_0x05_256 );
   DO_D( VPCMPGTB_256 );
   DO_D( VPCMPGTW_256 );
   DO_D( VPCMPGTD_256 );
   DO_D( VPMOVZXBD_256 );
   DO_D( VPMOVSXBD_256 );
   DO_D( VPALIGNR_256_1of3 );
   DO_D( VPALIGNR_256_2of3 );
   DO_D( VPALIGNR_256_3of3 );
   DO_D( VPBLENDW_256_0x00 );
   DO_D( VPBLENDW_256_0xFE );
   DO_D( VPBLENDW_256_0x30 );
   DO_D( VPBLENDW_256_0x21 );
   DO_D( VPBLENDW_256_0xD7 );
   DO_D( VPBLENDW_256_0xB5 );
   DO_D( VPBLENDW_256_0x85 );
   DO_D( VPBLENDW_256_0x29 );
   DO_D( VPSLLW_256 );
   DO_D( VPSRLW_256 );
   DO_D( VPSRAW_256 );
   DO_D( VPSLLD_256 );
   DO_D( VPSRLD_256 );
   DO_D( VPSRAD_256 );
   DO_D( VPSLLQ_256 );
   DO_D( VPSRLQ_256 );
   DO_D( VPMADDWD_256 );
   DO_D( VMOVNTDQA_256 );
   DO_D( VPACKSSWB_256 );
   DO_D( VPAVGB_256 );
   DO_D( VPAVGW_256 );
   DO_D( VPADDSB_256 );
   DO_D( VPADDSW_256 );
   DO_D( VPHADDW_256 );
   DO_D( VPHADDD_256 );
   DO_D( VPHADDSW_256 );
   DO_D( VPMADDUBSW_256 );
   DO_D( VPHSUBW_256 );
   DO_D( VPHSUBD_256 );
   DO_D( VPHSUBSW_256 );
   DO_D( VPABSB_256 );
   DO_D( VPABSW_256 );
   DO_D( VPMOVSXBQ_256 );
   DO_D( VPMOVSXWQ_256 );
   DO_D( VPACKUSDW_256 );
   DO_D( VPMOVZXBQ_256 );
   DO_D( VPMOVZXWQ_256 );
   DO_D( VPMOVZXDQ_256 );
   DO_D( VMPSADBW_256_0x0 );
   DO_D( VMPSADBW_256_0x39 );
   DO_D( VMPSADBW_256_0x32 );
   DO_D( VMPSADBW_256_0x2b );
   DO_D( VMPSADBW_256_0x24 );
   DO_D( VMPSADBW_256_0x1d );
   DO_D( VMPSADBW_256_0x16 );
   DO_D( VMPSADBW_256_0x0f );
   DO_D( VPSADBW_256 );
   DO_D( VPSIGNB_256 );
   DO_D( VPSIGNW_256 );
   DO_D( VPSIGND_256 );
   DO_D( VPMULHRSW_256 );
   DO_D( VBROADCASTI128 );
   DO_D( VEXTRACTI128_0x0 );
   DO_D( VEXTRACTI128_0x1 );
   DO_D( VINSERTI128_0x0 );
   DO_D( VINSERTI128_0x1 );
   DO_D( VPERM2I128_0x00 );
   DO_D( VPERM2I128_0xFF );
   DO_D( VPERM2I128_0x30 );
   DO_D( VPERM2I128_0x21 );
   DO_D( VPERM2I128_0x12 );
   DO_D( VPERM2I128_0x03 );
   DO_D( VPERM2I128_0x85 );
   DO_D( VPERM2I128_0x5A );
   DO_D( VBROADCASTSS_128 );
   DO_D( VBROADCASTSS_256 );
   DO_D( VBROADCASTSD_256 );
   DO_D( VPERMD );
   DO_D( VPERMQ_0x00 );
   DO_D( VPERMQ_0xFE );
   DO_D( VPERMQ_0x30 );
   DO_D( VPERMQ_0x21 );
   DO_D( VPERMQ_0xD7 );
   DO_D( VPERMQ_0xB5 );
   DO_D( VPERMQ_0x85 );
   DO_D( VPERMQ_0x29 );
   DO_D( VPERMPS );
   DO_D( VPERMPD_0x00 );
   DO_D( VPERMPD_0xFE );
   DO_D( VPERMPD_0x30 );
   DO_D( VPERMPD_0x21 );
   DO_D( VPERMPD_0xD7 );
   DO_D( VPERMPD_0xB5 );
   DO_D( VPERMPD_0x85 );
   DO_D( VPERMPD_0x29 );
   DO_D( VPBLENDD_128_0x00 );
   DO_D( VPBLENDD_128_0x02 );
   DO_D( VPBLENDD_128_0x04 );
   DO_D( VPBLENDD_128_0x06 );
   DO_D( VPBLENDD_128_0x08 );
   DO_D( VPBLENDD_128_0x0A );
   DO_D( VPBLENDD_128_0x0C );
   DO_D( VPBLENDD_128_0x0E );
   DO_D( VPBLENDD_256_0x00 );
   DO_D( VPBLENDD_256_0xFE );
   DO_D( VPBLENDD_256_0x30 );
   DO_D( VPBLENDD_256_0x21 );
   DO_D( VPBLENDD_256_0xD7 );
   DO_D( VPBLENDD_256_0xB5 );
   DO_D( VPBLENDD_256_0x85 );
   DO_D( VPBLENDD_256_0x29 );
   DO_D( VPSLLVD_128 );
   DO_D( VPSLLVD_256 );
   DO_D( VPSLLVQ_128 );
   DO_D( VPSLLVQ_256 );
   DO_D( VPSRLVD_128 );
   DO_D( VPSRLVD_256 );
   DO_D( VPSRLVQ_128 );
   DO_D( VPSRLVQ_256 );
   DO_D( VPSRAVD_128 );
   DO_D( VPSRAVD_256 );
   DO_D( VPBROADCASTB_128 );
   DO_D( VPBROADCASTB_256 );
   DO_D( VPBROADCASTW_128 );
   DO_D( VPBROADCASTW_256 );
   DO_D( VPBROADCASTD_128 );
   DO_D( VPBROADCASTD_256 );
   DO_D( VPBROADCASTQ_128 );
   DO_D( VPBROADCASTQ_256 );
   DO_D( VPMASKMOVD_128_LoadForm );
   DO_D( VPMASKMOVD_256_LoadForm );
   DO_D( VPMASKMOVQ_128_LoadForm );
   DO_D( VPMASKMOVQ_256_LoadForm );
   DO_D( VPMASKMOVD_128_StoreForm );
   DO_D( VPMASKMOVD_256_StoreForm );
   DO_D( VPMASKMOVQ_128_StoreForm );
   DO_D( VPMASKMOVQ_256_StoreForm );
#if defined(VGO_darwin)
   { int i; for (i = 0; i < sizeof(randArray); i++) randArray[i] = randUChar(); }
#else
   { int i; for (i = 0; i < sizeof(_randArray); i++) _randArray[i] = randUChar(); }
#endif
   DO_D( VGATHERDPS_128 );
   DO_D( VGATHERDPS_256 );
   DO_D( VGATHERQPS_128_1 );
   DO_D( VGATHERQPS_256_1 );
   DO_D( VGATHERQPS_128_2 );
   DO_D( VGATHERQPS_256_2 );
   DO_D( VGATHERDPD_128 );
   DO_D( VGATHERDPD_256 );
   DO_D( VGATHERQPD_128_1 );
   DO_D( VGATHERQPD_256_1 );
   DO_D( VGATHERQPD_128_2 );
   DO_D( VGATHERQPD_256_2 );
   DO_D( VPGATHERDD_128 );
   DO_D( VPGATHERDD_256 );
   DO_D( VPGATHERQD_128_1 );
   DO_D( VPGATHERQD_256_1 );
   DO_D( VPGATHERQD_128_2 );
   DO_D( VPGATHERQD_256_2 );
   DO_D( VPGATHERDQ_128 );
   DO_D( VPGATHERDQ_256 );
   DO_D( VPGATHERQQ_128_1 );
   DO_D( VPGATHERQQ_256_1 );
   DO_D( VPGATHERQQ_128_2 );
   DO_D( VPGATHERQQ_256_2 );
   return 0;
}
