#include"avx_tests.h"

GEN_test_RandM(VMOVQ_XMM_to_XMM_LOW_HIGH,
               "vmovq %%xmm9, %%xmm7",
               "vmovq %%xmm8, (%%rsi)")

// xmm0 is scratch
GEN_test_RandM(VMOVQ_XMM_to_XMM_LOW_LOW_HIGH,
               "vmovq %%xmm7, %%xmm0; vmovq %%xmm0, %%xmm8",
               "vmovq (%%rsi), %%xmm0; vmovq %%xmm0, %%xmm9")

int main ( void )
{
   DO_D( VMOVQ_XMM_to_XMM_LOW_HIGH );
   DO_D( VMOVQ_XMM_to_XMM_LOW_LOW_HIGH );

   return 0;
}
