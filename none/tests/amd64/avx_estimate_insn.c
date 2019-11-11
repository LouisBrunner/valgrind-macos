#include"avx_tests.h"

GEN_test_RandM(VRCPSS_128,
               "vrcpss %%xmm6,  %%xmm8, %%xmm7",
               "vrcpss (%%rsi), %%xmm8, %%xmm7")

GEN_test_RandM(VRCPPS_128,
               "vrcpps %%xmm6,  %%xmm8",
               "vrcpps (%%rsi), %%xmm8")

GEN_test_RandM(VRCPPS_256,
               "vrcpps %%ymm6,  %%ymm8",
               "vrcpps (%%rsi), %%ymm8")

GEN_test_RandM(VSQRTSS_128,
               "vsqrtss %%xmm6,  %%xmm8, %%xmm7",
               "vsqrtss (%%rsi), %%xmm8, %%xmm7")

GEN_test_RandM(VSQRTPS_128,
               "vsqrtps %%xmm6,  %%xmm8",
               "vsqrtps (%%rsi), %%xmm8")

GEN_test_RandM(VSQRTPS_256,
               "vsqrtps %%ymm6,  %%ymm8",
               "vsqrtps (%%rsi), %%ymm8")

GEN_test_RandM(VSQRTPD_128,
               "vsqrtpd %%xmm6,  %%xmm8",
               "vsqrtpd (%%rsi), %%xmm8")

GEN_test_RandM(VSQRTPD_256,
               "vsqrtpd %%ymm6,  %%ymm8",
               "vsqrtpd (%%rsi), %%ymm8")

GEN_test_RandM(VRSQRTSS_128,
               "vrsqrtss %%xmm6,  %%xmm8, %%xmm7",
               "vrsqrtss (%%rsi), %%xmm8, %%xmm7")

GEN_test_RandM(VRSQRTPS_128,
               "vrsqrtps %%xmm6,  %%xmm8",
               "vrsqrtps (%%rsi), %%xmm8")

GEN_test_RandM(VRSQRTPS_256,
               "vrsqrtps %%ymm6,  %%ymm8",
               "vrsqrtps (%%rsi), %%ymm8")

int main ( void )
{
   DO_D( VRCPSS_128 );
   DO_D( VRCPPS_128 );
   DO_D( VRCPPS_256 );
   DO_D( VSQRTSS_128 );
   DO_D( VSQRTPS_128 );
   DO_D( VSQRTPS_256 );
   DO_D( VSQRTPD_128 );
   DO_D( VSQRTPD_256 );
   DO_D( VRSQRTSS_128 );
   DO_D( VRSQRTPS_128 );
   DO_D( VRSQRTPS_256 );

   return 0;
}

