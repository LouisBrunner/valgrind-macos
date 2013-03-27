#include <stdio.h>
#include <string.h>

#define N 64
struct float_test {
   float x[N], y[N], z[N], expected[N], res[N];
} ft __attribute__((aligned (32)));

struct double_test {
   double x[N], y[N], z[N], expected[N], res[N];
} dt __attribute__((aligned (32)));

float plus_zero, plus_infty, minus_infty, nan_value;

static int testf( float x, float y )
{
   unsigned int a, b;
   memcpy( &a, &x, sizeof (a) );
   memcpy( &b, &y, sizeof (b) );
   if ((a & 0x7fc00000U) == 0x7fc00000U)
      return (b & 0x7fc00000U) != 0x7fc00000U;
   return memcmp( &a, &b, sizeof (a) ) != 0;
}

static int test_fmaf( void )
{
   int res = 0, i, j;
   float w;
   for (i = 0; i < N; i++) {
      int thisres = 0;
      __asm __volatile__ ("vfmadd132ss %2, %3, %0" : "=x" (w) : "0" (ft.x[i]), "x" (ft.y[i]), "x" (ft.z[i]));
      thisres |= testf( w, ft.expected[i] );
      __asm __volatile__ ("vfmadd132ss %2, %3, %0" : "=x" (w) : "0" (ft.x[i]), "m" (ft.y[i]), "x" (ft.z[i]));
      thisres |= testf( w, ft.expected[i] );
      __asm __volatile__ ("vfmadd213ss %3, %2, %0" : "=x" (w) : "0" (ft.x[i]), "x" (ft.y[i]), "x" (ft.z[i]));
      thisres |= testf( w, ft.expected[i] );
      __asm __volatile__ ("vfmadd213ss %3, %2, %0" : "=x" (w) : "0" (ft.x[i]), "x" (ft.y[i]), "m" (ft.z[i]));
      thisres |= testf( w, ft.expected[i] );
      __asm __volatile__ ("vfmadd231ss %2, %1, %0" : "=x" (w) : "x" (ft.x[i]), "x" (ft.y[i]), "0" (ft.z[i]));
      thisres |= testf( w, ft.expected[i] );
      __asm __volatile__ ("vfmadd231ss %2, %1, %0" : "=x" (w) : "x" (ft.x[i]), "m" (ft.y[i]), "0" (ft.z[i]));
      thisres |= testf( w, ft.expected[i] );
      if (thisres)
         printf( "Failure 1 %d %a %a\n", i, w, ft.expected[i] );
      res |= thisres;
      thisres = 0;
      __asm __volatile__ ("vfnmsub132ss %2, %3, %0" : "=x" (w) : "0" (ft.x[i]), "x" (ft.y[i]), "x" (ft.z[i]));
      thisres |= testf( -w, ft.expected[i] );
      __asm __volatile__ ("vfnmsub132ss %2, %3, %0" : "=x" (w) : "0" (ft.x[i]), "m" (ft.y[i]), "x" (ft.z[i]));
      thisres |= testf( -w, ft.expected[i] );
      __asm __volatile__ ("vfnmsub213ss %3, %2, %0" : "=x" (w) : "0" (ft.x[i]), "x" (ft.y[i]), "x" (ft.z[i]));
      thisres |= testf( -w, ft.expected[i] );
      __asm __volatile__ ("vfnmsub213ss %3, %2, %0" : "=x" (w) : "0" (ft.x[i]), "x" (ft.y[i]), "m" (ft.z[i]));
      thisres |= testf( -w, ft.expected[i] );
      __asm __volatile__ ("vfnmsub231ss %2, %1, %0" : "=x" (w) : "x" (ft.x[i]), "x" (ft.y[i]), "0" (ft.z[i]));
      thisres |= testf( -w, ft.expected[i] );
      __asm __volatile__ ("vfnmsub231ss %2, %1, %0" : "=x" (w) : "x" (ft.x[i]), "m" (ft.y[i]), "0" (ft.z[i]));
      thisres |= testf( -w, ft.expected[i] );
      if (thisres)
         printf( "Failure 2 %d %a %a\n", i, w, ft.expected[i] );
      res |= thisres;
   }
   for (i = 0; i < N; i++)
      ft.z[i] = -ft.z[i];
   for (i = 0; i < N; i++) {
      int thisres = 0;
      __asm __volatile__ ("vfmsub132ss %2, %3, %0" : "=x" (w) : "0" (ft.x[i]), "x" (ft.y[i]), "x" (ft.z[i]));
      thisres |= testf( w, ft.expected[i] );
      __asm __volatile__ ("vfmsub132ss %2, %3, %0" : "=x" (w) : "0" (ft.x[i]), "m" (ft.y[i]), "x" (ft.z[i]));
      thisres |= testf( w, ft.expected[i] );
      __asm __volatile__ ("vfmsub213ss %3, %2, %0" : "=x" (w) : "0" (ft.x[i]), "x" (ft.y[i]), "x" (ft.z[i]));
      thisres |= testf( w, ft.expected[i] );
      __asm __volatile__ ("vfmsub213ss %3, %2, %0" : "=x" (w) : "0" (ft.x[i]), "x" (ft.y[i]), "m" (ft.z[i]));
      thisres |= testf( w, ft.expected[i] );
      __asm __volatile__ ("vfmsub231ss %2, %1, %0" : "=x" (w) : "x" (ft.x[i]), "x" (ft.y[i]), "0" (ft.z[i]));
      thisres |= testf( w, ft.expected[i] );
      __asm __volatile__ ("vfmsub231ss %2, %1, %0" : "=x" (w) : "x" (ft.x[i]), "m" (ft.y[i]), "0" (ft.z[i]));
      thisres |= testf( w, ft.expected[i] );
      if (thisres)
         printf( "Failure 3 %d %a %a\n", i, w, ft.expected[i] );
      res |= thisres;
      thisres = 0;
      __asm __volatile__ ("vfnmadd132ss %2, %3, %0" : "=x" (w) : "0" (ft.x[i]), "x" (ft.y[i]), "x" (ft.z[i]));
      thisres |= testf( -w, ft.expected[i] );
      __asm __volatile__ ("vfnmadd132ss %2, %3, %0" : "=x" (w) : "0" (ft.x[i]), "m" (ft.y[i]), "x" (ft.z[i]));
      thisres |= testf( -w, ft.expected[i] );
      __asm __volatile__ ("vfnmadd213ss %3, %2, %0" : "=x" (w) : "0" (ft.x[i]), "x" (ft.y[i]), "x" (ft.z[i]));
      thisres |= testf( -w, ft.expected[i] );
      __asm __volatile__ ("vfnmadd213ss %3, %2, %0" : "=x" (w) : "0" (ft.x[i]), "x" (ft.y[i]), "m" (ft.z[i]));
      thisres |= testf( -w, ft.expected[i] );
      __asm __volatile__ ("vfnmadd231ss %2, %1, %0" : "=x" (w) : "x" (ft.x[i]), "x" (ft.y[i]), "0" (ft.z[i]));
      thisres |= testf( -w, ft.expected[i] );
      __asm __volatile__ ("vfnmadd231ss %2, %1, %0" : "=x" (w) : "x" (ft.x[i]), "m" (ft.y[i]), "0" (ft.z[i]));
      thisres |= testf( -w, ft.expected[i] );
      if (thisres)
         printf( "Failure 4 %d %a %a\n", i, w, ft.expected[i] );
      res |= thisres;
   }
   for (i = 0; i < N; i++)
      ft.z[i] = -ft.z[i];
   for (i = 0; i < N; i += 4) {
      int thisres = 0;
      __asm __volatile__ ("vmovaps (%1), %%xmm9; vmovaps (%2), %%xmm7; vmovaps (%3), %%xmm8;"
                          "vfmadd132ps %%xmm7, %%xmm8, %%xmm9;"
                          "vmovaps %%xmm9, (%0)" : : "r" (&ft.res[i]), "r" (&ft.x[i]),
                                                     "r" (&ft.y[i]), "r" (&ft.z[i]) : "xmm7", "xmm8", "xmm9");
      for (j = 0; j < 4; j++)
         thisres |= testf( ft.res[i+j], ft.expected[i+j] );
      __asm __volatile__ ("vmovaps (%1), %%xmm9; vmovaps (%3), %%xmm8;"
                          "vfmadd132ps (%2), %%xmm8, %%xmm9;"
                          "vmovaps %%xmm9, (%0)" : : "r" (&ft.res[i]), "r" (&ft.x[i]),
                                                     "r" (&ft.y[i]), "r" (&ft.z[i]) : "xmm7", "xmm8", "xmm9");
      for (j = 0; j < 4; j++)
         thisres |= testf( ft.res[i+j], ft.expected[i+j] );
      __asm __volatile__ ("vmovaps (%1), %%xmm9; vmovaps (%3), %%xmm7; vmovaps (%2), %%xmm8;"
                          "vfmadd213ps %%xmm7, %%xmm8, %%xmm9;"
                          "vmovaps %%xmm9, (%0)" : : "r" (&ft.res[i]), "r" (&ft.x[i]),
                                                     "r" (&ft.y[i]), "r" (&ft.z[i]) : "xmm7", "xmm8", "xmm9");
      for (j = 0; j < 4; j++)
         thisres |= testf( ft.res[i+j], ft.expected[i+j] );
      __asm __volatile__ ("vmovaps (%1), %%xmm9; vmovaps (%2), %%xmm8;"
                          "vfmadd213ps (%3), %%xmm8, %%xmm9;"
                          "vmovaps %%xmm9, (%0)" : : "r" (&ft.res[i]), "r" (&ft.x[i]),
                                                     "r" (&ft.y[i]), "r" (&ft.z[i]) : "xmm7", "xmm8", "xmm9");
      for (j = 0; j < 4; j++)
         thisres |= testf( ft.res[i+j], ft.expected[i+j] );
      __asm __volatile__ ("vmovaps (%3), %%xmm9; vmovaps (%2), %%xmm7; vmovaps (%1), %%xmm8;"
                          "vfmadd231ps %%xmm7, %%xmm8, %%xmm9;"
                          "vmovaps %%xmm9, (%0)" : : "r" (&ft.res[i]), "r" (&ft.x[i]),
                                                     "r" (&ft.y[i]), "r" (&ft.z[i]) : "xmm7", "xmm8", "xmm9");
      for (j = 0; j < 4; j++)
         thisres |= testf( ft.res[i+j], ft.expected[i+j] );
      __asm __volatile__ ("vmovaps (%3), %%xmm9; vmovaps (%1), %%xmm8;"
                          "vfmadd231ps (%2), %%xmm8, %%xmm9;"
                          "vmovaps %%xmm9, (%0)" : : "r" (&ft.res[i]), "r" (&ft.x[i]),
                                                     "r" (&ft.y[i]), "r" (&ft.z[i]) : "xmm7", "xmm8", "xmm9");
      for (j = 0; j < 4; j++)
         thisres |= testf( ft.res[i+j], ft.expected[i+j] );
      if (thisres) {
         printf( "Failure 5 %d", i );
         for (j = 0; j < 4; j++)
            printf( " %a %a", ft.res[i+j], ft.expected[i+j] );
         printf( "\n" );
      }
      res |= thisres;
      thisres = 0;
      __asm __volatile__ ("vmovaps (%1), %%xmm9; vmovaps (%2), %%xmm7; vmovaps (%3), %%xmm8;"
                          "vfnmsub132ps %%xmm7, %%xmm8, %%xmm9;"
                          "vmovaps %%xmm9, (%0)" : : "r" (&ft.res[i]), "r" (&ft.x[i]),
                                                     "r" (&ft.y[i]), "r" (&ft.z[i]) : "xmm7", "xmm8", "xmm9");
      for (j = 0; j < 4; j++)
         thisres |= testf( -ft.res[i+j], ft.expected[i+j] );
      __asm __volatile__ ("vmovaps (%1), %%xmm9; vmovaps (%3), %%xmm8;"
                          "vfnmsub132ps (%2), %%xmm8, %%xmm9;"
                          "vmovaps %%xmm9, (%0)" : : "r" (&ft.res[i]), "r" (&ft.x[i]),
                                                     "r" (&ft.y[i]), "r" (&ft.z[i]) : "xmm7", "xmm8", "xmm9");
      for (j = 0; j < 4; j++)
         thisres |= testf( -ft.res[i+j], ft.expected[i+j] );
      __asm __volatile__ ("vmovaps (%1), %%xmm9; vmovaps (%3), %%xmm7; vmovaps (%2), %%xmm8;"
                          "vfnmsub213ps %%xmm7, %%xmm8, %%xmm9;"
                          "vmovaps %%xmm9, (%0)" : : "r" (&ft.res[i]), "r" (&ft.x[i]),
                                                     "r" (&ft.y[i]), "r" (&ft.z[i]) : "xmm7", "xmm8", "xmm9");
      for (j = 0; j < 4; j++)
         thisres |= testf( -ft.res[i+j], ft.expected[i+j] );
      __asm __volatile__ ("vmovaps (%1), %%xmm9; vmovaps (%2), %%xmm8;"
                          "vfnmsub213ps (%3), %%xmm8, %%xmm9;"
                          "vmovaps %%xmm9, (%0)" : : "r" (&ft.res[i]), "r" (&ft.x[i]),
                                                     "r" (&ft.y[i]), "r" (&ft.z[i]) : "xmm7", "xmm8", "xmm9");
      for (j = 0; j < 4; j++)
         thisres |= testf( -ft.res[i+j], ft.expected[i+j] );
      __asm __volatile__ ("vmovaps (%3), %%xmm9; vmovaps (%2), %%xmm7; vmovaps (%1), %%xmm8;"
                          "vfnmsub231ps %%xmm7, %%xmm8, %%xmm9;"
                          "vmovaps %%xmm9, (%0)" : : "r" (&ft.res[i]), "r" (&ft.x[i]),
                                                     "r" (&ft.y[i]), "r" (&ft.z[i]) : "xmm7", "xmm8", "xmm9");
      for (j = 0; j < 4; j++)
         thisres |= testf( -ft.res[i+j], ft.expected[i+j] );
      __asm __volatile__ ("vmovaps (%3), %%xmm9; vmovaps (%1), %%xmm8;"
                          "vfnmsub231ps (%2), %%xmm8, %%xmm9;"
                          "vmovaps %%xmm9, (%0)" : : "r" (&ft.res[i]), "r" (&ft.x[i]),
                                                     "r" (&ft.y[i]), "r" (&ft.z[i]) : "xmm7", "xmm8", "xmm9");
      for (j = 0; j < 4; j++)
         thisres |= testf( -ft.res[i+j], ft.expected[i+j] );
      if (thisres) {
         printf( "Failure 6 %d", i );
         for (j = 0; j < 4; j++)
            printf( " %a %a", ft.res[i+j], ft.expected[i+j] );
         printf( "\n" );
      }
      res |= thisres;
   }
   for (i = 0; i < N; i++)
      ft.z[i] = -ft.z[i];
   for (i = 0; i < N; i += 4) {
      int thisres = 0;
      __asm __volatile__ ("vmovaps (%1), %%xmm9; vmovaps (%2), %%xmm7; vmovaps (%3), %%xmm8;"
                          "vfmsub132ps %%xmm7, %%xmm8, %%xmm9;"
                          "vmovaps %%xmm9, (%0)" : : "r" (&ft.res[i]), "r" (&ft.x[i]),
                                                     "r" (&ft.y[i]), "r" (&ft.z[i]) : "xmm7", "xmm8", "xmm9");
      for (j = 0; j < 4; j++)
         thisres |= testf( ft.res[i+j], ft.expected[i+j] );
      __asm __volatile__ ("vmovaps (%1), %%xmm9; vmovaps (%3), %%xmm8;"
                          "vfmsub132ps (%2), %%xmm8, %%xmm9;"
                          "vmovaps %%xmm9, (%0)" : : "r" (&ft.res[i]), "r" (&ft.x[i]),
                                                     "r" (&ft.y[i]), "r" (&ft.z[i]) : "xmm7", "xmm8", "xmm9");
      for (j = 0; j < 4; j++)
         thisres |= testf( ft.res[i+j], ft.expected[i+j] );
      __asm __volatile__ ("vmovaps (%1), %%xmm9; vmovaps (%3), %%xmm7; vmovaps (%2), %%xmm8;"
                          "vfmsub213ps %%xmm7, %%xmm8, %%xmm9;"
                          "vmovaps %%xmm9, (%0)" : : "r" (&ft.res[i]), "r" (&ft.x[i]),
                                                     "r" (&ft.y[i]), "r" (&ft.z[i]) : "xmm7", "xmm8", "xmm9");
      for (j = 0; j < 4; j++)
         thisres |= testf( ft.res[i+j], ft.expected[i+j] );
      __asm __volatile__ ("vmovaps (%1), %%xmm9; vmovaps (%2), %%xmm8;"
                          "vfmsub213ps (%3), %%xmm8, %%xmm9;"
                          "vmovaps %%xmm9, (%0)" : : "r" (&ft.res[i]), "r" (&ft.x[i]),
                                                     "r" (&ft.y[i]), "r" (&ft.z[i]) : "xmm7", "xmm8", "xmm9");
      for (j = 0; j < 4; j++)
         thisres |= testf( ft.res[i+j], ft.expected[i+j] );
      __asm __volatile__ ("vmovaps (%3), %%xmm9; vmovaps (%2), %%xmm7; vmovaps (%1), %%xmm8;"
                          "vfmsub231ps %%xmm7, %%xmm8, %%xmm9;"
                          "vmovaps %%xmm9, (%0)" : : "r" (&ft.res[i]), "r" (&ft.x[i]),
                                                     "r" (&ft.y[i]), "r" (&ft.z[i]) : "xmm7", "xmm8", "xmm9");
      for (j = 0; j < 4; j++)
         thisres |= testf( ft.res[i+j], ft.expected[i+j] );
      __asm __volatile__ ("vmovaps (%3), %%xmm9; vmovaps (%1), %%xmm8;"
                          "vfmsub231ps (%2), %%xmm8, %%xmm9;"
                          "vmovaps %%xmm9, (%0)" : : "r" (&ft.res[i]), "r" (&ft.x[i]),
                                                     "r" (&ft.y[i]), "r" (&ft.z[i]) : "xmm7", "xmm8", "xmm9");
      for (j = 0; j < 4; j++)
         thisres |= testf( ft.res[i+j], ft.expected[i+j] );
      if (thisres) {
         printf( "Failure 7 %d", i );
         for (j = 0; j < 4; j++)
            printf( " %a %a", ft.res[i+j], ft.expected[i+j] );
         printf( "\n" );
      }
      res |= thisres;
      thisres = 0;
      __asm __volatile__ ("vmovaps (%1), %%xmm9; vmovaps (%2), %%xmm7; vmovaps (%3), %%xmm8;"
                          "vfnmadd132ps %%xmm7, %%xmm8, %%xmm9;"
                          "vmovaps %%xmm9, (%0)" : : "r" (&ft.res[i]), "r" (&ft.x[i]),
                                                     "r" (&ft.y[i]), "r" (&ft.z[i]) : "xmm7", "xmm8", "xmm9");
      for (j = 0; j < 4; j++)
         thisres |= testf( -ft.res[i+j], ft.expected[i+j] );
      __asm __volatile__ ("vmovaps (%1), %%xmm9; vmovaps (%3), %%xmm8;"
                          "vfnmadd132ps (%2), %%xmm8, %%xmm9;"
                          "vmovaps %%xmm9, (%0)" : : "r" (&ft.res[i]), "r" (&ft.x[i]),
                                                     "r" (&ft.y[i]), "r" (&ft.z[i]) : "xmm7", "xmm8", "xmm9");
      for (j = 0; j < 4; j++)
         thisres |= testf( -ft.res[i+j], ft.expected[i+j] );
      __asm __volatile__ ("vmovaps (%1), %%xmm9; vmovaps (%3), %%xmm7; vmovaps (%2), %%xmm8;"
                          "vfnmadd213ps %%xmm7, %%xmm8, %%xmm9;"
                          "vmovaps %%xmm9, (%0)" : : "r" (&ft.res[i]), "r" (&ft.x[i]),
                                                     "r" (&ft.y[i]), "r" (&ft.z[i]) : "xmm7", "xmm8", "xmm9");
      for (j = 0; j < 4; j++)
         thisres |= testf( -ft.res[i+j], ft.expected[i+j] );
      __asm __volatile__ ("vmovaps (%1), %%xmm9; vmovaps (%2), %%xmm8;"
                          "vfnmadd213ps (%3), %%xmm8, %%xmm9;"
                          "vmovaps %%xmm9, (%0)" : : "r" (&ft.res[i]), "r" (&ft.x[i]),
                                                     "r" (&ft.y[i]), "r" (&ft.z[i]) : "xmm7", "xmm8", "xmm9");
      for (j = 0; j < 4; j++)
         thisres |= testf( -ft.res[i+j], ft.expected[i+j] );
      __asm __volatile__ ("vmovaps (%3), %%xmm9; vmovaps (%2), %%xmm7; vmovaps (%1), %%xmm8;"
                          "vfnmadd231ps %%xmm7, %%xmm8, %%xmm9;"
                          "vmovaps %%xmm9, (%0)" : : "r" (&ft.res[i]), "r" (&ft.x[i]),
                                                     "r" (&ft.y[i]), "r" (&ft.z[i]) : "xmm7", "xmm8", "xmm9");
      for (j = 0; j < 4; j++)
         thisres |= testf( -ft.res[i+j], ft.expected[i+j] );
      __asm __volatile__ ("vmovaps (%3), %%xmm9; vmovaps (%1), %%xmm8;"
                          "vfnmadd231ps (%2), %%xmm8, %%xmm9;"
                          "vmovaps %%xmm9, (%0)" : : "r" (&ft.res[i]), "r" (&ft.x[i]),
                                                     "r" (&ft.y[i]), "r" (&ft.z[i]) : "xmm7", "xmm8", "xmm9");
      for (j = 0; j < 4; j++)
         thisres |= testf( -ft.res[i+j], ft.expected[i+j] );
      if (thisres) {
         printf( "Failure 8 %d", i );
         for (j = 0; j < 4; j++)
            printf( " %a %a", ft.res[i+j], ft.expected[i+j] );
         printf( "\n" );
      }
      res |= thisres;
   }
   for (i = 1; i < N; i += 2)
      ft.z[i] = -ft.z[i];
   for (i = 0; i < N; i += 4) {
      int thisres = 0;
      __asm __volatile__ ("vmovaps (%1), %%xmm9; vmovaps (%2), %%xmm7; vmovaps (%3), %%xmm8;"
                          "vfmaddsub132ps %%xmm7, %%xmm8, %%xmm9;"
                          "vmovaps %%xmm9, (%0)" : : "r" (&ft.res[i]), "r" (&ft.x[i]),
                                                     "r" (&ft.y[i]), "r" (&ft.z[i]) : "xmm7", "xmm8", "xmm9");
      for (j = 0; j < 4; j++)
         thisres |= testf( ft.res[i+j], ft.expected[i+j] );
      __asm __volatile__ ("vmovaps (%1), %%xmm9; vmovaps (%3), %%xmm8;"
                          "vfmaddsub132ps (%2), %%xmm8, %%xmm9;"
                          "vmovaps %%xmm9, (%0)" : : "r" (&ft.res[i]), "r" (&ft.x[i]),
                                                     "r" (&ft.y[i]), "r" (&ft.z[i]) : "xmm7", "xmm8", "xmm9");
      for (j = 0; j < 4; j++)
         thisres |= testf( ft.res[i+j], ft.expected[i+j] );
      __asm __volatile__ ("vmovaps (%1), %%xmm9; vmovaps (%3), %%xmm7; vmovaps (%2), %%xmm8;"
                          "vfmaddsub213ps %%xmm7, %%xmm8, %%xmm9;"
                          "vmovaps %%xmm9, (%0)" : : "r" (&ft.res[i]), "r" (&ft.x[i]),
                                                     "r" (&ft.y[i]), "r" (&ft.z[i]) : "xmm7", "xmm8", "xmm9");
      for (j = 0; j < 4; j++)
         thisres |= testf( ft.res[i+j], ft.expected[i+j] );
      __asm __volatile__ ("vmovaps (%1), %%xmm9; vmovaps (%2), %%xmm8;"
                          "vfmaddsub213ps (%3), %%xmm8, %%xmm9;"
                          "vmovaps %%xmm9, (%0)" : : "r" (&ft.res[i]), "r" (&ft.x[i]),
                                                     "r" (&ft.y[i]), "r" (&ft.z[i]) : "xmm7", "xmm8", "xmm9");
      for (j = 0; j < 4; j++)
         thisres |= testf( ft.res[i+j], ft.expected[i+j] );
      __asm __volatile__ ("vmovaps (%3), %%xmm9; vmovaps (%2), %%xmm7; vmovaps (%1), %%xmm8;"
                          "vfmaddsub231ps %%xmm7, %%xmm8, %%xmm9;"
                          "vmovaps %%xmm9, (%0)" : : "r" (&ft.res[i]), "r" (&ft.x[i]),
                                                     "r" (&ft.y[i]), "r" (&ft.z[i]) : "xmm7", "xmm8", "xmm9");
      for (j = 0; j < 4; j++)
         thisres |= testf( ft.res[i+j], ft.expected[i+j] );
      __asm __volatile__ ("vmovaps (%3), %%xmm9; vmovaps (%1), %%xmm8;"
                          "vfmaddsub231ps (%2), %%xmm8, %%xmm9;"
                          "vmovaps %%xmm9, (%0)" : : "r" (&ft.res[i]), "r" (&ft.x[i]),
                                                     "r" (&ft.y[i]), "r" (&ft.z[i]) : "xmm7", "xmm8", "xmm9");
      for (j = 0; j < 4; j++)
         thisres |= testf( ft.res[i+j], ft.expected[i+j] );
      if (thisres) {
         printf( "Failure 9 %d", i );
         for (j = 0; j < 4; j++)
            printf( " %a %a", ft.res[i+j], ft.expected[i+j] );
         printf( "\n" );
      }
      res |= thisres;
   }
   for (i = 0; i < N; i++)
      ft.z[i] = -ft.z[i];
   for (i = 0; i < N; i += 4) {
      int thisres = 0;
      __asm __volatile__ ("vmovaps (%1), %%xmm9; vmovaps (%2), %%xmm7; vmovaps (%3), %%xmm8;"
                          "vfmsubadd132ps %%xmm7, %%xmm8, %%xmm9;"
                          "vmovaps %%xmm9, (%0)" : : "r" (&ft.res[i]), "r" (&ft.x[i]),
                                                     "r" (&ft.y[i]), "r" (&ft.z[i]) : "xmm7", "xmm8", "xmm9");
      for (j = 0; j < 4; j++)
         thisres |= testf( ft.res[i+j], ft.expected[i+j] );
      __asm __volatile__ ("vmovaps (%1), %%xmm9; vmovaps (%3), %%xmm8;"
                          "vfmsubadd132ps (%2), %%xmm8, %%xmm9;"
                          "vmovaps %%xmm9, (%0)" : : "r" (&ft.res[i]), "r" (&ft.x[i]),
                                                     "r" (&ft.y[i]), "r" (&ft.z[i]) : "xmm7", "xmm8", "xmm9");
      for (j = 0; j < 4; j++)
         thisres |= testf( ft.res[i+j], ft.expected[i+j] );
      __asm __volatile__ ("vmovaps (%1), %%xmm9; vmovaps (%3), %%xmm7; vmovaps (%2), %%xmm8;"
                          "vfmsubadd213ps %%xmm7, %%xmm8, %%xmm9;"
                          "vmovaps %%xmm9, (%0)" : : "r" (&ft.res[i]), "r" (&ft.x[i]),
                                                     "r" (&ft.y[i]), "r" (&ft.z[i]) : "xmm7", "xmm8", "xmm9");
      for (j = 0; j < 4; j++)
         thisres |= testf( ft.res[i+j], ft.expected[i+j] );
      __asm __volatile__ ("vmovaps (%1), %%xmm9; vmovaps (%2), %%xmm8;"
                          "vfmsubadd213ps (%3), %%xmm8, %%xmm9;"
                          "vmovaps %%xmm9, (%0)" : : "r" (&ft.res[i]), "r" (&ft.x[i]),
                                                     "r" (&ft.y[i]), "r" (&ft.z[i]) : "xmm7", "xmm8", "xmm9");
      for (j = 0; j < 4; j++)
         thisres |= testf( ft.res[i+j], ft.expected[i+j] );
      __asm __volatile__ ("vmovaps (%3), %%xmm9; vmovaps (%2), %%xmm7; vmovaps (%1), %%xmm8;"
                          "vfmsubadd231ps %%xmm7, %%xmm8, %%xmm9;"
                          "vmovaps %%xmm9, (%0)" : : "r" (&ft.res[i]), "r" (&ft.x[i]),
                                                     "r" (&ft.y[i]), "r" (&ft.z[i]) : "xmm7", "xmm8", "xmm9");
      for (j = 0; j < 4; j++)
         thisres |= testf( ft.res[i+j], ft.expected[i+j] );
      __asm __volatile__ ("vmovaps (%3), %%xmm9; vmovaps (%1), %%xmm8;"
                          "vfmsubadd231ps (%2), %%xmm8, %%xmm9;"
                          "vmovaps %%xmm9, (%0)" : : "r" (&ft.res[i]), "r" (&ft.x[i]),
                                                     "r" (&ft.y[i]), "r" (&ft.z[i]) : "xmm7", "xmm8", "xmm9");
      for (j = 0; j < 4; j++)
         thisres |= testf( ft.res[i+j], ft.expected[i+j] );
      if (thisres) {
         printf( "Failure 10 %d", i );
         for (j = 0; j < 4; j++)
            printf( " %a %a", ft.res[i+j], ft.expected[i+j] );
         printf( "\n" );
      }
      res |= thisres;
   }
   for (i = 1; i < N; i += 2)
      ft.z[i] = -ft.z[i];
   for (i = 0; i < N; i += 8) {
      int thisres = 0;
      __asm __volatile__ ("vmovaps (%1), %%ymm9; vmovaps (%2), %%ymm7; vmovaps (%3), %%ymm8;"
                          "vfmadd132ps %%ymm7, %%ymm8, %%ymm9;"
                          "vmovaps %%ymm9, (%0)" : : "r" (&ft.res[i]), "r" (&ft.x[i]),
                                                     "r" (&ft.y[i]), "r" (&ft.z[i]) : "xmm7", "xmm8", "xmm9");
      for (j = 0; j < 8; j++)
         thisres |= testf( ft.res[i+j], ft.expected[i+j] );
      __asm __volatile__ ("vmovaps (%1), %%ymm9; vmovaps (%3), %%ymm8;"
                          "vfmadd132ps (%2), %%ymm8, %%ymm9;"
                          "vmovaps %%ymm9, (%0)" : : "r" (&ft.res[i]), "r" (&ft.x[i]),
                                                     "r" (&ft.y[i]), "r" (&ft.z[i]) : "xmm7", "xmm8", "xmm9");
      for (j = 0; j < 8; j++)
         thisres |= testf( ft.res[i+j], ft.expected[i+j] );
      __asm __volatile__ ("vmovaps (%1), %%ymm9; vmovaps (%3), %%ymm7; vmovaps (%2), %%ymm8;"
                          "vfmadd213ps %%ymm7, %%ymm8, %%ymm9;"
                          "vmovaps %%ymm9, (%0)" : : "r" (&ft.res[i]), "r" (&ft.x[i]),
                                                     "r" (&ft.y[i]), "r" (&ft.z[i]) : "xmm7", "xmm8", "xmm9");
      for (j = 0; j < 8; j++)
         thisres |= testf( ft.res[i+j], ft.expected[i+j] );
      __asm __volatile__ ("vmovaps (%1), %%ymm9; vmovaps (%2), %%ymm8;"
                          "vfmadd213ps (%3), %%ymm8, %%ymm9;"
                          "vmovaps %%ymm9, (%0)" : : "r" (&ft.res[i]), "r" (&ft.x[i]),
                                                     "r" (&ft.y[i]), "r" (&ft.z[i]) : "xmm7", "xmm8", "xmm9");
      for (j = 0; j < 8; j++)
         thisres |= testf( ft.res[i+j], ft.expected[i+j] );
      __asm __volatile__ ("vmovaps (%3), %%ymm9; vmovaps (%2), %%ymm7; vmovaps (%1), %%ymm8;"
                          "vfmadd231ps %%ymm7, %%ymm8, %%ymm9;"
                          "vmovaps %%ymm9, (%0)" : : "r" (&ft.res[i]), "r" (&ft.x[i]),
                                                     "r" (&ft.y[i]), "r" (&ft.z[i]) : "xmm7", "xmm8", "xmm9");
      for (j = 0; j < 8; j++)
         thisres |= testf( ft.res[i+j], ft.expected[i+j] );
      __asm __volatile__ ("vmovaps (%3), %%ymm9; vmovaps (%1), %%ymm8;"
                          "vfmadd231ps (%2), %%ymm8, %%ymm9;"
                          "vmovaps %%ymm9, (%0)" : : "r" (&ft.res[i]), "r" (&ft.x[i]),
                                                     "r" (&ft.y[i]), "r" (&ft.z[i]) : "xmm7", "xmm8", "xmm9");
      for (j = 0; j < 8; j++)
         thisres |= testf( ft.res[i+j], ft.expected[i+j] );
      if (thisres) {
         printf( "Failure 11 %d", i );
         for (j = 0; j < 8; j++)
            printf( " %a %a", ft.res[i+j], ft.expected[i+j] );
         printf( "\n" );
      }
      res |= thisres;
      thisres = 0;
      __asm __volatile__ ("vmovaps (%1), %%ymm9; vmovaps (%2), %%ymm7; vmovaps (%3), %%ymm8;"
                          "vfnmsub132ps %%ymm7, %%ymm8, %%ymm9;"
                          "vmovaps %%ymm9, (%0)" : : "r" (&ft.res[i]), "r" (&ft.x[i]),
                                                     "r" (&ft.y[i]), "r" (&ft.z[i]) : "xmm7", "xmm8", "xmm9");
      for (j = 0; j < 8; j++)
         thisres |= testf( -ft.res[i+j], ft.expected[i+j] );
      __asm __volatile__ ("vmovaps (%1), %%ymm9; vmovaps (%3), %%ymm8;"
                          "vfnmsub132ps (%2), %%ymm8, %%ymm9;"
                          "vmovaps %%ymm9, (%0)" : : "r" (&ft.res[i]), "r" (&ft.x[i]),
                                                     "r" (&ft.y[i]), "r" (&ft.z[i]) : "xmm7", "xmm8", "xmm9");
      for (j = 0; j < 8; j++)
         thisres |= testf( -ft.res[i+j], ft.expected[i+j] );
      __asm __volatile__ ("vmovaps (%1), %%ymm9; vmovaps (%3), %%ymm7; vmovaps (%2), %%ymm8;"
                          "vfnmsub213ps %%ymm7, %%ymm8, %%ymm9;"
                          "vmovaps %%ymm9, (%0)" : : "r" (&ft.res[i]), "r" (&ft.x[i]),
                                                     "r" (&ft.y[i]), "r" (&ft.z[i]) : "xmm7", "xmm8", "xmm9");
      for (j = 0; j < 8; j++)
         thisres |= testf( -ft.res[i+j], ft.expected[i+j] );
      __asm __volatile__ ("vmovaps (%1), %%ymm9; vmovaps (%2), %%ymm8;"
                          "vfnmsub213ps (%3), %%ymm8, %%ymm9;"
                          "vmovaps %%ymm9, (%0)" : : "r" (&ft.res[i]), "r" (&ft.x[i]),
                                                     "r" (&ft.y[i]), "r" (&ft.z[i]) : "xmm7", "xmm8", "xmm9");
      for (j = 0; j < 8; j++)
         thisres |= testf( -ft.res[i+j], ft.expected[i+j] );
      __asm __volatile__ ("vmovaps (%3), %%ymm9; vmovaps (%2), %%ymm7; vmovaps (%1), %%ymm8;"
                          "vfnmsub231ps %%ymm7, %%ymm8, %%ymm9;"
                          "vmovaps %%ymm9, (%0)" : : "r" (&ft.res[i]), "r" (&ft.x[i]),
                                                     "r" (&ft.y[i]), "r" (&ft.z[i]) : "xmm7", "xmm8", "xmm9");
      for (j = 0; j < 8; j++)
         thisres |= testf( -ft.res[i+j], ft.expected[i+j] );
      __asm __volatile__ ("vmovaps (%3), %%ymm9; vmovaps (%1), %%ymm8;"
                          "vfnmsub231ps (%2), %%ymm8, %%ymm9;"
                          "vmovaps %%ymm9, (%0)" : : "r" (&ft.res[i]), "r" (&ft.x[i]),
                                                     "r" (&ft.y[i]), "r" (&ft.z[i]) : "xmm7", "xmm8", "xmm9");
      for (j = 0; j < 8; j++)
         thisres |= testf( -ft.res[i+j], ft.expected[i+j] );
      if (thisres) {
         printf( "Failure 12 %d", i );
         for (j = 0; j < 8; j++)
            printf( " %a %a", ft.res[i+j], ft.expected[i+j] );
         printf( "\n" );
      }
      res |= thisres;
   }
   for (i = 0; i < N; i++)
      ft.z[i] = -ft.z[i];
   for (i = 0; i < N; i += 8) {
      int thisres = 0;
      __asm __volatile__ ("vmovaps (%1), %%ymm9; vmovaps (%2), %%ymm7; vmovaps (%3), %%ymm8;"
                          "vfmsub132ps %%ymm7, %%ymm8, %%ymm9;"
                          "vmovaps %%ymm9, (%0)" : : "r" (&ft.res[i]), "r" (&ft.x[i]),
                                                     "r" (&ft.y[i]), "r" (&ft.z[i]) : "xmm7", "xmm8", "xmm9");
      for (j = 0; j < 8; j++)
         thisres |= testf( ft.res[i+j], ft.expected[i+j] );
      __asm __volatile__ ("vmovaps (%1), %%ymm9; vmovaps (%3), %%ymm8;"
                          "vfmsub132ps (%2), %%ymm8, %%ymm9;"
                          "vmovaps %%ymm9, (%0)" : : "r" (&ft.res[i]), "r" (&ft.x[i]),
                                                     "r" (&ft.y[i]), "r" (&ft.z[i]) : "xmm7", "xmm8", "xmm9");
      for (j = 0; j < 8; j++)
         thisres |= testf( ft.res[i+j], ft.expected[i+j] );
      __asm __volatile__ ("vmovaps (%1), %%ymm9; vmovaps (%3), %%ymm7; vmovaps (%2), %%ymm8;"
                          "vfmsub213ps %%ymm7, %%ymm8, %%ymm9;"
                          "vmovaps %%ymm9, (%0)" : : "r" (&ft.res[i]), "r" (&ft.x[i]),
                                                     "r" (&ft.y[i]), "r" (&ft.z[i]) : "xmm7", "xmm8", "xmm9");
      for (j = 0; j < 8; j++)
         thisres |= testf( ft.res[i+j], ft.expected[i+j] );
      __asm __volatile__ ("vmovaps (%1), %%ymm9; vmovaps (%2), %%ymm8;"
                          "vfmsub213ps (%3), %%ymm8, %%ymm9;"
                          "vmovaps %%ymm9, (%0)" : : "r" (&ft.res[i]), "r" (&ft.x[i]),
                                                     "r" (&ft.y[i]), "r" (&ft.z[i]) : "xmm7", "xmm8", "xmm9");
      for (j = 0; j < 8; j++)
         thisres |= testf( ft.res[i+j], ft.expected[i+j] );
      __asm __volatile__ ("vmovaps (%3), %%ymm9; vmovaps (%2), %%ymm7; vmovaps (%1), %%ymm8;"
                          "vfmsub231ps %%ymm7, %%ymm8, %%ymm9;"
                          "vmovaps %%ymm9, (%0)" : : "r" (&ft.res[i]), "r" (&ft.x[i]),
                                                     "r" (&ft.y[i]), "r" (&ft.z[i]) : "xmm7", "xmm8", "xmm9");
      for (j = 0; j < 8; j++)
         thisres |= testf( ft.res[i+j], ft.expected[i+j] );
      __asm __volatile__ ("vmovaps (%3), %%ymm9; vmovaps (%1), %%ymm8;"
                          "vfmsub231ps (%2), %%ymm8, %%ymm9;"
                          "vmovaps %%ymm9, (%0)" : : "r" (&ft.res[i]), "r" (&ft.x[i]),
                                                     "r" (&ft.y[i]), "r" (&ft.z[i]) : "xmm7", "xmm8", "xmm9");
      for (j = 0; j < 8; j++)
         thisres |= testf( ft.res[i+j], ft.expected[i+j] );
      if (thisres) {
         printf( "Failure 13 %d", i );
         for (j = 0; j < 8; j++)
            printf( " %a %a", ft.res[i+j], ft.expected[i+j] );
         printf( "\n" );
      }
      res |= thisres;
      thisres = 0;
      __asm __volatile__ ("vmovaps (%1), %%ymm9; vmovaps (%2), %%ymm7; vmovaps (%3), %%ymm8;"
                          "vfnmadd132ps %%ymm7, %%ymm8, %%ymm9;"
                          "vmovaps %%ymm9, (%0)" : : "r" (&ft.res[i]), "r" (&ft.x[i]),
                                                     "r" (&ft.y[i]), "r" (&ft.z[i]) : "xmm7", "xmm8", "xmm9");
      for (j = 0; j < 8; j++)
         thisres |= testf( -ft.res[i+j], ft.expected[i+j] );
      __asm __volatile__ ("vmovaps (%1), %%ymm9; vmovaps (%3), %%ymm8;"
                          "vfnmadd132ps (%2), %%ymm8, %%ymm9;"
                          "vmovaps %%ymm9, (%0)" : : "r" (&ft.res[i]), "r" (&ft.x[i]),
                                                     "r" (&ft.y[i]), "r" (&ft.z[i]) : "xmm7", "xmm8", "xmm9");
      for (j = 0; j < 8; j++)
         thisres |= testf( -ft.res[i+j], ft.expected[i+j] );
      __asm __volatile__ ("vmovaps (%1), %%ymm9; vmovaps (%3), %%ymm7; vmovaps (%2), %%ymm8;"
                          "vfnmadd213ps %%ymm7, %%ymm8, %%ymm9;"
                          "vmovaps %%ymm9, (%0)" : : "r" (&ft.res[i]), "r" (&ft.x[i]),
                                                     "r" (&ft.y[i]), "r" (&ft.z[i]) : "xmm7", "xmm8", "xmm9");
      for (j = 0; j < 8; j++)
         thisres |= testf( -ft.res[i+j], ft.expected[i+j] );
      __asm __volatile__ ("vmovaps (%1), %%ymm9; vmovaps (%2), %%ymm8;"
                          "vfnmadd213ps (%3), %%ymm8, %%ymm9;"
                          "vmovaps %%ymm9, (%0)" : : "r" (&ft.res[i]), "r" (&ft.x[i]),
                                                     "r" (&ft.y[i]), "r" (&ft.z[i]) : "xmm7", "xmm8", "xmm9");
      for (j = 0; j < 8; j++)
         thisres |= testf( -ft.res[i+j], ft.expected[i+j] );
      __asm __volatile__ ("vmovaps (%3), %%ymm9; vmovaps (%2), %%ymm7; vmovaps (%1), %%ymm8;"
                          "vfnmadd231ps %%ymm7, %%ymm8, %%ymm9;"
                          "vmovaps %%ymm9, (%0)" : : "r" (&ft.res[i]), "r" (&ft.x[i]),
                                                     "r" (&ft.y[i]), "r" (&ft.z[i]) : "xmm7", "xmm8", "xmm9");
      for (j = 0; j < 8; j++)
         thisres |= testf( -ft.res[i+j], ft.expected[i+j] );
      __asm __volatile__ ("vmovaps (%3), %%ymm9; vmovaps (%1), %%ymm8;"
                          "vfnmadd231ps (%2), %%ymm8, %%ymm9;"
                          "vmovaps %%ymm9, (%0)" : : "r" (&ft.res[i]), "r" (&ft.x[i]),
                                                     "r" (&ft.y[i]), "r" (&ft.z[i]) : "xmm7", "xmm8", "xmm9");
      for (j = 0; j < 8; j++)
         thisres |= testf( -ft.res[i+j], ft.expected[i+j] );
      if (thisres) {
         printf( "Failure 14 %d", i );
         for (j = 0; j < 8; j++)
            printf( " %a %a", ft.res[i+j], ft.expected[i+j] );
         printf( "\n" );
      }
      res |= thisres;
   }
   for (i = 1; i < N; i += 2)
      ft.z[i] = -ft.z[i];
   for (i = 0; i < N; i += 8) {
      int thisres = 0;
      __asm __volatile__ ("vmovaps (%1), %%ymm9; vmovaps (%2), %%ymm7; vmovaps (%3), %%ymm8;"
                          "vfmaddsub132ps %%ymm7, %%ymm8, %%ymm9;"
                          "vmovaps %%ymm9, (%0)" : : "r" (&ft.res[i]), "r" (&ft.x[i]),
                                                     "r" (&ft.y[i]), "r" (&ft.z[i]) : "xmm7", "xmm8", "xmm9");
      for (j = 0; j < 8; j++)
         thisres |= testf( ft.res[i+j], ft.expected[i+j] );
      __asm __volatile__ ("vmovaps (%1), %%ymm9; vmovaps (%3), %%ymm8;"
                          "vfmaddsub132ps (%2), %%ymm8, %%ymm9;"
                          "vmovaps %%ymm9, (%0)" : : "r" (&ft.res[i]), "r" (&ft.x[i]),
                                                     "r" (&ft.y[i]), "r" (&ft.z[i]) : "xmm7", "xmm8", "xmm9");
      for (j = 0; j < 8; j++)
         thisres |= testf( ft.res[i+j], ft.expected[i+j] );
      __asm __volatile__ ("vmovaps (%1), %%ymm9; vmovaps (%3), %%ymm7; vmovaps (%2), %%ymm8;"
                          "vfmaddsub213ps %%ymm7, %%ymm8, %%ymm9;"
                          "vmovaps %%ymm9, (%0)" : : "r" (&ft.res[i]), "r" (&ft.x[i]),
                                                     "r" (&ft.y[i]), "r" (&ft.z[i]) : "xmm7", "xmm8", "xmm9");
      for (j = 0; j < 8; j++)
         thisres |= testf( ft.res[i+j], ft.expected[i+j] );
      __asm __volatile__ ("vmovaps (%1), %%ymm9; vmovaps (%2), %%ymm8;"
                          "vfmaddsub213ps (%3), %%ymm8, %%ymm9;"
                          "vmovaps %%ymm9, (%0)" : : "r" (&ft.res[i]), "r" (&ft.x[i]),
                                                     "r" (&ft.y[i]), "r" (&ft.z[i]) : "xmm7", "xmm8", "xmm9");
      for (j = 0; j < 8; j++)
         thisres |= testf( ft.res[i+j], ft.expected[i+j] );
      __asm __volatile__ ("vmovaps (%3), %%ymm9; vmovaps (%2), %%ymm7; vmovaps (%1), %%ymm8;"
                          "vfmaddsub231ps %%ymm7, %%ymm8, %%ymm9;"
                          "vmovaps %%ymm9, (%0)" : : "r" (&ft.res[i]), "r" (&ft.x[i]),
                                                     "r" (&ft.y[i]), "r" (&ft.z[i]) : "xmm7", "xmm8", "xmm9");
      for (j = 0; j < 8; j++)
         thisres |= testf( ft.res[i+j], ft.expected[i+j] );
      __asm __volatile__ ("vmovaps (%3), %%ymm9; vmovaps (%1), %%ymm8;"
                          "vfmaddsub231ps (%2), %%ymm8, %%ymm9;"
                          "vmovaps %%ymm9, (%0)" : : "r" (&ft.res[i]), "r" (&ft.x[i]),
                                                     "r" (&ft.y[i]), "r" (&ft.z[i]) : "xmm7", "xmm8", "xmm9");
      for (j = 0; j < 8; j++)
         thisres |= testf( ft.res[i+j], ft.expected[i+j] );
      if (thisres) {
         printf( "Failure 15 %d", i );
         for (j = 0; j < 8; j++)
            printf( " %a %a", ft.res[i+j], ft.expected[i+j] );
         printf( "\n" );
      }
      res |= thisres;
   }
   for (i = 0; i < N; i++)
      ft.z[i] = -ft.z[i];
   for (i = 0; i < N; i += 8) {
      int thisres = 0;
      __asm __volatile__ ("vmovaps (%1), %%ymm9; vmovaps (%2), %%ymm7; vmovaps (%3), %%ymm8;"
                          "vfmsubadd132ps %%ymm7, %%ymm8, %%ymm9;"
                          "vmovaps %%ymm9, (%0)" : : "r" (&ft.res[i]), "r" (&ft.x[i]),
                                                     "r" (&ft.y[i]), "r" (&ft.z[i]) : "xmm7", "xmm8", "xmm9");
      for (j = 0; j < 8; j++)
         thisres |= testf( ft.res[i+j], ft.expected[i+j] );
      __asm __volatile__ ("vmovaps (%1), %%ymm9; vmovaps (%3), %%ymm8;"
                          "vfmsubadd132ps (%2), %%ymm8, %%ymm9;"
                          "vmovaps %%ymm9, (%0)" : : "r" (&ft.res[i]), "r" (&ft.x[i]),
                                                     "r" (&ft.y[i]), "r" (&ft.z[i]) : "xmm7", "xmm8", "xmm9");
      for (j = 0; j < 8; j++)
         thisres |= testf( ft.res[i+j], ft.expected[i+j] );
      __asm __volatile__ ("vmovaps (%1), %%ymm9; vmovaps (%3), %%ymm7; vmovaps (%2), %%ymm8;"
                          "vfmsubadd213ps %%ymm7, %%ymm8, %%ymm9;"
                          "vmovaps %%ymm9, (%0)" : : "r" (&ft.res[i]), "r" (&ft.x[i]),
                                                     "r" (&ft.y[i]), "r" (&ft.z[i]) : "xmm7", "xmm8", "xmm9");
      for (j = 0; j < 8; j++)
         thisres |= testf( ft.res[i+j], ft.expected[i+j] );
      __asm __volatile__ ("vmovaps (%1), %%ymm9; vmovaps (%2), %%ymm8;"
                          "vfmsubadd213ps (%3), %%ymm8, %%ymm9;"
                          "vmovaps %%ymm9, (%0)" : : "r" (&ft.res[i]), "r" (&ft.x[i]),
                                                     "r" (&ft.y[i]), "r" (&ft.z[i]) : "xmm7", "xmm8", "xmm9");
      for (j = 0; j < 8; j++)
         thisres |= testf( ft.res[i+j], ft.expected[i+j] );
      __asm __volatile__ ("vmovaps (%3), %%ymm9; vmovaps (%2), %%ymm7; vmovaps (%1), %%ymm8;"
                          "vfmsubadd231ps %%ymm7, %%ymm8, %%ymm9;"
                          "vmovaps %%ymm9, (%0)" : : "r" (&ft.res[i]), "r" (&ft.x[i]),
                                                     "r" (&ft.y[i]), "r" (&ft.z[i]) : "xmm7", "xmm8", "xmm9");
      for (j = 0; j < 8; j++)
         thisres |= testf( ft.res[i+j], ft.expected[i+j] );
      __asm __volatile__ ("vmovaps (%3), %%ymm9; vmovaps (%1), %%ymm8;"
                          "vfmsubadd231ps (%2), %%ymm8, %%ymm9;"
                          "vmovaps %%ymm9, (%0)" : : "r" (&ft.res[i]), "r" (&ft.x[i]),
                                                     "r" (&ft.y[i]), "r" (&ft.z[i]) : "xmm7", "xmm8", "xmm9");
      for (j = 0; j < 8; j++)
         thisres |= testf( ft.res[i+j], ft.expected[i+j] );
      if (thisres) {
         printf( "Failure 16 %d", i );
         for (j = 0; j < 8; j++)
            printf( " %a %a", ft.res[i+j], ft.expected[i+j] );
         printf( "\n" );
      }
      res |= thisres;
   }
   for (i = 1; i < N; i += 2)
      ft.z[i] = -ft.z[i];
   return res;
}

static int test( double x, double y )
{
   unsigned long long a, b;
   memcpy( &a, &x, sizeof (a) );
   memcpy( &b, &y, sizeof (b) );
   if ((a & 0x7ff8000000000000ULL) == 0x7ff8000000000000ULL)
      return (b & 0x7ff8000000000000ULL) != 0x7ff8000000000000ULL;
   return memcmp( &a, &b, sizeof (a) ) != 0;
}

static int test_fma( void )
{
   int res = 0, i, j;
   double w;
   for (i = 0; i < N; i++) {
      int thisres = 0;
      __asm __volatile__ ("vfmadd132sd %2, %3, %0" : "=x" (w) : "0" (dt.x[i]), "x" (dt.y[i]), "x" (dt.z[i]));
      thisres |= test( w, dt.expected[i] );
      __asm __volatile__ ("vfmadd132sd %2, %3, %0" : "=x" (w) : "0" (dt.x[i]), "m" (dt.y[i]), "x" (dt.z[i]));
      thisres |= test( w, dt.expected[i] );
      __asm __volatile__ ("vfmadd213sd %3, %2, %0" : "=x" (w) : "0" (dt.x[i]), "x" (dt.y[i]), "x" (dt.z[i]));
      thisres |= test( w, dt.expected[i] );
      __asm __volatile__ ("vfmadd213sd %3, %2, %0" : "=x" (w) : "0" (dt.x[i]), "x" (dt.y[i]), "m" (dt.z[i]));
      thisres |= test( w, dt.expected[i] );
      __asm __volatile__ ("vfmadd231sd %2, %1, %0" : "=x" (w) : "x" (dt.x[i]), "x" (dt.y[i]), "0" (dt.z[i]));
      thisres |= test( w, dt.expected[i] );
      __asm __volatile__ ("vfmadd231sd %2, %1, %0" : "=x" (w) : "x" (dt.x[i]), "m" (dt.y[i]), "0" (dt.z[i]));
      thisres |= test( w, dt.expected[i] );
      if (thisres)
         printf( "Failure 1 %d %a %a\n", i, w, dt.expected[i] );
      res |= thisres;
      thisres = 0;
      __asm __volatile__ ("vfnmsub132sd %2, %3, %0" : "=x" (w) : "0" (dt.x[i]), "x" (dt.y[i]), "x" (dt.z[i]));
      thisres |= test( -w, dt.expected[i] );
      __asm __volatile__ ("vfnmsub132sd %2, %3, %0" : "=x" (w) : "0" (dt.x[i]), "m" (dt.y[i]), "x" (dt.z[i]));
      thisres |= test( -w, dt.expected[i] );
      __asm __volatile__ ("vfnmsub213sd %3, %2, %0" : "=x" (w) : "0" (dt.x[i]), "x" (dt.y[i]), "x" (dt.z[i]));
      thisres |= test( -w, dt.expected[i] );
      __asm __volatile__ ("vfnmsub213sd %3, %2, %0" : "=x" (w) : "0" (dt.x[i]), "x" (dt.y[i]), "m" (dt.z[i]));
      thisres |= test( -w, dt.expected[i] );
      __asm __volatile__ ("vfnmsub231sd %2, %1, %0" : "=x" (w) : "x" (dt.x[i]), "x" (dt.y[i]), "0" (dt.z[i]));
      thisres |= test( -w, dt.expected[i] );
      __asm __volatile__ ("vfnmsub231sd %2, %1, %0" : "=x" (w) : "x" (dt.x[i]), "m" (dt.y[i]), "0" (dt.z[i]));
      thisres |= test( -w, dt.expected[i] );
      if (thisres)
         printf( "Failure 2 %d %a %a\n", i, w, dt.expected[i] );
      res |= thisres;
   }
   for (i = 0; i < N; i++)
      dt.z[i] = -dt.z[i];
   for (i = 0; i < N; i++) {
      int thisres = 0;
      __asm __volatile__ ("vfmsub132sd %2, %3, %0" : "=x" (w) : "0" (dt.x[i]), "x" (dt.y[i]), "x" (dt.z[i]));
      thisres |= test( w, dt.expected[i] );
      __asm __volatile__ ("vfmsub132sd %2, %3, %0" : "=x" (w) : "0" (dt.x[i]), "m" (dt.y[i]), "x" (dt.z[i]));
      thisres |= test( w, dt.expected[i] );
      __asm __volatile__ ("vfmsub213sd %3, %2, %0" : "=x" (w) : "0" (dt.x[i]), "x" (dt.y[i]), "x" (dt.z[i]));
      thisres |= test( w, dt.expected[i] );
      __asm __volatile__ ("vfmsub213sd %3, %2, %0" : "=x" (w) : "0" (dt.x[i]), "x" (dt.y[i]), "m" (dt.z[i]));
      thisres |= test( w, dt.expected[i] );
      __asm __volatile__ ("vfmsub231sd %2, %1, %0" : "=x" (w) : "x" (dt.x[i]), "x" (dt.y[i]), "0" (dt.z[i]));
      thisres |= test( w, dt.expected[i] );
      __asm __volatile__ ("vfmsub231sd %2, %1, %0" : "=x" (w) : "x" (dt.x[i]), "m" (dt.y[i]), "0" (dt.z[i]));
      thisres |= test( w, dt.expected[i] );
      if (thisres)
         printf( "Failure 3 %d %a %a\n", i, w, dt.expected[i] );
      res |= thisres;
      thisres = 0;
      __asm __volatile__ ("vfnmadd132sd %2, %3, %0" : "=x" (w) : "0" (dt.x[i]), "x" (dt.y[i]), "x" (dt.z[i]));
      thisres |= test( -w, dt.expected[i] );
      __asm __volatile__ ("vfnmadd132sd %2, %3, %0" : "=x" (w) : "0" (dt.x[i]), "m" (dt.y[i]), "x" (dt.z[i]));
      thisres |= test( -w, dt.expected[i] );
      __asm __volatile__ ("vfnmadd213sd %3, %2, %0" : "=x" (w) : "0" (dt.x[i]), "x" (dt.y[i]), "x" (dt.z[i]));
      thisres |= test( -w, dt.expected[i] );
      __asm __volatile__ ("vfnmadd213sd %3, %2, %0" : "=x" (w) : "0" (dt.x[i]), "x" (dt.y[i]), "m" (dt.z[i]));
      thisres |= test( -w, dt.expected[i] );
      __asm __volatile__ ("vfnmadd231sd %2, %1, %0" : "=x" (w) : "x" (dt.x[i]), "x" (dt.y[i]), "0" (dt.z[i]));
      thisres |= test( -w, dt.expected[i] );
      __asm __volatile__ ("vfnmadd231sd %2, %1, %0" : "=x" (w) : "x" (dt.x[i]), "m" (dt.y[i]), "0" (dt.z[i]));
      thisres |= test( -w, dt.expected[i] );
      if (thisres)
         printf( "Failure 4 %d %a %a\n", i, w, dt.expected[i] );
      res |= thisres;
   }
   for (i = 0; i < N; i++)
      dt.z[i] = -dt.z[i];
   for (i = 0; i < N; i += 2) {
      int thisres = 0;
      __asm __volatile__ ("vmovapd (%1), %%xmm9; vmovapd (%2), %%xmm7; vmovapd (%3), %%xmm8;"
                          "vfmadd132pd %%xmm7, %%xmm8, %%xmm9;"
                          "vmovapd %%xmm9, (%0)" : : "r" (&dt.res[i]), "r" (&dt.x[i]),
                                                     "r" (&dt.y[i]), "r" (&dt.z[i]) : "xmm7", "xmm8", "xmm9");
      for (j = 0; j < 2; j++)
         thisres |= test( dt.res[i+j], dt.expected[i+j] );
      __asm __volatile__ ("vmovapd (%1), %%xmm9; vmovapd (%3), %%xmm8;"
                          "vfmadd132pd (%2), %%xmm8, %%xmm9;"
                          "vmovapd %%xmm9, (%0)" : : "r" (&dt.res[i]), "r" (&dt.x[i]),
                                                     "r" (&dt.y[i]), "r" (&dt.z[i]) : "xmm7", "xmm8", "xmm9");
      for (j = 0; j < 2; j++)
         thisres |= test( dt.res[i+j], dt.expected[i+j] );
      __asm __volatile__ ("vmovapd (%1), %%xmm9; vmovapd (%3), %%xmm7; vmovapd (%2), %%xmm8;"
                          "vfmadd213pd %%xmm7, %%xmm8, %%xmm9;"
                          "vmovapd %%xmm9, (%0)" : : "r" (&dt.res[i]), "r" (&dt.x[i]),
                                                     "r" (&dt.y[i]), "r" (&dt.z[i]) : "xmm7", "xmm8", "xmm9");
      for (j = 0; j < 2; j++)
         thisres |= test( dt.res[i+j], dt.expected[i+j] );
      __asm __volatile__ ("vmovapd (%1), %%xmm9; vmovapd (%2), %%xmm8;"
                          "vfmadd213pd (%3), %%xmm8, %%xmm9;"
                          "vmovapd %%xmm9, (%0)" : : "r" (&dt.res[i]), "r" (&dt.x[i]),
                                                     "r" (&dt.y[i]), "r" (&dt.z[i]) : "xmm7", "xmm8", "xmm9");
      for (j = 0; j < 2; j++)
         thisres |= test( dt.res[i+j], dt.expected[i+j] );
      __asm __volatile__ ("vmovapd (%3), %%xmm9; vmovapd (%2), %%xmm7; vmovapd (%1), %%xmm8;"
                          "vfmadd231pd %%xmm7, %%xmm8, %%xmm9;"
                          "vmovapd %%xmm9, (%0)" : : "r" (&dt.res[i]), "r" (&dt.x[i]),
                                                     "r" (&dt.y[i]), "r" (&dt.z[i]) : "xmm7", "xmm8", "xmm9");
      for (j = 0; j < 2; j++)
         thisres |= test( dt.res[i+j], dt.expected[i+j] );
      __asm __volatile__ ("vmovapd (%3), %%xmm9; vmovapd (%1), %%xmm8;"
                          "vfmadd231pd (%2), %%xmm8, %%xmm9;"
                          "vmovapd %%xmm9, (%0)" : : "r" (&dt.res[i]), "r" (&dt.x[i]),
                                                     "r" (&dt.y[i]), "r" (&dt.z[i]) : "xmm7", "xmm8", "xmm9");
      for (j = 0; j < 2; j++)
         thisres |= test( dt.res[i+j], dt.expected[i+j] );
      if (thisres) {
         printf( "Failure 5 %d", i );
         for (j = 0; j < 2; j++)
            printf( " %a %a", dt.res[i+j], dt.expected[i+j] );
         printf( "\n" );
      }
      res |= thisres;
      thisres = 0;
      __asm __volatile__ ("vmovapd (%1), %%xmm9; vmovapd (%2), %%xmm7; vmovapd (%3), %%xmm8;"
                          "vfnmsub132pd %%xmm7, %%xmm8, %%xmm9;"
                          "vmovapd %%xmm9, (%0)" : : "r" (&dt.res[i]), "r" (&dt.x[i]),
                                                     "r" (&dt.y[i]), "r" (&dt.z[i]) : "xmm7", "xmm8", "xmm9");
      for (j = 0; j < 2; j++)
         thisres |= test( -dt.res[i+j], dt.expected[i+j] );
      __asm __volatile__ ("vmovapd (%1), %%xmm9; vmovapd (%3), %%xmm8;"
                          "vfnmsub132pd (%2), %%xmm8, %%xmm9;"
                          "vmovapd %%xmm9, (%0)" : : "r" (&dt.res[i]), "r" (&dt.x[i]),
                                                     "r" (&dt.y[i]), "r" (&dt.z[i]) : "xmm7", "xmm8", "xmm9");
      for (j = 0; j < 2; j++)
         thisres |= test( -dt.res[i+j], dt.expected[i+j] );
      __asm __volatile__ ("vmovapd (%1), %%xmm9; vmovapd (%3), %%xmm7; vmovapd (%2), %%xmm8;"
                          "vfnmsub213pd %%xmm7, %%xmm8, %%xmm9;"
                          "vmovapd %%xmm9, (%0)" : : "r" (&dt.res[i]), "r" (&dt.x[i]),
                                                     "r" (&dt.y[i]), "r" (&dt.z[i]) : "xmm7", "xmm8", "xmm9");
      for (j = 0; j < 2; j++)
         thisres |= test( -dt.res[i+j], dt.expected[i+j] );
      __asm __volatile__ ("vmovapd (%1), %%xmm9; vmovapd (%2), %%xmm8;"
                          "vfnmsub213pd (%3), %%xmm8, %%xmm9;"
                          "vmovapd %%xmm9, (%0)" : : "r" (&dt.res[i]), "r" (&dt.x[i]),
                                                     "r" (&dt.y[i]), "r" (&dt.z[i]) : "xmm7", "xmm8", "xmm9");
      for (j = 0; j < 2; j++)
         thisres |= test( -dt.res[i+j], dt.expected[i+j] );
      __asm __volatile__ ("vmovapd (%3), %%xmm9; vmovapd (%2), %%xmm7; vmovapd (%1), %%xmm8;"
                          "vfnmsub231pd %%xmm7, %%xmm8, %%xmm9;"
                          "vmovapd %%xmm9, (%0)" : : "r" (&dt.res[i]), "r" (&dt.x[i]),
                                                     "r" (&dt.y[i]), "r" (&dt.z[i]) : "xmm7", "xmm8", "xmm9");
      for (j = 0; j < 2; j++)
         thisres |= test( -dt.res[i+j], dt.expected[i+j] );
      __asm __volatile__ ("vmovapd (%3), %%xmm9; vmovapd (%1), %%xmm8;"
                          "vfnmsub231pd (%2), %%xmm8, %%xmm9;"
                          "vmovapd %%xmm9, (%0)" : : "r" (&dt.res[i]), "r" (&dt.x[i]),
                                                     "r" (&dt.y[i]), "r" (&dt.z[i]) : "xmm7", "xmm8", "xmm9");
      for (j = 0; j < 2; j++)
         thisres |= test( -dt.res[i+j], dt.expected[i+j] );
      if (thisres) {
         printf( "Failure 6 %d", i );
         for (j = 0; j < 2; j++)
            printf( " %a %a", dt.res[i+j], dt.expected[i+j] );
         printf( "\n" );
      }
      res |= thisres;
   }
   for (i = 0; i < N; i++)
      dt.z[i] = -dt.z[i];
   for (i = 0; i < N; i += 2) {
      int thisres = 0;
      __asm __volatile__ ("vmovapd (%1), %%xmm9; vmovapd (%2), %%xmm7; vmovapd (%3), %%xmm8;"
                          "vfmsub132pd %%xmm7, %%xmm8, %%xmm9;"
                          "vmovapd %%xmm9, (%0)" : : "r" (&dt.res[i]), "r" (&dt.x[i]),
                                                     "r" (&dt.y[i]), "r" (&dt.z[i]) : "xmm7", "xmm8", "xmm9");
      for (j = 0; j < 2; j++)
         thisres |= test( dt.res[i+j], dt.expected[i+j] );
      __asm __volatile__ ("vmovapd (%1), %%xmm9; vmovapd (%3), %%xmm8;"
                          "vfmsub132pd (%2), %%xmm8, %%xmm9;"
                          "vmovapd %%xmm9, (%0)" : : "r" (&dt.res[i]), "r" (&dt.x[i]),
                                                     "r" (&dt.y[i]), "r" (&dt.z[i]) : "xmm7", "xmm8", "xmm9");
      for (j = 0; j < 2; j++)
         thisres |= test( dt.res[i+j], dt.expected[i+j] );
      __asm __volatile__ ("vmovapd (%1), %%xmm9; vmovapd (%3), %%xmm7; vmovapd (%2), %%xmm8;"
                          "vfmsub213pd %%xmm7, %%xmm8, %%xmm9;"
                          "vmovapd %%xmm9, (%0)" : : "r" (&dt.res[i]), "r" (&dt.x[i]),
                                                     "r" (&dt.y[i]), "r" (&dt.z[i]) : "xmm7", "xmm8", "xmm9");
      for (j = 0; j < 2; j++)
         thisres |= test( dt.res[i+j], dt.expected[i+j] );
      __asm __volatile__ ("vmovapd (%1), %%xmm9; vmovapd (%2), %%xmm8;"
                          "vfmsub213pd (%3), %%xmm8, %%xmm9;"
                          "vmovapd %%xmm9, (%0)" : : "r" (&dt.res[i]), "r" (&dt.x[i]),
                                                     "r" (&dt.y[i]), "r" (&dt.z[i]) : "xmm7", "xmm8", "xmm9");
      for (j = 0; j < 2; j++)
         thisres |= test( dt.res[i+j], dt.expected[i+j] );
      __asm __volatile__ ("vmovapd (%3), %%xmm9; vmovapd (%2), %%xmm7; vmovapd (%1), %%xmm8;"
                          "vfmsub231pd %%xmm7, %%xmm8, %%xmm9;"
                          "vmovapd %%xmm9, (%0)" : : "r" (&dt.res[i]), "r" (&dt.x[i]),
                                                     "r" (&dt.y[i]), "r" (&dt.z[i]) : "xmm7", "xmm8", "xmm9");
      for (j = 0; j < 2; j++)
         thisres |= test( dt.res[i+j], dt.expected[i+j] );
      __asm __volatile__ ("vmovapd (%3), %%xmm9; vmovapd (%1), %%xmm8;"
                          "vfmsub231pd (%2), %%xmm8, %%xmm9;"
                          "vmovapd %%xmm9, (%0)" : : "r" (&dt.res[i]), "r" (&dt.x[i]),
                                                     "r" (&dt.y[i]), "r" (&dt.z[i]) : "xmm7", "xmm8", "xmm9");
      for (j = 0; j < 2; j++)
         thisres |= test( dt.res[i+j], dt.expected[i+j] );
      if (thisres) {
         printf( "Failure 7 %d", i );
         for (j = 0; j < 2; j++)
            printf( " %a %a", dt.res[i+j], dt.expected[i+j] );
         printf( "\n" );
      }
      res |= thisres;
      thisres = 0;
      __asm __volatile__ ("vmovapd (%1), %%xmm9; vmovapd (%2), %%xmm7; vmovapd (%3), %%xmm8;"
                          "vfnmadd132pd %%xmm7, %%xmm8, %%xmm9;"
                          "vmovapd %%xmm9, (%0)" : : "r" (&dt.res[i]), "r" (&dt.x[i]),
                                                     "r" (&dt.y[i]), "r" (&dt.z[i]) : "xmm7", "xmm8", "xmm9");
      for (j = 0; j < 2; j++)
         thisres |= test( -dt.res[i+j], dt.expected[i+j] );
      __asm __volatile__ ("vmovapd (%1), %%xmm9; vmovapd (%3), %%xmm8;"
                          "vfnmadd132pd (%2), %%xmm8, %%xmm9;"
                          "vmovapd %%xmm9, (%0)" : : "r" (&dt.res[i]), "r" (&dt.x[i]),
                                                     "r" (&dt.y[i]), "r" (&dt.z[i]) : "xmm7", "xmm8", "xmm9");
      for (j = 0; j < 2; j++)
         thisres |= test( -dt.res[i+j], dt.expected[i+j] );
      __asm __volatile__ ("vmovapd (%1), %%xmm9; vmovapd (%3), %%xmm7; vmovapd (%2), %%xmm8;"
                          "vfnmadd213pd %%xmm7, %%xmm8, %%xmm9;"
                          "vmovapd %%xmm9, (%0)" : : "r" (&dt.res[i]), "r" (&dt.x[i]),
                                                     "r" (&dt.y[i]), "r" (&dt.z[i]) : "xmm7", "xmm8", "xmm9");
      for (j = 0; j < 2; j++)
         thisres |= test( -dt.res[i+j], dt.expected[i+j] );
      __asm __volatile__ ("vmovapd (%1), %%xmm9; vmovapd (%2), %%xmm8;"
                          "vfnmadd213pd (%3), %%xmm8, %%xmm9;"
                          "vmovapd %%xmm9, (%0)" : : "r" (&dt.res[i]), "r" (&dt.x[i]),
                                                     "r" (&dt.y[i]), "r" (&dt.z[i]) : "xmm7", "xmm8", "xmm9");
      for (j = 0; j < 2; j++)
         thisres |= test( -dt.res[i+j], dt.expected[i+j] );
      __asm __volatile__ ("vmovapd (%3), %%xmm9; vmovapd (%2), %%xmm7; vmovapd (%1), %%xmm8;"
                          "vfnmadd231pd %%xmm7, %%xmm8, %%xmm9;"
                          "vmovapd %%xmm9, (%0)" : : "r" (&dt.res[i]), "r" (&dt.x[i]),
                                                     "r" (&dt.y[i]), "r" (&dt.z[i]) : "xmm7", "xmm8", "xmm9");
      for (j = 0; j < 2; j++)
         thisres |= test( -dt.res[i+j], dt.expected[i+j] );
      __asm __volatile__ ("vmovapd (%3), %%xmm9; vmovapd (%1), %%xmm8;"
                          "vfnmadd231pd (%2), %%xmm8, %%xmm9;"
                          "vmovapd %%xmm9, (%0)" : : "r" (&dt.res[i]), "r" (&dt.x[i]),
                                                     "r" (&dt.y[i]), "r" (&dt.z[i]) : "xmm7", "xmm8", "xmm9");
      for (j = 0; j < 2; j++)
         thisres |= test( -dt.res[i+j], dt.expected[i+j] );
      if (thisres) {
         printf( "Failure 8 %d", i );
         for (j = 0; j < 2; j++)
            printf( " %a %a", dt.res[i+j], dt.expected[i+j] );
         printf( "\n" );
      }
      res |= thisres;
   }
   for (i = 1; i < N; i += 2)
      dt.z[i] = -dt.z[i];
   for (i = 0; i < N; i += 2) {
      int thisres = 0;
      __asm __volatile__ ("vmovapd (%1), %%xmm9; vmovapd (%2), %%xmm7; vmovapd (%3), %%xmm8;"
                          "vfmaddsub132pd %%xmm7, %%xmm8, %%xmm9;"
                          "vmovapd %%xmm9, (%0)" : : "r" (&dt.res[i]), "r" (&dt.x[i]),
                                                     "r" (&dt.y[i]), "r" (&dt.z[i]) : "xmm7", "xmm8", "xmm9");
      for (j = 0; j < 2; j++)
         thisres |= test( dt.res[i+j], dt.expected[i+j] );
      __asm __volatile__ ("vmovapd (%1), %%xmm9; vmovapd (%3), %%xmm8;"
                          "vfmaddsub132pd (%2), %%xmm8, %%xmm9;"
                          "vmovapd %%xmm9, (%0)" : : "r" (&dt.res[i]), "r" (&dt.x[i]),
                                                     "r" (&dt.y[i]), "r" (&dt.z[i]) : "xmm7", "xmm8", "xmm9");
      for (j = 0; j < 2; j++)
         thisres |= test( dt.res[i+j], dt.expected[i+j] );
      __asm __volatile__ ("vmovapd (%1), %%xmm9; vmovapd (%3), %%xmm7; vmovapd (%2), %%xmm8;"
                          "vfmaddsub213pd %%xmm7, %%xmm8, %%xmm9;"
                          "vmovapd %%xmm9, (%0)" : : "r" (&dt.res[i]), "r" (&dt.x[i]),
                                                     "r" (&dt.y[i]), "r" (&dt.z[i]) : "xmm7", "xmm8", "xmm9");
      for (j = 0; j < 2; j++)
         thisres |= test( dt.res[i+j], dt.expected[i+j] );
      __asm __volatile__ ("vmovapd (%1), %%xmm9; vmovapd (%2), %%xmm8;"
                          "vfmaddsub213pd (%3), %%xmm8, %%xmm9;"
                          "vmovapd %%xmm9, (%0)" : : "r" (&dt.res[i]), "r" (&dt.x[i]),
                                                     "r" (&dt.y[i]), "r" (&dt.z[i]) : "xmm7", "xmm8", "xmm9");
      for (j = 0; j < 2; j++)
         thisres |= test( dt.res[i+j], dt.expected[i+j] );
      __asm __volatile__ ("vmovapd (%3), %%xmm9; vmovapd (%2), %%xmm7; vmovapd (%1), %%xmm8;"
                          "vfmaddsub231pd %%xmm7, %%xmm8, %%xmm9;"
                          "vmovapd %%xmm9, (%0)" : : "r" (&dt.res[i]), "r" (&dt.x[i]),
                                                     "r" (&dt.y[i]), "r" (&dt.z[i]) : "xmm7", "xmm8", "xmm9");
      for (j = 0; j < 2; j++)
         thisres |= test( dt.res[i+j], dt.expected[i+j] );
      __asm __volatile__ ("vmovapd (%3), %%xmm9; vmovapd (%1), %%xmm8;"
                          "vfmaddsub231pd (%2), %%xmm8, %%xmm9;"
                          "vmovapd %%xmm9, (%0)" : : "r" (&dt.res[i]), "r" (&dt.x[i]),
                                                     "r" (&dt.y[i]), "r" (&dt.z[i]) : "xmm7", "xmm8", "xmm9");
      for (j = 0; j < 2; j++)
         thisres |= test( dt.res[i+j], dt.expected[i+j] );
      if (thisres) {
         printf( "Failure 9 %d", i );
         for (j = 0; j < 2; j++)
            printf( " %a %a", dt.res[i+j], dt.expected[i+j] );
         printf( "\n" );
      }
      res |= thisres;
   }
   for (i = 0; i < N; i++)
      dt.z[i] = -dt.z[i];
   for (i = 0; i < N; i += 2) {
      int thisres = 0;
      __asm __volatile__ ("vmovapd (%1), %%xmm9; vmovapd (%2), %%xmm7; vmovapd (%3), %%xmm8;"
                          "vfmsubadd132pd %%xmm7, %%xmm8, %%xmm9;"
                          "vmovapd %%xmm9, (%0)" : : "r" (&dt.res[i]), "r" (&dt.x[i]),
                                                     "r" (&dt.y[i]), "r" (&dt.z[i]) : "xmm7", "xmm8", "xmm9");
      for (j = 0; j < 2; j++)
         thisres |= test( dt.res[i+j], dt.expected[i+j] );
      __asm __volatile__ ("vmovapd (%1), %%xmm9; vmovapd (%3), %%xmm8;"
                          "vfmsubadd132pd (%2), %%xmm8, %%xmm9;"
                          "vmovapd %%xmm9, (%0)" : : "r" (&dt.res[i]), "r" (&dt.x[i]),
                                                     "r" (&dt.y[i]), "r" (&dt.z[i]) : "xmm7", "xmm8", "xmm9");
      for (j = 0; j < 2; j++)
         thisres |= test( dt.res[i+j], dt.expected[i+j] );
      __asm __volatile__ ("vmovapd (%1), %%xmm9; vmovapd (%3), %%xmm7; vmovapd (%2), %%xmm8;"
                          "vfmsubadd213pd %%xmm7, %%xmm8, %%xmm9;"
                          "vmovapd %%xmm9, (%0)" : : "r" (&dt.res[i]), "r" (&dt.x[i]),
                                                     "r" (&dt.y[i]), "r" (&dt.z[i]) : "xmm7", "xmm8", "xmm9");
      for (j = 0; j < 2; j++)
         thisres |= test( dt.res[i+j], dt.expected[i+j] );
      __asm __volatile__ ("vmovapd (%1), %%xmm9; vmovapd (%2), %%xmm8;"
                          "vfmsubadd213pd (%3), %%xmm8, %%xmm9;"
                          "vmovapd %%xmm9, (%0)" : : "r" (&dt.res[i]), "r" (&dt.x[i]),
                                                     "r" (&dt.y[i]), "r" (&dt.z[i]) : "xmm7", "xmm8", "xmm9");
      for (j = 0; j < 2; j++)
         thisres |= test( dt.res[i+j], dt.expected[i+j] );
      __asm __volatile__ ("vmovapd (%3), %%xmm9; vmovapd (%2), %%xmm7; vmovapd (%1), %%xmm8;"
                          "vfmsubadd231pd %%xmm7, %%xmm8, %%xmm9;"
                          "vmovapd %%xmm9, (%0)" : : "r" (&dt.res[i]), "r" (&dt.x[i]),
                                                     "r" (&dt.y[i]), "r" (&dt.z[i]) : "xmm7", "xmm8", "xmm9");
      for (j = 0; j < 2; j++)
         thisres |= test( dt.res[i+j], dt.expected[i+j] );
      __asm __volatile__ ("vmovapd (%3), %%xmm9; vmovapd (%1), %%xmm8;"
                          "vfmsubadd231pd (%2), %%xmm8, %%xmm9;"
                          "vmovapd %%xmm9, (%0)" : : "r" (&dt.res[i]), "r" (&dt.x[i]),
                                                     "r" (&dt.y[i]), "r" (&dt.z[i]) : "xmm7", "xmm8", "xmm9");
      for (j = 0; j < 2; j++)
         thisres |= test( dt.res[i+j], dt.expected[i+j] );
      if (thisres) {
         printf( "Failure 10 %d", i );
         for (j = 0; j < 2; j++)
            printf( " %a %a", dt.res[i+j], dt.expected[i+j] );
         printf( "\n" );
      }
      res |= thisres;
   }
   for (i = 1; i < N; i += 2)
      dt.z[i] = -dt.z[i];
   for (i = 0; i < N; i += 4) {
      int thisres = 0;
      __asm __volatile__ ("vmovapd (%1), %%ymm9; vmovapd (%2), %%ymm7; vmovapd (%3), %%ymm8;"
                          "vfmadd132pd %%ymm7, %%ymm8, %%ymm9;"
                          "vmovapd %%ymm9, (%0)" : : "r" (&dt.res[i]), "r" (&dt.x[i]),
                                                     "r" (&dt.y[i]), "r" (&dt.z[i]) : "xmm7", "xmm8", "xmm9");
      for (j = 0; j < 4; j++)
         thisres |= test( dt.res[i+j], dt.expected[i+j] );
      __asm __volatile__ ("vmovapd (%1), %%ymm9; vmovapd (%3), %%ymm8;"
                          "vfmadd132pd (%2), %%ymm8, %%ymm9;"
                          "vmovapd %%ymm9, (%0)" : : "r" (&dt.res[i]), "r" (&dt.x[i]),
                                                     "r" (&dt.y[i]), "r" (&dt.z[i]) : "xmm7", "xmm8", "xmm9");
      for (j = 0; j < 4; j++)
         thisres |= test( dt.res[i+j], dt.expected[i+j] );
      __asm __volatile__ ("vmovapd (%1), %%ymm9; vmovapd (%3), %%ymm7; vmovapd (%2), %%ymm8;"
                          "vfmadd213pd %%ymm7, %%ymm8, %%ymm9;"
                          "vmovapd %%ymm9, (%0)" : : "r" (&dt.res[i]), "r" (&dt.x[i]),
                                                     "r" (&dt.y[i]), "r" (&dt.z[i]) : "xmm7", "xmm8", "xmm9");
      for (j = 0; j < 4; j++)
         thisres |= test( dt.res[i+j], dt.expected[i+j] );
      __asm __volatile__ ("vmovapd (%1), %%ymm9; vmovapd (%2), %%ymm8;"
                          "vfmadd213pd (%3), %%ymm8, %%ymm9;"
                          "vmovapd %%ymm9, (%0)" : : "r" (&dt.res[i]), "r" (&dt.x[i]),
                                                     "r" (&dt.y[i]), "r" (&dt.z[i]) : "xmm7", "xmm8", "xmm9");
      for (j = 0; j < 4; j++)
         thisres |= test( dt.res[i+j], dt.expected[i+j] );
      __asm __volatile__ ("vmovapd (%3), %%ymm9; vmovapd (%2), %%ymm7; vmovapd (%1), %%ymm8;"
                          "vfmadd231pd %%ymm7, %%ymm8, %%ymm9;"
                          "vmovapd %%ymm9, (%0)" : : "r" (&dt.res[i]), "r" (&dt.x[i]),
                                                     "r" (&dt.y[i]), "r" (&dt.z[i]) : "xmm7", "xmm8", "xmm9");
      for (j = 0; j < 4; j++)
         thisres |= test( dt.res[i+j], dt.expected[i+j] );
      __asm __volatile__ ("vmovapd (%3), %%ymm9; vmovapd (%1), %%ymm8;"
                          "vfmadd231pd (%2), %%ymm8, %%ymm9;"
                          "vmovapd %%ymm9, (%0)" : : "r" (&dt.res[i]), "r" (&dt.x[i]),
                                                     "r" (&dt.y[i]), "r" (&dt.z[i]) : "xmm7", "xmm8", "xmm9");
      for (j = 0; j < 4; j++)
         thisres |= test( dt.res[i+j], dt.expected[i+j] );
      if (thisres) {
         printf( "Failure 11 %d", i );
         for (j = 0; j < 4; j++)
            printf( " %a %a", dt.res[i+j], dt.expected[i+j] );
         printf( "\n" );
      }
      res |= thisres;
      thisres = 0;
      __asm __volatile__ ("vmovapd (%1), %%ymm9; vmovapd (%2), %%ymm7; vmovapd (%3), %%ymm8;"
                          "vfnmsub132pd %%ymm7, %%ymm8, %%ymm9;"
                          "vmovapd %%ymm9, (%0)" : : "r" (&dt.res[i]), "r" (&dt.x[i]),
                                                     "r" (&dt.y[i]), "r" (&dt.z[i]) : "xmm7", "xmm8", "xmm9");
      for (j = 0; j < 4; j++)
         thisres |= test( -dt.res[i+j], dt.expected[i+j] );
      __asm __volatile__ ("vmovapd (%1), %%ymm9; vmovapd (%3), %%ymm8;"
                          "vfnmsub132pd (%2), %%ymm8, %%ymm9;"
                          "vmovapd %%ymm9, (%0)" : : "r" (&dt.res[i]), "r" (&dt.x[i]),
                                                     "r" (&dt.y[i]), "r" (&dt.z[i]) : "xmm7", "xmm8", "xmm9");
      for (j = 0; j < 4; j++)
         thisres |= test( -dt.res[i+j], dt.expected[i+j] );
      __asm __volatile__ ("vmovapd (%1), %%ymm9; vmovapd (%3), %%ymm7; vmovapd (%2), %%ymm8;"
                          "vfnmsub213pd %%ymm7, %%ymm8, %%ymm9;"
                          "vmovapd %%ymm9, (%0)" : : "r" (&dt.res[i]), "r" (&dt.x[i]),
                                                     "r" (&dt.y[i]), "r" (&dt.z[i]) : "xmm7", "xmm8", "xmm9");
      for (j = 0; j < 4; j++)
         thisres |= test( -dt.res[i+j], dt.expected[i+j] );
      __asm __volatile__ ("vmovapd (%1), %%ymm9; vmovapd (%2), %%ymm8;"
                          "vfnmsub213pd (%3), %%ymm8, %%ymm9;"
                          "vmovapd %%ymm9, (%0)" : : "r" (&dt.res[i]), "r" (&dt.x[i]),
                                                     "r" (&dt.y[i]), "r" (&dt.z[i]) : "xmm7", "xmm8", "xmm9");
      for (j = 0; j < 4; j++)
         thisres |= test( -dt.res[i+j], dt.expected[i+j] );
      __asm __volatile__ ("vmovapd (%3), %%ymm9; vmovapd (%2), %%ymm7; vmovapd (%1), %%ymm8;"
                          "vfnmsub231pd %%ymm7, %%ymm8, %%ymm9;"
                          "vmovapd %%ymm9, (%0)" : : "r" (&dt.res[i]), "r" (&dt.x[i]),
                                                     "r" (&dt.y[i]), "r" (&dt.z[i]) : "xmm7", "xmm8", "xmm9");
      for (j = 0; j < 4; j++)
         thisres |= test( -dt.res[i+j], dt.expected[i+j] );
      __asm __volatile__ ("vmovapd (%3), %%ymm9; vmovapd (%1), %%ymm8;"
                          "vfnmsub231pd (%2), %%ymm8, %%ymm9;"
                          "vmovapd %%ymm9, (%0)" : : "r" (&dt.res[i]), "r" (&dt.x[i]),
                                                     "r" (&dt.y[i]), "r" (&dt.z[i]) : "xmm7", "xmm8", "xmm9");
      for (j = 0; j < 4; j++)
         thisres |= test( -dt.res[i+j], dt.expected[i+j] );
      if (thisres) {
         printf( "Failure 12 %d", i );
         for (j = 0; j < 4; j++)
            printf( " %a %a", dt.res[i+j], dt.expected[i+j] );
         printf( "\n" );
      }
      res |= thisres;
   }
   for (i = 0; i < N; i++)
      dt.z[i] = -dt.z[i];
   for (i = 0; i < N; i += 4) {
      int thisres = 0;
      __asm __volatile__ ("vmovapd (%1), %%ymm9; vmovapd (%2), %%ymm7; vmovapd (%3), %%ymm8;"
                          "vfmsub132pd %%ymm7, %%ymm8, %%ymm9;"
                          "vmovapd %%ymm9, (%0)" : : "r" (&dt.res[i]), "r" (&dt.x[i]),
                                                     "r" (&dt.y[i]), "r" (&dt.z[i]) : "xmm7", "xmm8", "xmm9");
      for (j = 0; j < 4; j++)
         thisres |= test( dt.res[i+j], dt.expected[i+j] );
      __asm __volatile__ ("vmovapd (%1), %%ymm9; vmovapd (%3), %%ymm8;"
                          "vfmsub132pd (%2), %%ymm8, %%ymm9;"
                          "vmovapd %%ymm9, (%0)" : : "r" (&dt.res[i]), "r" (&dt.x[i]),
                                                     "r" (&dt.y[i]), "r" (&dt.z[i]) : "xmm7", "xmm8", "xmm9");
      for (j = 0; j < 4; j++)
         thisres |= test( dt.res[i+j], dt.expected[i+j] );
      __asm __volatile__ ("vmovapd (%1), %%ymm9; vmovapd (%3), %%ymm7; vmovapd (%2), %%ymm8;"
                          "vfmsub213pd %%ymm7, %%ymm8, %%ymm9;"
                          "vmovapd %%ymm9, (%0)" : : "r" (&dt.res[i]), "r" (&dt.x[i]),
                                                     "r" (&dt.y[i]), "r" (&dt.z[i]) : "xmm7", "xmm8", "xmm9");
      for (j = 0; j < 4; j++)
         thisres |= test( dt.res[i+j], dt.expected[i+j] );
      __asm __volatile__ ("vmovapd (%1), %%ymm9; vmovapd (%2), %%ymm8;"
                          "vfmsub213pd (%3), %%ymm8, %%ymm9;"
                          "vmovapd %%ymm9, (%0)" : : "r" (&dt.res[i]), "r" (&dt.x[i]),
                                                     "r" (&dt.y[i]), "r" (&dt.z[i]) : "xmm7", "xmm8", "xmm9");
      for (j = 0; j < 4; j++)
         thisres |= test( dt.res[i+j], dt.expected[i+j] );
      __asm __volatile__ ("vmovapd (%3), %%ymm9; vmovapd (%2), %%ymm7; vmovapd (%1), %%ymm8;"
                          "vfmsub231pd %%ymm7, %%ymm8, %%ymm9;"
                          "vmovapd %%ymm9, (%0)" : : "r" (&dt.res[i]), "r" (&dt.x[i]),
                                                     "r" (&dt.y[i]), "r" (&dt.z[i]) : "xmm7", "xmm8", "xmm9");
      for (j = 0; j < 4; j++)
         thisres |= test( dt.res[i+j], dt.expected[i+j] );
      __asm __volatile__ ("vmovapd (%3), %%ymm9; vmovapd (%1), %%ymm8;"
                          "vfmsub231pd (%2), %%ymm8, %%ymm9;"
                          "vmovapd %%ymm9, (%0)" : : "r" (&dt.res[i]), "r" (&dt.x[i]),
                                                     "r" (&dt.y[i]), "r" (&dt.z[i]) : "xmm7", "xmm8", "xmm9");
      for (j = 0; j < 4; j++)
         thisres |= test( dt.res[i+j], dt.expected[i+j] );
      if (thisres) {
         printf( "Failure 13 %d", i );
         for (j = 0; j < 4; j++)
            printf( " %a %a", dt.res[i+j], dt.expected[i+j] );
         printf( "\n" );
      }
      res |= thisres;
      thisres = 0;
      __asm __volatile__ ("vmovapd (%1), %%ymm9; vmovapd (%2), %%ymm7; vmovapd (%3), %%ymm8;"
                          "vfnmadd132pd %%ymm7, %%ymm8, %%ymm9;"
                          "vmovapd %%ymm9, (%0)" : : "r" (&dt.res[i]), "r" (&dt.x[i]),
                                                     "r" (&dt.y[i]), "r" (&dt.z[i]) : "xmm7", "xmm8", "xmm9");
      for (j = 0; j < 4; j++)
         thisres |= test( -dt.res[i+j], dt.expected[i+j] );
      __asm __volatile__ ("vmovapd (%1), %%ymm9; vmovapd (%3), %%ymm8;"
                          "vfnmadd132pd (%2), %%ymm8, %%ymm9;"
                          "vmovapd %%ymm9, (%0)" : : "r" (&dt.res[i]), "r" (&dt.x[i]),
                                                     "r" (&dt.y[i]), "r" (&dt.z[i]) : "xmm7", "xmm8", "xmm9");
      for (j = 0; j < 4; j++)
         thisres |= test( -dt.res[i+j], dt.expected[i+j] );
      __asm __volatile__ ("vmovapd (%1), %%ymm9; vmovapd (%3), %%ymm7; vmovapd (%2), %%ymm8;"
                          "vfnmadd213pd %%ymm7, %%ymm8, %%ymm9;"
                          "vmovapd %%ymm9, (%0)" : : "r" (&dt.res[i]), "r" (&dt.x[i]),
                                                     "r" (&dt.y[i]), "r" (&dt.z[i]) : "xmm7", "xmm8", "xmm9");
      for (j = 0; j < 4; j++)
         thisres |= test( -dt.res[i+j], dt.expected[i+j] );
      __asm __volatile__ ("vmovapd (%1), %%ymm9; vmovapd (%2), %%ymm8;"
                          "vfnmadd213pd (%3), %%ymm8, %%ymm9;"
                          "vmovapd %%ymm9, (%0)" : : "r" (&dt.res[i]), "r" (&dt.x[i]),
                                                     "r" (&dt.y[i]), "r" (&dt.z[i]) : "xmm7", "xmm8", "xmm9");
      for (j = 0; j < 4; j++)
         thisres |= test( -dt.res[i+j], dt.expected[i+j] );
      __asm __volatile__ ("vmovapd (%3), %%ymm9; vmovapd (%2), %%ymm7; vmovapd (%1), %%ymm8;"
                          "vfnmadd231pd %%ymm7, %%ymm8, %%ymm9;"
                          "vmovapd %%ymm9, (%0)" : : "r" (&dt.res[i]), "r" (&dt.x[i]),
                                                     "r" (&dt.y[i]), "r" (&dt.z[i]) : "xmm7", "xmm8", "xmm9");
      for (j = 0; j < 4; j++)
         thisres |= test( -dt.res[i+j], dt.expected[i+j] );
      __asm __volatile__ ("vmovapd (%3), %%ymm9; vmovapd (%1), %%ymm8;"
                          "vfnmadd231pd (%2), %%ymm8, %%ymm9;"
                          "vmovapd %%ymm9, (%0)" : : "r" (&dt.res[i]), "r" (&dt.x[i]),
                                                     "r" (&dt.y[i]), "r" (&dt.z[i]) : "xmm7", "xmm8", "xmm9");
      for (j = 0; j < 4; j++)
         thisres |= test( -dt.res[i+j], dt.expected[i+j] );
      if (thisres) {
         printf( "Failure 14 %d", i );
         for (j = 0; j < 4; j++)
            printf( " %a %a", dt.res[i+j], dt.expected[i+j] );
         printf( "\n" );
      }
      res |= thisres;
   }
   for (i = 1; i < N; i += 2)
      dt.z[i] = -dt.z[i];
   for (i = 0; i < N; i += 4) {
      int thisres = 0;
      __asm __volatile__ ("vmovapd (%1), %%ymm9; vmovapd (%2), %%ymm7; vmovapd (%3), %%ymm8;"
                          "vfmaddsub132pd %%ymm7, %%ymm8, %%ymm9;"
                          "vmovapd %%ymm9, (%0)" : : "r" (&dt.res[i]), "r" (&dt.x[i]),
                                                     "r" (&dt.y[i]), "r" (&dt.z[i]) : "xmm7", "xmm8", "xmm9");
      for (j = 0; j < 4; j++)
         thisres |= test( dt.res[i+j], dt.expected[i+j] );
      __asm __volatile__ ("vmovapd (%1), %%ymm9; vmovapd (%3), %%ymm8;"
                          "vfmaddsub132pd (%2), %%ymm8, %%ymm9;"
                          "vmovapd %%ymm9, (%0)" : : "r" (&dt.res[i]), "r" (&dt.x[i]),
                                                     "r" (&dt.y[i]), "r" (&dt.z[i]) : "xmm7", "xmm8", "xmm9");
      for (j = 0; j < 4; j++)
         thisres |= test( dt.res[i+j], dt.expected[i+j] );
      __asm __volatile__ ("vmovapd (%1), %%ymm9; vmovapd (%3), %%ymm7; vmovapd (%2), %%ymm8;"
                          "vfmaddsub213pd %%ymm7, %%ymm8, %%ymm9;"
                          "vmovapd %%ymm9, (%0)" : : "r" (&dt.res[i]), "r" (&dt.x[i]),
                                                     "r" (&dt.y[i]), "r" (&dt.z[i]) : "xmm7", "xmm8", "xmm9");
      for (j = 0; j < 4; j++)
         thisres |= test( dt.res[i+j], dt.expected[i+j] );
      __asm __volatile__ ("vmovapd (%1), %%ymm9; vmovapd (%2), %%ymm8;"
                          "vfmaddsub213pd (%3), %%ymm8, %%ymm9;"
                          "vmovapd %%ymm9, (%0)" : : "r" (&dt.res[i]), "r" (&dt.x[i]),
                                                     "r" (&dt.y[i]), "r" (&dt.z[i]) : "xmm7", "xmm8", "xmm9");
      for (j = 0; j < 4; j++)
         thisres |= test( dt.res[i+j], dt.expected[i+j] );
      __asm __volatile__ ("vmovapd (%3), %%ymm9; vmovapd (%2), %%ymm7; vmovapd (%1), %%ymm8;"
                          "vfmaddsub231pd %%ymm7, %%ymm8, %%ymm9;"
                          "vmovapd %%ymm9, (%0)" : : "r" (&dt.res[i]), "r" (&dt.x[i]),
                                                     "r" (&dt.y[i]), "r" (&dt.z[i]) : "xmm7", "xmm8", "xmm9");
      for (j = 0; j < 4; j++)
         thisres |= test( dt.res[i+j], dt.expected[i+j] );
      __asm __volatile__ ("vmovapd (%3), %%ymm9; vmovapd (%1), %%ymm8;"
                          "vfmaddsub231pd (%2), %%ymm8, %%ymm9;"
                          "vmovapd %%ymm9, (%0)" : : "r" (&dt.res[i]), "r" (&dt.x[i]),
                                                     "r" (&dt.y[i]), "r" (&dt.z[i]) : "xmm7", "xmm8", "xmm9");
      for (j = 0; j < 4; j++)
         thisres |= test( dt.res[i+j], dt.expected[i+j] );
      if (thisres) {
         printf( "Failure 15 %d", i );
         for (j = 0; j < 4; j++)
            printf( " %a %a", dt.res[i+j], dt.expected[i+j] );
         printf( "\n" );
      }
      res |= thisres;
   }
   for (i = 0; i < N; i++)
      dt.z[i] = -dt.z[i];
   for (i = 0; i < N; i += 4) {
      int thisres = 0;
      __asm __volatile__ ("vmovapd (%1), %%ymm9; vmovapd (%2), %%ymm7; vmovapd (%3), %%ymm8;"
                          "vfmsubadd132pd %%ymm7, %%ymm8, %%ymm9;"
                          "vmovapd %%ymm9, (%0)" : : "r" (&dt.res[i]), "r" (&dt.x[i]),
                                                     "r" (&dt.y[i]), "r" (&dt.z[i]) : "xmm7", "xmm8", "xmm9");
      for (j = 0; j < 4; j++)
         thisres |= test( dt.res[i+j], dt.expected[i+j] );
      __asm __volatile__ ("vmovapd (%1), %%ymm9; vmovapd (%3), %%ymm8;"
                          "vfmsubadd132pd (%2), %%ymm8, %%ymm9;"
                          "vmovapd %%ymm9, (%0)" : : "r" (&dt.res[i]), "r" (&dt.x[i]),
                                                     "r" (&dt.y[i]), "r" (&dt.z[i]) : "xmm7", "xmm8", "xmm9");
      for (j = 0; j < 4; j++)
         thisres |= test( dt.res[i+j], dt.expected[i+j] );
      __asm __volatile__ ("vmovapd (%1), %%ymm9; vmovapd (%3), %%ymm7; vmovapd (%2), %%ymm8;"
                          "vfmsubadd213pd %%ymm7, %%ymm8, %%ymm9;"
                          "vmovapd %%ymm9, (%0)" : : "r" (&dt.res[i]), "r" (&dt.x[i]),
                                                     "r" (&dt.y[i]), "r" (&dt.z[i]) : "xmm7", "xmm8", "xmm9");
      for (j = 0; j < 4; j++)
         thisres |= test( dt.res[i+j], dt.expected[i+j] );
      __asm __volatile__ ("vmovapd (%1), %%ymm9; vmovapd (%2), %%ymm8;"
                          "vfmsubadd213pd (%3), %%ymm8, %%ymm9;"
                          "vmovapd %%ymm9, (%0)" : : "r" (&dt.res[i]), "r" (&dt.x[i]),
                                                     "r" (&dt.y[i]), "r" (&dt.z[i]) : "xmm7", "xmm8", "xmm9");
      for (j = 0; j < 4; j++)
         thisres |= test( dt.res[i+j], dt.expected[i+j] );
      __asm __volatile__ ("vmovapd (%3), %%ymm9; vmovapd (%2), %%ymm7; vmovapd (%1), %%ymm8;"
                          "vfmsubadd231pd %%ymm7, %%ymm8, %%ymm9;"
                          "vmovapd %%ymm9, (%0)" : : "r" (&dt.res[i]), "r" (&dt.x[i]),
                                                     "r" (&dt.y[i]), "r" (&dt.z[i]) : "xmm7", "xmm8", "xmm9");
      for (j = 0; j < 4; j++)
         thisres |= test( dt.res[i+j], dt.expected[i+j] );
      __asm __volatile__ ("vmovapd (%3), %%ymm9; vmovapd (%1), %%ymm8;"
                          "vfmsubadd231pd (%2), %%ymm8, %%ymm9;"
                          "vmovapd %%ymm9, (%0)" : : "r" (&dt.res[i]), "r" (&dt.x[i]),
                                                     "r" (&dt.y[i]), "r" (&dt.z[i]) : "xmm7", "xmm8", "xmm9");
      for (j = 0; j < 4; j++)
         thisres |= test( dt.res[i+j], dt.expected[i+j] );
      if (thisres) {
         printf( "Failure 16 %d", i );
         for (j = 0; j < 4; j++)
            printf( " %a %a", dt.res[i+j], dt.expected[i+j] );
         printf( "\n" );
      }
      res |= thisres;
   }
   for (i = 1; i < N; i += 2)
      dt.z[i] = -dt.z[i];
   return res;
}

int main( )
{
   int res = 0;
   int i = 0;
   plus_zero = 0.0;
   __asm __volatile__ ("" : : "r" (&plus_zero) : "memory");
   nan_value = plus_zero / plus_zero;
   plus_infty = 3.40282346638528859812e+38F * 16.0F;
   minus_infty = -plus_infty;
#define TEST_F( a, b, c, d ) \
   do {				\
      ft.x[i] = a;		\
      ft.y[i] = b;		\
      ft.z[i] = c;		\
      ft.expected[i] = d;	\
      i++;			\
   } while (0)
   TEST_F( 1.0, 2.0, 3.0, 5.0 );
   TEST_F( nan_value, 2.0, 3.0, nan_value );
   TEST_F( 1.0, nan_value, 3.0, nan_value );
   TEST_F( 1.0, 2.0, nan_value, nan_value );
   TEST_F( plus_infty, 0.0, nan_value, nan_value );
   TEST_F( minus_infty, 0.0, nan_value, nan_value );
   TEST_F( 0.0, plus_infty, nan_value, nan_value );
   TEST_F( 0.0, minus_infty, nan_value, nan_value );
   TEST_F( plus_infty, 0.0, 1.0, nan_value );
   TEST_F( minus_infty, 0.0, 1.0, nan_value );
   TEST_F( 0.0, plus_infty, 1.0, nan_value );
   TEST_F( 0.0, minus_infty, 1.0, nan_value );
   TEST_F( plus_infty, plus_infty, minus_infty, nan_value );
   TEST_F( minus_infty, plus_infty, plus_infty, nan_value );
   TEST_F( plus_infty, minus_infty, plus_infty, nan_value );
   TEST_F( minus_infty, minus_infty, minus_infty, nan_value );
   TEST_F( plus_infty, 3.5L, minus_infty, nan_value );
   TEST_F( minus_infty, -7.5L, minus_infty, nan_value );
   TEST_F( -13.5L, plus_infty, plus_infty, nan_value );
   TEST_F( minus_infty, 7.5L, plus_infty, nan_value );
   TEST_F( 1.25L, 0.75L, 0.0625L, 1.0L );
   TEST_F( -3.40282346638528859812e+38F, -3.40282346638528859812e+38F, minus_infty, minus_infty );
   TEST_F( 3.40282346638528859812e+38F / 2, 3.40282346638528859812e+38F / 2, minus_infty, minus_infty );
   TEST_F( -3.40282346638528859812e+38F, 3.40282346638528859812e+38F, plus_infty, plus_infty );
   TEST_F( 3.40282346638528859812e+38F / 2, -3.40282346638528859812e+38F / 4, plus_infty, plus_infty );
   TEST_F( plus_infty, 4, plus_infty, plus_infty );
   TEST_F( 2, minus_infty, minus_infty, minus_infty );
   TEST_F( minus_infty, minus_infty, plus_infty, plus_infty );
   TEST_F( plus_infty, minus_infty, minus_infty, minus_infty );
   TEST_F( 0x1.7ff8p+13, 0x1.000002p+0, 0x1.ffffp-24, 0x1.7ff802p+13 );
   TEST_F( 0x1.fffp+0, 0x1.00001p+0, -0x1.fffp+0, 0x1.fffp-20 );
   TEST_F( 0x1.9abcdep+127, 0x0.9abcdep-126, -0x1.f08948p+0, 0x1.bb421p-25 );
   TEST_F( 0x1.9abcdep+100, 0x0.9abcdep-126, -0x1.f08948p-27, 0x1.bb421p-52 );
   TEST_F( 0x1.fffffep+127, 0x1.001p+0, -0x1.fffffep+127, 0x1.fffffep+115 );
   TEST_F( -0x1.fffffep+127, 0x1.fffffep+0, 0x1.fffffep+127, -0x1.fffffap+127 );
   TEST_F( 0x1.fffffep+127, 2.0, -0x1.fffffep+127, 0x1.fffffep+127 );

   res |= test_fmaf( );
   i = 0;
#define TEST( a, b, c, d ) \
   do {				\
      dt.x[i] = a;		\
      dt.y[i] = b;		\
      dt.z[i] = c;		\
      dt.expected[i] = d;	\
      i++;			\
   } while (0)
   TEST( 1.0, 2.0, 3.0, 5.0 );
   TEST( nan_value, 2.0, 3.0, nan_value );
   TEST( 1.0, nan_value, 3.0, nan_value );
   TEST( 1.0, 2.0, nan_value, nan_value );
   TEST( plus_infty, 0.0, nan_value, nan_value );
   TEST( minus_infty, 0.0, nan_value, nan_value );
   TEST( 0.0, plus_infty, nan_value, nan_value );
   TEST( 0.0, minus_infty, nan_value, nan_value );
   TEST( plus_infty, 0.0, 1.0, nan_value );
   TEST( minus_infty, 0.0, 1.0, nan_value );
   TEST( 0.0, plus_infty, 1.0, nan_value );
   TEST( 0.0, minus_infty, 1.0, nan_value );
   TEST( plus_infty, plus_infty, minus_infty, nan_value );
   TEST( minus_infty, plus_infty, plus_infty, nan_value );
   TEST( plus_infty, minus_infty, plus_infty, nan_value );
   TEST( minus_infty, minus_infty, minus_infty, nan_value );
   TEST( plus_infty, 3.5L, minus_infty, nan_value );
   TEST( minus_infty, -7.5L, minus_infty, nan_value );
   TEST( -13.5L, plus_infty, plus_infty, nan_value );
   TEST( minus_infty, 7.5L, plus_infty, nan_value );
   TEST( 1.25L, 0.75L, 0.0625L, 1.0L );
   TEST( -1.79769313486231570815e+308L, -1.79769313486231570815e+308L, minus_infty, minus_infty );
   TEST( 1.79769313486231570815e+308L / 2, 1.79769313486231570815e+308L / 2, minus_infty, minus_infty );
   TEST( -1.79769313486231570815e+308L, 1.79769313486231570815e+308L, plus_infty, plus_infty );
   TEST( 1.79769313486231570815e+308L / 2, -1.79769313486231570815e+308L / 4, plus_infty, plus_infty );
   TEST( plus_infty, 4, plus_infty, plus_infty );
   TEST( 2, minus_infty, minus_infty, minus_infty );
   TEST( minus_infty, minus_infty, plus_infty, plus_infty );
   TEST( plus_infty, minus_infty, minus_infty, minus_infty );
   TEST( 0x1.7fp+13, 0x1.0000000000001p+0, 0x1.ffep-48, 0x1.7f00000000001p+13 );
   TEST( 0x1.fffp+0, 0x1.0000000000001p+0, -0x1.fffp+0, 0x1.fffp-52 );
   TEST( 0x1.0000002p+0, 0x1.ffffffcp-1, 0x1p-300, 1.0 );
   TEST( 0x1.0000002p+0, 0x1.ffffffcp-1, -0x1p-300, 0x1.fffffffffffffp-1 );
   TEST( 0x1.deadbeef2feedp+1023, 0x0.deadbeef2feedp-1022, -0x1.a05f8c01a4bfbp+1, 0x1.0989687bc9da4p-53 );
   TEST( 0x1.deadbeef2feedp+900, 0x0.deadbeef2feedp-1022, -0x1.a05f8c01a4bfbp-122, 0x1.0989687bc9da4p-176 );
   TEST( 0x1.fffffffffffffp+1023, 0x1.001p+0, -0x1.fffffffffffffp+1023, 0x1.fffffffffffffp+1011 );
   TEST( -0x1.fffffffffffffp+1023, 0x1.fffffffffffffp+0, 0x1.fffffffffffffp+1023, -0x1.ffffffffffffdp+1023 );
   TEST( 0x1.fffffffffffffp+1023, 2.0, -0x1.fffffffffffffp+1023, 0x1.fffffffffffffp+1023 );
   TEST( 0x1.6a09e667f3bccp-538, 0x1.6a09e667f3bccp-538, 0.0, 0.0 );
   TEST( 0x1.deadbeef2feedp-495, 0x1.deadbeef2feedp-495, -0x1.bf86a5786a574p-989, 0x0.0000042625a1fp-1022 );
   TEST( 0x1.deadbeef2feedp-503, 0x1.deadbeef2feedp-503, -0x1.bf86a5786a574p-1005, 0x0.0000000004262p-1022 );
   TEST( 0x1p-537, 0x1p-538, 0x1p-1074, 0x0.0000000000002p-1022 );
   TEST( 0x1.7fffff8p-968, 0x1p-106, 0x0.000001p-1022, 0x0.0000010000001p-1022 );
   TEST( 0x1.4000004p-967, 0x1p-106, 0x0.000001p-1022, 0x0.0000010000003p-1022 );
   TEST( 0x1.4p-967, -0x1p-106, -0x0.000001p-1022, -0x0.0000010000002p-1022 );
   TEST( -0x1.19cab66d73e17p-959, 0x1.c7108a8c5ff51p-107, -0x0.80b0ad65d9b64p-1022, -0x0.80b0ad65d9d59p-1022 );
   TEST( -0x1.d2eaed6e8e9d3p-979, -0x1.4e066c62ac9ddp-63, -0x0.9245e6b003454p-1022, -0x0.9245c09c5fb5dp-1022 );
   TEST( 0x1.153d650bb9f06p-907, 0x1.2d01230d48407p-125, -0x0.b278d5acfc3cp-1022, -0x0.b22757123bbe9p-1022 );
   TEST( -0x1.fffffffffffffp-711, 0x1.fffffffffffffp-275, 0x1.fffffe00007ffp-983, 0x1.7ffffe00007ffp-983 );

   res |= test_fma( );
   if (res == 0)
      printf( "Testing successful\n");
   return 0;
}
