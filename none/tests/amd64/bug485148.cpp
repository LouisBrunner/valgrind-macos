#include <stdio.h>
#include <x86intrin.h>

static __attribute__((noinline)) void
test_fma_ss(float dst[4], const float a[4], const float b[4], const float c[4])
{
   __m128 av = _mm_loadu_ps(a);
   __m128 bv = _mm_loadu_ps(b);
   __m128 cv = _mm_loadu_ps(c);

   __m128 dv = _mm_fmadd_ss(av, bv, cv);
   _mm_storeu_ps(dst, dv);
}

static __attribute__((noinline)) void
test_fma_sd(double dst[2], const double a[2], const double b[2], const double c[2])
{
   __m128d av = _mm_loadu_pd(a);
   __m128d bv = _mm_loadu_pd(b);
   __m128d cv = _mm_loadu_pd(c);

   __m128d dv = _mm_fmadd_sd(av, bv, cv);
   _mm_storeu_pd(dst, dv);
}

int main()
{
   float a[4] = {1, 2, 3, 4};
   float b[4] = {3, 11, 35, 1};
   float c[4] = {-1, -2, -19, 0};

   float dst_f[4];
   test_fma_ss(dst_f, a, b, c);

   printf("[%f %f %f %f]\n", dst_f[0], dst_f[1], dst_f[2], dst_f[3]);

   double d[2] = {5, 6};
   double e[2] = {2, 18};
   double f[2] = {3, 15};

   double dst_d[2];
   test_fma_sd(dst_d, d, e, f);

   printf("[%f %f]\n", dst_d[0], dst_d[1]);
}
