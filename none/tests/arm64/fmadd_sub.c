#include <math.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>

#define COUNT 5

static void
print_float(const char *ident, float x)
{
  union
  {
    float f;
    uint32_t i;
  } u;

  u.f = x;
  printf("%s = %08x = %.17g\n", ident, u.i, x);
}

static void
print_double(const char *ident, double x)
{
  union
  {
    double f;
    uint64_t i;
  } u;

  u.f = x;
  printf("%s = %016lx = %.17g\n", ident, u.i, x);
}

int
main(int argc, char **argv)
{
  float x[] = { 55,                  0.98076171874999996, 0, 1, 0xFFFFFFFF } ;
  float y[] = { 0.69314718055994529, 1.015625,            0, 1, 0xFFFFFFFF };
  float z[] = { 38.123094930796988,  1,                   0, 1, 0xFFFFFFFF };
  float dst = -5;

  double dx[] = { 55,                  0.98076171874999996, 0, 1, 0xFFFFFFFF } ;
  double dy[] = { 0.69314718055994529, 1.015625,            0, 1, 0xFFFFFFFF };
  double dz[] = { 38.123094930796988,  1,                   0, 1, 0xFFFFFFFF };
  double ddst= -5;

  int i;

  for (i = 0; i < COUNT; i++) {
	  //32bit variant
	  asm("fmadd %s0, %s1, %s2, %s3\n;" : "=w"(dst) : "w"(x[i]), "w"(y[i]), "w"(z[i]));
	  printf("FMADD 32bit: dst = z + x * y\n");
	  printf("%f = %f + %f * %f\n", dst, z[i], x[i], y[i]);
	  print_float("dst", dst);

	  // Floating-point negated fused multiply-add
	  asm("fnmadd %s0, %s1, %s2, %s3\n;" : "=w"(dst) : "w"(x[i]), "w"(y[i]), "w"(z[i]));
	  printf("FNMADD 32bit: dst = -z + (-x) * y\n");
	  printf("%f = -%f + (-%f) * %f\n", dst, z[i], x[i], y[i]);
	  print_float("dst", dst);

	  asm("fmsub %s0, %s1, %s2, %s3\n;" : "=w"(dst) : "w"(x[i]), "w"(y[i]), "w"(z[i]));
	  printf("FMSUB 32bit: dst = z + (-x) * y\n");
	  printf("%f = %f + (-%f) * %f\n", dst, z[i], x[i], y[i]);
	  print_float("dst", dst);

	  asm("fnmsub %s0, %s1, %s2, %s3\n;" : "=w"(dst) : "w"(x[i]), "w"(y[i]), "w"(z[i]));
	  printf("FNMSUB 32bit: dst = -z + x * y\n");
	  printf("%f = -%f + %f * %f\n", dst, z[i], x[i], y[i]);
	  print_float("dst", dst);

	  //64bit variant
	  asm("fmadd %d0, %d1, %d2, %d3\n;" : "=w"(ddst) : "w"(dx[i]), "w"(dy[i]), "w"(dz[i]));
	  printf("FMADD 64bit: dst = z + x * y\n");
	  printf("%f = %f + %f * %f\n", ddst, dz[i], dx[i], dy[i]);
	  print_double("dst", ddst);

	  asm("fnmadd %d0, %d1, %d2, %d3\n;" : "=w"(ddst) : "w"(dx[i]), "w"(dy[i]), "w"(dz[i]));
	  printf("FNMADD 64bit: dst = -z + (-x) * y\n");
	  printf("%f = -%f - %f * %f\n", ddst, dz[i], dx[i], dy[i]);
	  print_double("dst", ddst);

	  asm("fmsub %d0, %d1, %d2, %d3\n;" : "=w"(ddst) : "w"(dx[i]), "w"(dy[i]), "w"(dz[i]));
	  printf("FMSUB 64bit: dst = z + (-x) * y\n");
	  printf("%f = %f + (-%f) * %f\n", ddst, dz[i], dx[i], dy[i]);
	  print_double("dst", ddst);

	  asm("fnmsub %d0, %d1, %d2, %d3\n;" : "=w"(ddst) : "w"(dx[i]), "w"(dy[i]), "w"(dz[i]));
	  printf("FNMSUB 64bit: dst = -z + x * y\n");
	  printf("%f = -%f + %f * %f\n", ddst, dz[i], dx[i], dy[i]);
	  print_double("dst", ddst);

	  printf("\n");
  }

  return 0;
}

