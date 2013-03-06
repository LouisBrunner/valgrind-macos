
#include <stdio.h>
#include <string.h>

typedef  unsigned long long int  ULong;

__attribute__((noinline)) float s32_to_f32_imm1(int x)
{
    float y;
    __asm__ ("vcvt.f32.s32 %0, %1, #1" : "=w"(y) : "0"(x));
    return y;
}

__attribute__((noinline)) float s32_to_f32_imm32(int x)
{
    float y;
    __asm__ ("vcvt.f32.s32 %0, %1, #32" : "=w"(y) : "0"(x));
    return y;
}

void try_s32_to_f32 ( int x )
{
  float f32 = s32_to_f32_imm32(x);
  printf("s32_to_f32_imm32:  %11d  ->  %18.14e\n", x, (double)f32);
  f32 = s32_to_f32_imm1(x);
  printf("s32_to_f32_imm1:   %11d  ->  %18.14e\n", x, (double)f32);
}



__attribute__((noinline)) float u32_to_f32_imm1(int x)
{
    float y;
    __asm__ ("vcvt.f32.u32 %0, %1, #1" : "=w"(y) : "0"(x));
    return y;
}

__attribute__((noinline)) float u32_to_f32_imm32(int x)
{
    float y;
    __asm__ ("vcvt.f32.u32 %0, %1, #32" : "=w"(y) : "0"(x));
    return y;
}

void try_u32_to_f32 ( unsigned int x )
{
  float f32 = u32_to_f32_imm32(x);
  printf("u32_to_f32_imm32:  %11u  ->  %18.14e\n", x, (double)f32);
  f32 = u32_to_f32_imm1(x);
  printf("u32_to_f32_imm1:   %11u  ->  %18.14e\n", x, (double)f32);
}



__attribute__((noinline)) double s32_to_f64_imm1(int x)
{
    double block[2];
    memset(block, 0x55, sizeof(block));
    __asm__ __volatile__(
       "mov r8, %1"               "\n\t"
       "vldr d14, [%0, #8]"       "\n\t" // d14 <- junk
       "vmov s1, r8"              "\n\t"
       "vcvt.f64.s32 d14,d14,#1"  "\n\t"
       "vstr d14, [%0]"           "\n\t"
       : : /*IN*/"r"(&block[0]), "r"(x) : /*TRASH*/"r8","s28","d14","memory"
    );
    return block[0];
}

__attribute__((noinline)) double s32_to_f64_imm32(int x)
{
    double block[2];
    memset(block, 0x55, sizeof(block));
    __asm__ __volatile__(
       "mov r8, %1"                "\n\t"
       "vldr d14, [%0, #8]"        "\n\t" // d14 <- junk
       "vmov s28, r8"              "\n\t"
       "vcvt.f64.s32 d14,d14,#32"  "\n\t"
       "vstr d14, [%0]"            "\n\t"
       : : /*IN*/"r"(&block[0]), "r"(x) : /*TRASH*/"r8","s28","d14","memory"
    );
    return block[0];
}

void try_s32_to_f64 ( int x )
{
  double f64 = s32_to_f64_imm32(x);
  printf("s32_to_f64_imm32:  %11d  ->  %18.14e\n", x, f64);
  f64 = s32_to_f64_imm1(x);
  printf("s32_to_f64_imm1:   %11d  ->  %18.14e\n", x, f64);
}



__attribute__((noinline)) double u32_to_f64_imm1(int x)
{
    double block[2];
    memset(block, 0x55, sizeof(block));
    __asm__ __volatile__(
       "mov r8, %1"               "\n\t"
       "vldr d14, [%0, #8]"       "\n\t" // d14 <- junk
       "vmov s28, r8"             "\n\t"
       "vcvt.f64.u32 d14,d14,#1"  "\n\t"
       "vstr d14, [%0]"           "\n\t"
       : : /*IN*/"r"(&block[0]), "r"(x) : /*TRASH*/"r8","s28","d14","memory"
    );
    return block[0];
}

__attribute__((noinline)) double u32_to_f64_imm32(int x)
{
    double block[2];
    memset(block, 0x55, sizeof(block));
    __asm__ __volatile__(
       "mov r8, %1"                "\n\t"
       "vldr d14, [%0, #8]"        "\n\t" // d14 <- junk
       "vmov s28, r8"              "\n\t"
       "vcvt.f64.u32 d14,d14,#32"  "\n\t"
       "vstr d14, [%0]"            "\n\t"
       : : /*IN*/"r"(&block[0]), "r"(x) : /*TRASH*/"r8","s28","d14","memory"
    );
    return block[0];
}

void try_u32_to_f64 ( int x )
{
  double f64 = u32_to_f64_imm32(x);
  printf("u32_to_f64_imm32:  %11d  ->  %18.14e\n", x, f64);
  f64 = u32_to_f64_imm1(x);
  printf("u32_to_f64_imm1:   %11d  ->  %18.14e\n", x, f64);
}



__attribute__((noinline)) ULong f64_to_s32_imm1 ( double d )
{
  double block[5];
  memset(block, 0x55, sizeof(block));
  block[1] = d;
  __asm__ __volatile__(
    "mov r8, %0"               "\n\t"
    "vldr d14, [r8, #8]"       "\n\t"
    "vcvt.s32.f64 d14,d14,#1"  "\n\t"
    "vstr d14, [r8,#24]"       "\n\t"
    : : /*IN*/"r"(&block[0]) : /*TRASH*/"d14","r8","memory"
  );
  return *(ULong*)(&block[3]);
}

__attribute__((noinline)) ULong f64_to_s32_imm32 ( double d )
{
  double block[5];
  memset(block, 0x55, sizeof(block));
  block[1] = d;
  __asm__ __volatile__(
    "mov r8, %0"                "\n\t"
    "vldr d14, [r8, #8]"        "\n\t"
    "vcvt.s32.f64 d14,d14,#32"  "\n\t"
    "vstr d14, [r8,#24]"        "\n\t"
    : : /*IN*/"r"(&block[0]) : /*TRASH*/"d14","r8","memory"
  );
  return *(ULong*)(&block[3]);
}

void try_f64_to_s32 ( double d )
{
  ULong res = f64_to_s32_imm32(d);
  printf("f64_to_s32_imm32:  %18.14e  ->  0x%016llx\n", d, res);
  res = f64_to_s32_imm1(d);
  printf("f64_to_s32_imm1:   %18.14e  ->  0x%016llx\n", d, res);
}



__attribute__((noinline)) ULong f64_to_u32_imm1 ( double d )
{
  double block[5];
  memset(block, 0x55, sizeof(block));
  block[1] = d;
  __asm__ __volatile__(
    "mov r8, %0"               "\n\t"
    "vldr d14, [r8, #8]"       "\n\t"
    "vcvt.u32.f64 d14,d14,#1"  "\n\t"
    "vstr d14, [r8,#24]"       "\n\t"
    : : /*IN*/"r"(&block[0]) : /*TRASH*/"d14","r8","memory"
  );
  return *(ULong*)(&block[3]);
}

__attribute__((noinline)) ULong f64_to_u32_imm32 ( double d )
{
  double block[5];
  memset(block, 0x55, sizeof(block));
  block[1] = d;
  __asm__ __volatile__(
    "mov r8, %0"                "\n\t"
    "vldr d14, [r8, #8]"        "\n\t"
    "vcvt.u32.f64 d14,d14,#32"  "\n\t"
    "vstr d14, [r8,#24]"        "\n\t"
    : : /*IN*/"r"(&block[0]) : /*TRASH*/"d14","r8","memory"
  );
  return *(ULong*)(&block[3]);
}

void try_f64_to_u32 ( double d )
{
  ULong res = f64_to_u32_imm32(d);
  printf("f64_to_u32_imm32:  %18.14e  ->  0x%016llx\n", d, res);
  res = f64_to_u32_imm1(d);
  printf("f64_to_u32_imm1:   %18.14e  ->  0x%016llx\n", d, res);
}



int main ( void  )
{
  int i;
  double d;

  try_s32_to_f32(0);
  try_s32_to_f32(1);
  for (i = 100; i < 200; i++) {
     try_s32_to_f32(i);
  }
  try_s32_to_f32(0x7FFFFFFE);
  try_s32_to_f32(0x7FFFFFFF);
  try_s32_to_f32(0x80000000);
  try_s32_to_f32(0x80000001);
  try_s32_to_f32(0xFFFFFFFE);
  try_s32_to_f32(0xFFFFFFFF);

  printf("\n");

  try_u32_to_f32(0);
  try_u32_to_f32(1);
  for (i = 100; i < 200; i++) {
     try_u32_to_f32(i);
  }
  try_u32_to_f32(0x7FFFFFFE);
  try_u32_to_f32(0x7FFFFFFF);
  try_u32_to_f32(0x80000000);
  try_u32_to_f32(0x80000001);
  try_u32_to_f32(0xFFFFFFFE);
  try_u32_to_f32(0xFFFFFFFF);

  printf("\n");

  try_s32_to_f64(0);
  try_s32_to_f64(1);
  for (i = 100; i < 200; i++) {
     try_s32_to_f64(i);
  }
  try_s32_to_f64(0x7FFFFFFE);
  try_s32_to_f64(0x7FFFFFFF);
  try_s32_to_f64(0x80000000);
  try_s32_to_f64(0x80000001);
  try_s32_to_f64(0xFFFFFFFE);
  try_s32_to_f64(0xFFFFFFFF);

  printf("\n");

  try_u32_to_f64(0);
  try_u32_to_f64(1);
  for (i = 100; i < 200; i++) {
     try_u32_to_f64(i);
  }
  try_u32_to_f64(0x7FFFFFFE);
  try_u32_to_f64(0x7FFFFFFF);
  try_u32_to_f64(0x80000000);
  try_u32_to_f64(0x80000001);
  try_u32_to_f64(0xFFFFFFFE);
  try_u32_to_f64(0xFFFFFFFF);

  printf("\n");
  try_f64_to_s32(0.0);
  try_f64_to_s32(1.0);
  try_f64_to_s32(-1.0);
  try_f64_to_s32(0.0 / 0.0);
  for (d = -100000.01; d < 100000.0; d += 10000.0) {
     try_f64_to_s32(d);
  }

  printf("\n");
  try_f64_to_u32(0.0);
  try_f64_to_u32(1.0);
  try_f64_to_u32(-1.0);
  try_f64_to_u32(0.0 / 0.0);
  for (d = -100000.01; d < 100000.0; d += 10000.0) {
     try_f64_to_u32(d);
  }

  return 0;
}
