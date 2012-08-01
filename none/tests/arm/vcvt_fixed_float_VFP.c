
#include <stdio.h>

__attribute__((noinline)) float s_to_f32_imm1(int x)
{
    float y;
    __asm__ ("vcvt.f32.s32 %0, %1, #1" : "=w"(y) : "0"(x));
    return y;
}

__attribute__((noinline)) float s_to_f32_imm32(int x)
{
    float y;
    __asm__ ("vcvt.f32.s32 %0, %1, #32" : "=w"(y) : "0"(x));
    return y;
}

void try_s_to_f32 ( int x )
{
  float f32 = s_to_f32_imm32(x);
  printf("s_to_f32_imm32:  %11d  ->  %18.14e\n", x, (double)f32);
  f32 = s_to_f32_imm1(x);
  printf("s_to_f32_imm1:   %11d  ->  %18.14e\n", x, (double)f32);
}



__attribute__((noinline)) float u_to_f32_imm1(int x)
{
    float y;
    __asm__ ("vcvt.f32.u32 %0, %1, #1" : "=w"(y) : "0"(x));
    return y;
}

__attribute__((noinline)) float u_to_f32_imm32(int x)
{
    float y;
    __asm__ ("vcvt.f32.u32 %0, %1, #32" : "=w"(y) : "0"(x));
    return y;
}

void try_u_to_f32 ( unsigned int x )
{
  float f32 = u_to_f32_imm32(x);
  printf("u_to_f32_imm32:  %11u  ->  %18.14e\n", x, (double)f32);
  f32 = u_to_f32_imm1(x);
  printf("u_to_f32_imm1:   %11u  ->  %18.14e\n", x, (double)f32);
}


//__attribute__((noinline)) double s_to_f64_imm1(int x)
//{
//    double y;
//    __asm__ ("vcvt.f64.s32 %P0, %1, #4" : "=w"(y) : "0"((long long)x));
//    return y;
//}




int main ( void  )
{
  int i;
  //float f = foo(1);
  //__asm__ __volatile__("" : : "r"(f) : "cc","memory");
  try_s_to_f32(0);
  try_s_to_f32(1);
  for (i = 100; i < 200; i++) {
     try_s_to_f32(i);
  }
  try_s_to_f32(0x7FFFFFFE);
  try_s_to_f32(0x7FFFFFFF);
  try_s_to_f32(0x80000000);
  try_s_to_f32(0x80000001);
  try_s_to_f32(0xFFFFFFFE);
  try_s_to_f32(0xFFFFFFFF);
  printf("\n");
  try_u_to_f32(0);
  try_u_to_f32(1);
  for (i = 100; i < 200; i++) {
     try_u_to_f32(i);
  }
  try_u_to_f32(0x7FFFFFFE);
  try_u_to_f32(0x7FFFFFFF);
  try_u_to_f32(0x80000000);
  try_u_to_f32(0x80000001);
  try_u_to_f32(0xFFFFFFFE);
  try_u_to_f32(0xFFFFFFFF);
  return 0;
}
