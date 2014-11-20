
#include <stdio.h>
#include <assert.h>

typedef    signed long long int  Long;
typedef  unsigned long long int  ULong;

typedef    signed int  Int;
typedef  unsigned int  UInt;

typedef  union { double d64; float f32[2]; unsigned long long int i64; }  U;

//////////////////// D from X ////////////////////

__attribute__((noinline)) double do_scvtf_d_x_imm1 ( Long x )
{
   U block[2]; block[0].i64 = x; block[1].i64 = 0;
   __asm__ __volatile__(
      "ldr x13,[%0,#0];  scvtf d18,x13,#1;  str d18, [%0,#8]"
      ::"r"(&block[0]) : "memory", "x13","q18");
   return block[1].d64;
}
__attribute__((noinline)) double do_scvtf_d_x_imm32 ( Long x )
{
   U block[2]; block[0].i64 = x; block[1].i64 = 0;
   __asm__ __volatile__(
      "ldr x13,[%0,#0];  scvtf d18,x13,#32;  str d18, [%0,#8]"
      ::"r"(&block[0]) : "memory", "x13","q18");
   return block[1].d64;
}
__attribute__((noinline)) double do_scvtf_d_x_imm64 ( Long x )
{
   U block[2]; block[0].i64 = x; block[1].i64 = 0;
   __asm__ __volatile__(
      "ldr x13,[%0,#0];  scvtf d18,x13,#64;  str d18, [%0,#8]"
      ::"r"(&block[0]) : "memory", "x13","q18");
   return block[1].d64;
}

__attribute__((noinline)) double do_ucvtf_d_x_imm1 ( Long x )
{
   U block[2]; block[0].i64 = x; block[1].i64 = 0;
   __asm__ __volatile__(
      "ldr x13,[%0,#0];  ucvtf d18,x13,#1;  str d18, [%0,#8]"
      ::"r"(&block[0]) : "memory", "x13","q18");
   return block[1].d64;
}
__attribute__((noinline)) double do_ucvtf_d_x_imm32 ( Long x )
{
   U block[2]; block[0].i64 = x; block[1].i64 = 0;
   __asm__ __volatile__(
      "ldr x13,[%0,#0];  ucvtf d18,x13,#32;  str d18, [%0,#8]"
      ::"r"(&block[0]) : "memory", "x13","q18");
   return block[1].d64;
}
__attribute__((noinline)) double do_ucvtf_d_x_imm64 ( Long x )
{
   U block[2]; block[0].i64 = x; block[1].i64 = 0;
   __asm__ __volatile__(
      "ldr x13,[%0,#0];  ucvtf d18,x13,#64;  str d18, [%0,#8]"
      ::"r"(&block[0]) : "memory", "x13","q18");
   return block[1].d64;
}


//////////////////// D from W ////////////////////

__attribute__((noinline)) double do_scvtf_d_w_imm1 ( Int x )
{
   U block[2]; block[0].i64 = (Long)x; block[1].i64 = 0;
   __asm__ __volatile__(
      "ldr x13,[%0,#0];  scvtf d18,w13,#1;  str d18, [%0,#8]"
      ::"r"(&block[0]) : "memory", "x13","q18");
   return block[1].d64;
}
__attribute__((noinline)) double do_scvtf_d_w_imm16 ( Int x )
{
   U block[2]; block[0].i64 = (Long)x; block[1].i64 = 0;
   __asm__ __volatile__(
      "ldr x13,[%0,#0];  scvtf d18,w13,#16;  str d18, [%0,#8]"
      ::"r"(&block[0]) : "memory", "x13","q18");
   return block[1].d64;
}
__attribute__((noinline)) double do_scvtf_d_w_imm32 ( Int x )
{
   U block[2]; block[0].i64 = (Long)x; block[1].i64 = 0;
   __asm__ __volatile__(
      "ldr x13,[%0,#0];  scvtf d18,w13,#32;  str d18, [%0,#8]"
      ::"r"(&block[0]) : "memory", "x13","q18");
   return block[1].d64;
}

__attribute__((noinline)) double do_ucvtf_d_w_imm1 ( Int x )
{
   U block[2]; block[0].i64 = (Long)x; block[1].i64 = 0;
   __asm__ __volatile__(
      "ldr x13,[%0,#0];  ucvtf d18,w13,#1;  str d18, [%0,#8]"
      ::"r"(&block[0]) : "memory", "x13","q18");
   return block[1].d64;
}
__attribute__((noinline)) double do_ucvtf_d_w_imm16 ( Int x )
{
   U block[2]; block[0].i64 = (Long)x; block[1].i64 = 0;
   __asm__ __volatile__(
      "ldr x13,[%0,#0];  ucvtf d18,w13,#16;  str d18, [%0,#8]"
      ::"r"(&block[0]) : "memory", "x13","q18");
   return block[1].d64;
}
__attribute__((noinline)) double do_ucvtf_d_w_imm32 ( Int x )
{
   U block[2]; block[0].i64 = (Long)x; block[1].i64 = 0;
   __asm__ __volatile__(
      "ldr x13,[%0,#0];  ucvtf d18,w13,#32;  str d18, [%0,#8]"
      ::"r"(&block[0]) : "memory", "x13","q18");
   return block[1].d64;
}


//////////////////// S from X ////////////////////

__attribute__((noinline)) double do_scvtf_s_x_imm1 ( Long x )
{
   U block[2]; block[0].i64 = x; block[1].i64 = 0;
   __asm__ __volatile__(
      "ldr x13,[%0,#0];  scvtf s18,x13,#1;  str s18, [%0,#8]"
      ::"r"(&block[0]) : "memory", "x13","q18");
   return (double)block[1].f32[0];
}
__attribute__((noinline)) double do_scvtf_s_x_imm32 ( Long x )
{
   U block[2]; block[0].i64 = x; block[1].i64 = 0;
   __asm__ __volatile__(
      "ldr x13,[%0,#0];  scvtf s18,x13,#32;  str s18, [%0,#8]"
      ::"r"(&block[0]) : "memory", "x13","q18");
   return (double)block[1].f32[0];
}
__attribute__((noinline)) double do_scvtf_s_x_imm64 ( Long x )
{
   U block[2]; block[0].i64 = x; block[1].i64 = 0;
   __asm__ __volatile__(
      "ldr x13,[%0,#0];  scvtf s18,x13,#64;  str s18, [%0,#8]"
      ::"r"(&block[0]) : "memory", "x13","q18");
   return (double)block[1].f32[0];
}

__attribute__((noinline)) double do_ucvtf_s_x_imm1 ( Long x )
{
   U block[2]; block[0].i64 = x; block[1].i64 = 0;
   __asm__ __volatile__(
      "ldr x13,[%0,#0];  ucvtf s18,x13,#1;  str s18, [%0,#8]"
      ::"r"(&block[0]) : "memory", "x13","q18");
   return (double)block[1].f32[0];
}
__attribute__((noinline)) double do_ucvtf_s_x_imm32 ( Long x )
{
   U block[2]; block[0].i64 = x; block[1].i64 = 0;
   __asm__ __volatile__(
      "ldr x13,[%0,#0];  ucvtf s18,x13,#32;  str s18, [%0,#8]"
      ::"r"(&block[0]) : "memory", "x13","q18");
   return (double)block[1].f32[0];
}
__attribute__((noinline)) double do_ucvtf_s_x_imm64 ( Long x )
{
   U block[2]; block[0].i64 = x; block[1].i64 = 0;
   __asm__ __volatile__(
      "ldr x13,[%0,#0];  ucvtf s18,x13,#64;  str s18, [%0,#8]"
      ::"r"(&block[0]) : "memory", "x13","q18");
   return (double)block[1].f32[0];
}


//////////////////// S from W ////////////////////

__attribute__((noinline)) double do_scvtf_s_w_imm1 ( Int x )
{
   U block[2]; block[0].i64 = (Long)x; block[1].i64 = 0;
   __asm__ __volatile__(
      "ldr x13,[%0,#0];  scvtf s18,w13,#1;  str s18, [%0,#8]"
      ::"r"(&block[0]) : "memory", "x13","q18");
   return (double)block[1].f32[0];
}
__attribute__((noinline)) double do_scvtf_s_w_imm16 ( Int x )
{
   U block[2]; block[0].i64 = (Long)x; block[1].i64 = 0;
   __asm__ __volatile__(
      "ldr x13,[%0,#0];  scvtf s18,w13,#16;  str s18, [%0,#8]"
      ::"r"(&block[0]) : "memory", "x13","q18");
   return (double)block[1].f32[0];
}
__attribute__((noinline)) double do_scvtf_s_w_imm32 ( Int x )
{
   U block[2]; block[0].i64 = (Long)x; block[1].i64 = 0;
   __asm__ __volatile__(
      "ldr x13,[%0,#0];  scvtf s18,w13,#32;  str s18, [%0,#8]"
      ::"r"(&block[0]) : "memory", "x13","q18");
   return (double)block[1].f32[0];
}

__attribute__((noinline)) double do_ucvtf_s_w_imm1 ( Int x )
{
   U block[2]; block[0].i64 = (Long)x; block[1].i64 = 0;
   __asm__ __volatile__(
      "ldr x13,[%0,#0];  ucvtf s18,w13,#1;  str s18, [%0,#8]"
      ::"r"(&block[0]) : "memory", "x13","q18");
   return (double)block[1].f32[0];
}
__attribute__((noinline)) double do_ucvtf_s_w_imm16 ( Int x )
{
   U block[2]; block[0].i64 = (Long)x; block[1].i64 = 0;
   __asm__ __volatile__(
      "ldr x13,[%0,#0];  ucvtf s18,w13,#16;  str s18, [%0,#8]"
      ::"r"(&block[0]) : "memory", "x13","q18");
   return (double)block[1].f32[0];
}
__attribute__((noinline)) double do_ucvtf_s_w_imm32 ( Int x )
{
   U block[2]; block[0].i64 = (Long)x; block[1].i64 = 0;
   __asm__ __volatile__(
      "ldr x13,[%0,#0];  ucvtf s18,w13,#32;  str s18, [%0,#8]"
      ::"r"(&block[0]) : "memory", "x13","q18");
   return (double)block[1].f32[0];
}





int main ( void )
{
  assert(sizeof(U) == 8);

  //////////////////// D from X ////////////////////
#if 1
  printf("\nscvtf_d_x_imm1\n");
  printf("%18.12e\n", do_scvtf_d_x_imm1(0xFFFFFFFFFFFFFFFFUL));
  printf("%18.12e\n", do_scvtf_d_x_imm1(0));
  printf("%18.12e\n", do_scvtf_d_x_imm1(0x7FFFFFFFFFFFFFFFUL));
  printf("%18.12e\n", do_scvtf_d_x_imm1(1234));
  printf("%18.12e\n", do_scvtf_d_x_imm1(-1234));
  printf("%18.12e\n", do_scvtf_d_x_imm1(0x8000000000000000UL));

  printf("\nscvtf_d_x_imm32\n");
  printf("%18.12e\n", do_scvtf_d_x_imm32(0xFFFFFFFFFFFFFFFFUL));
  printf("%18.12e\n", do_scvtf_d_x_imm32(0));
  printf("%18.12e\n", do_scvtf_d_x_imm32(0x7FFFFFFFFFFFFFFFUL));
  printf("%18.12e\n", do_scvtf_d_x_imm32(1234));
  printf("%18.12e\n", do_scvtf_d_x_imm32(-1234));
  printf("%18.12e\n", do_scvtf_d_x_imm32(0x8000000000000000UL));

  printf("\nscvtf_d_x_imm64\n");
  printf("%18.12e\n", do_scvtf_d_x_imm64(0xFFFFFFFFFFFFFFFFUL));
  printf("%18.12e\n", do_scvtf_d_x_imm64(0));
  printf("%18.12e\n", do_scvtf_d_x_imm64(0x7FFFFFFFFFFFFFFFUL));
  printf("%18.12e\n", do_scvtf_d_x_imm64(1234));
  printf("%18.12e\n", do_scvtf_d_x_imm64(-1234));
  printf("%18.12e\n", do_scvtf_d_x_imm64(0x8000000000000000UL));

  printf("\nucvtf_d_x_imm1\n");
  printf("%18.12e\n", do_ucvtf_d_x_imm1(0xFFFFFFFFFFFFFFFFUL));
  printf("%18.12e\n", do_ucvtf_d_x_imm1(0));
  printf("%18.12e\n", do_ucvtf_d_x_imm1(0x7FFFFFFFFFFFFFFFUL));
  printf("%18.12e\n", do_ucvtf_d_x_imm1(1234));
  printf("%18.12e\n", do_ucvtf_d_x_imm1(-1234));
  printf("%18.12e\n", do_ucvtf_d_x_imm1(0x8000000000000000UL));

  printf("\nucvtf_d_x_imm32\n");
  printf("%18.12e\n", do_ucvtf_d_x_imm32(0xFFFFFFFFFFFFFFFFUL));
  printf("%18.12e\n", do_ucvtf_d_x_imm32(0));
  printf("%18.12e\n", do_ucvtf_d_x_imm32(0x7FFFFFFFFFFFFFFFUL));
  printf("%18.12e\n", do_ucvtf_d_x_imm32(1234));
  printf("%18.12e\n", do_ucvtf_d_x_imm32(-1234));
  printf("%18.12e\n", do_ucvtf_d_x_imm32(0x8000000000000000UL));

  printf("\nucvtf_d_x_imm64\n");
  printf("%18.12e\n", do_ucvtf_d_x_imm64(0xFFFFFFFFFFFFFFFFUL));
  printf("%18.12e\n", do_ucvtf_d_x_imm64(0));
  printf("%18.12e\n", do_ucvtf_d_x_imm64(0x7FFFFFFFFFFFFFFFUL));
  printf("%18.12e\n", do_ucvtf_d_x_imm64(1234));
  printf("%18.12e\n", do_ucvtf_d_x_imm64(-1234));
  printf("%18.12e\n", do_ucvtf_d_x_imm64(0x8000000000000000UL));

  //////////////////// D from W ////////////////////

  printf("\nscvtf_d_w_imm1\n");
  printf("%18.12e\n", do_scvtf_d_w_imm1(0xFFFFFFFF));
  printf("%18.12e\n", do_scvtf_d_w_imm1(0));
  printf("%18.12e\n", do_scvtf_d_w_imm1(0x7FFFFFFF));
  printf("%18.12e\n", do_scvtf_d_w_imm1(1234));
  printf("%18.12e\n", do_scvtf_d_w_imm1(-1234));
  printf("%18.12e\n", do_scvtf_d_w_imm1(0x80000000));

  printf("\nscvtf_d_w_imm16\n");
  printf("%18.12e\n", do_scvtf_d_w_imm16(0xFFFFFFFF));
  printf("%18.12e\n", do_scvtf_d_w_imm16(0));
  printf("%18.12e\n", do_scvtf_d_w_imm16(0x7FFFFFFF));
  printf("%18.12e\n", do_scvtf_d_w_imm16(1234));
  printf("%18.12e\n", do_scvtf_d_w_imm16(-1234));
  printf("%18.12e\n", do_scvtf_d_w_imm16(0x80000000));

  printf("\nscvtf_d_w_imm32\n");
  printf("%18.12e\n", do_scvtf_d_w_imm32(0xFFFFFFFF));
  printf("%18.12e\n", do_scvtf_d_w_imm32(0));
  printf("%18.12e\n", do_scvtf_d_w_imm32(0x7FFFFFFF));
  printf("%18.12e\n", do_scvtf_d_w_imm32(1234));
  printf("%18.12e\n", do_scvtf_d_w_imm32(-1234));
  printf("%18.12e\n", do_scvtf_d_w_imm32(0x80000000));

  printf("\nucvtf_d_w_imm1\n");
  printf("%18.12e\n", do_ucvtf_d_w_imm1(0xFFFFFFFF));
  printf("%18.12e\n", do_ucvtf_d_w_imm1(0));
  printf("%18.12e\n", do_ucvtf_d_w_imm1(0x7FFFFFFF));
  printf("%18.12e\n", do_ucvtf_d_w_imm1(1234));
  printf("%18.12e\n", do_ucvtf_d_w_imm1(-1234));
  printf("%18.12e\n", do_ucvtf_d_w_imm1(0x80000000));

  printf("\nucvtf_d_w_imm16\n");
  printf("%18.12e\n", do_ucvtf_d_w_imm16(0xFFFFFFFF));
  printf("%18.12e\n", do_ucvtf_d_w_imm16(0));
  printf("%18.12e\n", do_ucvtf_d_w_imm16(0x7FFFFFFF));
  printf("%18.12e\n", do_ucvtf_d_w_imm16(1234));
  printf("%18.12e\n", do_ucvtf_d_w_imm16(-1234));
  printf("%18.12e\n", do_ucvtf_d_w_imm16(0x80000000));

  printf("\nucvtf_d_w_imm32\n");
  printf("%18.12e\n", do_ucvtf_d_w_imm32(0xFFFFFFFF));
  printf("%18.12e\n", do_ucvtf_d_w_imm32(0));
  printf("%18.12e\n", do_ucvtf_d_w_imm32(0x7FFFFFFF));
  printf("%18.12e\n", do_ucvtf_d_w_imm32(1234));
  printf("%18.12e\n", do_ucvtf_d_w_imm32(-1234));
  printf("%18.12e\n", do_ucvtf_d_w_imm32(0x80000000));

  //////////////////// S from X ////////////////////

  printf("\nscvtf_s_x_imm1\n");
  printf("%18.12e\n", do_scvtf_s_x_imm1(0xFFFFFFFFFFFFFFFFUL));
  printf("%18.12e\n", do_scvtf_s_x_imm1(0));
  printf("%18.12e\n", do_scvtf_s_x_imm1(0x7FFFFFFFFFFFFFFFUL));
  printf("%18.12e\n", do_scvtf_s_x_imm1(1234));
  printf("%18.12e\n", do_scvtf_s_x_imm1(-1234));
  printf("%18.12e\n", do_scvtf_s_x_imm1(0x8000000000000000UL));

  printf("\nscvtf_s_x_imm32\n");
  printf("%18.12e\n", do_scvtf_s_x_imm32(0xFFFFFFFFFFFFFFFFUL));
  printf("%18.12e\n", do_scvtf_s_x_imm32(0));
  printf("%18.12e\n", do_scvtf_s_x_imm32(0x7FFFFFFFFFFFFFFFUL));
  printf("%18.12e\n", do_scvtf_s_x_imm32(1234));
  printf("%18.12e\n", do_scvtf_s_x_imm32(-1234));
  printf("%18.12e\n", do_scvtf_s_x_imm32(0x8000000000000000UL));

  printf("\nscvtf_s_x_imm64\n");
  printf("%18.12e\n", do_scvtf_s_x_imm64(0xFFFFFFFFFFFFFFFFUL));
  printf("%18.12e\n", do_scvtf_s_x_imm64(0));
  printf("%18.12e\n", do_scvtf_s_x_imm64(0x7FFFFFFFFFFFFFFFUL));
  printf("%18.12e\n", do_scvtf_s_x_imm64(1234));
  printf("%18.12e\n", do_scvtf_s_x_imm64(-1234));
  printf("%18.12e\n", do_scvtf_s_x_imm64(0x8000000000000000UL));

  printf("\nucvtf_s_x_imm1\n");
  printf("%18.12e\n", do_ucvtf_s_x_imm1(0xFFFFFFFFFFFFFFFFUL));
  printf("%18.12e\n", do_ucvtf_s_x_imm1(0));
  printf("%18.12e\n", do_ucvtf_s_x_imm1(0x7FFFFFFFFFFFFFFFUL));
  printf("%18.12e\n", do_ucvtf_s_x_imm1(1234));
  printf("%18.12e\n", do_ucvtf_s_x_imm1(-1234));
  printf("%18.12e\n", do_ucvtf_s_x_imm1(0x8000000000000000UL));

  printf("\nucvtf_s_x_imm32\n");
  printf("%18.12e\n", do_ucvtf_s_x_imm32(0xFFFFFFFFFFFFFFFFUL));
  printf("%18.12e\n", do_ucvtf_s_x_imm32(0));
  printf("%18.12e\n", do_ucvtf_s_x_imm32(0x7FFFFFFFFFFFFFFFUL));
  printf("%18.12e\n", do_ucvtf_s_x_imm32(1234));
  printf("%18.12e\n", do_ucvtf_s_x_imm32(-1234));
  printf("%18.12e\n", do_ucvtf_s_x_imm32(0x8000000000000000UL));

  printf("\nucvtf_s_x_imm64\n");
  printf("%18.12e\n", do_ucvtf_s_x_imm64(0xFFFFFFFFFFFFFFFFUL));
  printf("%18.12e\n", do_ucvtf_s_x_imm64(0));
  printf("%18.12e\n", do_ucvtf_s_x_imm64(0x7FFFFFFFFFFFFFFFUL));
  printf("%18.12e\n", do_ucvtf_s_x_imm64(1234));
  printf("%18.12e\n", do_ucvtf_s_x_imm64(-1234));
  printf("%18.12e\n", do_ucvtf_s_x_imm64(0x8000000000000000UL));

  //////////////////// S from W ////////////////////

  printf("\nscvtf_s_w_imm1\n");
  printf("%18.12e\n", do_scvtf_s_w_imm1(0xFFFFFFFF));
  printf("%18.12e\n", do_scvtf_s_w_imm1(0));
  printf("%18.12e\n", do_scvtf_s_w_imm1(0x7FFFFFFF));
  printf("%18.12e\n", do_scvtf_s_w_imm1(1234));
  printf("%18.12e\n", do_scvtf_s_w_imm1(-1234));
  printf("%18.12e\n", do_scvtf_s_w_imm1(0x80000000));

  printf("\nscvtf_s_w_imm16\n");
  printf("%18.12e\n", do_scvtf_s_w_imm16(0xFFFFFFFF));
  printf("%18.12e\n", do_scvtf_s_w_imm16(0));
  printf("%18.12e\n", do_scvtf_s_w_imm16(0x7FFFFFFF));
  printf("%18.12e\n", do_scvtf_s_w_imm16(1234));
  printf("%18.12e\n", do_scvtf_s_w_imm16(-1234));
  printf("%18.12e\n", do_scvtf_s_w_imm16(0x80000000));

  printf("\nscvtf_s_w_imm32\n");
  printf("%18.12e\n", do_scvtf_s_w_imm32(0xFFFFFFFF));
  printf("%18.12e\n", do_scvtf_s_w_imm32(0));
  printf("%18.12e\n", do_scvtf_s_w_imm32(0x7FFFFFFF));
  printf("%18.12e\n", do_scvtf_s_w_imm32(1234));
  printf("%18.12e\n", do_scvtf_s_w_imm32(-1234));
  printf("%18.12e\n", do_scvtf_s_w_imm32(0x80000000));

  printf("\nucvtf_s_w_imm1\n");
  printf("%18.12e\n", do_ucvtf_s_w_imm1(0xFFFFFFFF));
  printf("%18.12e\n", do_ucvtf_s_w_imm1(0));
  printf("%18.12e\n", do_ucvtf_s_w_imm1(0x7FFFFFFF));
  printf("%18.12e\n", do_ucvtf_s_w_imm1(1234));
  printf("%18.12e\n", do_ucvtf_s_w_imm1(-1234));
  printf("%18.12e\n", do_ucvtf_s_w_imm1(0x80000000));

  printf("\nucvtf_s_w_imm16\n");
  printf("%18.12e\n", do_ucvtf_s_w_imm16(0xFFFFFFFF));
  printf("%18.12e\n", do_ucvtf_s_w_imm16(0));
  printf("%18.12e\n", do_ucvtf_s_w_imm16(0x7FFFFFFF));
  printf("%18.12e\n", do_ucvtf_s_w_imm16(1234));
  printf("%18.12e\n", do_ucvtf_s_w_imm16(-1234));
  printf("%18.12e\n", do_ucvtf_s_w_imm16(0x80000000));

  printf("\nucvtf_s_w_imm32\n");
  printf("%18.12e\n", do_ucvtf_s_w_imm32(0xFFFFFFFF));
  printf("%18.12e\n", do_ucvtf_s_w_imm32(0));
  printf("%18.12e\n", do_ucvtf_s_w_imm32(0x7FFFFFFF));
  printf("%18.12e\n", do_ucvtf_s_w_imm32(1234));
  printf("%18.12e\n", do_ucvtf_s_w_imm32(-1234));
#endif
  printf("%18.12e\n", do_ucvtf_s_w_imm32(0x80000000));




#if 0
int i;
double d = -4.90;
for (i = 0; i < 100; i++) {
   printf("frintx_d(%f) = %f\n", d, do_frintx_d(d));
   d += 0.1 * (1.0 - 1.0 / 30.0);
}

float f = -4.90;
for (i = 0; i < 100; i++) {
   printf("frintx_s(%f) = %f\n", f, do_frintx_s(f));
   f += 0.1 * (1.0 - 1.0 / 30.0);
}
#endif

  return 0;
}
