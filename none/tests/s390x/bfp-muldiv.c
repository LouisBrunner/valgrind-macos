#include <stdio.h>
#include <assert.h>

/* Test BFP multiply and add/sub  32/64-bit. There are no such insns
   working with 128-bit data.
   Also test division making sure operand order is preserved. */

void maebr(float v1, float v2, float v3)
{
   float r1 = v1;

   __asm__ volatile("maebr %[r1],%[r3],%[r2]"
                    : [r1]"+f"(r1) : [r2]"f"(v2), [r3]"f"(v3));
   printf("maebr  %f * %f + %f  -> %f\n", v2, v3, v1, r1);
}

void madbr(double v1, double v2, double v3)
{
   double r1 = v1;

   __asm__ volatile("madbr %[r1],%[r3],%[r2]"
                    : [r1]"+f"(r1) : [r2]"f"(v2), [r3]"f"(v3));
   printf("madbr  %f * %f + %f  -> %f\n", v2, v3, v1, r1);
}

void msebr(float v1, float v2, float v3)
{
   float r1 = v1;

   __asm__ volatile("msebr %[r1],%[r3],%[r2]"
                    : [r1]"+f"(r1) : [r2]"f"(v2), [r3]"f"(v3));
   printf("msebr  %f * %f - %f  -> %f\n", v2, v3, v1, r1);
}

void msdbr(double v1, double v2, double v3)
{
   double r1 = v1;

   __asm__ volatile("msdbr %[r1],%[r3],%[r2]"
                    : [r1]"+f"(r1) : [r2]"f"(v2), [r3]"f"(v3));
   printf("msdbr  %f * %f - %f  -> %f\n", v2, v3, v1, r1);
}

static void debr(float v1, float v2)
{
   float r1 = v1;

   __asm__ volatile("debr %[r1],%[r2]"
                    : [r1]"+f"(v1)
                    : [r2]"f"(v2));
   printf("debr  %f / %f  -> %f\n", r1, v2, v1);
}

static void ddbr(double v1, double v2)
{
   double r1 = v1;

   __asm__ volatile("ddbr %[r1],%[r2]"
                    : [r1]"+f"(v1)
                    : [r2]"f"(v2));
   printf("ddbr  %f / %f  -> %f\n", r1, v2, v1);
}

static void dxbr(long double v1, long double v2)
{
   long double r1 = v1;

   __asm__ volatile("dxbr %[r1],%[r2]"
                    : [r1]"+f"(v1)
                    : [r2]"f"(v2));
   printf("dxbr  %Lf / %Lf  -> %Lf\n", r1, v2, v1);
}

int main(void)
{
   assert(sizeof(long double) == 16);

   // multiply and add
   maebr(10.5f, 20.25, 3.0);  // 4 byte values
   madbr(-10.5, 42.75, -2.0); // 8 byte values

   // multiply and sub
   msebr(10.5f, 20.25, 3.0);  // 4 byte values
   msdbr(-10.5, 42.75, -2.0); // 8 byte values

   /* Just checking that the operand order is not mixed up. */
   debr(10.0f, 2.0f);
   ddbr(10.0, 2.0);
   dxbr(10.0L, 2.0L);

   return 0;
}
