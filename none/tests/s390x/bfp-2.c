#include <stdio.h>

/* Test various BFP ops:
   - load negative
   - load positive
   - load complement
*/

void lnebr(float in)
{
   float out;

   __asm__ volatile("lnebr %[out],%[in]" : [out]"=f"(out) : [in]"f"(in) : "cc");
   printf("lnebr  %f  -> %f\n", in, out);
}

void lndbr(double in)
{
   double out;

   __asm__ volatile("lndbr %[out],%[in]" : [out]"=f"(out) : [in]"f"(in) : "cc");
   printf("lndbr  %f  -> %f\n", in, out);
}

void lpebr(float in)
{
   float out;

   __asm__ volatile("lpebr %[out],%[in]" : [out]"=f"(out) : [in]"f"(in) : "cc");
   printf("lpebr  %f  -> %f\n", in, out);
}

void lpdbr(double in)
{
   double out;

   __asm__ volatile("lpdbr %[out],%[in]" : [out]"=f"(out) : [in]"f"(in) : "cc");
   printf("lpdbr  %f  -> %f\n", in, out);
}

void lcebr(float in)
{
   float out;

   __asm__ volatile("lcebr %[out],%[in]" : [out]"=f"(out) : [in]"f"(in) : "cc");
   printf("lcebr  %f  -> %f\n", in, out);
}

void lcdbr(double in)
{
   double out;

   __asm__ volatile("lcdbr %[out],%[in]" : [out]"=f"(out) : [in]"f"(in) : "cc");
   printf("lcdbr  %f  -> %f\n", in, out);
}

int main(void)
{
   // load negative
   lnebr(-2.5f);   // 4 byte values
   lnebr(12.5f);   // 4 byte values

   lndbr(-0.5);    // 8 byte values
   lndbr(42.5);    // 8 byte values

   // load positive
   lpebr(-2.5f);   // 4 byte values
   lpebr(12.5f);   // 4 byte values

   lpdbr(-0.5);    // 8 byte values
   lpdbr(42.5);    // 8 byte values

   // load complement
   lcebr(-23.5f);  // 4 byte values
   lcebr(123.5f);  // 4 byte values

   lcdbr(-17.5);   // 8 byte values
   lcdbr(234.5);   // 8 byte values

   return 0;
}
