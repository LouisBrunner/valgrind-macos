/* Hexadecimal Floating Point insns

   HFP is generally not supported with very few exceptions.

   Carved out of bfp-2.c with no modifications. */

#include <stdio.h>

void lder(double prev, float in)
{
   unsigned long out;

   __asm__ volatile("lder %[prev],%[in]\n\t"
                    "std %[prev],%[out]" :
                    [out]"=R"(out) : [prev]"f"(prev), [in]"f"(in));
   printf("lder  %f  -> %lx\n", in, out);
}

void lde(double prev, float in)
{
   unsigned long out;

   __asm__ volatile("lde %[prev],%[in]\n\t"
                    "std %[prev],%[out]" :
                    [out]"=R"(out) : [prev]"f"(prev), [in]"R"(in));
   printf("lde  %f  -> %lx\n", in, out);
}

int main(void)
{
   // load lengthened
   lder(0.2, 321.5f);
   lder(0.9, -8388607.f);
   lde(0.2, -321.5f);
   lde(0.9, 8388607.f);

   return 0;
}
   
