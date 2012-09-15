#include <stdio.h>

/* Test BFP comparison for  32/64-bit. */

void cebr(float v1, float v2)
{
   int cc;

   __asm__ volatile("cebr %[r1],%[r2]\n\t"
                    "ipm   %[psw]\n\t"
                    "srl   %[psw],28\n\t"
                    : [psw]"=d"(cc) : [r1]"f"(v1), [r2]"f"(v2) : "cc");
   if (cc == 0)
      printf("cfebr:  %f == %f\n", v1, v2);
   if (cc == 1)
      printf("cfebr:  %f < %f\n", v1, v2);
   if (cc == 2)
      printf("cfebr:  %f > %f\n", v1, v2);
}

void cdbr(double v1, double v2)
{
   int cc;

   __asm__ volatile("cdbr %[r1],%[r2]\n\t"
                    "ipm   %[psw]\n\t"
                    "srl   %[psw],28\n\t"
                    : [psw]"=d"(cc) : [r1]"f"(v1), [r2]"f"(v2) : "cc");
   if (cc == 0)
      printf("cdebr:  %f == %f\n", v1, v2);
   if (cc == 1)
      printf("cdebr:  %f < %f\n", v1, v2);
   if (cc == 2)
      printf("cdebr:  %f > %f\n", v1, v2);
}

int main(void)
{
   float f1, f2;
   float d1, d2;

   // compare 4 bytes
   f1 = 3.14f;
   f2 = f1;
   cebr(f1, f2);
   f2 = f1 + 10.;
   cebr(f1, f2);
   f2 = f1 - 100.;
   cebr(f1, f2);

   // compare 8 bytes
   d1 = 2.78;
   d2 = d1;
   cdbr(d1, d2);
   d2 = d1 + 10.;
   cdbr(d1, d2);
   d2 = d1 - 100.;
   cdbr(d1, d2);

   return 0;
}
