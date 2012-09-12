#include <stdio.h>

/* Simple test to see that basic operators are mapped
   correctly. Uses default rounding mode. */

volatile double d1, d2;
volatile float f1, f2;

void fadd8(void)
{
   printf("%f + %f = %f\n", d1, d2, d1 + d2);
}

void fsub8(void)
{
   printf("%f - %f = %f\n", d1, d2, d1 - d2);
}

void fmul8(void)
{
   printf("%f * %f = %f\n", d1, d2, d1 * d2);
}

void fdiv8(void)
{
   printf("%f / %f = %f\n", d1, d2, d1 / d2);
}

void fadd4(void)
{
   register float r1 asm("f1") = f1;
   register float r2 asm("f2") = f2;

   __asm__ volatile ("aebr %[r1],%[r2]\n\t"
                     : [r1] "+f"(r1)
                     : [r2] "f"(r2) : "cc");
   printf("%f + %f = %f\n", f1, f2, r1);
}

void fsub4(void)
{
   register float r1 asm("f1") = f1;
   register float r2 asm("f2") = f2;

   __asm__ volatile ("sebr %[r1],%[r2]\n\t"
                     : [r1] "+f"(r1)
                     : [r2] "f"(r2) : "cc");
   printf("%f - %f = %f\n", f1, f2, r1);
}

void fmul4(void)
{
   register float r1 asm("f1") = f1;
   register float r2 asm("f2") = f2;

   __asm__ volatile ("meebr %[r1],%[r2]\n\t"
                     : [r1] "+f"(r1)
                     : [r2] "f"(r2) : "cc");
   printf("%f * %f = %f\n", f1, f2, r1);
}

void fdiv4(void)
{
   register float r1 asm("f1") = f1;
   register float r2 asm("f2") = f2;

   __asm__ volatile ("debr %[r1],%[r2]\n\t"
                     : [r1] "+f"(r1)
                     : [r2] "f"(r2) : "cc");
   printf("%f / %f = %f\n", f1, f2, r1);
}


int main()
{
   printf("double arithmetic\n");
   d1 = 10.5;
   d2 =  1.25;
   fadd8();
   fsub8();
   fmul8();
   fdiv8();

   printf("float arithmetic\n");
   f1 = 10.5f;
   f2 =  1.25f;
   fadd4();
   fsub4();
   fmul4();
   fdiv4();

   return 0;
}
