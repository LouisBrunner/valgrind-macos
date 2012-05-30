#include <stdint.h>
#include <stdio.h>
#include <string.h>

void 
test(int32_t op1_init, int32_t op2_init, int32_t op3_init)
{
   register int32_t op1 asm("8") = op1_init;
   register int32_t op3 asm("9") = op3_init;
   
   int32_t op2 = op2_init;
   int cc = 1; 

   __asm__ volatile (
           "cs      8,9,%1\n\t"
           "ipm     %0\n\t"
           "srl     %0,28\n\t"
           : "=d" (cc), "+Q" (op2), "+d"(op1), "+d"(op3)
           : 
           : "cc");
}

int main ()
{
   int op1, op2, op3;

   test(op1, 0x10000000, 0x12345678);   // complaint
   test(0x10000000, op2, 0x12345678);   // complaint
   test(0x10000000, 0x01000000, op3);   // no complaint

   return 0;
}
