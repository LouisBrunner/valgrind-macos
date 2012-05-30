#include <stdint.h>
#include <stdio.h>
#include <string.h>

void 
test(int64_t op1_init, int64_t op2_init, int64_t op3_init)
{
   register int64_t op1 asm("8") = op1_init;
   register int64_t op3 asm("9") = op3_init;
   
   int64_t op2 = op2_init;
   int cc = 1; 

   __asm__ volatile (
           "csg     8,9,%1\n\t"
           "ipm     %0\n\t"
           "srl     %0,28\n\t"
           : "=d" (cc), "+Q" (op2), "+d"(op1), "+d"(op3)
           : 
           : "cc");
}

int main ()
{
   int64_t op1, op2, op3;

   test(op1, 0x1000000000000000ull, 0x1234567887654321ull);  // complaint
   test(0x1000000000000000ull, op2, 0x1234567887654321ull);  // complaint
   test(0x1000000000000000ull, 0x1000000000000000ull, op3);  // no complaint

   return 0;
}
