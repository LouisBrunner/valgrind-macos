#include <stdint.h>
#include <stdio.h>
#include <string.h>

void 
test(int32_t op1_init, int32_t op2_init, int32_t op3_init, int expected_cc)
{
   register int32_t op1 asm("8") = op1_init;
   register int32_t op3 asm("9") = op3_init;
   
   int32_t op2 = op2_init;
   int cc = 1 - expected_cc; 

   printf("before op1 = %#x\n", op1);
   printf("before op2 = %#x\n", op2);
   printf("before op3 = %#x\n", op3);

   __asm__ volatile (
           "cs      8,9,%1\n\t"
           "ipm     %0\n\t"
           "srl     %0,28\n\t"
           : "=d" (cc), "+Q" (op2), "+d"(op1), "+d"(op3)
           : 
           : "cc");

   printf("after  op1 = %#x\n", op1);
   printf("after  op2 = %#x\n", op2);
   printf("after  op3 = %#x\n", op3);
   printf("cc = %d\n", cc);

   if (cc != expected_cc) {
      printf("condition code is incorrect\n");
   }
   if (expected_cc == 0) {
      if (op2 != op3) {
         printf("operand #2 not updated\n");
      }
   } else {
      if (op1 != op2) {
         printf("operand #1 not updated\n");
      }
   }
}

int main ()
{
   test(0x10000000, 0x10000000, 0x12345678, 0);
   test(0x10000000, 0x20000000, 0x12345678, 1);

   return 0;
}
