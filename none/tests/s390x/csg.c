#include <stdint.h>
#include <stdio.h>
#include <string.h>

void 
test(int64_t op1_init, int64_t op2_init, int64_t op3_init, int expected_cc)
{
   register int64_t op1 asm("8") = op1_init;
   register int64_t op3 asm("9") = op3_init;
   
   int64_t op2 = op2_init;
   int cc = 1 - expected_cc; 

   printf("before op1 = %#lx\n", op1);
   printf("before op2 = %#lx\n", op2);
   printf("before op3 = %#lx\n", op3);

   __asm__ volatile (
           "csg     8,9,%1\n\t"
           "ipm     %0\n\t"
           "srl     %0,28\n\t"
           : "=d" (cc), "+Q" (op2), "+d"(op1), "+d"(op3)
           : 
           : "cc");

   printf("after  op1 = %#lx\n", op1);
   printf("after  op2 = %#lx\n", op2);
   printf("after  op3 = %#lx\n", op3);
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
   test(0x1000000000000000ull, 0x1000000000000000ull, 0x1234567887654321ull, 0);
   test(0x1000000000000000ull, 0x2000000000000000ull, 0x1234567887654321ull, 1);

   return 0;
}
