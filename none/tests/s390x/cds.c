#include <stdint.h>
#include <stdio.h>

typedef struct {
   uint64_t high;
   uint64_t low;
} quad_word;

void 
test(quad_word op1_init, uint64_t op2_init, quad_word op3_init,
     int expected_cc)
{
   int cc = 1 - expected_cc;

   quad_word op1 = op1_init;
   uint64_t  op2 = op2_init;
   quad_word op3 = op3_init;

   quad_word op1_before = op1;
   uint64_t  op2_before = op2;
   quad_word op3_before = op3;

   printf("before op1 = (%#lx, %#lx)\n", op1.high, op1.low);
   printf("before op2 =  %#lx\n", op2);
   printf("before op3 = (%#lx, %#lx)\n", op3.high, op3.low);

   __asm__ volatile (
                     "lmg     %%r0,%%r1,%1\n\t"
                     "lmg     %%r2,%%r3,%3\n\t"
                     "cds     %%r0,%%r2,%2\n\t"  //  cds 1st,3rd,2nd
                     "stmg    %%r0,%%r1,%1\n"    // store r0,r1 to op1
                     "stmg    %%r2,%%r3,%3\n"    // store r2,r3 to op3
                     "ipm     %0\n\t"
                     "srl     %0,28\n\t"
                     : "=d" (cc), "+QS" (op1), "+QS" (op2), "+QS" (op3)
                     :
                     : "r0", "r1", "r2", "r3", "cc");

   printf("after  op1 = (%#lx, %#lx)\n", op1.high, op1.low);
   printf("after  op2 = %#lx\n", op2);
   printf("after  op3 = (%#lx, %#lx)\n", op3.high, op3.low);
   printf("cc = %d\n", cc);

   // Check the condition code
   if (cc != expected_cc) {
      printf("condition code is incorrect\n");
   }

   // op3 never changes
   if (op3.low != op3_before.low || op3.high != op3_before.high) {
      printf("operand #3 modified\n");
   }

   if (expected_cc == 0) {
      // 3rd operand stored at 2nd operand location

      // op1 did not change
      if (op1.low != op1_before.low || op1.high != op1_before.high) {
         printf("operand #1 modified\n");
      }

      // lower 32 bits of op2 are the lower 32 bits of op3.low
      if ((op2 & 0xffffffff) != (op3.low & 0xffffffff)) {
         printf("operand #2 [32:63] incorrect\n");
      }
      // higher 32 bits of op2 are the lower 32 bits of op3.high
      if ((op2 >> 32) != (op3.high & 0xffffffff)) {
         printf("operand #2 [0:31] incorrect\n");
      }
   } else {
      // 2nd operand stored at 1st operand location

      // op2 did not change
      if (op2 != op2_before) {
         printf("operand #2 modified\n");
      }

      // bits [0:31] of op1 (both parts) are unchanged
      if ((op1.high >> 32) != (op1_before.high >> 32) ||
          (op1.low  >> 32) != (op1_before.low >> 32)) {
         printf("operand #1 [0:31] modified\n");
      }

      if ((op1.low & 0xffffffff) != (op2 & 0xffffffff)) {
         printf("operand #1 low[32:63] incorrect\n");
      }
      if ((op1.high & 0xffffffff) != (op2 >> 32)) {
         printf("operand #1 high[32:63] not updated\n");
      }
   }
}

int main ()
{
   quad_word op1, op3;
   uint64_t  op2;

   // (op1.high[32:63], op1.low[32:63]) == op2
   op1.high = 0x0000000044556677ull;
   op1.low  = 0x111111118899aabbull;
   op2      = 0x445566778899aabbull;

   op3.high = op3.low = 0xdeadbeefdeadbabeull;
   test(op1, op2, op3, 0);

   // (op1.high[32:63], op1.low[32:63]) != op2
   op1.high = 0x1000000000000000ull;
   op1.low  = 0x0000000000000000ull;
   op2      = 0x8000000000000001ull;;
   op3.high = op3.low = 0xdeadbeefdeadbabeull;
   test(op1, op2, op3, 1);

   return 0;
}
