#include <stdint.h>
#include <stdio.h>

typedef struct {
   uint64_t high;
   uint64_t low;
} __attribute__((aligned(16))) quad_word;


/* CDSG needs quad-word alignment */
quad_word _op1, _op2, _op3;

void
test(quad_word op1_init, quad_word op2_init, quad_word op3_init,
     int expected_cc)
{
   quad_word op1_before, op2_before, op3_before;
   int cc = 1 - expected_cc;

   _op1 = op1_init;
   _op2 = op2_init;
   _op3 = op3_init;

   op1_before = _op1;
   op2_before = _op2;
   op3_before = _op3;

   printf("before op1 = (%#lx, %#lx)\n", _op1.high, _op1.low);
   printf("before op2 = (%#lx, %#lx)\n", _op2.high, _op2.low);
   printf("before op3 = (%#lx, %#lx)\n", _op3.high, _op3.low);

   __asm__ volatile (
                     "lmg     %%r0,%%r1,%1\n\t"
                     "lmg     %%r2,%%r3,%3\n\t"
                     "cdsg    %%r0,%%r2,%2\n\t"  //  cdsg 1st,3rd,2nd
                     "stmg    %%r0,%%r1,%1\n"    // store r0,r1 to _op1
                     "stmg    %%r2,%%r3,%3\n"    // store r2,r3 to _op3
                     "ipm     %0\n\t"
                     "srl     %0,28\n\t"
                     : "=d" (cc), "+QS" (_op1), "+QS" (_op2), "+QS" (_op3)
                     :
                     : "r0", "r1", "r2", "r3", "cc");

   printf("after  op1 = (%#lx, %#lx)\n", _op1.high, _op1.low);
   printf("after  op2 = (%#lx, %#lx)\n", _op2.high, _op2.low);
   printf("after  op3 = (%#lx, %#lx)\n", _op3.high, _op3.low);
   printf("cc = %d\n", cc);

   if (cc != expected_cc) {
      printf("condition code is incorrect\n");
   }

   // _op3 never changes
   if (_op3.low != op3_before.low || _op3.high != op3_before.high) {
      printf("operand #3 modified\n");
   }

   if (expected_cc == 0) {
      // 3rd operand stored at 2nd operand location

      // _op1 did not change
      if (_op1.low != op1_before.low || _op1.high != op1_before.high) {
         printf("operand #1 modified\n");
      }
      if (_op2.high != _op3.high || _op2.low != _op3.low) {
         printf("operand #2 incorrect\n");
      }
   } else {
      // 2nd operand stored at 1st operand location

      // _op2 did not change
      if (_op2.low != op2_before.low || _op2.high != op2_before.high) {
         printf("operand #2 modified\n");
      }

      if (_op1.high != _op2.high || _op1.low != _op2.low) {
         printf("operand #1 incorrect\n");
      }
   }
}

int main ()
{
   quad_word op1, op2, op3;

   // op1 == op2
   op1.high = 0x0011223344556677ull;
   op1.low  = 0x8899aabbccddeeffull;
   op2 = op1;
   op3.high = op3.low = 0xdeadbeefdeadbabeull;
   test(op1, op2, op3, 0);

   // op1 != op2 (only MSB differs)
   op1.high = 0x8000000000000000ull;
   op1.low  = 0x0000000000000000ull;
   op2.high = 0;
   op2.low  = 1;
   op3.high = op3.low = 0xdeadbeefdeadbabeull;
   test(op1, op2, op3, 1);

   // op1 != op2 (only LSB differs)
   op1.high = 0x0000000000000000ull;
   op1.low  = 0x0000000000000001ull;
   op2.high = 1;
   op2.low  = 0;
   op3.high = op3.low = 0xdeadbeefdeadbabeull;
   test(op1, op2, op3, 1);

   return 0;
}
