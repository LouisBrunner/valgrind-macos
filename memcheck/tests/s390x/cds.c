#include <stdint.h>
#include <stdio.h>

typedef struct {
   uint64_t high;
   uint64_t low;
} quad_word;

void 
test(quad_word op1_init, uint64_t op2_init, quad_word op3_init)
{
   int cc; // unused
   quad_word op1 = op1_init;
   uint64_t  op2 = op2_init;
   quad_word op3 = op3_init;

   __asm__ volatile (
                     "lmg     %%r0,%%r1,%1\n\t"
                     "lmg     %%r2,%%r3,%3\n\t"
                     "cds     %%r0,%%r2,%2\n\t"  //  cds 1st,3rd,2nd
                     "stmg    %%r0,%%r1,%1\n"    // store r0,r1 to op1
                     "stmg    %%r2,%%r3,%3\n"    // store r2,r3 to op3
                     : "=d" (cc), "+QS" (op1), "+QS" (op2), "+QS" (op3)
                     :
                     : "r0", "r1", "r2", "r3", "cc");

}

// Return a quad-word that only bits low[32:63] are undefined
quad_word
make_undefined(void)
{
   quad_word val;

   val.high = 0;
   val.low |= 0xFFFFFFFF00000000ull;

   return val;
}

void op1_undefined(void)
{
   quad_word op1, op3;
   uint64_t op2;

   // op1 undefined
   op1 = make_undefined();
   op2 = 42;
   op3.high = op3.low = 0xdeadbeefdeadbabeull;
   test(op1, op2, op3);  // complaint
}

void op2_undefined(void)
{
   quad_word op1, op3;
   uint64_t op2;

   op1.high = op1.low = 42;
   // op2 undefined
   op3.high = op3.low = 0xdeadbeefdeadbabeull;
   test(op1, op2, op3);  // complaint
}

void op3_undefined(void)
{
   quad_word op1, op3;
   uint64_t op2;

   op1.high = op1.low = 42;
   op2 = 100;
   op3 = make_undefined();
   test(op1, op2, op3);  // no complaint; op3 is just copied around
}

int main ()
{
   op1_undefined();
   op2_undefined();
   op3_undefined();

   return 0;
}
