#include <stdio.h>

/* -- Full population count -- */

static void test_popcnt(unsigned long op2)
{
   unsigned long result;
   int cc;

   __asm__(".insn   rrf,0xb9e10000,%[result],%[op2],8,0\n\t"
           "ipm     %[cc]\n\t"
           "srl     %[cc],28\n"
           : [result]"=d" (result),
             [cc]"=d" (cc)
           : [op2]"d" (op2)
           : "cc");
   printf("%016lx -> %2lu cc=%d\n", op2, result, cc);
}

int main(void)
{
   test_popcnt(0);
   test_popcnt(1);
   test_popcnt(0x8000000000000000UL);
   test_popcnt(~0UL);
   test_popcnt(0xff427e3800556bcd);
   return 0;
}
