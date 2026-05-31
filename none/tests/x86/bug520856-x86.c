/* test_cs_jmp.c - regression test for CS-prefixed indirect jmp */
#include <stdio.h>

static void test(void)
{
   /* A CS-prefixed indirect jmp through a register is simpler:
      2E FF E0 = cs jmp *%eax
      This uses the same 0x2E/0xFF path without a complex SIB byte */
   int dummy = (int)&&skip;
   __asm__ volatile (
       "movl %0, %%eax\n\t"
       ".byte 0x2E, 0xFF, 0xE0\n\t"   /* cs jmp *%eax */
       :
       : "r"(dummy)
       : "eax"
   );
   // should skip this
   printf("failed\n");
skip:
   return;
}

int main(void)
{
    test();
    return 0;
}
