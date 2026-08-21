/* Test for CS-prefixed indirect jmp.
 * The previous version had a problem loading the skip address on illumos
 * This version uses all inline asm and jumps around an int 3 to avoid
 * crashing with SIGTRAP. Prints "ok" on success.
*/
#include <stdio.h>

static void test(void)
{
   __asm__ volatile (
      "leal 1f, %%eax\n\t"
      ".byte 0x2E, 0xFF, 0xE0\n\t"
      ".int 3\n\t"
      "1:\n\t"
      :
      :
      : "eax"
   );
}

int main(void)
{
    test();
    printf("ok\n");
    return 0;
}
