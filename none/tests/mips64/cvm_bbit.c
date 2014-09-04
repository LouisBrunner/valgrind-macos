#include <stdio.h>

int main()
{
#if (_MIPS_ARCH_OCTEON)
   int t1 = 0;
   int t2 = 0;
   __asm__ volatile(
      ".set noreorder"          "\n\t"
      "move  $t0, $zero"        "\n\t"
      "label2:"                 "\n\t"
      "addiu $t0, $t0, 1"       "\n\t"
      "bbit0 $t0, 0x3, label2"  "\n\t"
      "nop"                     "\n\t"
      "move %0, $t0"            "\n\t"
      ".set reorder"            "\n\t"
      : "=r" (t1)
      :
      : "t0");
   __asm__ volatile(
      ".set noreorder"          "\n\t"
      "li    $t0, 0xff"         "\n\t"
      "label1:"                 "\n\t"
      "addiu $t0, $t0, -1"      "\n\t"
      "bbit1 $t0, 0x3, label1"  "\n\t"
      "nop"                     "\n\t"
      "move %0, $t0"            "\n\t"
      ".set reorder"            "\n\t"
      : "=r" (t2)
      :
      : "t0");

   printf("TEST bbit0: %s\n", t1 == 0x08 ? "PASS" : "FAIL");
   printf("TEST bbit1: %s\n", t2 == 0xF7 ? "PASS" : "FAIL");

#endif
   return 0;
}
