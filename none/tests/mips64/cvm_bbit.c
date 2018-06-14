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

   long long int lt1 = 0;
   long long int lt2 = 0;
   long long int lt3 = 0xff00000000;
   long long int lt4 = 0x100000000;
   /* Take 0x100000000 and loop until 35th bit is set
      by incrementing 0x100000000 at a time. */
   __asm__ volatile(
      ".set noreorder"           "\n\t"
      "move $t0, $zero"          "\n\t"
      "move $t1, %1"             "\n\t"
      "label4:"                  "\n\t"
      "dadd $t0, $t0, $t1"       "\n\t"
      "bbit032 $t0, 0x3, label4" "\n\t"
      "nop"                      "\n\t"
      "move %0, $t0"             "\n\t"
      ".set reorder"             "\n\t"
      : "=r" (lt1)
      : "r" (lt4)
      : "t0", "t1");
   /* Take 0xff00000000 and loop until 35th bit is cleared
      by decrementing 0x100000000 at a time. */
   __asm__ volatile(
      ".set noreorder"           "\n\t"
      "move $t0, %1"             "\n\t"
      "move $t1, %2"             "\n\t"
      "label3:"                  "\n\t"
      "dadd $t0, $t0, $t1"       "\n\t"
      "bbit132 $t0, 0x3, label3" "\n\t"
      "nop"                      "\n\t"
      "move %0, $t0"             "\n\t"
      ".set reorder"             "\n\t"
      : "=r" (lt2)
      : "r" (lt3), "r" (-lt4)
      : "t0", "t1");

   printf("TEST bbit032: %s\n", lt1 == 0x0800000000 ? "PASS" : "FAIL");
   printf("TEST bbit132: %s\n", lt2 == 0xF700000000 ? "PASS" : "FAIL");

#endif
   return 0;
}
