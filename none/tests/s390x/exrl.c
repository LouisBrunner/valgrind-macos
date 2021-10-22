#include <stdio.h>

char buffer[] ="0123456789abcdef";
char target[] ="XXXXXXXXXXXXXXXX";

int main(void)
{
   setbuf(stdout, NULL);

   printf("------- Copy 10+1 bytes from buffer to target\n");
   printf("------- EXRL to OR in the length\n");
   printf("before: buffer = |%s|\n", buffer);
   printf("before: target = |%s|\n", target);
   asm volatile( "lghi 2, 10\n\t"
                 ".insn ril,0xc60000000000,2,1f\n\t" // exrl 2, 1f
                 "j    2f\n\t"
                 "1:\n\t"
                 "mvc  0(1,%0),0(%1)\n\t"
                 "2:\n\t"
                 : : "a" (target), "a" (buffer) : "1", "2", "memory");
   printf("after:  buffer = |%s|\n", buffer);
   printf("after:  target = |%s|\n", target);
   printf("\n");

   printf("------- EXRL 0,... has no effect (writes out target)\n");
   printf("        target = |");
   asm volatile( "lghi 0, 0xff\n\t" // fill nonsense in r0
                 "lghi 2, 1\n\t"    // stdout
                 "lgr  3, %0\n\t"   // target
                 "lghi 4, %1\n\t"   // len
                 ".insn ril,0xc60000000000,0,1f\n\t" // exrl 0, 1f
                 "j    2f\n\t"
                 "1:\n\t"
                 "svc  4\n\t"       // NR_write
                 "2:\n\t"
                 : : "a" (target), "i" (sizeof target - 1)
                 : "0", "2", "3", "4");
   printf("|\n");
   printf("\n");

   printf("------- EXRL to OR in the syscall number (writes out target)\n");
   printf("        target = |");
   asm volatile( "lghi 1, 4\n\t"    // NR_write
                 "lghi 2, 1\n\t"    // stdout
                 "lgr  3, %0\n\t"   // target
                 "lghi 4, %1\n\t"   // len
                 ".insn ril,0xc60000000000,1,1f\n\t" // exrl 1, 1f
                 "j    2f\n\t"
                 "1:\n\t"
                 "svc  0\n\t"       // changed to NR_write
                 "2:\n\t"
                 : : "a" (target), "i" (sizeof target - 1)
                 : "1", "2", "3", "4");
   printf("|\n");
   printf("\n");

   printf("------- EXRL with negative offset\n");
   asm volatile( "j    2f\n\t"
                 "1:\n\t"
                 "mvc  2(1,%0),0(%0)\n\t"
                 "2:\n\t"
                 "lghi 1,8\n\t"
                 ".insn ril,0xc60000000000,1,1b\n\t" // exrl 1, 1b
                 : : "a" (target)
                 : "1", "2", "3", "4");
   printf("        target = |%s|\n", target);

   return 0;
}

