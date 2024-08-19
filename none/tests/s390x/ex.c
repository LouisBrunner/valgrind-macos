#include <stdio.h>

char buffer[] ="0123456789abcdef";
char target[] ="XXXXXXXXXXXXXXXX";

int main(void)
{
   unsigned long offset;

   setbuf(stdout, NULL);

   printf("------- Copy 10+1 bytes from buffer to target\n");
   printf("------- EX to OR in the length\n");
   printf("before: buffer = |%s|\n", buffer);
   printf("before: target = |%s|\n", target);
   asm volatile( "larl 1, 1f\n\t"
                 "lghi 2, 10\n\t"
                 "ex   2, 0(1)\n\t"
                 "j    2f\n\t"
                 "1:\n\t"
                 "mvc  0(1,%0),0(%1)\n\t"
                 "2:\n\t"
                 : : "a" (target), "a" (buffer): "1", "2", "memory");
   printf("after:  buffer = |%s|\n", buffer);
   printf("after:  target = |%s|\n", target);
   printf("\n");

   printf("------- EX 0,... has no effect (writes out target)\n");
   printf("        target = |");
   asm volatile( "lghi 0, 0xff\n\t" // fill nonsense in r0
                 "larl 1, 1f\n\t"
                 "lghi 2, 1\n\t"    // stdout  
                 "lgr  3, %0\n\t"   // target
                 "lghi 4, %1\n\t"   // len
                 "ex   0, 0(1)\n\t"
                 "j    2f\n\t"
                 "1:\n\t"
                 "svc  4\n\t"
                 "2:\n\t"
                 : : "a" (target), "i" (sizeof target - 1)
                 : "0", "1", "2", "3", "4");
   printf("|\n");
   printf("\n");

   printf("------- EX to OR in the syscall number (writes out target)\n");
   printf("        target = |");
   asm volatile( "lghi 5, 4\n\t"    // NR_write
                 "larl 1, 1f\n\t"
                 "lghi 2, 1\n\t"    // stdout
                 "lgr  3, %0\n\t"   // target
                 "lghi 4, %1\n\t"   // len
                 "ex   5, 0(1)\n\t"
                 "j    2f\n\t"
                 "1:\n\t"
                 "svc  0\n\t"       // changed to NR_write
                 "2:\n\t"
                 : : "a" (target), "i" (sizeof target - 1)
                 : "1", "2", "3", "4", "5");
   printf("|\n");
   printf("\n");

   printf("------- EX targeting a PC-relative instruction\n");
   asm volatile( "1:\n\t"
                 "larl 1,1b\n\t"
                 "lgr  2,1\n\t"
                 "ex   0, 0(2)\n\t"
                 "sgrk %0,1,2\n\t"
                 : "=d" (offset) :
                 : "1", "2");
   printf("        offset = |%016lx|\n", offset);
   printf("\n");

   printf("------- EX targeting a branch-and-link instruction\n");
   asm volatile( "larl  1,1f\n\t"
                 "ex    0, 0(1)\n\t"
                 ".insn e,0x0000\n\t"
                 "1:\n\t"
                 "brasl 2,2f\n\t"
                 "2:\n\t"
                 "sgrk %0,1,2\n\t"
                 : "=&d" (offset) :
                 : "1", "2");
   printf("        offset = |%016lx|\n", offset);
   printf("\n");

   return 0;
}

