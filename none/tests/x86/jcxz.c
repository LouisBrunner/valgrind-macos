
#include <stdio.h>

typedef  unsigned int  UInt;

UInt test_jcxz ( UInt arg )
{
   UInt block[2];
   block[0] = arg;
   block[1] = 0xdeadbeef;
   __asm__ __volatile__(
      "movl %0,%%ecx\n\t"
      "movl $0,%%eax\n"
      "0:\n\t"
      "jcxz 1f\n\t"
      "addl $1, %%eax\n\t"
      "subl $1, %%ecx\n\t"
      "jmp 0b\n"
      "1:\n\t"
      "movl %%eax, %1"
      : /*out*/ : /*in*/ "m"(block[0]),
                         "m"(block[1]) : /*trash*/ "eax","ecx","cc","memory"
   );
   return block[1];
}

UInt test_jecxz ( UInt arg )
{
   UInt block[2];
   block[0] = arg;
   block[1] = 0xdeadbeef;
   __asm__ __volatile__(
      "movl %0,%%ecx\n\t"
      "movl $0,%%eax\n"
      "0:\n\t"
      "jecxz 1f\n\t"
      "addl $1, %%eax\n\t"
      "subl $1, %%ecx\n\t"
      "jmp 0b\n"
      "1:\n\t"
      "movl %%eax, %1"
      : /*out*/ : /*in*/ "m"(block[0]),
                         "m"(block[1]) : /*trash*/ "eax","ecx","cc","memory"
   );
   return block[1];
}

int main ( void )
{
   UInt arg = 0x01028374;
   printf("test_jcxz(0x%x)  = 0x%x\n", arg, test_jcxz(arg));
   printf("test_jecxz(0x%x) = 0x%x\n", arg, test_jecxz(arg));
   return 0;
}
