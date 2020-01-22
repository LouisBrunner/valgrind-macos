
#include <stdio.h>

typedef  unsigned int  UInt;

int main ( void )
{

#define GO16(REG,VALUE) \
      value = VALUE; \
      __asm__ __volatile__( \
         "pushl %%" REG " \n\t" \
         "movl 0(" "%0" "), %%" REG " \n\t" \
         ".byte 0x66 \n\t" "bswapl %%" REG "\n\t" \
         "movl %%" REG ", 0(" "%0" ") \n\t" \
         "popl %%" REG "\n" \
             : : "r" (&value) : REG, "memory", "cc" \
      ); \
      printf("0x%08x\n", value)

   UInt value;
   GO16("eax", 0x12345678);
   GO16("ebx", 0x23456789);
   GO16("ecx", 0x3456789a);
   GO16("edx", 0x456789ab);
   GO16("esi", 0x56789abc);
   GO16("edi", 0x6789abcd);
   //GO16("ebp", 0x789abcde); // The compiler complains

   return 0;
}
