
#include <stdio.h>
#include <stdlib.h>

void do_fstenv ( void* p )
{
   asm("movl 8(%esp), %eax ; fstenv (%eax)");
}

void do_fldenv ( void* p )
{
   asm("movl 8(%esp), %eax ; fldenv (%eax)");
}

int main ( void )
{
   int i;
   unsigned short* buf = malloc(14*sizeof(short));
   for (i = 0; i < 14; i++)
      buf[i] = i;
   buf[0] = 0x037f;

   do_fldenv(buf);
   do_fstenv(buf);
   for (i = 0; i < 14; i++) {
      printf("%04x ", buf[i]);
      if (i > 0 && ((i % 12) == 11))
          printf("\n");
   }
   printf("\n");
   return 0;
}
