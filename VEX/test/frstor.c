
#include <stdio.h>
#include <stdlib.h>

void do_fsave ( void* p )
{
   asm("movl 8(%esp), %eax ; fsave (%eax)");
}

void do_frstor ( void* p )
{
   asm("movl 8(%esp), %eax ; frstor (%eax)");
}

int main ( void )
{
   int i;
   unsigned short* buf = malloc(54*sizeof(int));
   for (i = 0; i < 54; i++)
      buf[i] = i;
   buf[0] = 0x037f;

   for (i = 0; i < 8; i++)
      *(long double *)(&buf[14+5 *i]) = 0.1234 * i;

   do_frstor(buf);
   do_fsave(buf);
   for (i = 0; i < 54; i++) {
      printf("%04x ", buf[i]);
      if (i > 0 && ((i % 12) == 11))
          printf("\n");
   }
   printf("\n");
   return 0;
}
