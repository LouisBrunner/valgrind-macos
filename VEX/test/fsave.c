
#include <stdio.h>
#include <stdlib.h>

void do_fsave ( void* p )
{
   asm("fldpi ; fld1; fldln2 ; movl 8(%esp), %eax ; fsave (%eax)");
}

int main ( void )
{
   int i;
   unsigned int* buf = malloc(27*sizeof(int));
   do_fsave(buf);
   for (i = 0; i < 27; i++) {
      printf("%08x ", buf[i]);
      if (i > 0 && ((i % 6) == 5))
          printf("\n");
   }
   printf("\n");
   return 0;
}
