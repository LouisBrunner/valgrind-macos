
#include <stdio.h>
#include <stdlib.h>

void do_fstenv ( void* p )
{
   asm("fldpi ; fld1; fldln2 ; movl 8(%esp), %eax ; fstenv (%eax)");
}

int main ( void )
{
   int i;
   unsigned int* buf = malloc(7*sizeof(int));
   do_fstenv(buf);
   for (i = 0; i < 7; i++) {
      printf("%08x ", buf[i]);
      if (i > 0 && ((i % 6) == 5))
          printf("\n");
   }
   printf("\n");
   return 0;
}
