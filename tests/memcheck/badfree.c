

#include <stdio.h>
#include <stdlib.h>

int main ( void )
{
   void* p = (void*)0x87654321;
   int q[] = { 1, 2, 3 };
   
   /* Free a pointer to Never-Never Land */
   free(p);

   /* Free a pointer to a stack block */
   free(q);

   return 0;
}
