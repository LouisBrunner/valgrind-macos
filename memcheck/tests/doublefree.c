
#include <stdio.h>
#include <stdlib.h>

int main ( void )
{
   int i;
   void* p = malloc(177);
   for (i = 0; i < 2; i++)
     free(p);
   return 0;
}
