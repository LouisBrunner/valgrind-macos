
#include <stdio.h>
#include <stdlib.h>

int main ( void )
{
   char* aa = malloc(8);
   aa[-1] = 17;
   if (aa[-1] == 17) 
      printf("17\n"); else printf("not 17\n");
   return 0;
}
