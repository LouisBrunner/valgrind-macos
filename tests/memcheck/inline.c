
#include <stdio.h>
#include <stdlib.h>

__inline__
static int addemup ( int* arr )
{
   int i, j = 0;
   for (i = 0; i <= 10; i++)
      j += arr[i];
   return j;
}

int main ( void )
{
   int sum;
   int* a = calloc(10, sizeof(int));
   sum = addemup(a);
   printf("sum is %d\n", sum);
   return 0;
}
