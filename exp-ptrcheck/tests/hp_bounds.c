#include <stdlib.h>

int main(void)
{
   int  y;
   int* x = malloc(sizeof(int) * 100);

   y = x[95];   // ok
   y = x[100];  // overrun
   y = x[-1];   // underrun

   return 0;
}
