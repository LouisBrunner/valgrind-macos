#include <stdlib.h>

int main(void)
{
   void* x = malloc(10);

   int*        x4 = x-4;
   short int*  x2 = x-4;
   char*       x1 = x-1;

   // Invalid reads and writes of sizes 4, 2, 1
   int       y4 = *x4;  *x4 = y4;
   short int y2 = *x2;  *x2 = y2;
   char      y1 = *x1;  *x1 = y1;
   
   return 0;
}
