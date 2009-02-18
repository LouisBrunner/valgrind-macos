#include <stdlib.h>

int main(void)
{
   int i;
   for (i = 0; i < 200; i++) {
      malloc(400*i);    // Divisible by 16 -- no slop.
   }
   return 0;
}
