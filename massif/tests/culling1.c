#include <stdlib.h>

int main(void)
{
   int i;
   for (i = 0; i < 200; i++) {
      malloc(8);     // divisible by 8 -- no slop
   }
   return 0;
}
