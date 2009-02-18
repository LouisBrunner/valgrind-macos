#include <stdlib.h>

// In this test, the size of the insignificant nodes is greater than the
// size of two of the significant nodes.  This is quite common in big
// programs, but not so common in small tests, so we test for it here.
int main(void)
{
   malloc(16000);  // all sizes are divisible by 16 -- no slop
   malloc(240);
   malloc(192);
   malloc(16);
   malloc(16);
   malloc(16);
   malloc(16);
   malloc(16);
   malloc(16);
   malloc(16);
   malloc(16);
   malloc(16);
   malloc(16);
   malloc(16);
   malloc(16);
   malloc(16);
   malloc(16);
   malloc(16);
   malloc(16);
   malloc(16);
   malloc(16);
   malloc(16);
   malloc(16);

   
   return 0;
}
