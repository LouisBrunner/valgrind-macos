#include <stdlib.h>

// In this test, the size of the insignificant nodes is greater than the
// size of two of the significant nodes.  This is quite common in big
// programs, but not so common in small tests, so we test for it here.
int main(void)
{
   malloc(8000);  // all sizes are divisible by 8 -- no slop
   malloc(120);
   malloc(96);
   malloc(8);
   malloc(8);
   malloc(8);
   malloc(8);
   malloc(8);
   malloc(8);
   malloc(8);
   malloc(8);
   malloc(8);
   malloc(8);
   malloc(8);
   malloc(8);
   malloc(8);
   malloc(8);
   malloc(8);
   malloc(8);
   malloc(8);
   malloc(8);
   malloc(8);
   malloc(8);

   
   return 0;
}
