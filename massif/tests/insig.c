#include <stdlib.h>

// In this test, the size of the insignificant nodes is greater than the
// size of two of the significant nodes.  This is quite common in big
// programs, but not so common in small tests, so we test for it here.
int main(void)
{
   malloc(1000);
   malloc(15);
   malloc(12);
   malloc(1);
   malloc(1);
   malloc(1);
   malloc(1);
   malloc(1);
   malloc(1);
   malloc(1);
   malloc(1);
   malloc(1);
   malloc(1);
   malloc(1);
   malloc(1);
   malloc(1);
   malloc(1);
   malloc(1);
   malloc(1);
   malloc(1);
   malloc(1);
   malloc(1);
   malloc(1);

   
   return 0;
}
