#include <stdlib.h>

// Do some big allocations.  At one point, the threshold calculation was
// multiplying the szB by 10000 without using a Long, which was causing the
// threshold calculation to go wrong due to a 32-bit overflow.

int main(void)
{
   // 100MB all up.
   int i;
   for (i = 0; i < 10; i++) {
      malloc(10 * 1024 * 1024);
   }
   
   return 0;
}
