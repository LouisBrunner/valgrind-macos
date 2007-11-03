// This test does enough allocation and deallocation that the time-unit,
// when measured in bytes -- 6,000,000,000 -- exceeds 32-bits.  It also does
// it in a slightly uneven fashion so we get a range of different totals
// for the snapshots, including a zero-sized detailed snapshot.

#include <stdlib.h>

int main(void)
{
   int i, *x1, *x2, *x3, *x4;
   for (i = 0; i < 1500; i++) {
      x1 = malloc( 800 * 1000);
      x2 = malloc(1100 * 1000);
      free(x1);
      x3 = malloc(1200 * 1000);
      free(x2);
      free(x3);
      x4 = malloc( 900 * 1000);
      free(x4);
   }
   return 0;
}
