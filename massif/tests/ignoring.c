// When we cull and compute the new minimum time between snapshots, we want
// to ignore any gap between two uncullable snapshots, because it is not
// representative.  This program tests that.


#include <stdlib.h>

int main(void)
{
   int i;
   
   // The peak is from the first allocation.
   int* x = malloc(1024);
   free(x);

   // Now do an allocation to provide the post-peak baseline.
   malloc(512);

   // Now we do lots of allocations below the peak.  With the proper
   // handling, the allocations should still be smoothly distributed.
   // Without it, the snapshots in the second half of the graph would be
   // clustered much more closely than those in the first half.
   //

   for (i = 0; i < 350; i++) {
      int* y = malloc(256);
      free(y);
   }
   
   return 0;
}
