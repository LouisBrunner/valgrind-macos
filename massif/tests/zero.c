// Test zero-size allocations -- shouldn't cause division by zero, that kind
// of thing.

#include <stdlib.h>

int main(void)
{
   free(malloc(0));
   free(malloc(0));
   free(malloc(0));
   free(malloc(0));
   free(malloc(0));
   free(malloc(0));
   free(malloc(0));
   free(malloc(0));
   free(malloc(0));
   free(malloc(0));
   return 0;
}
