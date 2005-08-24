// In 3.0.0, Massif was badly broken on 64-bit platforms because it asked
// zero-sized redzones, and the allocator was forgetting to round the size
// up to sizeof(void*), which is the minimum.  This caused bugs #111090 and
// #111285.  This test is just a gentle allocation exercise which was
// failing.

#include <stdlib.h>
#include <stdio.h>

#define NN  100

int main(void)
{ 
   int i;
   char* a[NN];

   for (i = i; i < NN; i++) {
      a[i] = malloc(i);
   }
   
   for (i = i; i < NN; i++) {
      a[i] = realloc(a[i], NN - i);
   }
   
   for (i = i; i < NN; i++) {
      free(a[i]);
   }
   
   return 0;
}

