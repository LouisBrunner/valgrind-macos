#include <stdio.h>
#include <stdlib.h>

#define NLIVE 1000000

#define NITERS (3*1000*1000)

char* arr[NLIVE];

int main ( void )
{
   int i, j, nbytes = 0;
   printf("initialising\n");
   for (i = 0; i < NLIVE; i++)
      arr[i] = NULL;

   printf("running\n");
   j = -1;
   for (i = 0; i < NITERS; i++) {
      j++;
      if (j == NLIVE) j = 0;
      if (arr[j]) 
         free(arr[j]);
      arr[j] = malloc(nbytes);

      // Cycle through the sizes 0,8,16,24,32.  Zero will get rounded up to
      // 8, so the 8B bucket will get twice as much traffic.
      nbytes += 8;
      if (nbytes > 32)
         nbytes = 0;
   }

   for (i = 0; i < NLIVE; i++)
      if (arr[i])
         free(arr[i]);

   printf("done\n");
   return 0;
}
