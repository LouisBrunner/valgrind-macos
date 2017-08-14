
#include <stdio.h>
#include <stdlib.h>
#include <math.h>

int main ( void )
{
   int i, j;
   double s, r;
   s = 0.0;
   double* a1 = malloc(1000 * sizeof(double));
   double* a2 = malloc(1000 * sizeof(double));
   for (i = 0; i < 1000; i++) {
     a1[i] = s; 
     s += 0.3374893482232;
     a2[i] = s;
   }

   s = 0.0;
   r = 0.0;
   for (j = 0; j < 5000; j++) {
      for (i = 0; i < 1000; i++) {
         s += (a1[i] - a2[i]) * (a1[i] + a2[i]) - sqrt(r + 1.0);
         r += 0.001;
      }
   }
   printf("s = %f, r = %f\n", s, r );
   return 0;
}
