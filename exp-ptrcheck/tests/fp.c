
#include <stdlib.h>

int main ( void )
{
   double* dp = malloc(sizeof(double));
   float*  fp = malloc(sizeof(float));

   *dp += 3.0;    // ok
   *fp += 30.0;   // ok
   free(dp);
   free(fp);
   *dp += 3.0;    // bad, been freed
   *fp += 30.0;   // bad, been freed

   return 0;
}
