



#include <stdlib.h>

int main ( void )
{
   volatile double d;
   volatile float f;
   double* dp = malloc(sizeof(double));
   float* fp = malloc(sizeof(float));
   int* ip = (int*)0x1234567;
   d += 1.0;
   f += 10.0;
   *dp += ( d > 0.1  ? 2.0 : 3.0 );
   *fp += ( f > 0.1  ? 20.0 : 21.0 );
   free(dp);
   free(fp);
   *dp += 3.0;
   *fp += 30.0;
   free(ip);
   ip = malloc(sizeof(int));
   * ((double*)ip) = 1.2 + d;
   return 0;
}
