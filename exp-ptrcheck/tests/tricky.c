
#include <stdlib.h>

int main(void)
{
   // When I had n-u --> u, this gave a false positive... can happen because
   // p+up can give n if you are (un)lucky, because the result is close enough
   // to zero.
   int  u[20];
   int* p = malloc(sizeof(int) * 100);

   p[0] = 0;                           // ok
   int* n = (int*)((long)p + (long)u); // result is n, because near zero!
   int* x = (int*)((long)n - (long)u); // x == p
   x[0] = 0;                           // ok, originally caused false pos.

   return 0;
}
