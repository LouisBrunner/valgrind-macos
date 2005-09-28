
#include <stdio.h>
#include <math.h>

int isprime ( int n )
{
   int m;
   int sqrt_n = sqrt(n);
   for (m = 2; m <= sqrt_n+1; m++)  // +1 in case of obscure rounding error
      if ((n % m) == 0) return 0;
   return 1;
}

int main ( int argc, char** argv )
{
   int i;
   for (i = 79000; i < 81000; i++)
     if (isprime(i)) { printf ( "%d ", i ); fflush(stdout); }
   return 0;
}
