
#include <stdio.h>

int isprime ( int n )
{
   int m;
   for (m = 2; m < n; m++)
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
