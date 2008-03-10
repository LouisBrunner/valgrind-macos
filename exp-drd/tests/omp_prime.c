/** An OpenMP example.
 *  Based on the example listed on the following web page:
 *  http://developers.sun.com/sunstudio/downloads/ssx/tha/tha_using.html
 */


#include <assert.h>
#include <math.h>
#include <omp.h>
#include <stdio.h>
#include <stdlib.h>


static int is_prime(int* const pflag, int v)
{
  int i;
  int bound = floor(sqrt ((double)v)) + 1;

  for (i = 2; i < bound; i++)
  {
    /* No need to check against known composites */
    if (!pflag[i])
      continue;
    if (v % i == 0)
    {
      pflag[v] = 0;
      return 0;
    }
  }
  return (v > 1);
}

int main(int argc, char **argv)
{
  int i;
  int total = 0;
  const int n           = argc > 1 ? atoi(argv[1]) : 300;
  const int num_threads = argc > 2 ? atoi(argv[2]) : 4;
  int* primes;
  int* pflag;

  // Not the most user-friendly way to do error checking, but better than
  // nothing.
  assert(n > 2);
  assert(num_threads >= 1);

  primes = malloc(n * sizeof(primes[0]));
  pflag  = malloc(n * sizeof(pflag[0]));

  omp_set_num_threads(num_threads);
  omp_set_dynamic(0);

  for (i = 0; i < n; i++) {
    pflag[i] = 1;
  }

#pragma omp parallel for
  for (i = 2; i < n; i++)
  {
    if (is_prime(pflag, i))
    {
      primes[total] = i;
      total++;
    }
  }
  printf("Number of prime numbers between 2 and %d: %d\n",
         n, total);
  for (i = 0; i < total; i++)
  {
    printf("%d\n", primes[i]);
  }

  free(pflag);
  free(primes);

  return 0;
}
