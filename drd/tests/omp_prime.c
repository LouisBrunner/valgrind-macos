/** An OpenMP example.
 *  Based on the example listed on the following web page:
 *  http://developers.sun.com/sunstudio/downloads/ssx/tha/tha_using.html
 */


#include <assert.h>
#include <math.h>
#include <omp.h>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>  // getopt()
#include "../../drd/drd.h"


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
  int trace_total = 0;
  int silent = 0;
  int n;
  int num_threads = 2;
  int optchar;
  int* primes;
  int* pflag;

  while ((optchar = getopt(argc, argv, "qt:v")) != EOF)
  {
    switch (optchar)
    {
    case 'q':
      silent = 1;
      break;
    case 't':
      num_threads = atoi(optarg);
      break;
    case 'v':
      trace_total = 1;
      break;
    default:
      fprintf(stderr, "Error: unknown option '%c'.\n", optchar);
      return 1;
    }
  }

  if (optind + 1 != argc)
  {
    fprintf(stderr, "Error: wrong number of arguments.\n");
    return 1;
  }
  n = atoi(argv[optind]);

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

  if (trace_total)
    DRD_TRACE_VAR(total);

#pragma omp parallel for
  for (i = 2; i < n; i++)
  {
    if (is_prime(pflag, i))
    {
      primes[total] = i;
      total++;
    }
  }
  if (! silent)
  {
    printf("Number of prime numbers between 2 and %d: %d\n",
           n, total);
    for (i = 0; i < total; i++)
    {
      printf("%d\n", primes[i]);
    }
  }

  free(pflag);
  free(primes);

  return 0;
}
