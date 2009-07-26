/* Simple OpenMP test program that calls printf() from a parallel section. */

#include <assert.h>  // assert()
#include <omp.h>
#include <stdio.h>
#include <stdlib.h>  // atoi()
#include <unistd.h>  // getopt()

static void usage(const char* const exe)
{
  fprintf(stderr,
          "Usage: %s [-h] [-i <n>] [-q] [-t<n>]\n"
          "-h: display this information.\n"
          "-i <n>: number of loop iterations.\n"
          "-q: quiet mode -- do not print computed error.\n"
          "-t <n>: number of OMP threads.\n",
          exe);
}

int main(int argc, char** argv)
{
  int i;
  int optchar;
  int silent = 0;
  int tid;
  int num_iterations = 2;
  int num_threads = 2;

  while ((optchar = getopt(argc, argv, "hi:qt:")) != EOF)
  {
    switch (optchar)
    {
    case 'h': usage(argv[0]); return 1;
    case 'i': num_iterations = atoi(optarg); break;
    case 'q': silent = 1; break;
    case 't': num_threads = atoi(optarg); break;
    default:
      return 1;
    }
  }

  /*
   * Not the most user-friendly way of error checking, but still better than
   * no error checking.
   */
  assert(num_iterations > 0);
  assert(num_threads > 0);

  omp_set_num_threads(num_threads);
  omp_set_dynamic(0);

#pragma omp parallel for private(tid)
  for (i = 0; i < num_iterations; i++)
  {
    tid = omp_get_thread_num();
    if (! silent)
    {
      fprintf(stderr,
              "iteration %d; thread number = %d; number of threads = %d\n",
              i, tid, omp_get_num_threads());
    }
    else
    {
      fprintf(stderr, "%s", "");
    }
  }

  fprintf(stderr, "Finished.\n");

  return 0;
}
