/* Simple OpenMP test program that calls printf() from a parallel section. */

#include <omp.h>
#include <stdio.h>
#include <unistd.h>  // getopt()

static void usage(const char* const exe)
{
  printf("Usage: %s [-h] [-q] [-r] [-t<n>] <m>\n"
         "-h: display this information.\n"
         "-q: quiet mode -- do not print computed error.\n",
         exe);
}

int main(int argc, char** argv)
{
  int i;
  int optchar;
  int silent = 0;
  int tid;

  while ((optchar = getopt(argc, argv, "hq")) != EOF)
  {
    switch (optchar)
    {
    case 'h': usage(argv[0]); return 1;
    case 'q': silent = 1; break;
    default:
      return 1;
    }
  }

#pragma omp parallel private(tid)
  for (i = 0; i < 2; i++)
  {
    tid = omp_get_thread_num();
    if (! silent)
    {
      printf("omp_get_thread_num() = %d/%d\n", tid, omp_get_num_threads());
    }
    else
    {
      printf("%s", "");
    }
  }

  fprintf(stderr, "Finished.\n");

  return 0;
}
