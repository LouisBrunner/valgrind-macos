#include "tests/malloc.h"
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h> // getopt()
#include "../config.h"


static int s_quiet = 0;


#if defined(HAVE_MALLINFO)
static size_t check(size_t min, size_t max)
{
  struct mallinfo mi;
  size_t used;

  mi = mallinfo();

  if (! s_quiet)
  {
    printf("arena = %d\n", mi.arena);	    /* non-mmapped space allocated from system */
    printf("ordblks = %d\n", mi.ordblks);   /* number of free chunks */
    printf("smblks = %d\n", mi.smblks);	    /* number of fastbin blocks */
    printf("hblks = %d\n", mi.hblks);	    /* number of mmapped regions */
    printf("hblkhd = %d\n", mi.hblkhd);	    /* space in mmapped regions */
    printf("usmblks = %d\n", mi.usmblks);   /* maximum total allocated space */
    printf("fsmblks = %d\n", mi.fsmblks);   /* space available in freed fastbin blocks */
    printf("uordblks = %d\n", mi.uordblks); /* total allocated space */
    printf("fordblks = %d\n", mi.fordblks); /* total free space */
    printf("keepcost = %d\n", mi.keepcost); /* top-most, releasable (via malloc_trim) space */
    printf("(min = %zu, max = %zu)\n", min, max);
    printf("\n");
  }

  // size checks
  used = mi.uordblks + mi.hblkhd;
  if (used < min)
    exit(1);

  if (used > max)
    exit(2);

  // used should be reasonably close to min
  // define "reasonably" as within 20%
  if (used/5*4 > min)
    exit(3);

  // sanity checks
  if ((mi.ordblks == 0) != (mi.fordblks == 0))
    exit(10);

  if ((mi.smblks == 0) != (mi.fsmblks == 0))
    exit(11);

  if ((mi.hblks == 0) != (mi.hblkhd == 0))
    exit(12);

  if (mi.keepcost > mi.fordblks)
    exit(13);

  if (mi.fsmblks > mi.fordblks)
    exit(14);

  // arena should be reasonably close to fordblks + uordblks
  if (mi.arena < mi.fordblks + mi.uordblks)
    exit(15);

  if (mi.arena/5*4 > mi.fordblks + mi.uordblks)
    exit(16);

  return used;
}
#else
static size_t check(size_t min, size_t max)
{
  if (! s_quiet)
  {
    printf("mallinfo() is not supported on this platform.\n");
    printf("\n");
  }
  return 0;
}
#endif

int main(int argc, char** argv)
{
  void* ptr[40];
  int i;
  size_t min, max;
  int optchar;

  while ((optchar = getopt(argc, argv, "q")) != EOF)
  {
    switch (optchar)
    {
    case 'q':
      s_quiet = 1;
      break;
    default:
      fprintf(stderr, "Usage: %s [-q].\n", argv[0]);
      return 1;
    }
  }

  min = 0;
  for (i = 1; i <= 40; i++)
  {
    int size = i * i * 8;
    min += size;
    ptr[i - 1] = malloc(size);
  };

  max = check(min, (size_t)-1);

  for (i = 1; i <= 20; i++)
  {
    int size = i * i * 8;
    min -= size;
    max -= size;
    free(ptr[i - 1]);
  };

  check(min, max);

  for ( ; i <= 40; i++)
  {
    free(ptr[i - 1]);
  }

  fprintf(stderr, "Success.\n");

  return 0;
}
