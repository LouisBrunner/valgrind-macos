#include <stdlib.h>
#include <stdio.h>
#include "../../config.h"
#if defined(HAVE_MALLINFO)
#include <malloc.h>
#endif

#define BIGINCREASE 32000
int debug = 0;

void stats(char *msg)
{
#if defined(HAVE_MALLINFO)
  struct mallinfo mallinfo_result;
  mallinfo_result = mallinfo();
#endif

  /* from /usr/include/malloc.h */
  printf("%s\n", msg);

#if defined(HAVE_MALLINFO)
  printf("%10d int arena;    /* non-mmapped space allocated from system */\n", mallinfo_result.arena);
  printf("%10d int ordblks;  /* number of free chunks */\n", mallinfo_result.ordblks);
  printf("%10d int smblks;   /* number of fastbin blocks */\n", mallinfo_result.smblks);
  printf("%10d int hblks;    /* number of mmapped regions */\n", mallinfo_result.hblks);
  printf("%10d int hblkhd;   /* space in mmapped regions */\n", mallinfo_result.hblkhd);
  printf("%10d int usmblks;  /* maximum total allocated space */\n", mallinfo_result.usmblks);
  printf("%10d int fsmblks;  /* space available in freed fastbin blocks */\n", mallinfo_result.fsmblks);
  printf("%10d int uordblks; /* total allocated space */\n", mallinfo_result.uordblks);
  printf("%10d int fordblks; /* total free space */\n", mallinfo_result.fordblks);
  printf("%10d int keepcost; /* top-most, releasable (via malloc_trim) space */\n", mallinfo_result.keepcost);
  printf("\n");
#endif
}

int main(int argc, char *argv[])
{

  char *big = NULL;

  char *newbig;
  int malloc_failure = 0;
  unsigned long bigsize = 8; // current size of the (reallocated) big block.
  int i;
  int loop;

  // two optional arguments: [nr of loop] [debug]
  if (argc > 1)
     loop = atoi(argv[1]);
  else
     loop = 3000;

  if (argc > 2)
     debug = 1;

  bigsize += BIGINCREASE;
  big = malloc (bigsize);
  if (big == NULL)
     printf ("failure %d could not allocate size %lu\n",
             ++malloc_failure, bigsize);
  if (debug)
     printf("big 0x%p\n", big);

  for (i = 0; i < loop; i++)
    {
      bigsize += BIGINCREASE;
      newbig = malloc(bigsize);
      if (newbig == NULL)
         printf ("failure %d could not allocate size %lu\n",
                 ++malloc_failure, bigsize);
      free (big);
      big = newbig;
      if (debug)
         printf("big 0x%p\n", big);
    }

  printf ("after %d loops, last size block requested %lu\n", loop, bigsize);
  // verify if superblock fragmentation occurred
  // We consider that an arena of up to 3 times more than bigsize is ok.
  {
#if defined(HAVE_MALLINFO)
     struct mallinfo mallinfo_result;
     mallinfo_result = mallinfo();
     // Under valgrind, hblkhd is 0 : all the space is in arena.
     // Under native linux, some space is counted hblkhd.
     if (malloc_failure > 0)
        printf ("%d mallocs failed, below output is doubful\n", malloc_failure);
     if (mallinfo_result.arena + mallinfo_result.hblkhd > 3 * bigsize)
        printf("unexpected heap fragmentation %lu\n",
               (unsigned long) mallinfo_result.arena 
               + (unsigned long) mallinfo_result.hblkhd);
     else
#endif
        printf("reasonable heap usage\n");
  }

  if (debug)
     stats ("before freeing last block");
  free (big);
  if (debug)
     stats ("after freeing last block");

  return 0;
}
