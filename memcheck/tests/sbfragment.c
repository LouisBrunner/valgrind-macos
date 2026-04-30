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
  printf("%10lu int arena;    /* non-mmapped space allocated from system */\n", (unsigned long int)mallinfo_result.arena);
  printf("%10lu int ordblks;  /* number of free chunks */\n", (unsigned long int)mallinfo_result.ordblks);
  printf("%10lu int smblks;   /* number of fastbin blocks */\n", (unsigned long int)mallinfo_result.smblks);
  printf("%10lu int hblks;    /* number of mmapped regions */\n", (unsigned long int)mallinfo_result.hblks);
  printf("%10lu int hblkhd;   /* space in mmapped regions */\n", (unsigned long int)mallinfo_result.hblkhd);
  printf("%10lu int usmblks;  /* maximum total allocated space */\n", (unsigned long int)mallinfo_result.usmblks);
  printf("%10lu int fsmblks;  /* space available in freed fastbin blocks */\n", (unsigned long int)mallinfo_result.fsmblks);
  printf("%10lu int uordblks; /* total allocated space */\n", (unsigned long int)mallinfo_result.uordblks);
  printf("%10lu int fordblks; /* total free space */\n", (unsigned long int)mallinfo_result.fordblks);
  printf("%10lu int keepcost; /* top-most, releasable (via malloc_trim) space */\n", (unsigned long int)mallinfo_result.keepcost);
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
