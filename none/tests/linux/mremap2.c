#define _GNU_SOURCE

#include <stdio.h>

#include "tests/sys_mman.h"
#include <assert.h>
#include <stdlib.h>
#include <sys/types.h>
#include <unistd.h>
#include <errno.h>
#include <syscall.h>


#ifndef REMAP_FIXED
#define MREMAP_FIXED 2
#endif


static int PAGE;

void mapanon_fixed ( void* start, size_t length )
{
  void* r = mmap(start, length, PROT_NONE, 
                 MAP_FIXED|MAP_PRIVATE|MAP_ANONYMOUS, 0,0);
  assert(r != MAP_FAILED);
  assert(r == start);
}

void unmap_and_check ( void* start, size_t length )
{
   int r = munmap( start, length );
   assert(r == 0);
}

char* workingarea = NULL;
char* try_dst     = NULL;

// set up working area so expansion limit is 20*PAGE
//
//   |   10   |   20   |   10   |   60   |
//   |  pre   |  src   |  FREE  |  post  |
//
//  A suitable attempted fixed dst is workingarea + 150*PAGE.

char* setup ( void* other_stuff, int other_len )
{
  if (!workingarea) {
     workingarea = mmap(0, 200*PAGE, PROT_NONE, 
                           MAP_ANONYMOUS|MAP_PRIVATE, 0,0);
     assert(workingarea);
     try_dst = workingarea + 150*PAGE;
     unmap_and_check(workingarea, 200*PAGE);
  }

  if (other_stuff) {
    unmap_and_check(other_stuff, other_len);
  }

  // get rid of the old working area
  unmap_and_check( workingarea, 200*PAGE);

  // pre block
  mapanon_fixed( workingarea + 0*PAGE, 9*PAGE);

  // the area
  mapanon_fixed( workingarea + 10*PAGE, 20*PAGE );

  // upper half
  mapanon_fixed( workingarea + 40*PAGE, 60*PAGE );

  return workingarea + 10*PAGE;
}

/* show the working area */
void show ( void )
{
  int i,r __attribute__((unused));
  for (i = 0; i < 200; i++) {
    r = mprotect( workingarea + i * PAGE, PAGE, PROT_NONE );
    // We used to print 'X' or '.' according to the mprotect result, but the
    // results are too variable and the test was never reliable.  So now we
    // just always print '.'.  At least this test gives mremap a thorough
    // working out and so will detect egregious problems like crashes.
    //printf("%c", r == 0 ? 'X' : '.');
    printf(".");
    if (i == 49 || i == 99 || i == 149) printf("\n");
  }
  printf("\n");
}


char* dst = NULL;
char* src = NULL;
char* dst_impossible = NULL;


char* identify ( char* p )
{
  if (p == dst)            return "dst";
  if (p == src)            return "src";
  if (p == dst_impossible) return "dst_imp!";
  if (p == try_dst)        return "dst_poss";
  return "other";
}

int main ( void )
{
  int alocal, maymove, fixed, nsi, dstpossible;
  int newsizes[6] = { 19, 20, 21, 29, 30, 31 };

  char* tidythis = NULL;
  int  tidylen = 0;
  int firsttime = 1;
  char buf[100];

  dst_impossible = (char*)(&alocal) + 500 * 1000 * 1000;

  PAGE = sysconf(_SC_PAGESIZE);

  for (maymove = 0; maymove <= 1 ; maymove++) {
  for (fixed = 0; fixed <= 1; fixed++) {
    printf("\n");
  for (nsi = 0; nsi < 6; nsi++) {
  for (dstpossible = 0; dstpossible <= 1; dstpossible++) {

    char* r;
    int newsize = newsizes[nsi] * PAGE;
    int flags = (maymove ? MREMAP_MAYMOVE : 0)  |
                (fixed ? MREMAP_FIXED : 0);
    dst = dstpossible ? try_dst : dst_impossible;
    src = setup( tidythis, tidylen );

    if (firsttime) {
       printf("dst_possible   = %p\n", try_dst );
       printf("dst_impossible = %p\n", dst_impossible );
       printf("           src = %p\n", src);
       printf("\n");
       sprintf(buf, "cat /proc/%d/maps", getpid());
       if (0) system(buf);
       firsttime = 0;
    }

    printf("maymv %d   fixed %d   newsz %2d   dstpo %d  dst %p ->  ",
	   maymove, fixed, newsizes[nsi], dstpossible, dst );
    r = (char*)
        syscall(__NR_mremap, src, 20*PAGE, newsize, flags, dst, 0 );
    // We used to print the address or error, but that was also unreliable.
    //if (r == MAP_FAILED)
    //  printf("error %d\n", errno);
    //else
    //  printf("%p (== %s)\n", r, identify(r));
    printf("\n");

    if (1) {
       show();
       printf("\n");
    }

    if (r != MAP_FAILED) {
      if (r != src && r != try_dst && r != dst_impossible) {
	tidythis = r;
	tidylen = newsize;
      }
    }

  }
  }
  }
  }
  return 0;
}
