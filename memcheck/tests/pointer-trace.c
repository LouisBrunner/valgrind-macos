/* 
   Make sure that leak-check's pointer tracing avoids traps
 */

#include <stdio.h>
#include "memcheck/memcheck.h"
#include <sys/mman.h>
#include <stdlib.h>
#include <fcntl.h>
#include <unistd.h>

#if !defined(MAP_NORESERVE)
#  define MAP_NORESERVE 0
#endif

int main()
{
	char **volatile ptrs;
	int i;
	int fd;
	char *map;

	/* I _think_ the point of this is to fill ptrs with a pointer
	   to every 4th page in the entire address space, hence
	   guaranteeing that at least one of them points into one of
	   the below traps, and so checks that the leak checker
	   doesn't bomb when following them.  That's fine on 32-bit
	   platforms, but hopeless for a 64-bit system.  So the
	   following settings do achieve that on a 32-bit target but
	   merely make a 64-bit target give the same output without
	   properly testing it. */
        int ptrbits, stepbits, stepsize, nptrs;
	if (sizeof(void*) == 8) {
           /* 64-bit machine */
	   ptrbits = 32;   //bogus
           stepbits = 14+1;  //bogus
	   stepsize = (1 << stepbits);
	   nptrs = 1 << (ptrbits - stepbits);
	} else {
           /* 32-bit machine */
	   ptrbits = 32;
           stepbits = 14;
	   stepsize = (1 << stepbits);
	   nptrs = 1 << (ptrbits - stepbits);
	}

	ptrs = malloc(nptrs * sizeof(char *));
	for(i = 0; i < nptrs; i++)
		ptrs[i] = (char *)((long)i << stepbits);

	/* lay some traps */
	map = mmap(0, stepsize * 2, PROT_NONE, MAP_PRIVATE|MAP_NORESERVE|MAP_ANONYMOUS, -1, 0);
	if (map == (char *)-1)
		perror("trap 1 failed");

	map = mmap(0, stepsize * 2, PROT_WRITE, MAP_PRIVATE|MAP_NORESERVE|MAP_ANONYMOUS, -1, 0);
	if (map == (char *)-1)
		perror("trap 2 failed");

	/* non-zero mmap of a zero-length file -> SIGBUS */
	fd = open("./pointer-trace-test-file", O_RDWR | O_CREAT | O_EXCL, 0600);
	unlink("./pointer-trace-test-file");

	map = mmap(0, stepsize * 2, PROT_WRITE|PROT_READ, MAP_PRIVATE, fd, 0);
	if (map == (char *)-1)
		perror("trap 3 failed");
	//printf("trap 3 = %p-%p\n", map, map+stepsize*2);

	map = mmap(0, 256*1024, PROT_NONE, MAP_PRIVATE|MAP_NORESERVE|MAP_ANONYMOUS, -1, 0);
	if (map == (char *)-1)
		perror("trap 4 failed");
	else {
		munmap(map, 256*1024);
		VALGRIND_MAKE_MEM_DEFINED(map, 256*1024); /* great big fat lie */
	}

	VALGRIND_DO_LEAK_CHECK;

	ptrs = 0;

	return 0;
}
