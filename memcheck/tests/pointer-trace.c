/* 
   Make sure that leak-check's pointer tracing avoids traps
 */

#include <stdio.h>
#include "memcheck/memcheck.h"
#include <sys/mman.h>
#include <stdlib.h>
#include <fcntl.h>
#include <unistd.h>

int main()
{
	const int ptrbits = sizeof(void *) * 8;
	const int stepbits = 14;
	const int stepsize = (1 << stepbits);
	const int nptrs = 1 << (ptrbits - stepbits);

	char **volatile ptrs;
	int i;
	int fd;
	char *map;

	ptrs = malloc(nptrs * sizeof(char *));
	for(i = 0; i < nptrs; i++)
		ptrs[i] = (char *)(i << stepbits);

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
		VALGRIND_MAKE_READABLE(map, 256*1024); /* great big fat lie */
	}

	VALGRIND_DO_LEAK_CHECK;

	ptrs = 0;

	return 0;
}
