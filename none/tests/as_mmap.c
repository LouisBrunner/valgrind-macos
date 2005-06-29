#include <sys/mman.h>
#include <stdio.h>

int main()
{
	char local;
	char *top = (char *)(((unsigned long)&local + 0x0fffffff) & ~0x0fffffff);

        fprintf(stderr, "1\n");
	if (mmap((void *)0x00000000, 0x10000, PROT_READ|PROT_WRITE, 
		 MAP_PRIVATE|MAP_ANON|MAP_FIXED, -1, 0) == MAP_FAILED)
		fprintf(stderr, "mmap @ 0x00000000\n");

        fprintf(stderr, "2\n");
	if (mmap((void *)0x00010000, 0x10000, PROT_READ|PROT_WRITE, 
		 MAP_PRIVATE|MAP_ANON|MAP_FIXED, -1, 0) == MAP_FAILED)
		fprintf(stderr, "mmap @ 0x00010000\n");

        fprintf(stderr, "3\n");
	if (mmap((void *)0x50000000, 0x10000, PROT_READ|PROT_WRITE, 
		 MAP_PRIVATE|MAP_ANON|MAP_FIXED, -1, 0) == MAP_FAILED)
		fprintf(stderr, "mmap @ 0x50000000\n");

        fprintf(stderr, "4\n");
	if (mmap(top, 0x10000, PROT_READ|PROT_WRITE, 
		 MAP_PRIVATE|MAP_ANON|MAP_FIXED, -1, 0) == MAP_FAILED)
		fprintf(stderr, "mmap @ top\n");

        fprintf(stderr, "5\n");
	if (mmap(top+0x08000000, 0x10000, PROT_READ|PROT_WRITE, 
		 MAP_PRIVATE|MAP_ANON|MAP_FIXED, -1, 0) == MAP_FAILED)
		fprintf(stderr, "mmap @ top+.5G\n");

	return 0;
}
