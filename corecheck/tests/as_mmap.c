#include <sys/mman.h>
#include <stdio.h>

int main()
{
	char local;
	char *top = (char *)(((unsigned long)&local + 0x0fffffff) & ~0x0fffffff);

	if (mmap((void *)0x00000000, 0x10000, PROT_READ|PROT_WRITE, 
		 MAP_PRIVATE|MAP_ANON|MAP_FIXED, -1, 0) == MAP_FAILED)
		perror("mmap @ 0x00000000");
	if (mmap((void *)0x00010000, 0x10000, PROT_READ|PROT_WRITE, 
		 MAP_PRIVATE|MAP_ANON|MAP_FIXED, -1, 0) == MAP_FAILED)
		perror("mmap @ 0x00010000");
	if (mmap((void *)0x50000000, 0x10000, PROT_READ|PROT_WRITE, 
		 MAP_PRIVATE|MAP_ANON|MAP_FIXED, -1, 0) == MAP_FAILED)
		perror("mmap @ 0x50000000");
	if (mmap(top, 0x10000, PROT_READ|PROT_WRITE, 
		 MAP_PRIVATE|MAP_ANON|MAP_FIXED, -1, 0) == MAP_FAILED)
		perror("mmap @ top");
	if (mmap(top+0x08000000, 0x10000, PROT_READ|PROT_WRITE, 
		 MAP_PRIVATE|MAP_ANON|MAP_FIXED, -1, 0) == MAP_FAILED)
		perror("mmap @ top+.5G");

	return 0;
}
