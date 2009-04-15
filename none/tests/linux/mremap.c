#define _GNU_SOURCE
#include "tests/sys_mman.h"
#include <stdio.h>
#include <stdlib.h>

static char *mkmap(unsigned sz)
{
	static char *map;
	static unsigned mapsz;
	char *p;

	if (map != NULL)
		munmap(map, mapsz);

	p = (char *)mmap(0, sz, PROT_READ|PROT_WRITE, MAP_PRIVATE|MAP_ANONYMOUS, -1, 0);
	
	if (p == (char *)-1) {
		perror("mmap");
		exit(1);
	}

	map = p;
	mapsz = sz;

	return p;
}


int main()
{
	char *np;
	char *p;

	p = mkmap(1024*1024);
	np = mremap(p, 1024*1024, 256*1024, 0);	/* shrink, fixed */	
	if (np == (char *)-1)
		perror("mremap(shrink, fixed)");
	if (np != p)
		fprintf(stderr, "shrink, nomove: p=%p np=%p: shrink moved?!\n",
			p, np);
	if (np != (char *)-1)
		munmap(np, 256*1024);

	p = mkmap(1024*1024);
	np = mremap(p, 1024*1024, 256*1024, MREMAP_MAYMOVE);	/* shrink, maymove */	
	if (np == (char *)-1)
		perror("mremap(shrink, maymove)");
	if (np != p)
		fprintf(stderr, "shrink, maymove: p=%p np=%p: shrink moved?!\n",
			p, np);
	if (np != (char *)-1)
		munmap(np, 256*1024);

	p = mkmap(1024*1024);
	np = mremap(p, 1024*1024, 2048*1024, 0); /* grow, fixed */
	if (np == (char *)-1)
		perror("mremap(grow, fixed)");
	if (np != p)
		fprintf(stderr, "grow, nomove: p=%p np=%p: shrink moved?!\n",
			p, np);
	if (np != (char *)-1)
		munmap(np, 2048*1024);
	
	p = mkmap(1024*1024);
	np = mremap(p, 1024*1024, 2048*1024, MREMAP_MAYMOVE); /* grow, maymove */
	if (np == (char *)-1)
		perror("mremap(grow, maymove)");
	if (np != p)
		fprintf(stderr, "grow, maymove: p=%p np=%p: shrink moved?!\n",
			p, np);
	if (np != (char *)-1)
		munmap(np, 2048*1024);
	
	p = mkmap(1024*1024);
	munmap(p+512*1024, 4096);
	np = mremap(p, 512*1024, 1024*1024, 0); /* grow, nomove, constrained */
	if (np == (char *)-1)
		perror("mremap(grow, nomove, constrained)");
	else if (np == p)
		fprintf(stderr, "grow, maymove, constrained: p=%p np=%p (managed to grow without moving?!)\n",
			p, np);
	if (np != (char *)-1)
		munmap(np, 1024*1024);
	
	p = mkmap(1024*1024);
	munmap(p+512*1024, 4096);

	np = mremap(p, 512*1024, 1024*1024, MREMAP_MAYMOVE); /* grow, maymove, constrained */
	if (np == (char *)-1)
		perror("mremap(grow, maymove, constrained)");
	if (np == p)
		fprintf(stderr, "grow, maymove, constrained: p=%p np=%p (managed to grow without moving?!)\n",
			p, np);
	if (np != (char *)-1)
		munmap(np, 1024*1024);

	return 0;
}
