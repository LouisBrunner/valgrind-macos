#include <stdio.h>
#include <sys/mman.h>
#include <stdlib.h>
#include <unistd.h>

static unsigned int pagesize;

#define PAGES	1024u
#define LEN	(PAGES*pagesize)

static void *domap(void)
{
	void *ret = mmap(0, LEN, PROT_READ|PROT_WRITE, MAP_PRIVATE|MAP_ANONYMOUS, -1, 0);

	if (ret == (void *)-1) {
		perror("mmap");
		exit(1);
	}

	return ret;
}

/* unmap in pieces to exercise munmap more */
static void nibblemap(void *p)
{
	int off;
	int i;

	off = (random() & ~0x1fff) % LEN;
	
	for(i = 0; i < PAGES; i++) {
		munmap((char *)p + off, pagesize);
		off += 619*pagesize;
		off %= LEN;
	}
}

int main()
{
	int i;
	void *expect1, *expect2;

	pagesize = getpagesize();

	expect1 = domap();
	expect2 = domap();
	munmap(expect1, LEN);
	munmap(expect2, LEN);
	for(i = 0; i < 1000; i++) {
		void *m1, *m2;

		m1 = domap();
		if (m1 != expect1) {
			printf("FAIL: m=%p expect=%p\n",
			       m1, expect1);
			return 1;
		}
		m2 = domap();
		if (m2 != expect2) {
			printf("FAIL: m=%p expect=%p\n",
			       m2, expect2);
			return 1;
		}
		nibblemap(m2);
		munmap(m1, LEN);
	}
	
	printf("PASS\n");
	return 0;
}
