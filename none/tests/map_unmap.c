#include <stdio.h>
#include "tests/sys_mman.h"
#include <stdlib.h>
#include <unistd.h>

/* The code testing MAP_HUGETLB huge pages is disabled by default,
   as many distros do not have huge pages configured
   by default.
   To have e.g. 20 huge pages configured, do (as root)
      echo 20 > /proc/sys/vm/nr_hugepages
  Once this is done, uncomment the below, and recompile.
*/
//#define TEST_MAP_HUGETLB 1

/* Similarly, testing SHM_HUGETLB huge pages is disabled by default.
   To have shmget/shmat big pages working, do (as root)
      echo 500 > /proc/sys/vm/hugetlb_shm_group
   where 500 is the groupid of the user that runs this test
  Once this is done, uncomment the below, and recompile.
*/
//#define TEST_SHM_HUGETLB 1

// Size to use for huge pages
#define HUGESZ (4 * 1024 * 1024)

#ifdef TEST_MAP_HUGETLB
/* Ensure this compiles on pre 2.6 systems, or on glibc missing MAP_HUGETLB */
#ifndef MAP_HUGETLB
/* The below works for me on an f12/x86 linux */
#define MAP_HUGETLB 0x40000
#endif

#endif /* TEST_MAP_HUGETLB */

#ifdef TEST_SHM_HUGETLB
#include <sys/ipc.h>
#include <sys/shm.h>
#include <sys/stat.h>
#ifndef SHM_HUGETLB
#define SHM_HUGETLB 04000
#endif
#endif  /* TEST_SHM_HUGETLB */

static unsigned int pagesize;

#define PAGES	1024u
#define LEN	(PAGES*pagesize)

static void *domap(size_t len, int addflags)
{
	void *ret = mmap(0, len, PROT_READ|PROT_WRITE, MAP_PRIVATE|MAP_ANONYMOUS|addflags, -1, 0);

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

	off = (random() % LEN) & ~(pagesize-1);
	
	for(i = 0; i < PAGES; i++) {
		/* printf("unmapping off=%d\n", off/pagesize); */
		munmap((char *)p + off, pagesize);
		off += 619*pagesize;
		off %= LEN;
	}
}

static void prmaps()
{
	char buf[100];
	sprintf(buf, "/bin/cat /proc/%ld/maps", (long) getpid());
	system(buf);
	exit(1);
}

int main()
{
	int i;
	void *expect1, *expect2;

	pagesize = getpagesize();

	expect1 = domap(LEN, 0);
	expect2 = domap(LEN, 0);
	munmap(expect1, LEN);
	munmap(expect2, LEN);

	for(i = 0; i < 5; i++) {
		void *m1, *m2;

		m1 = domap(LEN, 0);
		if (m1 != expect1) {
			printf("FAIL i=%d: m1=%p expect1=%p\n",
			       i, m1, expect1);
			prmaps();
			return 1;
		}
		m2 = domap(LEN, 0);
		if (m2 != expect2) {
			printf("FAIL i=%d: m2=%p expect2=%p\n",
			       i, m2, expect2);
			prmaps();
			return 1;
		}
		nibblemap(m2);
		munmap(m1, LEN);
	}

#ifdef  TEST_MAP_HUGETLB
        {
           void *expect3;
           expect3 = domap(HUGESZ, MAP_HUGETLB);
           munmap(expect3, HUGESZ);
        }
#endif

#ifdef TEST_SHM_HUGETLB
        {
           int shmid;
           void *expect4;


           shmid = shmget(IPC_PRIVATE, HUGESZ, 
                          IPC_CREAT | IPC_EXCL | S_IRUSR | S_IWUSR | SHM_HUGETLB);
           if (shmid == -1) {
              perror("shmget");
              exit(1);
           }
           expect4 = shmat(shmid, NULL, 0);
           if (expect4 == (void*) -1){
              perror("shmat");
              exit(1);
           }
           if (shmdt(expect4) != 0) {
              perror("shmdt");
              exit(1);
           }
           if (shmctl(shmid, IPC_RMID, 0) != 0) {
              perror("shmctl IPC_RMID");
              exit(1);
           }         
        }
#endif

	printf("PASS\n");
	return 0;
}
