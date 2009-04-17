/* 
   Check that a fault signal handler gets the expected info
 */
#include <signal.h>
#include <stdio.h>
#include <stdlib.h>
#include <fcntl.h>
#include <setjmp.h>
#include <unistd.h>
#include "tests/asm.h"
#include "tests/sys_mman.h"

struct test {
	void (*test)(void);
	int sig;
	int code;
	volatile void *addr;
};

static const struct test *cur_test;



static jmp_buf escape;

#define BADADDR	((int *)0x1234)

#define FILESIZE	(16*1024)
#define MAPSIZE		(2*FILESIZE)

static char volatile *volatile mapping;

static int testsig(int sig, int want)
{
	if (sig != want) {
		fprintf(stderr, "  FAIL: expected signal %d, not %d\n", want, sig);
		return 0;
	} 
	return 1;
}

static int testcode(int code, int want)
{
	if (code != want) {
		fprintf(stderr, "  FAIL: expected si_code==%d, not %d\n", want, code);
		return 0;
	}
	return 1;
}

static int testaddr(void *addr, volatile void *want)
{
	if (addr != want) {
		fprintf(stderr, "  FAIL: expected si_addr==%p, not %p\n", want, addr);
		return 0;
	}
	return 1;

}

static void handler(int sig, siginfo_t *si, void *uc)
{
	int ok = 1;

	ok = ok && testsig(sig, cur_test->sig);
	ok = ok && testcode(si->si_code, cur_test->code);
	if (cur_test->addr)
		ok = ok && testaddr(si->si_addr, cur_test->addr);

	if (ok)
		fprintf(stderr, "  PASS\n");

	siglongjmp(escape, ok + 1);
}


extern char test1_ill;
static void test1()
{
	asm volatile(VG_SYM(test1_ill) ": ud2");
}

static void test2()
{
	asm volatile ("int3");
}

static void test3()
{
	asm volatile ("int $0x10");
}

static void test4()
{
	volatile int a;
	asm volatile ("add $1, %0;"/* set OF */
		      "into"
		      : "=a" (a) : "0" (0x7fffffff) : "cc");
}

static void test5()
{
	static int limit[2] = { 0, 10 };

	asm volatile ("bound %0, %1" : : "r" (11), "m" (limit[0]));
}

int main()
{
	int fd, i;
	static const int sigs[] = { SIGSEGV, SIGILL, SIGBUS, SIGFPE, SIGTRAP };
	struct sigaction sa;

	sa.sa_sigaction = handler;
	sa.sa_flags = SA_SIGINFO;
	sigfillset(&sa.sa_mask);
	
	for(i = 0; i < sizeof(sigs)/sizeof(*sigs); i++)
		sigaction(sigs[i], &sa, NULL);

	fd = open("faultstatus.tmp", O_CREAT|O_TRUNC|O_EXCL, 0600);
	if (fd == -1) {
		perror("tmpfile");
		exit(1);
	}
	unlink("faultstatus.tmp");
	ftruncate(fd, FILESIZE);

	mapping = mmap(0, MAPSIZE, PROT_READ, MAP_PRIVATE, fd, 0);
	close(fd);

	{
		const struct test tests[] = {
#define T(n, sig, code, addr) { test##n, sig, code, addr }
			T(1, SIGILL,	ILL_ILLOPN,     &test1_ill),

			T(2, SIGTRAP,	128,		0), /* TRAP_BRKPT? */
			T(3, SIGSEGV,	128,		0),
			T(4, SIGSEGV,   128,		0),

			/* This is an expected failure - Valgrind
			   doesn't implement the BOUND instruction,
			   and so issues a SIGILL instead. */
			T(5, SIGSEGV,   128,		0),
#undef T
		};

		for(i = 0; i < sizeof(tests)/sizeof(*tests); i++) {
			cur_test = &tests[i];
		
			if (sigsetjmp(escape, 1) == 0) {
				fprintf(stderr, "Test %d: ", i+1);
				tests[i].test();
				fprintf(stderr, "  FAIL: no fault, or handler returned\n");
			}
		}
	}

	return 0;
}

