#include <config.h>
#include <pthread.h>
#include <stdio.h>
#include <unistd.h>
#include <time.h>

#ifdef HAVE_TLS

#define COUNT 10

static int race;
static __thread int local;
__thread int global;
extern __thread int static_extern;
extern __thread int so_extern;

/* deliberate failure */
static int *test_race(void)
{
	return &race;
}

static int *test_local(void)
{
	return &local;
}

static int *test_global(void)
{
	return &global;
}

static int *test_static_extern(void)
{
	return &static_extern;
}

static int *test_so_extern(void)
{
	return &so_extern;
}

static const struct timespec awhile = { 0, 100000000 };

typedef int *(*func_t)(void);
struct testcase {
	const char *name;
	func_t func;
};

static void *tls_ptr(void *p)
{
	struct testcase *test = (struct testcase *)p;
	int *ip = (*test->func)();
	int here = 0;
	int i;

	for(i = 0; i < COUNT; i++) {
		int a = (*ip)++;
		int b = here++;
		if (a != b)
			printf("tls_ptr: case \"%s\" has mismatch: *ip=%d here=%d\n",
			       test->name, a, b);
		nanosleep(&awhile, 0);
	}

	return 0;
}

int *test_so_extern(void);
int *test_so_local(void);
int *test_so_global(void);

static const struct testcase tests[] = {
#define T(t)	{ #t, test_##t }
	T(race),
	T(local),
	T(global),
	T(static_extern),
	T(so_extern),
	T(so_local),
	T(so_global),
#undef T
};

#define NTESTS	(sizeof(tests)/sizeof(*tests))

int main()
{
	pthread_t threads[NTESTS*2];
	int curthread = 0;
	static 
	int i;

	for(i = 0; i < NTESTS; i++) {
		pthread_create(&threads[curthread++], NULL, tls_ptr, (void *)&tests[i]);
		pthread_create(&threads[curthread++], NULL, tls_ptr, (void *)&tests[i]);
	}

	for(i = 0; i < curthread; i++)
		pthread_join(threads[i], NULL);

	return 0;
}
#else
int main()
{
	printf("FAILED: no compiler support for __thread\n");
	return 1;
}
#endif
