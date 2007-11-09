
/* I don't think this is a very good test .. all this
   sleepery is highly confusing. */

/* test child thread inheriting data */

#include <stdio.h>
#include <pthread.h>
#include <unistd.h>

static volatile int shared[2];

static void *t1(void *v)
{
	volatile int *ip = (int *)v;
	if (0) printf("ta W\n");
	*ip += 44;
	*ip *= 2;
	sleep(1);
	return 0;
}

static void *t2(void *v)
{
	volatile int *ip = (int *)v;
	sleep(2);
	if (0) printf("tb W\n");
	*ip += 88;
	*ip *= 3;
	sleep(1);
	return 0;
}

int main(void)
{
	pthread_t a, b;
	volatile int ret = 0;

	sleep(0);

	shared[0] = 22;
	shared[1] = 77;

	pthread_create(&a, NULL, t1, (void *)&shared[0]);
	// a steals shared[0] from root thread, so is excl(a)	
	pthread_create(&b, NULL, t2, (void *)&shared[1]);
	// b steals shared[1] from root thread, so is excl(b)	

	pthread_join(a, NULL);
	// b's stuff (shared[1]) still belongs to b, so is excl(b)

	// ret is excl(root), and shared[0] is re-acquired as excl(root)
	// since a joined to root
	if (0) printf("r R1\n");
	ret += shared[0];	/* no error - a is finished */

	// but shared[1] is excl(b); hence we're reading excl(b)
	// without a lock and without a dependency edge
	if (0) printf("r R2\n");
	ret += shared[1];	/* expect error - b has not finished,
				   so we can't touch shared[1] yet */

	pthread_join(b, NULL);


	return ret;
}
