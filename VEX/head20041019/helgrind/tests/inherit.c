/* test child thread inheriting data */

// ***
//
// Helgrind should detect an error on line 48 for this test, but it doesn't!
//
// ***

#include <pthread.h>
#include <unistd.h>

static volatile int shared[2];

static void *t1(void *v)
{
	volatile int *ip = (int *)v;
	*ip += 44;
	*ip *= 2;
	sleep(1);
	return 0;
}

static void *t2(void *v)
{
	volatile int *ip = (int *)v;
	*ip += 88;
	*ip *= 3;
	sleep(2);
	return 0;
}

int main()
{
	pthread_t a, b;
	volatile int ret = 0;

	sleep(0);

	shared[0] = 22;
	shared[1] = 77;

	pthread_create(&a, NULL, t1, (void *)&shared[0]);	
	pthread_create(&b, NULL, t2, (void *)&shared[1]);

	pthread_join(a, NULL);

	ret += shared[0];	/* no error - a is finished */
	ret += shared[1];	/* expect error - b has not finished,
				   so we can't touch shared[1] yet */

	pthread_join(b, NULL);


	return ret;
}
