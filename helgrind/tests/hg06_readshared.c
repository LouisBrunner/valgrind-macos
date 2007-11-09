/* All OK - test allowed read sharing */

#include <pthread.h>
#include <assert.h>

static int shared;

static void *t1(void *v)
{
	return (void *)(long)(shared + 44);
}

static void *t2(void *v)
{
	return (void *)(long)(shared + 55);
}

int main()
{
	pthread_t a, b;

	shared = 22;

	pthread_create(&a, NULL, t1, NULL);	
	pthread_create(&b, NULL, t2, NULL);

	pthread_join(a, NULL);
	pthread_join(b, NULL);

	assert(shared == 22);

	return 0;
}
