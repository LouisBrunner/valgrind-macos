/* Make sure there are no thread lifetime related leaks... */
#include <pthread.h>
#include <stdio.h>

static void *func(void *v)
{
	return NULL;
}

int main()
{
	int i;

	for(i = 0; i < 10000; i++) {
		pthread_t th;

		if (i > 0 && i % 1000 == 0)
			printf("%d...\n", i);

		pthread_create(&th, NULL, func, NULL);
		pthread_join(th, NULL);
	}

	return 0;
}
