/* A simple race */

#include <pthread.h>
#include <unistd.h>

static int shared;

static void *th(void *v)
{
	shared++;

	return 0;
}

int main()
{
	const struct timespec delay = { 0, 100 * 1000 * 1000 };
	pthread_t a, b;

	pthread_create(&a, NULL, th, NULL);	
	nanosleep(&delay, 0);	/* force ordering */
	pthread_create(&b, NULL, th, NULL);

	nanosleep(&delay, 0);	/* avoid false ordering between threads */

	pthread_join(a, NULL);
	pthread_join(b, NULL);

	return 0;
}
