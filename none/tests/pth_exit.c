#include <pthread.h>
#include <unistd.h>

static void *th(void *v)
{
	sleep(1);
	pthread_exit(0);
}

int main()
{
	pthread_t a;

	pthread_create(&a, NULL, th, NULL);
	pthread_create(&a, NULL, th, NULL);
	pthread_create(&a, NULL, th, NULL);
	pthread_create(&a, NULL, th, NULL);

	pthread_exit(0);
}
