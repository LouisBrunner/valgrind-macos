/* All OK */

#include <pthread.h>

static pthread_mutex_t mx = PTHREAD_MUTEX_INITIALIZER;

static int shared;

static void *th(void *v)
{
	pthread_mutex_lock(&mx);
	shared++;
	pthread_mutex_unlock(&mx);

	return 0;
}

int main()
{
	pthread_t a, b;

	pthread_mutex_lock(&mx);
	pthread_mutex_unlock(&mx);

	pthread_create(&a, NULL, th, NULL);	
	pthread_create(&b, NULL, th, NULL);

	pthread_join(a, NULL);
	pthread_join(b, NULL);

	return 0;
}
