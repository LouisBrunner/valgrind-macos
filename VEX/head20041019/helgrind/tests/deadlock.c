/* Simple possible deadlock */
#include <pthread.h>

static pthread_mutex_t m1 = PTHREAD_MUTEX_INITIALIZER;
static pthread_mutex_t m2 = PTHREAD_MUTEX_INITIALIZER;

static void *t1(void *v)
{
	pthread_mutex_lock(&m1);
	pthread_mutex_lock(&m2);
	pthread_mutex_unlock(&m1);
	pthread_mutex_unlock(&m2);

	return 0;
}

static void *t2(void *v)
{
	pthread_mutex_lock(&m2);
	pthread_mutex_lock(&m1);
	pthread_mutex_unlock(&m1);
	pthread_mutex_unlock(&m2);

	return 0;
}

int main()
{
	pthread_t a, b;
	
	/* prevent spurious messages from the dynamic linker */
	pthread_mutex_lock(&m1);
	pthread_mutex_unlock(&m1);

	pthread_create(&a, NULL, t1, NULL);	
	pthread_create(&b, NULL, t2, NULL);

	pthread_join(a, NULL);
	pthread_join(b, NULL);

	return 0;
}

