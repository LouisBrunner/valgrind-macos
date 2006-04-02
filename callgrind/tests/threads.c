/* A simple example with 4 threads */

#include <pthread.h>
#include <unistd.h>

double a[1000];

static void init()
{
	int i;
	for(i=0;i<1000;i++) a[i] = (double)i;
}

static void *th(void *v)
{
	double sum = 0.0;
	int i,j;

	for(j=0;j<1000;j++)
		for(i=0;i<1000;i++)
			sum += a[i];

	*( (double*)v ) = sum;

	/* make sure that no threads is so fast that it finishes
         * before last thread is created, thus reusing the TID */
	sleep(1);

	return 0;
}

int main()
{
	pthread_t t[4];
	double sum[4];
	int i;

	init();

	for(i=0;i<4;i++)
		pthread_create(&t[i], NULL, th, &sum[i]);	

	for(i=0;i<4;i++)
		pthread_join(t[i], NULL);

	return 0;
}
