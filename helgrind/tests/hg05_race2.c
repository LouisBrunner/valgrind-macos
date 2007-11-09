/* A simple race - test symaddr */

#include <pthread.h>
#include <unistd.h>

struct foo {
	struct bar {
		int plop[22];
		char biff;
	} poot[11];
};

static void *th(void *v)
{
	struct foo *f = (struct foo *)v;

	f->poot[5].plop[11]++;

	return 0;
}

int main()
{
	struct foo foo;
	pthread_t a, b;

	pthread_create(&a, NULL, th, &foo);	
	sleep(1);		/* force ordering */
	pthread_create(&b, NULL, th, &foo);

	pthread_join(a, NULL);
	pthread_join(b, NULL);

	return 0;
}
