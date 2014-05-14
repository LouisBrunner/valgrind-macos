/* All OK */
#include <stdlib.h>
#include <string.h>
#include <pthread.h>

static pthread_mutex_t mx = PTHREAD_MUTEX_INITIALIZER;

static int shared = 0;
static char *ptr;

static void breakme(void)
{
   if (shared == 1)
      memset (ptr, 0x55, 1000);
}

static void *th(void *v)
{
	pthread_mutex_lock(&mx);
	shared++;
        if (shared == 1) {
           ptr = malloc (1008);
           breakme();
        }
        if (shared == 2) {
           free (ptr);
           breakme();
        }
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
