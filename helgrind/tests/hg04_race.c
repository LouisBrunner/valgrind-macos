/* A simple race */

#include <pthread.h>
#include <unistd.h>

static int shared;

__attribute__((noinline))
static void th10(void)
{
   shared++;
}

__attribute__((noinline))
static void th9(void)
{
   th10();
}

__attribute__((noinline))
static void th8(void)
{
   th9();
}

__attribute__((noinline))
static void th7(void)
{
   th8();
}

__attribute__((noinline))
static void th6(void)
{
   th7();
}

__attribute__((noinline))
static void th5(void)
{
   th6();
}

__attribute__((noinline))
static void th4(void)
{
   th5();
}

__attribute__((noinline))
static void th3(void)
{
   th4();
}

__attribute__((noinline))
static void th2(void)
{
   th3();
}


__attribute__((noinline))
static void th1(void)
{
   th2();
}

static void *th(void *v)
{
   th1();

   return 0;
}

int main()
{
	pthread_t a, b;

	pthread_create(&a, NULL, th, NULL);
	sleep(1);		/* force ordering */
	pthread_create(&b, NULL, th, NULL);

	pthread_join(a, NULL);
	pthread_join(b, NULL);

	return 0;
}
