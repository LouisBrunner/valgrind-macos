#include <features.h>
#include <signal.h>
#include <sys/types.h>
#include <pthread.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

char source[40] = "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\0";
char target[40] = "                                       \0";

pthread_t thread;

void *threadfunc(void *arg)
{
	char buf2[40];
	int i;

	memset(buf2, 0, sizeof(buf2));
	for (i=0; i<5000; i++)
		asm volatile(
			"lghi 2,0\n"
			"lghi 3,0\n"
			"lgr 4,%0\n"
			"lgr 5,%1\n"
			"larl 1,1f\n"
			"0: ex 0,0(1)\n"
			"j 2f\n"
			"1: mvc 0(30,4),0(5)\n"
			"2:\n"
		::"a" (buf2), "a" (source)
		: "1", "2", "3", "4", "5", "memory");
	printf("%s\n", buf2);
	pthread_exit(0);
}

int main()
{
	int i;

	pthread_create(&thread, NULL, threadfunc, NULL);

	for (i=0; i<5000; i++)
		asm volatile(
			"lghi 4,0\n"
			"lghi 5,0\n"
			"lgr 2,%0\n"
			"lgr 3,%1\n"
			"larl 1,1f\n"
			"0: ex 0,0(1)\n"
			"j 2f\n"
			"1: mvc 0(20,2),0(3)\n"
			"2:\n"
		::"a" (target), "a" (source)
		: "1", "2", "3", "4", "5", "memory");
	pthread_join(thread, NULL);
	printf("%s\n", target);
	pthread_exit(0);
}
