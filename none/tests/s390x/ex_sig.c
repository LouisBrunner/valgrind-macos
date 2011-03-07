#include <features.h>
#include <fpu_control.h>
#include <signal.h>
#include <sys/types.h>
#include <signal.h>
#include <stdio.h>
#include <stdlib.h>
#include <ucontext.h>
#include <unistd.h>

char source[40] = "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\0";
char target[40] = "                                       \0";

void handle_SIG(int sig)
{
	static int counter;
	char buf2[40];

	counter++;
	asm volatile(	"larl 1,1f\n"
			"ex 0,0(1)\n"
			"j 2f\n"
			"1: mvc 0(30,%0),0(%1)\n"
			"2:\n"
		::"a" (buf2), "a" (source)
		: "1");
	if (counter == 2) {
		printf("%s\n", target);
		exit(1);
	} else
		alarm(1);
}

int main()
{
	signal(SIGALRM, handle_SIG);
	alarm(1);

	asm volatile(	"larl 1,1f\n"
			"0: ex 0,0(1)\n"
			"j 0b\n"
			"1: mvc 0(20,%0),0(%1)\n"
		::"a" (target), "a" (source)
		: "1");
	exit(0);
}
