#include <signal.h>
#include <stdio.h>
#include <stdlib.h>
#include <sys/types.h>
#include <unistd.h>

static char *deep;

#define SIZE	(4*1024*1024)

static void handler(int sig)
{
	char here;

	if (&here < deep) {
		printf("PASSED\n");
		exit(0);
	}

	kill(getpid(), SIGUSR1);
}

int main()
{
	struct sigaction sa;

	char here;
	deep = &here - SIZE;

	sa.sa_handler = handler;
	sa.sa_flags = SA_NOMASK;
	sigemptyset(&sa.sa_mask);
	
	sigaction(SIGUSR1, &sa, NULL);

	kill(getpid(), SIGUSR1);

	printf("FAILED\n");
	exit(1);
}
