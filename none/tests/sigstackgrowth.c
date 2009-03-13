#include <signal.h>
#include <stdio.h>
#include <stdlib.h>
#include <sys/types.h>
#include <unistd.h>

/* What does this test do?  It checks that valgrind's signal frame
   building mechanism can create at least 4MB of signal delivery
   frames, hence that it can actually expand the stack by that much
   when delivering signals.  A fair-enough thing to want to test.

   It does this by getting into the signal handler, and then
   recursively invoking the handler by sending itself the signal
   again, until the stack has grown to 4MB from the starting frame
   (main).

   Consequence is: it is essential that we do not disable delivery of
   further signals within the handler itself, else the kernel will
   wait till the handler exits before delivering the next signal, the
   frame will be cleared, the stack will never grow, and we'll be in
   an infinite loop.

   Hence we *must* give the SA_NODEFER flag when setting up the
   handler.
*/

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
	sa.sa_flags = SA_NODEFER;
	sigemptyset(&sa.sa_mask);
	
	sigaction(SIGUSR1, &sa, NULL);

	kill(getpid(), SIGUSR1);

	printf("FAILED\n");
	exit(1);
}

