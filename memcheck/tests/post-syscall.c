#include <time.h>
#include <unistd.h>
#include <stdio.h>
#include <errno.h>
#include <signal.h>

#include "../memcheck.h"

/* Check that a syscall's POST function gets called if it completes
   due to being interrupted.  nanosleep is used here, because it
   writes a result even if it fails.  wait*() could also be used,
   because they successully complete if interrupted by SIGCHLD.
 */
static void handler(int s)
{
}

int main()
{
	struct timespec req, rem;
	int ret;

	req.tv_sec = 2;
	req.tv_nsec = 0;

	signal(SIGALRM, handler);

	alarm(1);
	ret = nanosleep(&req, &rem);
	
	if (ret != -1 || errno != EINTR) {
		fprintf(stderr, "FAILED: expected nanosleep to be interrupted\n");
	} else {
		VALGRIND_CHECK_VALUE_IS_DEFINED(rem);
		fprintf(stderr, "PASSED\n"); /* assuming CHECK_VALUE_IS_DEFINED doesn't print anything */
	}

	return 0;
}
