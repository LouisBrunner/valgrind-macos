#include <unistd.h>
#include <signal.h>
#include <errno.h>
#include <string.h>
#include <stdio.h>

int main(int argc, char **argv)
{
	if (argc == 1) {
		sigset_t all;


		sigfillset(&all);
		sigprocmask(SIG_SETMASK, &all, NULL);

		execl(argv[0], argv[0], "test", NULL);

		fprintf(stderr, "FAILED: execl failed with %s\n",
			strerror(errno));
		return 1;
	} else {
		sigset_t mask;
		int i;

		sigprocmask(SIG_SETMASK, NULL, &mask);

		for(i = 1; i < NSIG; i++) {
			if (i == SIGKILL || i == SIGSTOP)
				continue;

			if (!sigismember(&mask, i))
				printf("signal %d missing from mask\n", i);
		}
	}

	return 0;
}
