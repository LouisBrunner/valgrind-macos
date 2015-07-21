#include <unistd.h>
#include <signal.h>
#include <errno.h>
#include <string.h>
#include <stdio.h>
#include <sys/wait.h>
#include <sys/types.h>
#include <stdlib.h>

static void do_exec(const char *path, const char *arg, const sigset_t *mask)
{
	pid_t pid;
	int status;

	pid = fork();
	if (pid == -1) {
		perror("fork");
		exit(1);
	}
	if (pid == 0) {
		sigprocmask(SIG_SETMASK, mask, NULL);
		execl(path, path, arg, (char *) NULL);
			
		fprintf(stderr, "FAILED: execl failed with %s\n",
			strerror(errno));
	} else {
		int ret;
		do 
			ret = waitpid(pid, &status, 0);
		while(ret == -1 && errno == EINTR);
		if (ret != pid) {
			perror("waitpid");
			exit(1);
		}
		if (status != 0) {
			fprintf(stderr, "child exec failed\n");
			exit(1);
		}
	}
}

int main(int argc, char **argv)
{
	if (argc == 1) {
		sigset_t mask;

		sigfillset(&mask);
		do_exec(argv[0], "full", &mask);

		sigemptyset(&mask);
		do_exec(argv[0], "empty", &mask);
	} else {
		sigset_t mask;
		int i;
		int empty;

		if (strcmp(argv[1], "full") == 0)
			empty = 0;
		else if (strcmp(argv[1], "empty") == 0)
			empty = 1;
		else {
			fprintf(stderr, "empty or full?\n");
			exit(1);
		}

		sigprocmask(SIG_SETMASK, NULL, &mask);

		for(i = 1; i < NSIG; i++) {
			if (i == SIGKILL || i == SIGSTOP)
				continue;

			if (empty) {
				if (sigismember(&mask, i))
					printf("empty: signal %d added to mask\n", i);
			} else {
				if (!sigismember(&mask, i))
					printf("full: signal %d missing from mask\n", i);
			}
		}
	}

	return 0;
}
