#include <stdio.h>
#include <unistd.h>
#include <signal.h>
#include <errno.h>
#include <sys/wait.h>
#include <string.h>

/* Make sure that a blocking syscall restarts if hit by a signal,
   and SA_RESTART is set */

static void handler(int s)
{
}

int main()
{
	int pid;
	int fds[2];

	if (pipe(fds) == -1) {
		perror("FAIL: pipe\n");
		return 1;
	}

	pid = fork();

	if (pid == -1) {
		perror("fork failed");
		return 1;
	}

	if (pid == 0) {
		char ch = '?';
		int ret;
		struct sigaction sa;
		
		sa.sa_handler = handler;
		sigfillset(&sa.sa_mask);
		sa.sa_flags = SA_RESTART;

		sigaction(SIGUSR1, &sa, NULL);

		close(fds[1]);
		ret = read(fds[0], &ch, 1);

		if (ret != 1 || ch != 'x')
			fprintf(stderr, "FAIL: expected 1 byte, not %d/%s/%c\n", 
				ret, strerror(errno), ch);
	} else {
		signal(SIGPIPE, SIG_IGN);

		close(fds[0]);
		sleep(1);
		kill(pid, SIGUSR1);
		sleep(1);
		write(fds[1], "x", 1);

		waitpid(pid, NULL, 0);
	}

	return 0;
}
