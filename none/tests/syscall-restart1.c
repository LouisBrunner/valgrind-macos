#include <stdio.h>
#include <unistd.h>
#include <signal.h>
#include <errno.h>
#include <sys/wait.h>
#include <string.h>

/* Make sure that a blocking syscall returns EINTR if hit by a signal,
   and there's no SA_RESTART */

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
		sa.sa_flags = 0; /* no SA_RESTART */

		sigaction(SIGUSR1, &sa, NULL);

		close(fds[1]);
		ret = read(fds[0], &ch, 1);

		if (ret != -1 || errno != EINTR)
			fprintf(stderr, "FAIL: expected EINTR, not %d/%s/%c\n", 
				ret, strerror(errno), ch);
	} else {
		signal(SIGPIPE, SIG_IGN);

		close(fds[0]);
		sleep(1);
		kill(pid, SIGUSR1);
		sleep(1);
		if (write(fds[1], "x", 1) != -1 || errno != EPIPE)
			fprintf(stderr, "FAIL: expected write to fail with EPIPE, not %d\n", errno);

		waitpid(pid, NULL, 0);
	}

	return 0;
}
