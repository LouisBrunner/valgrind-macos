#include <signal.h>
#include <unistd.h>
#include <sys/wait.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>
#include <time.h>

static const struct timespec bip = { 0, 1000000000 / 5 };

static void handler(int sig)
{
}

/* Kill our child, but use a separate kill command.  This is so that
   it's running independently of Valgrind, and so is async with
   respect to thread scheduling. */
static void do_kill(int pid, int sig)
{
	int status;
	int killer;
	int ret;

	killer = vfork();
	
	if (killer == -1) {
		perror("killer/vfork");
		exit(1);
	}

	if (killer == 0) {
		char sigbuf[20];
		char pidbuf[20];
		sprintf(sigbuf, "-%d", sig);
		sprintf(pidbuf, "%d", pid);
		execl("/bin/kill", "kill", sigbuf, pidbuf, NULL);
		perror("exec failed");
		exit(1);
	}

	do 
		ret = waitpid(killer, &status, 0);
	while(ret == -1 && errno == EINTR);

	if (ret != killer) {
		perror("kill/waitpid");
		exit(1);
	}

	if (!WIFEXITED(status) || WEXITSTATUS(status) != 0) {
		printf("kill %d failed status=%s %d\n", killer, 
		       WIFEXITED(status) ? "exit" : "signal", 
		       WIFEXITED(status) ? WEXITSTATUS(status) : WTERMSIG(status));
		exit(1);
	}
}

static void test(int block, int caughtsig, int fatalsig)
{
	int pid;
	int status;
	int i;

	printf("testing: blocking=%d caught=%d fatal=%d... ", block, caughtsig, fatalsig);
	fflush(stdout);

	pid = fork();
	if (pid == -1) {
		perror("fork");
		exit(1);
	}

	if (pid == 0) {
		signal(caughtsig, handler);

		for(;;)
			if (block)
				pause();

	}

	nanosleep(&bip, 0);		/* wait for child to get going */

	for(i = 0; i < 5; i++) {
		do_kill(pid, caughtsig);	/* should be caught */
		nanosleep(&bip, 0);
		do_kill(pid, caughtsig);
		do_kill(pid, caughtsig);
	}

	nanosleep(&bip, 0);

	do_kill(pid, fatalsig);	/* should kill it */
	
	if (waitpid(pid, &status, 0) != pid)
		printf("FAILED: waitpid failed: %s\n", strerror(errno));
	else if (!WIFSIGNALED(status) || WTERMSIG(status) != fatalsig)
		printf("FAILED: child exited with unexpected status %s %d\n",
		       WIFEXITED(status) ? "exit" : "signal", 
		       WIFEXITED(status) ? WEXITSTATUS(status) : WTERMSIG(status));
	else
		printf("PASSED\n");
}

int main()
{
	static const int catch[] = { SIGSEGV, SIGUSR1 };
	static const int fatal[] = { SIGBUS, SIGUSR2 };
	int block, catchidx, fatalidx;

	setvbuf(stdout, NULL, _IOLBF, 0);
	
	for(block = 0; block < 2; block++)
		for(catchidx = 0; catchidx < sizeof(catch)/sizeof(*catch); catchidx++)
			for(fatalidx = 0; fatalidx < sizeof(fatal)/sizeof(*fatal); fatalidx++)
				test(block, catch[catchidx], fatal[fatalidx]);
	return 0;
}
