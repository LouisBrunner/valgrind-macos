/* 
   This test makes sure the thread exit notification signals don't
   interfere with the stack growth signals.

   Thread death notifications are sent as RT signals, which are
   queued.  In general, these notifications are ignored, since they're
   only used by the main thread if it has exited and is still waiting
   for the rest to exit.  

   The system has a finite limit to the number of RT signals which can
   be queued (typically 1024), and beyond that it stops queueing
   siginfo.  We rely on getting SIGSEGVs with siginfo information to
   grow the stack.  If we don't get the siginfo, then it just looks
   like the program crashed.

   The extra complication in this test is making sure that the
   unwanted signals are discarded while the main thread is blocked in
   a syscall.  So, to check this, main creates a new process, which
   attempts to grow the stack once all the threads have been created
   and exited.  main() itself is blocked waiting for the child
   process.

   Oh, and this test also makes sure that thread resources are cleaned
   up properly.
 */
#include <pthread.h>
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <unistd.h>
#include <signal.h>
#include <sys/wait.h>

static int grower;

static void handler(int sig)
{
}

static void *thr(void *v)
{
	return 0;
}

#define FRAME 4096

static void grow(int depth)
{
	volatile char frame[FRAME];

	memset((char *)frame, 0xff, sizeof(frame));

	if (depth > 1)
		grow(depth-1);
}

static void *maker(void *v)
{
	int i;

	sleep(1);

	/* Create lots of threads */
	printf("creating threads...\n");
	for(i = 0; i < 1300; i++) {
		pthread_t t;
		int ret;
		
		if (i % 100 == 0)
			printf("%d...\n", i);

		ret = pthread_create(&t, NULL, thr, NULL);
		if (ret) {
			printf("pthread_create failed: %s\n", strerror(ret));
			exit(1);
		}

		ret = pthread_join(t, NULL);
		if (ret) {
			printf("pthread_join failed: %s\n", strerror(ret));
			exit(1);
		}
	}
	
	kill(grower, SIGUSR1);

	return NULL;
}

int main()
{
	pthread_t pth;
	sigset_t mask;
	int status;
	struct sigaction sa;

	sigemptyset(&mask);
	sigaddset(&mask, SIGCHLD);
	sigprocmask(SIG_BLOCK, &mask, NULL);

	sa.sa_handler = handler;
	sa.sa_flags = 0;
	sigfillset(&sa.sa_mask);
	sigaction(SIGUSR1, &sa, NULL);
	
	grower = fork();

	if (grower == -1) {
		perror("fork");
		exit(1);
	}

	if (grower == 0) {
		pause();	/* child - wait for SIGUSR1 */
		grow(10);
		printf("stack grew OK\n");
		exit(0);
	}

	pthread_create(&pth, NULL, maker, NULL);

	/* wait for child */
	if (waitpid(grower, &status, 0) != grower)
		printf("FAILED\n");
	else if (WIFEXITED(status) && WEXITSTATUS(status) == 0)
		printf("PASS: child OK\n");
	else
		printf("FAILED: exit status=%d\n", status);

	pthread_join(pth, NULL);

	return 0;
}
