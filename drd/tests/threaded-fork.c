/* fork a process that has created a detached thread. */

#include <stdlib.h>
#include <sys/wait.h>
#include <pthread.h>
#include <sys/types.h>
#include <unistd.h>
#include <stdio.h>
#include <signal.h>

static void *threadmain(void *dummy)
{
	sleep((unsigned long)dummy);
	return NULL;
}

int main(int argc, char **argv)
{
        int ctr;
	pid_t childpid;
	pthread_t childthread;
	void *res;
	int status;

	pthread_create(&childthread, NULL, threadmain, (void *)2);
	pthread_detach(childthread);

	childpid = fork();
	switch (childpid) {
	case 0:
		pthread_create(&childthread, NULL, threadmain, 0);
		pthread_join(childthread, &res);
		exit(0);
		break;
	case -1:
		perror("FAILED: fork failed\n");
		break;
	default:
		break;
	}

	ctr = 0;
	while (waitpid(childpid, &status, 0) != childpid) {
		sleep(1);
		ctr++;
		if (ctr >= 10) {
		  fprintf(stderr, "FAILED - timeout waiting for child\n");
		  return 0;
		}
	}

	fprintf(stderr, "PASS\n");

	return 0;
}
