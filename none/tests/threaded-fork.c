#include <stdlib.h>
#include <sys/wait.h>
#include <pthread.h>
#include <sys/types.h>
#include <unistd.h>
#include <stdio.h>
#include <signal.h>

static void *threadmain( void *dummy )
{
	sleep( (int)dummy );
	return NULL;
}

int main( int argc, char **argv )
{
	pid_t childpid;
	pthread_t childthread;
	void *res;
	int status;

	pthread_create( &childthread, NULL, threadmain, (void *)2 );

	childpid = fork();
	switch( childpid ) {
	case 0:
		pthread_create( &childthread, NULL, threadmain, 0 );
		pthread_join( childthread, &res );
		exit(0);
		break;
	case -1:
		perror( "FAILED: fork failed\n" );
		break;
	default:
		break;
	}

	pthread_join( childthread, &res );
	while(waitpid(childpid, &status, 0) != childpid)
		;

	printf("PASS\n");

	return 0;
}


