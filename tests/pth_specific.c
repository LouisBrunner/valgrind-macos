/********************************************************
 * An example source module to accompany...
 *
 * "Using POSIX Threads: Programming with Pthreads"
 *     by Brad nichols, Dick Buttlar, Jackie Farrell
 *     O'Reilly & Associates, Inc.
 *
 ********************************************************
 * specific.c
 *
 */
#include <stdlib.h>
#include <stdio.h>
#include <unistd.h>

#include <sys/time.h>

#include <pthread.h>

#define NUM_THREADS  3
pthread_key_t     saved_time_key;


void free_time(void *arg )
{
  struct timeval *timev=(struct timeval *)arg; 
  printf("free_time:\n");
  free(timev);
}

void save_the_time(void)
{
  struct timeval *timev;

  timev = (struct timeval *)malloc(sizeof(struct timeval));
  
  gettimeofday(timev, NULL);

  printf("save_the_time: \t\t%ld %ld\n",timev->tv_sec, timev->tv_usec);


  pthread_setspecific(saved_time_key, (void *)timev);
  
}

void what_time_did_i_save(void)
{
  struct timeval *timev;

  timev = pthread_getspecific(saved_time_key);

  printf("what_time_did_i_save: \t%ld %ld\n",timev->tv_sec, timev->tv_usec);

}  

void *thread_routine(void *arg)
{
  int *my_id=(int *)arg;

  printf("thread_routine %d\n", *my_id);

  save_the_time();

  what_time_did_i_save();
 
  return(NULL); 
}

extern int 
main(void)
{
	int       i, *id_arg;
	pthread_t threads[NUM_THREADS];

	id_arg = (int *)malloc(NUM_THREADS*sizeof(int));

	printf("main : initializing the key\n");
	pthread_key_create(&saved_time_key, free_time);

	printf("main : spawing the threads\n");
	for (i = 0; i < NUM_THREADS; i++) {

		id_arg[i] = i;

		pthread_create(&(threads[i]), 
			       NULL,
			       thread_routine,
			       (void *) &(id_arg[i]));
	}


	for (i = 0; i < NUM_THREADS; i++) {
	  pthread_join(threads[i], NULL);
 	  printf("main : thread %d has finished. \n", i);
	}

	printf("main : goodbye\n");

	return 0;
}




