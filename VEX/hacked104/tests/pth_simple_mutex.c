/********************************************************
 * An example source module to accompany...
 *
 * "Using POSIX Threads: Programming with Pthreads"
 *     by Brad nichols, Dick Buttlar, Jackie Farrell
 *     O'Reilly & Associates, Inc.
 *
 ********************************************************
 * simple_mutex.c
 *
 * Simple multi-threaded example with a mutex lock.
 */
#include <stdlib.h>
#include <stdio.h>
#include <errno.h>
#include <pthread.h>

void do_one_thing(int *);
void do_another_thing(int *);
void do_wrap_up(int, int);

int r1 = 0, r2 = 0, r3 = 0;
pthread_mutex_t r3_mutex=PTHREAD_MUTEX_INITIALIZER;

extern int
main(int argc, char **argv)
{
  pthread_t       thread1, thread2;

  if (argc > 1) 
  	r3 = atoi(argv[1]);

  if (pthread_create(&thread1, 
		 NULL,
		 (void *) do_one_thing,
		 (void *) &r1) != 0)
	perror("pthread_create"),exit(1); 

  if (pthread_create(&thread2, 
		 NULL, 
		 (void *) do_another_thing,
		 (void *) &r2) != 0)
	perror("pthread_create"),exit(1); 
  
  if (pthread_join(thread1, NULL) != 0)
	perror("pthread_join"), exit(1);

  if (pthread_join(thread2, NULL) != 0)
	perror("pthread_join"), exit(1);

  do_wrap_up(r1, r2);

  return 0; 
}

void do_one_thing(int *pnum_times)
{
  int i, j, x;
  
  pthread_mutex_lock(&r3_mutex);
  if(r3 > 0) {
	x = r3;
	r3--;
  } else {
	x = 1;
  } 
  pthread_mutex_unlock(&r3_mutex); 

  for (i = 0;  i < 4; i++) {
    printf("doing one thing\n"); 
    for (j = 0; j < 100000; j++) x = x + i;
    (*pnum_times)++;
  }

}

void do_another_thing(int *pnum_times)
{
  int i, j, x;
  
  pthread_mutex_lock(&r3_mutex);
  if(r3 > 0) {
        x = r3;
        r3--;
  } else {
        x = 1;
  }
  pthread_mutex_unlock(&r3_mutex);

  for (i = 0;  i < 4; i++) {
    printf("doing another \n"); 
    for (j = 0; j < 100000; j++) x = x + i;
    (*pnum_times)++;
  }

}

void do_wrap_up(int one_times, int another_times)
{
  int total;

  total = one_times + another_times;
  printf("All done, one thing %d, another %d for a total of %d\n",
	one_times, another_times, total);
}
