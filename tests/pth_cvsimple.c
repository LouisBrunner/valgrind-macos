/********************************************************
 * An example source module to accompany...
 *
 * "Using POSIX Threads: Programming with Pthreads"
 *     by Brad nichols, Dick Buttlar, Jackie Farrell
 *     O'Reilly & Associates, Inc.
 *
 ********************************************************
 *
 * cvsimple.c
 *
 * Demonstrates pthread cancellation.
 *
 */

#include <stdio.h>
#include <pthread.h>

#define NUM_THREADS  3
#define TCOUNT 10
#define COUNT_THRES 12

int     count = 0;
int     thread_ids[3] = {0,1,2};
pthread_mutex_t count_lock=PTHREAD_MUTEX_INITIALIZER; 
pthread_cond_t count_hit_threshold=PTHREAD_COND_INITIALIZER; 

void *inc_count(void *idp)
{
  int i=0, save_state, save_type;
  int *my_id = idp;

  for (i=0; i<TCOUNT; i++) {
    pthread_mutex_lock(&count_lock);
    count++;
    printf("inc_counter(): thread %d, count = %d, unlocking mutex\n", 
	   *my_id, count);
    if (count == COUNT_THRES) {
      printf("inc_count(): Thread %d, count %d\n", *my_id, count);
      pthread_cond_signal(&count_hit_threshold);
    }
    pthread_mutex_unlock(&count_lock);
  }
  
  return(NULL);
}

void *watch_count(void *idp)
{
  int i=0, save_state, save_type;
  int *my_id = idp;

  printf("watch_count(): thread %d\n", *my_id);
  fflush(stdout);
  pthread_mutex_lock(&count_lock);

  while (count < COUNT_THRES) {
    pthread_cond_wait(&count_hit_threshold, &count_lock);
    printf("watch_count(): thread %d, count %d\n", *my_id, count);
  }

  pthread_mutex_unlock(&count_lock);
  
  return(NULL);
}

extern int
main(void)
{
  int       i;
  pthread_t threads[3];

  pthread_create(&threads[0], NULL, inc_count, (void *)&thread_ids[0]);
  pthread_create(&threads[1], NULL, inc_count, (void *)&thread_ids[1]);
  pthread_create(&threads[2], NULL, watch_count, (void *)&thread_ids[2]);

  for (i = 0; i < NUM_THREADS; i++) {
    pthread_join(threads[i], NULL);
  }

  return 0;
}


