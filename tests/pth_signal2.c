/********************************************************
 * An example source module to accompany...
 *
 * "Using POSIX Threads: Programming with Pthreads"
 *     by Brad nichols, Dick Buttlar, Jackie Farrell
 *     O'Reilly & Associates, Inc.
 *
 ********************************************************
 * stat_sigwait.c
 *
 * Simple example of pthreads and signals.
 */

#include <stdlib.h>
#include <stdio.h>
#include <unistd.h>
#include <signal.h>
#include <time.h>
#include <sys/types.h>

#include <pthread.h>

#define MAX_NUM_THREADS  10

pthread_mutex_t stats_lock = PTHREAD_MUTEX_INITIALIZER;
int mean, samples, total;

void *report_stats(void *p)
{
  int caught, i;
  sigset_t  sigs_to_catch;

  /* Identify our thread */
  printf("\nreport_stats() started.\n"); 

  /*
   * We inherited a thread sigmask with all the signals 
   * blocked.  So, we can wait on whatever signals we're
   * interested in and (as long as no other thread waits
   * for them) we'll be sure return from sigwait() to
   * handle it.
   */ 

  /* set this thread's signal mask to block out SIGUSR1 */
  sigemptyset(&sigs_to_catch);
  sigaddset(&sigs_to_catch, SIGUSR1);

  for (;;) {
     sigwait(&sigs_to_catch, &caught);

     pthread_mutex_lock(&stats_lock);
     mean = total/samples;
     printf("\nreport_stats(): mean = %d, samples = %d\n", mean, samples);
     pthread_mutex_unlock(&stats_lock);

     /* Delay for a while so it's obvious whether or not SIGUSR1 is
	still blocked here (it should be). */
     //     for (i = 0; i < 100000; i++) ;

  }
  return NULL;
}
/*
 * worker_thread --
 * 
 * Don't read too much into what this thread does.  It's
 * a very simpleminded example.  The only interesting thing
 * it does is write to the global statistics data-- which
 * means the thread processing the signal has to protect 
 * against simultaneous access.
 */
void *worker_thread(void *p) 
{
  time_t now;
  for (;;) {

    sleep(1 + (*(int*)p) % 2 );
  
    now = time(NULL);
 
    pthread_mutex_lock(&stats_lock);
    total+=((int)now)%60; /* probably not the safest thing to do but
		        it's just an example */
    samples++;
    pthread_mutex_unlock(&stats_lock);
  }
  /* Won't get here.  */
  return NULL;
}

extern int
main(void)
{
  int       i;
  pthread_t threads[MAX_NUM_THREADS];
  int       num_threads = 0;
  sigset_t  sigs_to_block;


  /* Identify our thread */
  printf("main() (pid %d) running in thread 0x%x\n", 
         getpid(), (int)pthread_self()); 

  /* 
   * Set this thread's signal mask to block SIGUSR1
   * Other threads will inherit the mask
   */
  sigemptyset(&sigs_to_block);
  sigaddset(&sigs_to_block, SIGUSR1);
  pthread_sigmask(SIG_BLOCK, &sigs_to_block, NULL);

  /* spawn statistics reporting thread */
  pthread_create(&threads[num_threads++],
	         NULL,
		 report_stats,
		 NULL);

  /* spawn the threads */
  for (i=num_threads; i<MAX_NUM_THREADS; i++) {
    pthread_create(&threads[num_threads++],
		   NULL,
                   worker_thread,
		   &i);
  }
 
  printf("main()\t\t\t\t%d threads created\n",num_threads);
  
  /* wait until all threads have finished */
  for (i = 0; i < num_threads; i++) {
    pthread_join(threads[i], NULL);
    printf("main()\t\tjoined to thread %d \n", i);
  }
  
  printf("main()\t\tall %d threads have finished. \n", num_threads);

  return 0;
}

