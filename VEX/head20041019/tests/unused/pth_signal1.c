/********************************************************
 * An example source module to accompany...
 *
 * "Using POSIX Threads: Programming with Pthreads"
 *     by Brad nichols, Dick Buttlar, Jackie Farrell
 *     O'Reilly & Associates, Inc.
 *
 ********************************************************
 * sig.c
 *
 * Simple example of pthreads and signals.
 */

#include <stdlib.h>
#include <stdio.h>
#include <unistd.h>
#include <signal.h>
#include <sys/types.h>

#include <pthread.h>

#define MAX_NUM_THREADS  10


void *catch_usr1(void *p)
{
  int signo=SIGUSR1;
  /* struct sigaction action; */
  int caught;
  sigset_t  sigs_to_catch;

  /* Identify our thread */
  printf("\ncatchit() signal %d processing running as thread 0x%x \n", 
	 signo, (int)pthread_self());
  printf("Someone please send pid %d a SIGUSR1\n", getpid());

  /*
   * We inherited a thread sigmask with all the signals 
   * blocked.  So, we can wait on whatever signals we're
   * interested in and (as long as no other thread waits
   * for them) we'll be sure return from sigwait() to
   * handle it.
   */ 

  /* set this thread's signal mask to block out all other signals */
  sigemptyset(&sigs_to_catch);
  sigaddset(&sigs_to_catch, signo);

  sigwait(&sigs_to_catch, &caught);

  printf("\ncatchit() signal %d processing thread caught signal %d\n", 
         signo, caught);

  return(NULL);
}

void bugcatcher(int sig) 
{
  printf("The BUGCATCHER caught signal %d in thread 0x%x\n", 
	 sig, (int)pthread_self());
  pthread_exit(0);
}

void *cause_sig_sync(void *p)
{
  int i, id;
  sigset_t sigs_to_catch;

  /* Identify our thread */
  printf("cause_sig_sync() running in thread 0x%x\n", (int)pthread_self()); 

  /* set this thread's signal mask to block out all other signals */
  sigemptyset(&sigs_to_catch);
  sigaddset(&sigs_to_catch, SIGSEGV);
  sigaddset(&sigs_to_catch, SIGBUS);
  pthread_sigmask(SIG_UNBLOCK, &sigs_to_catch, NULL);

  /* Loop simulating useful processing in this thread */
  for(i=1;i==i;i++) {
    printf("printing count: %4d\r",i);
    if (i%100 == 0) {
      id = *(int *)p; /* Guaranteed bad address */
    }
  }

  return(NULL); 
}

extern int
main(void)
{
  int       i;
  pthread_t threads[MAX_NUM_THREADS];
  int       num_threads = 0;
  sigset_t  sigs_to_block;
  struct    sigaction action;


  /* Identify our thread */
  printf("main() running in thread 0x%x\n", (int)pthread_self()); 

  /* 
   * Set this thread's signal mask to block out all other signals
   * Other threads will inherit the mask
   */
  sigfillset(&sigs_to_block);
  pthread_sigmask(SIG_BLOCK, &sigs_to_block, NULL);

  /* Set signal handler for catching SIGSEGV and SIGBUS */
  action.sa_handler=bugcatcher;
  sigaction(SIGSEGV, &action, NULL);
  sigaction(SIGBUS, &action, NULL);

  /* spawn the threads */
   
  /* Make sure we can catch synchronous signals as exceptions */
  pthread_create(&threads[num_threads++],
		 NULL,
                 cause_sig_sync,
		 NULL);
  
  /* Rather than install the action/handler for the process,
     we create a thread to wait for the signal */
  pthread_create(&threads[num_threads++],
		 NULL,
                 catch_usr1,
		 NULL);

  printf("main()\t\t\t\t%d threads created\n",num_threads);
  
  /* wait until all threads have finished */
  for (i = 0; i < num_threads; i++) {
    pthread_join(threads[i], NULL);
    printf("main()\t\tjoined to thread %d \n", i);
  }
  
  printf("main()\t\tall %d threads have finished. \n", num_threads);

  return 0;
}

