/********************************************************
 * An example source module to accompany...
 *
 * "Using POSIX Threads: Programming with Pthreads"
 *     by Brad nichols, Dick Buttlar, Jackie Farrell
 *     O'Reilly & Associates, Inc.
 *
 ********************************************************
 * cancel.c --
 *
 * Demonstrates pthread cancellation.
 *
 */

#include <stdlib.h>
#include <stdio.h>
#include <unistd.h>
#include <sys/types.h>

#include <pthread.h>

#define NUM_THREADS  3
#define MESSAGE_MAX_LEN 80

int               count=NUM_THREADS;    /* number of threads active */
pthread_mutex_t   lock=PTHREAD_MUTEX_INITIALIZER; /* mutual exclusion 
						     for count */
pthread_cond_t    init_done=PTHREAD_COND_INITIALIZER; /* signaled by 
							 each thread after
							 completing initial-
							 ization */
int id_arg[3] = {0,1,2};

/*
 * Cleanup routine: last_breath()
 */
void last_breath(char *messagep)
{  
  printf("\n\n%s last_breath() cleanup routine: free'ing %p\n\n", 
	 messagep, messagep);

  free(messagep);
}

/*
 * print_count()
 */
void print_count(char *messagep, int id, int i)
{
  int last_type,tmp_type;

  pthread_setcanceltype(PTHREAD_CANCEL_DEFERRED, &last_type);
  switch(id) {
  case 0:
    printf("%s %4d\n", messagep, i);
    break;
  case 1:
    printf("%s \t%4d\n", messagep, i);
    break;
  case 2:
    printf("%s \t\t%4d\n", messagep, i);
    break;
  }
  pthread_setcanceltype(last_type, &tmp_type);
}

/*
 * bullet_proof()
 */
void *bullet_proof(void *id_p)
{
  int i=0, last_state;
  int *my_id = id_p;
  char *messagep;


  messagep = (char *)malloc(MESSAGE_MAX_LEN);
  sprintf(messagep, "Bullet Proof, thread #%d: ", *my_id);

  printf("%s\tI'm Alive, setting general cancellation OFF\n", 
	 messagep);

  /* push last_breath() routine onto stack */
  pthread_cleanup_push( (void *)last_breath, (void *)messagep );
  
  /* We turn off general cancelability here ... */
  pthread_setcancelstate(PTHREAD_CANCEL_DISABLE, &last_state);
  
  pthread_mutex_lock(&lock);
  {
    printf("\n%s signaling main that my init is done\n", messagep);
    count -= 1;
    /* signal to program that entering loop */
    pthread_cond_signal(&init_done);
    pthread_mutex_unlock(&lock);
  }

  /* loop forever until picked off with a cancel */
  for(;;i++) {
    if (i%1000 == 0) 
      print_count(messagep, *my_id, i); 
    if (i%100000 == 0) {
      printf("\n%s This is the thread that never ends... #%d\n",
	     messagep, i);
    }
  }

  /* Never get this far   */

  /* This pop is required by the standard, every push must have a pop 
     in the same lexical block. */
  pthread_cleanup_pop(0);

  return(NULL);
}

/*
 * ask_for_it()
 */
void *ask_for_it(void *id_p)
{
  int i=0, last_state, last_type;
  int *my_id = id_p;
  char *messagep;


  messagep = (char *)malloc(MESSAGE_MAX_LEN);
  sprintf(messagep, "Ask For It, thread #%d: ", *my_id);

  /* push last_breath() routine onto stack */
  pthread_cleanup_push( (void *)last_breath, (void *)messagep);
  
  /* We can turn on general cancelability here. Disable async cancellation */
  printf("%s\tI'm Alive, setting deferred cancellation ON\n", 
	 messagep);
  pthread_setcanceltype(PTHREAD_CANCEL_DEFERRED, &last_type);
  pthread_setcancelstate(PTHREAD_CANCEL_ENABLE, &last_state);

  pthread_mutex_lock(&lock);
  {
    printf("\n%s signaling main that my init is done\n", messagep);
    count -= 1;
    /* signal to program that entering loop */
    pthread_cond_signal(&init_done);
    pthread_mutex_unlock(&lock);
  }

  /* loop forever until picked off with a cancel */
  for(;;i++) {
    if (i%1000 == 0)
      print_count(messagep, *my_id, i);
    if (i%10000 == 0) {
      printf("\n%s\tLook, %d, I'll tell you when you can cancel me.\n",
             messagep, i);
    }
    pthread_testcancel();
  }

  /* never get this far */

  /* This pop is required by the standard, every push must have a pop 
     in the same lexical block. */
  pthread_cleanup_pop(0);

  return(NULL);
}

/*
 * sitting_duck()
 */
void *sitting_duck(void *id_p)
{
  int i=0, last_state, last_type, last_tmp;
  int *my_id = id_p;
  char *messagep;


  messagep = (char *)malloc(MESSAGE_MAX_LEN);
  sprintf(messagep, "Sitting Duck, thread #%d: ", *my_id);

  /* push last_breath() routine onto stack */
  pthread_cleanup_push( (void *)last_breath, (void *)messagep);
  
  pthread_mutex_lock(&lock);
  {
    printf("\n%s signaling main that my init is done\n", messagep);
    count -= 1;
    /* signal to program that entering loop */
    pthread_cond_signal(&init_done);
    pthread_mutex_unlock(&lock);
  }

  /* Now, we're safe to turn on async cancellability */
  printf("%s\tI'm Alive, setting async cancellation ON\n", 
	 messagep);
  pthread_setcanceltype(PTHREAD_CANCEL_ASYNCHRONOUS, &last_type);
  pthread_setcancelstate(PTHREAD_CANCEL_ENABLE, &last_state);
 
  /* loop forever until picked off with a cancel */
  for(;;i++) {
    if (i%1000 == 0) 
      print_count(messagep, *my_id, i++);
    if (i%10000 == 0) {
      pthread_setcanceltype(PTHREAD_CANCEL_DEFERRED, &last_tmp);
      printf("\n%s\tHum, nobody here but us chickens. %d\n", messagep,i);
      pthread_setcanceltype(PTHREAD_CANCEL_ASYNCHRONOUS, &last_tmp);
    }
  }
  
  /* never get this far */

  /* This pop is required by the standard, every push must have a pop 
     in the same lexical block. */
  pthread_cleanup_pop(0);

  return(NULL);
}

extern int 
main(void)
{
  int       i;
  void *statusp;
  pthread_t threads[NUM_THREADS];


  /* spawn the threads */
  pthread_create(&(threads[0]), 
		 NULL,
		 ask_for_it,
		 (void *) &(id_arg[0]));

  pthread_create(&(threads[1]), 
		 NULL,
		 sitting_duck,
		 (void *) &(id_arg[1]));

  pthread_create(&(threads[2]), 
		 NULL,
		 bullet_proof,
		 (void *) &(id_arg[2]));

  printf("main(): %d threads created\n", NUM_THREADS);
  
  pthread_mutex_lock(&lock);
  
  /* wait until all threads have entered loops */
  while (count != 0) {
      pthread_cond_wait(&init_done, &lock);
  }

  pthread_mutex_unlock(&lock);

  printf("main(): all threads have signaled that ready\n");

  /* cancel each thread */
  for (i=0; i<NUM_THREADS; i++) {
    pthread_cancel(threads[i]);
  }

  /* wait until all threads have finished */
  for (i=0; i<NUM_THREADS; i++) {
    pthread_join(threads[i], &statusp);
    if (statusp == PTHREAD_CANCELED) {
      printf("main(): joined to thread %d, statusp=PTHREAD_CANCELED\n",i);
    } else {
      printf("main(): joined to thread %d\n",i);
    }
  }

  printf("main()\t\tall %d threads have finished. \n", NUM_THREADS);

  return 0;
}
