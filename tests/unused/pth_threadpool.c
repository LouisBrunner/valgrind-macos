/********************************************************
 * An example source module to accompany...
 *
 * "Using POSIX Threads: Programming with Pthreads"
 *     by Brad nichols, Dick Buttlar, Jackie Farrell
 *     O'Reilly & Associates, Inc.
 *
 ********************************************************
 * tpool.c -- 
 * 
 * Example thread pooling library
 */

#include <stdlib.h>
#include <stdio.h>
#include <unistd.h>
#include <sys/types.h>
#include <string.h>

#include <pthread.h>


/********************************************************
 * An example source module to accompany...
 *
 * "Using POSIX Threads: Programming with Pthreads"
 *     by Brad nichols, Dick Buttlar, Jackie Farrell
 *     O'Reilly & Associates, Inc.
 *
 ********************************************************
 * tpool.h --
 *
 * Structures for thread pool
 */

typedef struct tpool_work {
	void               (*routine)();
	void                *arg;
	struct tpool_work   *next;
} tpool_work_t;

typedef struct tpool {
	/* pool characteristics */
	int                 num_threads;
        int                 max_queue_size;
        int                 do_not_block_when_full;
        /* pool state */
	pthread_t           *threads;
        int                 cur_queue_size;
	tpool_work_t        *queue_head;
	tpool_work_t        *queue_tail;
	int                 queue_closed;
        int                 shutdown;
	/* pool synchronization */
        pthread_mutex_t     queue_lock;
        pthread_cond_t      queue_not_empty;
        pthread_cond_t      queue_not_full;
	pthread_cond_t      queue_empty;
} *tpool_t;

void tpool_init(
           tpool_t          *tpoolp,
           int              num_threads, 
           int              max_queue_size,
           int              do_not_block_when_full);

int tpool_add_work(
           tpool_t          tpool,
           void             (*routine)(),
	   void             *arg);

int tpool_destroy(
           tpool_t          tpool,
           int              finish);


/*-- end of tpool.h ----------------------------------*/


void *tpool_thread(void *);

void tpool_init(tpool_t   *tpoolp,
		int       num_worker_threads, 
		int       max_queue_size,
		int       do_not_block_when_full)
{
  int i, rtn;
  tpool_t tpool;
   
  /* allocate a pool data structure */ 
  if ((tpool = (tpool_t )malloc(sizeof(struct tpool))) == NULL)
    perror("malloc"), exit(1);

  /* initialize th fields */
  tpool->num_threads = num_worker_threads;
  tpool->max_queue_size = max_queue_size;
  tpool->do_not_block_when_full = do_not_block_when_full;
  if ((tpool->threads = 
       (pthread_t *)malloc(sizeof(pthread_t)*num_worker_threads)) 
      == NULL)
    perror("malloc"), exit(1);
  tpool->cur_queue_size = 0;
  tpool->queue_head = NULL; 
  tpool->queue_tail = NULL;
  tpool->queue_closed = 0;  
  tpool->shutdown = 0; 
  if ((rtn = pthread_mutex_init(&(tpool->queue_lock), NULL)) != 0)
    fprintf(stderr,"pthread_mutex_init %s\n",strerror(rtn)), exit(1);
  if ((rtn = pthread_cond_init(&(tpool->queue_not_empty), NULL)) != 0)
    fprintf(stderr,"pthread_cond_init %s\n",strerror(rtn)), exit(1);
  if ((rtn = pthread_cond_init(&(tpool->queue_not_full), NULL)) != 0)
    fprintf(stderr,"pthread_cond_init %s\n",strerror(rtn)), exit(1);
  if ((rtn = pthread_cond_init(&(tpool->queue_empty), NULL)) != 0)
    fprintf(stderr,"pthread_cond_init %s\n",strerror(rtn)), exit(1);

  /* create threads */
  for (i = 0; i != num_worker_threads; i++) {
    if ((rtn = pthread_create( &(tpool->threads[i]),
			      NULL,
			      tpool_thread,
			      (void *)tpool)) != 0)
      fprintf(stderr,"pthread_create %d\n",rtn), exit(1);
  }

  *tpoolp = tpool;
}

int tpool_add_work(
		   tpool_t          tpool,
		   void             (*routine)(),
		   void             *arg)
{
  int rtn;
  tpool_work_t *workp;

  if ((rtn = pthread_mutex_lock(&(tpool->queue_lock))) != 0)
    fprintf(stderr,"pthread_mutex_lock %d\n",rtn), exit(1);

  /* no space and this caller doesn't want to wait */
  if ((tpool->cur_queue_size == tpool->max_queue_size) &&
      tpool->do_not_block_when_full) {
    if ((rtn = pthread_mutex_unlock(&(tpool->queue_lock))) != 0)
      fprintf(stderr,"pthread_mutex_unlock %d\n",rtn), exit(1);

    return -1;
  }

  while( (tpool->cur_queue_size == tpool->max_queue_size) &&
	(!(tpool->shutdown || tpool->queue_closed))  ) {

    if ((rtn = pthread_cond_wait(&(tpool->queue_not_full),
				 &(tpool->queue_lock))) != 0)
      fprintf(stderr,"pthread_cond_waitA %d\n",rtn), exit(1);

  }

  /* the pool is in the process of being destroyed */
  if (tpool->shutdown || tpool->queue_closed) {
    if ((rtn = pthread_mutex_unlock(&(tpool->queue_lock))) != 0)
      fprintf(stderr,"pthread_mutex_unlock %d\n",rtn), exit(1);
 
    return -1;
  }


  /* allocate work structure */
  if ((workp = (tpool_work_t *)malloc(sizeof(tpool_work_t))) == NULL)
    perror("malloc"), exit(1);
  workp->routine = routine;
  workp->arg = arg;
  workp->next = NULL;

  printf("adder: adding an item %d\n", workp->routine);

  if (tpool->cur_queue_size == 0) {
    tpool->queue_tail = tpool->queue_head = workp;

     printf("adder: queue == 0, waking all workers\n");

    if ((rtn = pthread_cond_broadcast(&(tpool->queue_not_empty))) != 0)
      fprintf(stderr,"pthread_cond_signal %d\n",rtn), exit(1);;
  } else {
    tpool->queue_tail->next = workp;
    tpool->queue_tail = workp;
  }

  tpool->cur_queue_size++; 
  if ((rtn = pthread_mutex_unlock(&(tpool->queue_lock))) != 0)
    fprintf(stderr,"pthread_mutex_unlock %d\n",rtn), exit(1);
  return 1;
}

int tpool_destroy(tpool_t          tpool,
		  int              finish)
{
  int          i,rtn;
  tpool_work_t *cur_nodep;
  

  if ((rtn = pthread_mutex_lock(&(tpool->queue_lock))) != 0)
    fprintf(stderr,"pthread_mutex_lock %d\n",rtn), exit(1);

  /* Is a shutdown already in progress? */
  if (tpool->queue_closed || tpool->shutdown) {
    if ((rtn = pthread_mutex_unlock(&(tpool->queue_lock))) != 0)
      fprintf(stderr,"pthread_mutex_unlock %d\n",rtn), exit(1);
    return 0;
  }

  tpool->queue_closed = 1;

  /* If the finish flag is set, wait for workers to 
     drain queue */ 
  if (finish == 1) {
    while (tpool->cur_queue_size != 0) {
      if ((rtn = pthread_cond_wait(&(tpool->queue_empty),
				   &(tpool->queue_lock))) != 0)
	fprintf(stderr,"pthread_cond_waitB %d\n",rtn), exit(1);
    }
  }

  tpool->shutdown = 1;

  if ((rtn = pthread_mutex_unlock(&(tpool->queue_lock))) != 0)
    fprintf(stderr,"pthread_mutex_unlock %d\n",rtn), exit(1);


  /* Wake up any workers so they recheck shutdown flag */
  if ((rtn = pthread_cond_broadcast(&(tpool->queue_not_empty))) != 0)
    fprintf(stderr,"pthread_cond_broadcast %d\n",rtn), exit(1);
  if ((rtn = pthread_cond_broadcast(&(tpool->queue_not_full))) != 0)
    fprintf(stderr,"pthread_cond_broadcast %d\n",rtn), exit(1);


  /* Wait for workers to exit */
  for(i=0; i < tpool->num_threads; i++) {
    if ((rtn = pthread_join(tpool->threads[i],NULL)) != 0)
      fprintf(stderr,"pthread_join %d\n",rtn), exit(1);
  }

  /* Now free pool structures */
  free(tpool->threads);
  while(tpool->queue_head != NULL) {
    cur_nodep = tpool->queue_head->next; 
    tpool->queue_head = tpool->queue_head->next;
    free(cur_nodep);
  }
  free(tpool); 
}

void *tpool_thread(void *arg)
{
  tpool_t tpool = (tpool_t)arg; 
  int rtn;
  tpool_work_t	*my_workp;
	
  for(;;) {



    /* Check queue for work */ 
    if ((rtn = pthread_mutex_lock(&(tpool->queue_lock))) != 0)
      fprintf(stderr,"pthread_mutex_lock %d\n",rtn), exit(1);

    while ((tpool->cur_queue_size == 0) && (!tpool->shutdown)) {


      printf("worker %d: I'm sleeping again\n", pthread_self());

      if ((rtn = pthread_cond_wait(&(tpool->queue_not_empty),
				   &(tpool->queue_lock))) != 0)
	fprintf(stderr,"pthread_cond_waitC %d\n",rtn), exit(1);

    }
    sleep(1); 
 
    printf("worker %d: I'm awake\n", pthread_self());

    /* Has a shutdown started while i was sleeping? */
    if (tpool->shutdown == 1) {

      if ((rtn = pthread_mutex_unlock(&(tpool->queue_lock))) != 0)
	fprintf(stderr,"pthread_mutex_unlock %d\n",rtn), exit(1);
      pthread_exit(NULL);
    }


    /* Get to work, dequeue the next item */ 
    my_workp = tpool->queue_head;
    tpool->cur_queue_size--;
    if (tpool->cur_queue_size == 0)
      tpool->queue_head = tpool->queue_tail = NULL;
    else
      tpool->queue_head = my_workp->next;
 
    printf("worker %d: dequeuing item %d\n", pthread_self(), my_workp->next);

    /* Handle waiting add_work threads */
    if ((!tpool->do_not_block_when_full) &&
	(tpool->cur_queue_size ==  (tpool->max_queue_size - 1))) 

      if ((rtn = pthread_cond_broadcast(&(tpool->queue_not_full))) != 0)
	fprintf(stderr,"pthread_cond_broadcast %d\n",rtn), exit(1);

    /* Handle waiting destroyer threads */
    if (tpool->cur_queue_size == 0)

      if ((rtn = pthread_cond_signal(&(tpool->queue_empty))) != 0)
	fprintf(stderr,"pthread_cond_signal %d\n",rtn), exit(1);

    if ((rtn = pthread_mutex_unlock(&(tpool->queue_lock))) != 0)
      fprintf(stderr,"pthread_mutex_unlock %d\n",rtn), exit(1);
      
    /* Do this work item */
    (*(my_workp->routine))(my_workp->arg);
    free(my_workp);
  } 
  return(NULL);            
}


/********************************************************
 * An example source module to accompany...
 *
 * "Using POSIX Threads: Programming with Pthreads"
 *     by Brad nichols, Dick Buttlar, Jackie Farrell
 *     O'Reilly & Associates, Inc.
 *
 ********************************************************
 * tpool.c -- 
 * 
 * Example caller for thread pooling library
 */

char *s1[20]={  "STRING 0",
		"STRING 1",
                "STRING 2",
                "STRING 3",
                "STRING 4",
                "STRING 5",
                "STRING 6",
                "STRING 7",
                "STRING 8",
                "STRING 9",
		"STRING 10",
                "STRING 11",
                "STRING 12",
                "STRING 13",
                "STRING 14",
                "STRING 15",
                "STRING 16",
                "STRING 17",
                "STRING 18",
                "STRING 19"};

void r1(char * printstring)
{
   int i, x;

   printf("%s START\n", printstring);

   for (i = 0; i < 1000000; i++)  {
	x = x +i;
   }

   printf("%s DONE\n", printstring);
}

extern int
main(void)
{
   extern char *s1[];

   pthread_t t1,t2;
   int i;  

   tpool_t test_pool;

   tpool_init(&test_pool, 10, 20, 0);

   sleep(1);

   for ( i = 0; i < 5; i++) {
	   printf("tpool_add_work returned %d\n",
		tpool_add_work(test_pool, r1, s1[i]));

   }

   printf("main: all work queued\n"); 

   tpool_destroy(test_pool, 1); 

   return 0;
   
}  
