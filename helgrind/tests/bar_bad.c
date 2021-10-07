
/* This program checks that Helgrind reports the five degenerate
   uses of the barrier functions shown. */
#define _GNU_SOURCE
#include <pthread.h>
#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include <unistd.h>
#include <string.h>
#include "config.h"

void* child1 ( void* arg )
{
   pthread_barrier_wait( (pthread_barrier_t*)arg );
   return NULL;
}

void *sleep1 ( void* arg )
{
   /* Long sleep, we hope to never trigger. */
   sleep (10);
   pthread_barrier_wait ( (pthread_barrier_t*)arg );
   return NULL;
}

void *exit1 ( void* arg )
{
   /* Sleep a bit, then exit, we are done. */
   sleep (1);
   exit (0);
   return NULL;
}

int main ( void )
{
  pthread_barrier_t *bar1, *bar2, *bar3, *bar4, *bar5;
  /* int r; unused since pthread_cancel are commented out */
  pthread_t thr1, thr2, slp1, slp2, ext1;

  /* initialise a barrier with a zero count */
  fprintf(stderr, "\ninitialise a barrier with zero count\n");
  bar1 = malloc(sizeof(pthread_barrier_t));
  pthread_barrier_init(bar1, NULL, 0);

  /* initialise a barrier twice */
  fprintf(stderr, "\ninitialise a barrier twice\n");
  bar2 = malloc(sizeof(pthread_barrier_t));
  pthread_barrier_init(bar2, NULL, 1);
  pthread_barrier_init(bar2, NULL, 1);

  /* initialise a barrier which has threads waiting on it.
     This isn't too simple. */
  fprintf(stderr, "\ninitialise a barrier which has threads waiting on it\n");
  bar3 = malloc(sizeof(pthread_barrier_t));
  pthread_barrier_init(bar3, NULL, 2);
  /* create a thread, whose purpose is to "unblock" the barrier after
     some sleeping in case it keeps being blocked.  */
  pthread_create(&slp1, NULL, sleep1, (void*)bar3);
  /* create a thread, whose only purpose is to block on the barrier */
  pthread_create(&thr1, NULL, child1, (void*)bar3);
  /* guarantee that it gets there first */
  sleep(1);
  /* and now reinitialise */
  pthread_barrier_init(bar3, NULL, 3);

  /* destroy a barrier that has threads waiting at it */
  fprintf(stderr, "\ndestroy a barrier that has waiting threads\n");
  /* once again, create a thread, whose only purpose is to block. */
  bar4 = malloc(sizeof(pthread_barrier_t));
  pthread_barrier_init(bar4, NULL, 2);
  /* create a thread, whose purpose is to "unblock" the barrier after
     some sleeping in case it keeps being blocked. We hope it isn't
     needed, but if it is, because pthread_barier_destroy hangs
     and we will get an extra warning about the barrier being already
     destroyed. */
  pthread_create(&slp2, NULL, sleep1, (void*)bar4);
  /* create a thread, whose only purpose is to block on the barrier */
  pthread_create(&thr2, NULL, child1, (void*)bar4);
  /* guarantee that it gets there first */
  sleep(1);
  /* and now destroy */
  pthread_barrier_destroy(bar4);

  pthread_cancel(slp2);

  /* destroy a barrier that was never initialised.  This is a bit
     tricky, in that we have to fill the barrier with bytes which
     ensure that the pthread_barrier_destroy call doesn't crash for
     some reason.  One-fill seems to work ok on amd64-linux (glibc
     2.8). */
  fprintf(stderr, "\ndestroy a barrier that was never initialised\n");
  /* Create a thread that just exits the process after some sleep.
     We are really done at this point, even if we hang. */
  pthread_create(&ext1, NULL, exit1, NULL);
  bar5 = malloc(sizeof(pthread_barrier_t));
  assert(bar5);
  memset(bar5, 1, sizeof(*bar5));
#if !defined(VGO_freebsd)
  pthread_barrier_destroy(bar5);
#endif

  /* now we need to clean up the mess .. But skip canceling threads.  */
  /* r= pthread_cancel(thr1); assert(!r); // drd doesn't like it. Just exit.
  r= pthread_cancel(thr2); assert(!r); */

  free(bar1); free(bar2); free(bar3); free(bar4); free(bar5);

  /* Use exit, we want to kill any "sleeper threads". */
  exit (0);
}
