
#include <pthread.h>
#include <unistd.h>
#include <assert.h>
#include <signal.h>

/* Should see 3 threads exiting in different ways, all holding one (or
   two) locks. */

pthread_mutex_t mxC1  = PTHREAD_MUTEX_INITIALIZER;
pthread_mutex_t mxC2  = PTHREAD_MUTEX_INITIALIZER;
pthread_mutex_t mxC2b = PTHREAD_MUTEX_INITIALIZER;
pthread_mutex_t mxP   = PTHREAD_MUTEX_INITIALIZER;

/* This one exits in the normal way, by joining back */
void* child_fn1 ( void* arg )
{
  int r= pthread_mutex_lock( &mxC1 ); assert(!r);
  return NULL;
}

/* This one detaches, does its own thing. */
void* child_fn2 ( void* arg )
{
  int r;
  r= pthread_mutex_lock( &mxC2 ); assert(!r);
  r= pthread_mutex_lock( &mxC2b ); assert(!r);
  r= pthread_detach( pthread_self() ); assert(!r);
  return NULL;
}

/* Parent creates 2 children, takes a lock, waits, segfaults.  Use
   sleeps to enforce exit ordering, for repeatable regtesting. */
int main ( void )
{
   int r;
   pthread_t child1, child2;

   r= pthread_create(&child2, NULL, child_fn2, NULL); assert(!r);
   sleep(1);

   r= pthread_create(&child1, NULL, child_fn1, NULL); assert(!r);
   r= pthread_join(child1, NULL); assert(!r);
   sleep(1);

   r= pthread_mutex_lock( &mxP );
 
   kill( getpid(), SIGABRT );
   return 0;
}
