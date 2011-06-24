
#include <pthread.h>
#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include <unistd.h>

/* Test of the mechanism for showing all locks held by a thread.  Test
   the case where the earlier thread held, at the time of the access,
   some locks, at least one of which is deleted by the time the second
   access (the race) happens.  This causes problems for Helgrind's
   error reporting mechanism in that it can no longer show the deleted
   lock in the error message.x */

pthread_mutex_t mx1a;
pthread_mutex_t mx1b;
pthread_mutex_t mx2a;
pthread_mutex_t mx2b;

int x = 0;

void* child_fn1 ( void* arg )
{
   int r;
   // We are the first-accessing thread.  Take and release two locks
   // and then destroy one of them.
   r= pthread_mutex_lock(&mx1a);  assert(!r);
   r= pthread_mutex_lock(&mx1b);  assert(!r);
   x = 1;
   r= pthread_mutex_unlock(&mx1b);  assert(!r);
   r= pthread_mutex_unlock(&mx1a);  assert(!r);
   r= pthread_mutex_destroy(&mx1a);  assert(!r);
   sleep(1);
   return NULL;
}

void* child_fn2 ( void* arg )
{
   int r;
   // We are the second-accessing thread.  Take and release
   // our two locks, but don't otherwise mess with them. 
   sleep(1);
   r= pthread_mutex_lock(&mx2a);  assert(!r);
   r= pthread_mutex_lock(&mx2b);  assert(!r);
   x = 1;
   r= pthread_mutex_unlock(&mx2b);  assert(!r);
   r= pthread_mutex_unlock(&mx2a);  assert(!r);
   return NULL;
}

int main ( int argc, char** argv )
{
   pthread_t child1, child2;
   int r;

   r= pthread_mutex_init(&mx1a, NULL);  assert(!r);
   r= pthread_mutex_init(&mx1b, NULL);  assert(!r);
   r= pthread_mutex_init(&mx2a, NULL);  assert(!r);
   r= pthread_mutex_init(&mx2b, NULL);  assert(!r);

   r= pthread_create(&child2, NULL, child_fn2, NULL);  assert(!r);
   r= pthread_create(&child1, NULL, child_fn1, NULL);  assert(!r);

   r= pthread_join(child1, NULL);  assert(!r);
   r= pthread_join(child2, NULL);  assert(!r);

   // don't destroy mx1a; it's already destroyed.
   r= pthread_mutex_destroy(&mx1b);  assert(!r);
   r= pthread_mutex_destroy(&mx2a);  assert(!r);
   r= pthread_mutex_destroy(&mx2b);  assert(!r);

   return 0;
}
