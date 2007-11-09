
#include <pthread.h>
#include <stdio.h>
#include <stdlib.h>
#include <assert.h>

/* The simplest possible test that triggers a lock order acquisition
   error. */

int main ( void )
{
   int r;
   pthread_mutex_t mx1, mx2;
   r = pthread_mutex_init( &mx1, NULL ); assert(r==0);
   r = pthread_mutex_init( &mx2, NULL ); assert(r==0);

   r = pthread_mutex_lock( &mx1 ); assert(r==0);
   r = pthread_mutex_lock( &mx2 ); assert(r==0);

   r = pthread_mutex_unlock( &mx1 ); assert(r==0);
   r = pthread_mutex_unlock( &mx2 ); assert(r==0);

   r = pthread_mutex_lock( &mx2 ); assert(r==0); /* error */
   r = pthread_mutex_lock( &mx1 ); assert(r==0);

   r = pthread_mutex_unlock( &mx1 ); assert(r==0);
   r = pthread_mutex_unlock( &mx2 ); assert(r==0);

   r = pthread_mutex_destroy( &mx1 );
   r = pthread_mutex_destroy( &mx2 );

   return 0;
}
