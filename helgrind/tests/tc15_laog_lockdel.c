
#include <pthread.h>
#include <stdio.h>
#include <stdlib.h>
#include <assert.h>

/* Test that locks, having entered the lock acquisition tracking
   machinery, are forgotten by it when the memory they reside in
   becomes inaccessible. */

int main ( void )
{
   int r;
   pthread_mutex_t *mx1, *mx2;

   mx1 = malloc(sizeof(pthread_mutex_t));
   mx2 = malloc(sizeof(pthread_mutex_t));

   assert(mx1);
   assert(mx2);

   r = pthread_mutex_init( mx1, NULL ); assert(r==0);
   r = pthread_mutex_init( mx2, NULL ); assert(r==0);

   /* Establish order 1 -> 2 */
   fprintf(stderr, "Establish order 1 -> 2\n");
   r = pthread_mutex_lock( mx1 ); assert(r==0);
   r = pthread_mutex_lock( mx2 ); assert(r==0);

   r = pthread_mutex_unlock( mx1 ); assert(r==0);
   r = pthread_mutex_unlock( mx2 ); assert(r==0);

   /* Try order 2 -> 1.  This gives an error. */
   fprintf(stderr, "Try order 2 -> 1.  This gives an error.\n");
   r = pthread_mutex_lock( mx2 ); assert(r==0); /* error */
   r = pthread_mutex_lock( mx1 ); assert(r==0);

   r = pthread_mutex_unlock( mx1 ); assert(r==0);
   r = pthread_mutex_unlock( mx2 ); assert(r==0);

   /* Free 2 and re-allocate it.  This gives it a new identity, 
      so a second locking sequence 2 -> 1 should now be OK. */
   fprintf(stderr, 
           "Free 2 and re-allocate it.  This gives it a new identity,\n");
   fprintf(stderr, "so a second locking sequence 2 -> 1 should now be OK.\n");
   pthread_mutex_destroy( mx2 );
   free(mx2);
   mx2 = malloc(sizeof(pthread_mutex_t));
   assert(mx2);
   r = pthread_mutex_init( mx2, NULL ); assert(r==0);

   r = pthread_mutex_lock( mx2 ); assert(r==0); /* error */
   r = pthread_mutex_lock( mx1 ); assert(r==0);

   r = pthread_mutex_unlock( mx1 ); assert(r==0);
   r = pthread_mutex_unlock( mx2 ); assert(r==0);

   /* done */

   fprintf(stderr, "done\n");
   r = pthread_mutex_destroy( mx1 );
   r = pthread_mutex_destroy( mx2 );

   free( mx1 );
   free( mx2 );

   return 0;
}
