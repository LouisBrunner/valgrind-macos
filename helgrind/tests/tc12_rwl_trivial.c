
/* Needed for older glibcs (2.3 and older, at least) who don't
   otherwise "know" about pthread_rwlock_anything or about
   PTHREAD_MUTEX_RECURSIVE (amongst things). */
#define _GNU_SOURCE 1

#include <stdio.h>
#include "safe-pthread.h"
#include <assert.h>

/* Do trivial stuff with a reader-writer lock. */

int main ( void )
{
  int r;
  pthread_rwlock_t rwl;

  r = pthread_rwlock_init( &rwl, NULL );  assert(r == 0);

  r = pthread_rwlock_wrlock( &rwl );      assert(r == 0);
  r = pthread_rwlock_unlock( &rwl );      assert(r == 0);

  r = pthread_rwlock_rdlock( &rwl );      assert(r == 0);
  r = pthread_rwlock_rdlock( &rwl );      assert(r == 0);
  r = pthread_rwlock_unlock( &rwl );      assert(r == 0);
  r = pthread_rwlock_unlock( &rwl );      assert(r == 0);

  /* this should fail - lock is unowned now */
  r = pthread_rwlock_unlock( &rwl );
#if defined(VGO_darwin) || defined(VGO_solaris) || defined(VGO_freebsd)
  assert(r != 0);
#else
  assert(r == 0);
#endif

  r = pthread_rwlock_destroy( &rwl );     assert(r == 0);

  return 0;
}
