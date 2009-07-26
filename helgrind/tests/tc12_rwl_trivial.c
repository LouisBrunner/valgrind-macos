
/* Needed for older glibcs (2.3 and older, at least) who don't
   otherwise "know" about pthread_rwlock_anything or about
   PTHREAD_MUTEX_RECURSIVE (amongst things). */
#define _GNU_SOURCE 1

#include <stdio.h>
#include <pthread.h>
#include <assert.h>

#if defined(VGO_darwin)
#define OS_IS_DARWIN 1
#else
#define OS_IS_DARWIN 0
#endif

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
  r = pthread_rwlock_unlock( &rwl );      assert(OS_IS_DARWIN || r == 0);

  r = pthread_rwlock_destroy( &rwl );     assert(r == 0);

  return 0;
}
