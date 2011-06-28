/* This program attempts to verify that all functions that are
   supposed to be wrapped by tc_intercepts.c really are wrapped.  The
   main way it does this is to cause failures in those functions, so
   as to obtain various error messages which imply that the wrapper
   really did engage.

   Any regressions shown up by this program are potentially serious
   and should be investigated carefully. */

/* Needed for older glibcs (2.3 and older, at least) who don't
   otherwise "know" about some more exotic pthread stuff, in this case
   PTHREAD_MUTEX_ERRORCHECK. */
#define _GNU_SOURCE 1
#include <stdio.h>
#include <string.h>
#include <assert.h>
#include <unistd.h>
#include <pthread.h>
#include <semaphore.h>

#if !defined(__APPLE__)

#if !defined(__GLIBC_PREREQ)
# error "This program needs __GLIBC_PREREQ (in /usr/include/features.h)"
#endif

short unprotected = 0;

void* lazy_child ( void* v ) {
   assert(0); /* does not run */
}

void* racy_child ( void* v ) {
   unprotected = 1234;
   return NULL;
}

int main ( void )
{
   int r;
   /* pthread_t thr; */
   /* pthread_attr_t thra; */
   pthread_mutexattr_t mxa, mxa2;
   pthread_mutex_t mx, mx2, mx3, mx4;
   pthread_cond_t cv;
   struct timespec abstime;
   pthread_rwlock_t rwl;
   pthread_rwlock_t rwl2;
   pthread_rwlock_t rwl3;
   sem_t s1;

#  if __GLIBC_PREREQ(2,4)
   fprintf(stderr, 
           "\n\n------ This is output for >= glibc 2.4 ------\n");
#  else
   fprintf(stderr,
           "\n\n------ This is output for < glibc 2.4 ------\n");
#  endif

   /* --------- pthread_create/join --------- */

   fprintf(stderr,
   "\n---------------- pthread_create/join ----------------\n\n");

   /* make pthread_create fail */
   /* It's amazingly difficult to make pthread_create fail
      without first soaking up all the machine's resources.
      Instead, in order to demonstrate that it's really wrapped,
      create a child thread, generate a race error, and join with it
      again. */
   /* This just segfaults:
      memset( &thra, 0xFF, sizeof(thra) );
      r= pthread_create( &thr, NULL, lazy_child, NULL ); assert(r);
   */
   { pthread_t child;
     r= pthread_create( &child, NULL, racy_child, NULL ); assert(!r);
     sleep(1); /* just to ensure parent thread reports race, not child */
     unprotected = 5678;
     r= pthread_join( child, NULL ); assert(!r);
   }

   /* make pthread_join fail */
   r= pthread_join( pthread_self(), NULL ); assert(r);

   /* --------- pthread_mutex_lock et al --------- */

   fprintf(stderr,
   "\n---------------- pthread_mutex_lock et al ----------------\n\n");

   /* make pthread_mutex_init fail */
   memset( &mxa, 0xFF, sizeof(mxa) );
   r= pthread_mutex_init( &mx, &mxa );
#  if __GLIBC_PREREQ(2,4)
   assert(r); /* glibc >= 2.4: the call should fail */
#  else
   assert(!r); /* glibc < 2.4: oh well, glibc didn't bounce this */
#  endif

   /* make pthread_mutex_destroy fail */
   r= pthread_mutex_init( &mx2, NULL ); assert(!r);
   r= pthread_mutex_lock( &mx2 ); assert(!r);
   r= pthread_mutex_destroy( &mx2 ); assert(r);

   /* make pthread_mutex_lock fail (skipped on < glibc 2.4 because it
      doesn't fail, hence hangs the test) */
#  if __GLIBC_PREREQ(2,4)
   memset( &mx3, 0xFF, sizeof(mx3) );
   r= pthread_mutex_lock( &mx3 ); assert(r);
#  else
   fprintf(stderr, "\nmake pthread_mutex_lock fail: "
                   "skipped on glibc < 2.4\n\n");
#  endif

   /* make pthread_mutex_trylock fail */
   memset( &mx3, 0xFF, sizeof(mx3) );
   r= pthread_mutex_trylock( &mx3 ); assert(r);

   /* make pthread_mutex_timedlock fail */
   memset( &abstime, 0, sizeof(abstime) );
   memset( &mx3, 0xFF, sizeof(mx3) );
   r= pthread_mutex_timedlock( &mx3, &abstime ); assert(r);

   /* make pthread_mutex_unlock fail */
   memset( &mx3, 0xFF, sizeof(mx3) );
   r= pthread_mutex_unlock( &mx3 );
#  if __GLIBC_PREREQ(2,4)
   assert(r);
#  else
   assert(!r);
#  endif

   /* --------- pthread_cond_wait et al --------- */

   fprintf(stderr,
   "\n---------------- pthread_cond_wait et al ----------------\n\n");

   /* make pthread_cond_wait fail.  This is difficult.  Our cunning
      plan (tm) is to show up at pthread_cond_wait bearing a
      not-locked mutex of the ERRORCHECK flavour and hope (as is
      indeed the case with glibc-2.5) that pthread_cond_wait notices
      it is not locked, and bounces our request. */
   r= pthread_mutexattr_init( &mxa2 ); assert(!r);
   r= pthread_mutexattr_settype( &mxa2, PTHREAD_MUTEX_ERRORCHECK );
      assert(!r);
   r= pthread_mutex_init( &mx4, &mxa2 ); assert(!r);
   r= pthread_cond_init( &cv, NULL ); assert(!r);
   r= pthread_cond_wait( &cv, &mx4 ); assert(r);
   r= pthread_mutexattr_destroy( &mxa2 ); assert(!r);

   /* make pthread_cond_signal fail.  FIXME: can't figure out how
      to */
   r= pthread_cond_signal( &cv ); assert(!r);
   fprintf(stderr, "\nFIXME: can't figure out how to "
                   "verify wrap of pthread_cond_signal\n\n");

   /* make pthread_cond_broadcast fail.  FIXME: can't figure out how
      to */
   r= pthread_cond_broadcast( &cv ); assert(!r);
   fprintf(stderr, "\nFIXME: can't figure out how to "
                   "verify wrap of pthread_broadcast_signal\n\n");

   /* make pthread_cond_timedwait fail. */
   memset( &abstime, 0, sizeof(abstime) );
   abstime.tv_nsec = 1000000000 + 1;
   r= pthread_cond_timedwait( &cv, &mx4, &abstime ); assert(r);

   /* --------- pthread_rwlock_* --------- */

   fprintf(stderr,
   "\n---------------- pthread_rwlock_* ----------------\n\n");

   /* pthread_rwlock_init, pthread_rwlock_unlock */
   /* pthread_rwlock_init: can't make glibc's implementation fail.
      However, can demonstrate interceptedness by initialising but not
      locking a lock and then unlocking it.  Then the unlock call
      should say "first seen at .. the init call."  So this tests
      wrappedness of both calls. */
   r= pthread_rwlock_init( &rwl, NULL ); assert(!r);
   r= pthread_rwlock_unlock( &rwl ); 
   /* assert(r); *//* glibc doesn't complain.  It really ought to. Oh well. */

   /* We can infer the presence of wrapping for pthread_rwlock_rdlock,
      pthread_rwlock_wrlock and pthread_rwlock_unlock by making
      Thrcheck count the lockedness state, and warning when we unlock
      a not-locked lock.  Thusly: */
   r= pthread_rwlock_init( &rwl2, NULL ); assert(!r);

   /* w-lock it */
   fprintf(stderr, "(1) no error on next line\n");
   r= pthread_rwlock_wrlock( &rwl2 ); assert(!r);
   /* unlock it */
   fprintf(stderr, "(2) no error on next line\n");
   r= pthread_rwlock_unlock( &rwl2 ); assert(!r);
   /* unlock it again, get an error */
   fprintf(stderr, "(3)    ERROR on next line\n");
   r= pthread_rwlock_unlock( &rwl2 ); assert(!r);

   /* same game with r-locks */
   r= pthread_rwlock_init( &rwl2, NULL ); assert(!r);
   /* r-lock it twice */
   fprintf(stderr, "(4) no error on next line\n");
   r= pthread_rwlock_rdlock( &rwl2 ); assert(!r);
   fprintf(stderr, "(5) no error on next line\n");
   r= pthread_rwlock_rdlock( &rwl2 ); assert(!r);
   /* unlock it twice */
   fprintf(stderr, "(6) no error on next line\n");
   r= pthread_rwlock_unlock( &rwl2 ); assert(!r);
   fprintf(stderr, "(7) no error on next line\n");
   r= pthread_rwlock_unlock( &rwl2 ); assert(!r);
   /* unlock it again, get an error */
   fprintf(stderr, "(8)    ERROR on next line\n");
   r= pthread_rwlock_unlock( &rwl2 ); assert(!r);

   /* Lock rwl3 so the locked-lock-at-dealloc check can complain about
      it. */
   r= pthread_rwlock_init( &rwl3, NULL ); assert(!r);
   r= pthread_rwlock_rdlock( &rwl3 ); assert(!r);

   /* ------------- sem_* ------------- */

   /* This is pretty lame, and duplicates tc18_semabuse.c. */

   fprintf(stderr,
   "\n---------------- sem_* ----------------\n\n");

   /* verifies wrap of sem_init */
   /* Do sem_init with huge initial count - fails */
   r= sem_init(&s1, 0, ~0); assert(r);

   /* initialise properly */
   r= sem_init(&s1, 0, 0);

   /* in glibc, sem_destroy is a no-op; making it fail is
      impossible. */
   fprintf(stderr, "\nFIXME: can't figure out how to verify wrap of "
                   "sem_destroy\n\n");

   /* verifies wrap of sem_wait */
   /* Do 'wait' on a bogus semaphore.  This should fail, but on glibc
      it succeeds. */
   memset(&s1, 0x55, sizeof(s1));
   r= sem_wait(&s1); /* assert(r != 0); */

   /* this only fails with glibc 2.7 or later. */
   r= sem_post(&s1);
   fprintf(stderr, "\nFIXME: can't figure out how to verify wrap of "
                   "sem_post\n\n");

   sem_destroy(&s1);

   /* ------------- dealloc of mem holding locks ------------- */

   fprintf(stderr,
   "\n------------ dealloc of mem holding locks ------------\n\n");

   /* At this point it should complain about deallocation
      of memory containing locked locks:
         rwl3
   */

   return 0;
}

#else /* defined(__APPLE__) */
int main ( void )
{
   fprintf(stderr, "This program does not work on Mac OS X.\n");
   return 0;
}
#endif
