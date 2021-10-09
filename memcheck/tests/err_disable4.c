
/* Check that recycling thread slots doesn't cause new threads to
   inherit the disablement status of the previous thread to occupy
   that slot.

   1. Create N threads, disable error reporting in them, and get them
      all to exit (join with them).  That creates N thread slots that
      were vacated by threads with error reporting disabled.  There
      should be N complaints about threads exiting with errors
      disabled.

   2. Create N new threads and get them to wait at a barrier.

   3. Let them all go past the barrier and call err().  There
      should be N resulting error reports.

   4. Join with the N threads.
*/

#include <stdlib.h>
#include <stdio.h>
#include <assert.h>
#include <pthread.h>
#include <semaphore.h>
#include <limits.h>    /* PTHREAD_STACK_MIN */
#include "../include/valgrind.h"

char* block = NULL;
#  if !defined(VGO_darwin)
sem_t sem;
#  else
sem_t *sem;
static const char *semname = "Semaphore1";
#  endif

__attribute__((noinline)) void usechar ( char c )
{
   // Spook gcc into believing mysterious bad things are
   // happening behind its back, and that 'c' is definitely
   // used in some (unknown) way.
   __asm__ __volatile__("" : : "r"(c) : "memory","cc");
}

__attribute__((noinline)) void err ( void )
{
   usechar( block[5] );
}

void* child_fn_1 ( void* arg )
{
   // Disable error reporting, then wait to exit
   VALGRIND_DISABLE_ERROR_REPORTING;
#  if !defined(VGO_darwin)
   int r = sem_wait(&sem);  assert(!r);
#  else
   int r = sem_wait(sem);  assert(!r);
#  endif
   return NULL;
}

void* child_fn_2 ( void* arg )
{
   // make an error, then wait to exit
   err();
#  if !defined(VGO_darwin)
   int r = sem_wait(&sem);  assert(!r);
#  else
   int r = sem_wait(sem);  assert(!r);
#  endif
   return NULL;
}

#define NTHREADS 498 // VG_N_THREADS - 2

int main ( void )
{
  int r, i;
  pthread_t child[NTHREADS];

  block = malloc(10);
  free(block);

  // part 1
  fprintf(stderr, "\n-------- Letting %d threads exit "
                  "w/ errs disabled ------\n\n",
          NTHREADS);

  // set up the semaphore
#  if !defined(VGO_darwin)
  r = sem_init(&sem, 0, 0);  assert(!r);
#  else
  sem = sem_open(semname, O_CREAT, 0777, 0);  assert(!(sem == SEM_FAILED));
#  endif

  pthread_attr_t attr;
  r = pthread_attr_init(&attr); assert(!r);
#if !defined(VGO_freebsd)
  r = pthread_attr_setstacksize(&attr, PTHREAD_STACK_MIN);
#endif

  // create N threads to do child_fn_1 ...
  for (i = 0; i < NTHREADS; i++) {
     r = pthread_create(&child[i], &attr, child_fn_1, NULL);
     assert(!r);
  }

  // let them all exit
  for (i = 0; i < NTHREADS; i++) {
#  if !defined(VGO_darwin)
     r = sem_post(&sem);  assert(!r);
#  else
     r = sem_post(sem);  assert(!r);
#  endif
  }

  // join
  for (i = 0; i < NTHREADS; i++) {
     r = pthread_join(child[i], NULL);  assert(!r);
  }

  // part 2

  fprintf(stderr, "\n-------- Letting %d threads make an error "
                  "------\n\n",
          NTHREADS);
  // semaphore is already back at zero

  // create N threads to do child_fn_2 ...
  for (i = 0; i < NTHREADS; i++) {
     r = pthread_create(&child[i], &attr, child_fn_2, NULL);
     assert(!r);
  }

  // let them all exit
  for (i = 0; i < NTHREADS; i++) {
#  if !defined(VGO_darwin)
     r = sem_post(&sem);  assert(!r);
#  else
     r = sem_post(sem);  assert(!r);
#  endif
  }

  // join
  for (i = 0; i < NTHREADS; i++) {
     r = pthread_join(child[i], NULL);  assert(!r);
  }

  // Print the final error counts.  There need to be 498 errors
  // in 1 context.  Anything else, and something is not right.
  int nerrors = VALGRIND_COUNT_ERRORS;
  fprintf(stderr, "\n-------- Got %d errors (expected %d ==> %s) ------\n\n", 
          nerrors, NTHREADS, nerrors == NTHREADS ? "PASS" : "FAIL" );

  return 0;
}
