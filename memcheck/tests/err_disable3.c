
/* Check that a child thread doesn't inherit its parent's disablement
   status. */

#include <stdlib.h>
#include <stdio.h>
#include <assert.h>
#include <pthread.h>
#include <unistd.h>    // sleep

#include "../include/valgrind.h"

char* block = NULL;

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

void* child_fn ( void* arg )
{
   fprintf(stderr, "\n--------- c: start (expect 1) ---------\n\n");
   err();
   fprintf(stderr, "\n--------- c: end ---------\n\n");
   return NULL;
}

int main ( void )
{
  int r;
  pthread_t child;

  block = malloc(10);
  free(block);

  fprintf(stderr, "\n--------- p: disabling errors (expect 0) ---------\n\n");

  VALGRIND_DISABLE_ERROR_REPORTING;
  err();

  fprintf(stderr, "\n--------- p: creating child ---------\n\n");

  r = pthread_create(&child, NULL, child_fn, NULL);
  assert(!r);
  sleep(1); // let the child run first (determinism fix)
  fprintf(stderr, "\n--------- p: join child ---------\n\n");
  r = pthread_join(child, NULL);
  assert(!r);

  fprintf(stderr, "\n--------- p: re_enabled (expect 1) ---------\n\n");
  VALGRIND_ENABLE_ERROR_REPORTING;
  err();

  return 0;
}
