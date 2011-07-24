
/* Test simple use of the disable/enable macros. */

#include <stdlib.h>
#include <stdio.h>

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

int main ( void )
{
  block = malloc(10);
  free(block);

  fprintf(stderr, "\n--------- SIMPLE TEST ---------\n\n");
  fprintf(stderr, "\n--------- enabled (expect 1) ---------\n\n");

  err();

  fprintf(stderr, "\n--------- disabled (expect 0) ---------\n\n");
  VALGRIND_DISABLE_ERROR_REPORTING;

  err();

  fprintf(stderr, "\n--------- re-enabled (expect 1) ---------\n\n");
  VALGRIND_ENABLE_ERROR_REPORTING;

  err();



  fprintf(stderr, "\n--------- MULTI-LEVEL TEST (expect 2) ---------\n\n");

  // 4 times
  VALGRIND_DISABLE_ERROR_REPORTING;
  VALGRIND_DISABLE_ERROR_REPORTING;
  VALGRIND_DISABLE_ERROR_REPORTING;
  VALGRIND_DISABLE_ERROR_REPORTING; // lev = 4

  // now gradually undo them until an error appears
  err();  // hidden

  VALGRIND_ENABLE_ERROR_REPORTING; // lev = 3
  err();  // hidden

  VALGRIND_ENABLE_ERROR_REPORTING; // lev = 2
  err();  // hidden

  VALGRIND_ENABLE_ERROR_REPORTING; // lev = 1
  err();  // hidden

  VALGRIND_ENABLE_ERROR_REPORTING; // lev = 0
  err();  // visible

  VALGRIND_ENABLE_ERROR_REPORTING; // lev = 0 (won't go down further)
  err();  // visible

  fprintf(stderr, "\n--------- MULTI-LEVEL TEST end ---------\n\n");

  return 0;
}
