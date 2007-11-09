
/* Do stupid things with semaphores, and check that Thrcheck doesn't
   fall over and does report errors appropriately.  If nothing else
   this just checks that the relevant functions are getting
   intercepted. */

/* This is pretty lame, because making the sem_ functions fail is
   difficult.  Not sure it's really worth having. */

#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include <pthread.h>
#include <semaphore.h>
#include <string.h>

int main ( void )
{
  int r;
  sem_t s1;

  /* Do sem_init with huge initial count */
  r= sem_init(&s1, 0, ~0);

  /* initialise properly */
  r= sem_init(&s1, 0, 0);

  /* in glibc, sem_destroy is a no-op; making it fail is
     impossible. */

  /* Do 'wait' on a bogus semaphore.  This should fail, but on glibc
     it succeeds. */
  memset(&s1, 0x55, sizeof(s1));
  r= sem_wait(&s1); /* assert(r != 0); */

  /* this really ought to fail, but it doesn't. */
  r= sem_post(&s1); assert(!r);

  sem_destroy(&s1);

  return 0;
}
