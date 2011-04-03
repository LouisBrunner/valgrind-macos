
/* Do stupid things with semaphores, and check that Thrcheck doesn't
   fall over and does report errors appropriately.  If nothing else
   this just checks that the relevant functions are getting
   intercepted. */

/* This is pretty lame, because making the sem_ functions fail is
   difficult.  Not sure it's really worth having. */
#include <unistd.h>
#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include <pthread.h>
#include <semaphore.h>
#include <string.h>
void start_watchdog ( void );
int main ( void )
{
  int r __attribute__((unused));
  sem_t s1;
  start_watchdog();
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

  /* this only fails with glibc 2.7 and later. */
  r= sem_post(&s1);

  sem_destroy(&s1);

  return 0;
}

void* watchdog ( void* v )
{
  sleep(10);
  fprintf(stderr, "watchdog timer expired - not a good sign\n");
  exit(0);
}

void start_watchdog ( void )
{
  pthread_t t;
  int r;
  r= pthread_create(&t, NULL, watchdog, NULL);
  assert(!r);
}
