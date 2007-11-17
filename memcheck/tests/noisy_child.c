
#include <stdlib.h>
#include <sys/types.h>
#include <unistd.h>
#include <assert.h>

void do_child_badness ( char* p )
{
   /* Free it a second time */
   free(p);
}

void do_parent_badness ( char* p )
{
   /* Do a write off the end */
   p[10] = 42;
}


int main ( void )
{
  pid_t child;
  char* p = malloc(10); assert(p);
  free(p);

  /* parent does something bad */
  p[5] = 22;

  child = fork();
  assert(child != -1); /* assert fork did not fail */

  if (child == 0) {
     /* I am the child */
     do_child_badness(p);
  } else {
     /* I am the parent */
     do_parent_badness(p);
  }

  return 0;

}
