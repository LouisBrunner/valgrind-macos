
#include <pthread.h>
#include <stdio.h>
#include <stdlib.h>

/* Simple test program, no race: parent only modifies x after child
   has modified it and then joined with the parent.  Tests simple
   thread lifetime segment handling. */

/* A simple function to "use" a value, so that gcc can't
   possibly optimise it into nothing. */
static void use ( int x ) {
   __asm__ __volatile__( "nop" : : "r"(x) : "cc","memory" );
}

static void* worker_thread ( void* argV )
{
  int* arg = (int*)argV;
  use(arg[5]); /* read access */
  return NULL;
}

int main ( void )
{
   pthread_t thread_id;
   volatile int* x = malloc(10 * sizeof(int));
   x[5] = 1;
   /* x[5] is Excl(parent) */

   pthread_create(&thread_id, 0, worker_thread, (void*)x);

   use(x[5]); /* read access */

   /* Just before the threads join, x[5] is ShR (read by both parent
      and child) */
   pthread_join(thread_id, 0);
   /* x[5] is Excl(parent), because only parent and child accessed it
      and child has merged to parent.  So now it's ok for parent to
      access it without locking. */

   x[5] = 0; /* write access */

   return x[5];
}
