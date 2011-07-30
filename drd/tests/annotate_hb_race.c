/*
 * Test program with happens-before / happens-after annotations that triggers
 * a data race. The data race will only be reported if happens-after
 * annotations that occur in different threads are not totally ordered. Or:
 * this is a test for the implementation of ordering annotations.
 */


#include <stdio.h>
#include <pthread.h>
#include "unified_annotations.h"


static int s_i;


static void* thread_func(void* arg)
{
  int i;

  U_ANNOTATE_HAPPENS_AFTER(&s_i);
  i = s_i;
  U_ANNOTATE_HAPPENS_AFTER(&s_i);
  *(int*)arg = i;
  return NULL;
}

int main(int argc, char** argv)
{
  const struct timespec delay = { 0, 100 * 1000 * 1000 };
  pthread_t tid[2];
  int result[2];

  U_ANNOTATE_HAPPENS_BEFORE(&s_i);
  pthread_create(&tid[0], 0, thread_func, &result[0]);
  pthread_create(&tid[1], 0, thread_func, &result[1]);

  nanosleep(&delay, 0);

  s_i = 1;

  pthread_join(tid[0], NULL);
  pthread_join(tid[1], NULL);

  fprintf(stderr, "Done.\n");

  return 0;
}

/*
 * Local variables:
 * c-basic-offset: 2
 * End:
 */
