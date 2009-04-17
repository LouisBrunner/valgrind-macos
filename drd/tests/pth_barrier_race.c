/*
 * Test program that triggers a race between pthread_barrier_wait() and
 * pthread_barrier_destroy(): proper synchronization is missing between
 * the pthread_barrier_wait() and the pthread_barrier_destroy() calls. This
 * test program is based on the example that was posted on February 5, 2009 by
 * Christoph Bartoschek on the valgrind-users mailing list. Redistribution of
 * the source code below is permitted under the GPLv2 license.
 *
 * See also http://article.gmane.org/gmane.comp.debugging.valgrind/8945/match=pthread_barrier_wait
 */


#define _GNU_SOURCE

#include <pthread.h>
#include <stdlib.h>
#include <unistd.h>

static pthread_barrier_t* barrier;


static void* thread(void* arg)
{
  pthread_barrier_wait(barrier);
  return NULL;
}

int main()
{
  pthread_t tid;

  barrier = (pthread_barrier_t *) malloc(sizeof(*barrier));
  pthread_barrier_init(barrier, NULL, 2);

  pthread_create(&tid, NULL, thread, NULL);

  pthread_barrier_wait(barrier);
  /*
   * The sleep() call below ensures that the pthread_barrier_destroy() call
   * happens after the created thread has returned from pthread_barrier_wait().
   */
  sleep(1);
  pthread_barrier_destroy(barrier);
  free(barrier);

  pthread_join(tid, NULL);
  return 0;
}
