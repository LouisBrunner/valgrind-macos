/*
 * Test program for verifying whether pthread cleanup handlers are invoked
 * correctly.
 */


#include <stdbool.h>
#include <stdio.h>
#include <unistd.h>
#include <pthread.h>
#include <stdlib.h>


static pthread_mutex_t s_mutex;


static void cleanup_handler(void* param)
{
  fprintf(stderr, "Cleanup handler has been called.\n");
  pthread_mutex_unlock(&s_mutex);
}

static void* f(void *p)
{
  if (pthread_mutex_lock(&s_mutex) != 0)
  {
    fprintf(stderr, "pthread_mutex_lock()\n");
    exit(1);
  }

  pthread_cleanup_push(cleanup_handler, NULL);
  pthread_exit(0);
  pthread_cleanup_pop(true);
}


int main()
{
  pthread_t pt1, pt2;

  // Make sure the program exits in case a deadlock has been triggered.
  alarm(2);

  if (pthread_mutex_init(&s_mutex, NULL) != 0)
  {
    fprintf(stderr, "pthread_mutex_init()\n");
    exit(1);
  }
  if (pthread_create(&pt1, NULL, f, NULL) != 0)
  {
    fprintf(stderr, "pthread_create()\n");
    exit(1);
  }
  if (pthread_create(&pt2, NULL, f, NULL) != 0)
  {
    fprintf(stderr, "pthread_create()\n");
    exit(1);
  }

  pthread_join(pt1, 0);
  pthread_join(pt2, 0);

  fprintf(stderr, "Test succeeded.\n");

  return 0;
}
