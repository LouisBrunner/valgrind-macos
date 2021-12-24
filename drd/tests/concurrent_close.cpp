/*
 * Reproducer for bug #323905. See also
 * http://bugs.kde.org/show_bug.cgi?id=323905.
 */

#include <climits>    /* PTHREAD_STACK_MIN */
#include <cstdio>     /* fprintf() */
#include <fcntl.h>    /* O_RDONLY */
#include <pthread.h>
#include <unistd.h>   /* close() */
#include "config.h"

/* Happens with two threads also */
#define THREAD_COUNT 256

void* thread(void*)
{
  int fd;

  /* Happens with any file, not just /dev/null */
  fd = open("/dev/null", O_RDONLY);
  if (fd >= 0)
    close(fd);
  else
    fprintf(stderr, "Failed to open /dev/null\n");
  return 0;
}

int main()
{
  int i, r;
  pthread_attr_t attr;
  pthread_t threads[THREAD_COUNT];

  pthread_attr_init(&attr);
#if !defined(VGO_freebsd)
  pthread_attr_setstacksize(&attr, PTHREAD_STACK_MIN);
#endif
  for (i = 0; i < THREAD_COUNT; ++i) {
    r = pthread_create(&threads[i], &attr, thread, 0);
    if (r != 0) {
      fprintf(stderr, "Failed to create thread %d\n", i);
      return 1;
    }
  }
  pthread_attr_destroy(&attr);
  for (i = 0; i < THREAD_COUNT; ++i) {
    r = pthread_join(threads[i], 0);
    if (r != 0) {
      fprintf(stderr, "Failed to join thread %d\n", i);
      return 1;
    }
  }
  fprintf(stderr, "Done.\n");
  return 0;
}
