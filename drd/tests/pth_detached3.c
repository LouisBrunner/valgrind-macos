/* Invoke pthread_detach() with an invalid thread ID. */

#include <assert.h>
#include <errno.h>
#include <pthread.h>
#include <stdio.h>
#include <stdint.h>

static void* thread_func(void* arg)
{
  return 0;
}

int main(int argc, char** argv)
{
  pthread_t thread;

  pthread_create(&thread, NULL, thread_func, NULL);
  pthread_join(thread, NULL);

  /* Invoke pthread_detach() with the thread ID of a joined thread. */
  pthread_detach(thread);

  /* Invoke pthread_detach() with an invalid thread ID. */
  pthread_detach((pthread_t)((uintptr_t)thread + 8));

  fprintf(stderr, "Finished.\n");

  return 0;
}
