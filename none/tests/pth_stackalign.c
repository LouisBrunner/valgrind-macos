#include <inttypes.h>
#include <pthread.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

static void *thread_main(void *arg)
{
  uintptr_t address = (uintptr_t)&arg;

  printf("alignment = %" PRIuPTR "\n", address & 3U);

  return NULL;
}

int main(int argc, char **argv)
{
  pthread_t t;
  int e;

  if ((e = pthread_create(&t, NULL, thread_main, NULL)) != 0)
    {
      fprintf(stderr, "pthread_create: %s\n", strerror(e));
      exit(1);
    }
  
  if ((e = pthread_join(t, NULL)) != 0)
    {
      fprintf(stderr, "pthread_join: %s\n", strerror(e));
      exit(1);
    }

  exit(0);
}
