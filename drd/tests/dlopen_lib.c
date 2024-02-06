#include <stdio.h>
#include <stdint.h>
#include <pthread.h>
#include "dlopen_lib.h"

void *PrintHello(void *threadid)
{
  const long tid = (uintptr_t)threadid;

  printf("Hello World! It's me, thread #%ld!\n", tid);
  pthread_exit(NULL);
}


void foo()
{
  pthread_t thread;
  int rc;
  uintptr_t t = 1;

  printf("In main: creating thread %u\n", (unsigned)t);
  rc = pthread_create(&thread, NULL, PrintHello, (void *)t);
  if (rc)
    printf("ERROR; return code from pthread_create() is %d\n", rc);
  else
    pthread_join(thread, NULL);
}
