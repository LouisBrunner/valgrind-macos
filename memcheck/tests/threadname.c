#define _GNU_SOURCE
#include <stdio.h>
#include <pthread.h>
#include <string.h>
#include <stdlib.h>
#include <sys/prctl.h>
#include <sys/types.h>
#include <unistd.h>
#include <assert.h>

static pthread_t children[3];

void bad_things(int offset)
{
  char* m = malloc(sizeof(char)*offset);
  m[offset] = 0;
  free(m);
}

void* child_fn_2 ( void* arg )
{
  const char* threadname = "012345678901234";

  pthread_setname_np(pthread_self(), threadname);

  bad_things(4);

  return NULL;
}

void* child_fn_1 ( void* arg )
{
  const char* threadname = "try1";
  int r;

  pthread_setname_np(pthread_self(), threadname);

  bad_things(3);

  r = pthread_create(&children[2], NULL, child_fn_2, NULL);
  assert(!r);

  r = pthread_join(children[2], NULL);
  assert(!r);

  return NULL;
}

void* child_fn_0 ( void* arg )
{
  int r;

  bad_things(2);

  r = pthread_create(&children[1], NULL, child_fn_1, NULL);
  assert(!r);

  r = pthread_join(children[1], NULL);
  assert(!r);

  return NULL;
}

int main(int argc, const char** argv)
{
  int r;

  bad_things(1);

  r = pthread_create(&children[0], NULL, child_fn_0, NULL);
  assert(!r);

  r = pthread_join(children[0], NULL);
  assert(!r);
  
  bad_things(5);

  return 0;
}

