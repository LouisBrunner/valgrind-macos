
#include <stdio.h>
#include <assert.h>

#define __USE_GNU
#include <pthread.h>

void do_one_thing ( void* v )
{
  int i, j, res;
  for (i = 0; i < 10; i++) {
    for (j = 0; j < 10; j++) {
       printf("a "); fflush(stdout);
    }
    printf("\naaaaaaa-yielding\n");
    res = pthread_yield();
    assert(res == 0);
  }
}

void do_another_thing ( void* v )
{
  int i, j, res;
  for (i = 0; i < 10; i++) {
    for (j = 0; j < 10; j++) {
       printf("b "); fflush(stdout);
    }
    printf("\nbbbbbbb-yielding\n");
    res = pthread_yield();
    assert(res == 0);
  }
}


int main ( void )
{
  pthread_t t1, t2;
  pthread_create( &t1, NULL, (void*)do_one_thing, NULL );
  pthread_create( &t2, NULL, (void*)do_another_thing, NULL );
  pthread_join(t1, NULL);
  pthread_join(t2, NULL);
  printf("bye!\n");
  return 0;
}
