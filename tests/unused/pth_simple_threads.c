
#include <stdio.h>
#include <pthread.h>

int r1 = 0, r2 = 0;

void do_one_thing ( int* ntimes )
{
  int i, j, x;
  for (i = 0; i < 4; i++) {
    printf ("doing one thing\n");
    for (j = 0; j < 100000; j++) x = x + i;
    (*ntimes)++;
  }
}

void do_another_thing ( int* ntimes )
{
  int i, j, x;
  for (i = 0; i < 4; i++) {
    printf ("doing another\n");
    for (j = 0; j < 100000; j++) x = x + i;
    (*ntimes)++;
  }
}

void do_wrap_up ( int one_times, int another_times )
{
  int total = one_times + another_times;
  printf("wrap up: one thing %d, another %d, total %d\n",
         one_times, another_times, total );
}

int main ( void )
{
  pthread_t t1, t2;
  pthread_create( &t1, NULL, (void*)do_one_thing, (void*)&r1 );
  pthread_create( &t2, NULL, (void*)do_another_thing, (void*)&r2 );
  //  while (1) {}
  pthread_join(t1, NULL);
  pthread_join(t2, NULL);
  do_wrap_up(r1,r2);
  return 0;
}
