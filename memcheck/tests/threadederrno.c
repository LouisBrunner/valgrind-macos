
#include <pthread.h>
#include <stdio.h>
#include <errno.h>



void* thr2 ( void* v )
{
  FILE* f = fopen("bogus2", "r");
  printf("f2 = %p, errno2 = %d\n", f, errno);
  perror("wurble2");
  return NULL;
}

void* thr3 ( void* v )
{
  FILE* f = fopen("bogus3", "r");
  printf("f3 = %p, errno3 = %d\n", f, errno);
  perror("wurble3");
  return NULL;
}


int main ( void )
{
  FILE* f;
  pthread_t tid2, tid3;
  pthread_create(&tid2, NULL, &thr2, NULL);
  pthread_create(&tid3, NULL, &thr3, NULL);
  f = fopen("bogus", "r");
  printf("f1 = %p, errno1 = %d\n", f, errno);
  perror("wurble1");
  pthread_join(tid2, NULL);
  pthread_join(tid3, NULL);
  return 0;
}

