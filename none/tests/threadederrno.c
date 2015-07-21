/* Make sure we use the POSIX version of strerror_r() on Linux. */
#define _XOPEN_SOURCE 600

#include <pthread.h>
#include <stdio.h>
#include <errno.h>
#include <string.h>


void* thr2 ( void* v )
{
  char errstr[128];
  FILE* f = fopen("bogus2", "r");
  strerror_r(errno, errstr, sizeof(errstr));
  printf("f  = %ld, errno  = %d (%s)\n", (long)f, errno, errstr);
  return NULL;
}

void* thr3 ( void* v )
{
  char errstr[128];
  FILE* f = fopen("bogus3", "r");
  strerror_r(errno, errstr, sizeof(errstr));
  printf("f  = %ld, errno  = %d (%s)\n", (long)f, errno, errstr);
  return NULL;
}


int main ( void )
{
  char errstr[128];
  FILE* f;
  pthread_t tid2, tid3;
  pthread_create(&tid2, NULL, &thr2, NULL);
  pthread_create(&tid3, NULL, &thr3, NULL);
  f = fopen("bogus", "r");
  strerror_r(errno, errstr, sizeof(errstr));
  printf("f  = %ld, errno  = %d (%s)\n", (long)f, errno, errstr);
  pthread_join(tid2, NULL);
  pthread_join(tid3, NULL);
  return 0;
}

