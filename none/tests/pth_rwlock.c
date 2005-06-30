#define _XOPEN_SOURCE 600

#include <pthread.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define LOCKS 2000

int main(int argc, char **argv)
{
  pthread_rwlock_t locks[LOCKS];
  int n;
  int e;
  
  for (n = 0; n < LOCKS; n++) {
    if ((e = pthread_rwlock_init(locks + n, NULL)) != 0) {
      fprintf(stderr, "pthread_rwlock_init[%d]: %s\n", n, strerror(e));
      exit(1);
    }
  }
   
  for (n = 0; n < LOCKS; n++) {
    if ((e = pthread_rwlock_destroy(locks + n)) != 0) {
      fprintf(stderr, "pthread_rwlock_destroy[%d]: %s\n", n, strerror(e));
      exit(1);
    }
  }
 
  exit(0);
}
