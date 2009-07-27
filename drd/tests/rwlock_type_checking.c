/**
 * @file  rwlock_type_checking.c
 *
 * @brief Test whether DRD reports attempts to use a user-defined rwlock as
 *        a POSIX rwlock and vice versa.
 */


#define _GNU_SOURCE 1

#include <pthread.h>
#include <stdio.h>
#include "../../config.h"
#include "../../drd/drd.h"


int main(int argc, char** argv)
{
  pthread_rwlock_t posix_rwlock;
  int user_defined_rwlock;

  ANNOTATE_RWLOCK_CREATE(&user_defined_rwlock);
  pthread_rwlock_init(&posix_rwlock, 0);

  pthread_rwlock_rdlock((pthread_rwlock_t*)&user_defined_rwlock);

  ANNOTATE_READERLOCK_ACQUIRED(&posix_rwlock);

  pthread_rwlock_destroy(&posix_rwlock);
  ANNOTATE_RWLOCK_DESTROY(&user_defined_rwlock);

  fprintf(stderr, "Finished.\n");

  return 0;
}
