/*
 * Test program that invokes pthread_create@GLIBC_2.0().
 *
 * Note: pthread_create@GLIBC_2.0() is only available in 32-bit glibc versions,
 * not in 64-bit versions.
 */


#include <pthread.h>
#include <stdio.h>


extern int pthread_create_glibc_2_0(pthread_t*, const pthread_attr_t*,
                                    void *(*)(void*), void*);

__asm__(".symver pthread_create_glibc_2_0, pthread_create@GLIBC_2.0");


static void* thread_func(void *arg)
{
  fprintf(stderr, "The thread.\n");
  return 0;
}

int main(int argc, char** argv)
{
  int result;
  pthread_t thr;

  result = (*pthread_create_glibc_2_0)(&thr, 0, thread_func, 0);
  if (result != 0)
  {
    fprintf(stderr, "pthread_create() failed.\n");
    return 1;
  }
  pthread_join(thr, 0);
  fprintf(stderr, "Finished.\n");
  return 0;
}
