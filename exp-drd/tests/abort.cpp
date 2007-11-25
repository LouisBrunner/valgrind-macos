// assert(false) calls __assert_fail(), which in turn calls abort() and
// _IO_flush_all_lockp(). This last function triggers a race. Check that this
// race is suppressed. Note: the test program below is not sufficient for
// reproducing this race.


#include <iostream>
#include <fstream>
#include <cassert>
#include <pthread.h>

static pthread_mutex_t s_mutex;

void* thread_func(void*)
{
  pthread_mutex_lock(&s_mutex);
  pthread_mutex_unlock(&s_mutex);
  std::cout << "thread\n";
  assert(false);
  return 0;
}

int main(int argc, char** argv)
{
  pthread_mutex_init(&s_mutex, 0);
  pthread_t tid;
  pthread_mutex_lock(&s_mutex);
  pthread_create(&tid, 0, thread_func, 0);
  FILE* fp = fopen("/tmp/valgrind-drd-tests-abort", "w");
  fprintf(fp, "x");
  pthread_mutex_unlock(&s_mutex);
  pthread_join(tid, 0);
  pthread_mutex_destroy(&s_mutex);
  fclose(fp);

  return 0;
}
