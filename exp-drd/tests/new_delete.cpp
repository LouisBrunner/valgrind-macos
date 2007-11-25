#include <iostream>
#include <pthread.h>

void* thread_func(void*)
{
  delete new int;
  return 0;
}

int main(int argc, char** argv)
{
  pthread_t tid;
  std::cout << "main, before pthread_create()\n" << std::flush;
  pthread_create(&tid, 0, thread_func, 0);
  std::cout << "main, after pthread_create()\n" << std::flush;
  delete new int;
  std::cout << "main, before pthread_join()\n" << std::flush;
  pthread_join(tid, 0);
  std::cout << "main, after pthread_join()\n" << std::flush;
  return 0;
}
