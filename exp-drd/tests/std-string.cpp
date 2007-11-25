// Note: the code below is not yet sufficient for reproducing the race on
// basic_string<>::_Rep_base::_M_refcount


#include <iostream>
#include <pthread.h>
#include <string>
#include <unistd.h>


static std::string s_string;

static void* thread_func(void*)
{
  std::cout << "thread: string = " << s_string << std::endl;
  return 0;
}

int main(int argc, char** argv)
{
  const bool detached = argc <= 1;

  s_string = "(allocated by main thread)";

  pthread_t tid;
  pthread_attr_t attr;
  pthread_attr_init(&attr);
  pthread_attr_setdetachstate(&attr,
                              detached
                              ? PTHREAD_CREATE_DETACHED
                              : PTHREAD_CREATE_JOINABLE);
  pthread_create(&tid, &attr, thread_func, 0);
  pthread_attr_destroy(&attr);

  std::cout << std::flush;

  if (detached)
    sleep(1);
  else
    pthread_join(tid, 0);

  std::cout << std::flush;

  return 0;
}
