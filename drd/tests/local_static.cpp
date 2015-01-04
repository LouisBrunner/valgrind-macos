// A C++ compiler is supposed to have thread-safe statics

#include <cstdio>
#include <vector>
#include <pthread.h>

class Singleton
{
public:
  Singleton()
    : value(42)
  { }

  int value;
};

void* thread_func(void*)
{
  static Singleton singleton;

  fprintf(stderr, "%d\n", singleton.value);
  fprintf(stderr, "%d\n", singleton.value);
  fprintf(stderr, "%d\n", singleton.value);
  fprintf(stderr, "%d\n", singleton.value);
  fprintf(stderr, "%d\n", singleton.value);

  return 0;
}

int main(int, char**)
{
  std::vector<pthread_t> thread(2);
  void* v;

  for (std::vector<pthread_t>::iterator p = thread.begin(); p != thread.end();
       p++) {
    if (pthread_create(&*p, 0, thread_func, 0) != 0) {
      fprintf(stderr, "Creation of thread %d failed\n",
              (int)(&*p - &*thread.begin()));
      return 1;
    }
  }

  for (std::vector<pthread_t>::const_iterator p = thread.begin();
       p != thread.end(); p++) {
    pthread_join(*p, &v);
  }

  fprintf(stderr, "Done.\n");

  return 0;
}

