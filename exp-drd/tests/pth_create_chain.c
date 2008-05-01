// Create threads in such a way that there is a realistic chance that the
// parent thread finishes before the created thread finishes.


#include <assert.h>
#include <stdlib.h>
#include <stdio.h>
#include <pthread.h>


static pthread_t s_thread[1000];
static int       s_arg[1000];

static void* thread_func(void* p)
{
  int thread_count = *(int*)(p);
  if (thread_count > 0)
  {
    thread_count--;
    // std::cout << "create " << thread_count << std::endl;
    s_arg[thread_count] = thread_count;
    pthread_create(&s_thread[thread_count], 0, thread_func,
                   &s_arg[thread_count]);
#if 0
    std::cout << "created " << thread_count << "(" << s_thread[thread_count]
              << ")" << std::endl;
#endif
  }
  return 0;
}

int main(int argc, char** argv)
{
  unsigned thread_count;
  int i;

  thread_count = argc > 1 ? atoi(argv[1]) : 50;
  assert(thread_count <= sizeof(s_thread) / sizeof(s_thread[0]));
  assert(thread_count >= 1);
  thread_count--;
  // std::cout << "create " << thread_count << std::endl;
  pthread_create(&s_thread[thread_count], 0, thread_func,
                 &thread_count);
#if 0
  std::cout << "created " << thread_count << "(" << s_thread[thread_count]
            << ")" << std::endl;
#endif
  for (i = thread_count; i >= 0; i--)
  {
    // std::cout << "join " << i << "(" << s_thread[i] << ")" << std::endl;
    pthread_join(s_thread[i], 0);
  }
  return 0;
}
