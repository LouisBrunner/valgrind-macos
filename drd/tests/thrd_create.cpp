#include <threads.h>
#include <iostream>

int main()
{
  thrd_t thr;
  thrd_create(&thr, [](void *arg){std::cerr << "Hello, world!\n"; return 0;},
              NULL);
  thrd_join(thr, NULL);
  return 0;
}
