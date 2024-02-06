#include <stdio.h>
#include <threads.h>

int thread_entry(void *arg)
{
  fprintf(stderr, "Hello, world!\n");
  return 0;
}

int main()
{
  thrd_t thr;
  thrd_create(&thr, thread_entry, NULL);
  thrd_join(thr, NULL);
  return 0;
}
