#include <pthread.h>

int main(int argc, char** argv)
{
  pthread_barrier_t b;
  pthread_barrier_init(&b, 0, 1);
  pthread_barrier_init(&b, 0, 1);
  return 0;
}
