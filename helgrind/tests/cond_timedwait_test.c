#include <pthread.h>
#include <string.h>

int main() {
  pthread_mutex_t mutex = PTHREAD_MUTEX_INITIALIZER;
  pthread_cond_t cond = PTHREAD_COND_INITIALIZER;

  // This time has most definitely passed already. (Epoch)
  struct timespec now;
  memset(&now, 0, sizeof(now));

  pthread_mutex_lock(&mutex);
  pthread_cond_timedwait(&cond, &mutex, &now);
  pthread_mutex_unlock(&mutex);

  pthread_mutex_destroy(&mutex);
  pthread_cond_destroy(&cond);

  return 0;
}
