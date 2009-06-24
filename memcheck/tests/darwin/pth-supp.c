// This requires a suppression (macos-Cond-6).  Bug 196528.

#include <pthread.h>

int main() {
   pthread_rwlock_t mutex;
   pthread_rwlock_init(&mutex, NULL);
   return 0;
}
