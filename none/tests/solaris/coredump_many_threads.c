/*
 * Creates 8 threads. The fifth one (counting the main thread as well)
 * causes a core dump.
 */

#include <errno.h>
#include <pthread.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

#define NEVERENDING_SLEEP 10000

static pthread_barrier_t barrier;

static void *thread_func2(void *arg) {
   pthread_barrier_wait(&barrier);
   sleep(NEVERENDING_SLEEP);
   return NULL;
}

static void *thread_func3(void *arg) {
   pthread_barrier_wait(&barrier);
   sleep(NEVERENDING_SLEEP);
   return NULL;
}

static void *thread_func4(void *arg) {
   pthread_barrier_wait(&barrier);
   sleep(NEVERENDING_SLEEP);
   return NULL;
}

static void *thread_func5(void *arg) {
   pthread_barrier_wait(&barrier);
   sleep(2);

   char *x = (char *) 0x1;
   *x = 2;
   return NULL;
}

static void *thread_func6(void *arg) {
   pthread_barrier_wait(&barrier);
   sleep(NEVERENDING_SLEEP);
   return NULL;
}

static void *thread_func7(void *arg) {
   pthread_barrier_wait(&barrier);
   sleep(NEVERENDING_SLEEP);
   return NULL;
}

static void *thread_func8(void *arg) {
   pthread_barrier_wait(&barrier);
   sleep(NEVERENDING_SLEEP);
   return NULL;
}

static void *thread_func9(void *arg) {
   pthread_barrier_wait(&barrier);
   sleep(NEVERENDING_SLEEP);
   return NULL;
}

static void create_thread(void *(*thread_func)(void *))
{
   pthread_t thread;

   int ret = pthread_create(&thread, NULL, thread_func, NULL);
   if (ret != 0) {
      fprintf(stderr, "pthread_create: %s (%d)\n", strerror(ret), ret);
      exit(1);
   }
}

int main(int argc, const char *argv[])
{
   int ret = pthread_barrier_init(&barrier, NULL, 9);
   if (ret != 0) {
      fprintf(stderr, "pthread_barrier_init: %s (%d)\n",
              strerror(ret), ret);
      exit(1);
   }

   create_thread(thread_func2);
   create_thread(thread_func3);
   create_thread(thread_func4);
   create_thread(thread_func5);
   create_thread(thread_func6);
   create_thread(thread_func7);
   create_thread(thread_func8);
   create_thread(thread_func9);
   pthread_barrier_wait(&barrier);

   sleep(NEVERENDING_SLEEP);
   return 0;
}
