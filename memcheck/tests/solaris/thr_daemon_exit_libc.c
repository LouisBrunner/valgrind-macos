/* Creates several daemon threads and non-daemon threads.
   Tests that the process can exit even if the daemon threads are still running,
   as per thr_create(3C). */

#include <pthread.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <thread.h>
#include <unistd.h>

#define DAEMON_THREADS 5
#define NON_DAEMON_THREADS 6
#define SLEEP_100_MS usleep(100 * 1000)

static pthread_barrier_t barrier;

void *daemon_thread_func(void *arg) {
   size_t index = (size_t) arg;
   printf("DAEMON thread #%zu running\n", index); fflush(stdout);
   pthread_barrier_wait(&barrier);

   /* Give the non-daemon threads enough time to exit. */
   sleep(10);
   printf("DAEMON thread #%zu still running?!\n", index); fflush(stdout);
   return NULL;
}

void *normal_thread_func(void *arg) {
   size_t index = (size_t) arg;
   printf("non-daemon thread #%zu running\n", index); fflush(stdout);
   pthread_barrier_wait(&barrier);

   sleep(2);
   return NULL;
}

int main(void) {
   size_t i;
   int ret = pthread_barrier_init(&barrier, NULL,
                                  DAEMON_THREADS + NON_DAEMON_THREADS + 1);
   if (ret != 0) {
      fprintf(stderr, "pthread_barrier_init failed: %s\n", strerror(ret));
      return 1;
   }

   for (i = 0; i < DAEMON_THREADS; i++) {
      ret = thr_create(NULL, 0, daemon_thread_func, (void *) i,
                       THR_DAEMON, NULL);
      if (ret != 0) {
         fprintf(stderr, "thr_create failed: %s\n", strerror(ret));
         return 1;
      }
      SLEEP_100_MS;
   }

   for (i = 0; i < NON_DAEMON_THREADS; i++) {
      ret = thr_create(NULL, 0, normal_thread_func, (void *) i, 0, NULL);
      if (ret != 0) {
         fprintf(stderr, "thr_create failed: %s\n", strerror(ret));
         return 1;
      }
      SLEEP_100_MS;
   }

   pthread_barrier_wait(&barrier);

   printf("MAIN thread exiting\n");
   /* Exit only the main thread, not whole process.
      That is, do not exit(0) or return(0). */
   thr_exit(NULL);
}
