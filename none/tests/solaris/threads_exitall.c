/* Test that all threads are killed when exit() is called. */

#include <pthread.h>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>

void *thread_proc(void *arg)
{
   /* Wait for main thread to block. */
   sleep(2);

   /* Exit the program. */
   exit(0);

   return NULL;
}

int main(void)
{
   pthread_t thread;
   void *status;

   if (pthread_create(&thread, NULL, thread_proc, NULL)) {
      perror("pthread_create");
      return 1;
   }

   if (pthread_join(thread, &status)) {
      perror("pthread_join");
      return 1;
   }

   /* This code should not be reached. */
   fprintf(stderr, "Thread joined\n");

   return 0;
}

