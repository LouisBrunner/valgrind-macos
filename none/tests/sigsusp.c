#include <stdlib.h>
#include <pthread.h>
#include <unistd.h>
#include <stdio.h>
#include <signal.h>
static void* t_fn(void* v)
{
   sigset_t mask;

   sigfillset(&mask);
   sigsuspend(&mask);
   return NULL;
}

int main (int argc, char *argv[])
{
  pthread_t t1;

  pthread_create(&t1, NULL, t_fn, NULL);

  sleep(1); // Should be enough to have the thread in sigsuspend
  // printf("dying\n");
  exit(0);
}
