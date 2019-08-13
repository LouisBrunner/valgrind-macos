#include <signal.h>
#include <unistd.h>
#include <pthread.h>
#include <stdlib.h>
#include <stdio.h>

/* This reproduces bugs 409141 and 409367.
   valgrind ./pth_self_kill 9    was hanging.

   valgrind ./pth_self_kill 15 killotherthread  was looping.
*/

pthread_t parent_thr;

void *t(void *p)
{
   sleep (200);
   printf ("error: supposed to die without printing this\n");
   exit (5);
   return NULL;
}

void handler_15(int signum)
{
   pthread_join(parent_thr, NULL);
   exit(2);
}

int main(int argc, char **argv)
{
   pthread_t thr;

   parent_thr = pthread_self();

   struct sigaction sa_old;
   struct sigaction sa_new;

   sigaction(15, NULL, &sa_old);
   sa_new.sa_mask = sa_old.sa_mask;
   sa_new.sa_flags = sa_old.sa_flags;
   sa_new.sa_handler = &handler_15;
   sigaction(15, &sa_new, NULL);


   if (argc <= 1)
   {
      printf
         ("usage: pth_self_kill SIGNALNR [killotherthread] [sleepafterkill]\n");
      exit (1);
   }

   int s = atoi(argv[1]);

   pthread_create(&thr, NULL, t, NULL);
   sleep (1);
   if (argc > 2)
   {
      pthread_kill(thr, s);
      if (argc > 3)
         sleep (2);
   }
   else
      raise(s);
   sigaction(15, &sa_old, NULL);
}
