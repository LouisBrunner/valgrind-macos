#include <signal.h>
#include <unistd.h>
#include <pthread.h>
#include <stdlib.h>
#include <stdio.h>

/* This reproduces bugs 409141 and 409367.
   valgrind ./pth_self_kill 9    was hanging.

   valgrind ./pth_self_kill 15 killotherthread  was looping.
*/

void *t(void *p)
{
   sleep (200);
   printf ("error: supposed to die without printing this\n");
   exit (5);
   return NULL;
}

int main(int argc, char **argv)
{
   pthread_t thr;

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
}
