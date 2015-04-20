#define _GNU_SOURCE
#include <string.h>
#include <pthread.h>
#include <stdlib.h>
#include <stdio.h>
#include <unistd.h>
#include <sys/types.h>

// memrw provides a simulation of an application
// reading and writing memory, for the sake of tuning helgrind.
// It is a very simple (simplistic) model:
//  * only one thread
//  * only one exe context reading or writing the memory
//  * the working set of the application is unrealistically
//    concentrated on a consecutive nr of MB.
// At this moment, it was just used to tune the EvM data structure
// of helgrind.
// It would be nice to enhance this program to cope with a richer
// model e.g. multiple threads, many different stack traces touching
// the memory, better working set distribution, ...

static int nr_mb = 0; // total nr of mb used by the program
static int nr_mb_ws = 0; // nr_mb in program working set
static int nr_loops = 0; // nr of loops reading or writing the ws
static int nr_thr; // nr of threads (hardcoded to 1 currently)

// Note: the total nr of mb is what is explicitely allocated.
// On top of that, we have the stacks, local vars, lib vars, ...
// The working set is just the first nr_mb_ws of nr_mb.

static int verbose = 0;
static unsigned char **mb;

static void *memrw_fn(void *v)
{
   int loops, m, b;
   int write;
   int differs = 0;
   unsigned char prev = 0;

   for (loops = 0; loops < nr_loops; loops++) {
      // printf("loop %d write %d\n", loops, write);
      // Note: in case of multiple threads, we will have
      // to add lock/unlock somewhere in the below, maybe to lock
      // the MB we are reading or writing.
      for (m = 0; m < nr_mb_ws; m++) {
         for (b = 0; b < 1024 * 1024; b++) {
            write = b % 5 == 0;
            // Do some write or read operations.
            if (write) {
               if (mb[m][b] < 255)
                  mb[m][b] += differs;
               else
                  mb[m][b] = 0;
            } else {
               differs = mb[m][b] != prev;
               prev = mb[m][b];
            }
         }
      }
   }
   return NULL;
}

int main (int argc, char *argv[])
{
   int a;
   int ret;
   int i;
   pthread_t thr;

   // usage: memrw [-t nr_mb default 10] [-w nr_mb_ws default 10]
   //              [-l nr_loops_on_ws default 3]
   //              [-f fan_out default 0]
   //              [-v verbosity default 0]
   nr_mb = 10;
   nr_mb_ws = 10;
   nr_loops = 3;
   verbose = 0;
   for (a = 1; a < argc; a+=2) {
      if        (strcmp(argv[a], "-t") == 0) {
         nr_mb = atoi(argv[a+1]);
      } else if (strcmp(argv[a], "-w") == 0) {
         nr_mb_ws = atoi(argv[a+1]);
      } else if (strcmp(argv[a], "-l") == 0) {
         nr_loops = atoi(argv[a+1]);
      } else if (strcmp(argv[a], "-v") == 0) {
         verbose = atoi(argv[a+1]);
      } else {
         printf("unknown arg %s\n", argv[a]);
      }
   }
   if (nr_mb_ws > nr_mb)
      nr_mb_ws = nr_mb; // to make it easy to do loops combining values

   nr_thr = 1;

   printf ("total program memory -t %d MB"
           " working set -w %d MB"
           " working set R or W -l %d times"
           "\n",
           nr_mb,
           nr_mb_ws,
           nr_loops);

   printf ("creating and initialising the total program memory\n");
   mb = malloc(nr_mb * sizeof(char*));
   if (mb == NULL)
      perror("malloc mb");
   for (i = 0; i < nr_mb; i++) {
      mb[i] = calloc(1024*1024, 1);
      if (mb[i] == NULL)
         perror("malloc mb[i]");
   }

   printf("starting thread that will read or write the working set\n");
   ret = pthread_create(&thr, NULL, memrw_fn, &nr_thr);
   if (ret != 0)
      perror("pthread_create");
   printf("waiting for thread termination\n");

   ret = pthread_join(thr, NULL);
   if (ret != 0)
      perror("pthread_join");
   printf("thread terminated\n");

   return 0;
}
