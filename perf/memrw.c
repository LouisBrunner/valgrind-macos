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

static int sz_b; // size of a block
static int nr_b; // total nr of blocks used by the program
static int nr_b_ws; // nr_b in program working set
static int nr_loops; // nr of loops reading or writing the ws
static int nr_thr; // nr of threads (hardcoded to 1 currently)
static int nr_repeat; // nr of times we will allocate, use, then free total+ws

// Note: the total nr of MB is what is explicitly allocated.
// On top of that, we have the stacks, local vars, lib vars, ...
// The working set is just the first nr_b_ws blocks of nr_b.

static int verbose = 0;
static unsigned char **t_b; // Pointers to all blocks

static void *memrw_fn(void *v)
{
   int loops, m, b;
   int dowrite;
   int differs = 0;
   unsigned char prev = 0;

   for (loops = 0; loops < nr_loops; loops++) {
      // printf("loop %d dowrite %d\n", loops, dowrite);
      // Note: in case of multiple threads, we will have
      // to add lock/unlock somewhere in the below, maybe to lock
      // the MB we are reading or writing.
      for (m = 0; m < nr_b_ws; m++) {
         for (b = 0; b < sz_b; b++) {
            dowrite = b % 5 == 0;
            // Do some write or read operations.
            if (dowrite) {
               if (t_b[m][b] < 255)
                  t_b[m][b] += differs;
               else
                  t_b[m][b] = 0;
            } else {
               differs = t_b[m][b] != prev;
               prev = t_b[m][b];
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
   int r;
   pthread_t thr;

   // usage: memrw [-b blocksize default 1MB ]
   //              [-t nr_b default 10] [-w nr_b_ws default 10]
   //              [-l nr_loops_on_ws default 3]
   //              [-r nr_repeat default 1]
   //              [-f fan_out default 0]
   //              [-v verbosity default 0]
   sz_b = 1024 * 1024;
   nr_b = 10;
   nr_b_ws = 10;
   nr_loops = 3;
   nr_repeat = 1;
   verbose = 0;
   for (a = 1; a < argc; a+=2) {
      if        (strcmp(argv[a], "-b") == 0) {
         sz_b = atoi(argv[a+1]);
      } else if (strcmp(argv[a], "-t") == 0) {
         nr_b = atoi(argv[a+1]);
      } else if (strcmp(argv[a], "-w") == 0) {
         nr_b_ws = atoi(argv[a+1]);
      } else if (strcmp(argv[a], "-l") == 0) {
         nr_loops = atoi(argv[a+1]);
      } else if (strcmp(argv[a], "-r") == 0) {
         nr_repeat = atoi(argv[a+1]);
      } else if (strcmp(argv[a], "-v") == 0) {
         verbose = atoi(argv[a+1]);
      } else {
         printf("unknown arg %s\n", argv[a]);
      }
   }
   if (nr_b_ws > nr_b)
      nr_b_ws = nr_b; // to make it easy to do loops combining values

   nr_thr = 1;

   printf ("total program memory -t %llu MB"
           " working set -w %llu MB\n",
           ((unsigned long long)nr_b * sz_b) 
             / (unsigned long long) (1024*1024),
           ((unsigned long long)nr_b_ws * sz_b) 
             / (unsigned long long)(1024*1024));
   printf (" working set R or W -l %d times"
           " repeat the whole stuff -r %d times\n",
           nr_loops,
           nr_repeat);

   for (r = 0; r < nr_repeat; r++) {
      printf ("creating and initialising the total program memory\n");
      t_b = malloc(nr_b * sizeof(char*));
      if (t_b == NULL)
         perror("malloc t_b");
      for (i = 0; i < nr_b; i++) {
         t_b[i] = calloc(sz_b, 1);
         if (t_b[i] == NULL)
            perror("malloc t_b[i]");
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

      /* Now, free the memory used, for the next repeat */
      for (i = 0; i < nr_b; i++)
         free (t_b[i]);
      free (t_b);
      printf("memory freed\n");
   }

   return 0;
}
