#include <stdio.h>
#include <pthread.h>
#include <stdlib.h>
#define MANY 1000
#define LEVEL 100
typedef struct {
   pthread_mutex_t m[MANY];
   pthread_mutex_t d;
} Level;

static Level level[LEVEL];

static int stat_mutex_init = 0;
static int stat_mutex_lock = 0;
static int stat_mutex_unlock = 0;
static int stat_mutex_destroy = 0;

/* t2t.c : test program for the laog data structure performance testing
   and "shaking" : it creates, locks/unlocks and destroy mutex.

   USAGE:  t2t [many] [level] [loops]
   many (default 100) : how many locks are created/locked/unlocked at a certain level.
   level (default 1)  : how many levels of "nested locks" are done
   loops : how many times these locks are created and destroyed and locked/unlocked) */
#define check if (ret != 0) printf("error %d at line %d\n", ret, __LINE__)
int doit(int argc, char*argv[])
{
   int l, i;
   int ret;

   int clo_many = 100;
   int clo_level = 1;

   if (argc >= 2) clo_many = atoi(argv[1]);
   if (argc >= 3) clo_level = atoi(argv[2]);

   if (clo_many > MANY) {
      printf("error argv[1] (many arg) %d > max MANY %d\n", clo_many, MANY);
      exit(1);
   }

   if (clo_level > LEVEL) {
      printf("error argv[2] (level arg) %d > max LEVEL %d\n", clo_level, LEVEL);
      exit(1);
   }

   printf ("many %d level %d total_locks: %d\n", 
           clo_many, clo_level,
           clo_many * clo_level + clo_level * (clo_level == 1 ? 0 : 1));
  
   for (l = 0; l < clo_level; l++) {
      printf ("init level %d\n", l);
      for (i = 0; i < clo_many; i++) {
         ret = pthread_mutex_init (&level[l].m[i], NULL);
         check; 
         stat_mutex_init++;
      }
      if (clo_level > 1) {
         ret = pthread_mutex_init (&level[l].d, NULL);
         check;
         stat_mutex_init++;
      }
   }

   for (l = 0; l < clo_level; l++) {
      printf ("locking level %d\n", l);
      for (i = 0; i < clo_many; i++) {
         ret = pthread_mutex_lock (&level[l].m[i]);
         check;
         stat_mutex_lock++;
      }
      if (clo_level > 1) {
         ret = pthread_mutex_lock (&level[l].d);
         check;
         stat_mutex_lock++;
      }
   }

   for (l = 0; l < clo_level; l++) {
      printf ("unlocking level %d\n", l);
      for (i = 0; i < clo_many; i++) {
         ret = pthread_mutex_unlock (&level[l].m[i]);
         check;
         stat_mutex_unlock++;
      }
      if (clo_level > 1) {
         ret = pthread_mutex_unlock (&level[l].d);
         stat_mutex_unlock++;
         check;
      }
   }

   for (l = 0; l < clo_level; l++) {
      printf ("deleting level %d\n", l);
      if (clo_level > 1) {
         ret = pthread_mutex_destroy (&level[l].d);
         /// this tests the influence of the deletion in another order.
         check;
         stat_mutex_destroy++;
      }
      for (i = 0; i < clo_many; i++) {
         ret = pthread_mutex_destroy (&level[l].m[i]);
         check;
         stat_mutex_destroy++;
      }
   }
   return 0;
}

int main(int argc, char*argv[])
{
   int loops = 1;
   int i;
   if (argc >= 4) loops = atoi(argv[3]);
   
   printf ("loops %d\n", loops);
   for (i = 0; i < loops; i++)
      doit(argc, argv);

   printf ("stats: init %d lock %d unlock %d destroy %d\n",
           stat_mutex_init, stat_mutex_lock, 
           stat_mutex_unlock, stat_mutex_destroy);
   return 0;
}

