#include <string.h>
#include <pthread.h>
#include <stdlib.h>
#include <stdio.h>
#include <fcntl.h>
#include <unistd.h>
#include <sys/types.h>
#include <sys/syscall.h>
#include "../memcheck/memcheck.h"
int using_threads = 0; /* test collision with a global in gdbserver */
/* we will undefine one char on two */
static char undefined[10] = "undefined";

#define LOOPS 10000000
static int loopmain, loopt1, loopt2;

static double pi = 3.14159265358979323846264338327950288;

static pid_t gettid()
{
#ifdef __NT_gettid
   return syscall(__NR_gettid);
#else
   return getpid();
#endif
}
static void whoami(char *msg)
{
   printf("pid %d Thread %d %s\n", getpid(), gettid(), msg); fflush(stdout);
}

static int int_und;
static int sleeps = 15;
static void make_error (char *s)
{
  char *make_error_name __attribute__((unused)) = "make_error name";
  char c __attribute__((unused));
  double pi2 __attribute__((unused)) = 2.0 * pi;
  whoami(s);
  if (int_und == 0)
     printf ("%s int_und is zero %d\n", s, int_und);
  else
     printf ("%s int_und is not zero\n", s);
  fflush(stdout);
}

static void level ()
{
  char *level_name __attribute__((unused)) = "level name";
  make_error ("called from level");
}

static void loops (int *loopnr)
{
  int i, j;
  for (i = 0; i < LOOPS; i++)
    for (j = 0; j < LOOPS; j++)
      (*loopnr)++;
}
 
static void *brussels_fn(void *v)
{
  char *brussels_name __attribute__((unused)) = "Brussels";
  make_error ("called from Brussels");
  loopt1 = 1;
  while (! (loopt1 && loopt2 && loopmain))
    loopt1++;
  loops (&loopt1);
  return NULL;
}
static void *london_fn(void *v)
{
  char *london_name __attribute__((unused)) = "London";
  make_error ("called from London");
  loopt2 = 1;
  while (! (loopt1 && loopt2 && loopmain))
    loopt2++;
  loops (&loopt2);
  sleep(10);
  return NULL;
}
static void *petaouchnok_fn(void *v)
{
  char *petaouchnok_name __attribute__((unused)) = "Petaouchnok";
  struct timeval t;
  int i;
  for (i = 1; i <= sleeps; i++) {
      t.tv_sec = 5;
      t.tv_usec = 0;
      fprintf (stderr, "Petaouchnok sleep nr %d out of %d sleeping 5 seconds\n",
               i, sleeps);
      fflush(stderr);
      select (0, NULL, NULL, NULL, &t);
  }
  return NULL;
}
static void leaf(void) {}
static void breakme(int line)
{
   if (line > 1000)
      leaf(); // ensures not leaf, as ppc unwind implies VEX iropt precise exns
}
int main (int argc, char *argv[])
{
  char *main_name __attribute__((unused)) = "main name";
  pthread_t ebbr, egll, zzzz;
  int i = 1234;
  char undef = '?';
  char *some_mem __attribute__((unused)) = malloc(100);
  VALGRIND_MAKE_MEM_UNDEFINED(&undef, 1);
  int len = strlen(undefined);
  breakme(__LINE__); //break1
  for (i = len-1; i >= 0; i=i-2)
     undefined[i] = undef;
  *(char*)&int_und = undef;

  breakme(__LINE__); //break2

  if (argc > 1)
    sleeps = atoi(argv[1]);

  level();
  make_error ("called from main");

  pthread_create(&ebbr, NULL, brussels_fn, NULL);	
  pthread_create(&egll, NULL, london_fn, NULL);	
  pthread_create(&zzzz, NULL, petaouchnok_fn, NULL);	

  loopmain = 1;
  while (! (loopt1 && loopt2 && loopmain))
    loopmain++;
  for (i = 0; i < LOOPS; i++) {
     loopmain++;

     if (loopmain == 10000)
        make_error ("in main loop");
  }
  
  pthread_join(ebbr, NULL);

  make_error ("called from main (the end, before joining t3)");

  pthread_join(zzzz, NULL);

  if (argc > 2) {
     for (i = 0; i < 100; i++)
        if ((*(&undef + i*4000) == 0) || (*(&undef - i*4000) == 0)) {
           printf ("there are some null bytes here and there %d\n", i);
           fflush(stdout);
        }
  }
  exit(0);
}
