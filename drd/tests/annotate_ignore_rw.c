/* Test program for the annotations that suppress both reads and writes. */

#include <assert.h>  /* assert() */
#include <pthread.h>
#include <stdint.h>
#include <stdio.h>   /* EOF */
#include <unistd.h>  /* getopt() */
#include "../../drd/drd.h"

static int8_t s_a;
static int8_t s_b;
static int8_t s_c;

static void* thread_func(void* arg)
{
  /* Read s_a and modify s_b. */
  s_b = s_a;
  /* Modify s_c. */
  s_c = 1;

  return NULL;
}

int main(int argc, char** argv)
{
  const struct timespec delay = { 0, 100 * 1000 * 1000 };
  int optchar;
  int ign_rw = 1;
  pthread_t tid;

  while ((optchar = getopt(argc, argv, "r")) != EOF)
  {
    switch (optchar)
    {
    case 'r':
      ign_rw = 0;
      break;
    default:
      assert(0);
    }
  }

  pthread_create(&tid, 0, thread_func, 0);

  nanosleep(&delay, 0);

  if (ign_rw)
    ANNOTATE_IGNORE_READS_AND_WRITES_BEGIN();
  /* Read s_b and modify s_a. */
  s_a = s_b;
  if (ign_rw)
    ANNOTATE_IGNORE_READS_AND_WRITES_END();

  /*
   * Insert a delay here in order to make sure the load of s_c happens
   * after s_c has been modified.
   */
  sleep(1);

  /* Read s_c. */
  fprintf(stderr, "%s", "x" + s_c);

  pthread_join(tid, 0);

  fprintf(stderr, "Finished.\n");

  return 0;
}
