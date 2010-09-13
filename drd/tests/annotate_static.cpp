// Test for ANNOTATE_BENIGN_RACE_STATIC() and ANNOTATE_UNPROTECTED_READ().


#include <pthread.h> /* pthread_create() */
#include <stdio.h>   /* fprintf() */
#include "../../drd/drd.h"


/* Local variables. */

static int s_i;
static volatile int s_j;

ANNOTATE_BENIGN_RACE_STATIC(s_i, "Benign because duplicate assignment.");


/* Local functions. */

static inline void AnnotateIgnoreReadsBegin() { ANNOTATE_IGNORE_READS_BEGIN(); }
static inline void AnnotateIgnoreReadsEnd() { ANNOTATE_IGNORE_READS_END(); }

static void* thread_func(void*)
{
#if defined(__powerpc__) && __GNUC__ -0 == 4 && __GNUC_MINOR__ -0 == 3 \
    && __GNUC_PATCHLEVEL__ -0 == 0
  AnnotateIgnoreReadsBegin();
  int i = s_j;
  AnnotateIgnoreReadsEnd();
  s_i = i;
#else
  s_i = ANNOTATE_UNPROTECTED_READ(s_j);
#endif
  return 0;
}

int main(int argc, char** argv)
{
  pthread_t tid;

  pthread_create(&tid, 0, thread_func, NULL);
  s_j++;
  s_i = s_j;
  pthread_join(tid, NULL);

  fprintf(stderr, "Done.\n");

  return 0;
}
