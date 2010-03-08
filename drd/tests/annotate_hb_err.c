/* Test program that triggers several happens-before usage errors. */


#include <stdlib.h>
#include <stdio.h>
#include <pthread.h>
#include "unified_annotations.h"


int main(int argc, char** argv)
{
  pthread_mutex_t m;
  pthread_cond_t  cv;
  int i[64];

  pthread_mutex_init(&m, NULL);
  pthread_cond_init(&cv, NULL);

  /* happens-after without preceding happens-before. */
  ANNOTATE_HAPPENS_AFTER(&i);

  /* happens-after on a mutex. */
  ANNOTATE_HAPPENS_BEFORE(&m);

  /* happens-after on a condition variable. */
  ANNOTATE_HAPPENS_BEFORE(&cv);

  /* condition variable operation on a h.b. annotated object. */
  ANNOTATE_HAPPENS_BEFORE(&i);
  pthread_cond_init((pthread_cond_t*)&i, NULL);

  /* The sequence below is fine. */
  ANNOTATE_NEW_MEMORY(&i, sizeof(i));
  ANNOTATE_HAPPENS_BEFORE(&i);
  ANNOTATE_HAPPENS_AFTER(&i);
  ANNOTATE_NEW_MEMORY(&i, sizeof(i));
  ANNOTATE_HAPPENS_BEFORE(&i);
  ANNOTATE_NEW_MEMORY(&i, sizeof(i));

  /* happens-before after happens-after. */
  ANNOTATE_HAPPENS_BEFORE(&i);
  ANNOTATE_HAPPENS_AFTER(&i);
  ANNOTATE_HAPPENS_BEFORE(&i);

  fprintf(stderr, "Done.\n");
  return 0;
}

/*
 * Local variables:
 * c-basic-offset: 2
 * End:
 */
