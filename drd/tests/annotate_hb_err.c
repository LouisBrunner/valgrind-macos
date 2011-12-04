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
  U_ANNOTATE_HAPPENS_AFTER(&i);

  /* happens-after on a mutex. */
  U_ANNOTATE_HAPPENS_BEFORE(&m);

  /* happens-after on a condition variable. */
  U_ANNOTATE_HAPPENS_BEFORE(&cv);

  /* condition variable operation on a h.b. annotated object. */
  U_ANNOTATE_HAPPENS_BEFORE(&i);
  pthread_cond_init((pthread_cond_t*)&i, NULL);

  /* The sequence below is fine. */
  U_ANNOTATE_NEW_MEMORY(&i, sizeof(i));
  U_ANNOTATE_HAPPENS_BEFORE(&i);
  U_ANNOTATE_HAPPENS_AFTER(&i);
  U_ANNOTATE_NEW_MEMORY(&i, sizeof(i));
  U_ANNOTATE_HAPPENS_BEFORE(&i);
  U_ANNOTATE_NEW_MEMORY(&i, sizeof(i));

  /* happens-before after happens-after. */
  U_ANNOTATE_HAPPENS_BEFORE(&i);
  U_ANNOTATE_HAPPENS_AFTER(&i);
  U_ANNOTATE_HAPPENS_BEFORE(&i);

  fprintf(stderr, "Done.\n");
  return 0;
}
