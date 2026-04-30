#include <stdlib.h>
#include <assert.h>
#include <errno.h>
#include <stdint.h>
#include <stdio.h>
#include "../../memcheck.h"

int main(void)
{
   int *pi = NULL;
   int *newpi;
   VALGRIND_DO_LEAK_CHECK;
   /* should succeed */
   pi = reallocarray(pi, 10U, sizeof(int));
   assert(pi);
   fprintf(stderr, "Added leak check, expect 40 bytes still reachable\n");
   VALGRIND_DO_ADDED_LEAK_CHECK;
   /* on FreeBSD this does not free pi */
   pi = reallocarray(pi, 0U, sizeof(int));
   assert(pi);
   fprintf(stderr, "Added leak check, expect 1 byte still reachable\n");
   VALGRIND_DO_ADDED_LEAK_CHECK;
   pi = reallocarray(pi, 10U, 0U);
   assert(pi);
   free(pi);
   fprintf(stderr, "Added leak check, expect all blocks freed\n");
   VALGRIND_DO_CHANGED_LEAK_CHECK;
   pi = NULL;
   pi = reallocarray(pi, 10U, sizeof(int));
   assert(pi);
   fprintf(stderr, "Added leak check, expect 40 bytes still reachable\n");
   VALGRIND_DO_ADDED_LEAK_CHECK;
   errno = 0;
   /* this will fail in the malloc lib, not enough memory */
   newpi = reallocarray(pi, 1UL << 49, 1U);
   assert(!newpi);
   assert(errno == ENOMEM);
   fprintf(stderr, "Added leak check, expect 40 bytes still reachable\n");
   VALGRIND_DO_CHANGED_LEAK_CHECK;
   /* this will fail the reallocarray precondition */
   newpi = reallocarray(pi, SIZE_MAX/1000U, SIZE_MAX/1000U);
   assert(!newpi);
   assert(errno == ENOMEM);
   free(pi);
   fprintf(stderr, "Added leak check, expect all blocks freed\n");
   VALGRIND_DO_CHANGED_LEAK_CHECK;
}
