#include <stdlib.h>
#include <assert.h>
#include <errno.h>
#include <stdint.h>
#include "../../memcheck.h"

int main(void)
{
   int *pi = NULL;
   VALGRIND_DO_LEAK_CHECK;
   pi = reallocarray(pi, 10U, sizeof(int));
   VALGRIND_DO_ADDED_LEAK_CHECK;
   pi = reallocarray(pi, 0U, sizeof(int));
   VALGRIND_DO_ADDED_LEAK_CHECK;
   pi = reallocarray(pi, 10U, 0U);
   free(pi);
   VALGRIND_DO_CHANGED_LEAK_CHECK;
   pi = NULL;
   pi = reallocarray(pi, 10U, sizeof(int));
   VALGRIND_DO_ADDED_LEAK_CHECK;
   VALGRIND_DO_CHANGED_LEAK_CHECK;
   pi = reallocarray(pi, SIZE_MAX/1000U, SIZE_MAX/1000U);
   assert(!pi);
   assert(errno == ENOMEM);
   VALGRIND_DO_CHANGED_LEAK_CHECK;
}
