#include <stdlib.h>
#include <assert.h>
#include <errno.h>
#include "../../memcheck.h"

int main(void)
{
   int *pi = NULL;
   VALGRIND_DO_LEAK_CHECK;
   pi = reallocf(pi, 10*sizeof(int));
   VALGRIND_DO_ADDED_LEAK_CHECK;
   pi = reallocf(pi, 0);
   free(pi);
   VALGRIND_DO_CHANGED_LEAK_CHECK;
   pi = NULL;
   pi = realloc(pi, 10*sizeof(int));
   VALGRIND_DO_ADDED_LEAK_CHECK;
   errno = 0;
   pi = reallocf(pi, 1UL << 49);
   assert(!pi);
   assert(errno == ENOMEM);
   VALGRIND_DO_CHANGED_LEAK_CHECK;
}
