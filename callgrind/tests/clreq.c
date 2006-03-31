
// Similar to Cachegrind, check if instrumentation works in the presence
// of a client request. Uses a Callgrind request to check this.

#include "../callgrind.h"

int main(void)
{
   CALLGRIND_ZERO_STATS;

   return RUNNING_ON_VALGRIND;
}

