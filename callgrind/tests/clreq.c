
// Similar to Cachegrind, check if instrumentation works in the presence
// of a client request. Uses a Callgrind request to check this.

#include "../callgrind.h"

int some_work(int sum)
{
   int i;

   for(i=0;i<10;i++) sum += i; /* some dummy work */

   return sum;
}

int some_callgrind_clreqs()
{
   int sum = some_work(0);

   CALLGRIND_ZERO_STATS;

   sum += some_work(sum);

   CALLGRIND_DUMP_STATS;

   sum += some_work(sum);

   CALLGRIND_DUMP_STATS_AT("Please dump here");

   return some_work(sum);
}

int more_callgrind_clreqs()
{
   int sum = some_callgrind_clreqs();

   CALLGRIND_TOGGLE_COLLECT;

   sum += some_callgrind_clreqs();

   CALLGRIND_TOGGLE_COLLECT;

   return sum;
}

   
int main(void)
{
   more_callgrind_clreqs();

   CALLGRIND_STOP_INSTRUMENTATION;

   more_callgrind_clreqs();

   CALLGRIND_START_INSTRUMENTATION;

   more_callgrind_clreqs();

   CALLGRIND_STOP_INSTRUMENTATION;

   more_callgrind_clreqs();

   CALLGRIND_START_INSTRUMENTATION;

   return RUNNING_ON_VALGRIND;
}

