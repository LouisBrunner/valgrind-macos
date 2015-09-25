/* Test for system_stats syscall which is available on newer Solaris. */ 

#include "scalar.h"
#include <sys/system_stats.h>

int main(void)
{
   /* Uninitialised, but we know px[0] is 0x0. */
   long *px = malloc(sizeof(long));
   x0 = px[0];

   /* SYS_system_stats         154 */
   GO(SYS_system_stats, "1s 0m");
   SY(SYS_system_stats, x0 + SYSTEM_STATS_START); SUCC;

   return 0;
}

