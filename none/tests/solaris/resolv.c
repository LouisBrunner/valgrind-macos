/* Tests initialization of the resolver library.
 * Adapted from none/tests/resolv for Solaris.
 */

#include <resolv.h>
#include <stdio.h>
#include <strings.h>

int main(int argc, char *argv[])
{
   struct __res_state stats;
   bzero(&stats, sizeof(stats));

   printf("PRE stats->nscount = %d\n", stats.nscount );
   fflush(stdout);
   res_ninit(&stats);
   printf("POST stats->nscount = %d\n", ( int ) stats.nscount > 0 );
   fflush(stdout);
   return 0;
}
