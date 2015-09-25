/* Test for gethrusec which depends on the correct emulation of
   AT_SUN_SYSSTAT_ADDR in the auxiliary vector. */

#include <stdio.h>
#include <strings.h>
#include <sys/system_stats.h>

int main(void)
{
   hrtime_t t = 0;
   get_hrusec(&t);
   printf("get_hrusec(): %s\n", (t == 0) ? "FAIL" : "PASS");

   memtime_sec_t m = 0;
   memset(&m, 0, sizeof(m));
   get_sec_fromepoch(&m);
   printf("get_sec_fromepoch(): %s\n", (m == 0) ? "FAIL" : "PASS");

   t = 0;
   get_nsec_fromepoch(&t);
   printf("get_nsec_fromepoch(): %s\n", (t == 0) ? "FAIL" : "PASS");
   return 0;
}

