
#include <signal.h>
#include <sys/time.h>
#include <stdio.h>
#include <assert.h>

void hdp_tick ( int sigNo )
{
   int j;
   printf("tick "); fflush(stdout);
   for (j = 0; j < 10 * 5000; j++) ;
   printf("tock\n");
}

void hdp_init_profiling ( void )
{
   struct itimerval value;
   int ret;

   value.it_interval.tv_sec  = 0;
   value.it_interval.tv_usec = 50 * 1000;
   value.it_value = value.it_interval;

   signal(SIGPROF, hdp_tick);
   ret = setitimer(ITIMER_PROF, &value, NULL);
   assert(ret == 0);
}

int main ( void )
{
   hdp_init_profiling();
   while (1) {}
   return 0;
}
