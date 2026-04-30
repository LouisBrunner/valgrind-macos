#include <fcntl.h>
#include <signal.h>
#include <sys/time.h>
#include <unistd.h>
#include <stdlib.h>
#include <assert.h>
#include <errno.h>

volatile int     ticks = 0;
struct itimerval timert;
struct sigaction timer_action;

void handle_alrm(int sig) { ticks++; }

int main(int argc, char* argv[])
{
   timer_action.sa_handler = handle_alrm;
   sigemptyset(&timer_action.sa_mask);
   timer_action.sa_flags = SA_RESTART;

   sigaction(SIGALRM, &timer_action, NULL);

   timert.it_interval.tv_sec = timert.it_value.tv_sec = 0;
   timert.it_interval.tv_usec = timert.it_value.tv_usec = 100;
   setitimer(ITIMER_REAL, &timert, NULL);
   
   struct timespec ts_initialized = {0, 1000000};
   int ret;

   ret = clock_nanosleep(CLOCK_MONOTONIC, 0, &ts_initialized,
                         NULL);
   assert(ret == EINTR);

   struct timespec* too_small = malloc(1);

   ret = clock_nanosleep(CLOCK_MONOTONIC, 0, &ts_initialized,
                         too_small);
   assert(ret == EINTR);

   ret = clock_nanosleep(CLOCK_MONOTONIC, TIMER_ABSTIME, NULL,
                         &ts_initialized);
   assert(ret == EFAULT);
}
