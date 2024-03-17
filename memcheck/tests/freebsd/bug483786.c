#include "../../memcheck.h"
#include <time.h>

/* should complain about rqtp and rmtp */
void valgrind_should_complain(void)
{
   struct timespec ts_uninitialized = {0};

   VALGRIND_MAKE_MEM_UNDEFINED(&ts_uninitialized, sizeof(ts_uninitialized));

   clock_nanosleep(CLOCK_MONOTONIC, 0, &ts_uninitialized, &ts_uninitialized);
}

/* should have no complaints */
void valgrind_should_not_complain(void)
{
   struct timespec ts_initialized = {0};

   clock_nanosleep(CLOCK_MONOTONIC, 0, &ts_initialized, &ts_initialized);
}

/* should have no complaints */
void valgrind_should_not_complain2(void)
{
   struct timespec ts_initialized = {0};

   clock_nanosleep(CLOCK_MONOTONIC, TIMER_ABSTIME, &ts_initialized,
                   &ts_initialized);
}

int main(int argc, char** argv)
{

   valgrind_should_complain();
   valgrind_should_not_complain();
   valgrind_should_not_complain2();

   return (0);
}
