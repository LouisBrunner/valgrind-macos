
// Prior to 3.0.1, Cachegrind was failing if run on a program that uses
// client requests.  It was fixed in 3.0.1, but then reintroduced
// afterwards (reported as bug #116057).  So here we test it.

#include "../../include/valgrind.h"

int main(void)
{
   return RUNNING_ON_VALGRIND;
}
