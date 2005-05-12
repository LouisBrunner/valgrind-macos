#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include "../memcheck.h"

int main(void)
{
   int  x;
   int  y = 0;
   int* reachable;
   int* dubious;
   int* leaked;
   int  n_reachable  = 0;
   int  n_dubious    = 0;
   int  n_leaked     = 0;
   int  n_suppressed = 0;

   /* we require these longs to have same size as a machine word */
   assert(sizeof(long) == sizeof(void*));

   /* Error counting */
   printf("errors: %d\n", VALGRIND_COUNT_ERRORS);

   if (x == 0) {
      y++;
   } else {
      y--;
   }

   printf("errors: %d\n", VALGRIND_COUNT_ERRORS);

   /* Leak checking */
   VALGRIND_DO_LEAK_CHECK;
   VALGRIND_COUNT_LEAKS(n_leaked, n_dubious, n_reachable, n_suppressed);
   if (n_reachable == 24) n_reachable = 0; /* handle glibc differences */
   printf("leaks: %dB, %dB, %dB, %dB\n",
          n_leaked, n_dubious, n_reachable, n_suppressed);

   leaked = malloc(77);
   leaked = 0;

   dubious = malloc(88);
   dubious += 10;

   reachable = malloc(99);

   VALGRIND_DO_LEAK_CHECK;
   VALGRIND_DO_LEAK_CHECK;
   VALGRIND_COUNT_LEAKS(n_leaked, n_dubious, n_reachable, n_suppressed);
   if (n_reachable == 123) n_reachable = 99; /* handle glibc differences */
   printf("leaks: %dB, %dB, %dB, %dB\n",
          n_leaked, n_dubious, n_reachable, n_suppressed);

   printf("errors: %d\n", VALGRIND_COUNT_ERRORS);

   return 0;
}
