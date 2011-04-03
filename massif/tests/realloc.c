#include <stdlib.h>

int main(void)
{                                // All sizes are multiples of 16 -- no slop.
   int* x = realloc(NULL, 800);  // equivalent to malloc(800), and ends up
   int* y __attribute__((unused)); // calling Valgrind's (and Massif's) malloc

   x = realloc(x, 800);          // same size

   x = realloc(x, 400);          // smaller

   x = realloc(x, 1200);         // bigger

   y = realloc(x+10, 1600);      // bogus realloc

   x = realloc(x, 0);            // equivalent to free(x), and ends up
                                 // calling Valgrind's (and Massif's) free
   return 0;
}
