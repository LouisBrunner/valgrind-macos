#include <stdlib.h>

int main(void)
{
   int* x = realloc(NULL, 100);  // equivalent to malloc(100), and ends up
                                 // calling Valgrind's (and Massif's) malloc

   x = realloc(x, 100);          // same size

   x = realloc(x, 50);           // smaller

   x = realloc(x, 150);          // bigger

   realloc(x+10, 200);           // bogus realloc

   x = realloc(x, 0);            // equivalent to free(x), and ends up
                                 // calling Valgrind's (and Massif's) free
   return 0;
}
