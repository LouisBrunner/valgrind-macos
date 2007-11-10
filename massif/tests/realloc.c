#include <stdlib.h>

int main(void)
{
   int* x = realloc(NULL, 400);  // equivalent to malloc(400), and ends up
   int* y;                       // calling Valgrind's (and Massif's) malloc

   x = realloc(x, 400);          // same size

   x = realloc(x, 200);          // smaller

   x = realloc(x, 600);          // bigger

   y = realloc(x+10, 800);       // bogus realloc

   x = realloc(x, 0);            // equivalent to free(x), and ends up
                                 // calling Valgrind's (and Massif's) free
   return 0;
}
