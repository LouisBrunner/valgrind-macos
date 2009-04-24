#include <stdlib.h>

// All sizes are divisible by 16 -- no slop.

int* ignore1(void)
{
   // Allocating/freeing in an ignored function: ignored.
   int* ignored_x1 = malloc(400);
   int* ignored_x2 = malloc(400);
   free(ignored_x2);
   return ignored_x1;
}

void ignore2(int* x, int* ignored_x)
{
   // Growing/shrinking a non-ignored block in an ignored function: ignored.
   x = realloc(x, 800);   
   x = realloc(x, 400);   

   // Growing/shrinking an ignored block in an ignored function: ignored.
   ignored_x = realloc(ignored_x, 800);   
   ignored_x = realloc(ignored_x, 400);   
}

int main(void)
{
   int* x;
   int* ignored_x;

   // Not ignored.
   x = malloc(400);

   // Get an ignored block.
   ignored_x = ignore1();

   // Growing/shrinking a non-ignored block in a non-ignored function:
   // not ignored.
   x = realloc(x, 800);
   x = realloc(x, 400);

   // Growing/shrinking an ignored block in a non-ignored function: ignored.
   ignored_x = realloc(ignored_x, 800);
   ignored_x = realloc(ignored_x, 400);

   ignore2(x, ignored_x);

   x = realloc(ignored_x, 0);    // equivalent to 'free(ignored_x)'.

   return 0;
}

