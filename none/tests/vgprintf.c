#include "valgrind.h"
#include <stdio.h>

int
main (int argc, char **argv)
{
   int x = 0;
   x += VALGRIND_PRINTF("Yo ");
   x += VALGRIND_PRINTF("Yo ");
   x += VALGRIND_PRINTF("Ma\n");
   fprintf(stderr, "%d\n", x);
   x  = VALGRIND_PRINTF_BACKTRACE("Backtrace line one\nLine two:\n");
   fprintf(stderr, "%d\n", x);
   return 0;
}
