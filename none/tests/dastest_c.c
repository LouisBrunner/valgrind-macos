
#include <stdio.h>

// dastest.s
extern int dastest ( int );

int main ( void )
{
   int x = 49;
   printf("dastest: x = %d\n", x);
   printf("dastest: das(x) = %d\n", dastest(x));
   return 0;
}
