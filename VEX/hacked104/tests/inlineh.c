
#include <stdio.h>

extern int burble ( int );

__inline__
static int inline_thisfile ( void )
{
   return burble(17);
}

#include "inlineh.h"

void main ( void )
{
   int a;
   a = 0;
   a += inline_thisfile();
   a *= 100;
   a += inline_otherfile();
   a /= 100;
   printf("answer is %d\n", a);
}
