
#include <stdio.h>
#include <stdlib.h>

#include "valgrind.h"

int main1 ( void )
{
  int xxx, i;
  for (i = 0; i < 10; i++) VALGRIND_CHECK_DEFINED(xxx);
  return 0;
}

int main ( void )
{
   int i, sum, m;
   char* aa = calloc(100,1);
   sum = 0;

   VALGRIND_CHECK_READABLE(aa,100);

   m = VALGRIND_MAKE_WRITABLE( &aa[49], 1 );
   VALGRIND_CHECK_WRITABLE(aa,100);

   printf("m_na: returned value is %d\n", m );

   for (i = 0; i < 100; i++)
     sum += aa[i];
   printf("sum is %d\n", sum);

   m = VALGRIND_DISCARD(m);
   printf("m_rm: returned value is %d\n", m );

   for (i = 0; i < 100; i++)
     sum += aa[i];
   printf("sum is %d\n", sum);

   return 0;
}
