
#include <stdio.h>
#include <stdlib.h>

#include "../memcheck.h"

int main1 ( void )
{
  int xxx, i;
  for (i = 0; i < 10; i++) (void) VALGRIND_CHECK_VALUE_IS_DEFINED(xxx);
  return 0;
}

int main ( void )
{
   int i, sum, m;
   char* aa = calloc(100,1);
   sum = 0;

   (void) VALGRIND_CHECK_MEM_IS_DEFINED(aa,100);

   m = VALGRIND_MAKE_MEM_UNDEFINED( &aa[49], 1 );
   (void) VALGRIND_CHECK_MEM_IS_ADDRESSABLE(aa,100);

   printf("m_na: returned value is %d\n", m );

   for (i = 0; i < 100; i++)
     sum += aa[i];
   if (sum > 0)
      printf("sum is positive\n");
   else
      printf("sum is non-positive\n");

   m = VALGRIND_DISCARD(m);
   printf("m_rm: returned value is %d\n", m );

   for (i = 0; i < 100; i++)
     sum += aa[i];
   if (sum > 0)
      printf("sum is positive\n");
   else
      printf("sum is non-positive\n");

   return 0;
}
