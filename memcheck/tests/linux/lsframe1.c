
/* Demonstrate Memcheck correctly handling a 64M array on the stack.
   Requires --max-stackframe=67108884 or above.  And since it
   generates a very large stack, --main-stacksize=67200000
   (approximately) is also required. */

#include <stdio.h>

#define N_MBYTES 64

#define N_INTS ((N_MBYTES * 1048576) / sizeof(int))


int main ( void )
{
   int i, sum;
   int arr[N_INTS];
   fprintf(stderr, "lsframe1: start\n");
   for (i = 0; i < N_INTS; i++)
      arr[i] = i;
   sum = 0;
   for (i = 0; i < N_INTS; i++)
      sum += arr[i];
   fprintf(stderr, "lsframe1: done, result is %d\n", sum);
   return 0;
}
