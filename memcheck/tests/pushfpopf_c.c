
#include <stdio.h>

// in pushfpopf.s
extern int fooble ( int, int );

int main ( void )
{
   int arr[2];
   arr[0] = 3;
   //   arr[1] = 45;
   printf("fooble: result is %d\n", fooble(arr[0], arr[1]));
   return 0;
}
