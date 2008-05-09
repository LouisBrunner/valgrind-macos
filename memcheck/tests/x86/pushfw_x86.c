
/* Test case for http://bugs.kde.org/show_bug.cgi?id=157748 */

#include <stdio.h>

int arr[3];

int main ( void )
{
   /* eax points at arr[0] */
   __asm__ __volatile__(
      "movl %%esp,0(%%eax)\n\t"
      "pushfw\n\t"
      "movl %%esp,4(%%eax)\n\t"
      "popfw\n\t"
      "movl %%esp,8(%%eax)\n"
      : /*out*/ : /*in*/ "a"(&arr) : /*trash*/ "memory","cc"
   );

   printf("%x %x %x\n", arr[0]-arr[0], arr[0]-arr[1], arr[0]-arr[2]);
   return 0;
}
