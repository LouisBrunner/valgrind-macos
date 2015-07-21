/* Test for frealpathat syscall which is available on Solaris 11.1. */ 

#include "scalar.h"

int main(void)
{
   /* Uninitialised, but we know px[0] is 0x0. */
   long *px = malloc(sizeof(long));
   x0 = px[0];

   /* SYS_frealpathat           30 */
   GO(SYS_frealpathat, "4s 2m");
   SY(SYS_frealpathat, x0 - 1, x0 + 1, x0 + 2, x0 + 3); FAIL;

   return 0;
}

