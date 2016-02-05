/* Test for getrandom syscall which is available on Solaris 11. */ 

#include "scalar.h"

int main(void)
{
   /* Uninitialised, but we know px[0] is 0x0. */
   long *px = malloc(sizeof(long));
   x0 = px[0];

   /* SYS_getrandom            143 */
   GO(SYS_getrandom, "(getrandom) 3s 1m");
   SY(SYS_getrandom, x0 + 1, x0 + 1, x0); FAIL;

   return 0;
}

