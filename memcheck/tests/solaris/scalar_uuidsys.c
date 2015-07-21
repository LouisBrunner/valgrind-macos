/* Test for uuidsys syscall which is available on newer Solaris. */ 

#include "scalar.h"

int main(void)
{
   /* Uninitialised, but we know px[0] is 0x0. */
   long *px = malloc(sizeof(long));
   x0 = px[0];

   /* SYS_uuidsys              124 */
   GO(SYS_uuidsys, "1s 1m");
   SY(SYS_uuidsys, x0 + 1); FAIL;

   return 0;
}

