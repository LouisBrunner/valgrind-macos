/* Test for lwp_sigqueue syscall which is available since
   Solaris 11.2.
 */

#include "scalar.h"

int main(void)
{
   /* Uninitialised, but we know px[0] is 0x0. */
   long *px = malloc(sizeof(long));
   x0 = px[0];

   /* SYS_lwp_sigqueue          163 */
   GO(SYS_lwp_sigqueue, "5s 1m");
   SY(SYS_lwp_sigqueue, x0 - 1, x0, x0 + 1, x0, x0 - 1); FAIL;

   return 0;
}

