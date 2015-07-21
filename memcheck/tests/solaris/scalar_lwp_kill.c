/* Test for lwp_kill syscall which is available on illumos
   and Solaris 11 and 11.1. This syscall has been renamed
   on Solaris 11.2 to lwp_sigqueue and extra parameters added.
 */

#include "scalar.h"

int main(void)
{
   /* Uninitialised, but we know px[0] is 0x0. */
   long *px = malloc(sizeof(long));
   x0 = px[0];

   /* SYS_lwp_kill              163 */
   GO(SYS_lwp_kill, "2s 0m");
   SY(SYS_lwp_kill, x0 - 1, x0); FAIL;

   return 0;
}

