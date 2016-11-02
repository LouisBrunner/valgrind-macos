/* Basic syscall test for Solaris/amd64 specific syscalls. */

#include "scalar.h"

#include <sys/lwp.h>

int main(void)
{
   /* Uninitialised, but we know px[0] is 0x0. */
   long *px = malloc(sizeof(long));
   x0 = px[0];

   /* SYS_lwp_private           166 */
   GO(SYS_lwp_private, "3s 1m");
   SY(SYS_lwp_private, x0 + _LWP_GETPRIVATE, x0 + _LWP_FSBASE, x0); FAIL;

   return 0;
}
