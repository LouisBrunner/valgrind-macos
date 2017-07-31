/* Scalar test for commands A_GETSTAT and A_SETSTAT for auditon(2) subcode
   of the auditsys() syscall. Available on Solaris 11.3 and illumos,
   removed in Solaris 11.4. */

#include "scalar.h"

#include <bsm/audit.h>

__attribute__((noinline))
static void sys_auditsys(void)
{
   GO(SYS_auditsys, "(BSM_AUDITCTL,A_GETSTAT) 3s 1m");
   SY(SYS_auditsys, x0 + BSM_AUDITCTL, x0 + A_GETSTAT, x0); FAIL;
}

__attribute__((noinline))
static void sys_auditsys2(void)
{
   GO(SYS_auditsys, "(BSM_AUDITCTL,A_SETSTAT) 3s 1m");
   SY(SYS_auditsys, x0 + BSM_AUDITCTL, x0 + A_SETSTAT, x0); FAIL;
}

int main(void)
{
   /* Uninitialised, but we know px[0] is 0x0. */
   long *px = malloc(sizeof(long));
   x0 = px[0];

   /* SYS_auditsys              186 */
   sys_auditsys();
   sys_auditsys2();

   return 0;
}

