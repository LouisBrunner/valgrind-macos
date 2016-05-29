/* Scalar test for new modctl syscall commands available on newer Solaris. */

#include "scalar.h"
#include "../../../config.h"

#include <sys/modctl.h>
#if defined(HAVE_SYS_SYSNVL_H)
#include <sys/sysnvl.h>
#endif /* HAVE_SYS_SYSNVL_H */

__attribute__((noinline))
static void sys_modctl(void)
{
   GO(SYS_modctl, "(MODNVL_DEVLINKSYNC, GET) 5s 1m");
   SY(SYS_modctl, x0 + MODNVL_DEVLINKSYNC,
#     if defined(HAVE_SYS_SYSNVL_H)
      x0 + SYSNVL_OP_GET,
#     else
      x0 + MODCTL_NVL_OP_GET,
#     endif /* HAVE_SYS_SYSNVL_H */
      x0, x0 + 1, x0); FAIL;
}

__attribute__((noinline))
static void sys_modctl2(void)
{
   uint64_t buflen = x0 + 10;

   GO(SYS_modctl, "(MODNVL_DEVLINKSYNC, GET) 4s 2m");
   SY(SYS_modctl, x0 + MODNVL_DEVLINKSYNC,
#     if defined(HAVE_SYS_SYSNVL_H)
      x0 + SYSNVL_OP_GET,
#     else
      x0 + MODCTL_NVL_OP_GET,
#     endif /* HAVE_SYS_SYSNVL_H */
      x0 + 1, &buflen, x0 + 1); FAIL;
}

__attribute__((noinline))
static void sys_modctl3(void)
{
   GO(SYS_modctl, "(MODNVL_DEVLINKSYNC, UPDATE) 4s 1m");
   SY(SYS_modctl, x0 + MODNVL_DEVLINKSYNC,
#     if defined(HAVE_SYS_SYSNVL_H)
      x0 + SYSNVL_OP_UPDATE,
#     else
      x0 + MODCTL_NVL_OP_UPDATE,
#     endif /* HAVE_SYS_SYSNVL_H */
      x0, x0 + 1); FAIL;
}

__attribute__((noinline))
static void sys_modctl4(void)
{
   uint64_t buflen = x0 + 10;

   GO(SYS_modctl, "(MODNVL_DEVLINKSYNC, UPDATE) 4s 1m");
   SY(SYS_modctl, x0 + MODNVL_DEVLINKSYNC,
#     if defined(HAVE_SYS_SYSNVL_H)
      x0 + SYSNVL_OP_UPDATE,
#     else
      x0 + MODCTL_NVL_OP_UPDATE,
#     endif /* HAVE_SYS_SYSNVL_H */
      x0 + 1, &buflen); FAIL;
}

__attribute__((noinline))
static void sys_modctl5(void)
{
   GO(SYS_modctl, "(MODDEVINFO_CACHE_TS) 2s 1m");
   SY(SYS_modctl, x0 + MODDEVINFO_CACHE_TS, x0 + 1); FAIL;
}

int main(void)
{
   /* Uninitialised, but we know px[0] is 0x0. */
   long *px = malloc(sizeof(long));
   x0 = px[0];

   /* SYS_modctl                152 */
   sys_modctl();
   sys_modctl2();
   sys_modctl3();
   sys_modctl4();
   sys_modctl5();

   return 0;
}

