/* Scalar test for new zone syscall subcodes available on Solaris 11. */

#include "scalar.h"

#include <sys/zone.h>

__attribute__((noinline))
static void sys_zone(void)
{
   GO(SYS_zone, "(ZONE_LIST_DEFUNCT) 3s 1m");
   SY(SYS_zone, x0 + ZONE_LIST_DEFUNCT, x0 + 1, x0 + 2); FAIL;
}

__attribute__((noinline))
static void sys_zone2(void)
{
   uint_t numzones = x0 + 1;

   GO(SYS_zone, "(ZONE_LIST_DEFUNCT) 2s 1m");
   SY(SYS_zone, x0 + ZONE_LIST_DEFUNCT, x0 + 1, &numzones); SUCC;
}

__attribute__((noinline))
static void sys_zone3(void)
{
   GO(SYS_zone, "(ZONE_GETATTR_DEFUNCT) 5s 2m");
   SY(SYS_zone, x0 + ZONE_GETATTR_DEFUNCT, x0 + 1, x0, x0 + 2, x0 + 3);
   FAIL;
}

int main(void)
{
   /* Uninitialised, but we know px[0] is 0x0. */
   long *px = malloc(sizeof(long));
   x0 = px[0];

   /* SYS_zone                  227 */
   sys_zone();
   sys_zone2();
   sys_zone3();

   return 0;
}

