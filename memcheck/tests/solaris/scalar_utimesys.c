/* Test for utimesys() syscall which is available on illumos
   and older Solaris.
 */

#include "scalar.h"

__attribute__((noinline))
static void sys_utimesys(void)
{
   GO(SYS_utimesys, "(FUTIMENS) 3s 1m");
   SY(SYS_utimesys, x0 + 0, x0 - 1, x0 + 1); FAIL;
}

__attribute__((noinline))
static void sys_utimesys2(void)
{
   GO(SYS_utimesys, "(FUTIMENS) 3s 0m");
   SY(SYS_utimesys, x0 + 0, x0 - 1, x0 + NULL); FAIL;
}

__attribute__((noinline))
static void sys_utimesys3(void)
{
   GO(SYS_utimesys, "(UTIMENSAT) 5s 2m");
   SY(SYS_utimesys, x0 + 1, x0 - 1, x0 + 1, x0 + 1, x0); FAIL;
}

__attribute__((noinline))
static void sys_utimesys4(void)
{
   GO(SYS_utimesys, "(UTIMENSAT) 5s 0m");
   SY(SYS_utimesys, x0 + 1, x0 - 1, x0 + NULL, x0 + NULL, x0); FAIL;
}

int main(void)
{
   /* Uninitialised, but we know px[0] is 0x0. */
   long *px = malloc(sizeof(long));
   x0 = px[0];

   /* SYS_utimesys             110 */
   sys_utimesys();
   sys_utimesys2();
   sys_utimesys3();
   sys_utimesys4();

   return 0;
}

