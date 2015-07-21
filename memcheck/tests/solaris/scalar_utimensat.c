/* Test for utimensat() syscall which is available on newer Solaris. */ 

#include "scalar.h"

__attribute__((noinline))
static void sys_utimensat(void)
{
   GO(SYS_utimensat, "4s 2m");
   SY(SYS_utimensat, x0 - 1, x0 + 1, x0 + 1, x0); FAIL;
}

__attribute__((noinline))
static void sys_utimensat2(void)
{
   GO(SYS_utimensat, "4s 0m");
   SY(SYS_utimensat, x0 - 1, x0 + NULL, x0 + NULL, x0); FAIL;
}

int main(void)
{
   /* Uninitialised, but we know px[0] is 0x0. */
   long *px = malloc(sizeof(long));
   x0 = px[0];

   /* SYS_utimensat            110 */
   sys_utimensat();
   sys_utimensat2();

   return 0;
}

