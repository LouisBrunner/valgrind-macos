/* Test for lwp_name syscall which is available on newer Solaris. */ 

#include "scalar.h"

__attribute__((noinline))
static void sys_lwp_name(void)
{
   GO(SYS_lwp_name, "(lwp_setname) 3s 1m");
   SY(SYS_lwp_name, x0 + 0, x0, x0 + 1); FAIL;
}

__attribute__((noinline))
static void sys_lwp_name2(void)
{  
   GO(SYS_lwp_name, "(lwp_getname) 4s 1m");
   SY(SYS_lwp_name, x0 + 1, x0, x0 + 1, x0 + 2); FAIL;
}

int main(void)
{
   /* Uninitialised, but we know px[0] is 0x0. */
   long *px = malloc(sizeof(long));
   x0 = px[0];

   /* SYS_lwp_name              79 */
   sys_lwp_name();
   sys_lwp_name2();

   return 0;
}

