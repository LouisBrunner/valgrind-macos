/* Scalar test for new spawn syscall available on Solaris 11. */

#include "scalar.h"

#include <sys/spawn_impl.h>

__attribute__((noinline))
static void sys_spawn(void)
{
   GO(SYS_spawn, "5s 7m");
   SY(SYS_spawn, x0 + 1, x0 + 1, x0 - 1, x0 + 1, x0 - 1); FAIL;
}

__attribute__((noinline))
static void sys_spawn2(void)
{
   kspawn_attr_t ksa;
   ksa.ksa_version = x0 + SPAWN_VERSION;
   ksa.ksa_size = x0 + sizeof(ksa);
   ksa.ksa_attr_off = x0 + 0;
   ksa.ksa_path_off = x0 + 0;
   ksa.ksa_shell_off = x0 + 0;
   char *argenv = "";

   GO(SYS_spawn, "9s 1m");
   SY(SYS_spawn, x0 + 1, x0 + &ksa, sizeof(ksa), x0 + argenv, x0 + 1); FAIL;
}

__attribute__((noinline))
static void sys_spawn3(void)
{
   kspawn_attr_t ksa;
   ksa.ksa_version = x0 + SPAWN_VERSION;
   ksa.ksa_size = x0 + 0xbadcaffe;
   ksa.ksa_attr_off = x0 + 0xbadcaffe;
   ksa.ksa_attr_size = x0 + 1;
   ksa.ksa_path_off = x0 + 0xdeadcaffe;
   ksa.ksa_path_size = x0 + 2;
   ksa.ksa_shell_off = x0 + 0xdeadcaffe;
   ksa.ksa_shell_size = x0 + 3;
   char *argenv = "\1arg1\0\1arg2\0\1arg3\0\0\1env1\0\1env2\0\0";

   GO(SYS_spawn, "13s 4m");
   SY(SYS_spawn, x0 + 1, x0 + &ksa, x0 + 0xbadcaffe,
      x0 + argenv, x0 + sizeof(argenv)); FAIL;
}

__attribute__((noinline))
static void sys_spawn4(void)
{
   char path[] = "/bin/sh";
   char *argenv = "\2arg1\0\0";

   GO(SYS_spawn, "4s 0m");
   SY(SYS_spawn, path, x0, x0, x0 + argenv, x0 + sizeof(argenv));
   FAILx(EINVAL);
}

int main(void)
{
   /* Uninitialised, but we know px[0] is 0x0. */
   long *px = malloc(sizeof(long));
   x0 = px[0];

   /* SYS_spawn                   2 */
   sys_spawn();
   sys_spawn2();
   sys_spawn3();
   sys_spawn4();

   return 0;
}
