/* Scalar test for new shmsys syscall subcodes available on Solaris 11. */

#include "scalar.h"

#include <sys/shm.h>
#include <sys/shm_impl.h>

__attribute__((noinline))
static void sys_shmsys(void)
{
   GO(SYS_shmsys, "(SHMCTL,IPC_XSTAT64) 4s 1m");
   SY(SYS_shmsys, x0 + SHMCTL, x0, x0 + IPC_XSTAT64, x0 + 1); FAIL;
}

__attribute__((noinline))
static void sys_shmsys2(void)
{
   GO(SYS_shmsys, "(SHMADV,SHM_ADV_GET) 4s 1m");
   SY(SYS_shmsys, x0 + SHMADV, x0, x0 + SHM_ADV_GET, x0 + 1); FAIL;
}

__attribute__((noinline))
static void sys_shmsys3(void)
{
   GO(SYS_shmsys, "(SHMADV,SHM_ADV_SET) 4s 1m");
   SY(SYS_shmsys, x0 + SHMADV, x0, x0 + SHM_ADV_SET, x0 + 1); FAIL;
}

__attribute__((noinline))
static void sys_shmsys4(void)
{
   GO(SYS_shmsys, "(SHMGET_OSM) 5s 0m");
   SY(SYS_shmsys, x0 + SHMGET_OSM, x0, x0, x0, x0); FAIL;
}

int main(void)
{
   /* Uninitialised, but we know px[0] is 0x0. */
   long *px = malloc(sizeof(long));
   x0 = px[0];

   /* SYS_shmsys                 52 */
   sys_shmsys();
   sys_shmsys2();
   sys_shmsys3();
   sys_shmsys4();

   return 0;
}

