/* Basic ioctl scalar tests. */

#define __EXTENSIONS__ 1

#include "scalar.h"

#include <sys/crypto/ioctl.h>

/* crypto */
__attribute__((noinline))
static void sys_ioctl_CRYPTO_GET_PROVIDER_LIST(void)
{
   GO(SYS_ioctl, "(CRYPTO_GET_PROVIDER_LIST) 3s 1m");
   SY(SYS_ioctl, x0 - 1, x0 + CRYPTO_GET_PROVIDER_LIST, x0 + 1); FAIL;
}

__attribute__((noinline))
static void sys_ioctl_CRYPTO_GET_PROVIDER_LIST_2(void)
{
   crypto_get_provider_list_t pl;

   pl.pl_count = x0 + 1;

   GO(SYS_ioctl, "(CRYPTO_GET_PROVIDER_LIST) 4s 0m");
   SY(SYS_ioctl, x0 - 1, x0 + CRYPTO_GET_PROVIDER_LIST, &pl + x0); FAIL;
}

int main(void)
{
   /* Uninitialised, but we know px[0] is 0x0. */
   long *px = malloc(sizeof(long));
   x0 = px[0];

   /* crypto */
   sys_ioctl_CRYPTO_GET_PROVIDER_LIST();
   sys_ioctl_CRYPTO_GET_PROVIDER_LIST_2();

   return 0;
}

