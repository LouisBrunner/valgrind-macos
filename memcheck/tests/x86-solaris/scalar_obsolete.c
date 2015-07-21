/* Test for syscalls that are available on illumos but are removed on
   Solaris 11.  This test is compiled only on illumos. */

#include "../solaris/scalar.h"

#include <sys/fcntl.h>

/* Helper functions.  These are necessary if we've got two tests for a single
   syscall.  In that case, Memcheck can sometimes merge error messages.  Doing
   each test in its own function prevents that. */
__attribute__((noinline))
static void sys_open64(void)
{
   GO(SYS_open64, "(2-args) 2s 1m");
   SY(SYS_open64, x0, x0); FAIL;
}

__attribute__((noinline))
static void sys_open642(void)
{
   GO(SYS_open64, "(3-args) 3s 1m");
   SY(SYS_open64, x0, x0 | O_CREAT, x0); FAIL;
}

int main(void)
{
   /* Uninitialised, but we know px[0] is 0x0. */
   long *px = malloc(sizeof(long));
   x0 = px[0];

   /* SYS_stat64                215 */
   GO(SYS_stat64, "2s 2m");
   SY(SYS_stat64, x0, x0); FAIL;

   /* SYS_lstat64               216 */
   GO(SYS_lstat64, "2s 2m");
   SY(SYS_lstat64, x0, x0); FAIL;

   /* SYS_fstat64               217 */
   GO(SYS_fstat64, "2s 1m");
   SY(SYS_fstat64, x0, x0); FAIL;

   /* SYS_open64                225 */
   sys_open64();
   sys_open642();

   return 0;
}

