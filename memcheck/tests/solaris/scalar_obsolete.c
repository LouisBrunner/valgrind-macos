/* Test for syscalls that are available on illumos but are removed on
   Solaris 11.  This test is compiled only on illumos. */

#include "scalar.h"

#include <sys/fcntl.h>

__attribute__((noinline))
static void sys_open(void)
{
   GO(SYS_open, "(2-args) 2s 1m");
   SY(SYS_open, x0, x0); FAIL;
}

__attribute__((noinline))
static void sys_open2(void)
{
   GO(SYS_open, "(3-args) 3s 1m");
   SY(SYS_open, x0, x0 | O_CREAT, x0); FAIL;
}

int main(void)
{
   /* Uninitialised, but we know px[0] is 0x0. */
   long *px = malloc(sizeof(long));
   x0 = px[0];

   /* SYS_open                    5 */
   sys_open();
   sys_open2();

   /* SYS_link                    9 */
   GO(SYS_link, "2s 2m");
   SY(SYS_link, x0, x0); FAIL;

   /* SYS_unlink                 10 */
   GO(SYS_unlink, "1s 1m");
   SY(SYS_unlink, x0); FAIL;

   /* SYS_mknod                  14 */
   /* XXX Missing wrapper. */

   /* SYS_chmod                  15 */
   GO(SYS_chmod, "2s 1m");
   SY(SYS_chmod, x0, x0); FAIL;

   /* SYS_chown                  16 */
   GO(SYS_chown, "3s 1m");
   SY(SYS_chown, x0, x0, x0); FAIL;

   /* SYS_stat                   18 */
   GO(SYS_stat, "2s 2m");
   SY(SYS_stat, x0, x0); FAIL;

   /* SYS_fstat                  28 */
   GO(SYS_fstat, "2s 1m");
   SY(SYS_fstat, x0, x0); FAIL;

   /* SYS_access                 33 */
   GO(SYS_access, "2s 1m");
   SY(SYS_access, x0, x0); FAIL;

   /* SYS_rmdir                  79 */
   GO(SYS_rmdir, "1s 1m");
   SY(SYS_rmdir, x0); FAIL;

   /* SYS_mkdir                  80 */
   GO(SYS_mkdir, "2s 1m");
   SY(SYS_mkdir, x0, x0); FAIL;

   /* SYS_lstat                  88 */
   GO(SYS_lstat, "2s 2m");
   SY(SYS_lstat, x0, x0); FAIL;

   /* SYS_symlink                89 */
   GO(SYS_symlink, "2s 2m");
   SY(SYS_symlink, x0, x0); FAIL;

   /* SYS_readlink               90 */
   GO(SYS_readlink, "3s 2m");
   SY(SYS_readlink, x0, x0, x0 + 1); FAIL;

   /* SYS_fchmod                 93 */
   GO(SYS_fchmod, "2s 0m");
   SY(SYS_fchmod, x0 - 1, x0); FAIL;

   /* SYS_fchown                 94 */
   GO(SYS_fchown, "3s 0m");
   SY(SYS_fchown, x0, x0, x0); FAIL;

   /* SYS_lchown                130 */
   GO(SYS_lchown, "3s 1m");
   SY(SYS_lchown, x0, x0, x0); FAIL;

   /* SYS_rename                134 */
   GO(SYS_rename, "2s 2m");
   SY(SYS_rename, x0, x0); FAIL;

   /* SYS_stat64                215 */
   /* Tested in x86-solaris/scalar_obsolete.c. */

   /* SYS_lstat64               216 */
   /* Tested in x86-solaris/scalar_obsolete.c. */

   /* SYS_fstat64               217 */
   /* Tested in x86-solaris/scalar_obsolete.c. */

   /* SYS_open64                225 */
   /* Tested in x86-solaris/scalar_obsolete.c. */

   return 0;
}

