/* Basic syscall test for Solaris/x86 specific syscalls. */

#include "scalar.h"

#include <string.h>
#include <sys/fcntl.h>
#include <sys/lwp.h>
#include <sys/statvfs.h>

/* Helper functions.  These are necessary if we've got two tests for a single
   syscall.  In that case, Memcheck can sometimes merge error messages.  Doing
   each test in its own function prevents that. */
__attribute__((noinline))
static void sys_openat64(void)
{
   GO(SYS_openat64, "(3-args) 3s 1m");
   SY(SYS_openat64, x0 - 1, x0, x0); FAILx(EBADF);
}

__attribute__((noinline))
static void sys_openat642(void)
{
   GO(SYS_openat64, "(4-args) 4s 1m");
   SY(SYS_openat64, x0 - 1, x0, x0 | O_CREAT, x0); FAILx(EBADF);
}

__attribute__((noinline))
static void sys_statvfs64(void)
{
   GO(SYS_statvfs64, "2s 2m");
   SY(SYS_statvfs64, x0 + 1, x0 + 1); FAIL;
}

__attribute__((noinline))
static int sys_statvfs642(void)
{
   const char path[] = "/";
   struct statvfs64 stats;

   GO(SYS_statvfs64, "4s 0m");
   SY(SYS_statvfs64, x0 + path, x0 + &stats); SUCC;

   size_t basetype_len = strlen(stats.f_basetype);
   size_t fstr_len = strlen(stats.f_fstr);

   /* Now check that memory after the strings is reported uninitialized. */
   int x = 0;
   if (stats.f_basetype[basetype_len + 2] != ' ') x = -1; else x = -2;
   if (stats.f_fstr[fstr_len + 2] != ' ') x = -3; else x = -4;
   return x;
}

int main(void)
{
   /* Uninitialised, but we know px[0] is 0x0. */
   long *px = malloc(sizeof(long));
   x0 = px[0];

   /* SYS_fstatat64              67 */
   GO(SYS_fstatat64, "4s 2m");
   SY(SYS_fstatat64, x0 - 1, x0 + 1, x0, x0); FAILx(EBADF);

   /* SYS_openat64               69 */
   sys_openat64();
   sys_openat642();

   /* SYS_lwp_private           166 */
   GO(SYS_lwp_private, "3s 1m");
   SY(SYS_lwp_private, x0 + _LWP_GETPRIVATE, x0 + _LWP_GSBASE, x0); FAIL;

   /* SYS_llseek                175 */
   GO(SYS_llseek, "4s 0m");
   SY(SYS_llseek, x0 - 1, x0, x0, x0); FAILx(EBADF);

   /* SYS_getdents64            213 */
   GO(SYS_getdents64, "3s 1m");
   SY(SYS_getdents64, x0, x0, x0 + 1); FAIL;

   /* SYS_mmap64                214 */
   GO(SYS_mmap64, "7s 0m");
   SY(SYS_mmap64, x0, x0, x0, x0, x0, x0, x0); FAILx(EINVAL);

   /* SYS_stat64                215 */
   /* Tested in x86-solaris/scalar_obsolete.c. */

   /* SYS_lstat64               216 */
   /* Tested in x86-solaris/scalar_obsolete.c. */

   /* SYS_fstat64               217 */
   /* Tested in x86-solaris/scalar_obsolete.c. */

   /* SYS_statvfs64             218 */
   sys_statvfs64();
   sys_statvfs642();

   /* SYS_fstatvfs64            219 */
   GO(SYS_fstatvfs64, "2s 1m");
   SY(SYS_fstatvfs64, x0 - 1, x0 + 1); FAILx(EBADF);

   /* SYS_setrlimit64           220 */
   GO(SYS_setrlimit64, "2s 1m");
   SY(SYS_setrlimit64, x0, x0); FAIL;

   /* SYS_getrlimit64           221 */
   GO(SYS_getrlimit64, "2s 1m");
   SY(SYS_getrlimit64, x0, x0); FAIL;

   /* SYS_pread64               222 */
   GO(SYS_pread64, "5s 1m");
   SY(SYS_pread64, x0 - 1, x0, x0 + 1, x0, x0); FAILx(EBADF);

   /* SYS_pwrite64              223 */
   GO(SYS_pwrite64, "5s 1m");
   SY(SYS_pwrite64, x0 - 1, x0, x0 + 1, x0, x0); FAILx(EBADF);

   /* SYS_open64                225 */
   /* Tested in x86-solaris/scalar_obsolete.c. */

   return 0;
}

