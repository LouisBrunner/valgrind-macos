/* This is the riscv64 variant of memcheck/tests/x86-linux/scalar.c. It checks
   a selected subset of all syscalls, ones that are in some way interesting from
   the platform perspective. */

#define _GNU_SOURCE
#include "../../../include/vki/vki-scnums-riscv64-linux.h"
#include "../../memcheck.h"
#include <assert.h>
#include <errno.h>
#include <stdio.h>
#include <stdlib.h>
#include <sys/ptrace.h>
#include <sys/syscall.h>
#include <unistd.h>

#define GO(__NR_xxx, s)                                                        \
   fprintf(stderr,                                                             \
           "-----------------------------------------------------\n"           \
           "%4d:%24s %s\n"                                                     \
           "-----------------------------------------------------\n",          \
           __NR_xxx, #__NR_xxx, s);

#define SY   res = syscall
#define FAIL assert(-1 == res);
#define SUCC assert(-1 != res);

#define FAILx(E)                                                               \
   do {                                                                        \
      int myerrno = errno;                                                     \
      if (-1 == res) {                                                         \
         if (E == myerrno) {                                                   \
            /* as expected */                                                  \
         } else {                                                              \
            fprintf(stderr, "Expected error %s (%d), got %d\n", #E, E,         \
                    myerrno);                                                  \
            exit(1);                                                           \
         }                                                                     \
      } else {                                                                 \
         fprintf(stderr, "Expected error %s (%d), got success\n", #E, E);      \
         exit(1);                                                              \
      }                                                                        \
   } while (0);

int main(void)
{
   /* Uninitialised, but we know px[0] is 0x0. */
   long* px = malloc(sizeof(*px));
   long  x0 = px[0];
   long  res;

   /* Check the syscall number 0 and two trivial generic syscalls. */

   /* __NR_io_setup 0 */
   GO(__NR_io_setup, "2s 1m");
   SY(__NR_io_setup, x0, x0);
   FAIL;

   /* __NR_read 63 */
   /* Nb: here we are also getting an error from the syscall arg itself. */
   GO(__NR_read, "1+3s 1m");
   SY(__NR_read + x0, x0, x0, x0 + 1);
   FAIL;

   /* __NR_write 64 */
   GO(__NR_write, "3s 1m");
   SY(__NR_write, x0, x0, x0 + 1);
   FAIL;

   /* Check syscalls not implemented by the kernel on riscv64 and explicitly
      rejected by Valgrind. */

   /* __NR_kexec_load 104 */
   GO(__NR_kexec_load, "n/i");
   SY(__NR_kexec_load);
   FAILx(ENOSYS);

   /* __NR_fadvise64 223 */
   GO(__NR_fadvise64, "n/i");
   SY(__NR_fadvise64);
   FAILx(ENOSYS);

   /* __NR_rseq 293 */
   GO(__NR_rseq, "n/i");
   SY(__NR_rseq);
   FAILx(ENOSYS);

   /* __NR_clone3 435 */
   GO(__NR_clone3, "n/i");
   SY(__NR_clone3);
   FAILx(ENOSYS);

   /* Check platform-specific wrappers. */

   /* __NR_ptrace 117 */
   GO(__NR_ptrace, "4s 1m");
   SY(__NR_ptrace, x0 + PTRACE_PEEKTEXT, x0, x0, x0);
   FAIL;

   /* __NR_rt_sigreturn 139 */
   /* Skipped as it is not valid to call this syscall within this context. */
   GO(__NR_rt_sigreturn, "n/a");
   /*SY(__NR_rt_sigreturn); FAIL;*/

   /* __NR_mmap 222 */
   GO(__NR_mmap, "6s 0m");
   SY(__NR_mmap, x0, x0, x0, x0, x0 - 1, x0);
   FAIL;

   /* __NR_riscv_flush_icache 259 */
   GO(__NR_riscv_flush_icache, "3s 0m");
   SY(__NR_riscv_flush_icache, x0, x0, x0);
   SUCC;

   /* Finally, check an invalid syscall and __NR_exit. */

   /* No such syscall... */
   GO(9999, "1e");
   SY(9999);
   FAILx(ENOSYS);

   /* __NR_exit 1 */
   GO(__NR_exit, "1s 0m");
   SY(__NR_exit, x0);
   FAIL;

   assert(0);
}
