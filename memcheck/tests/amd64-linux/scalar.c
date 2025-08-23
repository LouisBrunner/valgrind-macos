#define _GNU_SOURCE

#include "../../memcheck.h"
#include "scalar.h"
#include <unistd.h>
#include <sys/resource.h>

// Here we are trying to trigger every syscall error (scalar errors and
// memory errors) for every syscall.  We do this by passing a lot of bogus
// arguments, mostly 0 and 1 (often it's 1 because NULL ptr args often aren't
// checked for memory errors, or in order to have a non-zero length used
// with some buffer).  So most of the syscalls don't actually succeed and do
// anything.
//
// Occasionally we have to be careful not to cause Valgrind to seg fault in
// its pre-syscall wrappers;  it does so because it can't know in general
// when memory is unaddressable, and so tries to dereference it when doing
// PRE_MEM_READ/PRE_MEM_WRITE calls.  (Note that Memcheck will
// always issue an error message immediately before these seg faults occur).
//
// The output has numbers like "3s 2m" for each syscall.  "s" is short for
// "scalar", ie. the argument itself is undefined.  "m" is short for "memory",
// ie. the argument points to memory which is unaddressable.

int main(void)
{
   // uninitialised, but we know px[0] is 0x0
   long* px  = malloc(sizeof(long));
   long  x0  = px[0];
   long  res;

   // All __NR_xxx numbers are taken from amd64

   /* Check the syscall number 0 and 1 two trivial generic syscalls. */

   /* __NR_read 0 */
   /* Nb: here we are also getting an error from the syscall arg itself. */
   GO(__NR_read, "1+3s 1m");
   SY(__NR_read + x0, x0, x0, x0 + 1); FAIL;

   /* __NR_write 1 */
   GO(__NR_write, "3s 1m");
   SY(__NR_write, x0, x0, x0 + 1); FAIL;

   // __NR_exit 60
   GO(__NR_exit, "below");
   // (see below)

   // __NR_getrlimit 97
   GO(__NR_getrlimit, "2s 1m");
   SY(__NR_getrlimit, x0, x0); FAIL;

   // __NR_setrlimit 160
   GO(__NR_setrlimit, "2s 1m");
   SY(__NR_setrlimit, x0, x0); FAILx(EFAULT);

  // __NR_waitid 247
   GO(__NR_waitid, "5s 0m");
   SY(__NR_waitid, x0, x0, x0, x0, x0); FAIL;

   GO(__NR_waitid, "(infop,ru) 5s 2m");
   SY(__NR_waitid, x0, x0, x0 + 1, x0, x0 + 1); FAIL;

   // __NR_prlimit64 302
   GO(__NR_prlimit64, "(nop) 4s 0m");
   SY(__NR_prlimit64, x0, x0 + RLIMIT_NOFILE, x0, x0); SUCC;

   GO(__NR_prlimit64, "(set) 4s 1m");
   SY(__NR_prlimit64, x0, x0 + RLIMIT_NOFILE, x0 + 1, x0); FAILx(EFAULT);

   GO(__NR_prlimit64, "(get) 4s 1m");
   SY(__NR_prlimit64, x0, x0 + RLIMIT_NOFILE, x0, x0 + 1); FAILx(EFAULT);

   GO(__NR_prlimit64, "(get+set) 4s 2m");
   SY(__NR_prlimit64, x0, x0 + RLIMIT_NOFILE, x0 + 1, x0 + 1); FAILx(EFAULT);

    // no such syscall...
   GO(9999, "1e");
   SY(9999); FAIL;

   // __NR_exit 1
   GO(__NR_exit, "1s 0m");
   SY(__NR_exit, x0); FAIL;

   assert(0);
}

