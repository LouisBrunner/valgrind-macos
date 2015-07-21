/* Tests that Valgrind correctly handles syscalls returning
   either 1 (in %rax) or 2 values (in %rdx:%rax). */

#include <stdio.h>
#include <sys/syscall.h>
#include <sys/types.h>

#define GARBAGE 0x0caffedeadbeef

static void syscall_rval(int sysno, uint64_t *rval_hi, uint64_t *rval_lo)
{
   __asm__ (
      "movq %[INPUT1],%%rdx\n"
      "movq %[SYSCALL_NUMBER],%%rax\n"
      "syscall\n"
      "movq %[RVAL_HI],%%rcx\n"
      "movq %%rdx,(%%rcx)\n"
      "movq %[RVAL_LO],%%rcx\n"
      "movq %%rax,(%%rcx)\n"
      : [RVAL_HI] "=m" (rval_hi), [RVAL_LO] "=m" (rval_lo)	/* output */
      : [INPUT1] "i" (GARBAGE), [SYSCALL_NUMBER] "g" (sysno)	/* input */
      : "rax", "rcx", "rdx", "cc", "memory");			/* clobbers */
}

static int syscall_rval1(void) {
   uint64_t valHi, valLo;

   /* Syscall lwp_self returns just tid in rax. */
   valHi = valLo = GARBAGE;
   syscall_rval(SYS_lwp_self, &valHi, &valLo);
   if ((valHi != GARBAGE) || (valLo != 1)) {
      fprintf(stderr, "rval1 FAILED [%#lx:%#lx]\n", valHi, valLo);
      return 1;
   }

   return 0;
}

static int syscall_rval2(void) {
   uint64_t valHi, valLo;

   /* Syscall getpid returns pid in rax and ppid in rdx. */
   valHi = valLo = GARBAGE;
   syscall_rval(SYS_getpid, &valHi, &valLo);
   if ((valHi == GARBAGE) || (valLo == GARBAGE)) {
      fprintf(stderr, "rval2 FAILED [%#lx:%#lx]\n", valHi, valLo);
      return 1;
   }

   return 0;
}

int main(void) {
   int ret = 0;

   ret |= syscall_rval1();
   ret |= syscall_rval2();

   if (ret != 0)
     fprintf(stderr, "FAIL\n");

   return ret;
}
