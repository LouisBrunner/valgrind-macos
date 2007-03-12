
#undef _GNU_SOURCE
#define _GNU_SOURCE 1

#include <signal.h>
#include <stdio.h>
#include <sys/ucontext.h>

static char* eip_at_sig = NULL;

static void int_handler(int signum, siginfo_t *si, void *uc_arg)
{
   ucontext_t *uc = (ucontext_t *)uc_arg;
   /* Note that uc->uc_mcontext is an embedded struct, not a pointer */
   mcontext_t *mc = &(uc->uc_mcontext);
   void *pc = (void*)mc->gregs[REG_EIP];
   printf("in int_handler, EIP is ...\n");
   eip_at_sig = pc;
}

static void register_handler(int sig, void *handler)
{
   struct sigaction sa;
   sa.sa_flags = SA_RESTART | SA_SIGINFO;
   sigfillset(&sa.sa_mask);
   sa.sa_sigaction = handler;
   sigaction(sig, &sa, NULL);
}

int main(void) {
   char *intaddr = NULL;
   puts("main");
   register_handler(SIGTRAP, int_handler);
   asm volatile(
      "movl $zz_int, %%edx\n"
      "mov %%edx, %0\n"
      "zz_int:\n"
      "int $3\n"
      : /* no outputs */
      : "m" (intaddr) /* input: address of var to store target addr to */
      : /* clobbers */ "edx"
      );
   /* intaddr is the address of the int 3 insn.  eip_at_sig is the PC
      after the exception, which should be the next insn along.
      Hence: */
   if (intaddr != NULL && eip_at_sig != NULL
       && eip_at_sig == intaddr+1)
     printf("PASS\n");
   else
     printf("FAIL\n");
   return 0;
}
