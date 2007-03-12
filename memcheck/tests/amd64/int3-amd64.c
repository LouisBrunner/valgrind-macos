
#undef _GNU_SOURCE
#define _GNU_SOURCE 1

#include <signal.h>
#include <stdio.h>
#include <sys/ucontext.h>

static char* rip_at_sig = NULL;

static void int_handler(int signum, siginfo_t *si, void *uc_arg)
{
   ucontext_t *uc = (ucontext_t *)uc_arg;
   /* Note that uc->uc_mcontext is an embedded struct, not a pointer */
   mcontext_t *mc = &(uc->uc_mcontext);
   void *pc = (void*)mc->gregs[REG_RIP];
   printf("in int_handler, RIP is ...\n");
   rip_at_sig = pc;
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
      "movabsq $zz_int, %%rdx\n"
      "mov %%rdx, %0\n"
      "zz_int:\n"
      "int $3\n"
      : /* no outputs */
      : "m" (intaddr) /* input: address of var to store target addr to */
      : /* clobbers */ "rdx"
      );
   /* intaddr is the address of the int 3 insn.  rip_at_sig is the PC
      after the exception, which should be the next insn along.
      Hence: */
   if (intaddr != NULL && rip_at_sig != NULL
       && rip_at_sig == intaddr+1)
     printf("PASS\n");
   else
     printf("FAIL\n");
   return 0;
}
