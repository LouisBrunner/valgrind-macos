/*
   Check that a fault signal handler gets the expected info
 */
#include <signal.h>
#include <stdio.h>
#include <stdlib.h>
#include <fcntl.h>
#include <setjmp.h>
#include <unistd.h>

struct test {
   void (*test)(void);
   int sig;
   int code;
};

static const struct test *curr_test;

static jmp_buf escape;

static int testsig(int sig, int want)
{
   if (sig != want) {
      fprintf(stderr, "  FAIL: expected signal %d, not %d\n", want, sig);
      return 0;
   }
   return 1;
}

static int testcode(int code, int want)
{
   if (code != want) {
      fprintf(stderr, "  FAIL: expected si_code==%d, not %d\n", want, code);
      return 0;
   }
   return 1;
}

static void handler(int sig, siginfo_t *si, void *uc)
{
   int ok = 1;

   ok = ok && testsig(sig, curr_test->sig);
   ok = ok && testcode(si->si_code, curr_test->code);

   if (ok)
      fprintf(stderr, "  PASS\n");

   siglongjmp(escape, ok + 1);
}

static void test1(void)
{
   __asm__ volatile("li $t0, 0x80000000\n\t"
                    "move $t1, $t0\n\t"
                    "add $a0, $t0, $t1\n\t"
                     : : : "t0", "t1", "a0", "cc", "memory");
}

static void test2()
{
   __asm__ volatile("li $t0, 0x7fffffff\n\t"
                    "li $a0, 0x7fff\n\t"
                    "add $a0, $t0, $a0\n\t"
                     : : : "t0", "a0", "cc", "memory");
}

static void test3(void)
{
   __asm__ volatile("li $t0, 0xffff0000\n\t"
                    "li $t1, 0x7fffffff\n\t"
                    "sub $a0, $t0, $t1\n\t"
                     : : : "t0", "t1", "a0", "cc", "memory");
}

int main()
{
   int i;
   static const int sigs[] = { SIGFPE };
   struct sigaction sa;
   sa.sa_sigaction = handler;
   sa.sa_flags = SA_SIGINFO;
   sigfillset(&sa.sa_mask);

   for(i = 0; i < sizeof(sigs)/sizeof(*sigs); i++)
      sigaction(sigs[i], &sa, NULL);

   const struct test tests[] = {
#define T(n, sig, code) { test##n, sig, code }
      T(1, SIGFPE, FPE_INTOVF),
      T(2, SIGFPE, FPE_INTOVF),
      T(3, SIGFPE, FPE_INTOVF),
#undef T
   };

   for(i = 0; i < sizeof(tests)/sizeof(*tests); i++) {
      curr_test = &tests[i];
      if (sigsetjmp(escape, 1) == 0) {
         fprintf(stderr, "Test %d: ", i+1);
         tests[i].test();
         fprintf(stderr, "  FAIL: no fault, or handler returned\n");
      }
   }
   return 0;
}
