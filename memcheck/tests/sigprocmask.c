
#include <signal.h>
#include <stdio.h>
#include <sys/syscall.h>
#include <unistd.h>

// Reg test for bug #93328: we were using too-big sigset types, and thus
// trashing memory when we wrote out the 'oldset' param from sigprocmask().

int main(void)
{
   int x[6], *s, *os, i, sysno;

   sysno = __NR_rt_sigprocmask;
#ifdef __NR_sigprocmask
   sysno = __NR_sigprocmask;
#endif

   x[0] = 0x11111111;
   x[1] = 0x89abcdef;
   x[2] = 0x22222222;
   x[3] = 0x33333333;
   x[4] = 0x0;
   x[5] = 0x44444444;

   s  = &x[1];
   os = &x[4];

   // Make sure the system is in a known state with no signals
   // blocked as perl has been known to leave some signals blocked
   // when starting child processes which can cause failures in
   // this test unless we reset things here.
   syscall(sysno, SIG_SETMASK, os, NULL);

   fprintf(stderr, "before\n");
   for (i = 0; i < 6; i++) {
      fprintf(stderr, "%x ", x[i]);
   }
   fprintf(stderr, "\n");

   syscall(sysno, SIG_BLOCK, s, os);

   fprintf(stderr, "after1\n");
   for (i = 0; i < 6; i++) {
      fprintf(stderr, "%x ", x[i]);
   }
   fprintf(stderr, "\n");
   
   syscall(sysno, SIG_BLOCK, s, os);

   fprintf(stderr, "after2\n");
   for (i = 0; i < 6; i++) {
      fprintf(stderr, "%x ", x[i]);
   }
   fprintf(stderr, "\n");
   
   return(0);
}
