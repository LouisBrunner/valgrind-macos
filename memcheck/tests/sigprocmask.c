
#include <signal.h>
#include <stdio.h>
#include <sys/syscall.h>
#include <unistd.h>

// Reg test for bug #93328: we were using too-big sigset types, and thus
// trashing memory when we wrote out the 'oldset' param from sigprocmask().

int main(void)
{
   int x[6], *s, *os, i;

   x[0] = 0x11111111;
   x[1] = 0x89abcdef;
   x[2] = 0x22222222;
   x[3] = 0x33333333;
   x[4] = 0x0;
   x[5] = 0x44444444;

   s  = &x[1];
   os = &x[4];

   fprintf(stderr, "before\n");
   for (i = 0; i < 6; i++) {
      fprintf(stderr, "%x ", x[i]);
   }
   fprintf(stderr, "\n");

   syscall(__NR_sigprocmask, SIG_BLOCK, s, os);

   fprintf(stderr, "after1\n");
   for (i = 0; i < 6; i++) {
      fprintf(stderr, "%x ", x[i]);
   }
   fprintf(stderr, "\n");
   
   syscall(__NR_sigprocmask, SIG_BLOCK, s, os);

   fprintf(stderr, "after2\n");
   for (i = 0; i < 6; i++) {
      fprintf(stderr, "%x ", x[i]);
   }
   fprintf(stderr, "\n");
   
   return(0);
}
