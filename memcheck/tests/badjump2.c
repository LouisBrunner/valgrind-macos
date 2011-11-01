#include <assert.h>
#include <setjmp.h>
#include <signal.h>
#include <stdio.h>
#include <stdlib.h>

// Regression test for bug 91162:  if a client had a SEGV signal handler,
// and jumped to a bogus address, Valgrind would abort.  With the fix,
// the following test runs to completion correctly.

static jmp_buf myjmpbuf;

static
void SIGSEGV_handler(int signum)
{
   longjmp(myjmpbuf, 1);
}

int main(void)
{
   struct sigaction sigsegv_new, sigsegv_saved;
   int res;

   /* Install own SIGSEGV handler */
   sigsegv_new.sa_handler  = SIGSEGV_handler;
   sigsegv_new.sa_flags    = 0;
#if !defined(__APPLE__)
   sigsegv_new.sa_restorer = NULL;
#endif
   res = sigemptyset( &sigsegv_new.sa_mask );
   assert(res == 0);

   res = sigaction( SIGSEGV, &sigsegv_new, &sigsegv_saved );
   assert(res == 0);

   if (setjmp(myjmpbuf) == 0) {
      // Jump to zero; will cause seg fault
#if defined(__powerpc64__)
      unsigned long int fn[3];
      fn[0] = 0;
      fn[1] = 0;
      fn[2] = 0;
#else
      void (*fn)(void) = 0;
#endif
      ((void(*)(void)) fn) ();
      fprintf(stderr, "Got here??\n");
   } else  {
      fprintf(stderr, "Signal caught, as expected\n");
   }

   return 0;
}

