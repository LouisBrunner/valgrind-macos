
#include "tests/malloc.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>
#include <signal.h>

void maybe_fault ( int delta )
{
   char* x = memalign16(32);
   memset(x, 0, 32);
   __asm__ __volatile__(
      "pabsb (%0),%%xmm7"
      : /*out*/ : /*in*/ "r"(x+delta) : /*trash*/"xmm7" );
   free(x);
}

void handler ( int signo )
{
#if defined(__FreeBSD__)
   assert(signo == SIGBUS);
#else
   assert(signo == SIGSEGV);
#endif
   fprintf(stderr, "three\n");
   exit(0);
}

int main ( void )
{
#if defined(__FreeBSD__)
   signal(SIGBUS, handler);
#else
   signal(SIGSEGV, handler);
#endif
   fprintf(stderr, "you should see: \"one\\ntwo\\nthree\\n\"\n");
   fprintf(stderr, "one\n");
   maybe_fault(0);
   fprintf(stderr, "two\n");
   maybe_fault(5);
   fprintf(stderr, "test failed! you shouldn't see this\n");
   return 0;
}
