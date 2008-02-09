
#include <malloc.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>
#include <signal.h>

void maybe_fault ( int delta )
{
   char* x = memalign(16/*alignment*/,32/*size*/);
   assert(x);
   assert(0 == ((16-1) & (unsigned long)x));
   memset(x, 0, 32);
   __asm__ __volatile__(
      "pabsb (%0),%%xmm7"
      : /*out*/ : /*in*/ "r"(x+delta) : /*trash*/"xmm7" );
   free(x);
}

void handler ( int signo )
{
   assert(signo == SIGSEGV);
   fprintf(stderr, "three\n");
   exit(0);
}

int main ( void )
{
   signal(SIGSEGV, handler);
   fprintf(stderr, "you should see: \"one\\ntwo\\nthree\\n\"\n");
   fprintf(stderr, "one\n");
   maybe_fault(0);
   fprintf(stderr, "two\n");
   maybe_fault(5);
   fprintf(stderr, "test failed! you shouldn't see this\n");
   return 0;
}
