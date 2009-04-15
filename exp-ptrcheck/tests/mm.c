#include "tests/sys_mman.h"
#include <unistd.h>
#include "arith_include1.c"

// For some reason, the stack frame below __GI_write is disappearing.
// Therefore, if I don't want the write errors to be merged, I have to
// ensure they have a different stack trace.  I do this by using this
// function.  Weird.
void mywrite(char* buf, int len)
{
   write(-1, buf, len);
}

int main(void)
{
   struct sigaction sigsegv;
   
   char c;
   
   // This fails due to a bad fd (at one point I was not handling failing
   // mmap() calls, and would have got a seg fault).
   char* res1 = mmap(0, 0, PROT_READ, MAP_PRIVATE, -1, 0 );

   // This succeeds but is meaningless.  Important thing is that the size is
   // zero, so Annelid should not subtract one from the size when doing any
   // range calculations.  (It did at one point, giving 0xffffffff, which
   // screwed everything up.)
   char* res2 = mmap(0, 0, PROT_READ, MAP_PRIVATE|MAP_ANONYMOUS, -1, 0 );

   // This succeeds and is useful.
   char* res3 = mmap(0, getpagesize(), PROT_READ, MAP_PRIVATE|MAP_ANONYMOUS, -1, 0);

   assert(MAP_FAILED == res1);
   assert(NULL       == res2);
   assert(MAP_FAILED != res3 && NULL != res3);

   // Intall SEGV handler 
   sigsegv.sa_handler = SEGV_handler;
   sigsegv.sa_flags   = 0;
   assert( 0 == sigemptyset( &sigsegv.sa_mask ) );
   assert( 0 == sigaction(SIGSEGV, &sigsegv, NULL) );

   #define TTT(i) \
      if (__builtin_setjmp(TTT_jmpbuf) == 0) { c = res3[i]; }

   TTT(0);
   TTT(-1);
   mywrite(res3,   5);
   mywrite(res3-1, 5);

   assert( 0 == munmap(res3, getpagesize()) );

   TTT(0);
   TTT(-1);
   mywrite(res3,   5);
   mywrite(res3-1, 5);
   
   return 0;
}
