#include "tests/sys_mman.h"

int main ( void )
{
#if defined(__powerpc64__) || defined(_AIX)
   /* on ppc64-linux, a function pointer points to a function
      descriptor, not to the function's entry point.  Hence to get
      uniform behaviour on all supported targets - a jump to an
      unmapped page - the following is needed. */
   unsigned long long int fake_fndescr[3];
   fake_fndescr[0] = (unsigned long long int)get_unmapped_page();
   fake_fndescr[1] = 0;
   fake_fndescr[2] = 0;
   return ((int(*)(void)) fake_fndescr) ();
#else
   char* p = get_unmapped_page();
   return ((int(*)(void)) p) ();
#endif
}

